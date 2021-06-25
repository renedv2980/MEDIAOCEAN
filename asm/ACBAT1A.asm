*          DATA SET ACBAT1A    AT LEVEL 096 AS OF 03/13/12                      
*PHASE T61B1AA                                                                  
         TITLE 'T61B1A - MEDIA/SPECIAL BILLING'                                 
*                                                                               
*        BATCH TYPES 26, 42 AND 55                                              
*                                                                               
T61B1A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGX-PROGD,**BA1A,R7,R5,CLEAR=YES,RR=R2                         
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         USING TWAD,RA                                                          
*-------------------------------------------------------------------*           
*        BUILD SCREEN DIRECTORY                                                 
*-------------------------------------------------------------------*           
         L     R1,=A(CTAXMOD)                                                   
         AR    R1,R2                                                            
         ST    R1,ACTAXMOD                                                      
*                                                                               
         L     R1,=A(USTAB)                                                     
         CLI   AGYCTRY,CTRYUSA     TEST FOR USA                                 
         BE    *+8                                                              
         L     R1,=A(CANTAB)       NO-ITS CANADIAN                              
         AR    R1,R2                                                            
*                                                                               
BLDSCR   CLI   0(R1),X'FF'         TEST FOR EOT                                 
         BE    CLEARFLD                                                         
         LM    RE,RF,0(R1)                                                      
         LA    RE,TWAD(RE)         FORM READ ADDRESS OF SCREEN FIELD            
         LA    RF,PROGD(RF)        FORM ADDRESS OF ADCON                        
         ST    RE,0(RF)                                                         
         LA    R1,L'USTAB(R1)                                                   
         B     BLDSCR                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
* CLEAR GST/PST FIELDS                                                          
*                                                                               
CLEARFLD DS    0H                                                               
         CLI   CSACT,ACTCHA                                                     
         BNE   *+16                                                             
         MVI   CLEAROK,C'N'        DON'T CLEAR GST/PST FOR CHANGE               
         MVI   USERPROV,C'Y'       USER ENTERED PROV FROM CHANGE                
         MVI   USERPST,C'Y'        USER ENTERED PST TYPE FOR CHANGE             
*                                                                               
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   CFLD30                                                           
         CLI   MODE,2              CANNOT CLEAR WHEN IN ANOTHER                 
         BE    CFLD30                PROGRAM                                    
         CLI   MODE,3                                                           
         BE    CFLD30                                                           
         CLI   CLEAROK,C'N'        NOT OK TO CLEAR?                             
         BE    CFLD20                                                           
*                                                                               
         L     R2,AGAMTH                                                        
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BO    CFLD05              ONLY CLEAR THE OLD ONES                      
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(L'CMCGAMT,R2),SPACES                                           
         XC    XTRAELM,XTRAELM     CLEAR PST FIELDS                             
*                                                                               
CFLD05   L     R2,ABASEH                                                        
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BO    CFLD05A             ONLY CLEAR THE OLD ONES                      
         MVC   8(L'CMCBASE,R2),SPACES        CLEAR INFO DATA TOO                
         OI    6(R2),X'80'                                                      
         MVI   5(R2),0                                                          
*                                                                               
CFLD05A  L     R2,APBASEH                                                       
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BO    CFLD05B             ONLY CLEAR THE OLD ONES                      
         MVC   8(L'CMCPBAS,R2),SPACES        CLEAR INFO DATA TOO                
         OI    6(R2),X'80'                                                      
         MVI   5(R2),0                                                          
*                                                                               
CFLD05B  L     R2,AINPTH                                                        
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BO    CFLD10              ONLY CLEAR THE OLD ONES                      
         MVI   8(R2),C' '                                                       
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CFLD10   L     R2,ATYPEH                                                        
         TM    4(R2),X'80'                                                      
         BO    *+12                                                             
         MVI   8(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6STRO        STEREO?                          
         BZ    CFLD18                                                           
         L     R2,APROVH                                                        
         TM    4(R2),X'80'         NEW PROVINCE?                                
         BO    CFLD25                                                           
         MVI   USERPROV,C'N'                                                    
         B     CFLD29                                                           
*                                                                               
CFLD18   XC    VENDPROV,VENDPROV                                                
         L     R2,APROVH                                                        
         TM    4(R2),X'80'                                                      
         BO    CFLD20                                                           
         XC    8(2,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
CFLD20   L     R2,APROVH                                                        
         TM    4(R2),X'80'         NEW PROVINCE?                                
         BNO   CFLD30              SAVE PROVINCE                                
CFLD25   MVI   USERPROV,C'Y'       USER ENTERED PROVINCE                        
CFLD29   MVC   KEEPPROV,8(R2)                                                   
         XC    XTRAELM,XTRAELM                                                  
*                                                                               
CFLD30   MVI   CLEAROK,C'N'        DON'T CLEAR UNTIL DONE                       
         EJECT                                                                  
*--------------------------------------------------------------------           
*        MODE=X'00'  ORDER OVERLAY                                              
*        MODE=X'01'  EXIT WITH ORDER DISPLAYED                                  
*        MODE=X'03'  CANADIAN SALES TAX SCREEN                                  
*--------------------------------------------------------------------           
PREVAL   ZAP   GST,=P'0'           INITIALIZE AMOUNTS                           
         ZAP   PST,=P'0'                                                        
         CLI   CSACT,ACTCHA        ITEM CHANGE?                                 
         BNE   PREVAL05                                                         
         CLI   PFKEY,X'FF'         FIRST TIME, IT SETS PFKEY TO FF              
         BNE   *+8                 YES                                          
         MVI   PFKEY,0                                                          
PREVAL05 CLI   MODE,3              CANDIAN TAX?                                 
         BE    PREVAL30                                                         
*                                                                               
         CLI   PFKEY,7             USE PF=7 TO LOAD                             
         BNE   PREVAL20                                                         
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    VALAMT                                                           
         MVI   ERRNUM,SPECIAL                                                   
         MVC   MSG,SPACES                                                       
         MVC   FVMSGNO,=Y(AE$CSNA)                                              
         LA    R2,CONACTH                                                       
         B     ERROR                                                            
*                                                                               
PREVAL20 CLI   PFKEY,0                                                          
         BE    VALAMT                                                           
         MVI   ERRNUM,251          INVALID PFKEY                                
         L     R2,TIACURS                                                       
         B     ERROR                                                            
*                                                                               
PREVAL30 CLI   AGYCTRY,CTRYCAN     CANADIAN?                                    
         BNE   VALAMT                                                           
         MVI   CSSPROG,2                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 ACTAXMOD,DMCB,(RC)                                               
         CLI   CSACT,ACTCHA        ARE WE DOING A CHANGE?                       
         BNE   CURSIT              NO, LEAVE                                    
         CLC   FVMSGNO,=AL2(FVFOK) IF ERROR, RETURN                             
         BNE   CURSIT                                                           
         MVC   FVMSGNO,=X'FFFF'    TELL BT61 WE HAVE TO COME BACK               
         B     CURSIT                                                           
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              VALIDATE AMOUNT FIELDS                                           
*-------------------------------------------------------------------*           
VALAMT   L     R2,AAMTH            BILL AMOUNT                                  
         ZIC   R3,5(R2)                                                         
         LA    R4,8(R2)                                                         
         BAS   R8,VALCASH                                                       
         ZAP   AMT,0(8,R4)                                                      
         CP    AMT,=P'2100000000'                                               
         BH    AMTOOLGE                                                         
         CP    AMT,=P'-2100000000'                                              
         BL    AMTOOLGE                                                         
*                                                                               
         L     R2,ANETH            NET AMOUNT                                   
         ZIC   R3,5(R2)                                                         
         LA    R4,8(R2)                                                         
         BAS   R8,VALCASH                                                       
         ZAP   NET,0(8,R4)                                                      
         CP    NET,=P'2100000000'                                               
         BH    AMTOOLGE                                                         
         CP    NET,=P'-2100000000'                                              
         BL    AMTOOLGE                                                         
*                                                                               
         CLI   INPUT,42            INPUT TYPE 42 HAS TWO TAX FIELDS             
         BNE   AMT0                TO VALIDATE                                  
         ZAP   TAXA,=P'0'                                                       
         ZAP   TAXB,=P'0'                                                       
*                                                                               
         LA    R2,CMBTAXAH                                                      
         CLI   5(R2),0                                                          
         BE    AMTAA                                                            
         ZIC   R3,5(R2)                                                         
         LA    R4,8(R2)                                                         
         BAS   R8,VALCASH                                                       
         ZAP   TAXA,0(8,R4)                                                     
*                                                                               
AMTAA    LA    R2,CMBTAXBH                                                      
         CLI   5(R2),0                                                          
         BE    AMT0                                                             
         ZIC   R3,5(R2)                                                         
         LA    R4,8(R2)                                                         
         BAS   R8,VALCASH                                                       
         ZAP   TAXB,0(8,R4)                                                     
*                                                                               
AMT0     L     R2,ACOMH            COMMISSION                                   
         OC    8(L'CMBCOM,R2),SPACES  COULD BE 2 AMOUNTS                        
         LA    R4,8(R2)            SEPARATED BY A COMMA                         
         MVI   SET,1               ASSUME 1 SET                                 
         XR    R3,R3                                                            
*                                                                               
AMT1     CLI   0(R4),C','          LOOK FOR COMMA OR SPACE                      
         BE    AMT2                                                             
         CLI   0(R4),X'41'                                                      
         BL    AMT2                                                             
         LA    R3,1(R3)            LENGTH IN R3                                 
         LA    R4,1(R4)                                                         
         B     AMT1                                                             
*                                                                               
AMT2     LA    R4,8(R2)            R4 TO DATA                                   
         BAS   R8,VALCASH          VALIDATE FIRST AMOUNT                        
         ZAP   COM,0(8,R4)         SAVE COMMISSION                              
         CP    COM,=P'2100000000'                                               
         BH    AMTOOLGE                                                         
         CP    COM,=P'-2100000000'                                              
         BL    AMTOOLGE                                                         
*                                                                               
         ZAP   COM2,=P'0'                                                       
         ZIC   R4,5(R2)                                                         
         CR    R3,R4               LENGTH IS EQUAL TO HEADER                    
         BE    AMT3                ONE AMOUNT                                   
*                                                                               
         LA    R4,9(R3,R2)         R4 TO SECOND                                 
         ZIC   R8,5(R2)            TOTAL LENGTH                                 
         SR    R8,R3               LESS FIRST                                   
         BCTR  R8,0                LESS ONE                                     
         LTR   R3,R8               R3 TO LENGTH OF SECOND                       
         BNP   ERROR                                                            
         BAS   R8,VALCASH                                                       
         ZAP   COM2,0(8,R4)        SAVE SECOND COMMISSION                       
         MVI   SET,2               THERE ARE TWO SETS                           
         CP    COM2,=P'2100000000'                                              
         BH    AMTOOLGE                                                         
         CP    COM2,=P'-2100000000'                                             
         BL    AMTOOLGE                                                         
*                                                                               
AMT3     ZAP   GST,=P'0'           ZERO GST BUCKET                              
         CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   AMT5                NO                                           
*                                                                               
         L     R2,AGAMTH                                                        
         CLI   5(R2),0             TEST FOR GST AMOUNT                          
         BE    AMT5                NO                                           
*                                                                               
         LA    R4,8(R2)            R4=A(FIELD INPUT)                            
         ZIC   R3,5(R2)            R3=FIELD LENGTH                              
         BAS   R8,VALCASH                                                       
         ZAP   GST,0(8,R4)                                                      
*                                                                               
         L     R2,ATYPEH           RE=A(GST TYPE FIELD)                         
         MVI   ERRNUM,NOINPUT                                                   
         CLI   5(R2),0             TEST FOR TYPE INPUT                          
         BE    ERROR               NONE-INCOMPATIBLE WITH A GST AMOUNT          
         MVI   ERRNUM,INVALID                                                   
         CLI   8(R2),C'*'          TEST GST NOT APPLICABLE                      
         BE    ERROR               ALSO NO GOOD WITH GST AMOUNT                 
*                                                                               
AMT5     ZAP   SPL,AMT             SPECIAL GROSS = BILL AMOUNT                  
*                                  UNLESS SPECIAL IS INPUT                      
         L     R2,ASPLH                                                         
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BZ    AMT7                NO SPECIAL AMOUNT                            
         LA    R4,8(R2)                                                         
         BAS   R8,VALCASH                                                       
         ZAP   SPL,0(8,R4)                                                      
         CP    SPL,=P'2100000000'                                               
         BH    AMTOOLGE                                                         
         CP    SPL,=P'-2100000000'                                              
         BL    AMTOOLGE                                                         
*                                                                               
AMT7     ZAP   CD,=P'0'                                                         
         L     R2,ACDH                                                          
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BZ    VALDAT              NO CASH DISCOUNT                             
         LA    R4,8(R2)                                                         
         BAS   R8,VALCASH                                                       
         ZAP   CD,0(8,R4)                                                       
         CP    CD,=P'2100000000'                                                
         BH    AMTOOLGE                                                         
         CP    CD,=P'-2100000000'                                               
         BL    AMTOOLGE                                                         
         B     VALDAT                                                           
         EJECT                                                                  
VALCASH  MVI   ERRNUM,INVAMNT      INVALID AMOUNT                               
         GOTO1 CASHVAL,DMCB,(X'82',(R4)),(R3)                                   
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         LA    R4,4(R1)            POINT R4 AT OUTPUT                           
         BR    R8                                                               
*                                                                               
VALDAT   L     R2,ADATH            VALIDATE THE DATE                            
         MVI   ERRNUM,INVDATE                                                   
         BAS   RE,ANY                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         L     R3,DMCB             R3=LENGTH OF DATE                            
         CLC   WORK(2),=C'70'      NOT BEFORE 1970                              
         BL    ERROR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,PDAT)                                    
         GOTO1 DATECHK,DMCB,PDAT                                                
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
*--------------------------------------------------------------------*          
*   FOR CANADA, THE USER CAN INPUT TWO DATES, THE FIRST IS THE BILL             
*   DATE AND THE SECOND IS AN OPTIONAL TAX POINT DATE ON THE TYPE 26            
*--------------------------------------------------------------------*          
VALDAT2  CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   VALDATX             NO                                           
*                                                                               
         MVC   TAXPOINT,PDAT       SET TAX POINT=BILL DATE                      
         CLM   R3,1,5(R2)          TEST IF DATE IS WHOLE FIELD                  
         BE    VALDATX             YES-EDIT IS DONE                             
*                                                                               
         CLI   INPUT,55                                                         
         BNE   VALDAT3                                                          
         MVI   ERRNUM,INVALID                                                   
         B     ERROR                                                            
*                                                                               
VALDAT3  LA    R3,8(R2,R3)         POINT TO END OF FIRST DATE                   
         MVI   ERRNUM,INVALID                                                   
         CLI   0(R3),C','          TEST COMMA FOLLOWS FIRST DATE                
         BNE   VALDATR             NO                                           
         MVI   ERRNUM,INVDATE                                                   
         LA    R3,1(R3)            BUMP PAST COMMA                              
         GOTO1 DATVAL,DMCB,(0,(R3)),WORK                                        
         OC    DMCB(4),DMCB        TEST FOR ERROR                               
         BZ    VALDATR                                                          
*                                                                               
VALDAT4  GOTO1 DATCON,DMCB,WORK,(1,TAXPOINT)                                    
         B     VALDATX                                                          
*                                                                               
VALDATR  LA    RE,8(R2)            COMPUTE DISP INTO FIELD                      
         SR    R3,RE                                                            
         STC   R3,ERRNDX                                                        
         B     ERROR                                                            
*                                                                               
VALDATX  B     VALDUE                                                           
*                                                                               
VALDUE   MVI   ERRNUM,INVDATE      DUE DATE                                     
         L     R2,ADUEH            FOR CURSOR REPOSITION ON ERROR               
         CLI   5(R2),0                                                          
         BE    VALADV                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),DUEDAT                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR               BAD DATE                                     
         CLC   DUEDAT(2),=C'78'    DON'T ALLOW PRIOR TO 1978                    
         BL    ERROR                                                            
*                                                                               
VALADV   XC    ADVMOS,ADVMOS       ADVERTISING MONTH                            
         L     R2,AMTHH                                                         
         CLI   5(R2),0                                                          
         BE    CLI                                                              
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR               BAD DATE                                     
         CLC   WORK(2),=C'78'      DON'T ALLOW PRIOR TO 1978                    
         BL    ERROR                                                            
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   ADVMOS,WORK+6                                                    
         L     R2,AESTH            MUST HAVE ESTIMATE\JOB                       
         GOTO1 ANY                 IF INPUTTING DATE                            
         EJECT                                                                  
CLI      LA    R8,COMPEL                                                        
         USING ACCOMPD,R8                                                       
         L     R2,ACLIH            CLIENT                                       
         MVI   ERRNUM,14                                                        
         BAS   RE,ANY                                                           
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(41),SPACES                                                 
         MVC   KEY+1(2),ACMPJOB    U/L FOR PRODUCTION                           
         TM    4(R2),X'20'                                                      
         BO    CLI2                                                             
         DROP  R8                                                               
*                                                                               
         L     R2,APROH                                                         
         NI    4(R2),X'FF'-X'20'   MUST VALIDATE PRODUCT                        
         L     R2,ACLINH                                                        
         MVC   FLD,SPACES          CLEAR CLIENT NAME                            
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         L     R2,APRONH                                                        
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         XC    CLIPROF,CLIPROF                                                  
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
*                                                                               
CLI2     L     R2,ACLIH                                                         
         XR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)                                                   
         LA    R4,KEY+3                                                         
         IC    R3,CLILNGTH         LEVA LENGTH                                  
         AR    R4,R3                                                            
*                                                                               
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVC   CLINUM,KEY                                                       
         OI    4(R2),X'20'                                                      
         L     R2,ACLINH                                                        
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME     CLIENT NAME                         
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         EJECT                                                                  
PROD     L     R2,APROH                                                         
         MVI   ERRNUM,15                                                        
         BAS   RE,ANY                                                           
         TM    4(R2),X'20'                                                      
         BO    PROD2                                                            
         L     R2,APRONH                                                        
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         XC    PRODPROF,PRODPROF                                                
*                                                                               
PROD2    L     R2,APROH                                                         
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),8(R2)                                                    
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
         OI    4(R2),X'20'                                                      
         L     R2,APRONH           PRODUCT NAME                                 
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME PRODUCT NAME                            
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
PROD4    MVC   PRODNUM,KEY         SAVE CLIENT/PROD KEY                         
         MVC   PRODNUM2,KEY        SAVE ALSO FOR JOB VERIFICATION               
         BAS   RE,PROFMERG         MERGE CLIENT/PROD PROFILES                   
         LA    R4,PROFILE                                                       
         USING ACPROFD,R4                                                       
         XC    ANOELM,ANOELM                                                    
         L     R2,AAOFH                                                         
         CLI   5(R2),0                                                          
         BE    PROD4F                                                           
*                                                                               
PROD4D   OI    9(R2),C' '                                                       
         GOTO1 AVALOFFC,DMCB,(X'80',8(R2))                                      
         CLI   ERRNUM,OK                                                        
         BNE   ERROR                                                            
         MVC   SVAOFF,8(R2)                                                     
         LA    R1,ANOELM           BUILD ANALYSED OFFICE ELEMENT                
         USING ANOELD,R1                                                        
         MVI   ANOEL,ANOELQ                                                     
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTPER     PERSON OFFICE                                
         MVC   ANOOFFC,SVAOFF                                                   
*                                                                               
PROD4F   MVC   SVFOFF,ACPROFFC     PRODUCTION OFFICE CODE                       
         MVC   SVCOFF,ACPROFFC                                                  
         CLI   ANOELM,0            IF NO ANALYSIS OFFICE INPUT                  
         BNE   *+10                DEFAULT TO FINANCIAL OFFICE                  
         MVC   SVAOFF,SVFOFF                                                    
                                                                                
*                                                                               
         XC    ELDB,ELDB           BUILD DB ELEMENT NOW                         
         LA    R6,ELDB                                                          
         USING FFTELD,R6                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'FFTCLPRA                               
         MVI   FFTTYPE,FFTTCLPR                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'FFTCLPRA                                               
         MVC   FFTCLAC,CLINUM+3                                                 
         OC    PRODNUM,PRODNUM                                                  
         BZ    *+10                                                             
         MVC   FFTPRAC,PRODNUM+6                                                
         OC    FFTCLPRA,SPACES                                                  
         DROP  R6                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        VALIDATE A/R A/P ACCOUNTS                                              
*--------------------------------------------------------------------*          
OFF2     XR    R6,R6               NO MORE PROFILES                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),ACPRRECV    RECEIVABLE CODE FROM PROFILE                 
         L     R2,AARNH            CLEAR AND XMIT AR NAME                       
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         L     R2,AARH                                                          
         MVI   ARSW,C'N'           SET A/R NOT INPUT                            
         CLI   INPUT,55                                                         
         BNE   *+8                                                              
         BAS   RE,ANY              REQUIRED FOR 55                              
         MVI   ERRNUM,INVALID                                                   
         CLI   5(R2),1                                                          
         BH    OFF3                A/R CODE INPUT                               
         BL    ARAC1               NO INPUT - NOT TYPE 55                       
         CLI   8(R2),C'/'          USE CODE FROM PROFILE                        
         BE    ARAC1                                                            
*                                                                               
OFF3     MVI   ARSW,C'Y'           A/R INPUT                                    
         MVI   ERRNUM,INVALID                                                   
         CLC   8(2,R2),=C'SI'                                                   
         BE    ERROR                                                            
         CLC   8(2,R2),=C'SJ'                                                   
         BE    ERROR                                                            
         CLC   8(2,R2),=C'SK'                                                   
         BE    ERROR                                                            
         LA    R3,LGTAB3                                                        
         LA    R4,LGTENT3                                                       
OFF3G    CLC   0(LGTLEN3,R3),8(R2)                                              
         BE    ERROR                                                            
         LA    R3,LGTLEN3(R3)                                                   
         BCT   R4,OFF3G                                                         
         MVI   ERRNUM,OK                                                        
*                                                                               
         MVC   KEY(1),COMPANY      ELSE USE                                     
         MVC   KEY+1(14),SPACES    THE ACCOUNT                                  
         IC    R3,5(R2)            THAT IS INPUT                                
         BCTR  R3,0                                                             
         CLI   8(R2),C'*'                                                       
         BE    OFF4                                                             
         EX    R3,*+8                                                           
         B     ARAC2                                                            
         MVC   KEY+1(0),8(R2)                                                   
*                                                                               
OFF4     MVC   KEY(15),PRODNUM         SCHLITZ SPECIAL CODE                     
         LTR   R3,R3                                                            
         BZ    ERROR                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+9(0),9(R2)      PUT 1-BYTE A/R WITH CLI/PRODUCT              
         MVC   KEY+1(2),=C'SR'     TO MAKE A RECEIVABLE ACCOUNT                 
         CLC   TWAAGY,=C'DM'       THIS SHIT IS FOR DOREMUS                     
         BNE   ARAC2               IRENE TO TELL WHEN DONE                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),9(R2)                                                   
         CLC   KEY+1(2),=C'SK'                                                  
         BE    ARAC2                                                            
         MVI   ERRNUM,INVALID                                                   
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
ARAC1    L     R2,ACLIH            POINT TO CLIENT IF ACPRREVC                  
ARAC2    BAS   RE,GETACC           ELSE R2 POINTS                               
         BAS   RE,CHECKACC                                                      
         MVC   ANALBYTE,ACCOST                                                  
         TM    ACCTSTAT,X'08'                                                   
         BZ    *+8                                                              
         OI    POSTSW,DEPTPST                                                   
         TM    ACCTSTAT,X'40'                                                   
         BZ    *+8                                                              
         OI    POSTSW,STAFPST                                                   
         CLI   ACCOST,C' '                                                      
         BNH   *+12                                                             
         OI    COSTSW,OLDCOST                                                   
         OI    COSTSW,ANYCOST                                                   
*                                                                               
                                                                                
         CLI   8(R2),C'*'                                                       
         BNE   ARAC3                                                            
         CLC   9(2,R2),=C'SE'                                                   
         BE    ARAC3                                                            
         L     R2,AAOFH                                                         
         MVI   ERRNUM,INVALID                                                   
         CLI   5(R2),0                                                          
         BE    ARAC3                                                            
         TM    POSTSW,DEPTPST                                                   
         BNZ   ARAC3                                                            
         TM    POSTSW,STAFPST                                                   
         BZ    ERROR                                                            
*                                                                               
ARAC3    CLI   ARSW,C'N'                                                        
         BE    ARAC4                                                            
         L     R2,AARNH                                                         
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         BAS   RE,MOVEFLD          IF A/R ACCOUNT PUT OUT NAME                  
         OI    6(R2),X'80'                                                      
*                                                                               
ARAC4    L     R2,AARH                                                          
         MVC   ARNUM,KEY           SAVE A/R ACCOUNT OR ACPRREVC                 
         MVC   ARNAM,ACCTNAME                                                   
         XC    EXPACC,EXPACC                                                    
         CLC   ARNUM+1(2),=C'SE'                                                
         BNE   ARAC5                                                            
         MVC   EXPACC,ARNUM                                                     
*                                                                               
ARAC5    L     RE,APRONH                                                        
         MVC   PRODNAM,8(RE)       DEAL WITH SALES ANALYSIS ELEMENT             
         MVC   KEY(15),PRODNUM                                                  
         L     R2,APROH                                                         
         BAS   RE,HIGH                                                          
         LA    R1,IOAREA                                                        
*                                                                               
ARAC6    CLI   0(R1),0                                                          
         BE    APAC                                                             
         CLI   0(R1),X'3D'                                                      
         BE    ARAC8                                                            
         ZIC   RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     ARAC6                                                            
*                                                                               
         USING ACSAND,R1                                                        
ARAC8    MVC   KEY(15),ACSACODE                                                 
         MVC   PRODNUM,ACSACODE                                                 
         BAS   RE,GETACC                                                        
         MVC   PRODNAM,ACCTNAME                                                 
         EJECT                                                                  
APAC     L     R2,AAPH             DEAL WITH A/P OVERRIDE                       
         XC    APNUM,APNUM                                                      
         CLI   5(R2),0                                                          
         BNE   APAC3                                                            
*                                                                               
         L     R2,AAPNH                                                         
         MVC   FLD,SPACES          CLEAR A/P NAME                               
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AAORH            AGENCY OF RECORD AMOUNT                      
         CLI   5(R2),0                                                          
         BE    APACA                                                            
         MVI   ERRNUM,INVALID                                                   
         B     ERROR                                                            
*                                                                               
APACA    L     R2,ACOFH            VENDOR OFFICE                                
         CLI   5(R2),0                                                          
         BE    BILLSRC                                                          
         MVI   ERRNUM,INVALID                                                   
         B     ERROR                                                            
*                                                                               
APAC3    OI    POSTSW,APOVER                                                    
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),=C'SE'     ASSUME U/L=SE                                
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)                                                   
         CLI   8(R2),C'*'          TEST LEDGER OVERRIDE                         
         BE    APAC4               YES                                          
         OI    POSTSW,APSEOVR                                                   
*                                                                               
         CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   APAC9               NO                                           
         L     RE,AINPTH                                                        
         CLI   INPUT,55            GST INPUT IS INVALID FOR TYPE 55             
         BNE   APAC3C                                                           
         CLI   5(RE),0                                                          
         BZ    APAC3C                                                           
         MVI   ERRNUM,INVALID                                                   
         B     ERROR                                                            
*                                                                               
APAC3C   CLI   8(RE),C'Y'          TEST INPUT GST ITEM                          
         BNE   APAC9               NO                                           
         MVI   ERRNUM,INVALID      YES-REJECT SE                                
         B     ERROR                                                            
*                                                                               
APAC4    MVI   ERRNUM,INVALID                                                   
         CLI   9(R2),C'S'          CHECK UNIT='S' OR 'G'                        
         BE    APAC6                                                            
         CLI   9(R2),C'G'                                                       
         BNE   ERROR                                                            
*                                                                               
APAC6    CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   APAC7               NO                                           
*                                                                               
         L     RE,AINPTH           TEST FOR INPUT GST ENTRY                     
         CLI   8(RE),C'Y'                                                       
         BNE   APAC7               NO                                           
*                                                                               
         LA    RE,INPLEDTB         RE=A(INPUT GST LEDGER LIST)                  
         LA    R0,NINPLEDG         R0=LOOP COUNTER                              
         CLC   9(2,R2),0(RE)       TEST U/L VERSUS TABLE                        
         BE    APAC7               YES-A VALID UNIT/LEDGER                      
         LA    RE,2(RE)                                                         
         BCT   R0,*-14                                                          
*                                                                               
         MVC   MSGNO,=Y(AE$INVLG)                                               
         B     ERROR                                                            
*                                                                               
APAC7    BCTR  R3,0                                                             
         MVC   KEY+1(L'KEY-1),SPACES                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),9(R2)                                                   
         CLC   KEY+1(2),=C'SE'                                                  
         BNE   *+8                                                              
         OI    POSTSW,APSEOVR                                                   
*                                                                               
APAC9    MVC   SVCOFF,SVFOFF                                                    
         L     R2,ACOFH            VENDOR OFFICE                                
         CLI   5(R2),0                                                          
         BE    APAC9F                                                           
         OI    9(R2),C' '                                                       
         GOTO1 AVALOFFC,DMCB,(X'80',8(R2))                                      
         CLI   ERRNUM,OK                                                        
         BNE   ERROR                                                            
*                                                                               
         LA    R3,LGTAB2                                                        
         LA    R4,LGTENT2                                                       
APAC9B   CLC   0(LGTLEN2,R3),KEY+1                                              
         BE    APAC9C                                                           
         LA    R3,LGTLEN2(R3)                                                   
         BCT   R4,APAC9B                                                        
         MVI   ERRNUM,INVALID                                                   
         B     ERROR                                                            
APAC9C   MVC   SVCOFF,8(R2)                                                     
*                                                                               
APAC9F   L     R2,AAPH                                                          
         CLC   9(2,R2),=C'SI'                                                   
         BE    ERROR                                                            
         CLC   9(2,R2),=C'SJ'                                                   
         BE    ERROR                                                            
         CLC   9(2,R2),=C'SK'                                                   
         BE    ERROR                                                            
         CLC   9(2,R2),=C'SR'                                                   
         BE    ERROR                                                            
         CLI   INPUT,55                                                         
         BNE   APAC10                                                           
         LA    R3,LGTAB1                                                        
         LA    R4,LGTENT1                                                       
APAC9G   CLC   0(LGTLEN1,R3),KEY+1                                              
         BE    APAC9H                                                           
         LA    R3,LGTLEN1(R3)                                                   
         BCT   R4,APAC9G                                                        
         B     APAC10                                                           
APAC9H   MVI   ERRNUM,INVALID                                                   
         B     ERROR                                                            
*                                                                               
APAC10   L     R2,AAPH                                                          
         CLC   ARNUM+1(2),=C'SE'                                                
         BNE   APAC15                                                           
         CLC   KEY+1(2),=C'SE'                                                  
         BNE   APAC15                                                           
APAC11   MVI   ERRNUM,INVALID      YES-REJECT SE                                
         B     ERROR                                                            
*                                                                               
APAC15   BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         CLC   KEY+1(2),=C'SE'                                                  
         BNE   APAC20                                                           
         MVC   EXPACC,KEY                                                       
APAC20   MVI   ERRNUM,INVALID                                                   
         MVC   ANALBYTE,ACCOST                                                  
         TM    ACCTSTAT,X'08'                                                   
         BZ    *+8                                                              
         OI    POSTSW,DEPTPST                                                   
         TM    ACCTSTAT,X'40'                                                   
         BZ    *+8                                                              
         OI    POSTSW,STAFPST                                                   
         CLI   ACCOST,C' '                                                      
         BNH   *+12                                                             
         OI    COSTSW,OLDCOST                                                   
         OI    COSTSW,ANYCOST                                                   
*                                                                               
*        L     R2,AARH                                                          
*        CLC   8(2,R2),=C'SE'                                                   
*        BE    APAC25                                                           
*        L     R2,AAOFH                                                         
*        MVI   ERRNUM,INVALID                                                   
*        CLI   5(R2),0                                                          
*        BE    APAC25                                                           
*        TM    POSTSW,DEPTPST                                                   
*        BNZ   APAC25                                                           
*        TM    POSTSW,STAFPST                                                   
*        BZ    ERROR                                                            
*                                                                               
APAC25   MVC   APNUM,ACCTNUM                                                    
         MVC   APNAM,ACCTNAME                                                   
         TM    4(R2),X'20'                                                      
         BO    APAC26                                                           
*                                                                               
         OI    4(R2),X'20'                                                      
APAC26   L     R2,AAPNH            MOVE NAME TO SCREEN                          
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,HIGH             RE-READ ACCOUNT                              
         MVI   ERRNUM,18                                                        
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
         LA    R4,IOAREA                                                        
APAC26A  CLI   0(R4),0             EOR                                          
         BE    APAC26X                                                          
         CLI   0(R4),ITCELQ        TAX TYPE ELEMENT                             
         BE    APAC26C                                                          
*                                                                               
APAC26B  SR    R3,R3               BUMP TO NEXT ELEMENT                         
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     APAC26A                                                          
*                                                                               
         USING ITCELD,R4                                                        
APAC26C  CLC   PDAT,ITCEFFD        IS TRANS DATE > EFF DATE?                    
         BL    APAC26B                                                          
         OC    ITCPROV,ITCPROV     NO PROV=GST                                  
         BZ    APAC26D                                                          
         MVC   VENDPSTT,ITCTYPE    SAVE PST TYPE                                
         MVC   VENDPROV,ITCPROV    AND THE PROVINCE                             
         B     APAC26B                                                          
*                                                                               
APAC26D  MVC   VENDTYPE,ITCTYPE    SAVE GST TYPE                                
         B     APAC26B                                                          
*                                                                               
APAC26X  CLI   AGYCTRY,CTRYCAN                                                  
         BNE   APAC27                                                           
         CLI   USERPROV,C'Y'       DID USER ENTER PROV?                         
         BE    APAC27              YES, NO NEED TO USE VENDOR'S                 
         L     R2,AINPTH           INPUT GST/PST ONLY                           
         CLI   8(R2),C'Y'                                                       
         BNE   APAC27                                                           
         L     R2,APROVH                                                        
         MVI   5(R2),2                                                          
         OI    6(R2),X'80'         MODIFY FOR NEXT INPUT                        
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6STRO        STEREO?                          
         BZ    APAC26Z                                                          
         CLC   VENDPROV,SPACES                                                  
         BNH   APAC27                                                           
APAC26Z  MVC   8(2,R2),VENDPROV                                                 
*                                                                               
APAC27   NI    POSTSW,X'FF'-AORINP                                              
         L     R2,AAORH                IF A/P ACCT HAS BEEN OVERRIDDEN          
         CLI   5(R2),0                 AND INPUT TO AOR BASIS FIELD             
         BE    BILLSRC                 NEED TO PUT THIS AMT IN GROSS            
         CLI   INPUT,55                                                         
         BNE   APAC28                                                           
         MVI   ERRNUM,INVALID                                                   
         B     ERROR                                                            
APAC28   ZIC   R3,5(R2)                                                         
         LA    R4,8(R2)                ON 1A ELEMENT FOR PAYABLE                
         BAS   R8,VALCASH              POSTING ONLY                             
         ZAP   DUB,0(8,R4)                                                      
         CP    DUB,=P'2100000000'                                               
         BH    AMTOOLGE                                                         
         CP    DUB,=P'-2100000000'                                              
         BL    AMTOOLGE                                                         
APAC30   CVB   RF,DUB                                                           
         STCM  RF,15,AORGRS                                                     
         OI    POSTSW,AORINP                                                    
*                                                                               
*                                                                               
BILLSRC  EQU   *                                                                
         L     R2,ABLSRH           BILLING SOURCE                               
         CLI   5(R2),0                                                          
         BE    DEPT10                                                           
         ZIC   R3,5(R2)                                                         
         LA    R4,8(R2)                                                         
         CLI   0(R4),C' '                                                       
         BL    ERROR                                                            
         LA    R4,1(R4)                                                         
         BCT   R3,*-12                                                          
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        VALIDATE DEPARTMENT AND STAFF                                          
*-------------------------------------------------------------------*           
DEPT10   DS    0H                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         XC    DEPTNUM,DEPTNUM                                                  
         XC    DEPTNAM,DEPTNAM                                                  
         XC    SVDEPT,SVDEPT                                                    
         L     R2,ADEPH                                                         
         CLI   5(R2),0                                                          
         BH    DEPT15                                                           
         TM    POSTSW,STAFPST                                                   
         BO    DEPT12                                                           
         TM    POSTSW,DEPTPST                                                   
         BZ    STAF10                                                           
DEPT12   MVI   ERRNUM,1                                                         
         B     ERROR                                                            
*                                                                               
DEPT15   DS    0H                                                               
         TM    POSTSW,STAFPST                                                   
         BO    DEPT19                                                           
         TM    POSTSW,DEPTPST                                                   
         BZ    DEPT20                                                           
DEPT19   OC    EXPACC,EXPACC                                                    
         BNZ   DEPT21                                                           
DEPT20   MVC   MSGNO,=Y(AE$ANFDP)                                               
         B     ERROR                                                            
*                                                                               
DEPT21   LA    R1,KEY+3                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2D'                                                  
         TM    COMPSTAT,X'20'                                                   
         BZ    DEPT22                                                           
         CLI   OFCLNGTH,0                                                       
         BE    DEPT22                                                           
         ZIC   RF,OFCLNGTH               VARIABLE OFFICE LENGTH                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),SVAOFF           MOVE IN OFFICE                         
         LA    R1,1(RF,R1)               BUMP PAST OFFICE IN KEY                
*                                                                               
DEPT22   OC    8(3,R2),SPACES            PADD WITH SPACES                       
         ZIC   RF,DPTLNGTH               VARIABLE DEPARTMENT LENGTH             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                      MOVE IN DEPARTMENT                     
         MVC   0(0,R1),8(R2)                                                    
         BAS   RE,GETACC                                                        
         MVC   DEPTNUM,ACCTNUM                                                  
         MVC   DEPTNAM,ACCTNAME                                                 
         MVC   SVDEPT,8(R2)                                                     
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'EXPACC),EXPACC                                             
         MVC   KEY+1(2),=C'28'                                                  
         BAS   RE,GETACC                                                        
         MVC   DSEXPNUM,ACCTNUM                                                 
         MVC   DSEXPNAM,ACCTNAME                                                
*                                                                               
STAF10   DS    0H                                                               
*                                                                               
         OC    EXPACC,EXPACC                                                    
         BZ    STAF12                                                           
         BAS   RE,GOCAT                                                         
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERROR                                                            
*                                                                               
STAF12   XC    STAFNUM,STAFNUM                                                  
         XC    STAFNAM,STAFNAM                                                  
         L     R2,ASTFH                                                         
         CLI   5(R2),0                                                          
         BH    STAF15                                                           
         TM    POSTSW,STAFPST                                                   
         BZ    EDGST                                                            
         MVI   ERRNUM,1                                                         
         B     ERROR                                                            
*                                                                               
STAF15   OC    EXPACC,EXPACC                                                    
         BZ    STAF20                                                           
         TM    POSTSW,STAFPST                                                   
         BO    STAF21                                                           
STAF20   MVC   MSGNO,=Y(AE$ANFAN)                                               
         B     ERROR                                                            
*                                                                               
STAF21   MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2P'                                                  
         LA    R1,KEY+3                                                         
         TM    COMPSTAT,X'20'                                                   
         BE    STAF40                                                           
         MVC   0(L'SVAOFF,R1),SVAOFF                                            
         ZIC   RF,OFCLNGTH               VARIABLE OFFICE LENGTH                 
         LA    R1,0(RF,R1)               BUMP PAST OFFICE IN KEY                
*                                                                               
STAF40   DS    0H                                                               
**T      TM    POSTSW,DEPTPST                                                   
**T      BZ    STAF45                                                           
         MVC   0(L'SVDEPT,R1),SVDEPT                                            
         ZIC   RF,DPTLNGTH               VARIABLE OFFICE LENGTH                 
         LA    R1,0(RF,R1)               BUMP PAST DEPT IN KEY                  
STAF45   ZIC   RF,5(R2)                  LENGTH OF STAFF INPUT                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),8(R2)             MOVE IN STAFF                          
         OC    KEY+1(L'KEY-1),SPACES                                            
         BAS   RE,GETACC                 VALIDATE ACCT                          
         MVC   STAFNUM,ACCTNUM           SAVE ACCT NUM                          
         MVC   STAFNAM,ACCTNAME               AND NAME                          
         EJECT                                                                  
EDGST    CLI   AGYCTRY,CTRYCAN     TEST CANADIAN                                
         BNE   AMTCHECK            NO NEED FOR GST/PST                          
*                                                                               
         MVI   ERRNUM,INVALID                                                   
         L     R2,AINPTH           INPUT GST FIELD                              
         CLI   5(R2),0                                                          
         BE    EDGST2                                                           
*                                                                               
         CLI   INPUT,55            TYPE 55 CANT ACCEPT GST INPUT                
         BE    ERROR                                                            
*                                                                               
         MVC   INPUTSW,8(R2)                                                    
         CLI   8(R2),C'Y'          TEST FOR INPUT GST ENTRY                     
         BE    EDGST1                                                           
         CLI   8(R2),C'N'                                                       
         BE    EDGST2                                                           
         B     ERROR                                                            
*                                                                               
EDGST1   L     R2,AAPH             FORCE ACCOUNT/PAYABLE OVERRIDE               
         MVI   ERRNUM,NOINPUT      FOR INPUT GST                                
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
*                                                                               
         L     R2,AAMTH                                                         
         MVI   ERRNUM,INVALID                                                   
         CP    AMT,=P'0'           FOR INPUT GST, BILL AMOUNT MUST = 0          
         BNE   ERROR                                                            
*                                                                               
EDGST2   BAS   RE,EDBASIS          EDIT BASIS FIELDS                            
         BNE   ERROR                                                            
*                                                                               
         CLI   PFKEY,7             CTAX PFKEY HIT?                              
         BNE   EDGST3              NO, GO THRU 1-PASS ROUTINE                   
         CLI   MODE,1              IN ROUTINE ALREADY?                          
         BNH   PREVAL30            NO, GO TO SCREEN                             
*                                                                               
EDGST3   MVI   MODE,4              SET UP FOR 1-PASS                            
         GOTO1 ACTAXMOD,DMCB,(RC)                                               
         CLI   CTXMODE,C'Z'                                                     
         BNE   AMTCHECK                                                         
         MVC   FVMSGNO,CTXMSGNO                                                 
         MVI   FVOMTYP,C'E'                                                     
         L     R2,FVADDR                                                        
         B     ERROR                                                            
         EJECT                                                                  
EDBASIS  NTR1                                                                   
         CLI   MODE,1              HAS TO BE IN TYPE 6 SCREEN                   
         BH    EDBASEXX                                                         
         MVI   ERRNUM,25           INVALID AMOUNT                               
         MVI   CTXMODE,C'G'        MODE TO LOOP AROUND FOR PST                  
*                                                                               
         LA    R4,BASIS                                                         
         L     R2,ABASEH                                                        
EDBASLP  CLI   CTXMODE,C'G'                                                     
         BE    *+12                                                             
         LA    R4,PBASIS                                                        
         L     R2,APBASEH                                                       
*                                                                               
         CLI   5(R2),0             ANY BASIS?                                   
         BE    EDBASEX             NEXT ONE, OR QUIT                            
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R3)                                  
         CLI   0(R1),X'FF'                                                      
         BE    EDBASERR                                                         
         ZAP   0(8,R4),4(8,R1)     SAVE IT                                      
*                                                                               
EDBASEX  CLI   CTXMODE,C'P'                                                     
         BE    EDBASEXX                                                         
         MVI   CTXMODE,C'P'                                                     
         B     EDBASLP                                                          
*                                                                               
EDBASEXX DS    0H                                                               
         SR    RE,RE                                                            
EDBASERR LTR   RE,RE                                                            
         XIT1  REGS=(R2)                                                        
*                                                                               
         EJECT                                                                  
AMTCHECK MVC   FVMSGNO,=Y(AE$INAMT)                                             
         MVI   FVOMTYP,C'E'                                                     
         L     R2,AAMTH            TO POSITION CURSOR                           
         ZAP   DUB,NET                                                          
         AP    DUB,COM                                                          
         AP    DUB,COM2            AMT = NET + COMMISSION + GST                 
         AP    DUB,GST                                                          
         AP    DUB,PST                                                          
         CLI   INPUT,42                                                         
         BNE   AMTCHKX                                                          
         AP    DUB,TAXA            FOR TYPE 42 -                                
         AP    DUB,TAXB            AMT = NET + COMM + TAX + TAX                 
AMTCHKX  CP    AMT,DUB                                                          
         BNE   ERROR                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        VALIDATE THE INCOME AND COSTING ACCOUNTS                               
*        THE VALID ENTRIES IN THIS FIELD ARE:                                   
*              (M1) (M1,CC) (M1,M2) (M1,M2,CC)                                  
*-------------------------------------------------------------------*           
VALACC   L     R2,ASYMH            SYSTEM/MEDIA                                 
         BAS   RE,ANY                                                           
         GOTO1 SCANNER,DMCB,(20,(R2)),(3,BLOCK)                                 
         MVI   ERRNUM,INVALID                                                   
         MVI   FNDX,1                                                           
         CLI   DMCB+4,0                                                         
         BE    EXIT                                                             
         SR    R4,R4                                                            
         IC    R4,DMCB+4           NUMBER OF INPUT FIELDS                       
         LA    R6,BLOCK                                                         
         USING SCAND,R6                                                         
         OC    8(L'CMBSYM,R2),SPACES   SPACE PAD THE FIELD                      
         MVI   MEDIANUM,0                                                       
         LA    R8,MEDIA1                                                        
*                                                                               
         USING POSTACD,R8                                                       
VALACC2  CLI   SCLEN2,0            FIRST MUST BE MEDIA                          
         BE    VALACC4             NO RIGHT SIDE                                
         CLI   SCLEN1,2                                                         
         BNE   ERROR                                                            
         CLC   SCDATA1(2),=C'MI'                                                
         BNE   VALACC8                                                          
         TM    COMPSTA4,X'10'      USING MEDIA INTERFACE RECORDS                
         BNO   ERROR                                                            
         CLI   MEDIANUM,2          ALREADY HAVE 2 INCOME ACCOUNTS               
         BE    ERROR                                                            
         CLI   SCLEN2,2                                                         
         BNE   ERROR                                                            
         MVC   MEDIA(2),SCDATA2                                                 
         B     VALACC6                                                          
*                                                                               
VALACC4  CLI   MEDIANUM,2                                                       
         BE    ERROR                                                            
         CLC   SCDATA1(3),=C'*SK'                                               
         BNE   VALACC5                                                          
         CLI   SCLEN1,15                                                        
         BH    ERROR                                                            
         CLI   INPUT,26                                                         
         BNE   ERROR                                                            
         MVC   INCNUM(1),COMPANY                                                
         MVC   INCNUM+1(14),SCDATA1+1                                           
         L     R2,AESTH                                                         
         MVI   ERRNUM,16                                                        
         BAS   RE,ANY                                                           
         BAS   RE,VALJOB                                                        
                                                                                
*        MVI   ERRNUM,NOINPUT      IF NO A/P ENTERED                            
*        L     R2,AAPH                                                          
*        CLI   5(R2),0                                                          
*        BNE   VALACC6                                                          
         L     R2,ANETH            NET AMOUNT                                   
         MVI   ERRNUM,INVAMNT      IF AMOUNT NOT ZERO                           
         CP    NET,=P'0'           AND NET NOT EQUAL TO ZERO                    
         BNE   ERROR               WE HAVE AN ERROR                             
         B     VALACC6                                                          
*                                                                               
VALACC5  CLI   SCLEN1,12                                                        
         BH    ERROR                                                            
         MVC   INCNUM(1),COMPANY                                                
         MVC   INCNUM+1(2),=C'SI'                                               
         MVC   INCNUM+3(12),SCDATA1                                             
*                                                                               
VALACC6  SR    R0,R0                                                            
         IC    R0,MEDIANUM                                                      
         AH    R0,=H'1'                                                         
         STC   R0,MEDIANUM                                                      
         LA    R8,POSTLNQ(R8)                                                   
         B     VALACC10                                                         
*                                                                               
VALACC8  CLC   SCDATA1(2),=C'CC'                                                
         BNE   ERROR                                                            
         CLI   SCLEN2,3                                                         
         BH    ERROR                                                            
         CLI   CC,0                                                             
         BNE   ERROR                                                            
         MVC   CC,SCDATA2                                                       
         CLI   SCDATA1+2,C' '                                                   
         BE    VALACC10                                                         
         TM    SCDATA1+2,X'F0'     THIRD CHARACTER IN CC FIELD                  
         BNO   ERROR               MUST BE NUMERIC                              
         MVC   CCDSP,SCDATA1+2     GET DISPLACEMENT IN BINARY                   
         NI    CCDSP,X'0F'                                                      
*                                                                               
VALACC10 LA    R6,SCLNQ(R6)        NEXT FIELD CAN BE CC=123                     
         SR    R0,R0                                                            
         IC    R0,FNDX                                                          
         AH    R0,=H'1'                                                         
         STC   R0,FNDX                                                          
         BCT   R4,VALACC2                                                       
*                                                                               
         MVI   FNDX,0                                                           
         CLC   SET,MEDIANUM        MUST HAVE THE SAME NUMBER OF INCOME          
         BNE   ERROR               ACCOUNTS AS COMMISSION AMOUNTS               
         LA    R8,MEDIA1           DO FIRST INCOME ACCOUNT                      
         BAS   RE,SYSM             VALIDATE ALL ACCOUNTS                        
         CLI   ERRNUM,0                                                         
         BNE   SYSMERR                                                          
*                                                                               
         CLI   MEDIANUM,1                                                       
         BE    VALACC12            ONLY ONE INCOME ACCOUNT                      
         LA    R8,MEDIA2           DO SECOND INCOME ACCOUNT                     
         BAS   RE,SYSM                                                          
         CLI   ERRNUM,0                                                         
         BNE   SYSMERR                                                          
*                                                                               
VALACC12 LA    R8,MEDIA1                                                        
         MVC   WORK(36),INCNAM     PUT OUT INCOME ACCOUNT NAME                  
         MVC   WORK+36(37),SPACES                                               
         LA    R8,MEDIA2                                                        
         CLI   INCNAM,0                                                         
         BE    *+14                                                             
         MVI   WORK+36,C'/'                                                     
         MVC   WORK+37(36),INCNAM                                               
         GOTO1 SQUASHER,DMCB,WORK,73                                            
         L     R2,ASYMNH                                                        
         MVC   FLD(L'WORK),WORK                                                 
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   INPUT,42                                                         
         BNE   VALACC19                                                         
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        VALIDATE TAX ACCOUNTS                                                  
*-------------------------------------------------------------------*           
         CLI   CMBTAXAH+5,0        TAX ENTERED                                  
         BNE   VALACC14            THEN VALIDATE ACCOUNT                        
         MVC   CMBTACA,SPACES                                                   
         OI    CMBTACAH+6,X'80'                                                 
         MVC   CMBTNMA,SPACES                                                   
         OI    CMBTNMAH+6,X'80'                                                 
         B     VALACC16                                                         
*                                                                               
VALACC14 LA    R2,CMBTACAH         FOR INPUT TYPE 42 ONLY                       
         BAS   RE,ANY                                                           
         XR    R6,R6                                                            
         OC    CMBTACA,SPACES                                                   
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SB'     ALWAYS SB                                    
         MVC   KEY+3(12),CMBTACA                                                
         MVC   KEY+15(27),SPACES                                                
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   TAXANUM,ACCTNUM     SAVE FOR POSTING                             
         MVC   TAXANAME,ACCTNAME                                                
         MVC   CMBTNMA,ACCTNAME                                                 
         OI    CMBTNMAH+6,X'80'                                                 
*                                                                               
VALACC16 CLI   CMBTAXBH+5,0        TAX ENTERED                                  
         BNE   VALACC18            THEN VALIDATE ACCOUNT                        
         MVC   CMBTACB,SPACES                                                   
         OI    CMBTACBH+6,X'80'                                                 
         MVC   CMBTNMB,SPACES                                                   
         OI    CMBTNMBH+6,X'80'                                                 
         B     VALACC19                                                         
*                                                                               
VALACC18 LA    R2,CMBTACBH         FOR INPUT TYPE 42 ONLY                       
         BAS   RE,ANY                                                           
         XR    R6,R6                                                            
         OC    CMBTACB,SPACES                                                   
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SB'                                                  
         MVC   KEY+3(12),CMBTACB                                                
         MVC   KEY+15(27),SPACES                                                
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   TAXBNUM,ACCTNUM     SAVE FOR POSTING                             
         MVC   TAXBNAME,ACCTNAME                                                
         MVC   CMBTNMB,ACCTNAME                                                 
         OI    CMBTNMBH+6,X'80'                                                 
VALACC19 B     POSTING                                                          
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              SYSTEM/MEDIA VALIDATION                                          
*              SET=1, OR 2   THE FIRST OR SECOND MEDIA ACCOUNTS                 
*-------------------------------------------------------------------*           
         USING POSTACD,R8                                                       
SYSM     NTR1                                                                   
         CLI   MEDIA,0                                                          
         BE    SYSM04                                                           
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'08'           GET MEDIA INTERFACE RECORD                   
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(2),MEDIA                                                   
         BAS   RE,HIGH                                                          
         MVI   ERRNUM,INVACC                                                    
         CLC   KEYSAVE,KEY                                                      
         BNE   SYSMERR             RECORD NOT FOUND                             
         MVI   ELCODE,X'19'                                                     
         LA    R4,KEY                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACMID,R4                                                         
         MVC   DESCRIPT,ACMIDESC   DESCRIPTION                                  
         MVC   COSCONUM(1),COMPANY                                              
         MVC   COSCONUM+1(2),=C'12'                                             
         MVC   COSCONUM+3(12),ACMICOST   SAVE COSTING ACCOUNT                   
         MVC   PAYNUMBR+1(14),ACMICNTL   AND MEDIA CONTROL                      
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACMICOMM        SET INCOME ACCOUNT                     
****     MVC   COSBLNUM,COSCONUM                                                
         B     SYSM06                                                           
*                                                                               
SYSM04   MVC   KEY,SPACES                                                       
         MVC   KEY(15),INCNUM                                                   
*                                                                               
SYSM06   L     R2,ASYMH                                                         
         LA    R6,WORK             FORCE READ OF INCOME ACCOUNT                 
         BAS   RE,GETACC           GET THE RECORD                               
         SR    R6,R6                                                            
         BAS   RE,CHECKACC         CHECK FOR POSTING                            
         MVC   INCNUM,KEY          SAVE THE INCOME KEY                          
         MVC   INCNAM,ACCTNAME     ACCOUNT NAME                                 
*                                                                               
         CLI   MEDIA,0             MEDIA INTERFACE RECORDS                      
         BNE   SYSM09              ALREADY HAVE COST ACCOUNT                    
*                                                                               
         CLC   INCNUM+1(2),=C'SK'                                               
         BE    SYSMX                                                            
*                                                                               
         MVC   COSCONUM,SPACES                                                  
         MVC   COSCONUM(1),COMPANY                                              
         MVC   COSCONUM+1(2),=C'12'                                             
         MVC   COSCONUM+3(1),ACCTCOST  COSTING COMMISSION CODE                  
****     MVC   COSBLNUM,COSCONUM                                                
         MVI   ELCODE,X'2C'                                                     
         L     R4,AIOAREA                                                       
         BAS   RE,GETEL                                                         
*                                                                               
SYSM08   BNE   SYSM09                                                           
         USING ACSPECD,R4                                                       
         CLI   ACSPTYP,ACSPOAN                                                  
         BE    *+12                                                             
         BAS   RE,NEXTEL                                                        
         B     SYSM08                                                           
         MVC   COSCONUM+3(12),ACSPACCT                                          
*                                                                               
         USING RSTELD,R4                                                        
SYSM09   L     R4,AIOAREA                                                       
         MVI   ELCODE,RSTELQ                                                    
         XC    SVSYSMED,SVSYSMED                                                
         BAS   RE,GETEL                                                         
         BNE   SYSM10                                                           
         MVC   SVSYSMED,RSTSYSME                                                
*                                                                               
SYSM10   LA    R4,PROFILE                                                       
         USING ACPROFD,R4                                                       
         MVC   CLICOST,ACPRCOST   CLIENT COSTING                                
         TM    POSTSW,STAFPST                                                   
         BZ    SYSM10C                                                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'29'                                                  
         MVC   KEY+3(12),CLICOST+3                                              
         L     R2,ACLIH                                                         
         BAS   RE,GETACC                                                        
         MVC   PCLINUM,ACCTNUM           SAVE ACCT NUM                          
         MVC   PCLINAM,ACCTNAME               AND NAME                          
*                                                                               
SYSM10C  TM    COSTSW,ANYCOST                                                   
         BZ    SYSM10E                                                          
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'1C'                                                  
         MVC   KEY+3(12),CLICOST+3                                              
         BAS   RE,GETACC                                                        
         MVC   PCOSNUM,ACCTNUM           SAVE ACCT NUM                          
         MVC   PCOSNAM,ACCTNAME               AND NAME                          
*                                                                               
         MVC   KEY+3(12),SPACES                                                 
         MVC   KEY+5(1),ANALBYTE                                                
         LA    R1,KEY+3                                                         
         CLC   COUNTRY,=C'UK'                                                   
         BE    B14F                                                             
         TM    COMPSTAT,X'20'                                                   
         BZ    B14F                                                             
         CLI   OFCLNGTH,0                                                       
         BE    B14F                                                             
         ZIC   RF,OFCLNGTH               VARIABLE OFFICE LENGTH                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),SVAOFF           MOVE IN OFFICE                         
         LA    R1,1(RF,R1)               BUMP PAST OFFICE IN KEY                
B14F     LA    RF,=C'9999'                                                      
         TM    POSTSW,DEPTPST                                                   
         BZ    *+8                                                              
         LA    RF,SVDEPT                                                        
         ZIC   R3,DPTLNGTH               VARIABLE DEPARTMENT LENGTH             
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                      MOVE IN DEPARTMENT                     
         MVC   0(0,R1),0(RF)                                                    
         LA    R1,1(R3,R1)                                                      
         MVC   0(1,R1),ANALBYTE                                                 
B15      MVC   KEY+1(2),=C'1P'                                                  
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST TEST NEW COSTING                   
         BNO   *+10                                                             
         MVC   KEY+3(12),=C'999999999999' SAYS VANESSA                          
         BAS   RE,GETACC                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    BADACC                                                           
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    BADACC                                                           
         MVC   CNEXPNUM,ACCTNUM                                                 
         MVC   CNEXPNAM,ACCTNAME                                                
*                                                                               
SYSM10E  LA    R1,CLICOST                                                       
         CLI   CC,0                WAS CLIENT COST OVERRIDE ENTERED ?           
         BE    SYSM12              NO, LOOK FOR STATUS                          
         CLC   MEDIANUM,SET        BYPASS IF NOT AT CORRECT MEDIA               
         BNE   SYSM12                                                           
         CLI   CCDSP,0             START POSITION  NOT SPECFIED                 
         BE    SYSM11              DEFAULT IS 7                                 
         SR    R3,R3                                                            
         IC    R3,CCDSP                                                         
         LA    R4,2(R3,R1)          START AT LEDGER                             
         B     *+8                                                              
*                                                                               
SYSM11   LA    R4,7(0,R1)          R4 POINTS TO POSITION IN COST KEY            
         LA    R3,3                MAX NUMBER OF ENTRIES                        
         LA    R6,CC               R6 POINTS TO OVERRIDE DATA                   
         B     SYSM15                                                           
*                                                                               
SYSM12   L     R6,AIOAREA          INCOME ACCOUNT IN IOAREA                     
         AH    R6,DATADISP                                                      
*                                                                               
SYSM13   CLI   0(R6),0                                                          
         BE    SYSM16                                                           
         CLI   0(R6),ACSTELQ       STATUS ELEMENT                               
         BE    SYSM14                                                           
         ZIC   R4,1(R6)                                                         
         AR    R6,R4                                                            
         B     SYSM13                                                           
*                                                                               
         USING ACSTATD,R6                                                       
SYSM14   OC    ACSTCNTR,SPACES                                                  
         CLI   ACSTCPOS,0          IF 0 START AT AT KEY+7                       
         BE    *+20                                                             
         LA    R4,2(R1)            ADDR OF KEY+2                                
         ZIC   R0,ACSTCPOS         STARTING POINT NUMBER                        
         AR    R4,R0               NEW STARTING POINT                           
         B     *+8                                                              
         LA    R4,7(R1)            CLIENT COSTING KEY+7                         
         LA    R3,3                                                             
         LA    R6,ACSTCNTR                                                      
*                                                                               
SYSM15   CLI   0(R6),C' '          REPLACE ONLY NON BLANK                       
         BE    *+10                                                             
         MVC   0(1,R4),0(R6)                                                    
         LA    R6,1(R6)                                                         
         LA    R4,1(R4)                                                         
         BCT   R3,*-22                                                          
*                                                                               
SYSM16   TM    COMPSTAT,X'10'      COSTING REQUIRED                             
         BZ    SYSM17              NO SKIP COSTING                              
*                                                                               
         MVC   KEY,SPACES          NO CHECK COMMISSION ACCT                     
         MVC   KEY(15),COSCONUM                                                 
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   COSCONAM,ACCTNAME   COMMISSION ACCOUNT NAME                      
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),CLICOST     CHECK CLIENT COSTING ACCOUNT                 
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CLICTNM,ACCTNAME    CLIENT COST ACCOUNT NAME                     
*                                                                               
         LA    R1,MEDIA1                                                        
         CR    R1,R8               IF NOT FIRST ACCOUNT GET OUT                 
         BNE   SYSMX                                                            
         MVC   KEY,SPACES                                                       
         MVC   COSBLNUM,COSCONUM   CHECK COSTING BILLING                        
         MVI   COSBLNUM+2,C'1'                                                  
         MVC   KEY(15),COSBLNUM                                                 
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   COSBLNAM,ACCTNAME                                                
*                                                                               
SYSM17   CP    NET,=P'0'           IF NET IS ZERO NO                            
         BE    SYSMX               PAYABLE POSTING                              
         L     RE,AAPH                                                          
         CLI   5(RE),0             IF USING PAYABLE OVERRIDE                    
         BNE   SYSMX               NO NEED TO VERIFY SZ(SAYS VANESSA)           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SZ'                                                  
         MVC   KEY+3(12),INCNUM+3                                               
         CLI   PAYNUMBR+1,0        DO WE HAVE MEDIA CNTRL FROM MI REC.          
         BE    *+10                                                             
         MVC   KEY+1(14),PAYNUMBR+1  IF WE DO - USE IT                          
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC         CHECK MEDIA/CONTROL                          
         MVC   PAYNUMBR,KEY                                                     
         MVC   PAYNAM,ACCTNAME                                                  
*                                                                               
SYSMX    MVI   ERRNUM,0            NO ERRORS                                    
*                                                                               
SYSMERR  XMOD1 1                                                                
         DROP  R8                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
POSTING  LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,DLDSELQ                                                   
         L     R2,ABILH                                                         
         BAS   RE,ANY                                                           
         CLI   INPUT,55            FOR INCOME ACCRUAL                           
         BNE   POST02                                                           
         CLI   5(R2),6             REFERENCE CAN'T BE MORE THAN 5               
         BL    POST02                                                           
         MVI   ERRNUM,INVALID                                                   
         B     ERROR                                                            
*                                                                               
POST02   MVC   DLDSREF,SPACES                                                   
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DLDSREF(0),8(R2)                                                 
         CLI   INPUT,55                                                         
         BNE   *+8                                                              
         MVI   DLDSREF+5,C'A'      ACCRUAL MUST BE AN 'A'                       
*                                                                               
         MVC   DLDSDATE,PDAT                                                    
         MVI   DLDSSBRF,0                                                       
         XC    DLDSSTAT(7),DLDSSTAT                                             
*                                                                               
         CLI   INPUT,42                                                         
         BNE   *+12                                                             
         LA    R2,CMBNARYH         FOR INPUT TYPE 42 ONLY                       
         B     *+8                                                              
*                                                                               
         L     R2,ANARH                                                         
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         BAS   RE,NARRSCAN                                                      
*                                                                               
         ST    R6,FULL                                                          
         LA    R6,DLDSNARR                                                      
         SR    R6,R8               ELEMENT - NARRATIVE                          
         AH    R6,=H'2'                                                         
         A     R6,FULL             R6 = L'NARRATIVE                             
         STH   R6,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         SH    R6,=H'2'                                                         
         STH   R6,HALF                                                          
         MVC   DLDSLEN,HALF+1                                                   
         ZIC   R3,DLDSLEN                                                       
         EJECT                                                                  
*-----------------------------------------------------------------*             
*              DEAL WITH SPECIAL ELEMENTS                                       
*-----------------------------------------------------------------*             
         AR    R8,R3                                                            
         ST    R8,SAVR8                                                         
         XC    EL1A,EL1A                                                        
         MVC   EL23,SPACES                                                      
         MVC   EL27,SPACES                                                      
POST04   LA    R6,EL1A                                                          
         USING ACMTD,R6                                                         
         MVC   ACMTMOS,ADVMOS      ADV MOS  OR BATCH MOS                        
         OC    ACMTMOS,ACMTMOS     TO MT ELEMENT                                
         BNZ   *+10                                                             
         MVC   ACMTMOS,PMOS                                                     
         L     R2,ADSPH                                                         
         MVC   ACMTDSCP,8(R2)      DESCRIPTION                                  
         OC    ACMTDSCP,SPACES                                                  
         L     R2,AESTH                                                         
         CLI   5(R2),0                                                          
         BE    POST12              NO ESTIMATE                                  
         CLI   5(R2),3             IS IT ESTIMATE OR JOB                        
         BH    POST10                                                           
         MVC   ACMTEST(3),8(R2)       ESTIMATE NUMBER                           
         OC    ACMTEST,SPACES                                                   
         B     POST12                                                           
*                                                                               
POST10   MVC   ACMTJOB,8(R2)       JOB NUMBER                                   
         OC    ACMTJOB,SPACES                                                   
*                                                                               
POST12   LA    R6,EL23                                                          
         USING ACOTHERD,R6                                                      
         OC    ADVMOS,ADVMOS       ADD MOS TO OTHER ELEMENT                     
         BZ    POST14                                                           
         MVC   ACOTDATE,ADVMOS                                                  
         L     RE,APROH                                                         
         MVC   ACOTNUM(3),8(RE)      PRODUCT                                    
         MVC   ACOTNUM+3(6),8(R2)    ESTIMATE                                   
         OC    ACOTNUM,SPACES                                                   
*                                                                               
POST14   L     R2,ANUMH                                                         
         CLI   5(R2),0             ANYTHING INPUT                               
         BE    POST50                                                           
*                                                                               
         GOTO1 SCANNER,DMCB,(20,(R2)),(10,BLOCK)                                
         MVI   ERRNUM,INVALID                                                   
         MVI   FNDX,1                                                           
         CLI   DMCB+4,0                                                         
         BE    POSTERR                                                          
         ZIC   R4,DMCB+4           NUMBER OF INPUT FIELDS                       
         LA    R8,BLOCK                                                         
         USING SCAND,R8                                                         
         USING ACOTHERD,R6                                                      
*                                                                               
POST16   MVI   ERRNUM,INVALID                                                   
         CLC   SCDATA1(2),=C'SN'   SPECIAL NUMBER                               
         BE    POST18                                                           
         CLC   SCDATA1(2),=C'E2'   ESTIMATE 2                                   
         BNE   POST26                                                           
*                                                                               
POST18   CLI   SCLEN2,20           MAXIMUM 9 CHARS.                             
         BH    POSTERR                                                          
         ZIC   RF,SCLEN2                                                        
         LTR   RF,RF                                                            
         BZ    POSTERR                                                          
*                                                                               
         LA    R6,ELA2             BUILD A A2 USER ELEMENT                      
POST20   CLI   0(R6),0             EMPTY SPACE                                  
         BE    POST22                                                           
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     POST20                                                           
*                                                                               
         USING ACUFD,R6                                                         
POST22   MVI   ACUFEL,ACUFELQ                                                   
         LA    R1,32(RF)           LENGTH IS 32 PLUS INPUT                      
         STC   R1,ACUFLEN          SET LENGTH                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACUFDATA(0),SCDATA2 VARIABLE DATA                                
         MVI   ACUFSEQ,1           SEQUENCE                                     
         CLC   SCDATA1(2),=C'SN'                                                
         BNE   POST24                                                           
         MVC   ACUFCODE,=C'SN'     CODE                                         
         MVC   ACUFDESC,=CL12'SPECIAL NO. '                                     
         B     *+16                                                             
*                                                                               
POST24   MVC   ACUFCODE,=C'E2'     CODE                                         
         MVC   ACUFDESC,=CL12'ESTIMATE 2  '                                     
         MVI   ACUFMXLN,X'14'                                                   
         B     POST48                                                           
*                                                                               
POST26   CLI   INPUT,26            TYPE 26 ONLY                                 
         BNE   POST48                                                           
         CLC   SCDATA1(2),=C'DA'   SPECIAL DATE FOR RECEIVABLES                 
         BNE   POST28                                                           
         L     RE,AMTHH                                                         
         CLI   5(RE),0             CAN'T HAVE DA=                               
         BNE   POSTERR             AND ADV MONTH                                
         USING ACOTHERD,R6                                                      
         LA    R6,EL23                                                          
         MVI   ERRNUM,INVDATE                                                   
         GOTO1 DATVAL,DMCB,(2,SCDATA2),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    POSTERR                                                          
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   ACOTDATE,WORK+6                                                  
         B     POST48                                                           
*                                                                               
POST28   CLC   SCDATA1(3),=C'DES'            ESTIMATE DESCRIPTION FOR           
         BNE   POST30                        '1A' ELEM                          
         L     RE,ADSPH                                                         
         CLI   5(RE),0                       CAN'T HAVE IN BOTH                 
         BNE   POSTERR                                                          
         USING ACMTD,R6                                                         
         LA    R6,EL1A                                                          
         ZIC   RF,SCLEN2                                                        
         LTR   RF,RF                                                            
         BZ    POSTERR                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     POST48                                                           
         MVC   ACMTDSCP(0),SCDATA2                                              
*                                                                               
POST30   CLC   SCDATA1(3),=C'JOB'            JOB NUMBER FOR '1A' ELEM           
         BNE   POST32                                                           
         CLI   SCLEN2,L'ACMTJOB                                                 
         BH    POSTERR                                                          
         L     RE,AESTH                                                         
         CLI   5(RE),0             CAN'T HAVE JOB                               
         BH    POSTERR                                                          
         ZIC   RF,SCLEN2                                                        
         LTR   RF,RF                                                            
         BZ    POSTERR                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     POST48                                                           
         MVC   ACMTJOB(0),SCDATA2                                               
*                                                                               
         USING ACABILLD,R6                                                      
POST32   LA    R6,EL27                                                          
         CLC   SCDATA1(2),=C'EA'   EA NUMBER                                    
         BNE   POST38                                                           
         MVI   ERRNUM,6            INVALID FORMAT                               
         CLI   SCLEN2,10           HARD CODE FOR LENGTH OF 10                   
         BNE   POST34              5-2-1                                        
         CLI   SCDATA2+5,C'-'      AND FOR HYPHEN PLACEMENT                     
         BNE   POSTERR                                                          
         CLI   SCDATA2+8,C'-'                                                   
         BNE   POSTERR                                                          
         B     POST36                                                           
*                                                                               
POST34   CLI   SCLEN2,11           OR 11 6-2-1                                  
         BNE   POSTERR                                                          
         CLI   SCDATA2+6,C'-'      AND FOR HYPHEN PLACEMENT                     
         BNE   POSTERR                                                          
         CLI   SCDATA2+9,C'-'                                                   
         BNE   POSTERR                                                          
*                                                                               
POST36   SR    R1,R1                                                            
         IC    R1,SCLEN2                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     POST48                                                           
         MVC   ACABEANO(0),SCDATA2      MOVE IN EA NUMBER                       
*                                                                               
POST38   CLC   SCDATA1(2),=C'AC'   ACCOUNT NUMBER                               
         BNE   POST44                                                           
         CLI   SCLEN2,13           HARD CODE FOR LENGTH OF 13                   
         BNE   POSTERR                                                          
         MVC   WORK(L'ACABACNO),=20X'F0' CHECK FOR ALL NUMERIC                  
         MVI   ERRNUM,6            INVALID FORMAT                               
         CLI   SCDATA2+4,C'-'      AND FOR HYPHEN PLACEMENT                     
         BNE   POSTERR                                                          
         MVZ   WORK(4),SCDATA2                                                  
         CLI   SCDATA2+9,C'-'      SCHEME 1                                     
         BNE   POST40                                                           
         MVZ   WORK+4(4),SCDATA2+5                                              
         MVZ   WORK+8(3),SCDATA2+10                                             
         B     POST42                                                           
*                                                                               
POST40   CLI   SCDATA2+8,C'-'      SCHEME 2                                     
         BNE   POSTERR                                                          
         MVZ   WORK+4(3),SCDATA2+5                                              
         MVZ   WORK+7(4),SCDATA2+9                                              
*                                                                               
POST42   MVI   ERRNUM,INVNUM                                                    
         CLC   WORK(L'ACABACNO),=20X'F0'                                        
         BNE   POSTERR                                                          
         MVC   ACABACNO(13),SCDATA2 HARD CODE FOR MOVE                          
         B     POST48                                                           
*                                                                               
POST44   CLC   SCDATA1(3),=C'ENO'  ESTIMATE NUMBER                              
         BNE   POST46                                                           
         CLI   SCLEN2,L'ACABESNO   CAN BE ANYTHING                              
         BH    POSTERR                                                          
         ZIC   RF,SCLEN2                                                        
         LTR   RF,RF                                                            
         BZ    POSTERR                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACABESNO(0),SCDATA2                                              
         B     POST48                                                           
*                                                                               
         USING ACABILLD,R6                                                      
POST46   CLC   SCDATA1(3),=C'BUD'  BUDGET NUMBER                                
         BNE   POSTERR                                                          
         CLI   SCLEN2,L'ACABBUNO   MUST BE FULL LENGTH                          
         BNE   POSTERR                                                          
         MVI   ERRNUM,INVNUM                                                    
         TM    SCVAL2,X'80'        AND MUST BE NUMERIC                          
         BZ    POSTERR                                                          
         MVC   ACABBUNO(L'ACABBUNO),SCDATA2                                     
*                                                                               
POST48   LA    R8,SCLNQ(R8)        BUMP TO NEXT FIELD                           
         ZIC   RE,FNDX             INCREMENT FIELD NUMBER                       
         LA    RE,1(RE)                                                         
         STC   RE,FNDX                                                          
         BCT   R4,POST16           PROCESS NEXT FIELD                           
*                                                                               
         USING ACOTHERD,R6         DSECT TO COVER '23' ELEM                     
POST50   LA    R6,EL23             VALIDATE OTHERS ELEMENT                      
         CLC   ACOTNUM,SPACES      NUMBER IS REQUIRED IF DATE ENTERED           
         BNE   POST52                                                           
         CLC   ACOTDATE,SPACES                                                  
         BNE   ERR23                                                            
         B     POST56                                                           
*                                                                               
POST52   MVC   ACOTEL(2),=X'230F'  FINISH BUILDING IT                           
         L     R2,ASYMH                                                         
         MVC   ACOTPROF(1),8(R2)                                                
         CLI   8(R2),C'S'          SPOT                                         
         BE    POST54                                                           
         CLI   8(R2),C'P'          PRINT                                        
         BE    POST54                                                           
         MVI   ACOTPROF,C'X'                                                    
*                                                                               
POST54   L     R8,SAVR8                                                         
         MVC   0(L'EL23,R8),EL23                                                
         IC    R3,ACOTLEN                                                       
         AR    R8,R3                                                            
*                                                                               
POST56   CLC   EL27,SPACES         ANYTHING INPUT                               
         BE    POST58                                                           
         LA    R6,EL27             YES, FILL IN REST OF ELEMENT                 
         USING ACABILLD,R6                                                      
         MVI   ACABEL,X'27'                                                     
         MVI   ACABLEN,L'EL27                                                   
         CLC   ACABESNO(L'ACABESNO+L'ACABBUNO),SPACES                           
         BNE   *+8                                                              
         MVI   ACABLEN,X'1E'       SMALL EL. IF NO ESNO OR BUNO                 
         MVC   0(L'EL27,R8),EL27                                                
         IC    R3,ACABLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         USING ACMTD,R6                                                         
POST58   LA    R6,EL1A                                                          
         MVI   ACMTEL,ACMTELQ            ELEM CODE                              
         MVI   ACMTLEN,ACMTLNQ           ELEM LENGTH                            
         OC    SVSYSMED,SVSYSMED                                                
         BNZ   POST60                                                           
         L     RE,ASYMH                                                         
         MVC   ACMTSYS(2),8(RE)          SYSTEM/MEDIA                           
         CLC   8(3,RE),=C'MI='           IS IT AN MI RECORD                     
         BNE   *+10                                                             
         MVC   ACMTSYS(2),11(RE)         GET MI SYSTEM/MEDIA                    
         B     *+10                                                             
*                                                                               
POST60   MVC   ACMTSYS(2),SVSYSMED                                              
         L     RE,ACLIH                                                         
         MVC   ACMTCLI,8(RE)             CLIENT                                 
         OC    ACMTCLI,SPACES                                                   
         L     RE,APROH                                                         
         MVC   ACMTPRD,8(RE)             PRODUCT                                
         OC    ACMTPRD,SPACES                                                   
*                                                                               
         CVB   RF,SPL                    GROSS BILLING                          
         STCM  RF,15,ACMTGRS                                                    
         CVB   RF,NET                    NET BILLING                            
         STCM  RF,15,ACMTNET                                                    
         CVB   RF,COM                    COMMISSION                             
         STCM  RF,15,ACMTCOM                                                    
         CVB   RF,CD                     CASH DISCOUNT                          
         STCM  RF,15,ACMTCD                                                     
         CVB   RF,AMT                    RECEIVABLE                             
         STCM  RF,15,ACMTRECV                                                   
*                                                                               
         CLI   INPUTSW,C'Y'        TEST INPUT GST                               
         BE    *+12                YES-SKIP SETTING GST AMOUNT                  
         CVB   RF,GST                    GST                                    
         STCM  RF,15,ACMTVAT                                                    
*                                                                               
         L     R8,SAVR8                                                         
         MVC   0(L'EL1A,R8),EL1A         ADD ELEM TO REC                        
         IC    R3,ACMTLEN                                                       
         AR    R8,R3                                                            
         ST    R8,SAVR8                                                         
         CLI   ELA2,0              ANY A2 ELEMENT                               
         BE    POST64                                                           
         LA    R6,ELA2                                                          
*                                                                               
         USING ACUFD,R6                                                         
POST62   CLI   0(R6),0                                                          
         BE    POST64                                                           
         IC    R3,ACUFLEN                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),ACUFEL                                                   
         LA    R8,1(R3,R8)                                                      
         ST    R8,SAVR8                                                         
         ZIC   R3,1(R6)                                                         
         AR    R6,R3                                                            
         B     POST62                                                           
*                                                                               
         USING TRCASHD,R8                                                       
POST64   L     R8,SAVR8                                                         
         CP    CD,=P'0'                                                         
         BE    POST66                                                           
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         ZAP   TRCSAMNT,CD                                                      
         IC    R3,TRCSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         USING TRCASHD,R8                                                       
POST66   MVI   TRCSEL,TRCSELQ      INCOME TO RECEIVABLE                         
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'I'       POSTING                                      
         ZAP   TRCSAMNT,COM                                                     
         IC    R3,TRCSLEN                                                       
         AR    R8,R3                                                            
         CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   *+8                 NO                                           
         BAS   RE,PTAXES           POST TAX MEMOS FOR RECEIVABLE ACCT           
*                                                                               
         USING TRDUED,R8                                                        
         OC    DUEDAT,DUEDAT       IF DUEDATE CREATE A 61                       
         BZ    POST68                                                           
         MVC   TRDUEL(2),=X'6104'                                               
         GOTO1 DATCON,DMCB,(0,DUEDAT),(2,TRDUDATE)                              
         IC    R3,TRDUEN                                                        
         AR    R8,R3                                                            
                                                                                
POST68   BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT RECEIVABLES                            
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,ARNUM                                                   
         MVC   DLPSDBNM,ARNAM                                                   
         MVC   DLPSCRAC,SPACES                                                  
         LA    R2,DLPSCRAC+3                                                    
         LA    RF,MEDIA1                                                        
         USING POSTACD,RF                                                       
         GOTO1 CHOPPER,DMCB,(36,INCNAM),(12,(R2)),(0,1)                         
         OC    DESCRIPT,DESCRIPT   DESCRIPTION FROM MI RECORD                   
         BZ    *+10                                                             
         MVC   DLPSCRAC+3(12),DESCRIPT                                          
*                                                                               
         L     R2,ABLSRH           BILLING SOURCE                               
         CLI   5(R2),0                                                          
         BE    POST70                                                           
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         MVC   DLPSCRAC+3(12),SPACES                                            
         EX    R3,*+8                                                           
         B     POST70                                                           
         MVC   DLPSCRAC+3(0),8(R2)                                              
*                                                                               
POST70   MVC   DLPSCRNM,SPACES     SO NAME IS SPACES                            
         MVI   DLPSTYPE,0          SUBSIDIARY                                   
         ZAP   DLPSAMNT,AMT                                                     
         MVC   DLPSANAL,SVFOFF                                                  
         CLC   DLPSDBAC+1(2),=C'SX'     IF A/R ACCOUNT SX MAKE IT               
         BNE   POST72                   A MINUS CREDIT                          
         MVI   DLPSEL,DLPSECRQ                                                  
         MP    DLPSAMNT,=P'-1'                                                  
         MVC   DLPSDBAC,DLPSCRAC                                                
         MVC   DLPSDBNM,DLPSCRNM                                                
         MVC   DLPSCRAC,ARNUM                                                   
         MVC   DLPSCRNM,ARNAM                                                   
*                                                                               
POST72   ZIC   R3,DLPSLEN          ADVANCE POSTING POINTER                      
         AR    R8,R3                                                            
         CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   POST74              NO                                           
         CLI   GSTSW,C'Y'                                                       
         BNE   POST74                                                           
         BAS   RE,POSTGST                                                       
         BAS   RE,KEEPTAX          KEEP TAX MEMOS IF INPUT<>Y                   
*                                                                               
         USING TRCASHD,R8                                                       
POST74   MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'G'       GROSS                                        
         ZAP   TRCSAMNT,SPL        SPECIAL AMOUNT                               
         IC    R3,TRCSLEN                                                       
         AR    R8,R3                                                            
         USING ACMTD,R8                                                         
         MVC   0(L'EL1A,R8),EL1A                                                
         IC    R3,ACMTLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
                                                                                
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT INCOME                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,PRODNUM    CONTRA IS CLIENT PRODUCT                     
         MVC   DLPSDBNM,PRODNAM                                                 
         LA    RF,MEDIA1                                                        
         CLC   INCNUM+1(2),=C'SK'                                               
         BNE   *+16                                                             
         MVC   DLPSDBAC,JOBNUM     BUT CLIENT, PRODUCT & JOB FOR SK.            
         MVC   DLPSDBNM,JOBNAM                                                  
         MVC   DLPSCRAC,INCNUM                                                  
         MVC   DLPSCRNM,INCNAM                                                  
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,COM                                                     
         MVC   DLPSANAL,SVFOFF                                                  
         MVC   TEMPSAV(DLPSLNQ),0(R8)                                           
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         CLI   INPUT,42                                                         
         BNE   POST78                                                           
*                                                                               
*---------------------------------------------------------------------*         
*              POSTINGS FOR TAXES                                               
*---------------------------------------------------------------------*         
         CLI   CMBTAXAH+5,0        FOR INPUT TYPE 42 ONLY                       
         BE    POST76                                                           
         BAS   RE,ADDANL                                                        
         MVC   0(DLPSLNQ,R8),TEMPSAV                                            
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,X'6A'    CREDIT INCOME                                    
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,PRODNUM    CONTRA IS CLIENT PRODUCT                     
         MVC   DLPSDBNM,PRODNAM                                                 
         MVC   DLPSCRAC,TAXANUM                                                 
         MVC   DLPSCRNM,TAXANAME                                                
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,TAXA                                                    
         MVC   DLPSANAL,SVFOFF                                                  
         MVC   TEMPSAV(DLPSLNQ),0(R8)                                           
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
POST76   CLI   CMBTAXBH+5,0        FOR INPUT TYPE 42 ONLY                       
         BE    POST78                                                           
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
         MVC   0(DLPSLNQ,R8),TEMPSAV                                            
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,X'6A'    CREDIT INCOME                                    
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,PRODNUM    CONTRA IS CLIENT PRODUCT                     
         MVC   DLPSDBNM,PRODNAM                                                 
         MVC   DLPSCRAC,TAXBNUM                                                 
         MVC   DLPSCRNM,TAXBNAME                                                
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,TAXB                                                    
         MVC   DLPSANAL,SVFOFF                                                  
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
POST78   CP    NET,=PL8'0'         NET ZERO NO PAYABLE POSTING                  
         BE    POST88                                                           
         CLC   INCNUM+1(2),=C'SK'  OR IF SK                                     
         BE    POST88                                                           
         CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   POST80                                                           
         CLI   GSTSW,C'Y'          TEST GST APPLICABLE                          
         BNE   POST80                                                           
         CLI   INPUTSW,C'Y'        TEST INPUT GST                               
*        BNE   POST80              NO                                           
         BAS   RE,PTAXES                                                        
*                                                                               
         USING XPYELD,R8                                                        
POST80   MVI   XPYEL,XPYELQ        EXTRA PAYMENT FOR AOR                        
         MVI   XPYLN,XPYLNQ                                                     
         ZAP   XPYCD,=P'0'         NO C.D.                                      
         MVC   XPYCLI,SPACES                                                    
         L     RE,ACLINH           CLIENT NAME                                  
         MVC   XPYCLI,8(RE)                                                     
         OC    XPYCLI,SPACES                                                    
         L     RE,APRONH           PRODUCT NAME                                 
         MVC   XPYPRO,8(RE)                                                     
         OC    XPYPRO,SPACES                                                    
         L     RE,ABILH            INVOICE NUMBER                               
         MVC   XPYINV,SPACES                                                    
         MVC   XPYINV(L'CMBBIL),8(RE)                                           
         OC    XPYINV,SPACES                                                    
         GOTO1 DATCON,DMCB,(1,PDAT),(X'20',XPYPER)                              
         MVC   XPYPER+6(6),XPYPER                                               
         MVI   XPYTYPE,C'1'                                                     
         XC    XPYEST(5),XPYEST                                                 
         LA    R3,XPYLNQ                                                        
         AR    R8,R3                                                            
*                                                                               
         USING ACMTD,R8                                                         
         MVC   0(L'EL1A,R8),EL1A         ADD ELEM TO REC                        
         TM    POSTSW,AORINP                                                    
         BZ    *+10                                                             
         MVC   ACMTGRS,AORGRS                                                   
         LA    R3,ACMTLNQ                                                       
         AR    R8,R3                                                            
*                                                                               
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
                                                                                
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT PAYABLE                               
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,CLINUM     CONTRA IS CLIENT                             
         L     R2,ACLINH                                                        
         MVC   DLPSDBNM,8(R2)                                                   
         OC    DLPSDBNM,SPACES                                                  
         MVC   DLPSCRAC,PAYNUMBR                                                
         MVC   DLPSCRNM,PAYNAM                                                  
         OC    APNUM,APNUM         A/P OVERRIDE                                 
         BZ    POST82                                                           
         MVC   DLPSCRAC,APNUM                                                   
         MVC   DLPSCRNM,APNAM                                                   
*                                                                               
POST82   MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,NET                                                     
         MVC   DLPSANAL,SVFOFF                                                  
         LA    R3,LGTAB2                                                        
         LA    R4,LGTENT2                                                       
*                                                                               
POST84   CLC   0(LGTLEN2,R3),APNUM+1                                            
         BE    POST86                                                           
         LA    R3,LGTLEN2(R3)                                                   
         BCT   R4,POST84                                                        
         B     *+10                                                             
*                                                                               
POST86   MVC   DLPSANAL,SVCOFF                                                  
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         EJECT                                                                  
*                                                                               
POST88   CLI   MEDIANUM,1                                                       
         BE    POST90              ONE INCOME ACCOUNT                           
         USING TRCASHD,R8                                                       
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'G'       GROSS                                        
         ZAP   TRCSAMNT,=P'0'                                                   
         IC    R3,TRCSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT INCOME                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,PRODNUM    CONTRA IS CLIENT PRODUCT                     
         MVC   DLPSDBNM,PRODNAM                                                 
         LA    RF,MEDIA2                                                        
         USING POSTACD,RF                                                       
         MVC   DLPSCRAC,INCNUM                                                  
         MVC   DLPSCRNM,INCNAM                                                  
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,COM2                                                    
         MVC   DLPSANAL,SVFOFF                                                  
         MVC   TEMPSAV(DLPSLNQ),0(R8)                                           
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
POST90   TM    COMPSTAT,X'10'          COSTING REQUIRED                         
         BZ    POST92                  NO - CHECK DEPARTMENT POSTINGS           
         CLC   INCNUM+1(2),=C'SK'                                               
         BE    RECADD                                                           
                                                                                
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
                                                                                
         MVC   0(DLPSLNQ,R8),TEMPSAV                                            
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDCQ         DEBIT AND CREDIT                         
         MVI   DLPSLEN,DLPSLNQ                                                  
         LA    RF,MEDIA1                                                        
         MVC   DLPSDBAC,CLICOST                                                 
         MVC   DLPSDBNM,CLICTNM                                                 
         MVC   DLPSCRAC,COSCONUM                                                
         MVC   DLPSCRNM,COSCONAM                                                
         MVI   DLPSTYPE,X'80'                                                   
         ZAP   DLPSAMNT,COM                                                     
         MVC   DLPSANAL,SVFOFF                                                  
         MVC   TEMPSAV(DLPSLNQ),0(R8)                                           
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
                                                                                
         BAS   RE,ADDADB                                                        
                                                                                
         MVC   0(DLPSLNQ,R8),TEMPSAV                                            
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDCQ                                                  
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,CLICOST                                                 
         MVC   DLPSDBNM,CLICTNM                                                 
         MVC   DLPSCRAC,COSBLNUM                                                
         MVC   DLPSCRNM,COSBLNAM                                                
         MVI   DLPSTYPE,X'80'                                                   
         ZAP   DLPSAMNT,SPL                                                     
         MVC   DLPSANAL,SVFOFF                                                  
         MVC   TEMPSAV(DLPSLNQ),0(R8)                                           
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         CLI   MEDIANUM,1                                                       
         BE    POST92                                                           
*                                                                               
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
*                                                                               
         MVC   0(DLPSLNQ,R8),TEMPSAV                                            
         USING DLPOSTD,R8                                                       
         LA    RF,MEDIA2                                                        
         MVI   DLPSEL,DLPSEDCQ                                                  
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,CLICOST                                                 
         MVC   DLPSCRAC,COSCONUM                                                
         MVC   DLPSCRNM,COSCONAM                                                
         MVI   DLPSTYPE,X'80'                                                   
         ZAP   DLPSAMNT,COM2                                                    
         MVC   DLPSANAL,SVFOFF                                                  
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*------------------------------------------------------------------*            
*        ANALYZED POSTINGS                                                      
*------------------------------------------------------------------*            
         USING CATD,RE                                                          
POST92   TM    COSTSW,ANYCOST                                                   
         BZ    POST98                                                           
         CLC   CNEXPNUM,SPACES     MAKE SURE YOU HAVE ACCOUNTS                  
         BNH   POST98                                                           
         CLC   PCOSNUM,SPACES                                                   
         BNH   POST98                                                           
         CLC   CR13NUM,SPACES                                                   
         BNH   POST98                                                           
*                                                                               
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
*                                                                               
         MVI   DLPSEL,X'69'        DEBIT DEPT/PERS                              
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSANAL,SVFOFF                                                  
*&&US*&& MVI   DLPSTYPE,0                                                       
         OI    DLPSTYPE,X'80'      SUBSIDIARY                                   
         MVC   DLPSDBAC,CNEXPNUM                                                
         MVC   DLPSDBNM,CNEXPNAM                                                
         MVC   DLPSCRAC,PCOSNUM                                                 
         MVC   DLPSCRNM,PCOSNAM                                                 
         ZAP   DLPSAMNT,AMT                                                     
         TM    POSTSW,APSEOVR                                                   
         BNO   POST94                                                           
         ZAP   DLPSAMNT,NET                                                     
         MP    DLPSAMNT,=P'-1'                                                  
*                                                                               
POST94   ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
                                                                                
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
                                                                                
         MVI   DLPSEL,X'6A'        CREDIT CLIENT                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSANAL,SVFOFF                                                  
*&&US*&& MVI   DLPSTYPE,0                                                       
         OI    DLPSTYPE,X'80'      SUBSIDIARY                                   
         MVC   DLPSDBAC,CR13NUM                                                 
         MVC   DLPSDBNM,CR13NAM                                                 
         MVC   DLPSCRAC,PCOSNUM                                                 
         MVC   DLPSCRNM,PCOSNAM                                                 
         ZAP   DLPSAMNT,AMT                                                     
         TM    POSTSW,APSEOVR                                                   
         BNO   POST96                                                           
         ZAP   DLPSAMNT,NET                                                     
         MP    DLPSAMNT,=P'-1'                                                  
*                                                                               
POST96   ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
POST98   TM    POSTSW,DEPTPST                                                   
         BZ    POST102                                                          
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
         MVI   DLPSEL,X'68'                                                     
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,DEPTNUM                                                 
         MVC   DLPSDBNM,DEPTNAM                                                 
         MVC   DLPSANAL,SVFOFF                                                  
         MVC   DLPSCRAC,DSEXPNUM                                                
         MVC   DLPSCRNM,DSEXPNAM                                                
*&&US*&& MVI   DLPSTYPE,0                                                       
         OI    DLPSTYPE,X'80'      SUBSIDIARY                                   
         ZAP   DLPSAMNT,AMT                                                     
         TM    POSTSW,APSEOVR                                                   
         BNO   POST100                                                          
         ZAP   DLPSAMNT,NET                                                     
         MP    DLPSAMNT,=P'-1'                                                  
*                                                                               
POST100  ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
POST102  TM    POSTSW,STAFPST                                                   
         BZ    RECADD                                                           
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
         MVI   DLPSEL,X'6A'        CREDIT CLIENT IN 29                          
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSANAL,SVFOFF                                                  
*&&US*&& MVI   DLPSTYPE,0                                                       
         OI    DLPSTYPE,X'80'      SUBSIDIARY                                   
         MVC   DLPSDBNM,STAFNAM                                                 
         MVC   DLPSCRAC,PCLINUM                                                 
         MVC   DLPSCRNM,PCLINAM                                                 
         ZAP   DLPSAMNT,AMT                                                     
         TM    POSTSW,APSEOVR                                                   
         BNO   POST104                                                          
         ZAP   DLPSAMNT,NET                                                     
         MP    DLPSAMNT,=P'-1'                                                  
*                                                                               
POST104  MVI   DLPSDBAC,C'*'       CONTRA IS *EXPENSE-STAFF                     
         MVC   DLPSDBAC+1(12),EXPACC+3                                          
         L     R2,AARH                                                          
         CLC   8(2,R2),=C'SE'                                                   
         BE    *+8                                                              
         L     R2,AAPH                                                          
         ZIC   R1,5(R2)                                                         
         CLI   TENO,X'F0'          DO WE WANT TO TAKE 'N' BYTES                 
         BNL   POST106             FROM R.H.END OF T/E ACCOUNT                  
         L     RF,AARH                                                          
         CLI   5(RF),0                                                          
         BZ    *+8                                                              
         SH    R1,=H'2'            SUBTRACT FOR A/R OVERRIDE ONLY               
         TM    POSTSW,APOVER              COVER INPUT OF * FOR                  
         BZ    *+6                        A/P OVERRIDE                          
         BCTR  R1,0                                                             
         B     POST108                                                          
*                                                                               
POST106  PACK  DUB,TENO                                                         
         CVB   R4,DUB                                                           
         MVC   DLPSDBAC+1(14),SPACES                                            
         MVI   ERRNUM,2                                                         
         ZIC   RF,5(R2)            INPUT MUST BE AT LEAST 'N' LONG              
         SR    RF,R4                                                            
         BM    ERROR                                                            
         LA    R3,EXPACC+1                                                      
         LA    RE,0(RF,R3)                                                      
*                                                                               
         TM    POSTSW,AROVER              COVER INPUT OF * FOR                  
         BZ    *+6                        A/R OVERRIDE                          
         BCTR  RE,0                                                             
*                                                                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   DLPSDBAC+1(0),0(RE)                                              
         LR    R1,R4                                                            
*                                                                               
POST108  STC   R1,BYTE                                                          
         LA    RF,DLPSDBAC+1(R1)                                                
         L     RE,AARH                                                          
         CLI   5(RE),0                                                          
         BNE   POST110                                                          
         LA    RF,1(RF)                                                         
         B     POST112                                                          
*                                                                               
POST110  L     RE,AAPH                                                          
         CLI   5(RE),0                                                          
         BE    POST112                                                          
         LA    RF,1(RF)                                                         
*                                                                               
POST112  MVI   0(RF),C'-'                                                       
         LA    R1,DLPSDBAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),STAFNUM+3                                                
*                                                                               
         MVC   TEMPSAV(DLPSLNQ),0(R8)                                           
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
         MVC   0(DLPSLNQ,R8),TEMPSAV                                            
         MVI   DLPSEL,X'69'                                                     
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSANAL,SVFOFF                                                  
*&&US*&& MVI   DLPSTYPE,0                                                       
         OI    DLPSTYPE,X'80'      SUBSIDIARY                                   
         MVC   DLPSCRAC,DLPSDBAC                                                
         MVC   DLPSDBAC,STAFNUM    MINUS CREDIT STAFF                           
         MVC   DLPSDBNM,STAFNAM                                                 
         ZAP   DLPSAMNT,AMT                                                     
         TM    POSTSW,APSEOVR                                                   
         BNO   POST114                                                          
         ZAP   DLPSAMNT,NET                                                     
         MP    DLPSAMNT,=P'-1'                                                  
*                                                                               
POST114  MVC   DLPSCRNM,PCLINAM    CONTRA IS *EXPENSE-CLIENT                    
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
         LA    RF,DLPSCRAC+1(R1)                                                
         L     RE,AARH                                                          
         CLI   5(RE),0                                                          
         BNE   POST116                                                          
         LA    RF,1(RF)                                                         
         B     POST118                                                          
*                                                                               
POST116  L     RE,AAPH                                                          
         CLI   5(RE),0                                                          
         BE    POST118                                                          
         LA    RF,1(RF)                                                         
*                                                                               
POST118  LA    R1,DLPSCRAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),PCLINUM+3                                                
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                                                                               
*---------------------------------------------------------------------*         
RECADD   MVI   0(R8),0             END OF RECORD                                
         LA    R8,1(R8)                                                         
         LA    R3,IOAREA           GRT LENGTH                                   
         SR    R8,R3                                                            
         STH   R8,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         BAS   RE,PUTDAY                                                        
*                                                                               
         SR    R3,R3                                                            
         XC    WORK,WORK                                                        
         L     R2,ABILH                                                         
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
         ZAP   TRANSAMT,AMT                                                     
         MVC   WORK+6(4),FULL                                                   
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)                                                 
         BAS   RE,ADSCRINF                                                      
*                                                                               
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   RECADD03                                                         
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6STRO        STEREO?                          
         BO    RECADD05                                                         
RECADD03 MVI   CLEAROK,C'Y'                                                     
         XC    BASIS,BASIS                                                      
         XC    PBASIS,PBASIS                                                    
         XC    KEEPPROV,KEEPPROV                                                
         MVI   GSTSW,C'N'                                                       
         MVI   USERPROV,C'N'                                                    
         MVI   USERPST,C'N'                                                     
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   RECADD20                                                         
         MVI   CMCPROVH+4,0                                                     
*                                                                               
RECADD05 MVC   WORK,SPACES                                                      
         CLI   CMCBASEH+5,0                                                     
         BE    RECADD10                                                         
         MVC   WORK(L'CMCBASE),CMCBASE                                          
         MVI   CMCBASE,C'*'                                                     
         MVC   CMCBASE+1(L'CMCBASE),WORK                                        
         OI    CMCBASEH+6,X'80'                                                 
*                                                                               
RECADD10 MVC   WORK,SPACES                                                      
         CLI   CMCPBASH+5,0                                                     
         BE    RECADD20                                                         
         MVC   WORK(L'CMCPBAS),CMCPBAS                                          
         MVI   CMCPBAS,C'*'                                                     
         MVC   CMCPBAS+1(L'CMCPBAS),WORK                                        
         OI    CMCPBASH+6,X'80'                                                 
*                                                                               
RECADD20 L     R2,ADATH                                                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
* PTAXES - POST TAX MEMO ELEMENTS                                     *         
*---------------------------------------------------------------------*         
         USING SCIELD,RE                                                        
PTAXES   NTR1                                                                   
         MVI   BYTE,SCITTAXP       LOOK FOR GST FIRST                           
PTAX005  L     R2,AIOA             CANADIAN TAX POSTINGS                        
         ZICM  RF,0(R2),2          GET LENGTH                                   
         LTR   RF,RF                                                            
         BZ    PTAXXIT                                                          
         SH    RF,=H'2'            SUBTRACT 2-BYTE LENGTH                       
         LA    RE,2(R2)                                                         
         LR    R1,RE                                                            
         AR    R1,RF               POINT TO END OF POSTINGS                     
*                                                                               
PTAX010  CR    RE,R1                                                            
         BNL   PTAX050                                                          
         CLI   0(RE),SCIELQ        IS IT A SUBSID CASH ELEMENT?                 
         BNE   PTAX020                                                          
         CLC   SCITYPE,BYTE                                                     
         BE    PTAX030                                                          
*                                                                               
PTAX020  SR    R0,R0                                                            
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     PTAX010                                                          
*                                                                               
PTAX030  ZIC   R3,SCILN            COPY ELEMENT                                 
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),SCIEL                                                    
         LA    R8,1(R3,R8)                                                      
PTAX050  CLI   BYTE,SCITTQST       DID WE FINISH W/ PST?                        
         BE    PTAXXIT             THEN WE ARE DONE                             
         MVI   BYTE,SCITTQST       NO, DO PST                                   
         B     PTAX005                                                          
PTAXXIT  XIT1  REGS=(R8)                                                        
         DROP  RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* KEEPTAX - KEEP TAX MEMO ELEMENTS IF INPUT=N                         *         
*---------------------------------------------------------------------*         
         USING SCIELD,R1                                                        
KEEPTAX  NTR1                                                                   
         CLI   INPUTSW,C'Y'        INPUT GST/PST?                               
         BNE   KTAXXIT                                                          
         LR    R1,R8                                                            
*                                                                               
KTAX010  SH    R1,=Y(SCILN3Q)                                                   
         CLI   0(R1),SCIELQ        IS IT A SUBSID CASH ELEMENT?                 
         BNE   KTAXXIT                                                          
         CLI   SCITYPE,SCITTAXP    IS IT A GST?                                 
         BNE   KTAX010             NO, PST GO BACK ONE                          
*                                                                               
         LR    R8,R1                                                            
KTAXXIT  XIT1  REGS=(R8)                                                        
         DROP  R1                                                               
         EJECT                                                                  
ADSCRINF NTR1                                                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    RF,XTRAELM                                                       
         GOTO1 AADACDAY,BOPARM,(X'80',IOAREA),BOPL61,BOWORK1,(RF)               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERRXIT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
* VALJOB - VALIDATE FOR A JOB IF *SK ENTERED                          *         
*---------------------------------------------------------------------*         
VALJOB   NTR1                                                                   
         MVC   KEY(L'PRODNUM2),PRODNUM2                                         
         LA    R4,KEY+3                                                         
         SR    R3,R3                                                            
         IC    R3,PRDLNGTH                                                      
         AR    R4,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),8(R2)                                                    
         SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         OI    4(R2),X'20'                                                      
         MVC   JOBNUM,KEY          SAVE THE JOB                                 
         XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              SUB-ROUTINE TO POST TO THE GST LEDGER                            
*              ON EXIT, R8 IS RETURNED                                          
*---------------------------------------------------------------------*         
POSTGST  NTR1  ,                                                                
         L     R2,AIOA             CANDIAN TAX POSTINGS                         
         ZICM  RF,0(R2),2          GET LENGTH                                   
         LTR   RF,RF               ANY?                                         
         BZ    POSTGSTX                                                         
         SH    RF,=H'2'            SUBTRACT 2-BYTE LENGTH                       
         LA    RE,2(R2)                                                         
         LR    R0,R8                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         LR    R8,R0                                                            
*                                                                               
*&&DO                                                                           
         USING TRCASHD,R8                                                       
         MVI   TRCSEL,TRCSELQ      ADD A X'50' ELEMENT                          
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'N'       TYPE='N'                                     
         ZAP   TRCSAMNT,COM                                                     
         AP    TRCSAMNT,COM2       GST NET=COMMISSION FOR ZERO BILLS            
         CP    AMT,=P'0'                                                        
         BE    *+10                                                             
         AP    TRCSAMNT,NET        BASIS=NET + COMM FOR ALL OTHERS              
         CLI   INPUTSW,C'Y'        TEST FOR INPUT TAX TRANSACTION               
         BNE   *+10                                                             
         MP    TRCSAMNT,=P'-1'                                                  
*                                                                               
         OC    BASIS,BASIS         TEST FOR GST BASIS OVERRIDE                  
         BZ    POSTGST1            NO                                           
*                                                                               
         ZAP   TRCSAMNT,BASIS                                                   
         MVC   WORK(L'CMCBASE),CMCBASE PLUG IN A STAR BEFORE BASIS              
         MVI   CMCBASE,C'*'                                                     
         MVC   CMCBASE+1(L'CMCBASE-1),WORK                                      
         L     R2,ABASEH                                                        
         OI    6(R2),X'80'         XMIT IT BACK                                 
*                                                                               
POSTGST1 ZIC   R3,TRCSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
POSTGST2 CLC   PDAT,TAXPOINT       TEST FOR TAX POINT OVERRIDE                  
         BE    POSTGST4            NO                                           
*                                                                               
         USING GDAELD,R8                                                        
         MVI   GDAEL,GDAELQ        ADD A GENERAL DATE ELEMENT                   
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATTAXP    TYPE=TAX POINT                               
         MVC   GDADATE,TAXPOINT                                                 
         MVC   TEMPSAV(DLPSLNQ),0(R8)                                           
         IC    R3,GDALN                                                         
         AR    R8,R3                                                            
*                                                                               
         USING DLPOSTD,R8                                                       
POSTGST4 DS    0H                                                               
**T      BAS   RE,ADDANL                                                        
         MVC   0(DLPSLNQ,R8),TEMPSAV                                            
         L     R1,AVTC                                                          
         USING VTCD,R1                                                          
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSANAL,SVCOFF                                                  
         ZAP   DLPSAMNT,GST                                                     
         MVI   DLPSTYPE,0                                                       
         CLI   INPUTSW,C'Y'        TEST FOR INPUT GST                           
         BE    POSTGST6            YES                                          
*                                                                               
         MVI   DLPSEL,DLPSECRQ     CREDIT THE GST ACCOUNT                       
         MVC   DLPSCRAC,VTCACT                                                  
         MVC   DLPSCRNM,VTCACTNM                                                
         MVC   DLPSDBAC,PRODNUM                                                 
         MVC   DLPSDBNM,PRODNAM                                                 
         B     POSTGST8                                                         
*                                                                               
POSTGST6 MVI   DLPSEL,DLPSEDRQ     DEBIT GST FOR INPUT TAX                      
         MVC   DLPSDBAC,VTCACT                                                  
         MVC   DLPSDBNM,VTCACTNM                                                
         MVC   DLPSCRAC,PAYNUMBR   CONTRA A/C=SYS/MED NAME                      
         MVC   DLPSCRNM,PAYNAM                                                  
         OC    APNUM,APNUM         TEST FOR PAYABLE ACCOUNT                     
         BZ    *+16                NO                                           
         MVC   DLPSCRAC,APNUM      YES-USE IT AS CONTRA A/C                     
         MVC   DLPSCRNM,APNAM                                                   
         MP    DLPSAMNT,=P'-1'     REVERSE THE SIGN                             
         CLC   APNUM+1(2),=C'SV'                                                
         BNE   *+10                                                             
         MVC   DLPSANAL,SVCOFF                                                  
*                                                                               
POSTGST8 DS    0H                                                               
*&&                                                                             
POSTGSTX XIT1  REGS=(R8)                                                        
         EJECT                                                                  
*--------------------------------------------------------------                 
*        GOTO CATCALL TO GET 13 ACCT                                            
*--------------------------------------------------------------                 
GOCAT    NTR1                                                                   
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST                                    
         BNO   GOCAT50                                                          
*                                                                               
GOCAT10  DS    0H                                                               
         LA    R1,CATBLK                                                        
         USING CATD,R1                                                          
         MVC   CATDMGR,DATAMGR     BUILD CONTROL BLOCK                          
         MVC   CATSEAC,EXPACC      DEBIT ACCOUNT                                
         MVC   CATOFF,SVAOFF       OFFICE                                       
         MVC   CATDPT,SVDEPT       DEPARTMENT                                   
         GOTO1 VCATCALL,CATD                                                    
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),CATACC3                                                  
         OC    KEY+1,SPACES                                                     
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   CATERR,0                                                         
         BE    GOCAT20                                                          
         MVC   FVMSGNO,=AL2(AE$IANAL)                                           
         B     GOCATXIT                                                         
*                                                                               
GOCAT20  CLI   CATPST,C'N'         NO COST POSTING                              
         BE    GOCATXIT                                                         
         MVC   COSTANAL,CATCDE                                                  
         MVC   CR13NUM,CATACC3     SAVE 13 ACCOUNT                              
         OI    COSTSW,NEWCOST                                                   
         OI    COSTSW,ANYCOST                                                   
         B     GOCATXIT                                                         
*                                                                               
GOCAT50  MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'13'                                                  
         MVC   KEY+3(L'ANALBYTE),ANALBYTE                                       
         BAS   RE,GETACC                                                        
         MVC   CR13NUM,ACCTNUM                                                  
         MVC   CR13NAM,ACCTNAME                                                 
*                                                                               
GOCATXIT B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        ADD ANALYZED OFFICE ELEMENT TO IO AREA                                 
*------------------------------------------------------------------*            
ADDANL   CLI   ANOELM,0            TEST OFFICE ELEMENT                          
         BER   RE                                                               
         ST    R1,FULL                                                          
         LA    R1,ANOELM                                                        
         USING ANOELD,R1                                                        
         SR    RF,RF                                                            
         IC    RF,ANOLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),ANOEL                                                    
         LA    R8,1(RF,R8)         R8 TO NEXT AREA                              
         L     R1,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*------------------------------------------------------------------*            
*        ADD FREEFORM ELEMENT FOR CLIENT & PRODUCT                              
*------------------------------------------------------------------*            
ADDADB   CLI   ELDB,0              TEST OFFICE ELEMENT                          
         BER   RE                                                               
         ST    RF,FULL                                                          
         LA    R1,ELDB                                                          
         USING FFTELD,R1                                                        
         SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),ELDB                                                     
         LA    R8,1(RF,R8)         R8 TO NEXT AREA                              
         L     RF,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                   
*---------------------------------------------------------------------*         
MOVEFLD  ST    RE,FULL                                                          
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),FLD                                                      
*                                                                               
MOVEFLDX L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ERROR ROUITINES                                                        
*---------------------------------------------------------------------*         
POSTERR  XC    CONHEAD,CONHEAD     ERROR MESSAGE WITH FIELD INDEX               
         GOTO1 GETMSG,DMCB+12,(ERRNUM,MSG),(FNDX,DMCB),0                        
         B     EXIT                                                             
*                                                                               
ERR23    MVC   FVMSGNO,=Y(AE$SNRDE)                                             
         B     ERROR                                                            
*                                                                               
         EJECT                                                                  
AMTOOLGE MVC   MSGNO,=Y(AE$AMLGE)                                               
         B     ERROR                                                            
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        TABLE 1 - VALID LEDGERS FOR INPUT GST                                  
*        TABLE 2 - INVALID LEDGERS FOR INPUT TO AP FIELD                        
*---------------------------------------------------------------------*         
INPLEDTB DS    0CL2                                                             
         DC    C'SV'                                                            
         DC    C'SW'                                                            
         DC    C'SX'                                                            
         DC    C'SY'                                                            
         DC    C'SS'                                                            
         DC    C'ST'                                                            
         DC    C'SQ'                                                            
         DC    C'SP'                                                            
         DC    C'SU'                                                            
NINPLEDG EQU   (*-INPLEDTB)/L'INPLEDTB                                          
*                                                                               
LGTAB1   DS    0C                                                               
         DC    C'SP'                                                            
LGTLEN1  EQU   *-LGTAB1                                                         
         DC    C'SQ'                                                            
         DC    C'SS'                                                            
         DC    C'ST'                                                            
         DC    C'SU'                                                            
         DC    C'SV'                                                            
         DC    C'SW'                                                            
         DC    C'SX'                                                            
         DC    C'SY'                                                            
LGTENT1  EQU   (*-LGTAB1)/LGTLEN1                                               
*                                                                               
*                                                                               
LGTAB2   DS    0C                                                               
         DC    C'SB'                                                            
LGTLEN2  EQU   *-LGTAB2                                                         
         DC    C'SP'                                                            
         DC    C'SQ'                                                            
         DC    C'SS'                                                            
         DC    C'ST'                                                            
         DC    C'SU'                                                            
         DC    C'SV'                                                            
         DC    C'SW'                                                            
         DC    C'SX'                                                            
         DC    C'SY'                                                            
LGTENT2  EQU   (*-LGTAB2)/LGTLEN2                                               
*                                                                               
*                                                                               
LGTAB3   DS    0C                                                               
         DC    C'SP'                                                            
LGTLEN3  EQU   *-LGTAB3                                                         
         DC    C'SI'                                                            
         DC    C'SJ'                                                            
         DC    C'SK'                                                            
         DC    C'SQ'                                                            
         DC    C'SS'                                                            
         DC    C'ST'                                                            
         DC    C'SU'                                                            
         DC    C'SV'                                                            
         DC    C'SW'                                                            
         DC    C'SX'                                                            
         DC    C'SY'                                                            
LGTENT3  EQU   (*-LGTAB3)/LGTLEN3                                               
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         DROP  R4                                                               
       ++INCLUDE ACBATCODE                                                      
*                                                                               
NOCTAX   DC    C'** ERROR - CANADIAN SCREEN NOT AVAILABLE'                      
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* US SCREEN TABLE  (BYTES 0-3=DISP TO FIELD, BYTES 4-7=DISP TO ADCON)           
*---------------------------------------------------------------------*         
USTAB    DS    0D                                                               
         DC    AL4(CMBDATH-TWAD),AL4(ADATH-PROGD)                               
         DC    AL4(CMBDUEH-TWAD),AL4(ADUEH-PROGD)                               
         DC    AL4(CMBSYMH-TWAD),AL4(ASYMH-PROGD)                               
         DC    AL4(CMBSYMNH-TWAD),AL4(ASYMNH-PROGD)                             
         DC    AL4(CMBCLIH-TWAD),AL4(ACLIH-PROGD)                               
         DC    AL4(CMBCLINH-TWAD),AL4(ACLINH-PROGD)                             
         DC    AL4(CMBPROH-TWAD),AL4(APROH-PROGD)                               
         DC    AL4(CMBPRONH-TWAD),AL4(APRONH-PROGD)                             
         DC    AL4(CMBESTH-TWAD),AL4(AESTH-PROGD)                               
         DC    AL4(CMBMTHH-TWAD),AL4(AMTHH-PROGD)                               
         DC    AL4(CMBDSPH-TWAD),AL4(ADSPH-PROGD)                               
         DC    AL4(CMBBILH-TWAD),AL4(ABILH-PROGD)                               
         DC    AL4(CMBAMTH-TWAD),AL4(AAMTH-PROGD)                               
         DC    AL4(CMBAORH-TWAD),AL4(AAORH-PROGD)                               
         DC    AL4(CMBBASH-TWAD),AL4(ABASH-PROGD)                               
         DC    AL4(CMBNETH-TWAD),AL4(ANETH-PROGD)                               
         DC    AL4(CMBCOMH-TWAD),AL4(ACOMH-PROGD)                               
         DC    AL4(CMBSPLH-TWAD),AL4(ASPLH-PROGD)                               
         DC    AL4(CMBOFFH-TWAD),AL4(ACOFH-PROGD)                               
         DC    AL4(CMBAOFH-TWAD),AL4(AAOFH-PROGD)                               
         DC    AL4(CMBDEPH-TWAD),AL4(ADEPH-PROGD)                               
         DC    AL4(CMBSTFH-TWAD),AL4(ASTFH-PROGD)                               
         DC    AL4(CMBARH-TWAD),AL4(AARH-PROGD)                                 
         DC    AL4(CMBARNH-TWAD),AL4(AARNH-PROGD)                               
         DC    AL4(CMBAPH-TWAD),AL4(AAPH-PROGD)                                 
         DC    AL4(CMBAPNH-TWAD),AL4(AAPNH-PROGD)                               
         DC    AL4(CMBBLSRH-TWAD),AL4(ABLSRH-PROGD)                             
         DC    AL4(CMBCDH-TWAD),AL4(ACDH-PROGD)                                 
         DC    AL4(CMBNUMH-TWAD),AL4(ANUMH-PROGD)                               
         DC    AL4(CMBNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    X'FF'                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* CA SCREEN TABLE  (BYTES 0-3=DISP TO FIELD, BYTES 4-7=DISP TO ADCON)           
*---------------------------------------------------------------------*         
CANTAB   DS    0D                                                               
         DC    AL4(CMCTYPEH-TWAD),AL4(ATYPEH-PROGD)                             
         DC    AL4(CMCPROVH-TWAD),AL4(APROVH-PROGD)                             
         DC    AL4(CMCINPTH-TWAD),AL4(AINPTH-PROGD)                             
         DC    AL4(CMCGAMTH-TWAD),AL4(AGAMTH-PROGD)                             
         DC    AL4(CMCDATH-TWAD),AL4(ADATH-PROGD)                               
         DC    AL4(CMCDUEH-TWAD),AL4(ADUEH-PROGD)                               
         DC    AL4(CMCSYMH-TWAD),AL4(ASYMH-PROGD)                               
         DC    AL4(CMCSYMNH-TWAD),AL4(ASYMNH-PROGD)                             
         DC    AL4(CMCCLIH-TWAD),AL4(ACLIH-PROGD)                               
         DC    AL4(CMCCLINH-TWAD),AL4(ACLINH-PROGD)                             
         DC    AL4(CMCPROH-TWAD),AL4(APROH-PROGD)                               
         DC    AL4(CMCPRONH-TWAD),AL4(APRONH-PROGD)                             
         DC    AL4(CMCESTH-TWAD),AL4(AESTH-PROGD)                               
         DC    AL4(CMCMTHH-TWAD),AL4(AMTHH-PROGD)                               
         DC    AL4(CMCDSPH-TWAD),AL4(ADSPH-PROGD)                               
         DC    AL4(CMCBILH-TWAD),AL4(ABILH-PROGD)                               
         DC    AL4(CMCAMTH-TWAD),AL4(AAMTH-PROGD)                               
         DC    AL4(CMCNETH-TWAD),AL4(ANETH-PROGD)                               
         DC    AL4(CMCCOMH-TWAD),AL4(ACOMH-PROGD)                               
         DC    AL4(CMCSPLH-TWAD),AL4(ASPLH-PROGD)                               
         DC    AL4(CMCBASEH-TWAD),AL4(ABASEH-PROGD)                             
         DC    AL4(CMCPBASH-TWAD),AL4(APBASEH-PROGD)                            
         DC    AL4(CMCARH-TWAD),AL4(AARH-PROGD)                                 
         DC    AL4(CMCARNH-TWAD),AL4(AARNH-PROGD)                               
         DC    AL4(CMCAPH-TWAD),AL4(AAPH-PROGD)                                 
         DC    AL4(CMCAPNH-TWAD),AL4(AAPNH-PROGD)                               
         DC    AL4(CMCBLSRH-TWAD),AL4(ABLSRH-PROGD)                             
         DC    AL4(CMCCDH-TWAD),AL4(ACDH-PROGD)                                 
         DC    AL4(CMCNUMH-TWAD),AL4(ANUMH-PROGD)                               
         DC    AL4(CMCNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    AL4(CMCAORH-TWAD),AL4(AAORH-PROGD)                               
         DC    AL4(CMCOFFH-TWAD),AL4(ACOFH-PROGD)                               
         DC    AL4(CMCAOFH-TWAD),AL4(AAOFH-PROGD)                               
         DC    AL4(CMCDEPH-TWAD),AL4(ADEPH-PROGD)                               
         DC    AL4(CMCSTFH-TWAD),AL4(ASTFH-PROGD)                               
         DC    X'FF'                                                            
         EJECT                                                                  
CTAXMOD  DS    0D                                                               
         NMOD1 0,**CTAX**                                                       
*                                                                               
         L     RC,0(R1)                                                         
         USING PROGD,RC                                                         
         USING TWAD,RA                                                          
         USING GWS,R9                                                           
         MVI   CTXMODE,C'E'                                                     
         CLI   MODE,3                                                           
         BE    CTAX50                                                           
         MVI   CTXMODE,C'B'                                                     
         CLI   MODE,4                                                           
         BNE   *+8                                                              
         MVI   CTXMODE,C'G'        GO THROUGH                                   
*                                                                               
         CLI   CSACT,ACTCHA                                                     
         BNE   CTAX10                                                           
         CLI   CTAXBEF,C'Y'        DID WE READ BEFORE?                          
         BE    *+8                 YES, DON'T RESET AGAIN                       
         BAS   RE,EXTRAELM                                                      
CTAX10   MVC   CTXXTELM,XTRAELM                                                 
*                                                                               
         MVC   AMTBLK(2),=C'TT'    NEED WORK CODE FOR BT41                      
         ZAP   AMTBLK+2(6),NET     NET + COMMISION                              
         ZAP   AMTBLK+8(6),AMT                                                  
         ZAP   AMTBLK+14(6),COM                                                 
         ZAP   AMTBLK+20(6),COM2                                                
*        LA    RF,7                                                             
*        LA    RE,AMTBLK+8         CLEAR THE REST                               
*TAX13   MVC   0(2,RE),SPACES                                                   
*        ZAP   2(6,RE),=P'0'                                                    
*        LA    RE,8(RE)                                                         
*        BCT   RF,CTAX13                                                        
*                                                                               
         MVC   CTXACC(1),COMPANY                                                
         MVC   CTXOFF,SVCOFF                                                    
         MVI   CTXGORN,C'N'                                                     
         L     R2,APROVH                                                        
         MVC   CTXPROV,8(R2)                                                    
         MVC   CTXDATE,TAXPOINT                                                 
         MVC   CTXPDATE,PDAT                                                    
         MVC   CTXVENGT,VENDTYPE                                                
         MVC   CTXVENPT,VENDPSTT                                                
         L     R2,ATYPEH                                                        
         MVC   CTXGSTT,8(R2)                                                    
         MVC   CTXGSTTN,SPACES                                                  
         L     R2,AGAMTH                                                        
         MVC   CTXLGSTA,5(R2)                                                   
         MVC   CTXGSTA,8(R2)                                                    
         MVC   CTXGBASE,BASIS                                                   
         MVC   CTXPBASE,PBASIS                                                  
         MVC   CTXUPSTT,USERPST                                                 
         MVC   CTXUPROV,USERPROV                                                
*                                                                               
         MVI   CTXGINPT,C'N'       NOT GST INPUT, DEFAULT                       
         MVC   CTXCNTRA,PRODNUM    CONTRA-ACCOUNT                               
         MVC   CTXCNTRN,PRODNAM                                                 
         L     R2,AINPTH                                                        
         CLI   8(R2),C'Y'                                                       
         BNE   CTAX50                                                           
         MVI   CTXGINPT,C'Y'       GST INPUT                                    
*                                                                               
CTAX15   MVC   CTXCNTRA,PAYNUMBR   CONTRA-ACCOUNT                               
         MVC   CTXCNTRN,PAYNAM                                                  
         OC    APNUM,APNUM         TEST FOR PAYABLE ACCOUNT                     
         BZ    *+16                NO                                           
         MVC   CTXCNTRA,APNUM      YES, USE IT AS CONTRA                        
         MVC   CTXCNTRN,APNAM                                                   
*                                                                               
CTAX50   LA    R3,X'41'            CTAX SCREEN                                  
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         L     R3,AVTC                                                          
         GOTO1 (RF),DMCB,(X'1A',CTXDATA),(R9),AMTBLK,(R3)                       
         MVI   MODE,3                                                           
         CLI   CTXMODE,C'E'                                                     
         BE    CTAXXIT                                                          
         CLI   CTXMODE,C'G'        GOOD 1-PASS                                  
         BE    CTAX60                                                           
         CLI   CTXMODE,C'Z'        ERROR ON 1-PASS                              
         BE    CTAX60                                                           
         CLI   CTXMODE,C'X'                                                     
         BNE   CTAXXIT                                                          
CTAX60   MVI   MODE,0                                                           
         CLI   CTXMODE,C'Z'                                                     
         BE    *+8                                                              
         MVI   CTAXBEF,C'Y'                                                     
         MVC   GSTSW,CTXAPPL                                                    
*                                                                               
         ZAP   GST,CTXGST                                                       
         ZAP   PST,CTXPST                                                       
*                                                                               
         MVC   USERPST,CTXUPSTT                                                 
         CLI   CTXUPROV,C'Y'                                                    
         BNE   CTAX61                                                           
         MVI   USERPROV,C'Y'                                                    
         MVC   KEEPPROV,CTXPROV                                                 
*                                                                               
CTAX61   L     R2,APROVH                                                        
         MVC   8(2,R2),CTXPROV                                                  
         OI    6(R2),X'80'                                                      
         L     R2,ATYPEH                                                        
         MVC   8(1,R2),CTXGSTT                                                  
         OI    6(R2),X'80'                                                      
         L     R2,AGAMTH                                                        
         MVC   8(10,R2),SPACES                                                  
         MVC   5(1,R2),CTXLGSTA                                                 
         CLI   CTXLGSTA,0                                                       
         BZ    CTAX65                                                           
         ZIC   R1,CTXLGSTA                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),CTXGSTA                                                  
CTAX65   OI    6(R2),X'80'                                                      
*                                                                               
CTAX70   MVC   XTRAELM,CTXXTELM                                                 
         L     R2,ATYPEH                                                        
         ST    R2,FADR                                                          
         CLI   CTXMODE,C'G'        EXCEPT FOR 1 PASS                            
         BE    CTAXXIT                                                          
         OI    6(R2),X'01'         MODIFIED FIELD, TO HIT ENTER                 
*                                                                               
         CLI   CTXMODE,C'Z'                                                     
         BNE   CTAXXIT                                                          
         L     R2,ATYPEH                                                        
         CLI   CTXERR,2                                                         
         BNE   *+8                                                              
         L     R2,AGAMTH                                                        
         CLI   CTXERR,3                                                         
         BNE   *+8                                                              
         L     R2,APROVH                                                        
         ST    R2,FVADDR                                                        
*                                                                               
CTAXXIT  XMOD1                                                                  
         EJECT                                                                  
*------------------------------------------------------------------             
* EXTRAELM - EXTRACTS THE SPECIAL 2ND SCREEN FROM ITEM RECORD                   
*------------------------------------------------------------------             
EXTRAELM NTR1                                                                   
         LA    R2,BCITECUR         HAVE TO READ ITEM RECORD                     
         USING LSTTABD,R2                                                       
         MVC   IODAOVER,LSTTDA                                                  
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO1                                                          
         USING TBARECD,R2                                                       
         LA    R2,TBARFST          DISP TO 1ST ELEMENT                          
         USING SFSELD,R2                                                        
EXTRA10  CLI   SFSEL,0             END OF ELEMENTS                              
         BE    EXTRAXIT                                                         
         CLI   SFSEL,SFSELQ        SCREEN FIELD SAVE ELEMENT                    
         BNE   EXTRA50                                                          
         CLI   SFSFLDN,128         1ST FIELD # SAVED                            
         BNE   EXTRA50                                                          
         MVC   XTRAELM,0(R2)       COPY CHUNK                                   
         B     EXTRAXIT                                                         
*                                                                               
EXTRA50  ZIC   R0,SFSLN                                                         
         AR    R2,R0                                                            
         B     EXTRA10                                                          
EXTRAXIT XIT1                                                                   
*                                                                               
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
       ++INCLUDE ACBATE4D                                                       
         ORG   TWAHOLE                                                          
AMTBLK   DS    CL64                                                             
GSTSW    DS    CL1                                                              
CTAXBEF  DS    CL1                                                              
KEEPPROV DS    CL2                 SAVE PROVINCE                                
TMPNET   DS    PL6                                                              
TMPGRS   DS    PL6                                                              
TMPGST   DS    PL6                                                              
TMPPST   DS    PL6                                                              
XTRAELM  DS    CL71                                                             
CLEAROK  DS    CL1                                                              
USERPROV DS    CL1                 FLAG, USER ENTERED PROVINCE                  
USERPST  DS    CL1                 FLAG, USER ENTERED PST TYPE                  
         EJECT                                                                  
         ORG   CONTABH                                                          
       ++INCLUDE ACBATD1D                                                       
         ORG   CONTABH                                                          
       ++INCLUDE ACBATC0D                                                       
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DSECT FOR 42-BYTE SCAN BLOCK                                     
*---------------------------------------------------------------------*         
SCAND    DSECT                                                                  
SCLEN1   DS    CL1                                                              
SCLEN2   DS    CL1                                                              
SCVAL1   DS    CL1                                                              
SCVAL2   DS    CL1                                                              
SCBIN1   DS    CL4                                                              
SCBIN2   DS    CL4                                                              
SCDATA1  DS    CL10                                                             
SCDATA2  DS    CL20                                                             
SCLNQ    EQU   *-SCAND                                                          
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              LOCAL WORKING STORAGE                                            
*---------------------------------------------------------------------*         
PROGD    DSECT                                                                  
BLOCK    DS    10CL42                                                           
EL23     DS    CL(ACOTLNQ1)                                                     
EL27     DS    CL57                                                             
EL1A     DS    CL(ACMTLNQ)                                                      
ELA2     DS    CL120                                                            
ELDB     DS    CL(FFTLN1Q+L'FFTDLEN+L'FFTCLPRA)                                 
SET      DS    CL1                                                              
CLINUM   DS    CL15                SJ LEVEL ONE ACCT NUM                        
PRODNUM  DS    CL15                SJ LEVEL TWO ACCT NUM                        
PRODNUM2 DS    CL15                SJ LEVEL TWO ACCT NUM                        
PRODNAM  DS    CL36                SJ  "          "  NAME                       
JOBNUM   DS    CL15                SJ LEVEL THREE ACCT NUM                      
JOBNAM   DS    CL36                SJ  "            "  NAME                     
ARNUM    DS    CL15                ACCT REC NUM                                 
ARNAM    DS    CL36                ACCT REC NAME                                
PAYNUMBR DS    CL15                                                             
PAYNAM   DS    CL36                                                             
COSBLNUM DS    CL15                11 ACCOUNT                                   
COSBLNAM DS    CL36                11 ACCOUNT NAME                              
TAXANUM  DS    CL15                                                             
TAXANAME DS    CL36                                                             
TAXBNUM  DS    CL15                                                             
TAXBNAME DS    CL36                                                             
APNUM    DS    CL15                ACCT PAYABLE NUM                             
APNAM    DS    CL36                ACCT PAYABLE NAME                            
*        ANALYSIS POSTINGS                                                      
DEPTNUM  DS    CL15                2D DEPT NUM                                  
DEPTNAM  DS    CL36                2D DEPT NAME                                 
DSEXPNUM DS    CL15                28 ACCT NUM                                  
DSEXPNAM DS    CL36                28 ACCT NAME                                 
STAFNUM  DS    CL15                2P LEVEL 3 ACCT NUM                          
STAFNAM  DS    CL36                2P  "        "  NAME                         
PCLINUM  DS    CL15                29 ACCT NUM                                  
PCLINAM  DS    CL36                29   "  NAME                                 
PCOSNUM  DS    CL15                1C ACCT NUM                                  
PCOSNAM  DS    CL36                1C   "  NAME                                 
CNEXPNUM DS    CL15                1P ACCT NUM                                  
CNEXPNAM DS    CL36                1P   "  NAME                                 
CR13NUM  DS    CL15                13 ACCT NUM                                  
CR13NAM  DS    CL36                13 ACCT NAME                                 
MEDIANUM DS    CL1                 SAVEAREA FOR # MEDIAS ENTERED                
BYTE     DS    CL1                                                              
ANALBYTE DS    CL1                                                              
CC       DS    CL3                 COSTING OVERRIDE                             
CCDSP    DS    CL1                 DISPLACEMENT TO COST OVERRIDE                
SVAOFF   DS    CL2                                                              
SVFOFF   DS    CL2                                                              
SVCOFF   DS    CL2                                                              
SVDEPT   DS    CL3                                                              
TAXA     DS    D                                                                
TAXB     DS    D                                                                
AMT      DS    D                   BILL AMOUNT                                  
NET      DS    D                   NET AMOUNT                                   
COM      DS    D                   COMMISSION                                   
COM2     DS    D                                                                
GST      DS    D                   GST AMOUNT                                   
PST      DS    D                   PST AMOUNT                                   
SPL      DS    D                   SPECIAL                                      
CD       DS    D                                                                
BASIS    DS    D                   GST BASIS                                    
PBASIS   DS    D                   PST BASIS                                    
PDAT     DS    CL3                 PACKED DATE                                  
TAXPOINT DS    PL3                 TAX POINT DATE                               
INPUTSW  DS    CL1                 Y=GST IS INPUT TAX                           
COSTAC   DS    CL1                                                              
ELCODE   DS    CL1                                                              
DUEDAT   DS    CL6                                                              
DESCRIPT DS    CL12                                                             
ADVMOS   DS    CL2                 ADVERTISING MONTH OF SERVICE                 
MEDIA1   DS    CL(POSTLNQ)         POSTING ACCOUNTS FOR FIRST AMOUNT            
MEDIA2   DS    CL(POSTLNQ)                                                      
SVSYSMED DS    CL2                 SYSTEM MEDIA OVERRIDE                        
ARSW     DS    CL1                                                              
*                                                                               
ADATH    DS    A                   A(DATE FIELD)                                
ADUEH    DS    A                                                                
ASYMH    DS    A                                                                
ASYMNH   DS    A                                                                
ACLIH    DS    A                                                                
ACLINH   DS    A                                                                
APROH    DS    A                                                                
APRONH   DS    A                                                                
AESTH    DS    A                                                                
AMTHH    DS    A                   A(ADV MONTH FIELD)                           
AAMTH    DS    A                                                                
AAORH    DS    A                                                                
ABASH    DS    A                                                                
ADSPH    DS    A                                                                
ABILH    DS    A                                                                
ANETH    DS    A                                                                
ACOMH    DS    A                                                                
ASPLH    DS    A                                                                
ATYPEH   DS    A                                                                
*TYPNH   DS    A                                                                
AGAMTH   DS    A                                                                
AINPTH   DS    A                                                                
ABASEH   DS    A                                                                
ACOFH    DS    A                                                                
AAOFH    DS    A                                                                
ADEPH    DS    A                                                                
ASTFH    DS    A                                                                
AARH     DS    A                                                                
AARNH    DS    A                                                                
AAPH     DS    A                                                                
AAPNH    DS    A                                                                
ABLSRH   DS    A                                                                
ACDH     DS    A                                                                
ANUMH    DS    A                                                                
ANARH    DS    A                                                                
APROVH   DS    A                                                                
APBASEH  DS    A                                                                
*                                                                               
SAVR8    DS    F                                                                
AORGRS   DS    F                                                                
*                                                                               
ANOELM   DS    CL(ANOLNQ)                                                       
CATBLK   DS    CL(CATLNQ)                                                       
TEMPSAV  DS    CL(DLPSLNQ)                                                      
EXPACC   DS    CL15                                                             
COSTSW   DS    CL1                                                              
NEWCOST  EQU   X'01'                                                            
OLDCOST  EQU   X'02'                                                            
ANYCOST  EQU   X'04'                                                            
POSTSW   DS    CL1                                                              
DEPTPST  EQU   X'01'                                                            
STAFPST  EQU   X'02'                                                            
APOVER   EQU   X'04'                                                            
AROVER   EQU   X'08'                                                            
AORINP   EQU   X'10'                                                            
APSEOVR  EQU   X'20'                                                            
COSTANAL DS    CL5                                                              
ACTAXMOD DS    A                   GST/PST ROUTINE                              
VENDTYPE DS    CL1                 GST TAX CODE FROM OPT MAINT                  
VENDPROV DS    CL2                 PROVINCE FROM OPT MAINT                      
VENDPSTT DS    CL1                 PST TAX CODE FROM OPT MAINT                  
       ++INCLUDE ACBATCTAX                                                      
TMPMODE  DS    CL1                                                              
KEY      DS    CL49                                                             
IOAREA   DS    2500C                                                            
PROGX    DS    0C                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        DSECT TO COVER POSTING ACCOUNT INFO                                    
*-------------------------------------------------------------------*           
POSTACD  DSECT                                                                  
MEDIA    DS    CL2                 MEDIA CODE                                   
INCNUM   DS    CL15                INCOME ACCOUNT (SI)                          
INCNAM   DS    CL36                               NAME                          
CLICOST  DS    CL15                CLIENT COSTING ANALYSIS (1C)                 
CLICTNM  DS    CL36                                       NAME                  
COSCONUM DS    CL15                COMMISSION ANALYSIS (12)                     
COSCONAM DS    CL36                                    NAME                     
POSTLNQ  EQU   *-POSTACD                                                        
         EJECT                                                                  
*        ACGENBOTH                                                              
*        ACGENDAY                                                               
*        ACGENFILE                                                              
*        DDFLDIND                                                               
*        ACCATCALLD                                                             
*        FAUTL                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE ACCATCALLD                                                     
       ++INCLUDE FAUTL                                                          
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096ACBAT1A   03/13/12'                                      
         END                                                                    
