*          DATA SET ACBAT06    AT LEVEL 081 AS OF 12/04/12                      
*PHASE T61B06A                                                                  
         TITLE 'MANUAL PRODUCTION BILLING'                                      
T61B06   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,*BAT06*,R7,CLEAR=YES,RR=R2                          
         USING PROGD,RC                                                         
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING TWAD,RA                                                          
         ST    R2,RELO                                                          
         L     R1,=A(CTAXMOD)                                                   
         AR    R1,R2                                                            
         ST    R1,ACTAXMOD                                                      
*                                                                               
* US AND CANADIAN SCREENS ARE IDENTICAL UP THROUGH COMMISSION FIELD             
*                                                                               
         EJECT                                                                  
*------------------------------------------------------------                   
*        VALIDATE INPUT ACCOUNTS                                                
*------------------------------------------------------------                   
*                                                                               
         BAS   RE,GETODAY                                                       
*                                                                               
         MVC   TMPMODE,MODE                                                     
         NI    TMPMODE,X'0F'                                                    
         CLI   TMPMODE,1                                                        
         BH    B0                                                               
         LA    R2,BILDATH          EDIT BILL DATE FIRST                         
         MVI   ERRNUM,INVDATE                                                   
         CLI   BILDATH+5,0                                                      
         BNE   *+12                                                             
         BAS   RE,GETODAY                                                       
         B     B0                                                               
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*                                                                               
B0       GOTO1 DATCON,DMCB,(0,WORK),(1,BILLDATE)                                
         CLI   BILLDATE,X'70'                                                   
         BL    ERROR                                                            
         GOTO1 DATECHK,DMCB,BILLDATE                                            
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
*                                                                               
         CLI   CSACT,ACTCHA                                                     
         BNE   *+16                                                             
         MVI   CLEAROK,C'N'        DON'T CLEAR GST/PST FOR CHANGE               
         MVI   USERPROV,C'Y'       USER ENTERED PROV FROM CHANGE                
         MVI   USERPST,C'Y'        USER ENTERED PST TYPE FOR CHANGE             
*                                                                               
B0A0010  CLI   AGYCTRY,CTRYCAN                                                  
         BNE   B0A0040                                                          
         CLI   MODE,3                                                           
         BE    B0A0040                                                          
         CLI   CLEAROK,C'N'        NOT OK TO CLEAR?                             
         BE    B0A0030                                                          
         LA    R2,BICTYPNH                                                      
         MVC   8(L'BICTYPN,R2),SPACES                                           
         OI    6(R2),X'80'                                                      
*                                                                               
B0A0020  LA    R2,BICTYPEH                                                      
         TM    4(R2),X'80'                                                      
         BO    *+12                                                             
         MVI   8(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,BICGAMTH                                                      
         TM    4(R2),X'80'                                                      
         BO    *+18                                                             
         OI    6(R2),X'80'                                                      
         MVI   5(R2),0                                                          
         XC    8(L'BICGAMT,R2),8(R2)                                            
         XC    XTRAELM,XTRAELM     CLEAR PST FIELDS                             
*                                                                               
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6STRO        STEREO?                          
         BZ    B0A0030                                                          
         TM    BICPROVH+4,X'80'    NEW PROVINCE?                                
         BO    B0A0035                                                          
         MVI   USERPROV,C'N'                                                    
         B     B0A0038                                                          
*                                                                               
B0A0030  LA    R2,BICPROVH                                                      
         TM    4(R2),X'80'         NEW PROVINCE?                                
         BNO   B0A0040             SAVE PROVINCE                                
B0A0035  MVI   USERPROV,C'Y'       USER ENTERED PROVINCE                        
B0A0038  MVC   KEEPPROV,8(R2)                                                   
         XC    XTRAELM,XTRAELM                                                  
*                                                                               
B0A0040  MVI   CLEAROK,C'N'        DON'T CLEAR UNTIL DONE                       
*                                                                               
*--------------------------------------------------------------------           
*        MODE=X'00'  ORDER OVERLAY                                              
*        MODE=X'01'  EXIT WITH ORDER DISPLAYED                                  
*        MODE=X'03'  CANADIAN SALES TAX SCREEN                                  
*--------------------------------------------------------------------           
B0A0050  CLI   CSACT,ACTCHA        ITEM CHANGE?                                 
         BNE   B0A0060                                                          
         CLI   PFKEY,X'FF'         FIRST TIME, IT SETS PFKEY TO FF              
         BNE   *+8                 YES                                          
         MVI   PFKEY,0                                                          
B0A0060  CLI   MODE,3              CANDIAN TAX?                                 
         BE    B0A0070                                                          
*                                                                               
         CLI   PFKEY,7             USE PF=7 TO LOAD                             
         BNE   B0A0065                                                          
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    B0A0061                                                          
         MVC   FVMSGNO,=Y(AE$CSNA)                                              
         LA    R2,CONACTH                                                       
         B     ERROR                                                            
*                                                                               
B0A0061  ZAP   GST,=P'0'                                                        
         ZAP   PST,=P'0'                                                        
         B     B1                                                               
*                                                                               
B0A0065  CLI   PFKEY,0                                                          
         BE    B1                                                               
         MVI   ERRNUM,251          INVALID PFKEY                                
         L     R2,TIACURS                                                       
         B     ERROR                                                            
*                                                                               
B0A0070  CLI   AGYCTRY,CTRYCAN     CANADIAN?                                    
         BNE   B1                                                               
         MVI   CSSPROG,2                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 ACTAXMOD,DMCB,(RC)                                               
         CLI   CSACT,ACTCHA        ARE WE DOING A CHANGE?                       
         BNE   CURSIT              NO, LEAVE                                    
         CLC   FVMSGNO,=AL2(FVFOK) IF ERROR, RETURN                             
         BNE   CURSIT                                                           
         MVC   FVMSGNO,=X'FFFF'    TELL BT61 WE HAVE TO COME BACK               
         B     CURSIT                                                           
*                                                                               
B1       MVC   KEY,SPACES                                                       
         MVC   COSTAC,SPACES                                                    
         MVC   KEY(1),COMPANY                                                   
         SR    R3,R3                                                            
         LA    R2,BILCLIH                                                       
         BAS   RE,ANY                                                           
         LA    R4,COMPEL                                                        
         USING ACCOMPD,R4                                                       
         MVC   KEY+1(2),ACMPJOB    U/L FOR CLI/PROD/JOBS                        
         TM    BILCLIH+4,X'20'                                                  
         BO    B2                                                               
         NI    BILPROH+4,X'DF'                                                  
         NI    BILJOBH+4,X'DF'                                                  
         MVC   BILCLIN,SPACES                                                   
         OI    BILCLINH+6,X'80'                                                 
         MVC   BILPRON,SPACES                                                   
         OI    BILPRONH+6,X'80'                                                 
         MVC   BILJOBN,SPACES                                                   
         OI    BILJOBNH+6,X'80'                                                 
*                                                                               
B2       IC    R3,BILCLIH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),BILCLI                                                  
         LA    R5,KEY+3                                                         
         IC    R3,CLILNGTH         LEVA LENGTH                                  
         AR    R5,R3               READY FOR PRODUCT                            
*                                                                               
         TM    BILCLIH+4,X'20'                                                  
         BO    B4                                                               
         XC    CLIPROF,CLIPROF                                                  
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'10'      LOCKED CLIENT                                
         BO    ERROR                                                            
         MVC   BILCLIN,ACCTNAME                                                 
         OI    BILCLIH+4,X'20'                                                  
         OI    BILCLINH+6,X'80'                                                 
*                                                                               
B4       LA    R2,BILPROH                                                       
         BAS   RE,ANY                                                           
         TM    BILPROH+4,X'20'                                                  
         BO    B6                                                               
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         NI    BILJOBH+4,X'DF'                                                  
         MVC   BILPRON,SPACES                                                   
         OI    BILPRONH+6,X'80'                                                 
         MVC   BILJOBN,SPACES                                                   
         OI    BILJOBNH+6,X'80'                                                 
*                                                                               
B6       IC    R3,BILPROH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),BILPRO                                                   
         LA    R5,KEY+3                                                         
         IC    R3,PRDLNGTH         LEVB LENGTH                                  
         AR    R5,R3               READY FOR JOB                                
*                                                                               
         TM    BILPROH+4,X'20'                                                  
         BO    B8                                                               
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
         MVC   BILPRON,ACCTNAME                                                 
         OI    BILPROH+4,X'20'                                                  
         OI    BILPRONH+6,X'80'                                                 
*                                                                               
B8       MVC   PRODNUM,KEY                                                      
         MVC   PRODNAM,BILPRON                                                  
         OC    PRODNAM,SPACES                                                   
*                                                                               
         SR    R6,R6               FIND SALES ANALYSIS ELEMENT                  
         BAS   RE,HIGH                                                          
         LA    R1,IOAREA                                                        
*                                                                               
B8A      CLI   0(R1),0                                                          
         BE    B9                                                               
         CLI   0(R1),X'3D'                                                      
         BE    B8C                                                              
         ZIC   RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     B8A                                                              
*                                                                               
         USING ACSAND,R1                                                        
B8C      MVC   SAVEKEY,KEY                                                      
         MVC   KEY(15),ACSACODE                                                 
         MVC   PRODNUM,ACSACODE                                                 
         BAS   RE,GETACC                                                        
         MVC   PRODNAM,ACCTNAME                                                 
         MVC   KEY(15),SAVEKEY                                                  
*                                                                               
B9       DS    0H                                                               
         LA    R2,BILJOBH                                                       
         BAS   RE,ANY                                                           
         MVC   SAVEJOB,SPACES                                                   
         IC    R3,BILJOBH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),BILJOB                                                   
         MVC   SAVEJOB,KEY+6       PROD/JOB                                     
         TM    BILJOBH+4,X'20'                                                  
         BO    B10                                                              
         LA    R6,JOBPROF                                                       
         XC    JOBPROF,JOBPROF                                                  
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    ERROR                                                            
         MVI   ERRNUM,23                                                        
         TM    ACCTSTAT,X'20'                                                   
         BO    ERROR                                                            
         MVC   BILJOBN,ACCTNAME                                                 
         OI    BILJOBNH+6,X'80'                                                 
         OI    BILJOBH+4,X'20'                                                  
*                                                                               
B10      MVC   JOBNUM,KEY                                                       
         MVC   JOBNAME,BILJOBN     NAME FROM SCREEN                             
*                                                                               
         LA    RE,JOBNUM                                                        
         GOTO1 ASETJOB,DMCB,(RE)                                                
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    *+16                NO                                           
         LA    R2,BILJOBH          YES, STOP THEM                               
         MVI   ERRNUM,45                                                        
         B     ERROR                                                            
*                                                                               
         USING GOBLOCKD,RE                                                      
         L     RE,AGOBLOCK                                                      
         CLI   JOBFORM,C'N'        DO ONLY FOR NEW FORMAT JOB                   
         BNE   B11                                                              
         CLI   GOBATCH,C'A'        AND ONLY IF OPTION IS 'A'                    
         BNE   B11                                                              
         GOTO1 AOPTVAL                                                          
         BNE   ERROR                                                            
*                                                                               
B11      L     RE,AGOBLOCK                                                      
         MVC   VENDTYPE,GOTAXCOD   SAVE DEFAULT INFO                            
         MVC   VENDPROV,GOPSTPRV                                                
         MVC   VENDPSTT,GOPSTCOD                                                
         CLI   USERPROV,C'Y'                                                    
         BE    B11Z                                                             
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   B11Z                                                             
         MVI   BICPROVH+5,2                                                     
         OI    BICPROVH+6,X'80'                                                 
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6STRO        STEREO?                          
         BZ    B11G                                                             
         CLC   VENDPROV,SPACES                                                  
         BNH   B11Z                                                             
B11G     MVC   BICPROV,VENDPROV                                                 
         DROP  RE                                                               
*                                                                               
B11Z     BAS   RE,PROFMERG                                                      
         LA    R5,PROFILE                                                       
         USING ACPROFD,R5                                                       
         MVC   CSJOBNUM,ACPRCOST   COSTING SUSP/CLIENT A/C                      
         MVC   RECLINUM,ACPRRECV   RECEIVABLE / CLIENT A/C                      
         MVC   OFFICE,ACPROFFC                                                  
         LA    R2,BILCLIH                                                       
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),RECLINUM                                                 
         LA    R5,JOBPROF   USE JOB RECEIVABLE- IF PRESENT                      
         CLC   ACPRRECV+1(2),ACMPRECV                                           
         BNE   *+16                                                             
         MVC   RECLINUM,ACPRRECV                                                
         MVC   KEY(15),RECLINUM                                                 
         SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   RECLINAM,ACCTNAME                                                
         CLC   ACPROFFC,SPACES                                                  
         BNH   B12                 TAKE U. FOR A. FROM JOB PROFILE              
         MVC   OFFICE,ACPROFFC                                                  
         DROP  R5                                                               
*                                                                               
B12      MVC   DUMMNUM,SPACES                                                   
         MVC   DUMMNAME,SPACES                                                  
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'09'           GET MEDIA RECORD                             
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(1),BILJOB     MEDIA IS FIRST BYTE OF JOB                   
         BAS   RE,HIGH                                                          
         MVC   FVMSGNO,=Y(AE$MMR)  MISSING MEDIA RECORD'                        
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR               MEDIA RECORD MUST EXIST                      
*                                                                               
B12A     MVI   ELCODE,ACMDELQ      SEARCH FOR MEDIA ELEMENT                     
         L     R4,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                X'11' ELEMENT MUST EXIST                     
*                                                                               
         USING ACMEDIAD,R4                                                      
B12B     MVC   DUMMNUM+3(12),ACMDDESC+3                                         
         MVC   COSTAC,SPACES       CLEAR COSTING ACCOUNT                        
         CLI   ACMDLEN,ACMDLNQ2    USING EXPANDED ELEMENT?                      
         BL    B12C                                                             
         CLC   ACMDCOST,SPACES     EXPANDED ACCT MAY CONTAIN SPACES             
         BNH   B12C                                                             
         MVC   COSTAC,ACMDCOST     USE 12 BYTE COST ACCOUNT                     
         B     *+10                                                             
B12C     MVC   COSTAC(1),ACMDANAL  GET COST CODE FROM MEDIA                     
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACMDCOMM),ACMDCOMM  MOVE IN COMMISSION ACCOUNT             
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   COMMNUM,ACCTNUM                                                  
         MVC   COMMNAME,ACCTNAME   GET COMMISSION ACCOUNT NAME                  
*                                                                               
         LA    R2,BILCOMH          COMMISSION ACCOUNT                           
         CLI   5(R2),0             ANY INPUT FOR INCOME ACCT?                   
         BE    B13                 NO, PICK UP INFO FROM MEDIA RECORD           
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY      COMPANY CODE                                 
         MVC   KEY+1(2),=C'SI'     GET THE INCOME ACCOUNT                       
         MVC   KEY+3(12),BILCOM    SI ACCNT THAT THEY ENTERED                   
         OC    KEY+1(14),SPACES    PAD WITH BLANKS                              
*                                                                               
B12D     SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   COMMNUM,ACCTNUM     SAVE COMMISSION NAME TO USE                  
         MVC   COMMNAME,ACCTNAME                                                
*                                                                               
         MVI   ELCODE,ACSPELQ      LOOK FOR COST ACCOUNT OVERRIDE               
         L     R4,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
*                                                                               
         USING ACSPECD,R4                                                       
B12E     DS    0H                  2C COST ACCOUNT OVERRIDES 30 COSTING         
         CLI   ACSPTYP,ACSPOAN     X'03' IS TYPE INDICATOR FOR ANALYSIS         
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BNE   B12F                TAKE COST ACCT FROM 30 ELEMENT               
         B     B12E                                                             
         MVC   COSTAC,ACSPACCT                                                  
         B     B13                 NO COST OVERRIDE IF 2C ELEM PRESENT          
*                                                                               
B12F     MVC   COSTAC,SPACES                                                    
         MVI   ELCODE,ACSTELQ      SEARCH FOR STATUS ELEMENT                    
         L     R4,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                30 ELEMENT MUST BE PRESENT                   
*                                                                               
         USING ACSTATD,R4                                                       
B12G     CLC   COSTAC,SPACES                                                    
         BNE   *+10                                                             
         MVC   COSTAC(1),ACSTCOST                                               
         OC    ACSTCNTR,SPACES     COSTING OVERRIDE IN EFFECT?                  
         CLC   ACSTCNTR,SPACES     IF ALL SPACES THEN NO OVERRIDE               
         BE    B13                                                              
*                                                                               
         CLI   ACSTCPOS,0          IF 0 START AT CSJOBNUM+7                     
         BE    *+20                                                             
         LA    R5,CSJOBNUM+2                                                    
         ZIC   R0,ACSTCPOS         INSERT STARTING POINT NUMBER                 
         AR    R5,R0               NEW STARTING POINT                           
         B     *+8                                                              
         LA    R5,CSJOBNUM+7       COSTING ACCOUNT                              
         LA    R3,3                                                             
         LA    R4,ACSTCNTR                                                      
*                                                                               
         CLI   0(R4),C' '          REPLACE ONLY NON-BLANK                       
         BE    *+10                                                             
         MVC   0(1,R5),0(R4)                                                    
         LA    R5,1(R5)                                                         
         LA    R4,1(R4)                                                         
         BCT   R3,*-22                                                          
*                                                                               
B13      MVC   BILCOM,COMMNUM+3    DISPLAY COMMISSION ACCT                      
         OI    BILCOMH+6,X'80'                                                  
         MVC   BILCOMN,COMMNAME    DISPLAY COMMISSION ACCT NAME                 
         OI    BILCOMNH+6,X'80'                                                 
*                                                                               
B13A     LA    R2,BILDUMH                                                       
         CLI   5(R2),0                                                          
         BE    B14                                                              
         MVC   DUMMNUM,SPACES                                                   
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DUMMNUM+3(0),BILDUM                                              
*                                                                               
B14      LA    R2,BILCLIH                                                       
         TM    COMPSTAT,X'10'      COSTING REQUIRED                             
         BZ    B15                                                              
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),CSJOBNUM                                                 
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CSJOBNAM,ACCTNAME                                                
*                                                                               
         MVC   KEY+2(15),SPACES                                                 
         MVC   KEY+1(2),=C'12'     HARD U/L FOR COSTING COMMISSIONS             
         MVC   KEY+3(12),COSTAC    COSTING CODE                                 
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CNCOMNUM,ACCTNUM                                                 
         MVC   CNCOMNAM,ACCTNAME                                                
*                                                                               
         MVI   KEY+2,C'1'          BILLINGS LEDGER                              
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CNBILNUM,ACCTNUM                                                 
         MVC   CNBILNAM,ACCTNAME                                                
*                                                                               
B15      MVI   ERRNUM,INVAMNT      INVALID AMOUNT                               
         LA    R2,BILAMTH                                                       
         TM    4(R2),X'20'                                                      
         BO    B15A                                                             
         IC    R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'82',BILAMT),(R3)                                 
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         ZAP   AMT,4(8,R1)                                                      
         OI    4(R2),X'20'                                                      
*                                                                               
B15A     LA    R2,BILNETH                                                       
         TM    4(R2),X'20'                                                      
         BO    B15B                                                             
         IC    R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'82',BILNET),(R3)                                 
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         ZAP   NET,4(8,R1)                                                      
         OI    4(R2),X'20'                                                      
*                                                                               
B15B     LA    R2,BILCAMH                                                       
         TM    4(R2),X'20'                                                      
         BO    B15C                                                             
         IC    R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'82',BILCAM),(R3)                                 
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         ZAP   COMM,4(8,R1)                                                     
         OI    4(R2),X'20'                                                      
*                                                                               
B15C     ZAP   GST,=P'0'                                                        
         ZAP   PST,=P'0'                                                        
*                                                                               
         CLI   AGYCTRY,CTRYCAN     TEST CANADIAN SCREEN                         
         BNE   B15C20              NO                                           
         BAS   RE,EDBASIS          EDIT BASIS                                   
         BNE   ERROR                                                            
*                                                                               
         CLI   PFKEY,7                                                          
         BNE   B15C10                                                           
         CLI   MODE,1                                                           
         BNH   B0A0070                                                          
*                                                                               
B15C10   MVI   MODE,4                                                           
         GOTO1 ACTAXMOD,DMCB,(RC)                                               
         CLI   CTXMODE,C'Z'                                                     
         BNE   B15C20                                                           
         MVC   FVMSGNO,CTXMSGNO                                                 
         MVI   FVOMTYP,C'E'                                                     
         L     R2,FVADDR                                                        
         B     ERROR                                                            
*                                                                               
B15C20   LA    R2,BILAMTH                                                       
         ZAP   DUB,NET                                                          
         AP    DUB,COMM                                                         
         AP    DUB,GST                                                          
         AP    DUB,PST                                                          
         CP    DUB,AMT                                                          
         BE    B15C30                                                           
         MVC   FVMSGNO,=Y(AE$INAMT)                                             
         B     ERROR                                                            
*                                                                               
B15C30   ZAP   CD,=P'0'                                                         
         LA    R2,BILCDH                                                        
         CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   *+8                                                              
         LA    R2,BICCDH                                                        
*                                                                               
         CLI   5(R2),0                                                          
         BE    B15C2                                                            
         IC    R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R3)                                  
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         ZAP   CD,4(8,R1)                                                       
*                                                                               
B15C2    MVI   ERRNUM,INVDATE      DUE DATE                                     
         LA    R2,BILDUEH          FOR CURSOR REPOSITION ON ERROR               
         XC    DUEDAT,DUEDAT                                                    
         CLI   BILDUEH+5,0                                                      
         BE    B15E                                                             
         GOTO1 DATVAL,DMCB,(0,BILDUE),DUEDAT                                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR               BAD DATE                                     
         CLC   DUEDAT(2),=C'78'    DON'T ALLOW PRIOR TO 1978                    
         BL    ERROR                                                            
         EJECT                                                                  
*------------------------------------------------------------                   
*        BUILD 64 ELEMENT                                                       
*------------------------------------------------------------                   
*                                                                               
B15E     DS    0H                  BUILD MEDIA TRANSFER ELEMENT                 
         LA    R4,MTEL                                                          
         USING ACMTD,R4                                                         
         XC    0(ACMTLNQ,R4),0(R4)                                              
         MVI   ACMTEL,ACMTELQ      ADD MEDIA TRANSFER FOR TO SI POSTING         
         MVI   ACMTLEN,ACMTLNQ                                                  
         MVI   ACMTSYS,C'J'                                                     
         MVC   ACMTMED,JOBNUM+9      MEDIA CODE                                 
         MVC   ACMTCLI(12),JOBNUM+3  CLI/PRD/JOB                                
         MVC   ACMTMOS,PMOS                                                     
         MVC   ACMTDSCP,JOBNAME                                                 
         OC    ACMTDSCP,SPACES                                                  
         CVB   R0,AMT                                                           
         STCM  R0,15,ACMTGRS         BILLING (GROSS)                            
         STCM  R0,15,ACMTRECV        AND RECEIVABLE                             
         CVB   R0,COMM                                                          
         STCM  R0,15,ACMTCOM         INCOME                                     
         CVB   R0,NET                                                           
         STCM  R0,15,ACMTNET         PAYABLE                                    
         CVB   R0,CD                                                            
         STCM  R0,15,ACMTCD          CASH DISCOUNT                              
         CVB   R0,GST                                                           
         STCM  R0,15,ACMTVAT       VAT/GST AMOUNT                               
*                                                                               
         LA    RE,IOAREA                                                        
         LA    RF,L'IOAREA                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,DLDSELQ                                                   
         LA    R2,BILBILLH                                                      
         BAS   RE,ANY                                                           
         MVC   DLDSREF,SPACES                                                   
         ZIC   R3,BILBILLH+5                                                    
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DLDSREF(0),BILBILL                                               
*                                                                               
B16      MVC   DLDSDATE,BILLDATE                                                
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         MVC   DLDSNARR(8),=C'BILL NO '                                         
         MVC   DLDSNARR+8(6),SPACES                                             
         LA    R2,BILBILLH                                                      
         ZIC   R3,BILBILLH+5                                                    
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DLDSNARR+8(0),BILBILL                                            
         LA    R3,9(R3)            L' CONSTANT + BILL NO                        
         LA    R4,DLDSNARR         ADD BILLING INFORMATION                      
         AR    R4,R3                                                            
         MVI   0(R4),X'FF'         DELIMITER                                    
         LA    R4,1(R4)                                                         
         MVC   0(15,R4),=CL15'MANUAL BILLING'                                   
         MVC   15(78,R4),=13PL6'0'          FILL IN LATER                       
         ZAP   15(6,R4),COMM       COMMISSION AMT                               
         ZAP   21(6,R4),CD         CASH DISCOUNT                                
         ZAP   27(6,R4),NET        BILLING(AT ZERO VAT)                         
         AP    27(6,R4),COMM                                                    
         MVI   57(R4),TRNBTMAN     SET BILLING TYPE                             
         LA    R3,94(R3)                                                        
*                                                                               
         LA    R4,DLDSNARR                                                      
         SR    R4,R8               L' ELEMENT - NARRATIVE                       
         AR    R4,R3                                                            
         STC   R4,DLDSLEN                                                       
         SR    R3,R3                                                            
         ZIC   R3,DLDSLEN                                                       
         AR    R8,R3                                                            
         EJECT                                                                  
*------------------------------------------------------------                   
*        BUILD 4 MAIN ACCOUNTING ENTRIES                                        
*------------------------------------------------------------                   
*                                                                               
BIL02    CLI   AGYCTRY,CTRYCAN     TEST FOR CANADA                              
         BNE   BIL04               NO                                           
         CLI   GSTSW,C'Y'          TEST FOR GST IN EFFECT                       
         BNE   BIL04               NO                                           
*                                                                               
         L     R1,AVTC                                                          
         USING VTCD,R1                                                          
         OC    VTCACT,VTCACT       IS THERE ANY VAT INFO?                       
         BZ    BIL03               NO, DON'T BUILD VBIEL                        
*                                                                               
         USING VBIELD,R8                                                        
         XC    VBIEL(VBILNQ),VBIEL BUILD VAT BILLED ELEMENT                     
         MVI   VBIEL,VBIELQ                                                     
         MVI   VBILN,VBILNQ                                                     
         MVC   VBITYPE,VTCTYPE                                                  
         MVC   VBIRATE,VTCRATE                                                  
         MVC   VBIINDS,VTCINDS                                                  
         MVC   VBIACCT,VTCACT                                                   
         MVC   VBIDATE,VTCEFFD                                                  
         ZAP   VBIVAT,GST                                                       
         ZAP   VBIGROSS,AMT        GROSS BILLED                                 
         ZAP   VBICOMM,COMM        COMMISSION                                   
         ZIC   R3,VBILN                                                         
         AR    R8,R3                                                            
*                                                                               
BIL03    LA    R1,PSTBLK                                                        
         OC    VTCACT,VTCACT       DO WE HAVE PST?                              
         BZ    BIL04               NO, DON'T BUILD PBIEL                        
*                                                                               
         USING PBIELD,R8                                                        
         XC    PBIEL(PBILNQ),PBIEL        BUILD PST BILLED ELEMENT              
         MVI   PBIEL,PBIELQ                                                     
         MVI   PBILN,PBILNQ                                                     
         MVC   PBITYPE,VTCTYPE                                                  
         MVC   PBIRATE,VTCRATE                                                  
         MVC   PBIINDS,VTCINDS                                                  
         MVC   PBIPRV,VTCPRV                                                    
         MVC   PBIACCT,VTCACT                                                   
         MVC   PBIDATE,VTCEFFD                                                  
         ZAP   PBIPST,PST                                                       
         ZAP   PBIGROSS,AMT        GROSS BILLED                                 
         ZAP   PBICOMM,COMM        COMMISSION                                   
         ZIC   R3,PBILN                                                         
         AR    R8,R3                                                            
*                                                                               
         USING TRDUED,R8                                                        
BIL04    OC    DUEDAT,DUEDAT       IF DUEDATE CREATE A 61                       
         BZ    BIL06                                                            
         MVC   TRDUEL(2),=X'6104'                                               
         GOTO1 DATCON,DMCB,(0,DUEDAT),(2,TRDUDATE)                              
         ZIC   R3,TRDUEN                                                        
         AR    R8,R3                                                            
*                                                                               
         USING BSCELD,R8                                                        
BIL06    MVI   BSCEL,BSCELQ                                                     
         MVI   BSCLN,BSCLNQ                                                     
         MVC   BSCBSRC,DUMMNUM+3                                                
         OC    BSCBSRC,SPACES                                                   
         MVC   BSCBOFF,OFFICE                                                   
         ZIC   R3,BSCLN                                                         
         AR    R8,R3                                                            
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     SINGLE CREDIT                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,RECLINUM                                                
         MVC   DLPSDBNM,RECLINAM                                                
         MVC   DLPSCRAC,JOBNUM                                                  
         MVC   DLPSCRNM,JOBNAME                                                 
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,=C'99'                                                  
         ZAP   DLPSAMNT,NET                                                     
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
*------------------------------------------------------------                   
*        SET UP THE SI POSTING                                                  
*------------------------------------------------------------                   
*                                                                               
         USING ACMTEL,R4                                                        
         LA    R4,MTEL                                                          
         IC    R3,ACMTLEN                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),ACMTEL        ADD MEDIA TRANSFER DATA                    
         LA    R8,1(R3,R8)                                                      
*                                                                               
         USING TRCASHD,R8                                                       
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'G'                                                    
         ZAP   TRCSAMNT,AMT                                                     
         CLI   BILCOM,C'*'         IF POSTING IS TO SK                          
         BNE   *+10                MEMO MUST BE ZERO                            
         ZAP   TRCSAMNT,=P'0'                                                   
         ZIC   R3,TRCSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         CLI   AGYCTRY,CTRYCAN     CANADIAN AGENCY?                             
         BNE   *+8                 NO, DON'T TAX THEM AS SUCH                   
         BAS   RE,PTAXES           POST CANADIAN TAX                            
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ                                                  
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,PRODNUM                                                 
         CLI   BILCOM,C'*'         SK OVERRIDE                                  
         BNE   BIL08                                                            
         LA    R1,BILJOBH          APPEND JOB CODE INPUT AFTER CLT/PRD          
         ZIC   R5,5(R1)                                                         
         BCTR  R5,0                                                             
         LA    R2,9                                                             
         LA    R2,DLPSDBAC(R2)     =POSITION PAST CLT/PRD                       
         EX    R5,*+8                                                           
         B     BIL08                                                            
         MVC   0(0,R2),8(R1)                                                    
*                                                                               
BIL08    MVC   DLPSDBNM,PRODNAM                                                 
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSCRAC,COMMNUM                                                 
         MVC   DLPSCRNM,COMMNAME                                                
         ZAP   DLPSAMNT,COMM                                                    
         MVC   DLPSANAL,OFFICE     UNIT FOR ANALYSIS                            
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
BIL10    CLI   AGYCTRY,CTRYCAN     TEST CANADIAN SCREEN                         
         BNE   BIL15               NO                                           
         CLI   GSTSW,C'Y'          TEST FOR GST APPLICABLE                      
         BNE   BIL15                                                            
*                                                                               
         L     R2,AIOA                                                          
         ZICM  RF,0(R2),2                                                       
         LTR   RF,RF               NOTHING                                      
         BZ    BIL15                                                            
         SH    RF,=H'2'            SUBTRACT LENGTH                              
         LA    RE,2(R2)            COPY POSTINGS                                
         LR    R0,R8                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         LR    R8,R0                                                            
*                                                                               
*------------------------------------------------------------                   
*        SET UP THE SR POSTING                                                  
*------------------------------------------------------------                   
*                                                                               
         USING ACMTEL,R4                                                        
BIL15    LA    R4,MTEL                                                          
         IC    R3,ACMTLEN                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),ACMTEL        ADD MEDIA TRANSFER DATA                    
         LA    R8,1(R3,R8)                                                      
*                                                                               
         USING ACOTHERD,R8                                                      
         MVC   ACOTEL(2),=X'230F'  OTHERS ELEMENT FOR JOB NUMBER                
         MVC   ACOTNUM(13),SPACES                                               
         MVC   ACOTNUM,SAVEJOB                                                  
         MVI   ACOTPROF,C'J'                                                    
         ZIC   R3,ACOTLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         CP    CD,=P'0'                                                         
         BE    BIL20                                                            
         USING TRCASHD,R8                                                       
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         ZAP   TRCSAMNT,CD                                                      
         ZIC   R3,TRCSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         USING TRCASHD,R8                                                       
BIL20    MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'I'       ADD INCOME TO RECEIVABLE                     
         ZAP   TRCSAMNT,COMM       POSTING                                      
         CLI   BILCOM,C'*'         IF INCOME IS POSTING TO SK                   
         BNE   *+10                                                             
         ZAP   TRCSAMNT,=P'0'      NO MEMO INCOME ON RECEIVABLES                
         ZIC   R3,TRCSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         USING TRDUED,R8                                                        
         OC    DUEDAT,DUEDAT       IF DUEDATE CREATE A 61                       
         BZ    BIL25                                                            
         MVC   TRDUEL(2),=X'6104'                                               
         GOTO1 DATCON,DMCB,(0,DUEDAT),(2,TRDUDATE)                              
         ZIC   R3,TRDUEN                                                        
         AR    R8,R3                                                            
*                                                                               
         USING DLPOSTD,R8                                                       
BIL25    DS    0H                                                               
         MVI   DLPSEL,DLPSEDRQ     DEBIT                                        
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSCRAC,DUMMNUM                                                 
         MVC   DLPSCRNM,DUMMNAME                                                
         MVC   DLPSDBAC,RECLINUM                                                
         MVC   DLPSDBNM,RECLINAM                                                
         MVC   DLPSANAL,OFFICE                                                  
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,AMT                                                     
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         TM    COMPSTAT,X'10'      COSTING REQUIRED                             
         BZ    RECADD              NO ADD RECORD                                
         CLI   BILCOM,C'*'         NO ANALYSIS POSTINGS FOR SK                  
         BE    BIL35                                                            
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDCQ     DOUBLE POSTING                               
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBNM,CSJOBNAM                                                
         MVC   DLPSDBAC,CSJOBNUM                                                
         MVC   DLPSCRAC,CNCOMNUM                                                
         MVC   DLPSCRNM,CNCOMNAM                                                
         MVI   DLPSTYPE,X'80'                                                   
         MVC   DLPSANAL,OFFICE                                                  
         ZAP   DLPSAMNT,COMM                                                    
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
BIL35    MVI   DLPSEL,DLPSEDCQ                                                  
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBNM,CSJOBNAM                                                
         MVC   DLPSDBAC,CSJOBNUM                                                
         MVC   DLPSCRAC,CNBILNUM                                                
         MVC   DLPSCRNM,CNBILNAM                                                
         ZAP   DLPSAMNT,AMT                                                     
         MVC   DLPSANAL,OFFICE                                                  
         MVI   DLPSTYPE,X'80'                                                   
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
RECADD   MVI   0(R8),0                                                          
         LA    R8,1(R8)                                                         
         LA    R3,IOAREA                                                        
         SR    R8,R3                                                            
         STH   R8,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         BAS   RE,PUTDAY                                                        
*                                                                               
         XC    WORK,WORK                                                        
         SR    R3,R3                                                            
         ZIC   R3,BILBILLH+5                                                    
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),BILBILL                                                  
         ZAP   TRANSAMT,AMT                                                     
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)                                                 
*                                                                               
         BAS   RE,ADSCRINF                                                      
         MVI   GSTSW,C'N'                                                       
         XC    KEEPPROV,KEEPPROV                                                
         XC    BASIS,BASIS                                                      
         XC    PBASIS,PBASIS                                                    
         MVI   CLEAROK,C'Y'                                                     
         MVI   USERPROV,C'N'                                                    
         MVI   USERPST,C'N'                                                     
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   BIL90                                                            
         MVI   BICPROVH+4,0                                                     
         MVC   WORK,SPACES                                                      
         CLI   BICBASEH+5,0                                                     
         BE    BIL80                                                            
         MVC   WORK(L'BICBASE),BICBASE                                          
         MVI   BICBASE,C'*'                                                     
         MVC   BICBASE+1(L'BICBASE),WORK                                        
         OI    BICBASEH+6,X'80'                                                 
         OI    BICPBASH+6,X'80'                                                 
BIL80    MVC   WORK,SPACES                                                      
         CLI   BICPBASH+5,0                                                     
         BE    BIL90                                                            
         MVC   WORK(L'BICPBAS),BICPBAS                                          
         MVI   BICPBAS,C'*'                                                     
         MVC   BICPBAS+1(L'BICPBAS),WORK                                        
         OI    BICPBASH+6,X'80'                                                 
         OI    BICPBASH+6,X'80'                                                 
*                                                                               
BIL90    LA    R2,BILBILLH                                                      
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
*                                                                               
         USING TRCASHD,R8                                                       
PTAXES   MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'T'                                                    
         ZAP   TRCSAMNT,GST                                                     
         LA    R8,TRCSLNQ1(R8)                                                  
*                                                                               
         CLI   BICPROVH+5,0        HAS TO BE A PROVINCE                         
         BZ    PTAXXIT                                                          
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'Q'                                                    
         ZAP   TRCSAMNT,PST                                                     
         LA    R8,TRCSLNQ1(R8)                                                  
PTAXXIT  BR    RE                                                               
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
EDBASIS  NTR1                                                                   
         CLI   MODE,1              HAS TO BE IN TYPE 6 SCREEN                   
         BH    EDBASEXX                                                         
         MVI   ERRNUM,25           INVALID AMOUNT                               
         MVI   CTXMODE,C'G'        MODE TO LOOP AROUND FOR PST                  
*                                                                               
         LA    R4,BASIS                                                         
         LA    R2,BICBASEH                                                      
EDBASLP  CLI   CTXMODE,C'G'                                                     
         BE    *+12                                                             
         LA    R4,PBASIS                                                        
         LA    R2,BICPBASH                                                      
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
         EJECT                                                                  
*                                                                               
ADSCRINF NTR1                                                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    RF,XTRAELM                                                       
         GOTO1 AADACDAY,BOPARM,(X'80',IOAREA),BOPL61,BOWORK1,(RF)               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERRXIT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------                   
*        ACBATCODE                                                              
*------------------------------------------------------------                   
*                                                                               
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*------------------------------------------------------------                   
*        LITERAL DECLARATIONS                                                   
*------------------------------------------------------------                   
*                                                                               
NOCTAX   DC    C'** ERROR - CANADIAN SCREEN NOT AVAILABLE'                      
         LTORG                                                                  
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
         AP    AMTBLK+2(6),COMM                                                 
         LA    RF,7                                                             
         LA    RE,AMTBLK+8         CLEAR THE REST                               
CTAX13   MVC   0(2,RE),SPACES                                                   
         ZAP   2(6,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   RF,CTAX13                                                        
*                                                                               
         MVC   CTXACC(1),COMPANY                                                
         MVC   CTXOFF,OFFICE                                                    
         MVI   CTXGORN,C'N'                                                     
         MVC   CTXPROV,BICPROV                                                  
         MVC   CTXDATE,BILLDATE                                                 
         MVC   CTXVENGT,VENDTYPE                                                
         MVC   CTXVENPT,VENDPSTT                                                
         MVC   CTXGSTT,BICTYPE                                                  
         MVC   CTXGSTTN,BICTYPN                                                 
         LA    R2,BICGAMTH                                                      
         MVC   CTXLGSTA,5(R2)                                                   
         MVC   CTXGSTA,8(R2)                                                    
         MVC   CTXGBASE,BASIS                                                   
         MVC   CTXPBASE,PBASIS                                                  
         MVC   CTXUPSTT,USERPST                                                 
         MVC   CTXUPROV,USERPROV                                                
*                                                                               
CTAX15   MVC   CTXCNTRA,JOBNUM     CONTRA-ACCOUNT IS JOB                        
         MVC   CTXCNTRN,JOBNAME                                                 
*                                                                               
CTAX50   LA    R3,X'41'            CTAX SCREEN                                  
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         MVC   DMCB+12(4),AVTC                                                  
         LA    R3,PSTBLK                                                        
         ST    R3,DMCB+16                                                       
         MVC   DMCB+12(4),AVTC                                                  
         GOTO1 (RF),DMCB,(X'06',CTXDATA),(R9),AMTBLK                            
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
CTAX61   LA    R2,BICPROVH                                                      
         MVC   8(2,R2),CTXPROV                                                  
         OI    6(R2),X'80'                                                      
         LA    R2,BICTYPEH                                                      
         MVC   8(1,R2),CTXGSTT                                                  
         OI    6(R2),X'80'                                                      
         LA    R2,BICTYPNH                                                      
         MVC   8(21,R2),CTXGSTTN                                                
         OI    6(R2),X'80'                                                      
         LA    R2,BICGAMTH                                                      
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
         LA    R2,BICTYPEH                                                      
         ST    R2,FADR                                                          
         CLI   CTXMODE,C'G'        EXCEPT FOR 1 PASS                            
         BE    CTAXXIT                                                          
         OI    6(R2),X'01'         MODIFIED FIELD, TO HIT ENTER                 
*                                                                               
         CLI   CTXMODE,C'Z'                                                     
         BNE   CTAXXIT                                                          
         LA    R2,BICTYPEH                                                      
         CLI   CTXERR,2                                                         
         BNE   *+8                                                              
         LA    R2,BICGAMTH                                                      
         CLI   CTXERR,3                                                         
         BNE   *+8                                                              
         LA    R2,BICPROVH                                                      
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
*------------------------------------------------------------                   
*        ACBATDSECT / ACBATF9D                                                  
*------------------------------------------------------------                   
*                                                                               
       ++INCLUDE ACBATDSECT                                                     
       ++INCLUDE ACBATF9D                                                       
         ORG   TWAHOLE                                                          
AMT      DS    D                                                                
COMM     DS    D                                                                
NET      DS    D                                                                
GST      DS    D                                                                
PST      DS    D                                                                
CD       DS    D                                                                
COSTAC   DS    CL12                COST ANALYSIS ACCOUNT                        
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
PSTBLK   DS    CL(VTCLNQ)                                                       
         EJECT                                                                  
         ORG   CONTABH                                                          
       ++INCLUDE ACBATE2D                                                       
         EJECT                                                                  
*------------------------------------------------------------                   
*        WORK STORAGE DSECT                                                     
*------------------------------------------------------------                   
*                                                                               
PROGD    DSECT                                                                  
JOBNUM   DS    CL15                                                             
PRODNUM  DS    CL15                                                             
PRODNAM  DS    CL36                                                             
JOBNAME  DS    CL36                                                             
DUMMNUM  DS    CL15                                                             
DUMMNAME DS    CL36                                                             
CSJOBNUM DS    CL15                                                             
CSJOBNAM DS    CL36                                                             
RECLINUM DS    CL15                                                             
RECLINAM DS    CL36                                                             
COMMNUM  DS    CL15                INCOME  ACCOUNT SI OR SK                     
COMMNAME DS    CL36                                   - NAME                    
CNCOMNUM DS    CL15                                                             
CNCOMNAM DS    CL36                                                             
CNBILNUM DS    CL15                                                             
CNBILNAM DS    CL36                                                             
BILCASH  DS    CL6                                                              
OFFICE   DS    CL2                                                              
ELCODE   DS    XL1                 USED FOR GETEL ROUTINE                       
SAVEKEY  DS    CL15                                                             
SAVEJOB  DS    CL9                                                              
BILLDATE DS    PL3                                                              
DUEDAT   DS    CL6                                                              
BASIS    DS    D                   OPTIONAL GST BASIS OVERRIDE                  
PBASIS   DS    D                   OPTIONAL PST BASIS OVERRIDE                  
MTEL     DS    CL(ACMTLNQ)         MEDIA SUMMARY ELEMENT                        
RELO     DS    A                                                                
ACTAXMOD DS    A                                                                
VENDTYPE DS    CL1                 GST TAX CODE FROM OPT MAINT                  
VENDPROV DS    CL2                 PROVINCE CODE FROM OPT MAINT                 
VENDPSTT DS    CL1                 PST TAX CODE FORM OPT MAINT                  
       ++INCLUDE ACBATCTAX                                                      
TMPMODE  DS    CL1                                                              
KEY      DS    CL49                                                             
IOAREA   DS    CL2000                                                           
PROGDX   DS    0C                                                               
         EJECT                                                                  
*------------------------------------------------------------                   
*        EXTERNAL INCLUDES                                                      
*------------------------------------------------------------                   
*                                                                               
*        ACGENBOTH                                                              
*        ACGENDAY                                                               
*        ACGENFILE                                                              
*        DDFLDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'081ACBAT06   12/04/12'                                      
         END                                                                    
