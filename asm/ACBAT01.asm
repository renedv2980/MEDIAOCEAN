*          DATA SET ACBAT01    AT LEVEL 066 AS OF 12/10/14                      
*PHASE T61B01A                                                                  
*INCLUDE RIGHT                                                                  
         TITLE 'INVOICE - TYPE 1'                                               
T61B01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,*BAT01*,R7,CLEAR=YES,RR=R2                          
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         L     R1,=V(RIGHT)                                                     
         AR    R1,R2                                                            
         ST    R1,RIGHT                                                         
         ST    R2,RELO1                                                         
*                                                                               
* BUILD SCREEN DIRECTORY                                                        
*                                                                               
* CANADIAN SCREEN UP TO GST FIELDS MUST MATCH US SCREEN TO ASSURE               
* CORRECT PURCHASE ORDER HANDLING BY 07 OVERLAY                                 
*                                                                               
         LA    R1,USTAB                                                         
         CLI   AGYCTRY,CTRYUSA     TEST FOR US SCREEN                           
         BE    *+8                                                              
         LA    R1,CANTAB           NO-ITS CANADIAN                              
*                                                                               
PIC002   CLI   0(R1),X'FF'         TEST FOR EOT                                 
         BE    PIC002A                                                          
         LM    RE,RF,0(R1)                                                      
         LA    RE,TWAD(RE)         FORM READ ADDRESS OF SCREEN FIELD            
         LA    RF,PROGD(RF)        FORM ADDRESS OF ADCON                        
         ST    RE,0(RF)                                                         
         LA    R1,L'USTAB(R1)                                                   
         B     PIC002                                                           
*                                                                               
PIC002A  GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         MVC   TMPMODE,MODE                                                     
         NI    TMPMODE,X'0F'                                                    
         CLI   TMPMODE,1           PROCESSING OTHER SCREENS                     
         BH    PIC002C             DON'T CHECK DATES                            
         L     R2,ADATH                                                         
         MVI   ERRNUM,13                                                        
         CLI   5(R2),0                                                          
         BE    PIC002B                                                          
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         B     *+8                                                              
PIC002B  BAS   RE,GETODAY                                                       
PIC002C  GOTO1 DATCON,DMCB,(0,WORK),(1,SAVEDATE)                                
         GOTO1 DATECHK,DMCB,SAVEDATE                                            
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
*                                                                               
         CLI   CSACT,ACTCHA                                                     
         BNE   *+16                                                             
         MVI   CLEAROK,C'N'        DON'T CLEAR GST/PST FOR CHANGE               
         MVI   USERPROV,C'Y'       USER ENTERED PROV FROM CHANGE                
         MVI   USERPST,C'Y'        USER ENTERED PST TYPE FOR CHANGE             
* CLEAR GST/PST FIELDS                                                          
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   PIC002X                                                          
         CLI   MODE,2              CANNOT CLEAR WHEN IN ANOTHER                 
         BE    PIC002X               PROGRAM                                    
         CLI   MODE,3                                                           
         BE    PIC002X                                                          
         CLI   CLEAROK,C'N'        NOT OK TO CLEAR?                             
         BE    PIC002G                                                          
         L     R2,AGORNH                                                        
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BO    PIC002D             ONLY CLEAR THE OLD ONES                      
         OI    6(R2),X'80'                                                      
         MVI   8(R2),0                                                          
         L     R2,AGSTXH                                                        
         MVC   8(L'INCGSTX,R2),SPACES        CLEAR INFO DATA TOO                
         OI    6(R2),X'80'                                                      
         L     R2,ATYPNH                                                        
         MVC   8(L'INCGTYN,R2),SPACES                                           
         OI    6(R2),X'80'                                                      
*                                                                               
PIC002D  L     R2,ATYPEH                                                        
         TM    4(R2),X'80'                                                      
         BO    *+12                                                             
         MVI   8(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AGAMTH                                                        
         TM    4(R2),X'80'                                                      
         BO    *+18                                                             
         OI    6(R2),X'80'                                                      
         MVI   5(R2),0                                                          
         XC    8(40,R2),8(R2)                                                   
         XC    XTRAELM,XTRAELM     CLEAR PST FIELDS                             
*                                                                               
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6STRO        STEREO?                          
         BZ    PIC002E                                                          
         L     R2,APROVH                                                        
         TM    4(R2),X'80'         NEW PROVINCE?                                
         BO    PIC002J                                                          
         MVI   USERPROV,C'N'                                                    
         B     PIC002K                                                          
*                                                                               
PIC002E  L     R2,ASUPH                                                         
         TM    4(R2),X'80'                                                      
         BNO   PIC002G                                                          
         XC    VENDPROV,VENDPROV                                                
         L     R2,APROVH                                                        
         TM    4(R2),X'80'                                                      
         BO    PIC002G                                                          
         XC    8(2,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
PIC002G  L     R2,APROVH                                                        
         TM    4(R2),X'80'         NEW PROVINCE?                                
         BNO   PIC002X             SAVE PROVINCE                                
PIC002J  MVI   USERPROV,C'Y'       USER ENTERED PROVINCE                        
PIC002K  MVC   KEEPPROV,8(R2)                                                   
         XC    XTRAELM,XTRAELM                                                  
*                                                                               
PIC002X  MVI   CLEAROK,C'N'        DON'T CLEAR UNTIL DONE                       
         L     R2,RELO1                                                         
*                                                                               
*--------------------------------------------------------------                 
*        MODE=X'00' EDIT SCREEN - INPUT                                         
*        MODE=X'01' EDIT SCREEN FROM P.O. RECALL                                
*        MODE=X'02' EDIT SALES/USE SCREEN                                       
*        MODE=X'03' EDIT CANADIAN SALES TAX SCREEN                              
*        MODE=X'80' VALID CASH AMOUNT FROM LAST INPUT                           
*--------------------------------------------------------------                 
PIC003   CLI   CSACT,ACTCHA        ENTERING FROM ITEM CHANGE?                   
         BNE   PIC003A             NO                                           
         CLI   TWASCRN,BTS201U     YES, CHANGING TAX SCREEN?                    
         BE    PIC005              YES, CALL TAXMOD                             
         CLI   PFKEY,X'FF'         FIRST TIME, IT SETS PKEY TO FF               
         BNE   *+8                 YES                                          
         MVI   PFKEY,0                                                          
         CLI   PFKEY,9             THIS KEY INVALID FOR CHANGE                  
         BNE   *+8                                                              
         MVI   PFKEY,0                                                          
*                                                                               
PIC003A  CLI   MODE,2              SALES TAX                                    
         BE    PIC004                                                           
         CLI   MODE,3              CANADIAN TAX                                 
         BE    PIC005C                                                          
*                                                                               
         TM    MODE,X'80'          VALID AMOUNT FROM LAST INPUT                 
         BO    *+10                                                             
         ZAP   LCSHTOT,=P'0'       IF NOT - INITIALIZE LAST AMOUNT              
         NI    MODE,X'FF'-X'80'                                                 
         CLI   PFKEY,7             USE PF=7 TO LAOD                             
         BNE   PIC003B             AND EDIT CANADIAN TAX SCREEN                 
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    PIC003A1                                                         
         MVI   ERRNUM,SPECIAL                                                   
         LA    R2,CONACTH                                                       
         MVC   FVMSGNO,=Y(AE$CSNA)                                              
         B     ERROR                                                            
*                                                                               
PIC003A1 ZAP   TOTNET,=P'0'                                                     
         ZAP   TOTGRS,=P'0'                                                     
         ZAP   TOTGST,=P'0'                                                     
         ZAP   TOTPST,=P'0'                                                     
         B     PIC008                                                           
*                                                                               
PIC003B  CLI   PFKEY,9             USE PF=9 TO LOAD                             
         BNE   PIC006              AND EDIT SALES/USE TAX SCREEN                
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   PIC004                                                           
         MVI   ERRNUM,SPECIAL                                                   
         LA    R2,CONACTH                                                       
         MVC   FVMSGNO,=Y(AE$INVPF)                                             
         B     ERROR                                                            
*                                                                               
PIC004   TM    JOBSTAT,ACOXJOB     IS THIS AN X-JOB ?                           
         BZ    PIC005              NO                                           
         MVI   ERRNUM,47           YES, PRINT AN MESSAGE                        
         L     R2,TIACURS                                                       
         B     ERROR                                                            
*                                                                               
PIC005   L     RF,=A(TAXMOD)                                                    
         AR    RF,R2                                                            
         MVI   CSSPROG,1           RESET THE PF KEYS                            
         GOTO1 (RF),DMCB,(RC)                                                   
         B     CURSIT                                                           
*                                                                               
PIC005C  CLI   AGYCTRY,CTRYCAN     CANADIAN?                                    
         BNE   PIC006                                                           
         L     RF,=A(CTAXMOD)      DO CANADIAN TAX SCREEN                       
         L     R2,RELO1                                                         
         AR    RF,R2                                                            
         MVI   CSSPROG,2                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   CSACT,ACTCHA        ARE WE DOING A CHANGE                        
         BNE   CURSIT              NO, LEAVE                                    
         CLC   FVMSGNO,=AL2(FVFOK) IF ERROR, RETURN                             
         BNE   CURSIT                                                           
         MVC   FVMSGNO,=X'FFFF'    TELL BT61 WE HAVE TO COME BACK               
         B     CURSIT                                                           
*                                                                               
PIC006   CLI   PFKEY,0             OTHER PFKEYS ARE INVALID                     
         BE    PIC008                                                           
         MVI   ERRNUM,251          INVALID PF KEY                               
         L     R2,TIACURS          ADDRESS OF CURSOR FIELD                      
         B     ERROR                                                            
*                                                                               
PIC008   CLI   MODE,0              MODE=0 - GO TO ORDER OVERLAY                 
         BE    PIC010                                                           
         L     R2,AORDH                                                         
         TM    4(R2),X'80'         CHECK IF FIELD CHANGED                       
         BO    *+14                YES                                          
         CLC   ORDNO,8(R2)         SAME ORDER - CARRY ON                        
         BE    PIC020                                                           
         MVI   MODE,0              OTHERWISE RE-READ                            
PIC010   LA    R3,7                GET ADDRESS OF BAT07 OVERLAY                 
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         LA    R2,WRKC1            PASS ADDRESS OF WRKCODE TABLE                
         GOTO1 (RF),DMCB,,(R9),(R2)                                             
*                                                                               
         L     R2,ADOCH                                                         
         CLI   MODE,1              EXIT WITH ORDER DISPLAYED                    
         BNE   *+16                                                             
         L     RE,AORDH                                                         
         NI    6(RE),X'FF'-X'40'   SWITCH OFF CURSOR                            
         B     EXIT                                                             
*                                                                               
         CLI   ERRNUM,X'FF'                                                     
         BNE   CURSIT                                                           
         SPACE 1                                                                
PIC020   L     R2,ADOCH            NO ORDER AND NO ERROR                        
         MVC   REFSAVE,SPACES                                                   
         MVC   DOCSAVE,SPACES                                                   
         GOTO1 SCANNER,DMCB,(R2),(2,WORK)                                       
         MVI   ERRNUM,2                                                         
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         CLI   WORK,6                                                           
         BH    ERROR                                                            
         MVC   DOCSAVE,WORK+12                                                  
         CLC   DOCSAVE,SPACES      INPUT STARTED WITH A COMMA                   
         BE    ERROR                                                            
         MVC   DOCLEN,WORK                                                      
         CLI   4(R1),1             ONE ENTRY ONLY                               
         BNE   ERROR                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        VALIDATE INPUT ACCOUNTS AND SAVE THEIR NAMES AND NUMBERS               
*--------------------------------------------------------------                 
*                                                                               
         USING ACCOMPD,R5                                                       
PIC050   L     R2,ACLIH            VALIDATE CLIENT                              
         LA    R5,COMPEL                                                        
         BAS   RE,ANY                                                           
         MVI   ERRNUM,14                                                        
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPJOB    U/L FOR CLI/PRO/JOBS                         
         TM    4(R2),X'20'         HAS CLIENT CHANGED ?                         
         BO    PIC055              NO                                           
*                                                                               
         GOTO1 =A(CLEARX),DMCB,(RC),RR=RELO1  CLEAR UNUSED X-JOB FIELDS         
         L     R2,APROH                                                         
         NI    4(R2),X'FF'-X'20'   TURN OFF PREV VALID IN PROD                  
         L     R2,AJOBH                                                         
         NI    4(R2),X'FF'-X'20'                                                
*                                                                               
         L     R2,ACLINH                                                        
         MVC   FLD,SPACES          CLEAR THE CLIENT NAME                        
         BAS   RE,MOVEFLD                                                       
*                                                                               
         L     R2,APRONH                                                        
         MVC   FLD,SPACES          CLEAR THE PRODUCT NAME                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         L     R2,AJOBNH                                                        
         MVC   FLD,SPACES          CLEAR THE JOB NAME                           
         BAS   RE,MOVEFLD                                                       
*                                                                               
PIC055   MVI   ERRNUM,37                                                        
         L     R2,ACLIH                                                         
         CLC   5(1,R2),CLILNGTH                                                 
         BH    ERROR                                                            
*                                                                               
         SR    R3,R3               GET LENGTH OF PRODUCT                        
         SR    RF,RF                                                            
         IC    R3,PRDLNGTH                                                      
         IC    RF,CLILNGTH                                                      
         SR    R3,RF                                                            
         STC   R3,LPRO                                                          
*                                                                               
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)                                                   
*                                                                               
         XC    CLIPROF,CLIPROF                                                  
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,28                                                        
         TM    ACCTSTAT,X'10'      IS CLIENT LOCKED ?                           
         BO    ERROR               YES, ERROR                                   
*                                                                               
         L     R2,ACLINH                                                        
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         BAS   RE,MOVEFLD                                                       
         L     R2,ACLIH                                                         
         OI    4(R2),X'20'         NOTE CLIENT AS PREV VALID                    
*                                                                               
PIC065   LA    R6,CLIPROF                                                       
         OC    TWAACCS(2),TWAACCS  TEST LIMIT ACCESS                            
         BZ    PIC070                                                           
         CLC   TWAACCS(2),SPACES                                                
         BE    PIC070                                                           
         MVI   ERRNUM,55                                                        
         CLI   TWAACCS,C'*'        OFFICE -CODE                                 
         BE    PIC070                                                           
         CLI   TWAACCS,C'$'        AND LIST WILL BE CHECK BY BASE               
         BE    PIC070                                                           
         CLC   TWAACCS(2),KEY+3    2 CHARACTER CLIENT MATCH                     
         BNE   ERROR                                                            
*                                                                               
PIC070   MVC   CLINUM,KEY                                                       
         L     RE,ACLINH                                                        
         MVC   CLINAME,8(RE)                                                    
         LA    R4,KEY+3                                                         
         SR    R3,R3                                                            
         IC    R3,CLILNGTH         LEVA LENGTH                                  
         AR    R4,R3               POINT TO PRODUCT POSITION                    
         MVI   ERRNUM,15                                                        
         L     R2,APROH                                                         
         BAS   RE,ANY                                                           
         TM    4(R2),X'20'         HAS PRODUCT CHANGED ?                        
         BO    PIC075              NO                                           
*                                                                               
         GOTO1 =A(CLEARX),DMCB,(RC),RR=RELO1  CLEAR UNUSED X-JOB FIELDS         
         L     R2,AJOBH                                                         
         NI    4(R2),X'FF'-X'20'                                                
*                                                                               
         L     R2,APRONH                                                        
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         L     R2,AJOBNH                                                        
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
PIC075   MVI   ERRNUM,37                                                        
         L     R2,APROH                                                         
         CLC   5(1,R2),LPRO                                                     
         BH    ERROR                                                            
*                                                                               
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),8(R2)                                                    
*                                                                               
         XC    PRODPROF,PRODPROF                                                
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,28                                                        
         TM    ACCTSTAT,X'10'      IS PRODUCT LOCKED ?                          
         BO    ERROR               YES, ERROR                                   
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         L     R2,APRONH                                                        
         BAS   RE,MOVEFLD                                                       
         L     R2,APROH                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
PIC080   L     RE,APRONH                                                        
         MVC   PRONAME,8(RE)                                                    
         OC    PRONAME,SPACES                                                   
         MVC   PRONUM,KEY                                                       
         MVI   ERRNUM,16                                                        
         L     R2,AJOBH                                                         
         BAS   RE,ANY                                                           
         TM    4(R2),X'20'         HAS JOB CHANGED ?                            
         BO    PIC082              NO                                           
         GOTO1 =A(CLEARX),DMCB,(RC),RR=RELO1  CLEAR UNUSED X-JOB FIELDS         
*                                                                               
PIC082   LA    R4,KEY+3                                                         
         SR    R3,R3                                                            
         IC    R3,PRDLNGTH         LEVB LENGTH                                  
         AR    R4,R3               READY FOR JOB                                
*                                                                               
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),8(R2)                                                    
         XC    JOBPROF,JOBPROF                                                  
         LA    R6,JOBPROF                                                       
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'      DOES JOB HAVE BALANCE ELEMENT ?              
         BZ    ERROR               NO, ERROR                                    
         MVI   ERRNUM,23                                                        
         TM    ACCTSTAT,X'20'      IS JOB CLOSED ?                              
         BO    ERROR               YES, ERROR                                   
         MVI   ERRNUM,28                                                        
         TM    ACCTSTAT,X'10'      IS IT LOCKED ?                               
         BO    ERROR               YES, ERROR                                   
*                                                                               
         L     R2,AJOBNH                                                        
         MVC   FLD,SPACES          DISPLAY THE JOB NAME                         
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         BAS   RE,MOVEFLD                                                       
         L     R2,AJOBH                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
PIC085   MVC   JOBNUM,KEY                                                       
         L     RE,AJOBNH                                                        
         MVC   JOBNAME,8(RE)       NAME FROM SCREEN                             
         OC    JOBNAME,SPACES                                                   
*                                                                               
         LA    R4,JOBNUM                                                        
         GOTO1 ASETJOB,DMCB,(R4)                                                
         MVC   JOBSTAT,ACOPSTAT                                                 
*                                                                               
         USING GOBLOCKD,R4                                                      
         L     R4,AGOBLOCK                                                      
         MVC   CLCDPASS,GOCLICD                                                 
*                                                                               
         BAS   RE,PROFMERG         MERGE PROFILES                               
         LA    R4,PROFILE                                                       
         USING ACPROFD,R4                                                       
         MVC   OFFICE,ACPROFFC     GET OFFICE CODE                              
         MVC   COFFICE,ACPROFFC    SAVE FOR SC POSTINGS                         
         LA    R4,JOBPROF                                                       
         CLC   ACPROFFC,SPACES                                                  
         BNH   *+16                                                             
         MVC   OFFICE,ACPROFFC                                                  
         MVC   COFFICE,ACPROFFC                                                 
*                                                                               
         L     R2,ACOFH            CREDIT OFFICE                                
         CLI   5(R2),0             YES, WAS ANYTHING ENTERED ?                  
         BE    PIC088              NO, LEAVE IT ALONE                           
         MVC   COFFICE,SPACES      YES, THAT'S THE ONE WE WANT                  
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   COFFICE(0),8(R2)                                                 
         GOTO1 AVALOFFC,DMCB,(X'80',COFFICE)                                    
         BNE   ERROR                                                            
*                                                                               
         USING GOXBLOCK,R4                                                      
PIC088   TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    PIC090              NO,JUST USE CLIENT/PRODUCT OFFICE            
         L     R4,AGOXBLK                                                       
         OC    GOAWOFOF,GOAWOFOF   DOES OPTION MAINT HAVE AN OFFICE ?           
         BZ    *+10                NO, LEAVE IT ALONE                           
         MVC   OFFICE,GOAWOFOF     YES, SAVE THAT ONE                           
         DROP  R4                                                               
*                                                                               
         L     R2,AFOFH                                                         
         CLI   5(R2),0             YES, WAS ANYTHING ENTERED ?                  
         BE    PIC090              NO, LEAVE IT ALONE                           
         MVC   OFFICE,SPACES       YES, THAT'S THE ONE WE WANT                  
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFICE(0),8(R2)                                                  
         GOTO1 AVALOFFC,DMCB,(X'80',OFFICE)                                     
         BNE   ERROR                                                            
*                                                                               
PIC090   OI    4(R2),X'20'         FINANCIAL OFFICE VALIDATED                   
         SR    R6,R6               NO PROFILES                                  
         XC    SAVNUM,SAVNUM                                                    
         L     R2,ATOTH                                                         
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         L     R2,ASUPH            VENDOR/ BANK A/C / PETTY CASH                
         BAS   RE,ANY                                                           
*                                                                               
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)                                                   
*                                                                               
         XC    DISC,DISC                                                        
         MVC   KEY+1(2),ACMPSUPP   UNIT/LEDGER FOR PROD VENDOR                  
         CLC   KEY+3(3),=C'*SW'    WAS U/L OVERRIDEN ?                          
         BE    PIC100              YES, SHIFT KEY                               
         CLC   KEY+3(3),=C'*SX'                                                 
         BE    PIC100                                                           
         CLC   KEY+3(3),=C'*SV'                                                 
         BE    PIC100                                                           
         CLC   KEY+3(3),=C'*SY'                                                 
         BNE   PIC105                                                           
*                                                                               
PIC100   MVC   KEY+1(20),SPACES                                                 
         MVC   KEY+1(14),9(R2)                                                  
         OC    KEY+1(14),SPACES                                                 
*                                                                               
PIC105   MVC   KEY+15(17),SPACES   FLUSH OUT POSSIBLE BAD ENTRY                 
         BAS   RE,HIGH                                                          
         MVI   ERRNUM,INVACC                                                    
         CLC   KEY(15),KEYSAVE     TRY TO FIND DISCOUNT ELEMENT                 
         BNE   ERROR                                                            
         CLI   8(R2),C'*'                                                       
         BE    *+14                                                             
         MVC   20(3,R2),SPACES                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R4,IOAREA                                                        
         SR    R3,R3               CLEAR R3=WORK REGISTER                       
*                                                                               
PIC110   CLI   0(R4),0                                                          
         BE    PIC135                                                           
         CLI   0(R4),ACADELQ                                                    
         BE    PIC120                                                           
         CLI   0(R4),X'38'                                                      
         BE    PIC125                                                           
         CLI   0(R4),ITCELQ        TEST FOR ITC DEFAULT                         
         BE    PIC130                                                           
*                                                                               
PIC115   IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     PIC110                                                           
*                                                                               
PIC120   L     RE,ASUPNH           BUMP PAST SUPPLIER NAME                      
         LA    RE,8(RE)                                                         
         MVI   25(RE),C' '                                                      
         MVC   26(20,RE),3(R4)     DISPLAY 1ST 10 OF ADDRESS                    
         B     PIC115                                                           
*                                                                               
PIC125   L     R2,ACDH                                                          
         CLI   8(R2),C'N'          IGNORE CASH DISCOUNT?                        
         BE    PIC115              YES-BRANCH                                   
         MVC   HALF,2(R4)          CALCULATE C.D.                               
         LH    R1,HALF                                                          
         CVD   R1,DUB1                                                          
         ZAP   DISC,DUB1                                                        
         B     PIC115                                                           
*                                                                               
         USING ITCELD,R4                                                        
PIC130   CLC   SAVEDATE,ITCEFFD    TEST TRANS DATE >= EFFECTIVE DATE            
         BL    PIC115              NO                                           
         OC    ITCPROV,ITCPROV     NO PROV=GST                                  
         BZ    PIC130G                                                          
         MVC   VENDPSTT,ITCTYPE    SAVE PST TYPE                                
         MVC   VENDPROV,ITCPROV                                                 
         B     PIC115                                                           
*                                                                               
PIC130G  MVC   VENDTYPE,ITCTYPE     SAVE GST TYPE                               
         B     PIC115                                                           
         DROP  R4                                                               
*                                                                               
PIC135   BAS   RE,GETACC                                                        
         MVC   SVSTAT,ACCTSTAT                                                  
         MVC   SVSTAT2,ACSTAT2                                                  
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'10'      ACCOUNT IS LOCKED                            
         BO    ERROR                                                            
         MVC   VENNUM,ACCTNUM                                                   
         MVC   VENNAME,ACCTNAME                                                 
         CLC   VENNUM+1(2),=C'SA'  DEAL WITH OPEN ITEM ADVANCES                 
         BNE   PIC140                                                           
         L     R2,ADOCH                                                         
         MVI   ERRNUM,0                                                         
         BAS   RE,CHECKSA                                                       
         CLI   ERRNUM,0                                                         
         BNE   ERROR                                                            
         SPACE 1                                                                
PIC140   L     R2,ASUPH                                                         
         TM    4(R2),X'20'                                                      
         BO    PIC150                                                           
         OI    4(R2),X'20'                                                      
*                                                                               
         L     R2,ASUPNH                                                        
         MVC   8(24,R2),ACCTNAME   PRINT NAME, LEAVE ROOM FOR ADDRESS           
         OI    6(R2),X'80'                                                      
         OC    DISC,DISC                                                        
         BZ    PIC150                                                           
         EDIT  DISC,(5,WORK),2                                                  
         MVI   WORK+5,C' '                                                      
         MVC   WORK+6(25),ACCTNAME                                              
         GOTO1 SQUASHER,DMCB,WORK,42                                            
         L     R4,DMCB+4                                                        
         LA    R3,8(R2)                                                         
         GOTO1 CHOPPER,DMCB,((R4),WORK),(24,(R3)),1                             
*                                                                               
PIC150   CLI   USERPROV,C'Y'       DID USER ENTERED PROVINCE?                   
         BE    PIC155              USER PROVINCE                                
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   PIC155                                                           
         L     R2,APROVH                                                        
         MVI   5(R2),2                                                          
         OI    6(R2),X'80'         MODIFY FOR NEXT INPUT                        
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6STRO        STEREO?                          
         BZ    PIC153                                                           
         CLC   VENDPROV,SPACES                                                  
         BNH   PIC155                                                           
PIC153   MVC   8(2,R2),VENDPROV                                                 
*                                                                               
PIC155   SR    R6,R6               NO MORE PROFILES REQUIRED                    
         MVI   ERRNUM,1                                                         
         L     R2,AWRKH            ONE W/C FIELD MUST BE INPUT                  
         L     RE,ANCWKH                                                        
         CLI   5(RE),0                                                          
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         ZAP   CSHTOT,=P'0'                                                     
         L     R3,AWRKH            VALIDATE COMMISSIONABLE FIELDS               
         LA    R4,WRKC1                                                         
         GOTO1 =A(VALWAM),DMCB,(RC),(R3),(R4),RR=RELO1                          
         BNE   ERROR                                                            
         L     R3,ANCWKH           VALIDATE NON-COMMISSIONABLE FIELDS           
         LA    R4,WRKNC1                                                        
         GOTO1 =A(VALWAM),DMCB,(RC),(R3),(R4),RR=RELO1                          
         BNE   ERROR                                                            
*                                                                               
         L     R2,ACOFNH           CLEAR CREDIT OFFICE NAME                     
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         L     R2,ACOFH            6REDIT OFFICE                                
         CLI   5(R2),0             WAS CREDIT OFFICE ENTERED ?                  
         BNE   PIC160              YES, USE IT                                  
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'COFFICE),COFFICE                                           
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
PIC160   MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY      READ OFFICE FOR NAME                         
         MVC   KEY+1(2),=C'2D'                                                  
         MVC   KEY+3(2),COFFICE                                                 
         BAS   RE,GETACC                                                        
*                                                                               
         L     R2,ACOFNH                                                        
         MVC   FLD,SPACES                                                       
         MVC   FLD,ACCTNAME                                                     
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
PIC193   L     R2,ACOFH                                                         
         OI    4(R2),X'20'         CREDIT OFFICE VALIDATED                      
*                                                                               
         CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   PIC195              NO                                           
         CLI   PFKEY,7                                                          
         BNE   PIC193A                                                          
         CLI   MODE,1                                                           
         BH    PIC193A                                                          
         B     PIC005C                                                          
*                                                                               
PIC193A  GOTO1 =A(EDTAX),DMCB,(RC),RR=RELO1                                     
         BE    PIC193B                                                          
         CLI   ERRNUM,OK           ERROR OCCURED                                
         BNE   ERROR                                                            
*                                                                               
PIC193B  L     RF,=A(CTAXMOD)                                                   
         L     RE,RELO1                                                         
         AR    RF,RE                                                            
         MVC   MSG,SPACES                                                       
         MVC   TMPMODE,MODE                                                     
         MVI   MODE,4                                                           
         GOTO1 (RF),DMCB,(RC)                                                   
         MVC   MODE,TMPMODE                                                     
         CLI   CTXMODE,C'Z'                                                     
         BNE   PIC193X                                                          
         MVI   FVOMTYP,C'E'                                                     
         MVC   FVMSGNO,CTXMSGNO                                                 
         L     R2,FVADDR                                                        
         B     ERROR                                                            
*                                                                               
PIC193X  MVI   FVOMTYP,C'E'                                                     
         XC    FVMSGNO,FVMSGNO                                                  
         ZAP   TOTNET,TMPNET                                                    
         ZAP   TOTGRS,TMPGRS                                                    
         ZAP   TOTGST,TMPGST                                                    
         ZAP   TOTPST,TMPPST                                                    
         ZAP   CSHTOT,TMPGRS                                                    
         MVC   WRKC1(64),AMTBLK      NEW AMOUNTS                                
*                                                                               
PIC195   TM    COMPSTA2,X'08'      CHECK FOR DUPLICATE PAYMENT?                 
         BZ    PIC220                                                           
         TM    SVSTAT2,X'08'       SKIP DUPLICATE CHECK FOR THIS ACC?           
         BO    PIC220                                                           
         CLC   VENNUM+1(2),=C'SV'                                               
         BE    *+14                                                             
         CLC   VENNUM+1(2),=C'SW'                                               
         BNE   PIC220                                                           
         EJECT                                                                  
*--------------------------------------------------------------                 
*        CHECK FOR DUPLICATE PAYMENTS                                           
*--------------------------------------------------------------                 
*                                                                               
         ZAP   DUB1,=P'0'                                                       
         MVC   KEY,SPACES                                                       
         USING KEYD,R3                                                          
         LA    R3,KEY                                                           
         USING KEYSAVED,R4                                                      
         LA    R4,KEYSAVE                                                       
         MVC   KVENNUM,VENNUM                                                   
         L     R2,ADOCH                                                         
         MVI   ERRNUM,59                                                        
         BAS   RE,HIGH                                                          
PIC200   CLC   KVENNUM,KSVENNUM                                                 
         BNE   PIC215                                                           
         USING TRNRECD,R6                                                       
         LA    R6,KEY                                                           
         TM    TRNRSTAT,TRNSDRFT   IS THIS A DRAFT TRANSACTIONS ?               
         BO    PIC210              YES, IGNORE IT                               
         DROP  R6                                                               
*                                                                               
         USING TRANSD,R6                                                        
         LA    R6,IOAREA                                                        
         CLI   TRNSEL,TRNSELQ                                                   
         BNE   PIC210                                                           
         TM    TRNSSTAT,X'80'      IGNORE DEBITS                                
         BO    PIC210                                                           
         TM    TRNSSTAT,X'20'      AND OFFSETS                                  
         BO    PIC210                                                           
         CLC   TRNSREF,DOCSAVE     LOOK FOR SAME REF/AMOUNT                     
         BNE   PIC210                                                           
         AP    DUB1,TRNSAMNT                                                    
         MVI   ELCODE,TRCSELQ                                                   
PIC202   BAS   RE,NEXTEL                                                        
         BNE   PIC210                                                           
         USING TRCASHD,R6                                                       
         CLI   TRCSTYPE,C'D'                                                    
         BNE   PIC202                                                           
         AP    DUB1,TRCSAMNT                                                    
PIC210   BAS   RE,SEQ                                                           
         B     PIC200                                                           
PIC215   CP    DUB1,=P'0'                                                       
         BE    PIC220              NO MATCH ON INVOICE                          
         CP    DUB1,CSHTOT                                                      
         BE    ERROR                                                            
         B     PIC220                                                           
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*--------------------------------------------------------------                 
*        VALIDATE INPUT ACCOUNTS AND SAVE THEIR                                 
*        NAMES AND NUMBERS (CONTINUED)'                                         
*--------------------------------------------------------------                 
*                                                                               
PIC220   MVI   CDSW,C'N'                                                        
         OC    DISC,DISC           IF WE HAVE CASH DISCOUNT,                    
         BZ    PIC268                                                           
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BO    PIC268              YES                                          
         CLI   COCDPASS,C'N'       AND WE ARE KEEPING HOLD OF IT                
         BE    *+12                                                             
         CLI   CLCDPASS,C'N'       WE HAVE TO LOOK FOR SOMEWHERE                
         BNE   PIC268                                                           
*                                                                               
         DS    0H                  CHECK IF USING MEDIA RECS                    
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'09'           MEDIA RECORD                                 
         MVC   KEY+1(1),COMPANY                                                 
         L     RE,AJOBH                                                         
         MVC   KEY+2(1),8(RE)      MEDIA IS FIRST BYTE OF JOB                   
         BAS   RE,HIGH                                                          
         MVC   FVMSGNO,=Y(AE$MMR)                                               
         L     R2,AAMTH                                                         
         CLC   KEYSAVE,KEY                                                      
         BNE   ERROR               MEDIA RECORD MUST BE PRESENT                 
*                                                                               
         MVI   ELCODE,ACMDELQ                                                   
         LA    R6,KEY                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MEDIA RECORD MUST HAVE AN 11 ELEMNT          
*                                                                               
         USING ACMEDIAD,R6                                                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         CLI   ACMDCSHD+1,C' '     DEFAULT TO SIMD IF ACCT IS SPACES            
         BNH   PIC264                                                           
         MVC   KEY+1(14),ACMDCSHD+1 LOAD IN CASH DISCOUNT ACCOUNT               
         B     PIC267                                                           
*                                                                               
PIC264   MVC   KEY+1(2),PRODUL     CHECK PRODUCTION LEDGER FOR DEFAULT          
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE,KEY                                                      
         BE    PIC265                                                           
*                                                                               
         MVC   FVMSGNO,=Y(AE$MPLR)                                              
         L     R2,AAMTH                                                         
         B     ERROR                                                            
*                                                                               
PIC265   MVI   ELCODE,ACLTELQ      GET THE LEDGER ELEMENT                       
         LA    R6,KEY                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACLEDGD,R6                                                       
         CLI   ACLTLEN,ACLTLNQ     TEST ELEMENT LONG ENOUGH                     
         BL    PIC266              NO                                           
         CLC   ACLTCDAC,SPACES     TEST FOR A CD A/C                            
         BNH   PIC266              NO                                           
         MVC   KEY+1(14),ACLTCDAC                                               
         B     PIC267                                                           
*                                                                               
PIC266   MVC   KEY+1(4),=C'SIMD'   SET DEFAULT CD ACCOUNT                       
*                                                                               
PIC267   L     R2,AAMTH            CRAZY CURSOR POSITION                        
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CDNUM,ACCTNUM                                                    
         MVC   CDNAME,ACCTNAME                                                  
         MVI   CDSW,C'Y'                                                        
         EJECT                                                                  
PIC268   LA    R5,COMPEL                                                        
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         USING ACCOMPD,R5                                                       
         L     R2,AEXPH                                                         
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BO    PIC271              YES                                          
         MVI   ERRNUM,44           NO, ANALYSIS AND EXPENSE ARE INVALID         
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         L     R2,AFOFH                                                         
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         L     R2,AAOFH                                                         
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         L     R2,ADEPH                                                         
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         L     R2,ASTFH                                                         
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         B     PIC278                                                           
*                                                                               
         USING GOXBLOCK,R4                                                      
PIC271   L     R4,AGOXBLK          GET EXTENDED GOBLOCK                         
         L     R2,AEXPH                                                         
         CLI   5(R2),0             WAS EXPENSE ENTERED ?                        
         BNE   PIC273              YES, USE IT                                  
         MVC   FLD,SPACES          CLEAR THE                                    
         OC    GOAWOA,GOAWOA       NO, IS THERE ONE IN OPTION MAINT ?           
         BZ    PIC272              NO, ERROR                                    
         CLC   GOAWOA(2),=C'SE'    IS THIS AN 'SE' ACCOUNT ?                    
         BE    *+18                YES                                          
         MVI   FLD,C'*'            NO, INDICATOR OVERRIDE                       
         MVC   FLD+1(L'GOAWOA),GOAWOA                                           
         B     *+10                                                             
         MVC   FLD(L'GOAWOA-2),GOAWOA+2                                         
*                                                                               
PIC272   BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
         BAS   RE,ANY              X-JOBS MUST HAVE EXPENSE                     
*                                                                               
PIC273   OI    4(R2),X'20'         EXPENSE VALIDATED                            
         L     R2,AFOFH                                                         
         CLI   5(R2),0             WAS A FINANCIAL OFFICE ENTERED ?             
         BNE   PIC274              YES, USE IT                                  
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWOFOF),GOAWOFOF                                         
         OC    GOAWOFOF,GOAWOFOF                                                
         BNZ   *+10                                                             
         MVC   FLD(L'OFFICE),OFFICE                                             
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
PIC274   MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY      READ OFFICE FOR NAME                         
         MVC   KEY+1(2),=C'2D'                                                  
         MVC   KEY+3(2),OFFICE                                                  
         BAS   RE,GETACC                                                        
*                                                                               
         L     R2,AFOFNH                                                        
         MVC   FLD,SPACES                                                       
         MVC   FLD,ACCTNAME                                                     
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         OI    4(R2),X'20'         FINANCIAL OFFICE VALIDATED                   
         L     R2,AAOFH                                                         
*                                                                               
         SR    R0,R0                                                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2D'                                                  
         BAS   RE,GETACC                                                        
*                                                                               
         USING ACLELD,R6                                                        
         L     R6,AIO1                                                          
         MVI   ELCODE,ACLELQ       ACCOUNT LENGTHS                              
         BAS   RE,GETEL                                                         
         BNE   PIC274X                                                          
         IC    R0,ACLVLEN          SAVE IT IN EXCEL BLOCK                       
         CLI   ACLVLEN,12          ** FOR (SUCH) ONLY, ALL DEPT                 
         BE    PIC275                                                           
PIC274X  CLI   5(R2),0             WAS ANALYSIS OFFICE ENTERED ?                
         BNE   PIC275              YES, USE IT                                  
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWOAOF),GOAWOAOF                                         
         OC    GOAWOAOF,GOAWOAOF                                                
         BNZ   *+10                                                             
         MVC   FLD(L'OFFICE),OFFICE                                             
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
PIC275   OI    4(R2),X'20'         ANALYSIS OFFICE VALIDATED                    
         L     R2,ADEPH                                                         
         CLI   5(R2),0             WAS DEPARTMENT ENTERED ?                     
         BNE   PIC276              YES, USE IT                                  
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWODEP),GOAWODEP                                         
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
PIC276   OI    4(R2),X'20'         DEPARTMENT VALIDATED                         
         L     R2,ASTFH                                                         
         CLI   5(R2),0             WAS STAFF ENTERED ?                          
         BNE   PIC277              YES, USE IT                                  
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWOSTF),GOAWOSTF                                         
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
         DROP  R4                                                               
*                                                                               
         USING EXCELD,R4                                                        
PIC277   OI    4(R2),X'20'         STAFF VALIDATED                              
         LA    R4,EXCWORK          ADDRESS ACEXCEL WORKAREA                     
         MVI   EXCACT,EXCAVAL      VALIDATE MODE                                
         MVC   EXCAEXP,AEXPH       PASS A(EXPENSE ACCOUNT HEADER)               
         MVC   EXCAANO,AAOFH       PASS A(ANALYSIS OFFICE HEADER)               
         MVC   EXCADEP,ADEPH       PASS A(DEPARTMENT HEADER)                    
         MVC   EXCASTF,ASTFH       PASS A(STAFF HEADER)                         
         MVC   EXCSECAC,VENNUM     PASS VENDOR NUMBER                           
         MVC   EXCSECAN,VENNAME    PASS VENDOR NAME                             
         ST    R9,EXCAGWS                                                       
         STC   R0,EXC2DLEN         DEPT LENGTH                                  
         GOTO1 VEXCEL,EXCELD                                                    
         BNE   CURSIT              EXIT IMMEDIATELY                             
*                                                                               
         MVC   FLD,SPACES          MOVE NAMES TO SCREEN                         
         MVC   FLD(L'EXCSENM),EXCSENM                                           
         L     R2,AEXPNH                                                        
         BAS   RE,MOVEFLD                                                       
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'EXC2PNM),EXC2PNM                                           
         CLC   FLD,SPACES                                                       
         BH    *+10                                                             
         MVC   FLD(L'EXC2DNM),EXC2DNM                                           
         L     R2,ASTFNH                                                        
         BAS   RE,MOVEFLD                                                       
         DROP  R4                                                               
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 64 ELEMENT                                                       
*--------------------------------------------------------------                 
*                                                                               
PIC278   LA    R8,IOAREA+2                                                      
         SR    R3,R3                                                            
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,X'64'                                                     
         MVC   DLDSREF,DOCSAVE                                                  
         MVC   DLDSDATE,SAVEDATE                                                
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         L     R2,AURGH            URGENT                                       
         CLI   8(R2),C'U'          URGENT?                                      
         BE    PIC280                                                           
         CLI   5(R2),0                                                          
         BE    PIC290                                                           
         MVI   ERRNUM,2                                                         
         B     ERROR                                                            
*                                                                               
PIC280   OI    DLDSSTAT,X'40'                                                   
PIC290   XC    DLDSSTAT+1(6),DLDSSTAT+1                                         
         L     R2,ANARH                                                         
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         BAS   RE,NARRSCAN                                                      
         SR    R3,R3                                                            
         LA    R5,DLDSNARR                                                      
         SR    R5,R8               R5 = ELEMENT - NARRATIVE                     
         AR    R5,R6               R6 = L'NARRATIVE                             
         STH   R5,HALF                                                          
         MVC   DLDSLEN,HALF+1                                                   
         OC    CLINAME,SPACES                                                   
         OC    JOBNAME,SPACES                                                   
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 69 AND 6A MAIN ACCOUNTING ELEMENTS                               
*--------------------------------------------------------------                 
*                                                                               
         AR    R8,R5               BUILD 69&6A MAIN ACCOUNTING ELEMENTS         
         CLC   REFSAVE,SPACES                                                   
         BE    PIC300                                                           
         USING ACOTHERD,R8                                                      
         MVC   ACOTEL(2),=X'230F'                                               
         MVC   ACOTNUM(13),SPACES                                               
         MVC   ACOTNUM(L'REFSAVE),REFSAVE                                       
         ZIC   R3,ACOTLEN                                                       
         AR    R8,R3                                                            
PIC300   DS    0H                                                               
         BAS   RE,JOBDEB           BUILD JOB DEBIT POSTINGS                     
*                                                                               
         CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   PIC370                                                           
         CLI   GSTSW,C'Y'          TEST GST APPLICABLE                          
         BNE   PIC370              NO                                           
*                                                                               
         L     R2,AIOA                                                          
         ZICM  RF,0(R2),2                                                       
         LTR   RF,RF               NOTHING                                      
         BZ    PIC370                                                           
         SH    RF,=H'2'            SUBTRACT LENGTH                              
         LA    RE,2(R2)            COPY POSTINGS                                
         LR    R0,R8                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         LR    R8,R0                                                            
*                                                                               
         USING ACOTHERD,R8                                                      
PIC370   MVC   ACOTEL(2),=X'230F'  BUILD 'OTHERS' ELEMENT FOR                   
         MVC   ACOTNUM(13),SPACES  PRODUCT AND JOB                              
         L     R2,APROH                                                         
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ACOTNUM(0),8(R2)                                                 
         L     R2,AJOBH                                                         
         IC    R3,5(R2)                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ACOTNUM+6(0),8(R2)                                               
         IC    R3,ACOTLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    PIC372              NO                                           
*                                                                               
         USING EXCELD,R4                                                        
         USING TRSDESCD,R8                                                      
         LA    R4,EXCWORK                                                       
         MVI   TRSDEL,TRSDELQ                                                   
         MVC   TRSDLEN,=YL1(L'ACKEYACC-1+2)                                     
         MVC   TRSDACCS(L'ACKEYACC-1),EXCSEAC+1                                 
         IC    R3,TRSDLEN                                                       
         AR    R8,R3                                                            
         DROP  R4,R8                                                            
*                                                                               
PIC372   CP    CDAMNT,=P'0'                                                     
         BE    PIC380              CD FOR VENDOR                                
*                                                                               
         USING TRCASHD,R8                                                       
         ZAP   TRCSAMNT,CDAMNT     CD ELEMENT FOR VENDOR                        
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         SR    R5,R5                                                            
         IC    R5,TRCSLEN                                                       
         AR    R8,R5                                                            
*                                                                               
PIC380   CLI   ORDNOEL,0           TEST TO ADD ORDER ELEMENT                    
         BE    *+14                                                             
         MVC   0(L'ORDNOEL,R8),ORDNOEL                                          
         LA    R8,L'ORDNOEL(R8)                                                 
*                                                                               
         TM    ACOPSTAT,ACOXJOB    TEST FOR X-JOB                               
         BZ    *+14                                                             
         MVC   0(L'TRSELEM,R8),TRSELEM  YES-ADD TRANSACTION STAT ELEM           
         LA    R8,L'TRSELEM(R8)                                                 
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,X'6A'        CREDIT VENDOR                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSDBAC,CLINUM     CLIENT AS CONTRA ACCOUNT                     
         MVC   DLPSDBNM,CLINAME                                                 
         MVC   DLPSCRAC,VENNUM                                                  
         MVC   DLPSCRNM,VENNAME                                                 
         CLC   VENNUM+1(2),=C'SA'  OR SOMETHING ELSE FOR ADVANCES               
         BNE   PIC390                                                           
         OC    SAVNUM,SAVNUM                                                    
         BZ    PIC390                                                           
         MVC   DLPSDBAC,SAVNUM                                                  
         MVC   DLPSDBNM,SAVNAME                                                 
*                                                                               
PIC390   MVC   DLPSANAL,SPACES                                                  
         MVC   DLPSANAL,COFFICE                                                 
         ZAP   DLPSAMNT,CSHTOT                                                  
         SP    DLPSAMNT,CDAMNT                                                  
         ZIC   R5,DLPSLEN                                                       
         AR    R8,R5                                                            
*                                                                               
         CLI   CDSW,C'Y'                                                        
         BNE   PIC400                                                           
*                                                                               
         USING DLPOSTD,R8                                                       
         TM    ACOPSTAT,ACOXJOB                                                 
         BZ    *+14                                                             
         MVC   DLPOSTD(L'TRSELEM),TRSELEM                                       
         LA    R8,L'TRSELEM(R8)                                                 
*                                                                               
         MVI   DLPSEL,X'6A'        CREDIT TO CD-INCOME                          
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSCRAC,CDNUM                                                   
         MVC   DLPSCRNM,CDNAME                                                  
         MVC   DLPSDBAC,PRONUM     CLI/PROD AS CONTRA                           
         MVC   DLPSDBNM,PRONAME                                                 
         ZAP   DLPSAMNT,CDAMNT                                                  
         MVC   DLPSANAL,SPACES                                                  
         MVC   DLPSANAL,OFFICE                                                  
         ZIC   R5,DLPSLEN                                                       
         AR    R8,R5                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD SUBSIDIARY ELEMENTS IF NECESSARY                                 
*--------------------------------------------------------------                 
*                                                                               
PIC400   TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    PIC435              NO                                           
*                                                                               
         USING EXCELD,R4                                                        
         USING GOXBLOCK,R3                                                      
         LA    R4,EXCWORK                                                       
         L     R3,AGOXBLK                                                       
         MVI   EXCACT,EXCAPST      POSTING MODE                                 
         MVC   EXCAEXP,AEXPH       A(EXPENSE ACCOUNT HEADER)                    
         MVC   EXCAANO,AAOFH       A(ANALYSIS OFFICE HEADER)                    
         MVC   EXCADEP,ADEPH       A(DEPARTMENT HEADER)                         
         MVC   EXCASTF,ASTFH       A(STAFF HEADER)                              
         MVC   EXCSECAC,VENNUM     VENDOR NUMBER                                
         MVC   EXCSECAN,VENNAME    VENDOR NAME                                  
         MVC   EXCCLNT,CLINUM+3    CLIENT                                       
         MVC   EXCPROD,PRONUM+6    PRODUCT                                      
         ST    R9,EXCAGWS          A(GLOBAL WORKING STORAGE)                    
         MVC   EXCFINO,OFFICE      FINANCIAL OFFICE                             
         CLI   GSTSW,C'Y'          TEST GST APPLICABLE                          
         BE    PIC410                                                           
         ZAP   EXCAMNT,CSHTOT      USE CSHTOT IF NOT GST                        
         B     *+10                                                             
PIC410   ZAP   EXCAMNT,TOTNET      AMOUNT                                       
         CLI   CDSW,C'Y'           ARE WE KEEPING CD ?                          
         BE    *+16                YES                                          
         SP    EXCAMNT,CDAMNT      NO, SUBTRACT CD FROM AMOUNT                  
         ZAP   EXCDAMNT,CDAMNT     AND PASS IT FOR ELEMENT                      
         ST    R8,EXCADAY                                                       
         GOTO1 VEXCEL,EXCELD                                                    
         BNE   CURSIT                                                           
         L     R8,EXCADAY          GET UPDATED ACCDAY POINTER                   
         DROP  R3,R4                                                            
*                                                                               
PIC435   MVI   0(R8),0                                                          
         LA    R8,1(R8)                                                         
         LA    R3,IOAREA                                                        
         SR    R8,R3                                                            
         STH   R8,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         BAS   RE,PUTDAY                                                        
*                                                                               
         MVI   GSTSW,C'N'          RESET CANADA SCREEN                          
         MVI   CTAXBEF,C'N'        RESET CANADA SCREEN                          
         XC    KEEPPROV,KEEPPROV                                                
         MVI   CLEAROK,C'Y'        CLEAR GST/PST NEXT TIME                      
         MVI   USERPROV,C'N'                                                    
         MVI   USERPST,C'N'                                                     
         CLI   AGYCTRY,CTRYCAN     CANADIAN?                                    
         BNE   *+12                                                             
         L     R2,APROVH                                                        
         MVI   4(R2),0                                                          
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ADD ENTRY TO TWA1                                                      
*--------------------------------------------------------------                 
*                                                                               
         XC    WORK,WORK                                                        
         L     R2,ADOCH                                                         
         SR    R3,R3                                                            
         IC    R3,DOCLEN                                                        
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)       REF                                          
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         ZAP   TRANSAMT,CSHTOT                                                  
         ZAP   LCSHTOT,COMAMNT1    COMMISSIONABLE AMOUNT 1                      
         AP    LCSHTOT,COMAMNT2                          2                      
         AP    LCSHTOT,COMAMNT3                          3                      
         AP    LCSHTOT,COMAMNT4                          4                      
         BAS   RE,ADSCRINF                                                      
         L     R2,ATOT                                                          
         MVC   0(10,R2),=C'ITEM TOTAL'                                          
         EDIT  (P6,CSHTOT),(13,11(R2)),2,MINUS=YES                              
         GOTO1 SQUASHER,DMCB,ATOT,25                                            
         L     R2,ATOTH                                                         
         OI    6(R2),X'80'                                                      
         CLI   MODE,1                                                           
         BNE   PIC440                                                           
         ZAP   TOTCASH,CSHTOT                                                   
         LA    R3,7                                                             
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         LA    R2,WRKC1                                                         
         GOTO1 (RF),DMCB,,(R9),(R2)  PASS ADDRESS OF GLOBAL/LOCAL AREAS         
         CLI   ERRNUM,X'FE'                                                     
         BL    EXIT                                                             
         BE    *+8                                                              
PIC440   L     R2,AORDH                                                         
         MVI   ERRNUM,X'FF'                                                     
         OI    MODE,X'80'          HAVE VALID AMOUNT                            
         B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
*---------------------------------------------------------------                
* ADSCRINF - POST & SAVE 2ND SCREEN'S INFO                                      
*---------------------------------------------------------------                
ADSCRINF NTR1  ,                                                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    RF,XTRAELM                                                       
         GOTO1 AADACDAY,BOPARM,(X'80',IOAREA),BOPL61,BOWORK1,(RF)               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERRXIT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*--------------------------------------------------------------                 
*        SUB-ROUTINE TO BUILD THE JOB POSTING ELEMENTS                          
*        AT ENTRY, R8=POSTING RECORD POINTER                                    
*--------------------------------------------------------------                 
*                                                                               
         USING EXCELD,R4                                                        
JOBDEB   NTR1  ,                                                                
         LA    R4,EXCWORK                                                       
         ZAP   CDAMNT,=P'0'        CLEAR CASH DISCOUNT BUCKET                   
         XC    POSTEL,POSTEL       CLEAR MODEL POSTING ELEMENT                  
         LA    RE,POSTEL                                                        
         USING DLPOSTD,RE                                                       
         MVI   DLPSEL,DLPSEDRQ     MODEL JOB DEBIT                              
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSDBAC,JOBNUM                                                  
         MVC   DLPSDBNM,JOBNAME                                                 
         MVC   DLPSCRAC,VENNUM                                                  
         MVC   DLPSCRNM,VENNAME                                                 
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    JOBDEB2             NO                                           
         MVC   DLPSCRAC,EXCSEAC    YES, USE EXPENSE ACCOUNT INSTEAD             
         MVC   DLPSCRNM,EXCSENM                                                 
         DROP  R4,RE                                                            
*                                                                               
JOBDEB2  XC    ORDNOEL,ORDNOEL     CLEAR ORDER NUMBER ELEMENT AREA              
         XC    MEMO4C,MEMO4C                                                    
         XC    MEMO50,MEMO50                                                    
         XC    TRSELEM,TRSELEM                                                  
         XC    ANOELEM,ANOELEM                                                  
         XC    PAKELEM,PAKELEM                                                  
         L     RE,AORDH                                                         
         CLI   5(RE),0             IS THERE AN ORDER NUMBER?                    
         BE    JOBDEB3             NO                                           
*                                                                               
         LA    R1,ORDNOEL                                                       
         USING ACNOD,R1            ORDER NUMBER ELEMENT                         
         MVC   ACNOEL(2),=X'250A'                                               
         MVC   ACNO(6),ORDNO                                                    
         LA    RE,8(RE)            POINT TO FIELD                               
         CLI   7(RE),C'P'          PARTIAL ORDER?                               
         BNE   *+8                                                              
         MVI   ACNOSTAT,C'P'       YES, SET PARTIAL FLAG                        
         DROP  R1                                                               
*                                                                               
JOBDEB3  TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    JOBDEB4             NO                                           
         USING TRCASHD,R1                                                       
         LA    R1,MEMO50           YES, BUILD MEMO WITH AMOUNT                  
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'S'                                                    
         ZAP   TRCSAMNT,=P'0'                                                   
         DROP  R1                                                               
*                                                                               
         USING TRSDESCD,R1                                                      
         LA    R1,MEMO4C           BUILD MEMO WITH ACCOUNT                      
         MVI   TRSDEL,TRSDELQ                                                   
         MVC   TRSDLEN,=YL1(L'ACKEYACC-1+2)                                     
         MVC   TRSDACCS(L'ACKEYACC-1),VENNUM+1                                  
         SPACE 1                                                                
         LA    R1,TRSELEM          BUILD SKELETAL TRANSACTION STATUS            
         USING TRSELD,R1                                                        
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         OI    TRSSTAT3,TRSSXJOB                                                
         DROP  R1                                                               
         SPACE 1                                                                
JOBDEB4  LA    R1,ANOELEM          BUILD OFFICE ELEMENT                         
         USING ANOELD,R1                                                        
         MVI   ANOEL,ANOELQ                                                     
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTCLI                                                  
         MVC   ANOOFFC,COFFICE                                                  
         DROP  R1                                                               
*                                                                               
         CLC   VENNUM+1(2),=C'SV'  CHECK IF CREDIT ACCOUNT NEEDED               
         BE    JOBDEB6                                                          
         CLC   VENNUM+1(2),=C'SW'                                               
         BE    JOBDEB6                                                          
         CLC   VENNUM+1(2),=C'SX'                                               
         BE    JOBDEB6                                                          
         CLC   VENNUM+1(2),=C'SY'                                               
         BNE   JOBDEB7                                                          
*                                                                               
JOBDEB6  LA    R1,PAKELEM          BUILD PAYABLE ACCOUNT ELEMENT                
         USING PAKELD,R1                                                        
         MVI   PAKEL,PAKELQ                                                     
         MVI   PAKLN,PAKLNQ                                                     
         MVC   PAKACC,VENNUM                                                    
         MVC   PAKOFF,COFFICE                                                   
         MVC   PAKCON,CLINUM                                                    
         MVC   PAKDATE,SAVEDATE                                                 
         MVC   PAKREF,DOCSAVE                                                   
         DROP  R1                                                               
         SPACE 1                                                                
* LOOP THROUGH WORKCODE TABLE BUILDING ONE POSTING PER WC AMT                   
*                                                                               
JOBDEB7  LA    R2,4                R2=LOOP COUNTER                              
         LA    R3,WRKC1            COMMISSIONABLE WORK CODES                    
         LA    R4,WRKNC1           NON-COMMISSIONABLE WORK CODES                
*                                                                               
JOBDEB8  CLC   0(2,R3),SPACES      Q, COMMISSIONABLE WORK CODE                  
         BE    JOBDEB16            N, IGNORE POSTING                            
*                                                                               
         CLI   ANOELEM,X'00'       DO WE HAVE AN OFFICE ELEMENT?                
         BE    *+14                NO                                           
         MVC   0(L'ANOELEM,R8),ANOELEM                                          
         LA    R8,L'ANOELEM(R8)                                                 
*                                                                               
         CLI   PAKELEM,X'00'       DO WE HAVE A PAYABLE ELEMENT?                
         BE    *+14                NO                                           
         MVC   0(L'PAKELEM,R8),PAKELEM                                          
         LA    R8,L'PAKELEM(R8)                                                 
*                                                                               
         CLI   ORDNOEL,0           TEST TO ADD ORDER ELEMENT                    
         BE    *+14                                                             
         MVC   0(L'ORDNOEL,R8),ORDNOEL                                          
         LA    R8,L'ORDNOEL(R8)                                                 
*                                                                               
         CLI   MEMO4C,X'00'        DO WE HAVE A 4C ELEMENT ?                    
         BE    *+14                NO                                           
         MVC   0(L'MEMO4C,R8),MEMO4C                                            
         LA    R8,L'MEMO4C(R8)                                                  
*                                                                               
         OC    DISC,DISC           Q, ANY DISCOUNT                              
         BZ    JOBDEB9             N, IGNORE CASH DISCOUNT POSTING              
*                                                                               
         ZAP   DUB1,2(6,R3)        Y, CALCULATE CASH DISCOUNT                   
         MP    DUB1,DISC                                                        
         DP    DUB1,=PL3'10000'                                                 
         CP    DUB1(5),=P'0'       Q, ANY CASH DISCOUNT AMOUNT                  
         BE    JOBDEB9             N, IGNORE POSTING                            
         AP    CDAMNT,DUB1(5)      CASH DISCOUNT ACCUMULATOR                    
         CLI   CDSW,C'Y'           Q, DO WE KEEP CASH DISCOUNT                  
         BE    JOBDEB9             Y, DON'T CREATE SUBSID. CASHEL.              
*                                                                               
         USING TRCASHD,R8          N, THEN CREATE SUBSID. CASH EL.              
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         ZAP   TRCSAMNT,DUB1(5)                                                 
         LA    R8,TRCSLNQ1(R8)                                                  
*                                                                               
JOBDEB9  CLI   MEMO50,X'00'        DO WE HAVE A 50 ELEMENT ?                    
         BE    JOBDEB12            NO                                           
*                                                                               
         MVC   0(L'MEMO50,R8),MEMO50                                            
         ZAP   TRCSAMNT,2(6,R3)                                                 
         OC    DISC,DISC           Q, IS THERE A CASH DISCOUNT                  
         BZ    JOBDEB10            N                                            
         CLI   CDSW,C'Y'           DO WE KEEP CASH DISCOUNT ?                   
         BE    JOBDEB10            YES                                          
         SP    TRCSAMNT,DUB1(5)    NO, SUBTRACT IT FROM AMOUNT                  
*                                                                               
JOBDEB10 LA    R8,TRCSLNQ1(R8)                                                  
         DROP  R8                                                               
*                                                                               
         USING DLPOSTD,R8                                                       
JOBDEB12 TM    ACOPSTAT,ACOXJOB    TEST FOR X-JOB                               
         BZ    *+14                                                             
         MVC   DLPOSTD(L'TRSELEM),TRSELEM  YES-INSERT TRANS STAT ELEM           
         LA    R8,L'TRSELEM(R8)                                                 
*                                                                               
         MVC   0(DLPSLNQ,R8),POSTEL SET MODEL POSTING ELEMENT                   
         MVC   DLPSANAL,0(R3)      WORK CODE                                    
         ZAP   DLPSAMNT,2(6,R3)    WORK CODE AMOUNT                             
         OC    DISC,DISC           Q, IS THERE A CASH DISCOUNT                  
         BZ    JOBDEB14            N                                            
         CLI   CDSW,C'Y'           Q, DO WE KEEP CASH DISCOUNT                  
         BE    JOBDEB14            Y, SKIP NEXT INSTRUCTION                     
         SP    DLPSAMNT,DUB1(5)    N, LESS CASH DISCOUNT                        
*                                                                               
JOBDEB14 TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    *+10                NO                                           
         ZAP   DLPSAMNT,=P'0'      YES, CLEAR AMOUNT                            
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
JOBDEB16 CLC   0(2,R4),SPACES      Q, NON-COMMISSIONABLE WORK CODE              
         BE    JOBDEB20            N, CONTINUE THROUGH LOOP                     
*                                                                               
         CLI   ANOELEM,X'00'       DO WE HAVE AN OFFICE ELEMENT?                
         BE    *+14                NO                                           
         MVC   0(L'ANOELEM,R8),ANOELEM                                          
         LA    R8,L'ANOELEM(R8)                                                 
*                                                                               
         CLI   PAKELEM,X'00'       DO WE HAVE A PAYABLE ELEMENT?                
         BE    *+14                NO                                           
         MVC   0(L'PAKELEM,R8),PAKELEM                                          
         LA    R8,L'PAKELEM(R8)                                                 
*                                                                               
         CLI   ORDNOEL,0           TEST TO ADD ORDER ELEMENT                    
         BE    *+14                                                             
         MVC   0(L'ORDNOEL,R8),ORDNOEL                                          
         LA    R8,L'ORDNOEL(R8)                                                 
*                                                                               
         CLI   MEMO4C,X'00'        DO WE HAVE A 4C ELEMENT ?                    
         BE    *+14                NO                                           
         MVC   0(L'MEMO4C,R8),MEMO4C                                            
         LA    R8,L'MEMO4C(R8)                                                  
*                                                                               
         CLI   MEMO50,X'00'        DO WE HAVE A 50 ELEMENT ?                    
         BE    JOBDEB18            NO                                           
*                                                                               
         USING TRCASHD,R8                                                       
         MVC   0(L'MEMO50,R8),MEMO50                                            
         ZAP   TRCSAMNT,2(6,R4)                                                 
         LA    R8,TRCSLNQ1(R8)                                                  
         DROP  R8                                                               
*                                                                               
         USING DLPOSTD,R8                                                       
JOBDEB18 TM    ACOPSTAT,ACOXJOB    TEST X-JOB                                   
         BZ    *+14                                                             
         MVC   DLPOSTD(L'TRSELEM),TRSELEM                                       
         LA    R8,L'TRSELEM(R8)                                                 
*                                                                               
         MVC   0(DLPSLNQ,R8),POSTEL SET MODEL ELEMENT                           
         MVC   DLPSANAL,0(R4)      NON-COMMISSIONABLE WORK CODE                 
         ZAP   DLPSAMNT,2(6,R4)    NON-COMMISSIONABLE AMOUNT                    
         OI    DLPSTYPE,X'40'      NON-COMMISSIONABLE TYPE                      
*                                                                               
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    *+10                NO                                           
         ZAP   DLPSAMNT,=P'0'      YES, CLEAR AMOUNT                            
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
JOBDEB20 LA    R3,8(R3)            NEXT COMMISSIONABLE WC AND AMT               
         LA    R4,8(R4)            NEXT NON-COMMISSIONABLE WC AND AMT           
         BCT   R2,JOBDEB8          LOOP THROUGH ALL 4 WC POSSIBILITIES          
*                                                                               
JOBDEBX  XIT1  REGS=(R8)                                                        
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LOOK AT ''SA'' OR ''SB'' LEDGER AND POSTINGS                           
*--------------------------------------------------------------                 
*                                                                               
CHECKSA  NTR1                                                                   
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),VENNUM+1                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CHECKNO                                                          
         LA    RF,IOAREA                                                        
         SR    R1,R1                                                            
CHECK02  CLI   0(RF),0             SEE IF LEDGER IS OPEN ITEM                   
         BE    CHECKNO                                                          
         CLI   0(RF),X'14'                                                      
         BE    *+14                                                             
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     CHECK02                                                          
         SPACE 1                                                                
         USING ACLEDGD,RF                                                       
         CLI   ACLTLIKE,C'R'                                                    
         BNE   XIT                                                              
         MVC   KEY(15),VENNUM                                                   
         BAS   RE,HIGH                                                          
         B     *+8                                                              
CHECK10  BAS   RE,SEQ                                                           
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CHECKNO                                                          
         LA    RF,IOAREA                                                        
         CLI   0(RF),X'43'                                                      
         BNE   CHECK12                                                          
         USING TRSUBHD,RF          SAVE CONTRA-ACCOUNT DETAILS                  
         SR    R1,R1                                                            
         MVC   SAVNAME,SPACES                                                   
         MVC   SAVNUM,TRSBACNT                                                  
         IC    R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         EX    R1,*+8                                                           
         B     CHECK10                                                          
         MVC   SAVNAME(0),TRSBNAME                                              
*                                                                               
CHECK12  CLI   0(RF),X'44'                                                      
         BNE   CHECK10                                                          
         USING TRANSD,RF                                                        
         TM    TRNSSTAT,X'80'      MUST BE DEBIT                                
         BZ    CHECK10                                                          
         CLC   SAVEDATE,TRNSDATE   AND MATCH ON DATE                            
         BNE   CHECK10                                                          
         L     R2,ADOCH                                                         
         SR    R1,R1                                                            
         IC    R1,DOCLEN                                                        
         BCTR  R1,0                AND REF                                      
         EX    R1,*+12                                                          
         BE    XIT                                                              
         B     CHECK10                                                          
         CLC   TRNSREF(0),8(R2)    EXECUTED                                     
*                                                                               
CHECKNO  MVI   ERRNUM,53                                                        
XIT      XIT1                                                                   
         EJECT                                                                  
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SVRE                                                          
         ST    R3,SVR3                                                          
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
         LA    R3,8(R2,R1)         GET LAST BYTE OF FIELD                       
         LA    RF,1(R1)            GET LENGTH OF FIELD                          
         CLI   0(R3),C' '          LOOK FOR SIGNIFICANT DATA                    
         BH    *+10                                                             
         BCTR  R3,0                                                             
         BCT   RF,*-10                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
MOVEFLDX L     R3,SVR3                                                          
         L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* US SCREEN TABLE  ( BYTES 0-3=DISP TO FIELD, BYTES 4-7=DISP TO ADCON )         
*                                                                               
USTAB    DS    0D                                                               
         DC    AL4(INUORDH-TWAD),AL4(AORDH-PROGD)                               
         DC    AL4(INUOHSTH-TWAD),AL4(AOHSTH-PROGD)                             
         DC    AL4(INUDOCH-TWAD),AL4(ADOCH-PROGD)                               
         DC    AL4(INUDATH-TWAD),AL4(ADATH-PROGD)                               
         DC    AL4(INUCLIH-TWAD),AL4(ACLIH-PROGD)                               
         DC    AL4(INUCLINH-TWAD),AL4(ACLINH-PROGD)                             
         DC    AL4(INUPROH-TWAD),AL4(APROH-PROGD)                               
         DC    AL4(INUPRONH-TWAD),AL4(APRONH-PROGD)                             
         DC    AL4(INUJOBH-TWAD),AL4(AJOBH-PROGD)                               
         DC    AL4(INUJOBNH-TWAD),AL4(AJOBNH-PROGD)                             
         DC    AL4(INUSUPH-TWAD),AL4(ASUPH-PROGD)                               
         DC    AL4(INUSUPNH-TWAD),AL4(ASUPNH-PROGD)                             
         DC    AL4(INUURGH-TWAD),AL4(AURGH-PROGD)                               
         DC    AL4(INUCDH-TWAD),AL4(ACDH-PROGD)                                 
         DC    AL4(INUWRKH-TWAD),AL4(AWRKH-PROGD)                               
         DC    AL4(INUWCNMH-TWAD),AL4(AWCNMH-PROGD)                             
         DC    AL4(INUAMTH-TWAD),AL4(AAMTH-PROGD)                               
         DC    AL4(INUNCWKH-TWAD),AL4(ANCWKH-PROGD)                             
         DC    AL4(INUWNNMH-TWAD),AL4(AWNNMH-PROGD)                             
         DC    AL4(INUNCAH-TWAD),AL4(ANCAH-PROGD)                               
         DC    AL4(INUCOFH-TWAD),AL4(ACOFH-PROGD)                               
         DC    AL4(INUCOFNH-TWAD),AL4(ACOFNH-PROGD)                             
         DC    AL4(INUAOFH-TWAD),AL4(AAOFH-PROGD)                               
         DC    AL4(INUDEPH-TWAD),AL4(ADEPH-PROGD)                               
         DC    AL4(INUSTFH-TWAD),AL4(ASTFH-PROGD)                               
         DC    AL4(INUSTFNH-TWAD),AL4(ASTFNH-PROGD)                             
         DC    AL4(INUEXPH-TWAD),AL4(AEXPH-PROGD)                               
         DC    AL4(INUEXPNH-TWAD),AL4(AEXPNH-PROGD)                             
         DC    AL4(INUFOFH-TWAD),AL4(AFOFH-PROGD)                               
         DC    AL4(INUFOFNH-TWAD),AL4(AFOFNH-PROGD)                             
         DC    AL4(INUNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    AL4(INUTOTH-TWAD),AL4(ATOTH-PROGD)                               
         DC    AL4(INUTOT-TWAD),AL4(ATOT-PROGD)                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* CA SCREEN TABLE  ( BYTES 0-3=DISP TO FIELD, BYTES 4-7=DISP TO ADCON )         
*                                                                               
CANTAB   DS    0D                                                               
         DC    AL4(INCORDH-TWAD),AL4(AORDH-PROGD)                               
         DC    AL4(INCOHSTH-TWAD),AL4(AOHSTH-PROGD)                             
         DC    AL4(INCDOCH-TWAD),AL4(ADOCH-PROGD)                               
         DC    AL4(INCDATH-TWAD),AL4(ADATH-PROGD)                               
         DC    AL4(INCCLIH-TWAD),AL4(ACLIH-PROGD)                               
         DC    AL4(INCCLINH-TWAD),AL4(ACLINH-PROGD)                             
         DC    AL4(INCPROH-TWAD),AL4(APROH-PROGD)                               
         DC    AL4(INCPRONH-TWAD),AL4(APRONH-PROGD)                             
         DC    AL4(INCJOBH-TWAD),AL4(AJOBH-PROGD)                               
         DC    AL4(INCJOBNH-TWAD),AL4(AJOBNH-PROGD)                             
         DC    AL4(INCSUPH-TWAD),AL4(ASUPH-PROGD)                               
         DC    AL4(INCSUPNH-TWAD),AL4(ASUPNH-PROGD)                             
         DC    AL4(INCURGH-TWAD),AL4(AURGH-PROGD)                               
         DC    AL4(INCCDH-TWAD),AL4(ACDH-PROGD)                                 
         DC    AL4(INCWRKH-TWAD),AL4(AWRKH-PROGD)                               
         DC    AL4(INCWCNMH-TWAD),AL4(AWCNMH-PROGD)                             
         DC    AL4(INCAMTH-TWAD),AL4(AAMTH-PROGD)                               
         DC    AL4(INCNCWKH-TWAD),AL4(ANCWKH-PROGD)                             
         DC    AL4(INCWNNMH-TWAD),AL4(AWNNMH-PROGD)                             
         DC    AL4(INCNCAH-TWAD),AL4(ANCAH-PROGD)                               
         DC    AL4(INCCOFH-TWAD),AL4(ACOFH-PROGD)                               
         DC    AL4(INCCOFNH-TWAD),AL4(ACOFNH-PROGD)                             
         DC    AL4(INCGTYPH-TWAD),AL4(ATYPEH-PROGD)                             
         DC    AL4(INCGTYNH-TWAD),AL4(ATYPNH-PROGD)                             
         DC    AL4(INCGORNH-TWAD),AL4(AGORNH-PROGD)                             
         DC    AL4(INCGSTXH-TWAD),AL4(AGSTXH-PROGD)                             
         DC    AL4(INCGAMTH-TWAD),AL4(AGAMTH-PROGD)                             
         DC    AL4(INCPROVH-TWAD),AL4(APROVH-PROGD)                             
         DC    AL4(INCAOFH-TWAD),AL4(AAOFH-PROGD)                               
         DC    AL4(INCDEPH-TWAD),AL4(ADEPH-PROGD)                               
         DC    AL4(INCSTFH-TWAD),AL4(ASTFH-PROGD)                               
         DC    AL4(INCSTFNH-TWAD),AL4(ASTFNH-PROGD)                             
         DC    AL4(INCEXPH-TWAD),AL4(AEXPH-PROGD)                               
         DC    AL4(INCEXPNH-TWAD),AL4(AEXPNH-PROGD)                             
         DC    AL4(INCFOFH-TWAD),AL4(AFOFH-PROGD)                               
         DC    AL4(INCFOFNH-TWAD),AL4(AFOFNH-PROGD)                             
         DC    AL4(INCNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    AL4(INCTOTH-TWAD),AL4(ATOTH-PROGD)                               
         DC    AL4(INCTOT-TWAD),AL4(ATOT-PROGD)                                 
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LITERAL DECLARATIONS                                                   
*--------------------------------------------------------------                 
*                                                                               
         LTORG                                                                  
NOCTAX   DC    C'** ERROR - CANADIAN SCREEN NOT AVAILABLE'                      
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        VALIDATE UP TO 4 WORK CODES AND AMOUNTS                                
*--------------------------------------------------------------                 
*                                                                               
VALWAM   DS    0D                                                               
         NMOD1 0,VALWAM            VALIDATE UPTO 4 (WORK CODES AMOUNTS)         
         L     RC,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                  R4=A(OUTPUT VALUES)                          
         USING VALD,R3             R3=A(WORK CODE FIELD HDR)                    
         NI    MULTWCSW,X'00'                                                   
         MVC   VALWNM,SPACES       CLEAR & TRANSMIT WORKCODE NAMES              
         OI    VALWNMH+6,X'80'                                                  
         MVC   0(2,R4),SPACES      CLEAR OUTPUT VALUES                          
         ZAP   2(6,R4),=P'0'                                                    
         MVC   8(2,R4),SPACES                                                   
         ZAP   10(6,R4),=P'0'                                                   
         MVC   16(2,R4),SPACES                                                  
         ZAP   18(6,R4),=P'0'                                                   
         MVC   24(2,R4),SPACES                                                  
         ZAP   26(6,R4),=P'0'                                                   
         MVI   ERRNUM,1                                                         
         LA    R2,VALWRKH                                                       
         CLI   VALWRKH+5,0         CHECK FOR INPUT IN BOTH FIELDS               
         BNE   VALW02                                                           
         CLI   VALAMTH+5,0                                                      
         BNE   VALWERR                                                          
         B     VALWOK              NO INPUT IN BOTH IS OK                       
*                                                                               
VALW02   LA    R2,VALAMTH                                                       
         CLI   VALAMTH+5,0                                                      
         BE    VALWERR                                                          
         LA    R2,VALWRKH          VALIDATE WORK CODES                          
         GOTO1 SCANNER,DMCB,(R2),(4,WORK)                                       
         MVI   ERRNUM,19                                                        
         CLI   4(R1),0                                                          
         BE    VALWERR                                                          
         MVC   SAVWLINE,4(R1)      SAVE NUMBER OF SCAN LINES                    
         MVI   THISLINE,1                                                       
         LA    R5,WORK             R5=A(SCAN BLOCK ENTRY)                       
         MVC   SVWCNMS,SPACES                                                   
         LA    R6,SVWCNMS                                                       
         LR    R8,R4                                                            
*                                                                               
         USING GOBLOCKD,R1                                                      
VALW04   L     R1,AGOBLOCK                                                      
         CLC   THISLINE,SAVWLINE                                                
         BH    VALW16                                                           
         MVI   ERRNUM,2                                                         
         CLI   0(R5),1             CHECK L'INPUT                                
         BL    VALWERR                                                          
         CLI   0(R5),2                                                          
         BH    VALWERR                                                          
         CLI   1(R5),0                                                          
         BNE   VALWERR                                                          
         MVI   ERRNUM,19                                                        
         MVC   0(2,R8),12(R5)                                                   
         CLC   0(2,R8),=C'99'                                                   
         BE    VALWERR                                                          
         LA    R1,GOUWLIST         CHECK IT'S A BILLABLE W/C                    
         BAS   RE,VALW06                                                        
         B     VALW10                                                           
         DROP  R1                                                               
*                                                                               
VALW06   LA    R0,6                                                             
VALW08   CLC   0(2,R8),0(R1)                                                    
         BE    VALWERR                                                          
         LA    R1,2(R1)                                                         
         BCT   R0,VALW08                                                        
         BR    RE                                                               
*                                                                               
VALW10   MVC   KEY,SPACES          NOW READ ANALYSIS RECORD                     
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),JOBNUM                                                  
         MVC   KEY+4(2),0(R8)                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'ACCOUNT',KEY,KEY                  
         MVI   ERRNUM,0                                                         
         CLI   8(R1),0             CHECK FOR ERRORS                             
         BNE   VALWERR                                                          
         LA    R1,IOAREA                                                        
         SR    RE,RE                                                            
*                                                                               
VALW12   CLI   0(R1),0             FIND NAME ELEMENT                            
         BE    VALW14                                                           
         CLI   0(R1),ACANELQ                                                    
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VALW12                                                           
*                                                                               
         USING ACANALD,R1                                                       
         TM    MULTWCSW,FSTWC                                                   
         BZ    *+14                                                             
         MVC   0(2,R6),=C', '                                                   
         LA    R6,2(R6)                                                         
         MVC   0(11,R6),ACANDESC   ONLY HAVE ROOM FOR 11                        
         GOTO1 RIGHT,DMCB,(R6),11                                               
*                                                                               
VALW14   LA    R5,32(R5)           BUMP TO NEXTS                                
         LA    R6,11(R6)                                                        
         OI    MULTWCSW,FSTWC                                                   
         LA    R8,8(R8)                                                         
         ZIC   R1,THISLINE                                                      
         LA    R1,1(R1)                                                         
         STC   R1,THISLINE                                                      
         B     VALW04                                                           
*                                                                               
VALW16   ST    R6,SVR6                                                          
         NI    MULTWCSW,X'FF'-FSTWC                                             
         ZIC   R6,SAVWLINE                                                      
         LTR   R6,R6                                                            
         BNP   VALW18                                                           
         MVC   TSTWCNMS,SVWCNMS                                                 
         GOTO1 SQUASHER,DMCB,TSTWCNMS,49 ELIMINATE MULT. SPACES                 
*                                                                               
VALW18   MVC   VALWNM(49),TSTWCNMS VALIDATE AMOUNTS                             
         L     R6,SVR6                                                          
         LA    R2,VALAMTH                                                       
         GOTO1 SCANNER,DMCB,(R2),WORK                                           
         MVI   ERRNUM,2                                                         
         CLI   4(R1),0                                                          
         BE    VALWERR                                                          
         MVI   ERRNUM,107                                                       
         CLI   4(R1),4                                                          
         BH    VALWERR                                                          
         MVC   SAVALINE,4(R1)      SAVE NUMBER OF SCAN LINES                    
         MVI   ERRNUM,1                                                         
         CLC   SAVALINE,SAVWLINE   CHECK SAME AS NUMBER OF W/C'S                
         BL    VALWERR                                                          
         LA    R2,VALWRKH                                                       
         BH    VALWERR                                                          
         MVI   THISLINE,1                                                       
         LA    R5,WORK             R5=A(SCAN BLOCK ENTRY)                       
         LR    R8,R4                                                            
         MVI   FVINDX,1                                                         
*                                                                               
VALW28   CLC   THISLINE,SAVALINE                                                
         BH    VALW32                                                           
         MVI   ERRNUM,2                                                         
         CLI   1(R5),0                                                          
         BNE   VALWERR                                                          
         ZIC   R0,0(R5)            CHECK FOR VALID CASH FILED                   
         GOTO1 AMTVAL,DMCB,12(R5),(R0)                                          
         MVI   ERRNUM,25                                                        
         CLI   0(R1),0                                                          
         BNE   VALWERR                                                          
         L     R1,4(R1)                                                         
         LA    R1,0(R1)                                                         
         ZAP   2(6,R8),0(8,R1)                                                  
         AP    CSHTOT,0(8,R1)                                                   
*                                                                               
         LA    R4,JOBNUM           RE-READ THE JOB W/WORKCODE                   
         GOTO1 ASETJOB,DMCB,(X'80',(R4)),(R8)                                   
         GOTO1 AOPTVAL                                                          
         BNE   VALWERR                                                          
*                                                                               
         CLI   CHECK,C' '          CHECKING AMOUNT ?                            
         BE    VALW30              NO                                           
         CLI   CHECK,C'T'          YES, CHECKING TOTAL?                         
         BE    VALW30              YES, SKIP FOR NOW                            
*                                                                               
         MVI   ERRNUM,30                                                        
         GOTO1 AWRKVAL,DMCB,(R8)                                                
         BH    VALWERR                                                          
*                                                                               
VALW30   LA    R5,32(R5)           BUMP TO NEXTS                                
         LA    R8,8(R8)                                                         
         ZIC   R1,THISLINE                                                      
         LA    R1,1(R1)                                                         
         STC   R1,THISLINE                                                      
         SR    R1,R1                                                            
         IC    R1,FVINDX                                                        
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         B     VALW28                                                           
*                                                                               
VALW32   CLI   CHECK,C'T'          ARE WE CHECKING THE TOTAL?                   
         BNE   VALWOK              NO                                           
         MVC   WORK,SPACES                                                      
         ZAP   WORK+2(6),CSHTOT                                                 
         LA    R8,WORK                                                          
         GOTO1 AWRKVAL,DMCB,(R8)                                                
         BH    VALWERR                                                          
*                                                                               
VALWOK   MVI   ERRNUM,X'FF'        SET CC=EQ IF ALL OK                          
VALWERR  CLI   ERRNUM,X'FF'                                                     
         XIT1  REGS=(R2)                                                        
         DROP  R1,R3,R6                                                         
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        SUB-ROUTINE TO EDIT TAX FIELDS AND DISPLAY DATA                        
*        ON EXIT, CC=EQ IF OK, NEQ IF ERROR AND R2 SET TO ERR FLD               
*--------------------------------------------------------------                 
EDTAX    DS    0D                                                               
         NMOD1 0,EDTAX                                                          
         L     RC,0(R1)                                                         
         MVI   GSTSW,C'N'                                                       
         MVI   ERRNUM,OK                                                        
         L     R2,ATYPEH                                                        
         CLI   8(R2),C'*'                                                       
         BE    EDTAXX                                                           
*                                                                               
         L     R2,AGORNH           POINT TO GROSS/NET                           
         MVI   ERRNUM,1            MISSING INPUT                                
         CLI   5(R2),0                                                          
         BE    EDTAXX                                                           
*                                                                               
         MVC   GORN,8(R2)                                                       
         MVI   ERRNUM,2                                                         
         CLI   GORN,C'G'                                                        
         BE    *+12                                                             
         CLI   GORN,C'N'                                                        
         BNE   EDTAXX                                                           
*                                                                               
         MVI   CTXMODE,C'G'                                                     
         MVI   ERRNUM,OK                                                        
         CLI   CTAXBEF,C'Y'        HAVE TO HAVE USE CTAX SCREEN                 
         BNE   EDTAXX                                                           
*                                                                               
EDTAX50  ZAP   TOTNET,TMPNET                                                    
         ZAP   TOTGRS,TMPGRS                                                    
         ZAP   TOTGST,TMPGST                                                    
         ZAP   TOTPST,TMPPST                                                    
*                                                                               
         ZAP   CSHTOT,TOTGRS                                                    
EDTAXX   CLI   ERRNUM,OK           YES-RESET VENDOR AMT=GROSS                   
XITR2    XIT1  REGS=(R2)                                                        
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        CLEAR UNUSED X-JOB FIELDS                                              
*--------------------------------------------------------------                 
*                                                                               
CLEARX   DS    0D                                                               
         NMOD1 0,**CLRX**                                                       
         L     RC,0(R1)                                                         
         USING PROGD,RC                                                         
         USING TWAD,RA                                                          
         USING GWS,R9                                                           
         MVC   FLD,SPACES                                                       
*                                                                               
         CLI   CSACT,ACTCHA        ENTERING FROM ITEM/CHANGE?                   
         BE    CLEARXIT                                                         
         L     R2,ACOFH                                                         
         TM    4(R2),X'80'         HAS ACCOUNT BEEN ENTERED ?                   
         BO    *+12                YES, USE IT                                  
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         L     R2,ACOFNH                                                        
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         L     R2,AEXPH                                                         
         TM    4(R2),X'80'         HAS ACCOUNT BEEN ENTERED ?                   
         BO    *+12                YES, USE IT                                  
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         L     R2,AEXPNH                                                        
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         L     R2,AFOFH                                                         
         TM    4(R2),X'80'         HAS ACCOUNT BEEN ENTERED ?                   
         BO    *+12                YES, USE IT                                  
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         L     R2,AFOFNH                                                        
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         L     R2,AAOFH                                                         
         TM    4(R2),X'80'         HAS ACCOUNT BEEN ENTERED ?                   
         BO    *+12                YES, USE IT                                  
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         L     R2,ADEPH                                                         
         TM    4(R2),X'80'         HAS ACCOUNT BEEN ENTERED ?                   
         BO    *+12                YES, USE IT                                  
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         L     R2,ASTFH                                                         
         TM    4(R2),X'80'         HAS ACCOUNT BEEN ENTERED ?                   
         BO    *+12                YES, USE IT                                  
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         L     R2,ASTFNH                                                        
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
CLEARXIT XMOD1                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        SET UP BLOCK FOR SALES/USE TAX                                         
*--------------------------------------------------------------                 
*                                                                               
TAXMOD   DS    0D                                                               
         NMOD1 0,**TAX**                                                        
         L     RC,0(R1)                                                         
         USING PROGD,RC                                                         
         USING TWAD,RA                                                          
         USING GWS,R9                                                           
         CLI   CSACT,ACTCHA        ENTERING FROM ITEM/CHANGE?                   
         BE    TAX50                                                            
         MVI   STXMODE,C'E'        EDIT MODE                                    
         CLI   MODE,2                                                           
         BE    TAX50                                                            
         MVI   STXMODE,C'B'        BUILD SCREEN                                 
*                                                                               
*              BUILD BLOCK FOR TAX OVERLAY                                      
*                                                                               
         MVC   STXACC(1),COMPANY                                                
         MVC   STXACC+1(2),=C'SJ'                                               
         L     R2,ACLIH                                                         
         ZIC   R1,5(R2)            CLIENT CODE                                  
         LTR   R1,R1                                                            
         BZ    TAX15               IF NO CLIENT                                 
         BCTR  R1,0                SKIP PRODUCT AND JOB                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STXACC+3(0),8(R2)                                                
*                                                                               
         ZIC   R4,CLILNGTH         LEVEL A LENGTH                               
         LA    R3,STXACC+3(R4)                                                  
         L     R2,APROH                                                         
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)          PRODUCT                                      
         BZ    TAX15                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)                                                    
*                                                                               
         ZIC   R4,PRDLNGTH         LEVEL B LENGTH                               
         LA    R3,STXACC+3(R4)                                                  
         L     R2,AJOBH                                                         
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)          JOB                                          
         BZ    TAX15                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)                                                    
*                                                                               
TAX15    L     R2,ADOCH                                                         
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)          REFERENCE(DOCUMENT)                          
         BZ    TAX20                                                            
         BCTR  R1,0                                                             
         CH    R1,=AL2(L'STXREF-1) NOT MORE THAN 6                              
         BH    *-6                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STXREF(0),8(R2)                                                  
*                                                                               
TAX20    L     R2,AORDH                                                         
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)          ORDER NUMBER                                 
         BZ    TAX25                                                            
         BCTR  R1,0                                                             
         CH    R1,=AL2(L'STXORD-1) NOT MORE THAN 6                              
         BH    *-6                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STXORD(0),8(R2)                                                  
*                                                                               
TAX25    L     R2,ADATH                                                         
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)          DATE                                         
         BZ    TAX30                                                            
         BCTR  R1,0                                                             
         CH    R1,=AL2(L'STXDTE-1) NOT MORE THAN 6                              
         BH    *-6                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STXDTE(0),8(R2)                                                  
*                                                                               
TAX30    L     R2,ACOFH                                                         
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)          CREDIT OFFICE                                
         BZ    TAX40                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STXCOFF(0),8(R2)                                                 
*                                                                               
TAX40    MVC   STXAMT,LCSHTOT      CASH AMOUNT                                  
         GOTO1 ANARRSCN,DMCB,ANARH,STXNARR                                      
*                                                                               
TAX50    LA    R3,X'40'                                                         
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,STXDATA,(R9)    GO TO TAX OVERLAY                      
         CLI   CSACT,ACTCHA        ENTERING FROM ITEM/CHANGE?                   
         BE    TAXXIT                                                           
         MVI   MODE,2                                                           
         CLI   STXMODE,C'E'        STILL IN EDIT MODE                           
         BE    TAXXIT              OVERLAY HAS SET-UP CURSOR                    
         MVI   MODE,0              SALES TAX IS DONE                            
         L     R2,AORDH                                                         
         ST    R2,FADR                                                          
         L     R2,AAMTH                                                         
         CLI   8(R2),C'*'          ENSURE AMOUNT IS INVALID                     
         BE    TAXXIT              IN CASE THEY ACCIDENTLY HIT ENTER            
         MVC   FLD+1(L'FLD-1),8(R2)                                             
         MVI   FLD,C'*'                                                         
         BAS   RE,MOVEFLD                                                       
TAXXIT   XMOD1                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LITERAL DECLARATION                                                    
*--------------------------------------------------------------                 
*                                                                               
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
         MVC   AMTBLK,WRKC1        WC AMOUNTS, COMM AND NON-COMM                
         MVC   CTXACC(1),COMPANY                                                
         MVC   CTXOFF,COFFICE                                                   
         L     R2,AGORNH                                                        
         MVC   CTXGORN,8(R2)                                                    
         L     R2,APROVH                                                        
         MVC   CTXPROV,8(R2)                                                    
         MVC   CTXDATE,SAVEDATE                                                 
         MVC   CTXACC,JOBNUM                                                    
         MVC   CTXVENGT,VENDTYPE                                                
         MVC   CTXVENPT,VENDPSTT                                                
         L     R2,ATYPEH                                                        
         MVC   CTXGSTT,8(R2)                                                    
         L     R2,ATYPNH                                                        
         MVC   CTXGSTTN,8(R2)                                                   
         L     R2,AGAMTH                                                        
         MVC   CTXLGSTA,5(R2)                                                   
         MVC   CTXGSTA,8(R2)                                                    
         MVC   CTXUPSTT,USERPST                                                 
         MVC   CTXUPROV,USERPROV                                                
*                                                                               
CTAX15   MVC   CTXCNTRA,VENNUM     VENDOR                                       
         MVC   CTXCNTRN,VENNAME                                                 
         OC    VENNUM,VENNUM       ANY VENDOR AVAILABLE?                        
         BNZ   CTAX50              NO, USE CONTRA ACCOUNT                       
         MVC   CTXCNTRA,CRDNUM                                                  
         MVC   CTXCNTRN,CRDNAME                                                 
*                                                                               
CTAX50   LA    R3,X'41'            CTAX SCREEN                                  
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,CTXDATA,(R9),AMTBLK                                    
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
         L     RF,AORDH                                                         
         CLI   5(RF),0             ANY ORDER?                                   
         BE    *+8                                                              
         MVI   MODE,1              BACK FROM ORDER                              
*                                                                               
         CLI   CTXMODE,C'Z'                                                     
         BE    *+8                                                              
         MVI   CTAXBEF,C'Y'        USED CTAX BEFORE                             
         MVC   GSTSW,CTXAPPL                                                    
*                                                                               
         MVC   TMPNET,CTXNET                                                    
         MVC   TMPGST,CTXGST                                                    
         MVC   TMPPST,CTXPST                                                    
         MVC   TMPGRS,CTXGRS                                                    
*                                                                               
         MVC   USERPST,CTXUPSTT                                                 
         CLI   CTXUPROV,C'Y'                                                    
         BNE   CTAX61                                                           
         MVI   USERPROV,C'Y'                                                    
         MVC   KEEPPROV,CTXPROV                                                 
*                                                                               
CTAX61   L     R2,AGORNH           SHOW CHANGES                                 
         MVC   8(1,R2),CTXGORN                                                  
         OI    6(R2),X'80'                                                      
         L     R2,APROVH                                                        
         MVC   8(2,R2),CTXPROV                                                  
         OI    6(R2),X'80'                                                      
         L     R2,ATYPEH                                                        
         MVC   8(1,R2),CTXGSTT                                                  
         OI    6(R2),X'80'                                                      
         L     R2,ATYPNH                                                        
         MVC   8(21,R2),CTXGSTTN                                                
         OI    6(R2),X'80'                                                      
         L     R2,AGAMTH                                                        
         MVC   5(1,R2),CTXLGSTA                                                 
         MVC   8(40,R2),CTXGSTA                                                 
CTAX65   OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AGSTXH                                                        
         XC    8(L'INCGSTX,R2),8(R2)                                            
         CLI   GSTSW,C'Y'                                                       
         BNE   CTAX69                                                           
         OC    CTXGST,CTXGST       DO WE HAVE GST?                              
         BZ    CTAX70                                                           
         L     R2,AGSTXH                                                        
         MVC   8(4,R2),=C'GST='                                                 
         EDIT  CTXGST,(14,12(R2)),2,ZERO=NOBLANK,ALIGN=LEFT                     
CTAX69   OI    6(R2),X'80'                                                      
*                                                                               
CTAX70   MVC   XTRAELM,CTXXTELM                                                 
         L     R2,ATYPEH           PUT A * IN AMT WHEN WE RETURN                
         ST    R2,FADR                                                          
         CLI   CTXMODE,C'G'        EXCEPT FOR 1 PASS                            
         BE    CTAXXIT                                                          
         OI    6(R2),X'01'         MODIFIED FIELD, TO HIT ENTER                 
*                                                                               
         CLI   CTXMODE,C'Z'                                                     
         BNE   CTAXXIT                                                          
         L     R2,AGORNH                                                        
         CLI   CTXERR,1                                                         
         BNE   *+8                                                              
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
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LITERAL POOL                                                           
*--------------------------------------------------------------                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        LOCAL STORAGE                                                          
*--------------------------------------------------------------                 
*                                                                               
PROGD    DSECT                                                                  
RELO1    DS    A                                                                
RIGHT    DS    V                                                                
*                                                                               
* SCREEN DIRECTORY                                                              
*                                                                               
AORDH    DS    A                   A(ORDER NUMBER HEADER)                       
AOHSTH   DS    A                   A(ORDER HISTORY HEADER)                      
ADOCH    DS    A                   A(DOCUMENT NUMBER HEADER)                    
ADATH    DS    A                   A(TRANSACTION DATE HEADER)                   
ACLIH    DS    A                   A(CLIENT HEADER)                             
ACLINH   DS    A                   A(CLIENT NAME HEADER                         
APROH    DS    A                   A(PRODUCT HEADER)                            
APRONH   DS    A                   A(PRODUCT NAME HEADER)                       
AJOBH    DS    A                   A(JOB HEADER)                                
AJOBNH   DS    A                   A(JOB NAME HEADER)                           
ASUPH    DS    A                   A(SUPPLIER HEADER)                           
ASUPNH   DS    A                   A(SUPPLIER NAME HEADER)                      
AURGH    DS    A                   A(URGENT HEADER)                             
ACDH     DS    A                   A(CASH DISCOUNT HEADER)                      
AWRKH    DS    A                   A(WORK CODE HEADER)                          
AWCNMH   DS    A                   A(WORK CODE NAMES HEADER)                    
AAMTH    DS    A                   A(AMOUNTS HEADER)                            
ANCWKH   DS    A                   A(NON-COMM WORKCODES HEADER)                 
AWNNMH   DS    A                   A(NON-COMM WORKCODE NAMES HEADER)            
ANCAH    DS    A                   A(AMOUNTS HEADER)                            
ACOFH    DS    A                   A(CREDIT OFFICE HEADER)                      
ACOFNH   DS    A                   A(CREDIT OFFICE NAME HEADER)                 
ATYPEH   DS    A                   A(GST TYPE HEADER)                           
ATYPNH   DS    A                   A(GST TYPE NAME FIELD HEADER)                
AGORNH   DS    A                   A(GROSS OR NET HEADER)                       
AGSTXH   DS    A                   A(GST EXTRA DATA HEADER)                     
AGAMTH   DS    A                   A(GST AMOUNTS)                               
APROVH   DS    A                   A(PST PROVINCE)                              
AAOFH    DS    A                   A(ANALYSIS OFFICE HEADER)                    
ADEPH    DS    A                   A(DEPARTMENT HEADER)                         
ASTFH    DS    A                   A(STAFF HEADER)                              
ASTFNH   DS    A                   A(STAFF NAME HEADER)                         
AEXPH    DS    A                   A(EXPENSE HEADER)                            
AEXPNH   DS    A                   A(EXPENSE NAME HEADER)                       
AFOFH    DS    A                   A(FINANCIAL OFFICE HEADER)                   
AFOFNH   DS    A                   A(FINANCIAL OFFICE NAME HEADER)              
ANARH    DS    A                   A(NARRATIVE HEADER)                          
ATOTH    DS    A                   A(TOTAL HEADER)                              
ATOT     DS    A                   A(TOTAL FIELD)                               
*                                                                               
SV3REGS  DS    0F                                                               
SVR2     DS    F                                                                
SVR3     DS    F                                                                
SVR4     DS    F                                                                
SVR6     DS    F                                                                
SVRE     DS    F                                                                
ELCODE   DS    C                                                                
*                                                                               
CRDNUM   DS    CL15                CREDIT ACCOUNT NUMBER                        
CRDNAME  DS    CL36                CREDIT ACCOUNT NAME                          
*                                                                               
VENNUM   DS    CL15                VENDOR NUMBER                                
VENNAME  DS    CL36                VENDOR NAME                                  
*                                                                               
JOBNUM   DS      CL15              JOB NUMBER                                   
JOBNAME  DS      CL36              JOB NAME                                     
*                                                                               
CLINUM   DS    CL15                CLIENT NUMBER                                
CLINAME  DS    CL36                CLIENT NAME                                  
*                                                                               
PRONUM   DS    CL15                PRODUCT NUMBER                               
PRONAME  DS    CL36                PRODUCT NAME                                 
*                                                                               
SAVNUM   DS    CL15                CONTRA ACCOUNT NUMBER OF SA POSTING          
SAVNAME  DS    CL36                CONTRA ACCOUNT NAME OF SA POSTING            
*                                                                               
CDSW     DS    CL1                 CD ACCOUNT INDICATOR                         
CDNUM    DS    CL15                CD ACCOUNT NUMBER                            
CDNAME   DS    CL36                CD ACCOUNT NAME                              
DISC     DS    PL3                 CD AMOUNT                                    
*                                                                               
SAVEDATE DS    CL3                                                              
SECNDSW  DS    CL1                                                              
OFFICE   DS    CL2                                                              
COFFICE  DS    CL2                                                              
*                                                                               
WRKC1    DS    CL2                                                              
COMAMNT1 DS    PL6                                                              
WRKC2    DS    CL2                                                              
COMAMNT2 DS    PL6                                                              
WRKC3    DS    CL2                                                              
COMAMNT3 DS    PL6                                                              
WRKC4    DS    CL2                                                              
COMAMNT4 DS    PL6                                                              
WRKNC1   DS    CL2                                                              
AMNTNC1  DS    PL6                                                              
WRKNC2   DS    CL2                                                              
AMNTNC2  DS    PL6                                                              
WRKNC3   DS    CL2                                                              
AMNTNC3  DS    PL6                                                              
WRKNC4   DS    CL2                                                              
AMNTNC4  DS    PL6                                                              
*                                                                               
CSHTOT   DS    PL6                                                              
CDAMNT   DS    PL6                                                              
*                                                                               
VENDOR   DS    CL1                 VENDOR REQUIRED INDICATOR                    
VENDTYPE DS    CL1                 VENDOR'S GST TYPE                            
VENDPSTT DS    CL1                 VENDOR'S PST TYPE                            
VENDPROV DS    CL2                 VENDOR'S PROVINCE, VNDR REC (LFM)            
*                                                                               
TOTNET   DS    PL6                                                              
TOTGRS   DS    PL6                                                              
TOTGST   DS    PL6                                                              
TOTPST   DS    PL6                                                              
*                                                                               
*STSW    DS    CL1                 Y=GST APPLICABLE, N=NOT APPLICABLE           
GORN     DS    CL1                 G=GROSS, N=NET                               
NAMTS    DS    XL1                 N'AMOUNT ENTRIES                             
NGST     DS    XL1                 N'GST AMOUNTS INPUT                          
AMTTAB   DS    8XL(AMTLNQ)                                                      
ALARGEST DS    A                                                                
*        SPACE 1                                                                
SVWCNMS  DS    CL((4*(L'ACANDESC-4+2))-2)                                       
TSTWCNMS DS    CL((4*(L'ACANDESC-4+2))-2)                                       
MULTWCSW DS    X         USED FOR PREFIXING ', ' BEFORE MULT WC NAMES           
FSTWC    EQU   B'10000000'                                                      
SAVWLINE DS    X                                                                
SAVALINE DS    X                                                                
THISLINE DS    X                                                                
DOCLEN   DS    CL1                                                              
DOCSAVE  DS    CL6                                                              
REFSAVE  DS    CL6                                                              
SVSTAT   DS    CL1                                                              
SVSTAT2  DS    CL1                                                              
POSTEL   DS    CL(DLPSLNQ)         POSTING ELEMENT AREA                         
ORDNOEL  DS    XL10                                                             
MEMO4C   DS    XL(L'ACKEYACC-1+2)                                               
MEMO50   DS    XL(TRCSLNQ1)                                                     
TRSELEM  DS    XL(TRSLNQ)                                                       
ANOELEM  DS    CL(ANOLNQ)                                                       
PAKELEM  DS    CL(PAKLNQ)                                                       
LPRO     DS    XL1                                                              
*                                                                               
EXCWORK  DS    0D                                                               
         DS    CL(EXCELNQ)                                                      
*                                                                               
       ++INCLUDE ACBATSTAX                                                      
       ++INCLUDE ACBATCTAX                                                      
*                                                                               
TMPMODE  DS    CL1                 TEMPORARY MODE                               
KEY      DS    CL49                                                             
IOAREA   DS    3000C                                                            
PROGDX   DS    0C                                                               
         EJECT                                                                  
*--------------------------------------------------------------                 
*        KEY DSECT                                                              
*--------------------------------------------------------------                 
*                                                                               
KEYD     DSECT                                                                  
KVENNUM  DS    CL15                                                             
         DS    CL17                                                             
KDATE    DS    XL3                                                              
KDOC     DS    CL6                                                              
*                                                                               
KEYSAVED DSECT                                                                  
KSVENNUM DS    CL15                                                             
         DS    CL17                                                             
KSDATE   DS    XL3                                                              
KSDOC    DS    CL6                                                              
         SPACE 2                                                                
VALD     DSECT                                                                  
VALWRKH  DS    CL8                                                              
VALWRK   DS    CL11                                                             
         DS    CL8                                                              
VALWNMH  DS    CL8                                                              
VALWNM   DS    CL49                                                             
         DS    CL8                                                              
         DS    CL17                                                             
VALAMTH  DS    CL8                                                              
VALAMT   DS    CL40                                                             
         SPACE 2                                                                
* DSECT TO COVER WORKCODE AMOUNT TABLE                                          
*                                                                               
AMTD     DSECT                                                                  
AMTWC    DS    CL2                 WORKCODE                                     
AMTNET   DS    PL6                 NET                                          
AMTGRS   DS    PL6                 GROSS                                        
AMTGST   DS    PL6                 GST                                          
AMTLNQ   EQU   *-AMTD                                                           
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATFED - SCREEN DSECT                                                
*--------------------------------------------------------------                 
*                                                                               
       ++INCLUDE ACBATFED                                                       
         EJECT                                                                  
         ORG   OSVALS                                                           
* TAX OVERLAY (ACBAT40) USES OSVALS+200                                         
*                                                                               
USERL    EQU   OSVALSL-(OSVALSL-200)                                            
USERAREA DS    0F                                                               
CLCDPASS DS    CL1                 OPTION BY CLIENT TO KEEP CD                  
LCSHTOT  DS    PL6                                                              
JOBSTAT  DS    X                   SAVE AREA FOR ACOPSTAT                       
AMTBLK   DS    CL64                                                             
GSTSW    DS    CL1                 FLAG FOR CTAX                                
CTAXBEF  DS    CL1                 USED CTAX BEFORE                             
KEEPPROV DS    CL2                 SAVE PROVINCE                                
TMPNET   DS    PL6                                                              
TMPGRS   DS    PL6                                                              
TMPGST   DS    PL6                                                              
TMPPST   DS    PL6                                                              
XTRAELM  DS    CL71                                                             
CLEAROK  DS    CL1                 FLAG FOR CLEAR GST/PST FIELD                 
USERPROV DS    CL1                 FLAG, USER ENTERED PROVINCE                  
USERPST  DS    CL1                 FLAG, USER ENTERED PST TYPE                  
         DS    CL(USERL-(*-USERAREA)) SPARE                                     
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATC1D - CANADIAN SCREEN DSECT                                       
*--------------------------------------------------------------                 
*                                                                               
         ORG   CONTABH                                                          
       ++INCLUDE ACBATC1D                                                       
         EJECT                                                                  
*FAUTL                                                                          
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
*ACEXCELD                                                                       
       ++INCLUDE ACEXCELD                                                       
         EJECT                                                                  
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENDAY                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066ACBAT01   12/10/14'                                      
         END                                                                    
