*          DATA SET ACBAT03    AT LEVEL 012 AS OF 12/10/14                      
*PHASE T61B03A                                                                  
*INCLUDE RIGHT                                                                  
         TITLE 'T61B03 CHECK - TYPE 3'                                          
T61B03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,T61B03,R7,CLEAR=YES,RR=R2                           
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         L     R1,=A(USTAB)                                                     
         AR    R1,R2                                                            
         ST    R1,AUSTAB                                                        
         L     R1,=A(CANTAB)                                                    
         AR    R1,R2                                                            
         ST    R1,ACANTAB                                                       
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
         L     R1,AUSTAB                                                        
         CLI   AGYCTRY,CTRYUSA     TEST FOR US SCREEN                           
         BE    *+8                                                              
         L     R1,ACANTAB          NO-ITS CANADIAN                              
*                                                                               
CHK02    CLI   0(R1),X'FF'         TEST FOR EOT                                 
         BE    CHK02X                                                           
         LM    RE,RF,0(R1)                                                      
         LA    RE,TWAD(RE)         FORM READ ADDRESS OF SCREEN FIELD            
         LA    RF,PROGD(RF)        FORM ADDRESS OF ADCON                        
         ST    RE,0(RF)                                                         
         LA    R1,L'USTAB(R1)                                                   
         B     CHK02                                                            
*                                                                               
CHK02X   GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         MVC   TMPMODE,MODE                                                     
         NI    TMPMODE,X'0F'                                                    
         CLI   TMPMODE,1                                                        
         BH    CHK02X2                                                          
         L     R2,ADATH                                                         
         MVI   ERRNUM,13                                                        
         CLI   5(R2),0                                                          
         BNE   CHK02X1                                                          
         BAS   RE,GETODAY                                                       
         B     CHK02X2                                                          
*                                                                               
CHK02X1  DS    0H                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*                                                                               
CHK02X2  DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,SAVEDATE)                                
         GOTO1 DATECHK,DMCB,SAVEDATE                                            
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
*                                                                               
         XC    SAVNUM,SAVNUM                                                    
         CLI   CSACT,ACTCHA                                                     
         BNE   *+16                                                             
         MVI   CLEAROK,C'N'        DON'T CLEAR GST/PST FOR CHANGE               
         MVI   USERPROV,C'Y'       USER ENTERED PROV FROM CHANGE                
         MVI   USERPST,C'Y'        USER ENTERED PST TYPE FOR CHANGE             
* CLEAR GST/PST FIELDS                                                          
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   CHK03                                                            
         CLI   MODE,2              CANNOT CLEAR WHEN IN ANOTHER                 
         BE    CHK03                 PROGRAM                                    
         CLI   MODE,3                                                           
         BE    CHK03                                                            
         CLI   CLEAROK,C'N'        NOT OK TO CLEAR?                             
         BE    CHK02Z3                                                          
         L     R2,AGORNH                                                        
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BO    CHK02Z1             ONLY CLEAR THE OLD ONES                      
         OI    6(R2),X'80'                                                      
         MVI   8(R2),0                                                          
         L     R2,AGSTXH                                                        
         MVC   8(L'CHCGSTX,R2),SPACES        CLEAR INFO DATA TOO                
         OI    6(R2),X'80'                                                      
         L     R2,ATYPNH                                                        
         MVC   8(L'CHCGTYN,R2),SPACES                                           
         OI    6(R2),X'80'                                                      
*                                                                               
CHK02Z1  L     R2,ATYPEH           CLEAR TYPE                                   
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
         BZ    CHK02Z2                                                          
         L     R2,APROVH                                                        
         TM    4(R2),X'80'         NEW PROVINCE?                                
         BO    CHK02Z8                                                          
         MVI   USERPROV,C'N'                                                    
         B     CHK02Z9                                                          
*                                                                               
CHK02Z2  L     R2,ASUPH                                                         
         TM    4(R2),X'80'                                                      
         BNO   CHK02Z3                                                          
         XC    VENDPROV,VENDPROV                                                
         L     R2,APROVH                                                        
         TM    4(R2),X'80'                                                      
         BO    CHK02Z3                                                          
         XC    8(2,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
CHK02Z3  L     R2,APROVH                                                        
         TM    4(R2),X'80'         NEW PROVINCE?                                
         BNO   CHK03               SAVE PROVINCE                                
CHK02Z8  MVI   USERPROV,C'Y'       USER ENTERED PROVINCE                        
CHK02Z9  MVC   KEEPPROV,8(R2)                                                   
         XC    XTRAELM,XTRAELM                                                  
*                                                                               
CHK03    MVI   CLEAROK,C'N'        DON'T CLEAR UNTIL DONE                       
         L     R2,RELO1                                                         
*-------------------------------------------------------------------            
*        MODE=X'00' EDIT SCREEN - INPUT                                         
*        MODE=X'01' EDIT SCREEN FROM P.O. RECALL                                
*        MODE=X'02' EDIT SALES/USE SCREEN                                       
*        MODE=X'03' EDIT CANADIAN TAX SCREEN                                    
*        MODE=X'80' VALID CASH AMOUNT FROM LAST INPUT                           
*-------------------------------------------------------------------            
*                                                                               
         CLI   CSACT,ACTCHA        ENTERING FROM ITEM/CHANGE?                   
         BNE   CHK04               NO                                           
         CLI   TWASCRN,BTS03G      NO, CHANGING TAX SCREEN?                     
         BE    CHK06               YES                                          
         CLI   PFKEY,X'FF'         FIRST TIME, IT SETS PFKEY TO X'FF'           
         BNE   *+8                 YES                                          
         MVI   PFKEY,0                                                          
*                                                                               
CHK04    CLI   MODE,2              SALES TAX                                    
         BE    CHK05                                                            
         CLI   MODE,3              OR CANADIAN TAX                              
         BE    CHK07                                                            
         TM    MODE,X'80'      DO I HAVE VALID AMOUNT FROM LAST INPUT           
         BO    *+10                                                             
         ZAP   LCSHTOT,=P'0'       IF NOT - INITIALIZE LAST AMOUNT              
         NI    MODE,X'FF'-X'80'                                                 
         CLI   PFKEY,7             USE PF=7 TO LOAD                             
         BNE   CHK04A              AND EDIT CANDIAN TAX SCREEN                  
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    CHK04A0                                                          
         LA    R2,CONACTH                                                       
         MVI   ERRNUM,SPECIAL                                                   
         MVC   MSG,SPACES                                                       
         MVC   FVMSGNO,=Y(AE$CSNA)                                              
         B     ERROR                                                            
*                                                                               
CHK04A0  ZAP   TOTNET,=P'0'                                                     
         ZAP   TOTGRS,=P'0'                                                     
         ZAP   TOTGST,=P'0'                                                     
         ZAP   QSTBAS,=P'0'                                                     
         B     CHK10               AND EDIT CANDIAN TAX SCREEN                  
CHK04A   CLI   PFKEY,9             USE PF=9 TO LOAD                             
         BNE   CHK08                 & EDIT SALES/USE TAX SCREEN                
*                                                                               
CHK05    TM    JOBSTAT,ACOXJOB     IS THIS AN X-JOB ?                           
         BZ    CHK06               NO                                           
         MVI   ERRNUM,47           YES, PRINT A MESSAGE                         
         LA    R2,CONACTH                                                       
         B     ERROR                                                            
*                                                                               
CHK06    DS    0H                                                               
         L     RF,=A(TAXMOD)                                                    
         AR    RF,R2                                                            
         MVI   CSSPROG,1           RESET THE PFKEYS                             
         GOTO1 (RF),DMCB,(RC)                                                   
         B     CURSIT                                                           
*                                                                               
CHK07    CLI   AGYCTRY,CTRYCAN                                                  
         BNE   CHK08                                                            
         L     RF,=A(CTAXMOD)      DO CANADIAN TAX SCREEN                       
         L     R2,RELO1                                                         
         AR    RF,R2                                                            
         MVI   CSSPROG,2           SEE PFKSPROG IN 00                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   CSACT,ACTCHA        ARE WE DOING A CHANGE?                       
         BNE   CURSIT              NO, LEAVE                                    
         CLC   FVMSGNO,=AL2(FVFOK) ON ERROR, WE'LL RETURN                       
         BNE   CURSIT                                                           
         MVC   FVMSGNO,=X'FFFF'    TELL BT61 WE HAVE TO COME BACK               
         B     CURSIT                                                           
*                                                                               
CHK08    CLI   PFKEY,0             OTHER PFKEYS ARE INVALID                     
         BE    CHK10                                                            
         MVI   ERRNUM,251                                                       
         LA    R2,CONACTH          INVALID PFKEY                                
         B     ERROR                                                            
*                                                                               
CHK10    L     R2,AORDH                                                         
         CLC   ORDNO,8(R2)         SAME ORDER - CARRY ON                        
         BE    CHK14                                                            
         MVI   MODE,0              OTHERWISE RE-READ                            
*                                                                               
CHK12    LA    R3,7                                                             
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,,(R9),WRKC1                                            
         L     R2,ADOCH                                                         
         CLI   MODE,1              EXIT WITH ORDER DISPLAYED                    
         BNE   *+16                                                             
         L     RE,AORDH                                                         
         NI    6(RE),X'FF'-X'40'   SWITCH OFF CURSOR                            
         B     EXIT                                                             
*                                                                               
         CLI   ERRNUM,X'FF'                                                     
         BE    CHK14               CARRY ON IF NO ORDER AND NO ERROR            
         B     CURSIT                                                           
*                                                                               
CHK14    DS    0H                                                               
         L     R2,ADOCH                                                         
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
         CLI   4(R1),1                                                          
         BE    CHK16               ONE ENTRY                                    
         CLI   WORK+32,6                                                        
         BH    ERROR                                                            
         MVC   REFSAVE,WORK+32+12                                               
         CLC   REFSAVE,SPACES      INPUT ENDED WITH A COMMA                     
         BE    ERROR                                                            
*                                                                               
CHK16    DS    0H                                                               
         EJECT                                                                  
*-------------------------------------------------------------------            
*              VALIDATE ALL INPUT ACCOUNTS & SAVE THEIR NAMES/NUMBERS           
*-------------------------------------------------------------------            
*                                                                               
         USING ACCOMPD,R5                                                       
         L     R2,ACLIH            ADDRESS CLIENT                               
         LA    R5,COMPEL                                                        
         BAS   RE,ANY                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPJOB    GET UNIT/LEDGER FROM COMPANY                 
         TM    4(R2),X'20'         HAS CLIENT CHANGED ?                         
         BO    CHK22               NO                                           
*                                                                               
         GOTO1 =A(CLEARX),DMCB,(RC),RR=RELO1 CLEAR UNUSED X-JOB FIELDS          
         L     R2,APROH            CLEAR VALIDATE FROM                          
         NI    4(R2),X'FF'-X'20'    PRODUCT AND JOB                             
         L     R2,AJOBH                                                         
         NI    4(R2),X'FF'-X'20'                                                
*                                                                               
         L     R2,ACLINH           CLEAR CLIENT NAME                            
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         L     R2,APRONH           CLEAR PRODUCT NAME                           
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         L     R2,AJOBNH           CLEAR JOB NAME                               
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
CHK22    MVI   ERRNUM,37                                                        
         L     R2,ACLIH            FORMAT KEY FOR READ                          
         CLC   5(1,R2),CLILNGTH                                                 
         BH    ERROR                                                            
*                                                                               
         SR    R3,R3                                                            
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
         TM    4(R2),X'20'         IF PREVIOUSLY VALIDATED, SKIP READ           
         BO    CHK24                                                            
         MVI   ERRNUM,14                                                        
         BAS   RE,READ                                                          
*                                                                               
         XC    CLIPROF,CLIPROF     READ ACCOUNT AND GET PROFILE                 
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,28                                                        
         TM    ACCTSTAT,X'10'      IS CLIENT LOCKED ?                           
         BO    ERROR               YES, ERROR                                   
*                                                                               
         L     R2,ACLINH           MOVE NAME TO SCREEN                          
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         BAS   RE,MOVEFLD                                                       
         L     R2,ACLIH                                                         
         OI    4(R2),X'20'         INDICATE VALIDATED                           
*                                                                               
CHK24    MVC   CLINUM,KEY          SAVE NUMBER AND NAME                         
         L     R2,ACLINH                                                        
         MVC   CLINAME,8(R2)                                                    
         OC    CLINAME,SPACES                                                   
*                                                                               
         OC    TWAACCS,TWAACCS     TEST FOR LIMITED ACCESS                      
         BZ    CHK26                                                            
         CLC   TWAACCS,SPACES                                                   
         BE    CHK26                                                            
         MVI   ERRNUM,55                                                        
         CLI   TWAACCS,C'*'        OFFICE CODE                                  
         BE    CHK26                                                            
         CLI   TWAACCS,C'$'        AND LIST WILL BE CHECK BY BASE               
         BE    CHK26                                                            
         CLC   TWAACCS(2),KEY+3    2 CHARACTER CLIENT MATCH                     
         BNE   ERROR                                                            
*                                                                               
CHK26    L     R2,APROH            ADDRESS PRODUCT NUMBER                       
         BAS   RE,ANY              ERROR IF NOT ENTERED                         
         TM    4(R2),X'20'         HAS PRODUCT CHANGED ?                        
         BO    CHK28               NO                                           
*                                                                               
         GOTO1 =A(CLEARX),DMCB,(RC),RR=RELO1 CLEAR UNUSED X-JOB FIELDS          
         L     R2,AJOBH            CLEAR VALIDATE FROM JOB                      
         NI    4(R2),X'FF'-X'20'                                                
*                                                                               
         L     R2,APRONH           CLEAR PRODUCT NAME                           
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         L     R2,AJOBNH           CLEAR JOB NAME                               
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
CHK28    LA    R4,KEY+3            GET POSITION FOR PRODUCT NUMBER              
         SR    R3,R3                                                            
         IC    R3,CLILNGTH                                                      
         AR    R4,R3                                                            
*                                                                               
         MVI   ERRNUM,37                                                        
         L     R2,APROH            FORMAT KEY FOR READ                          
         CLC   5(1,R2),LPRO                                                     
         BH    ERROR                                                            
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),8(R2)                                                    
*                                                                               
         TM    4(R2),X'20'         IF PREVIOUSLY VALIDATED, SKIP READ           
         BO    CHK30                                                            
*                                                                               
         XC    PRODPROF,PRODPROF   READ ACCOUNT AND GET PROFILE                 
         MVI   ERRNUM,15                                                        
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
*                                                                               
         MVI   ERRNUM,28                                                        
         TM    ACCTSTAT,X'10'      IS PRODUCT LOCKED ?                          
         BO    ERROR               YES, ERROR                                   
*                                                                               
         L     R2,APRONH           MOVE NAME TO SCREEN                          
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         BAS   RE,MOVEFLD                                                       
         L     R2,APROH                                                         
         OI    4(R2),X'20'         INDICATE VALIDATED                           
*                                                                               
CHK30    MVC   PRONUM,KEY          SAVE NUMBER AND NAME                         
         L     R2,APRONH                                                        
         MVC   PRONAME,8(R2)                                                    
         OC    PRONAME,SPACES                                                   
*                                                                               
         L     R2,AJOBH            ADDRESS JOB NUMBER                           
         BAS   RE,ANY              ERROR IF NOT ENTERED                         
         TM    4(R2),X'20'         HAS JOB CHANGED ?                            
         BO    CHK31A              NO                                           
*                                                                               
         GOTO1 =A(CLEARX),DMCB,(RC),RR=RELO1 CLEAR UNUSED X-JOB FIELDS          
*                                                                               
CHK31A   LA    R4,KEY+3            GET POSITION FOR JOB NUMBER                  
         SR    R3,R3                                                            
         IC    R3,PRDLNGTH                                                      
         AR    R4,R3                                                            
*                                                                               
         SR    R3,R3               FORMAT KEY FOR READ                          
         IC    R3,5(R2)                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),8(R2)                                                    
*                                                                               
         XC    JOBPROF,JOBPROF     READ ACCOUNT AND GET PROFILE                 
         MVI   ERRNUM,16                                                        
         LA    R6,JOBPROF                                                       
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC         CHECK STATUS                                 
*                                                                               
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'      DOES JOB HAVE BALANCE ELEMENT ?              
         BZ    ERROR               NO, ERROR                                    
         MVI   ERRNUM,23                                                        
         TM    ACCTSTAT,X'20'      IS JOB CLOSED ?                              
         BO    ERROR               YES, ERROR                                   
         MVI   ERRNUM,28                                                        
         TM    ACCTSTAT,X'10'      IS JOB LOCKED ?                              
         BO    ERROR               YES, ERROR                                   
*                                                                               
         L     R2,AJOBNH           MOVE NAME TO SCREEN                          
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         BAS   RE,MOVEFLD                                                       
         L     R2,AJOBH                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
         MVC   JOBNUM,KEY          SAVE NUMBER AND NAME                         
         L     R2,AJOBNH                                                        
         MVC   JOBNAME,8(R2)                                                    
         OC    JOBNAME,SPACES                                                   
*                                                                               
         LA    R4,JOBNUM           DO GETOPT CALL                               
         GOTO1 ASETJOB,DMCB,(R4)                                                
         MVC   JOBSTAT,ACOPSTAT    SAVE JOB STATUS BYTE                         
*                                                                               
         USING GOBLOCKD,R4                                                      
         L     R4,AGOBLOCK                                                      
         MVC   CLCDPASS,GOCLICD    SAVE CD OPTION                               
*                                                                               
         BAS   RE,PROFMERG         MERGE PROFILES                               
         LA    R4,PROFILE          GET OFFICE CODE                              
         USING ACPROFD,R4                                                       
         MVC   OFFICE,ACPROFFC                                                  
         MVC   COFFICE,ACPROFFC    SAVE FOR SC POSTING                          
         LA    R4,JOBPROF                                                       
         CLC   ACPROFFC,SPACES                                                  
         BNH   *+16                                                             
         MVC   OFFICE,ACPROFFC                                                  
         MVC   COFFICE,ACPROFFC                                                 
*                                                                               
         L     R2,ACOFH            CREDIT OFFICE                                
         CLI   5(R2),0             WAS OFFICE OVERRIDDEN ?                      
         BE    CHK31               NO, LEAVE IT ALONE                           
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
CHK31    TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    CHK32               NO, JUST USE CLIENT/PRODUCT OFFICE           
         L     R4,AGOXBLK                                                       
         OC    GOAWOFOF,GOAWOFOF   DOES OPTION MAINT HAVE AN OFFICE ?           
         BZ    *+10                NO, LEAVE IT ALONE                           
         MVC   OFFICE,GOAWOFOF     YES, SAVE THAT ONE                           
         DROP  R4                                                               
*                                                                               
         L     R2,AFOFH                                                         
         CLI   5(R2),0             WAS OFFICE OVERRIDDEN ?                      
         BE    CHK32               NO, LEAVE IT ALONE                           
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
CHK32    OI    4(R2),X'20'         INDICATE FINCIAL OFFICE VALIDATED            
         L     R2,ATOTH            CLEAR TOTAL FIELD                            
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         L     R2,ACRDH            ADDRESS CREDIT ACCOUNT                       
         BAS   RE,ANY              ERROR IF NOT ENTERED                         
         TM    4(R2),X'20'         HAS CREDIT ACCOUNT CHANGED ?                 
         BO    CHK34               NO                                           
*                                                                               
         L     R2,ACRDNH           CLEAR CREDIT ACCOUNT NAME                    
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
CHK34    L     R2,ACRDH            FORMAT KEY FOR READ                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)                                                   
*                                                                               
         USING ACCOMPD,R5                                                       
         LA    R5,COMPEL                                                        
         MVC   KEY+1(2),ACMPBANK   GET U/L FROM COMPANY                         
         CLI   KEY+3,C'*'          WAS U/L OVERRIDDEN ?                         
         BNE   CHK36               NO                                           
         MVC   KEY+1(20),SPACES    YES, SHIFT DATA OVER                         
         MVC   KEY+1(14),9(R2)                                                  
         OC    KEY+1(14),SPACES                                                 
*                                                                               
         CLC   ACMPPETY,KEY+1      IS THIS A PETTY CASH ACCOUNT ?               
         BE    CHK36               YES                                          
         CLC   =C'SA',KEY+1        NO, CHECK IF ALLOWABLE                       
         BE    CHK36                                                            
         CLC   =C'SB',KEY+1                                                     
         BE    CHK36                                                            
         CLC   =C'SR',KEY+1                                                     
         BE    CHK36                                                            
*                                                                               
         CLI   AGYCTRY,CTRYCAN     TEST CANADIAN SCREEN                         
         BNE   ERROR               NO                                           
         USING CPYELD,R6                                                        
         LA    R6,BCCPYEL                                                       
         CLC   72(2,R6),KEY+1                                                   
         BNE   ERROR                                                            
         DROP  R6                                                               
*                                                                               
CHK36    SR    R6,R6               NO PROFILES NEEDED                           
         BAS   RE,GETACC                                                        
         MVC   SVSTAT,ACCTSTAT                                                  
         BAS   RE,CHECKACC         CHECK STATUS                                 
         MVC   CRDNUM,ACCTNUM                                                   
         MVC   CRDNAME,ACCTNAME                                                 
*                                                                               
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED ?                       
         BO    CHK38               YES, NO NEED TO PUT ON SCREEN                
         L     R2,ACRDNH           MOVE NAME TO SCREEN                          
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         BAS   RE,MOVEFLD                                                       
         L     R2,ACRDH                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
CHK38    CLC   CRDNUM+1(2),=C'SA'  IS THIS AN ADVANCE ?                         
         BNE   CHK39               NO                                           
         L     R2,ADOCH            YES, VALIDATE AND GET DETAILS                
         MVI   ERRNUM,0                                                         
         BAS   RE,CHECKSA                                                       
         CLI   ERRNUM,0            ANY ERRORS ?                                 
         BNE   ERROR               YES                                          
         EJECT                                                                  
CHK39    SR    R6,R6               PROFILES NOT REQUIRED                        
         MVI   ERRNUM,1                                                         
         L     R2,AWRKH            ONE W/C FIELD MUST BE INPUT                  
         L     RE,ANCWKH                                                        
         CLI   5(RE),0                                                          
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         ZAP   CSHTOT,=P'0'                                                     
*                                  VALIDATE COMMISSIONABLE FIELDS               
         L     R3,AWRKH                                                         
         LA    R4,WRKC1                                                         
         BRAS  RE,VALWAM                                                        
         BNE   ERROR                                                            
*                                  VALIDATE NON-COMMISSIONABLE FIELDS           
         L     R3,ANCWKH                                                        
         LA    R4,WRKNC1                                                        
         BRAS  RE,VALWAM                                                        
         BNE   ERROR                                                            
*                                                                               
*                                                                               
         L     R2,ACOFNH           CLEAR CREDIT OFFICE NAME                     
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         L     R2,ACOFH                                                         
         CLI   5(R2),0             WAS CREDIT OFFICE ENTERED ?                  
         BNE   CHK40               YES, USE IT                                  
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'COFFICE),COFFICE                                           
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
CHK40    MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY      GET OFFICE NAME                              
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
CHK41    L     R2,ACOFH                                                         
         OI    4(R2),X'20'         CREDIT OFFICE VALIDATED                      
         EJECT                                                                  
         XC    VENNUM,VENNUM       DEAL WITH VENDOR POSTING                     
         MVI   VENDOR,C'N'                                                      
*                                                                               
         L     R2,ASUPNH           CLEAR SUPPLIER NAME                          
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         L     R2,ASUPH                                                         
         TM    COMPSTA2,X'04'      IS SUPPLIER REQUIRED ?                       
         BZ    CHK42               NO                                           
         CLC   CRDNUM+1(2),=C'SR'  YES, IS CREDIT SR?                           
         BE    CHK42               YES, VENDOR NOT REQUIRED                     
         BAS   RE,ANY              NO, ERROR IF NOT THERE                       
         B     CHK44                                                            
*                                                                               
CHK42    CLI   5(R2),0             WAS SUPPLIER ENTERED ?                       
         BE    CHK62               NO                                           
*                                                                               
CHK44    MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPSUPP   YES, GET U/L FROM COMPANY                    
*                                                                               
CHK46    SR    R3,R3               FORM KEY FOR READ                            
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)                                                   
*                                                                               
         CLC   KEY+3(3),=C'*SW'    WAS U/L OVERRIDEM ?                          
         BE    CHK48               YES, SHIFT KEY                               
         CLC   KEY+3(3),=C'*SX'                                                 
         BE    CHK48                                                            
         CLC   KEY+3(3),=C'*SV'                                                 
         BE    CHK48                                                            
         CLC   KEY+3(3),=C'*SY'                                                 
         BNE   CHK50                                                            
*                                                                               
CHK48    MVC   KEY+1(20),SPACES                                                 
         MVC   KEY+1(14),9(R2)                                                  
         OC    KEY+1(14),SPACES                                                 
*                                                                               
CHK50    BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   SVSTAT,ACCTSTAT                                                  
         MVC   VENNUM,ACCTNUM      SAVE ACCOUNT NUMBER AND NAME                 
         MVC   VENNAME,ACCTNAME                                                 
*                                                                               
         L     R2,ASUPNH           MOVE NAME TO SCREEN                          
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         BAS   RE,MOVEFLD                                                       
*                                                                               
         BAS   RE,HIGH             REREAD ACCOUNT                               
         MVI   ERRNUM,18                                                        
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR                                                            
         LA    R4,IOAREA                                                        
*                                                                               
CHK52    CLI   0(R4),0                                                          
         BE    CHK59                                                            
         CLI   0(R4),ACADELQ       ADDRESS ELEMENT                              
         BE    CHK58                                                            
         CLI   0(R4),ITCELQ        TAX TYPE ELEMENT                             
         BE    CHK56                                                            
*                                                                               
CHK54    SR    R3,R3                                                            
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     CHK52                                                            
*                                                                               
         USING ITCELD,R4                                                        
CHK56    CLC   SAVEDATE,ITCEFFD    IS TRANS DATE > EFFECTIVE DATE ?             
         BL    CHK54               NO                                           
         OC    ITCPROV,ITCPROV     NO PROV = GST                                
         BZ    CHK56G                                                           
         MVC   VENDPSTT,ITCTYPE    SAVE THE PST TYPE                            
         MVC   VENDPROV,ITCPROV    AND THE PROVINCE                             
         B     CHK54                                                            
*                                                                               
CHK56G   MVC   VENDTYPE,ITCTYPE    YES, SAVE THIS TYPE                          
         B     CHK54                                                            
*                                                                               
CHK58    L     RE,ASUPNH           BUMP PAST SUPPLIER NAME                      
         LA    RE,8(RE)                                                         
         MVI   25(RE),C' '                                                      
         MVC   26(10,RE),3(R4)        DISPLAY 1ST 10 OF ADDRESS                 
         B     CHK54                                                            
*                                                                               
CHK59    CLI   USERPROV,C'Y'       DID USER ENTERED PROVINCE?                   
         BE    CHK60               USE IT INSTEAD                               
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   CHK60                                                            
         L     R2,APROVH                                                        
         MVI   5(R2),2                                                          
         OI    6(R2),X'80'         MODIFY FOR NEXT INPUT                        
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6STRO        STEREO?                          
         BZ    CHK59A                                                           
         CLC   VENDPROV,SPACES                                                  
         BNH   CHK60                                                            
CHK59A   MVC   8(2,R2),VENDPROV                                                 
*                                                                               
CHK60    TM    SVSTAT,X'04'        IS SUPPLIER REQUIRED ?                       
         BZ    CHK62               NO                                           
*                                                                               
         L     R2,ASUPH            YES, MUST ALSO HAVE '2C' AN '27'             
         MVC   KEY+1(2),=C'2C'                                                  
         BAS   RE,GETACC                                                        
*                                                                               
         MVI   VENDOR,C'Y'                                                      
         MVC   V2CNUM,ACCTNUM                                                   
         MVC   V2CNAM,ACCTNAME                                                  
*                                                                               
         MVC   KEY+1(14),=CL14'27999'                                           
         BAS   RE,GETACC                                                        
         MVC   V27NUM,ACCTNUM                                                   
         MVC   V27NAME,ACCTNAME                                                 
         EJECT                                                                  
* EDIT GST FIELDS IF CANADA                                                     
*                                                                               
CHK62    CLI   AGYCTRY,CTRYCAN     TEST CANADIAN SCREEN                         
         BNE   CHK64               NO                                           
*                                                                               
CHK62NC  CLI   PFKEY,7             DO THIS SO WE GET VENDOR #                   
         BNE   CHK63                                                            
         CLI   MODE,1                                                           
         BH    CHK63                                                            
         B     CHK07                                                            
*                                                                               
         USING CPYELD,R5                                                        
CHK63    LA    R5,BCCPYEL                                                       
         CLC   72(2,R5),CRDNUM+1   TEST FOR CREDIT TO SG                        
         BE    CHK64                                                            
         BAS   RE,EDTAX            WE ARE READY TO CHECK                        
         BE    CHK63A                                                           
         CLI   ERRNUM,OK           ERROR OCCURED                                
         BNE   ERROR                                                            
         DROP  R5                                                               
*                                                                               
CHK63A   L     RF,=A(CTAXMOD)                                                   
         L     RE,RELO1                                                         
         AR    RF,RE                                                            
         MVC   MSG,SPACES                                                       
         MVC   TMPMODE,MODE                                                     
         MVI   MODE,4              WANT TO GO THROUGH                           
         GOTO1 (RF),DMCB,(RC)                                                   
         MVC   MODE,TMPMODE                                                     
         CLI   CTXMODE,C'Z'                                                     
         BNE   CHK63X                                                           
         MVI   FVOMTYP,C'E'                                                     
         MVC   FVMSGNO,CTXMSGNO                                                 
         L     R2,FVADDR                                                        
         B     ERROR                                                            
*                                                                               
CHK63X   MVI   FVOMTYP,C'E'                                                     
         XC    FVMSGNO,FVMSGNO                                                  
         ZAP   TOTNET,TMPNET                                                    
         ZAP   TOTGRS,TMPGRS                                                    
         ZAP   TOTGST,TMPGST                                                    
         ZAP   TOTPST,TMPPST                                                    
         ZAP   CSHTOT,TMPGRS                                                    
         MVC   WRKC1(64),AMTBLK                                                 
*                                                                               
CHK64    LA    R5,COMPEL                                                        
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         USING ACCOMPD,R5                                                       
         L     R2,AEXPH                                                         
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BO    CHK66               YES                                          
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
         B     CHK76                                                            
*                                                                               
*                                                                               
         USING GOXBLOCK,R4                                                      
CHK66    L     R4,AGOXBLK          GET EXTENDED GOBLOCK                         
         L     R2,AEXPH                                                         
         CLI   5(R2),0             WAS AN EXPENSE ACCOUNT ENTERED ?             
         BNE   CHK70               YES, USE IT                                  
         MVC   FLD,SPACES                                                       
         OC    GOAWOA,GOAWOA       NO, IS THERE ONE IN OPTION MAINT ?           
         BZ    CHK68               NO, ERROR                                    
         CLC   GOAWOA(2),=C'SE'    IS THIS AN 'SE' ACCOUNT ?                    
         BE    *+18                YES                                          
         MVI   FLD,C'*'            NO, INDICATOR OVERRIDE                       
         MVC   FLD+1(L'GOAWOA),GOAWOA                                           
         B     *+10                                                             
         MVC   FLD(L'GOAWOA-2),GOAWOA+2                                         
*                                                                               
CHK68    BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
         BAS   RE,ANY              X-JOBS MUST HAVE EXPENSE                     
*                                                                               
CHK70    OI    4(R2),X'20'         EXPENSE VALIDATED                            
         L     R2,AFOFH                                                         
         CLI   5(R2),0             WAS A FINANCIAL OFFICE ENTERED ?             
         BNE   CHK72               YES, USE IT                                  
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWOFOF),GOAWOFOF                                         
         OC    GOAWOFOF,GOAWOFOF                                                
         BNZ   *+10                                                             
         MVC   FLD(L'OFFICE),OFFICE                                             
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
CHK72    MVC   KEY,SPACES                                                       
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
         CLI   5(R2),0             WAS ANALYSIS OFFICE ENTERED ?                
         BNE   CHK73               YES, USE IT                                  
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWOAOF),GOAWOAOF                                         
         OC    GOAWOAOF,GOAWOAOF                                                
         BNZ   *+10                                                             
         MVC   FLD(L'OFFICE),OFFICE                                             
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
CHK73    OI    4(R2),X'20'         ANALYSIS OFFICE VALIDATED                    
         L     R2,ADEPH                                                         
         CLI   5(R2),0             WAS A DEPARTMENT ENTERED ?                   
         BNE   CHK74               YES, USE IT                                  
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWODEP),GOAWODEP                                         
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
CHK74    OI    4(R2),X'20'         DEPARTMENT VALIDATED                         
         L     R2,ASTFH                                                         
         CLI   5(R2),0             WAS A STAFF ENTERED ?                        
         BNE   CHK75               YES, USE IT                                  
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWOSTF),GOAWOSTF                                         
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
         DROP  R4                                                               
*                                                                               
         USING EXCELD,R4                                                        
CHK75    OI    4(R2),X'20'         STAFF VALIDATED                              
         LA    R4,EXCWORK          ADDRESS ACEXCEL WORKAREA                     
         MVI   EXCACT,EXCAVAL      VALIDATE MODE                                
         MVC   EXCAEXP,AEXPH       PASS A(EXPENSE ACCOUNT HEADER)               
         MVC   EXCAANO,AAOFH       PASS A(ANALYSIS OFFICE HEADER)               
         MVC   EXCADEP,ADEPH       PASS A(DEPARTMENT HEADER)                    
         MVC   EXCASTF,ASTFH       PASS A(STAFF HEADER)                         
         MVC   EXCSECAC,CRDNUM     PASS CREDIT NUMBER                           
         MVC   EXCSECAN,CRDNAME    PASS CREDIT NAME                             
         MVC   EXCCLNT,CLINUM+3    CLIENT                                       
         MVC   EXCPROD,PRONUM+6    PRODUCT                                      
         OC    VENNUM,VENNUM       DO WE HAVE A VENDOR NUMBER ?                 
         BZ    CHK75A              NO, LEAVE IT AS IS                           
         TM    COMPSTA3,X'08'      YES, IS CONTRA ALWAYS CREDIT ?               
         BO    CHK75A              YES, LEAVE IT AS IS                          
         MVC   EXCSECAC,VENNUM     NO, PASS VENDOR NUMBER                       
         MVC   EXCSECAN,VENNAME    PASS VENDOR NAME                             
*                                                                               
CHK75A   ST    R9,EXCAGWS                                                       
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
*-------------------------------------------------------------------            
*        BUILD 64 ELEMENT                                                       
*-------------------------------------------------------------------            
*                                                                               
CHK76    LA    R8,IOAREA+2                                                      
         SR    R3,R3                                                            
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,DLDSELQ                                                   
         MVC   DLDSREF,DOCSAVE                                                  
         MVC   DLDSDATE,SAVEDATE                                                
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
*                                                                               
         XC    DLDSSTAT+1(6),DLDSSTAT+1                                         
         L     R2,ANARH                                                         
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         BAS   RE,NARRSCAN                                                      
         SR    R3,R3                                                            
         LA    R5,DLDSNARR                                                      
         SR    R5,R8               R5 = ELEMENT - NARRATIVE                     
         AR    R5,R6               R6 = L'NARRATIVE                             
         STH   R5,HALF                                                          
         STH   R5,HALF                                                          
         MVC   DLDSLEN,HALF+1                                                   
         EJECT                                                                  
*---------------------------------------------------------------                
*        BUILD 69&6A MAIN ACCOUNTING ELEMENTS                                   
*---------------------------------------------------------------                
*                                                                               
         AR    R8,R5                                                            
         XC    ORDNOEL,ORDNOEL                                                  
         XC    OTHEREL,OTHEREL                                                  
         XC    WORK69,WORK69                                                    
         XC    MEMO50,MEMO50                                                    
         XC    MEMO4C,MEMO4C                                                    
         XC    TRSELEM,TRSELEM                                                  
*                                                                               
         CLC   REFSAVE,SPACES                                                   
         BE    CHK78                                                            
         USING ACOTHERD,R1                                                      
         LA    R1,OTHEREL                                                       
         MVI   ACOTEL,X'23'                                                     
         MVI   ACOTLEN,ACOTLNQ1                                                 
         MVC   ACOTNUM(13),SPACES                                               
         MVC   ACOTNUM(L'REFSAVE),REFSAVE                                       
*                                                                               
CHK78    L     RE,AORDH                                                         
         CLI   5(RE),0                                                          
         BE    CHK80                                                            
*                                                                               
         USING ACNOD,R1            EXTRA NO. ELEMENT FOR ORDER NUMBER           
         LA    R1,ORDNOEL                                                       
         MVC   ACNOEL(2),=X'250A'                                               
         MVC   ACNO(6),ORDNO                                                    
         LA    RE,8(RE)            POINT TO FIELD                               
         CLI   7(RE),C'P'          PARTIAL ORDER?                               
         BNE   *+8                                                              
         MVI   ACNOSTAT,C'P'       YES, SET PARTIAL FLAG                        
         DROP  R1                                                               
*                                                                               
CHK80    TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    CHK82               NO                                           
         USING TRCASHD,R1                                                       
         LA    R1,MEMO50                                                        
         MVI   TRCSEL,TRCSELQ      YES, BUILD MEMO WITH AMOUNT                  
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'S'                                                    
         ZAP   TRCSAMNT,=P'0'                                                   
         DROP  R1                                                               
*                                                                               
         LA    R1,TRSELEM                                                       
         USING TRSELD,R1                                                        
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         OI    TRSSTAT3,TRSSXJOB   SET X-JOB INDICATOR                          
         DROP  R1                                                               
*                                                                               
         USING TRSDESCD,R1                                                      
         LA    R1,MEMO4C           BUILD MEMO WITH ACCOUNT                      
         MVI   TRSDEL,TRSDELQ                                                   
         MVC   TRSDLEN,=YL1(L'ACKEYACC-1+2)                                     
         MVC   TRSDACCS(L'ACKEYACC-1),CRDNUM+1                                  
         OC    VENNUM,VENNUM                                                    
         BZ    CHK82                                                            
         TM    COMPSTA3,X'08'                                                   
         BO    CHK82                                                            
         MVC   TRSDACCS(L'ACKEYACC-1),VENNUM+1                                  
         DROP  R1                                                               
*                                                                               
         USING DLPOSTD,R1                                                       
         USING EXCELD,R4                                                        
CHK82    LA    R1,WORK69                                                        
         LA    R4,EXCWORK                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT JOB                                    
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSDBAC,JOBNUM                                                  
         MVC   DLPSDBNM,JOBNAME                                                 
         MVC   DLPSCRAC,CRDNUM     CONTRA IS CREDIT ACCOUNT                     
         MVC   DLPSCRNM,CRDNAME                                                 
         OC    VENNUM,VENNUM       DO WE HAVE A VENDOR ?                        
         BZ    CHK84               NO, LEAVE CONTRA AS IS                       
         TM    COMPSTA2,X'02'      YES, IS CONTRA ALWAYS CREDIT ?               
         BO    CHK84               YES, LEAVE AS IS                             
         MVC   DLPSCRAC,VENNUM     NO, REPLACE WITH VENDOR                      
         MVC   DLPSCRNM,VENNAME                                                 
*                                                                               
CHK84    TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    CHK86               NO                                           
         MVC   DLPSCRAC,EXCSEAC    YES, USE EXPENSE ACCOUNT INSTEAD             
         MVC   DLPSCRNM,EXCSENM                                                 
         DROP  R4                                                               
*                                                                               
CHK86    LA    R2,4                LOOP COUNTER                                 
         MVC   AMTBLK(64),WRKC1    COPY WORKCODES                               
         LA    R3,AMTBLK           COMMISSIONABLE WORKCODES                     
         LA    R4,AMTBLK+32        NON-COMMISSIONABLE WORKCODES                 
*                                                                               
CHK87    CLC   0(2,R3),SPACES      COMMISSIONABLE WORKCODE ?                    
         BE    CHK88               NO                                           
*                                                                               
         CLI   ORDNOEL,X'00'       YES, DO WE HAVE AN ORDER # ?                 
         BE    *+14                NO                                           
         MVC   0(L'ORDNOEL,R8),ORDNOEL                                          
         LA    R8,L'ORDNOEL(R8)                                                 
*                                                                               
         CLI   OTHEREL,X'00'       YES, DO WE HAVE AN OTHER # ?                 
         BE    *+14                NO                                           
         MVC   0(L'OTHEREL,R8),OTHEREL                                          
         LA    R8,L'OTHEREL(R8)                                                 
*                                                                               
         CLI   MEMO4C,X'00'        DO WE HAVE A 4C ELEMENT ?                    
         BE    *+14                NO                                           
         MVC   0(L'MEMO4C,R8),MEMO4C                                            
         LA    R8,L'MEMO4C(R8)     MEMO OF VENDOR ACCOUNT                       
*                                                                               
         USING TRCASHD,R8                                                       
         CLI   MEMO50,X'00'        DO WE HAVE A 50 ELEMENT ?                    
         BE    *+20                NO                                           
         MVC   0(L'MEMO50,R8),MEMO50                                            
         ZAP   TRCSAMNT,2(6,R3)                                                 
         LA    R8,L'MEMO50(R8)     MEMO OF ACTUAL AMOUNT                        
         DROP  R8                                                               
*                                                                               
         TM    ACOPSTAT,ACOXJOB    TEST FOR X-JOB                               
         BZ    *+14                                                             
         MVC   0(L'TRSELEM,R8),TRSELEM  YES-INSERT TRANS. STATUS ELEM           
         LA    R8,L'TRSELEM(R8)                                                 
*                                                                               
         MVC   DLPSANAL,0(R3)      GET WORKCODE AND AMOUNT                      
         ZAP   DLPSAMNT,2(6,R3)                                                 
         MVI   DLPSTYPE,X'00'      SET THE TYPE                                 
*                                                                               
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    *+10                NO                                           
         ZAP   DLPSAMNT,=P'0'      YES, CLEAR AMOUNT                            
*                                                                               
         MVC   0(L'WORK69,R8),WORK69                                            
         LA    R8,L'WORK69(R8)                                                  
*                                                                               
CHK88    CLC   0(2,R4),SPACES      NON-COMMISSIONABLE ?                         
         BE    CHK90               NO, LOOP THROUGH                             
*                                                                               
         CLI   ORDNOEL,X'00'       YES, DO WE HAVE AN ORDER # ?                 
         BE    *+14                NO                                           
         MVC   0(L'ORDNOEL,R8),ORDNOEL                                          
         LA    R8,L'ORDNOEL(R8)                                                 
*                                                                               
         CLI   OTHEREL,X'00'       YES, DO WE HAVE AN OTHER # ?                 
         BE    *+14                NO                                           
         MVC   0(L'OTHEREL,R8),OTHEREL                                          
         LA    R8,L'OTHEREL(R8)                                                 
*                                                                               
         CLI   MEMO4C,X'00'        DO WE HAVE A 4C ELEMENT ?                    
         BE    *+14                NO                                           
         MVC   0(L'MEMO4C,R8),MEMO4C                                            
         LA    R8,L'MEMO4C(R8)     MEMO OF VENDOR ACCOUNT                       
*                                                                               
         USING TRCASHD,R8                                                       
         CLI   MEMO50,X'00'        DO WE HAVE A 50 ELEMENT ?                    
         BE    *+20                NO                                           
         MVC   0(L'MEMO50,R8),MEMO50                                            
         ZAP   TRCSAMNT,2(6,R4)                                                 
         LA    R8,L'MEMO50(R8)     MEMO OF ACTUAL AMOUNT                        
         DROP  R8                                                               
*                                                                               
         TM    ACOPSTAT,ACOXJOB    TEST FOR X-JOB                               
         BZ    *+14                                                             
         MVC   0(L'TRSELEM,R8),TRSELEM                                          
         LA    R8,L'TRSELEM(R8)                                                 
*                                                                               
         MVC   DLPSANAL,0(R4)      GET WORKCODE AND AMOUNT                      
         ZAP   DLPSAMNT,2(6,R4)                                                 
         OI    DLPSTYPE,X'40'      SET THE TYPE                                 
*                                                                               
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    *+10                NO                                           
         ZAP   DLPSAMNT,=P'0'      YES, CLEAR AMOUNT                            
*                                                                               
         MVC   0(L'WORK69,R8),WORK69                                            
         LA    R8,L'WORK69(R8)                                                  
*                                                                               
CHK90    LA    R3,8(R3)            NEXT COMMISSIONABLE                          
         LA    R4,8(R4)            NEXT NON-COMMISSIONABLE                      
         BCT   R2,CHK87                                                         
         DROP  R1                                                               
*                                                                               
CHK94    CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   CHK98                                                            
         CLI   GSTSW,C'Y'          TEST GST APPLICABLE                          
         BNE   CHK98               NO                                           
*                                                                               
         L     R2,AIOA                                                          
         ZICM  RF,0(R2),2                                                       
         LTR   RF,RF               NOTHING?                                     
         BZ    CHK98                                                            
         SH    RF,=H'2'            SUBTRACT LENGTH                              
         LA    RE,2(R2)            COPY POSTINGS                                
         LR    R0,R8                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         LR    R8,R0                                                            
*                                                                               
         USING ACOTHERD,R8                                                      
CHK98    CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   *+14                                                             
         B     *+14                                                             
         CLC   =C'SG',CRDNUM+1     QST POSTING?                                 
         BE    CHK107                                                           
         MVC   ACOTEL(2),=X'230F'  BUILD 'OTHERS' ELEMENT FOR                   
         MVC   ACOTNUM(13),SPACES  PRODUCT AND JOB                              
         L     RE,APROH                                                         
         SR    R3,R3                                                            
         IC    R3,5(RE)                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ACOTNUM(0),8(RE)                                                 
*                                                                               
         L     RE,AJOBH                                                         
         SR    R3,R3                                                            
         IC    R3,5(RE)                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ACOTNUM+6(0),8(RE)                                               
         IC    R3,ACOTLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         USING TRSDESCD,R8                                                      
         MVI   TRSDEL,TRSDELQ                                                   
         MVC   TRSDLEN,=YL1(L'ACKEYACC-1+2)                                     
         MVC   TRSDACCS(L'ACKEYACC-1),JOBNUM+1                                  
         IC    R3,TRSDLEN                                                       
         AR    R8,R3                                                            
         DROP  R8                                                               
*                                                                               
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    CHK99               NO                                           
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
CHK99    CLC   CRDNUM+1(2),=C'SR'  CREDIT TO SR                                 
         BNE   CHK102              BECOMES A MINUS DEBIT                        
         USING ACMTD,R8                                                         
         XC    0(ACMTLNQ,R8),0(R8)                                              
         MVI   ACMTEL,ACMTELQ      ADD MEDIA TRANSFER FOR TO SR POSTING         
         MVI   ACMTLEN,ACMTLNQ                                                  
         MVI   ACMTSYS,C'J'                                                     
         MVC   ACMTMED,JOBNUM+9      MEDIA CODE                                 
         MVC   ACMTCLI(12),JOBNUM+3    CLI/PRD/JOB                              
         MVC   ACMTMOS,PMOS                                                     
         MVC   ACMTDSCP,JOBNAME                                                 
         OC    ACMTDSCP,SPACES                                                  
         ZAP   DUB,CSHTOT                                                       
         MP    DUB,=P'-1'                                                       
         CVB   R0,DUB                                                           
         STCM  R0,15,ACMTGRS         BILLING (GROSS)                            
         STCM  R0,15,ACMTRECV        AND RECEIVABLE                             
         STCM  R0,15,ACMTNET         PAYABLE                                    
         SR    R3,R3                                                            
         IC    R3,ACMTLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         USING DLPOSTD,R8                                                       
         TM    ACOPSTAT,ACOXJOB                                                 
         BZ    *+14                                                             
         MVC   DLPOSTD(L'TRSELEM),TRSELEM                                       
         LA    R8,L'TRSELEM(R8)                                                 
*                                                                               
         MVC   DLPSEL(2),=X'6971'  MINUS DEBIT TO SR                            
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSDBAC,CRDNUM                                                  
         MVC   DLPSDBNM,CRDNAME                                                 
         MVC   DLPSCRAC,JOBNUM     CLI/PRD/JOB SR                               
         MVC   DLPSCRNM,JOBNAME                                                 
         MVC   DLPSCRAC(3),SPACES  NO C/U/L IN CONTRA                           
         L     R2,ASRCH                                                         
         CLI   5(R2),0             SOURCE INPUT?                                
         BE    CHK100                                                           
         OC    8(L'CHUSRC,R2),SPACES                                            
         MVC   DLPSCRAC,SPACES                                                  
         MVC   DLPSCRNM,SPACES                                                  
         MVC   DLPSCRAC+3(12),8(R2)  CONTRA IS SOURCE                           
         MVC   DLPSCRNM+3(12),8(R2)                                             
*                                                                               
CHK100   MVC   DLPSANAL,SPACES                                                  
         MVC   DLPSANAL,OFFICE                                                  
         ZAP   DLPSAMNT,CSHTOT                                                  
         MP    DLPSAMNT,=P'-1'                                                  
         SR    R3,R3                                                            
         IC    R3,DLPSLEN                                                       
         B     CHK108                                                           
*                                                                               
         USING DLPOSTD,R8                                                       
CHK102   TM    ACOPSTAT,ACOXJOB                                                 
         BZ    *+14                                                             
         MVC   0(L'TRSELEM,R8),TRSELEM                                          
         LA    R8,L'TRSELEM(R8)                                                 
*                                                                               
         MVI   DLPSEL,DLPSECRQ     CREDIT SUPPLIER                              
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSDBAC,PRONUM     CLI/PROD FOR CONTRA                          
         MVC   DLPSDBNM,PRONAME                                                 
         MVC   DLPSCRAC,CRDNUM                                                  
         MVC   DLPSCRNM,CRDNAME                                                 
*                                                                               
         CLC   CRDNUM+1(2),=C'SA'  OR SOMETHING ELSE FOR ADVANCES               
         BNE   CHK106                                                           
         OC    SAVNUM,SAVNUM                                                    
         BZ    CHK106                                                           
         MVC   DLPSDBAC,SAVNUM                                                  
         MVC   DLPSDBNM,SAVNAME                                                 
*                                                                               
CHK106   MVC   DLPSANAL,SPACES                                                  
         MVC   DLPSANAL,COFFICE                                                 
         ZAP   DLPSAMNT,CSHTOT                                                  
         SR    R3,R3                                                            
         IC    R3,DLPSLEN                                                       
         B     CHK108                                                           
         EJECT                                                                  
*---------------------------------------------------------------                
*        BUILD CREDIT POSTING FOR QST                                           
*---------------------------------------------------------------                
         USING TRCASHD,R8                                                       
CHK107   MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'N'                                                    
         ZAP   TRCSAMNT,QSTBAS                                                  
         ZIC   R3,TRCSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         USING DLPOSTD,R8                                                       
         TM    ACOPSTAT,ACOXJOB                                                 
         BZ    *+14                                                             
         MVC   0(L'TRSELEM,R8),TRSELEM                                          
         LA    R8,L'TRSELEM(R8)                                                 
*                                                                               
         MVI   DLPSEL,DLPSECRQ     CREDIT SUPPLIER                              
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSDBAC,PRONUM     CLI/PROD FOR CONTRA                          
         MVC   DLPSDBNM,PRONAME                                                 
         MVC   DLPSCRAC,CRDNUM                                                  
         MVC   DLPSCRNM,CRDNAME                                                 
         MVC   DLPSANAL,OFFICE                                                  
         ZAP   DLPSAMNT,CSHTOT                                                  
         IC    R3,DLPSLEN                                                       
         EJECT                                                                  
*---------------------------------------------------------------                
*        BUILD SUBSIDIARY ELEMENTS - IF NECESSARY                               
*---------------------------------------------------------------                
*                                                                               
CHK108   CLI   VENDOR,C'Y'        POSTING TO '2C'                               
         BNE   CHK110                                                           
         LR    R6,R8                                                            
         AR    R8,R3                                                            
*                                                                               
         TM    ACOPSTAT,ACOXJOB                                                 
         BZ    *+14                                                             
         MVC   0(L'TRSELEM,R8),TRSELEM                                          
         LA    R8,L'TRSELEM(R8)                                                 
*                                                                               
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R6)                                                    
         LA    R3,1(R3)                                                         
         MVI   DLPSEL,X'68'                                                     
         OI    DLPSTYPE,X'80'                                                   
         MVC   DLPSDBAC,V2CNUM                                                  
         MVC   DLPSDBNM,V2CNAM                                                  
         MVC   DLPSCRAC,V27NUM                                                  
         MVC   DLPSCRNM,V27NAME                                                 
         ZAP   DLPSAMNT,CSHTOT                                                  
         MVC   DLPSANAL,COFFICE    NO, USE CREDIT OFFICE                        
*                                                                               
CHK110   SR    R3,R3                                                            
         IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    CHK112              NO                                           
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
         MVC   EXCSECAC,CRDNUM     PASS CREDIT NUMBER                           
         MVC   EXCSECAN,CRDNAME    PASS CREDIT NAME                             
         MVC   EXCCLNT,CLINUM+3    CLIENT                                       
         MVC   EXCPROD,PRONUM+6    PRODUCT                                      
         OC    VENNUM,VENNUM       DO WE HAVE A VENDOR NUMBER ?                 
         BZ    CHK111              NO, LEAVE IT AS IS                           
         TM    COMPSTA3,X'08'      YES, IS CONTRA ALWAYS CREDIT ?               
         BO    CHK111              YES, LEAVE IT AS IS                          
         MVC   EXCSECAC,VENNUM     NO, PASS VENDOR NUMBER                       
         MVC   EXCSECAN,VENNAME    PASS VENDOR NAME                             
*                                                                               
CHK111   ST    R9,EXCAGWS          A(GLOBAL WORKING STORAGE)                    
         MVC   EXCFINO,OFFICE      FINANCIAL OFFICE                             
         CLI   GSTSW,C'Y'                                                       
         BE    CHK111A                                                          
         ZAP   EXCAMNT,CSHTOT      USE CSHTOT IF NOT GST                        
         B     *+10                                                             
CHK111A  ZAP   EXCAMNT,TOTNET      AMOUNT                                       
         ST    R8,EXCADAY                                                       
         GOTO1 VEXCEL,EXCELD                                                    
         BNE   CURSIT                                                           
         L     R8,EXCADAY          GET UPDATED ACCDAY POINTER                   
         DROP  R3,R4                                                            
*                                                                               
CHK112   MVI   0(R8),0                                                          
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
*---------------------------------------------------------------                
*        ADD ENTRY TO TWA1                                                      
*---------------------------------------------------------------                
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
         ZAP   LCSHTOT,COMAMNT1                                                 
         AP    LCSHTOT,COMAMNT2                                                 
         AP    LCSHTOT,COMAMNT3                                                 
         AP    LCSHTOT,COMAMNT4                                                 
         BAS   RE,ADSCRINF                                                      
*                                                                               
         L     R2,ATOT                                                          
         MVC   0(10,R2),=C'ITEM TOTAL'                                          
         LA    R2,11(R2)                                                        
         EDIT  (P6,CSHTOT),(13,(R2)),2,MINUS=YES                                
         GOTO1 SQUASHER,DMCB,ATOT,25                                            
         L     R2,ATOTH                                                         
         OI    6(R2),X'80'                                                      
         CLI   MODE,1                                                           
         BNE   CHK114                                                           
         ZAP   TOTCASH,CSHTOT                                                   
*                                                                               
         LA    R3,7                                                             
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,,(R9),AMTBLK                                           
         CLI   ERRNUM,X'FE'                                                     
         BL    EXIT                                                             
         BE    *+8                                                              
*                                                                               
CHK114   L     R2,AORDH                                                         
         MVI   ERRNUM,X'FF'                                                     
         OI    MODE,X'80'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------                
*        SUB-ROUTINE TO EDIT TAX FIELDS AND DISPLAY DATA                        
*        ON EXIT, CC=EQ IF OK, NEQ IF ERROR AND R2 SET TO ERR FLD               
*---------------------------------------------------------------                
*                                                                               
EDTAX    NTR1  ,                                                                
         MVI   GSTSW,C'N'                                                       
         MVI   ERRNUM,OK                                                        
         L     R2,ATYPEH                                                        
         CLI   8(R2),C'*'          GST/PST NOT APPLICABLE                       
         BE    EDTAXX                                                           
*                                                                               
         L     R2,AGORNH           POINT TO GROSS OR NET                        
         MVI   ERRNUM,1            MISSING INPUT                                
         CLI   5(R2),0                                                          
         BE    EDTAXX                                                           
*                                                                               
         MVC   GORN,8(R2)          EXTRACT GROSS OR NET VALUE                   
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
         ZAP   CSHTOT,TOTGRS       YES-RESET VENDOR AMT=GROSS                   
EDTAXX   CLI   ERRNUM,OK                                                        
XITR2    XIT1  REGS=(R2)                                                        
CTAXSCRN DC    CL35'** PLEASE RUN CANADIAN TAX SCREEN'                          
         EJECT                                                                  
*---------------------------------------------------------------                
*        VALIDATE 'SA' ACCOUNT                                                  
*---------------------------------------------------------------                
*                                                                               
CHECKSA  NTR1                                                                   
         MVC   KEY+1(14),SPACES    READ RECORD                                  
         MVC   KEY+1(2),CRDNUM+1                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CHECKNO                                                          
*                                                                               
         LA    RF,IOAREA                                                        
         SR    R1,R1                                                            
*                                                                               
CHECK02  CLI   0(RF),0             FIND LEDGER ELEMENT                          
         BE    CHECKNO                                                          
         CLI   0(RF),ACLTELQ                                                    
         BE    CHECK04                                                          
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     CHECK02                                                          
*                                                                               
         USING ACLEDGD,RF                                                       
CHECK04  CLI   ACLTLIKE,C'R'       IS THIS LIKE A RECEIVABLE ?                  
         BNE   CHECKYS             NO, OKAY                                     
         MVC   KEY(15),CRDNUM      YES, KEEP READING                            
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
CHECK06  BAS   RE,SEQ                                                           
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CHECKNO                                                          
         LA    RF,IOAREA                                                        
         CLI   0(RF),X'43'         IS THIS A SUBACCOUNT ?                       
         BNE   CHECK08             NO                                           
         USING TRSUBHD,RF                                                       
         MVC   SAVNUM,TRSBACNT     YES, SAVE NUMBER AND NAME                    
         MVC   SAVNAME,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         EX    R1,*+8                                                           
         B     CHECK06                                                          
         MVC   SAVNAME(0),TRSBNAME                                              
*                                                                               
CHECK08  CLI   0(RF),TRNSELQ       IS THIS A TRANSACTION ?                      
         BNE   CHECK06             NO, KEEP LOOKING                             
         USING TRANSD,RF                                                        
         TM    TRNSSTAT,X'80'      YES, IS IT A DEBIT ?                         
         BZ    CHECK06             NO, KEEP LOOKING                             
         CLC   SAVEDATE,TRNSDATE   YES, DOES DATE MATCH ?                       
         BNE   CHECK06             NO                                           
         L     RE,ADOCH            YES, LOOK FOR MATCH REFERENCE                
         SR    R1,R1                                                            
         IC    R1,DOCLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,CHECKDOC                                                      
         BE    CHECKYS                                                          
         B     CHECK06                                                          
*                                                                               
CHECKDOC CLC   TRNSREF(0),8(RE)    EXECUTED                                     
CHECKNO  MVI   ERRNUM,53                                                        
CHECKYS  B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
         ST    R3,SAVER3                                                        
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
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
MOVEFLDX L     R3,SAVER3                                                        
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------                
* ADSCRINF - TRY TO SAVE 2ND SCREEN'S INFO                                      
*---------------------------------------------------------------                
ADSCRINF NTR1  ,                                                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    RF,XTRAELM                                                       
         GOTO1 AADACDAY,BOPARM,(X'80',IOAREA),BOPL61,BOWORK1,(RF)               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERRXIT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------                
*        ACBATCODE                                                              
*---------------------------------------------------------------                
*                                                                               
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*---------------------------------------------------------------                
*        LITERAL DECLARATIONS                                                   
*---------------------------------------------------------------                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* US SCREEN TABLE  ( BYTES 0-3=DISP TO FIELD, BYTES 4-7=DISP TO ADCON )         
*                                                                               
USTAB    DS    0D                                                               
         DC    AL4(CHUORDH-TWAD),AL4(AORDH-PROGD)                               
         DC    AL4(CHUOHSTH-TWAD),AL4(AOHSTH-PROGD)                             
         DC    AL4(CHUDOCH-TWAD),AL4(ADOCH-PROGD)                               
         DC    AL4(CHUDATH-TWAD),AL4(ADATH-PROGD)                               
         DC    AL4(CHUCLIH-TWAD),AL4(ACLIH-PROGD)                               
         DC    AL4(CHUCLINH-TWAD),AL4(ACLINH-PROGD)                             
         DC    AL4(CHUPROH-TWAD),AL4(APROH-PROGD)                               
         DC    AL4(CHUPRONH-TWAD),AL4(APRONH-PROGD)                             
         DC    AL4(CHUJOBH-TWAD),AL4(AJOBH-PROGD)                               
         DC    AL4(CHUJOBNH-TWAD),AL4(AJOBNH-PROGD)                             
         DC    AL4(CHUSUPH-TWAD),AL4(ASUPH-PROGD)                               
         DC    AL4(CHUSUPNH-TWAD),AL4(ASUPNH-PROGD)                             
         DC    AL4(CHUWRKH-TWAD),AL4(AWRKH-PROGD)                               
         DC    AL4(CHUWCNMH-TWAD),AL4(AWCNMH-PROGD)                             
         DC    AL4(CHUAMTH-TWAD),AL4(AAMTH-PROGD)                               
         DC    AL4(CHUCOFH-TWAD),AL4(ACOFH-PROGD)                               
         DC    AL4(CHUCOFNH-TWAD),AL4(ACOFNH-PROGD)                             
         DC    AL4(CHUNCWKH-TWAD),AL4(ANCWKH-PROGD)                             
         DC    AL4(CHUWNNMH-TWAD),AL4(AWNNMH-PROGD)                             
         DC    AL4(CHUNCAH-TWAD),AL4(ANCAH-PROGD)                               
         DC    AL4(CHUAOFH-TWAD),AL4(AAOFH-PROGD)                               
         DC    AL4(CHUDEPH-TWAD),AL4(ADEPH-PROGD)                               
         DC    AL4(CHUSTFH-TWAD),AL4(ASTFH-PROGD)                               
         DC    AL4(CHUSTFNH-TWAD),AL4(ASTFNH-PROGD)                             
         DC    AL4(CHUEXPH-TWAD),AL4(AEXPH-PROGD)                               
         DC    AL4(CHUEXPNH-TWAD),AL4(AEXPNH-PROGD)                             
         DC    AL4(CHUFOFH-TWAD),AL4(AFOFH-PROGD)                               
         DC    AL4(CHUFOFNH-TWAD),AL4(AFOFNH-PROGD)                             
         DC    AL4(CHUCRDH-TWAD),AL4(ACRDH-PROGD)                               
         DC    AL4(CHUCRDNH-TWAD),AL4(ACRDNH-PROGD)                             
         DC    AL4(CHUCRDN-TWAD),AL4(ACRDN-PROGD)                               
         DC    AL4(CHUSRCH-TWAD),AL4(ASRCH-PROGD)                               
         DC    AL4(CHUNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    AL4(CHUTOTH-TWAD),AL4(ATOTH-PROGD)                               
         DC    AL4(CHUTOT-TWAD),AL4(ATOT-PROGD)                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* CA SCREEN TABLE  ( BYTES 0-3=DISP TO FIELD, BYTES 4-7=DISP TO ADCON )         
*                                                                               
CANTAB   DS    0D                                                               
         DC    AL4(CHCORDH-TWAD),AL4(AORDH-PROGD)                               
         DC    AL4(CHCOHSTH-TWAD),AL4(AOHSTH-PROGD)                             
         DC    AL4(CHCDOCH-TWAD),AL4(ADOCH-PROGD)                               
         DC    AL4(CHCDATH-TWAD),AL4(ADATH-PROGD)                               
         DC    AL4(CHCCLIH-TWAD),AL4(ACLIH-PROGD)                               
         DC    AL4(CHCCLINH-TWAD),AL4(ACLINH-PROGD)                             
         DC    AL4(CHCPROH-TWAD),AL4(APROH-PROGD)                               
         DC    AL4(CHCPRONH-TWAD),AL4(APRONH-PROGD)                             
         DC    AL4(CHCJOBH-TWAD),AL4(AJOBH-PROGD)                               
         DC    AL4(CHCJOBNH-TWAD),AL4(AJOBNH-PROGD)                             
         DC    AL4(CHCSUPH-TWAD),AL4(ASUPH-PROGD)                               
         DC    AL4(CHCSUPNH-TWAD),AL4(ASUPNH-PROGD)                             
         DC    AL4(CHCWRKH-TWAD),AL4(AWRKH-PROGD)                               
         DC    AL4(CHCWCNMH-TWAD),AL4(AWCNMH-PROGD)                             
         DC    AL4(CHCAMTH-TWAD),AL4(AAMTH-PROGD)                               
         DC    AL4(CHCCOFH-TWAD),AL4(ACOFH-PROGD)                               
         DC    AL4(CHCCOFNH-TWAD),AL4(ACOFNH-PROGD)                             
         DC    AL4(CHCNCWKH-TWAD),AL4(ANCWKH-PROGD)                             
         DC    AL4(CHCWNNMH-TWAD),AL4(AWNNMH-PROGD)                             
         DC    AL4(CHCNCAH-TWAD),AL4(ANCAH-PROGD)                               
         DC    AL4(CHCAOFH-TWAD),AL4(AAOFH-PROGD)                               
         DC    AL4(CHCDEPH-TWAD),AL4(ADEPH-PROGD)                               
         DC    AL4(CHCSTFH-TWAD),AL4(ASTFH-PROGD)                               
         DC    AL4(CHCSTFNH-TWAD),AL4(ASTFNH-PROGD)                             
         DC    AL4(CHCEXPH-TWAD),AL4(AEXPH-PROGD)                               
         DC    AL4(CHCEXPNH-TWAD),AL4(AEXPNH-PROGD)                             
         DC    AL4(CHCFOFH-TWAD),AL4(AFOFH-PROGD)                               
         DC    AL4(CHCFOFNH-TWAD),AL4(AFOFNH-PROGD)                             
         DC    AL4(CHCCRDH-TWAD),AL4(ACRDH-PROGD)                               
         DC    AL4(CHCCRDNH-TWAD),AL4(ACRDNH-PROGD)                             
         DC    AL4(CHCCRDN-TWAD),AL4(ACRDN-PROGD)                               
         DC    AL4(CHCSRCH-TWAD),AL4(ASRCH-PROGD)                               
         DC    AL4(CHCGTYPH-TWAD),AL4(ATYPEH-PROGD)                             
         DC    AL4(CHCGTYNH-TWAD),AL4(ATYPNH-PROGD)                             
         DC    AL4(CHCGORNH-TWAD),AL4(AGORNH-PROGD)                             
         DC    AL4(CHCGSTXH-TWAD),AL4(AGSTXH-PROGD)                             
         DC    AL4(CHCGAMTH-TWAD),AL4(AGAMTH-PROGD)                             
         DC    AL4(CHCPROVH-TWAD),AL4(APROVH-PROGD)                             
         DC    AL4(CHCNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    AL4(CHCTOTH-TWAD),AL4(ATOTH-PROGD)                               
         DC    AL4(CHCTOT-TWAD),AL4(ATOT-PROGD)                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*---------------------------------------------------------------                
*        ROUTINE TO VALIDATE UP TO 4 WORK CODES AND 4 AMOUNTS                   
*---------------------------------------------------------------                
*                                                                               
*        R3=A(WORK CODE FIELD HDR),R4=A(OUTPUT VALUES)                          
*                                                                               
VALWAM   NTR1  BASE=*,LABEL=*                                                   
         USING VALD,R3                                                          
         NI    MULTWCSW,X'00'                                                   
         MVC   VALWNM,SPACES       CLEAR & TRANSMIT NAME                        
         OI    VALWNMH+6,X'80'                                                  
         MVC   0(2,R4),SPACES      CLEAR OUTPUT VALUES                          
         ZAP   2(6,R4),=P'0'                                                    
         MVC   8(2,R4),SPACES                                                   
         ZAP   10(6,R4),=P'0'                                                   
         MVC   16(2,R4),SPACES                                                  
         ZAP   18(6,R4),=P'0'                                                   
         MVC   24(2,R4),SPACES                                                  
         ZAP   26(6,R4),=P'0'                                                   
*                                                                               
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
*                                  VALIDATE WORK CODES                          
         LA    R2,VALWRKH                                                       
         GOTO1 SCANNER,DMCB,(R2),WORK                                           
         MVI   ERRNUM,19                                                        
         CLI   4(R1),0                                                          
         BE    VALWERR                                                          
*                                                                               
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
*                                                                               
VALW08   CLC   0(2,R8),0(R1)                                                    
         BE    VALWERR                                                          
         LA    R1,2(R1)                                                         
         BCT   R0,VALW08                                                        
         BR    RE                                                               
*                                                                               
VALW10   MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),JOBNUM                                                  
         MVC   KEY+4(2),0(R8)                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'ACCOUNT',KEY,KEY                  
         MVI   ERRNUM,0                                                         
         CLI   8(R1),0             CHECK FOR ERRORS                             
         BNE   VALWERR                                                          
         LA    R1,IOAREA                                                        
         SR    RE,RE                                                            
*                                  FIND NAME ELEMENT                            
VALW12   CLI   0(R1),0                                                          
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
         MVC   0(11,R6),ACANDESC                                                
         GOTO1 RIGHT,DMCB,(R6),11                                               
*                                                                               
VALW14   LA    R5,32(R5)           BUMP TO NEXT                                 
         LA    R6,11(R6)                                                        
         OI    MULTWCSW,FSTWC                                                   
         LA    R8,8(R8)                                                         
         SR    R1,R1                                                            
         IC    R1,THISLINE                                                      
         LA    R1,1(R1)                                                         
         STC   R1,THISLINE                                                      
         B     VALW04                                                           
*                                                                               
VALW16   ST    R6,SAVER6                                                        
         NI    MULTWCSW,X'FF'-FSTWC                                             
         ZIC   R6,SAVWLINE                                                      
         LTR   R6,R6                                                            
         BNP   VALW18                                                           
         MVC   TSTWCNMS,SVWCNMS                                                 
         GOTO1 SQUASHER,DMCB,TSTWCNMS,49    ELIMINATE EXTRA SPACES              
*                                                                               
VALW18   MVC   VALWNM(49),TSTWCNMS                                              
         L     R6,SAVER6                                                        
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
         BNE   VALWERR                                                          
         MVI   THISLINE,1                                                       
         LA    R5,WORK             R5=A(SCAN BLOCK ENTRY)                       
         LR    R8,R4                                                            
         MVI   FVINDX,1                                                         
*                                                                               
VALW20   CLC   THISLINE,SAVALINE                                                
         BH    VALWOK                                                           
         MVI   ERRNUM,2                                                         
         CLI   1(R5),0                                                          
         BNE   VALWERR                                                          
         SR    R0,R0                                                            
         IC    R0,0(R5)            CHECK FOR VALID CASH FILED                   
         GOTO1 AMTVAL,DMCB,12(R5),(R0)                                          
         MVI   ERRNUM,25                                                        
         CLI   0(R1),0                                                          
         BNE   VALWERR                                                          
         L     R1,4(R1)                                                         
         LA    R1,0(R1)                                                         
         ZAP   2(6,R8),0(8,R1)                                                  
         AP    CSHTOT,0(8,R1)                                                   
*                                                                               
         LA    R4,JOBNUM           RE-READ THE JOB                              
         GOTO1 ASETJOB,DMCB,(X'80',(R4)),(R8)                                   
         GOTO1 AOPTVAL                                                          
         BNE   VALWERR             ERROR                                        
*                                                                               
         CLI   CHECK,C' '          CHECKING AMOUNT ?                            
         BE    VALW22              NO                                           
*                                                                               
         MVI   ERRNUM,30                                                        
         GOTO1 AWRKVAL,DMCB,(R8)                                                
         BH    VALWERR                                                          
*                                                                               
VALW22   LA    R5,32(R5)                                                        
         LA    R8,8(R8)                                                         
         SR    R1,R1                                                            
         IC    R1,THISLINE                                                      
         LA    R1,1(R1)                                                         
         STC   R1,THISLINE                                                      
         SR    R1,R1                                                            
         IC    R1,FVINDX                                                        
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         B     VALW20                                                           
*                                                                               
VALWOK   MVI   ERRNUM,X'FF'        SET CC=EQ IF ALL OK                          
VALWERR  CLI   ERRNUM,X'FF'                                                     
         XIT1  REGS=(R2)                                                        
         DROP  R1,R3                                                            
         EJECT                                                                  
*---------------------------------------------------------------                
*        CLEAR UNUSED X-JOB FIELDS                                              
*---------------------------------------------------------------                
*                                                                               
CLEARX   DS    0D                                                               
         NMOD1 0,**CLRX**                                                       
         L     RC,0(R1)                                                         
         USING PROGD,RC                                                         
         USING TWAD,RA                                                          
         USING GWS,R9                                                           
         MVC   FLD,SPACES                                                       
*                                                                               
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
         XMOD1                                                                  
         EJECT                                                                  
*---------------------------------------------------------------                
*        SET UP BLOCK FOR SALES/USE TAX                                         
*---------------------------------------------------------------                
*                                                                               
TAXMOD   DS    0D                                                               
         NMOD1 0,**TAX**                                                        
         L     RC,0(R1)                                                         
         USING PROGD,RC                                                         
         USING TWAD,RA                                                          
         USING GWS,R9                                                           
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
         SR    R1,R1                                                            
         IC    R1,5(R2)              CLIENT CODE                                
         LTR   R1,R1                                                            
         BZ    TAX15                  IF NO CLIENT                              
         BCTR  R1,0                   SKIP PRODUCT AND JOB                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STXACC+3(0),8(R2)                                                
*                                                                               
         SR    R4,R4                                                            
         IC    R4,CLILNGTH          LEVEL A LENGTH                              
         LA    R3,STXACC+3(R4)                                                  
         L     R2,APROH                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,5(R2)              PRODUCT                                    
         LTR   R1,R1                                                            
         BZ    TAX15                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)                                                    
*                                                                               
         SR    R4,R4                                                            
         IC    R4,PRDLNGTH          LEVEL B LENGTH                              
         LA    R3,STXACC+3(R4)                                                  
         L     R2,AJOBH                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,5(R2)              JOB                                        
         LTR   R1,R1                                                            
         BZ    TAX15                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)                                                    
*                                                                               
TAX15    L     R2,ADOCH                                                         
         SR    R1,R1                                                            
         IC    R1,5(R2)              REFERENCE(DOCUMENT)                        
         LTR   R1,R1                                                            
         BZ    TAX20                                                            
         BCTR  R1,0                                                             
         CH    R1,=AL2(L'STXREF-1)   NOT MORE THAN 6                            
         BH    *-6                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STXREF(0),8(R2)                                                  
*                                                                               
TAX20    L     R2,AORDH                                                         
         SR    R1,R1                                                            
         IC    R1,5(R2)              ORDER NUMBER                               
         LTR   R1,R1                                                            
         BZ    TAX25                                                            
         BCTR  R1,0                                                             
         CH    R1,=AL2(L'STXORD-1)   NOT MORE THAN 6                            
         BH    *-6                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STXORD(0),8(R2)                                                  
*                                                                               
TAX25    L     R2,ADATH                                                         
         SR    R1,R1                                                            
         IC    R1,5(R2)              DATE                                       
         LTR   R1,R1                                                            
         BZ    TAX30                                                            
         BCTR  R1,0                                                             
         CH    R1,=AL2(L'STXDTE-1)   NOT MORE THAN 6                            
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
TAX40    MVC   STXAMT,LCSHTOT       CASH AMOUNT                                 
         GOTO1 ANARRSCN,DMCB,ANARH,STXNARR                                      
*                                                                               
TAX50    LA    R3,X'40'                                                         
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,STXDATA,(R9)    GO TO TAX OVERLAY                      
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
*---------------------------------------------------------------                
*        MORE LITERAL DECLARATIONS                                              
*---------------------------------------------------------------                
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
         CLI   CTXMODE,C'Z'                                                     
         BE    *+8                                                              
         MVI   CTAXBEF,C'Y'                                                     
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
         OI    4(R2),X'80'                                                      
         L     R2,ATYPEH                                                        
         MVC   8(1,R2),CTXGSTT                                                  
         OI    6(R2),X'80'                                                      
         L     R2,ATYPNH                                                        
         MVC   8(21,R2),CTXGSTTN                                                
         OI    6(R2),X'80'                                                      
         L     R2,AGAMTH                                                        
         MVC   5(1,R2),CTXLGSTA                                                 
         MVC   8(40,R2),CTXGSTA                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AGSTXH                                                        
         XC    8(L'CHCGSTX,R2),8(R2)                                            
         CLI   GSTSW,C'Y'                                                       
         BNE   CTAX69                                                           
         OC    CTXGST,CTXGST       DO WE HAVE GST?                              
         BZ    CTAX70                                                           
         MVC   8(4,R2),=C'GST='                                                 
         EDIT  CTXGST,(14,12(R2)),2,ZERO=NOBLANK,MINUS=YES,ALIGN=LEFT           
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
*--------------------------------------------------------------------           
* EXTRAELM - EXTRACTS THE SPECIAL 2ND SCREEN FROM ITEM RECORD                   
*--------------------------------------------------------------------           
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
*---------------------------------------------------------------                
*        MORE LITERAL DECLARATIONS                                              
*---------------------------------------------------------------                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------                
*        LOCAL WORKING STORAGE                                                  
*---------------------------------------------------------------                
*                                                                               
PROGD    DSECT                                                                  
RELO1    DS    A                                                                
SAVERE   DS    A                                                                
SAVER3   DS    A                                                                
SAVER6   DS    A                                                                
RIGHT    DS    V                                                                
*                                                                               
*  SCREEN DIRECTORY                                                             
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
AWRKH    DS    A                   A(WORK CODE HEADER)                          
AWCNMH   DS    A                   A(WORK CODE NAMES HEADER)                    
AAMTH    DS    A                   A(AMOUNTS HEADER)                            
ACOFH    DS    A                   A(CREDIT OFFICE HEADER)                      
ACOFNH   DS    A                   A(CREDIT OFFICE NAME HEADER)                 
ANCWKH   DS    A                   A(NON-COMM WORKCODES HEADER)                 
AWNNMH   DS    A                   A(NON-COMM WORKCODE NAMES HEADER)            
ANCAH    DS    A                   A(AMOUNTS HEADER)                            
AAOFH    DS    A                   A(ANALYSIS OFFICE HEADER)                    
ADEPH    DS    A                   A(DEPARTMENT HEADER)                         
ASTFH    DS    A                   A(STAFF HEADER)                              
ASTFNH   DS    A                   A(STAFF NAME HEADER)                         
AEXPH    DS    A                   A(EXPENSE HEADER)                            
AEXPNH   DS    A                   A(EXPENSE NAME HEADER)                       
AFOFH    DS    A                   A(FINANCIAL OFFICE HEADER)                   
AFOFNH   DS    A                   A(FINANCIAL OFFICE NAME HEADER)              
ACRDH    DS    A                   A(CASH HEADER)                               
ACRDNH   DS    A                   A(CASH NAME HEADER)                          
ACRDN    DS    A                   A(CASH NAME FIELD)                           
ASRCH    DS    A                   A(SOURCE HEADER)                             
ATYPEH   DS    A                                                                
ATYPNH   DS    A                                                                
AGORNH   DS    A                   A(GROSS OR NET HEADER)                       
AGSTXH   DS    A                                                                
AGAMTH   DS    A                                                                
APROVH   DS    A                   A(PROVINCE CODE)                             
ANARH    DS    A                   A(NARRATIVE HEADER)                          
ATOTH    DS    A                   A(TOTAL HEADER)                              
ATOT     DS    A                   A(TOTAL FIELD)                               
*                                                                               
CRDNUM   DS    CL15                CREDIT ACCOUNT NUMBER                        
CRDNAME  DS    CL36                CREDIT ACCOUNT NAME                          
*                                                                               
VENNUM   DS    CL15                VENDOR NUMBER                                
VENNAME  DS    CL36                VENDOR NAME                                  
*                                                                               
CLINUM   DS    CL15                CLIENT NUMBER                                
CLINAME  DS    CL36                CLIENT NAME                                  
*                                                                               
PRONUM   DS    CL15                PRODUCT NUMBER                               
PRONAME  DS    CL36                PRODUCT NAME                                 
*                                                                               
JOBNUM   DS    CL15                JOB NUMBER                                   
JOBNAME  DS    CL36                JOB NAME                                     
*                                                                               
SAVNUM   DS    CL15                CONTRA ACCOUNT NUMBER OF SA POSTING          
SAVNAME  DS    CL36                CONTRA ACCOUNT NAME OF SA POSTING            
*                                                                               
V2CNUM   DS    CL15                2C ACCOUNT NUMBER                            
V2CNAM   DS    CL36                2C ACCOUNT NAME                              
*                                                                               
V27NUM   DS    CL15                27 ACCOUNT NUMBER                            
V27NAME  DS    CL36                27 ACCOUNT NAME                              
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
NWCS     EQU   (*-WRKC1)/8         N'WORKCODE AMOUNTS                           
*                                                                               
CSHTOT   DS    PL6                                                              
SAVWLINE DS    X                                                                
SAVALINE DS    X                                                                
THISLINE DS    X                                                                
DOCLEN   DS    CL1                                                              
DOCSAVE  DS    CL6                                                              
REFSAVE  DS    CL6                                                              
SVSTAT   DS    CL1                                                              
VENDOR   DS    CL1                 VENDOR REQUIRED INDICATOR                    
VENDTYPE DS    CL1                 VENDOR'S GST TYPE                            
VENDPSTT DS    CL1                 VENDOR'S PST TYPE                            
VENDPROV DS    CL2                 VENDOR PROVINCE, VNDR REC (LFM)              
*                                                                               
TOTNET   DS    PL6                                                              
TOTGRS   DS    PL6                                                              
TOTGST   DS    PL6                                                              
TOTPST   DS    PL6                                                              
QSTBAS   DS    PL6                                                              
GORN     DS    CL1                 G=GROSS, N=NET                               
NAMTS    DS    XL1                 N'AMOUNT ENTRIES                             
NGST     DS    XL1                 N'GST AMOUNTS INPUT                          
AMTTAB   DS    (NWCS)XL(AMTLNQ)                                                 
ALARGEST DS    A                                                                
*                                                                               
SVWCNMS  DS    CL((4*(L'ACANDESC+2))-2)                                         
TSTWCNMS DS    CL((4*(L'ACANDESC+2))-2)                                         
MULTWCSW DS    X                                                                
FSTWC    EQU   X'80'                                                            
*                                                                               
ORDNOEL  DS    XL10                                                             
OTHEREL  DS    XL(ACOTLNQ1)                                                     
MEMO50   DS    XL(TRCSLNQ1)                                                     
MEMO4C   DS    XL(L'ACKEYACC-1+2)                                               
TRSELEM  DS    XL(TRSLNQ)                                                       
WORK69   DS    XL(DLPSLNQ)                                                      
*                                                                               
LPRO     DS    XL1                                                              
AUSTAB   DS    A                                                                
ACANTAB  DS    A                                                                
*                                                                               
EXCWORK  DS    0D                                                               
         DS    CL(EXCELNQ)                                                      
*                                                                               
       ++INCLUDE ACBATSTAX                                                      
       ++INCLUDE ACBATCTAX                                                      
*                                                                               
TMPMODE  DS    CL1                                                              
KEY      DS    CL49                                                             
IOAREA   DS    3000C                                                            
PROGDX   DS    0C                                                               
         EJECT                                                                  
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
AMTPST   DS    PL6                 PST                                          
AMTLNQ   EQU   *-AMTD                                                           
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
         EJECT                                                                  
*---------------------------------------------------------------                
*        ACBATFCD                                                               
*---------------------------------------------------------------                
*                                                                               
       ++INCLUDE ACBATFCD                                                       
         EJECT                                                                  
         ORG   OSVALS                                                           
* TAX OVERLAY (ACBAT40) USERS OSVALS+200                                        
*                                                                               
USERL    EQU   OSVALSL-(OSVALSL-200)                                            
USERAREA DS    0F                                                               
CLCDPASS DS    CL1                 OPTION BY CLIENT TO KEEP CD                  
LCSHTOT  DS    PL6                                                              
JOBSTAT  DS    X                   SAVE AREA FOR ACOPSTAT                       
AMTBLK   DS    CL64                                                             
GSTSW    DS    CL1                 FLAG FOR GST/PST APPLICABLE                  
CTAXBEF  DS    CL1                 FLAG FOR USED CTAX                           
KEEPPROV DS    CL2                 SAVE PROVINCE                                
TMPNET   DS    PL6                                                              
TMPGRS   DS    PL6                                                              
TMPGST   DS    PL6                                                              
TMPPST   DS    PL6                                                              
XTRAELM  DS    CL71                                                             
CLEAROK  DS    CL1                 FLAG FOR CLEAR GST/PST FIELD                 
USERPROV DS    CL1                 FLAG, USER ENTERED PROVINCE                  
USERPST  DS    CL1                 FLAG, USER ENTERED PST TYPE                  
         DS    CL(USERL-(*-USERAREA))  SPARE                                    
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATC2D - CANADIAN SCREEN DSECT                                       
*--------------------------------------------------------------                 
*                                                                               
         ORG   CONTABH                                                          
       ++INCLUDE ACBATC2D                                                       
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
**PAN#1  DC    CL21'012ACBAT03   12/10/14'                                      
         END                                                                    
