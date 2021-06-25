*          DATA SET ACBAT15    AT LEVEL 082 AS OF 02/14/03                      
*PHASE T61B15A                                                                  
         TITLE 'INVOICE/CHECK/PETTY CASH (NON-CHARGEABLE)-T61B15'               
*                                                                               
*        BATCH TYPES 21,22,23 AND 24                                            
*                                                                               
T61B15   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,*BAT15*,R7,CLEAR=YES,RR=R2                          
         USING PROGD,RC                                                         
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING TWAD,RA                                                          
         ST    R2,PRELO                                                         
*                                                                               
* BUILD SCREEN DIRECTORY                                                        
*                                                                               
* CANADIAN SCREEN DIFFERS WITH US SCREEN BELOW INVOICE AMOUNT FIELD             
*                                                                               
         L     R1,=A(USTAB)                                                     
         CLI   AGYCTRY,CTRYUSA     TEST FOR USA                                 
         BE    *+8                                                              
         L     R1,=A(CANTAB)       NO-ITS CANADIAN                              
         AR    R1,R2                                                            
*                                                                               
INV1     CLI   0(R1),X'FF'         TEST FOR EOT                                 
         BE    DAT01                                                            
         LM    RE,RF,0(R1)                                                      
         LA    RE,TWAD(RE)         FORM READ ADDRESS OF SCREEN FIELD            
         LA    RF,PROGD(RF)        FORM ADDRESS OF ADCON                        
         ST    RE,0(RF)                                                         
         LA    R1,L'USTAB(R1)                                                   
         B     INV1                                                             
*                                                                               
DAT01    GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         MVC   TMPMODE,MODE                                                     
         NI    TMPMODE,X'0F'                                                    
         CLI   TMPMODE,1           PROCESSING OTHER SCREENS?                    
         BH    DAT03               DON'T CHECK DATE                             
         LA    R2,PICDATH          VALIDATE THE DATE                            
         MVC   FVMSGNO,=Y(AE$INDAT)                                             
         CLI   INPUT,24                                                         
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         CLI   PICDATH+5,0                                                      
         BNE   DAT02                                                            
         BAS   RE,GETODAY                                                       
         B     DAT03                                                            
*                                                                               
DAT02    GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         CLC   WORK(2),=C'60'      NOT BEFORE 1960                              
         BL    ERROR                                                            
*                                                                               
DAT03    GOTO1 DATCON,DMCB,(0,WORK),(1,SAVEDATE)                                
         GOTO1 DATECHK,DMCB,SAVEDATE                                            
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         EJECT                                                                  
         CLI   CSACT,ACTCHA                                                     
         BNE   *+16                                                             
         MVI   CLEAROK,C'N'        DON'T CLEAR GST/PST FOR CHANGE               
         MVI   USERPROV,C'Y'       USER ENTERED PROV FROM CHANGE                
         MVI   USERPST,C'Y'        USER ENTERED PST TYPE FOR CHANGE             
*                                                                               
INV2     CLI   AGYCTRY,CTRYCAN                                                  
         BNE   INV3X                                                            
         CLI   MODE,2              CANNOT CLEAR WHEN IN ANOTHER                 
         BE    INV3X               PROGRAM                                      
         CLI   MODE,3                                                           
         BE    INV3X                                                            
         CLI   CLEAROK,C'N'        NOT OK TO CLEAR?                             
         BE    INV3G                                                            
         L     R2,AGORNH                                                        
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BO    INV3C               ONLY CLEAR THE OLD ONES                      
         OI    6(R2),X'80'                                                      
         MVI   8(R2),0                                                          
         L     R2,AGSTXH                                                        
         MVC   8(L'PITGSTX,R2),SPACES        CLEAR INFO DATA TOO                
         OI    6(R2),X'80'                                                      
         L     R2,ATYPNH                                                        
         MVC   8(L'PITTYPN,R2),SPACES                                           
         OI    6(R2),X'80'                                                      
*                                                                               
INV3C    L     R2,ATYPEH                                                        
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
         XC    8(L'PITGAMT,R2),8(R2)                                            
         XC    XTRAELM,XTRAELM     CLEAR PST FIELDS                             
*                                                                               
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6STRO        STEREO?                          
         BZ    INV3E                                                            
         L     R2,APROVH                                                        
         TM    4(R2),X'80'         NEW PROVINCE?                                
         BO    INV3J                                                            
         MVI   USERPROV,C'N'                                                    
         B     INV3K                                                            
*                                                                               
INV3E    LA    R2,PITCRTH          SUPPLIER                                     
         TM    4(R2),X'80'                                                      
         BNO   INV3G                                                            
         XC    VENDPROV,VENDPROV                                                
         L     R2,APROVH                                                        
         TM    4(R2),X'80'                                                      
         BO    INV3G                                                            
         XC    8(2,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
INV3G    L     R2,APROVH                                                        
         TM    4(R2),X'80'         NEW PROVINCE?                                
         BNO   INV3X               SAVE PROVINCE                                
INV3J    MVI   USERPROV,C'Y'       USER ENTERED PROVINCE                        
INV3K    MVC   KEEPPROV,8(R2)                                                   
         XC    XTRAELM,XTRAELM                                                  
*                                                                               
INV3X    MVI   CLEAROK,C'N'        DON'T CLEAR UNTIL DONE                       
         L     R2,PRELO                                                         
*                                                                               
*--------------------------------------------------------------------           
*        MODE=X'00'  ORDER OVERLAY                                              
*        MODE=X'01'  EXIT WITH ORDER DISPLAYED                                  
*        MODE=X'03'  CANADIAN SALES TAX SCREEN                                  
*--------------------------------------------------------------------           
INV5     CLI   CSACT,ACTCHA        ITEM CHANGE?                                 
         BNE   INV10                                                            
         CLI   PFKEY,X'FF'         FIRST TIME, IT SETS PFKEY TO FF              
         BNE   *+8                 YES                                          
         MVI   PFKEY,0                                                          
INV10    CLI   MODE,3              CANDIAN TAX?                                 
         BE    INV20                                                            
*                                                                               
         CLI   PFKEY,7             USE PF=7 TO LOAD                             
         BNE   INV15                                                            
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    INV11                                                            
         MVC   FVMSGNO,=Y(AE$CSNA)  CANADIAN SCREEN N/A                         
         LA    R2,CONACTH                                                       
         B     ERROR                                                            
*                                                                               
INV11    ZAP   NTAMNT,=P'0'                                                     
         ZAP   GRAMNT,=P'0'                                                     
         ZAP   GST,=P'0'                                                        
         ZAP   PST,=P'0'                                                        
         B     INV50                                                            
*                                                                               
INV15    CLI   PFKEY,0                                                          
         BE    INV50                                                            
         MVI   ERRNUM,251          INVALID PFKEY                                
         L     R2,TIACURS                                                       
         B     ERROR                                                            
*                                                                               
INV20    CLI   AGYCTRY,CTRYCAN     CANADIAN?                                    
         BNE   INV50                                                            
         L     RF,=A(CTAXMOD)      DO CANADIAN TAX SCREEN                       
         L     R2,PRELO                                                         
         AR    RF,R2                                                            
         MVI   CSSPROG,2                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   CSACT,ACTCHA        ARE WE DOING A CHANGE?                       
         BNE   CURSIT              NO, LEAVE                                    
         CLC   FVMSGNO,=AL2(FVFOK) IF ERROR, RETURN                             
         BNE   CURSIT                                                           
         MVC   FVMSGNO,=X'FFFF'    TELL BT61 WE HAVE TO COME BACK               
         B     CURSIT                                                           
*                                                                               
INV50    MVC   FLD,SPACES                                                       
         L     R2,ACLINH                                                        
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         L     R2,APRONH                                                        
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AFDPNH                                                        
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         L     R2,APDPNH                                                        
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         L     R2,ASTFNH                                                        
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         L     R2,AVNDNH                                                        
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
*                                                                               
         CLI   PICORDH+5,0         IS THERE AN ORDER                            
         BE    DOC01                                                            
*                                                                               
         TM    PICORDH+4,X'80'                                                  
         BO    *+12                                                             
         MVI   MODE,1                                                           
         B     DOC01                                                            
         CLI   MODE,0              MODE=0 GO TO ORDER OVERLAY                   
         BE    ORD02                                                            
         CLC   ORDNO,PICORD        SAME ORDER NO - CARRY ON                     
         BE    DOC01                                                            
         MVI   MODE,0              ELSE REREAD                                  
*                                                                               
ORD02    LA    R3,22               T61B16                                       
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,,(R9)                                                  
         LA    R2,PICDOCH                                                       
         CLI   MODE,1              EXIT WITH ORDER DISPLAYED                    
         BNE   *+12                                                             
         NI    PICORDH+6,X'BF'     SET OFF CURSOR                               
         B     EXIT                                                             
         CLI   ERRNUM,OK           NO ERROR AND NO ORDER - CONTINUE             
         BE    CURSIT                                                           
         OI    PICORDH+6,X'01'     MODIFY FOR ERROR                             
         B     CURSIT                                                           
*                                                                               
DOC01    MVI   ANLSW,0             INITIALIZE ANALYSIS FLAG                     
         MVI   SRSW,0                                                           
         MVI   V29SW,C'N'          TO CONTROL CONTRA-ACC ON 29                  
         ZAP   CDAMNT,=P'0'                                                     
         ZAP   DISC,=P'0'                                                       
*        MVC   SRSAVE,SPACES                                                    
*                                                                               
         LA    R2,PICDOCH          DOCUMENT NUMBER                              
         BAS   RE,ANY                                                           
         MVC   SAVDOC,PICDOC                                                    
         OC    SAVDOC,SPACES                                                    
         CLI   INPUT,24                                                         
         BNE   DBT01                                                            
         CLC   PICDOC(4),=C'NEXT'                                               
         BE    DBT01                                                            
         MVC   FVMSGNO,=Y(AE$INVIF)                                             
         CLI   PICDOCH+5,6         FOR TYPE 24 INPUT MUST BE                    
         BNE   ERROR               6-LONG AND NUMERIC                           
         TM    PICDOCH+4,X'08'                                                  
         BZ    ERROR                                                            
         MVC   LASTDOC,SAVDOC      SAVE LAST DOCUMENT NUMBER                    
         EJECT                                                                  
*              VALIDATE THE DEBIT ACCOUNT                                       
*                                                                               
DBT01    SR    R6,R6               NO MORE PROFILES                             
         LA    R2,PICDBTH                                                       
         BAS   RE,ANY                                                           
         MVC   FVMSGNO,=Y(AE$INVIF)                                             
         GOTO1 SCANNER,DMCB,(R2),(2,WORK)                                       
         CLI   DMCB+4,2            SCAN ACCOUNT,CC=NN                           
         BH    ERROR                                                            
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         CLI   WORK+1,0                                                         
         BNE   ERROR               ACCOUNT HAS NO RIGHT SIDE                    
         SR    R3,R3                                                            
         IC    R3,WORK                                                          
         STC   R3,DBTLEN           SAVE LENGTH OF DEBIT ACCOUNT                 
         CLI   DMCB+4,1                                                         
         BE    DBT05               ONLY ACCOUNT INPUT                           
         LA    R1,WORK+32          SECOND HALF IS CC=                           
         CLC   12(2,R1),=C'CC'                                                  
         BNE   ERROR                                                            
         CLI   0(R1),2                                                          
         BE    DBT03               NO NUMBER AFTER CC                           
         CLI   0(R1),3                                                          
         BNE   ERROR               MORE THAN CCN                                
         CLI   14(R1),C'1'                                                      
         BL    ERROR                                                            
         CLI   14(R1),C'9'                                                      
         BH    ERROR                                                            
         SR    R0,R0                                                            
         IC    R0,14(R1)                                                        
         SLL   R0,28                                                            
         SRL   R0,28                                                            
         STC   R0,OVPOS                                                         
*                                                                               
DBT03    SR    R3,R3                                                            
         IC    R3,1(R1)                                                         
         LTR   R3,R3               NO RIGHT SIDE AFTER CC=                      
         BZ    ERROR                                                            
         CH    R3,=H'3'                                                         
         BH    ERROR               TOO MANY CHARACTERS                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   OVCODE(0),22(R1)    SAVE OVERRIDE CODE                           
         OC    OVCODE,SPACES                                                    
*                                                                               
DBT05    MVC   FVMSGNO,=Y(AE$INACC)                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SE'     DEFAULT FOR EXPENSE ACCOUNT                  
         LA    RF,OPTLDBT          OPTIONAL LEDGERS FOR EXPENSE                 
         LA    R4,PICDBT           R4=TO ACCOUNT CODE                           
         SR    R3,R3                                                            
         IC    R3,DBTLEN           LENGTH OF DEBIT ACCOUNT                      
         BCTR  R3,0                                                             
         LA    RE,KEY+3                                                         
         CLI   0(R4),C'*'          OVERRIDE U/L                                 
         BNE   DBT09                                                            
         LA    R4,1(R4)                                                         
         SH    R3,=H'1'            ADJUST LENGTH FOR *                          
         BM    ERROR               MISSING ACCOUNT CODE                         
         LA    RE,KEY+1            RE=UNIT POSITION IN KEY                      
*                                                                               
DBT07    SR    R1,R1                                                            
         CLI   1(RF),C' '          ANY LEDGER?                                  
         BE    *+8                                                              
         LA    R1,1                LEDGER IN TABLE                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(RF)       MATCH INPUT CODE TO TABLE                    
         BE    DBT09                                                            
         LA    RF,2(RF)                                                         
         CLI   0(RF),X'FF'         END OF TABLE - INVALID ACCOUNT               
         BE    ERROR                                                            
         B     DBT07                                                            
*                                                                               
DBT09    EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)       CODE TO KEY                                  
         CLC   KEY+1(2),=C'SR'     SR IS GOOD FOR 21 22 23 ONLY                 
         BNE   DBT15                                                            
         CLI   INPUT,24                                                         
         BE    ERROR                                                            
         OI    SRSW,SRDR           DEBIT TO SR                                  
*                                                                               
DBT15    LA    R2,PICDBTH          GET THE DEBIT ACCOUNT                        
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   DBTNAM,ACCTNAME                                                  
         MVC   DBTNUM,ACCTNUM                                                   
         MVC   PICDBTN,ACCTNAME                                                 
         OI    PICDBTNH+6,X'80'                                                 
         MVC   COSTANAL,SPACES                                                  
         MVC   COSTANAL(1),ACCTCOST   SAVE COST CODE                            
         TM    ACCTSTAT,X'40'      GENERATE PERS EXPENSE                        
         BZ    *+8                 FOR THIS EXPENSE ACCOUNT                     
         OI    ANLSW,ANLSTF                                                     
         TM    ACCTSTAT,X'08'      DEPARTMENT                                   
         BZ    DBT21                                                            
         OI    ANLSW,ANLDPT                                                     
         MVC   KEY+3(12),SPACES                                                 
         BAS   RE,READ             GET SE LEDGER                                
         L     R4,AIOAREA1                                                      
         MVI   ELCODE,ACLTELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACLEDGD,R4                                                       
DBT19    CLI   ACLTLEN,X'20'                                                    
         BL    DBT21               OLD ELEMENT                                  
         CLI   ACLTDPOS,0                                                       
         BE    DBT21                                                            
         SR    R1,R1                                                            
         IC    R1,ACLTDLEN         LENGTH OF DEPT                               
         BCTR  R1,0                                                             
         LA    R3,DBTNUM+2                                                      
         SR    R0,R0                                                            
         IC    R0,ACLTDPOS                                                      
         AR    R3,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FINDPT(0),0(R3)     GET DEPT CODE FROM DEBIT ACCOUNT             
         OC    FINDPT,SPACES       GET EXPENSE DEPT                             
*                                                                               
DBT21    OC    CPJNUM,CPJNUM       NOTHING IN COSTING A/C.SKIP OV'RIDE          
         BZ    CRT01                                                            
         CLC   DBTNUM+1(2),=C'SE'  OVERRIDES ON SE A/C ONLY                     
         BNE   CRT01                                                            
         OC    OVPOS(4),OVPOS                                                   
         BZ    DBT23               NO OVERRIDE ON DEBIT LINE                    
         CLI   OVPOS,0                                                          
         BE    *+20                                                             
         LA    R3,CPJNUM+2                                                      
         SR    R0,R0                                                            
         IC    R0,OVPOS                                                         
         AR    R3,R0                                                            
         B     *+8                                                              
         LA    R3,CPJNUM+7                                                      
         LA    R1,OVCODE                                                        
         B     DBT29                                                            
*                                                                               
DBT23    MVC   KEY(15),DBTNUM      GET EXPENSE ACCOUNT, AGAIN                   
         BAS   RE,GETACC                                                        
         L     R4,AIOAREA1                                                      
         MVI   ELCODE,ACSTELQ      GET STATUS ELEMENT                           
         BAS   RE,GETEL                                                         
         BNE   CRT01                                                            
*                                                                               
         USING ACSTATD,R4                                                       
         OC    ACSTCNTR,SPACES                                                  
         CLI   ACSTCPOS,0          IS THERE AN OVERRIDE                         
         BE    *+20                                                             
         LA    R3,CPJNUM+2         YES.                                         
         SR    R0,R0                                                            
         IC    R0,ACSTCPOS         GET IT                                       
         AR    R3,R0               POINT TO STARTING POSITION                   
         B     *+8                                                              
         LA    R3,CPJNUM+7         NO OVERRIDE                                  
         LA    R1,ACSTCNTR         OVERRIDE CHARACTERS                          
*                                                                               
DBT29    LA    RE,3                3 POS'NS MAX CAN BE OVERRIDDEN               
         CLI   0(R1),C' '          CANNOT OVERRIDE WITH A SPACE                 
         BE    *+10                                                             
         MVC   0(1,R3),0(R1)       MOVE IN OVERRIDE CHARACTERS                  
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   RE,*-22                                                          
*                                                                               
         MVC   KEY(15),CPJNUM      VERIFY THAT ACCOUNT EXISTS                   
         BAS   RE,GETACC                                                        
*        MVC   CPJRNAME,ACCTNAME                                                
*                                                                               
         EJECT                                                                  
*              VALIDATE CREDIT ACCOUNT                                          
*                                                                               
CRT01    SR    R6,R6               NO PROFILES                                  
         LA    R2,PICCRTH          SUPPLIER/ BANK A/C / PETTY CASH              
         BAS   RE,ANY                                                           
         LA    R5,COMPEL                                                        
         USING ACCOMPD,R5                                                       
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVI   KEY+1,C'S'                                                       
         MVC   KEY+2(1),ACMPSUPX   DEFAULT FOR EXPENSE VENDOR                   
         LA    RF,OPTLCRT          OPTIONAL LEDGERS FOR TYPE 21                 
         CLI   INPUT,21                                                         
         BE    *+14                                                             
         MVC   KEY+1(2),ACMPBANK   DEFAULT BANK ACCOUNT                         
         LA    RF,OPTLBANK         OPTIONAL LEDGERS FOR 22, 23,24               
         MVC   FVMSGNO,=Y(AE$INACC)                                             
         LA    R4,PICCRT           R4=TO ACCOUNT CODE                           
         SR    R3,R3                                                            
         IC    R3,PICCRTH+5        R3=LENGTH OF INPUT                           
         BCTR  R3,R0                                                            
         LA    RE,KEY+3                                                         
         CLI   0(R4),C'*'          OVERRIDE U/L                                 
         BNE   CRT05                                                            
         LA    R4,1(R4)                                                         
         SH    R3,=H'1'            ADJUST LENGTH FOR *                          
         BM    ERROR               MISSING ACCOUNT CODE                         
         LA    RE,KEY+1            RE=UNIT POSITION IN KEY                      
         CLI   INPUT,21                                                         
         BNE   CRT03                                                            
         CLC   0(2,R4),ACMPSUPP    PROD. SUPPLIER IS ALTERNATIVE                
         BE    CRT05                                                            
*                                                                               
CRT03    SR    R1,R1                                                            
         CLI   1(RF),C' '          ANY LEDGER?                                  
         BE    *+8                                                              
         LA    R1,1                LEDGER IN TABLE (COMPARE 2 BYTES)            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(RF)       MATCH INPUT CODE TO TABLE                    
         BE    CRT05                                                            
         LA    RF,2(RF)                                                         
         CLI   0(RF),X'FF'         END OF TABLE - INVALID ACCOUNT               
         BE    ERROR                                                            
         B     CRT03                                                            
*                                                                               
CRT05    EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)       CODE TO KEY                                  
         MVI   FVOMTYP,C'E'                                                     
         CLC   KEY+1(2),=C'SR'     *SR(BILLING SOURCE)                          
         BNE   CRT09                                                            
         MVC   FVMSGNO,=Y(AE$INVIF)                                             
         TM    SRSW,SRDR           DEBIT IS AN SR ACCOUNT                       
         BO    ERROR                                                            
         OI    SRSW,SRCR           CREDITING SR                                 
*                                                                               
CRT09    BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   SVSTAT,ACCTSTAT     SAVE ACCOUNT STATUS                          
         MVC   CRTNUM,ACCTNUM                                                   
         MVC   CRTNAM,ACCTNAME                                                  
         MVC   PICCRTN,CRTNAM                                                   
         OI    PICCRTNH+6,X'80'                                                 
         CLI   AGYCTRY,CTRYCAN     TEST FOR CANADA                              
         BNE   CRT12               NO                                           
*                                                                               
         MVI   ELCODE,ITCELQ       LOOK FOR INPUT TAX TYPE DEFAULT              
         L     R4,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         USING ITCELD,R4                                                        
*                                                                               
CRT10    BNE   CRT12                                                            
         CLC   SAVEDATE,ITCEFFD    TEST TRANS DATE VS. EFFECTIVE                
         BL    CRT11                                                            
         OC    ITCPROV,ITCPROV     NO PROV=GST                                  
         BZ    CRT10G                                                           
         MVC   VENDPSTT,ITCTYPE    SAVE THE PST TYPE                            
         MVC   VENDPROV,ITCPROV                                                 
         B     CRT11                                                            
CRT10G   MVC   VENDTYPE,ITCTYPE                                                 
*                                                                               
CRT11    BAS   RE,NEXTEL                                                        
         B     CRT10                                                            
         DROP  R4                                                               
*                                                                               
CRT12    CLI   USERPROV,C'Y'       USER ENTERED PROVINCE?                       
         BE    CRT12X              YES, USE THAT INSTEAD                        
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   CRT12X                                                           
         L     R2,APROVH                                                        
         MVI   5(R2),2                                                          
         OI    6(R2),X'80'                                                      
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6STRO        STEREO?                          
         BZ    CRT12M                                                           
         CLC   VENDPROV,SPACES                                                  
         BNH   CRT12X                                                           
CRT12M   MVC   8(2,R2),VENDPROV                                                 
*                                                                               
CRT12X   CLI   INPUT,21                                                         
         BNE   CRT19                                                            
         CLI   PICCDH+5,0          CD OPTION - DEFAULT IS YES                   
         BE    CRT13                                                            
         CLI   PICCD,C'Y'                                                       
         BE    CRT13                                                            
         CLI   PICCD,C'N'          OPTION NOT TO TAKE CD                        
         BE    CRT19                                                            
         MVC   FVMSGNO,=Y(AE$INVIF)                                             
         LA    R2,PICCDH                                                        
         B     ERROR                                                            
*                                                                               
CRT13    MVI   ELCODE,X'38'                                                     
         L     R4,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         BNE   CRT19                                                            
*                                                                               
         USING ACVATD,R4                                                        
         SR    R1,R1                                                            
         ICM   R1,3,ACVTRATE       CASH DISCOUNT                                
         CVD   R1,DUB                                                           
         ZAP   DISC,DUB                                                         
         TM    COMPSTAT,X'08'      POST C.D. TO INCOME?                         
         BNO   CRT19               NO, GIVE IT TO THE CLIENT                    
         MVC   CDACC,SPACES                                                     
         MVC   CDACC(1),COMPANY                                                 
         MVC   CDACC+1(4),=C'SIMD' MISCELLANEOUS CD                             
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),CRTNUM       VENDOR LEDGER                                
         BAS   RE,READ             GET C.D. ACCOUNT FROM LEDGER                 
         L     R4,AIOAREA1                                                      
         MVI   ELCODE,ACLTELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACLEDGD,R4                                                       
         CLI   ACLTLEN,X'20'                                                    
         BL    CRT17               OLD ELEMENT                                  
         CLI   ACLTCDAC,X'40'                                                   
         BNH   CRT17               NO C.D. SPECIFIED (USE SIMD)                 
         MVC   CDACC+1(14),ACLTCDAC                                             
*                                                                               
CRT17    MVC   KEY,SPACES                                                       
         MVC   KEY(L'CDACC),CDACC                                               
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CDNAME,ACCTNAME                                                  
*                                                                               
*                                                                               
CRT19    LA    RF,ADVLDG           CHECK ADVANCE LEDGERS                        
CRT21    CLI   0(RF),X'FF'                                                      
         BE    AMT01               NOT AN ADVANCE LEDGER                        
         CLC   CRTNUM+1(2),0(RF)                                                
         BE    *+12                                                             
         LA    RF,2(RF)                                                         
         B     CRT21                                                            
*                                                                               
         LA    R2,PICDOCH          FIND MATCHING ITEM IN ADVANCE LEDG.          
         MVI   ERRNUM,OK                                                        
         BAS   RE,CHECKSA                                                       
         CLI   ERRNUM,OK                                                        
         BNE   ERROR                                                            
*                                                                               
AMT01    LA    R2,PICAMTH          VALIDATE AMOUNT                              
         BAS   RE,ANY                                                           
         MVC   FVMSGNO,=Y(AE$INAMT)                                             
         SR    R3,R3                                                            
         IC    R3,PICAMTH+5                                                     
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R3)                                  
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         ZAP   DUB,4(8,R1)                                                      
         ZAP   GRAMNT,DUB                                                       
         ZAP   NTAMNT,DUB                                                       
         CP    DISC,=P'0'          NO CASH DISCOUNT                             
         BE    AMT03                                                            
         ZAP   DUB,NTAMNT                                                       
         MP    DUB,DISC                                                         
         SRP   DUB,64-4,5                                                       
         ZAP   CDAMNT,DUB                                                       
         EDIT  CDAMNT,(10,PICDAMT),2,ALIGN=LEFT,MINUS=YES,COMMAS=YES            
         MVC   PICCDCD,=C'CASH DISCOUNT'                                        
*                                                                               
AMT03    CLI   INPUT,24                                                         
         BNE   VGRNT                                                            
         CP    GRAMNT,=P'0'                                                     
         BNH   ERROR                                                            
         EJECT                                                                  
*--------------------------------------------------------------------*          
* VALIDATE GROSS / NET FIELD IF SCRIPT AND CANADA                    *          
*--------------------------------------------------------------------*          
VGRNT    L     RF,BCAUTL           TEST SCRIPT EXECUTION                        
         TM    TSTAT6-UTLD(RF),TST6SCRP                                         
         BZ    CLN01                                                            
         CLI   AGYCTRY,CTRYCAN     TEST FOR CANADA                              
         BNE   CLN01                                                            
         L     R2,AGORNH                                                        
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=Y(AE$MISIF)                                             
         B     ERROR                                                            
*                                                                               
         CLI   8(R2),C'G'                                                       
         BE    CLN01                                                            
         CLI   8(R2),C'N'                                                       
         BE    CLN01                                                            
         MVC   FVMSGNO,=Y(AE$GRNET)                                             
         B     ERROR                                                            
         EJECT                                                                  
*              VALIDATE THE CLIENT CODE                                         
*                                                                               
CLN01    L     R2,ACLIH                                                         
*        TM    SRSW,SRCR           SR CREDITS                                   
*        BZ    *+8                                                              
*        BAS   RE,ANY              CLIENT IS REQUIRED INPUT                     
         CLI   5(R2),0                                                          
         BE    SRC01               NO CLIENT/PROD                               
         MVC   FVMSGNO,=Y(AE$INCLI)                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPJOB    U/L FOR CLI/PRO/                             
*                                                                               
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)                                                   
*                                                                               
         SR    R6,R6                                                            
         XC    CLIPROF,CLIPROF                                                  
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         L     R2,ACLINH                                                        
         BAS   RE,MOVEFLD                                                       
         MVC   CLINUM,ACCTNUM                                                   
         MVC   CLINAME,ACCTNAME                                                 
         EJECT                                                                  
*              VALIDATE THE PRODUCT CODE                                        
*                                                                               
PRD01    L     R2,APROH                                                         
*        TM    SRSW,SRCR           ANY SR POSTINGS                              
*        BZ    *+8                                                              
*        BAS   RE,ANY              CLIENT IS REQUIRED INPUT                     
         CLI   5(R2),0                                                          
         BNE   *+12                NO PRODUCT                                   
         L     R2,ACLIH            POINT BACK TO CLIENT                         
         B     PRD05                                                            
         MVC   FVMSGNO,=Y(AE$INPRO)                                             
         LA    R4,KEY+3                                                         
         SR    R3,R3                                                            
         IC    R3,PRODHEIR         LEVA LENGTH                                  
         AR    R4,R3               R4=PRODUCT CODE                              
         IC    R3,5(R2)                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),8(R2)                                                    
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         L     R2,APRONH                                                        
         BAS   RE,MOVEFLD                                                       
         MVC   PRONUM,ACCTNUM                                                   
         MVC   PRONAME,ACCTNAME                                                 
         L     R2,APROH            POINT BACK TO PRODUCT                        
*                                                                               
PRD05    BAS   RE,PROFMERG         MERGE PROFILES                               
         LA    R4,PROFILE                                                       
         USING ACPROFD,R4                                                       
         MVC   CPJNUM,ACPRCOST     COSTING CODE FROM PROFILE                    
*        MVC   CPJRNUM,ACPRRECV    RECEIVABLE CODE                              
         MVC   CLIOFC,ACPROFFC                                                  
         MVC   KEY(15),CPJNUM                                                   
         SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CPJNAME,ACCTNAME                                                 
*                                                                               
*        TM    SRSW,SRCR           POSTING TO RECEIVABLE                        
*        BZ    SRC01                                                            
*        MVC   KEY(15),CPJRNUM     RECEIVABLE ACCOUNT                           
*        BAS   RE,GETACC                                                        
*        MVC   CPJRNAME,ACCTNAME   NAME                                         
         EJECT                                                                  
*              SOURCE FOR SR POSTINGS                                           
*                                                                               
SRC01    L     R2,ASRCH            SOURCE FIELD                                 
         CLI   5(R2),0                                                          
         BNE   SRC03                                                            
         TM    SRSW,SRDR+SRCR                                                   
         BZ    DPTEXP01            SOURCE NOT REQUIRED                          
         BAS   RE,ANY              MISSING INPUT                                
*                                                                               
SRC03    MVC   FVMSGNO,=Y(AE$INVIF)                                             
         TM    SRSW,SRDR+SRCR                                                   
         BZ    ERROR               SOURCE NOT REQUIRED                          
*        MVC   SRSAVE(L'PICCRT-3),PICCRT+3   SAVE BILLING SOURCE                
*        OC    SRSAVE,SPACES                                                    
         EJECT                                                                  
*              DEPARTMENT EXPENSE ANALYSIS (1P)                                 
*                                                                               
DPTEXP01 GOTO1 =A(OFDPV),DMCB,(RC),RR=PRELO  VALIDATE OFF/DEPART                
         CLI   ERRNUM,OK                                                        
         BNE   ERROR                                                            
         TM    COMPSTAT,X'10'                                                   
         BNO   *+8                                                              
         OI    COSTSW,COSTACC                                                   
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST TEST NEW COSTING                   
         BNO   DPTEXP02                                                         
         MVI   COSTSW,0                                                         
         LA    R1,CATBLK                                                        
         USING CATD,R1                                                          
         MVC   CATDMGR,DATAMGR     BUILD CONTROL BLOCK                          
         MVC   CATSEAC,DBTNUM      DEBIT ACCOUNT                                
         MVC   CATOFF,ANLOFC       OFFICE                                       
         MVC   CATDPT,SPCDPT       DEPARTMENT                                   
         GOTO1 VCATCALL,CATD                                                    
         LA    R2,PICDBTH                                                       
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),CATACC3                                                  
         CLI   CATERR,0                                                         
         BE    DPTEXPA                                                          
         MVC   FVMSGNO,=AL2(AE$IANAL)                                           
         B     ERROR                                                            
*                                                                               
DPTEXPA  CLI   CATPST,C'N'         NO COST POSTING                              
         BE    DPTEXP02                                                         
         OI    COSTSW,COSTACC+COSTNEW                                           
         MVC   COSTANAL,CATCDE                                                  
         MVC   EXANLNUM,CATACC3    SAVE 13 ACCOUNT                              
*                                                                               
DPTEXP02 CLI   COSTANAL,X'40'                                                   
         BE    *+12                IF NO COSTING REQUIRED                       
         TM    COSTSW,COSTACC                                                   
         BO    DPTEXP07                                                         
         TM    SRSW,SRDR+SRCR      POSTING TO SR                                
         BNZ   DPT01                                                            
         TM    ANLSW,ANLSTF        OR STAFF IS NOT REQUIRED                     
         BO    DPT01                                                            
         L     RE,ACLIH                                                         
         CLI   5(RE),0             THEN CLIENT INPUT NOT ALLOWED                
         BE    DPTEXP03                                                         
         L     R2,ACLIH                                                         
         B     DPTEXP05                                                         
*                                                                               
DPTEXP03 L     R2,APROH            AND PRODUCT INPUT NOT ALLOWED                
         CLI   5(R2),0                                                          
         BE    DPT01                                                            
*                                                                               
DPTEXP05 MVC   FVMSGNO,=Y(AE$ANFAN)   ACCT NOT FLAGGED FOR ANALYSIS             
         B     ERROR                                                            
*                                                                               
DPTEXP07 OI    COSTSW,COSTACC      COST ACCOUNTING                              
         L     R2,ACLIH                                                         
         BAS   RE,ANY                                                           
         L     R2,APROH                                                         
         LA    RF,BCCPYEL          TEST PRODUCT REQUIRED FOR EXPENSE            
         TM    BCCPYST5-BCCPYEL(RF),CPYSEXPP EXPENSE PRODUCT                    
         BNO   *+8                                                              
         BAS   RE,ANY                                                           
         L     R2,AFDPTH                                                        
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),=C'1P'    1P/OF/DPT/CATEGORY                            
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST TEST NEW COSTING                   
         BNO   DPTEXP08                                                         
         MVC   KEY+3(12),=C'999999999999' SAYS VANESSA                          
         B     DPTEXP11                                                         
*                                                                               
DPTEXP08 LA    R1,KEY+3                                                         
         CLI   OFFSW,C'Y'                                                       
         BNE   DPTEXP09                                                         
         MVC   0(L'ANLOFC,R1),ANLOFC ANALYSIS OFFICE                            
         SR    RF,RF                                                            
         IC    RF,OFCLNGTH         LENGTH OF OFFICE                             
         AR    R1,RF                                                            
*                                                                               
DPTEXP09 LA    RF,=C'9999'         DEFAULT DEPT.                                
         TM    ANLSW,ANLDPT                                                     
         BZ    *+8                                                              
         LA    RF,SPCDPT           SPECIAL DEPT.                                
         SR    R3,R3                                                            
         IC    R3,DPTLNGTH         LENGTH OF DEPARTMENT                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RF)       DEPT CODE OR 999                             
         LA    R1,1(R3,R1)         R1=END OF KEY                                
         MVC   0(L'COSTANAL,R1),COSTANAL    EXPENSE CATEGORY                    
*                                                                               
DPTEXP11 BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CNEXPNUM,ACCTNUM                                                 
         MVC   CNEXPNAM,ACCTNAME                                                
         EJECT                                                                  
*              DEPARTMENT ANALYSIS (28 AND 2D)                                  
*                                                                               
DPT01    MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         TM    ANLSW,ANLDPT        DEPARTMENT ANALYSIS                          
         BNO   CLA01               NO - SKIP 28 2D CHECK                        
         MVC   KEY+1(2),=C'28'     HARD L FOR DEPT SUSPENSE                     
         LA    R2,PICDBTH                                                       
         MVC   KEY+3(12),DBTNUM+3  EXPENSE ACCOUNT                              
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=Y(AE$INACP)                                             
         MVC   DSEXPNUM,ACCTNUM                                                 
         MVC   DSEXPNAM,ACCTNAME                                                
         BAS   RE,CHECKACC                                                      
*                                                                               
         MVC   FVMSGNO,=Y(AE$INACC)                                             
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2D'    BUILD THE 2D KEY                              
         MVC   KEY+3(12),SPACES                                                 
         MVC   KEY+3(L'ANLOFC),ANLOFC                                           
         SR    R1,R1                                                            
         IC    R1,OFCLNGTH         LENGTH OF OFFICE                             
         LA    R1,KEY+3(R1)                                                     
         LA    RF,SPCDPT           SPECIAL DEPT OVERRIDES FINDPT FOR 2D         
         L     R2,APDPTH                                                        
         CLI   5(R2),0                                                          
         BNE   DPT04                                                            
         LA    RF,FINDPT           USE FINANCIAL DEPT FOR 2D                    
         L     R2,AFDPTH                                                        
         CLI   5(R2),0                                                          
         BE    *+10                NO FIN OR SPEC DPT=ERROR ON CHECKACC         
DPT04    MVC   0(L'FINDPT,R1),0(RF)                                             
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   DEPACNUM,ACCTNUM                                                 
         MVC   DEPACNAM,ACCTNAME                                                
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         L     R2,APDPTH           BUILD OFFICE/DEPT/ FOR PERSON                
         CLI   5(R2),0                                                          
         L     R2,APDPNH           POINT TO CORRECT DPT NAME FIELD              
         BNE   *+8                                                              
         L     R2,AFDPNH                                                        
         BAS   RE,MOVEFLD                                                       
         EJECT                                                                  
*              CLIENT ANALYSIS (29)                                             
*                                                                               
CLA01    L     R2,ACLIH                                                         
         MVC   KEY+3(12),SPACES                                                 
         TM    ANLSW,ANLSTF        NEED TO GENERATE PERS EXPENSE                
         BO    CLA05                                                            
*                                                                               
CLA03    L     RE,ASTFH                                                         
         CLI   5(RE),0             IF EXPSW=N, THEN                             
         BE    VND01               INPUT TO STAFF NOT ALLOWED                   
         L     R2,ASTFH                                                         
         MVC   FVMSGNO,=Y(AE$ANFST)                                             
         B     ERROR                                                            
*                                                                               
CLA05    MVC   KEY+1(14),SPACES    BUILD 29 POSTING                             
         MVC   KEY+1(2),=C'29'                                                  
         MVC   KEY+3(12),CPJNUM+3  USE COSTING CODE                             
         OC    CPJNUM,CPJNUM       IF PRESENT                                   
         BNZ   CLA11                                                            
         MVC   KEY+3(12),=12C'9'                                                
*                                                                               
CLA07    L     RE,APROH                                                         
         CLI   5(RE),0             IF NO COSTING ACCOUNT THEN                   
         BE    CLA11                                                            
         L     R2,APROH            INPUT TO PRODUCT NOT ALLOWED                 
         MVC   FVMSGNO,=Y(AE$INVIF)                                             
         B     ERROR                                                            
*                                                                               
CLA11    BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=Y(AE$INACP)                                             
         BAS   RE,CHECKACC                                                      
         MVC   PSCLINUM,ACCTNUM                                                 
         MVC   PSCLINAM,ACCTNAME                                                
*                                                                               
         EJECT                                                                  
*              STAFF ACCOUNTS (2P)                                              
*                                                                               
STF01    L     R2,ASTFH                                                         
         TM    ANLSW,ANLSTF                                                     
         BZ    VND01                                                            
         BAS   RE,ANY                                                           
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),SPACES      SPACE PAD INPUT FIELD                        
*                                                                               
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),=C'2P'     HARD LED. FOR PERS.                          
         BAS   RE,READ             READ LEDGER                                  
         L     R4,AIOAREA1                                                      
         MVI   ELCODE,ACHRELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACHEIRD,R4                                                       
         MVI   LEV2P,0                                                          
         LA    R3,3                2P SHOULD NOT HAVE 4 LEVELS                  
         LA    RE,ACHRLEVC         FIND THE LAST LEVEL                          
         CLI   0(RE),12                                                         
         BE    *+16                                                             
         SH    RE,=H'16'                                                        
         BCT   R3,*-12                                                          
         B     STF21               MORE THAN 3 LEVELS(CAUSE ERROR)              
*                                                                               
         CH    R3,=H'1'            FIRST LEVEL (LOW LEVEL)                      
         BE    STF19               2P HAS ONE LEVEL (STAFF)                     
         LR    R1,RE                                                            
         SH    R1,=H'16'           R1=SECOND TO LAST LEVEL                      
         MVC   LEV2P,0(R1)         SAVE DISPLACEMENT TO STAFF                   
*                                                                               
         LA    R1,KEY+3                                                         
         CH    R3,=H'2'            2 LEVELS DEPT/STAFF                          
         BE    STF17                                                            
         MVC   0(L'ANLOFC,R1),ANLOFC 3 LEVELS ARE OFFICE/DEPT/STAFF             
         SR    RE,RE                                                            
         IC    RE,OFCLNGTH                                                      
         AR    R1,RE               R1 TO DEPT LEVEL                             
*                                                                               
STF17    MVC   0(L'FINDPT,R1),FINDPT                                            
         L     RF,APDPTH           IS THERE AN OVERRIDE DPT?                    
         CLI   5(RF),0                                                          
         BE    STF19               NO  -CONTINUE                                
         L     R2,AFDPTH                                                        
         BAS   RE,GETACC           YES -GET 2P DEPT ACCOUNT/NAME                
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         L     R2,AFDPNH           DISPLAY ON FIN DPT FIELD                     
         BAS   RE,MOVEFLD                                                       
         L     R2,ASTFH            RESTORE R2 TO THE PERSON FIELD               
*                                                                               
STF19    SR    R1,R1                                                            
         IC    R1,LEV2P            DISPLACEMENT TO STAFF                        
         LA    RF,KEY+3(R1)                                                     
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)       STAFF CODE TO KEY                            
*                                                                               
         BAS   RE,GETACC           GET 2P ACCOUNT                               
         BAS   RE,CHECKACC                                                      
         MVC   PERSNUM,ACCTNUM                                                  
         MVC   PERSNAME,ACCTNAME                                                
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         L     R2,ASTFNH                                                        
         BAS   RE,MOVEFLD                                                       
*                                                                               
STF21    MVC   KEY(15),PERSNUM                                                  
         BAS   RE,GETACC           READ 2P FOR PERSON                           
         L     R4,AIOAREA1                                                      
         MVI   ELCODE,ACSTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   VND01                                                            
*                                                                               
         USING ACSTATD,R4                                                       
         TM    ACSTSTX,X'80'                                                    
         BZ    VND01                                                            
         MVI   V29SW,C'Y'          MAKE CONTRA ON 29 THE VENDOR                 
         EJECT                                                                  
*              VENDOR ACCOUNTS (2C)                                             
*                                                                               
VND01    MVI   CONSULT,C'N'                                                     
         L     R2,AVNDH                                                         
         CLI   INPUT,21                                                         
         BE    VND03                                                            
         CLI   V29SW,C'Y'                                                       
         BNE   VND03               IF CONTRA ON 29 IS VENDOR                    
         TM    SRSW,SRCR           CREDITING SR                                 
         BO    VND03                                                            
         BAS   RE,ANY              MUST HAVE VENDOR                             
*                                                                               
VND03    TM    SRSW,SRCR                                                        
         BNO   VND05                                                            
         CLI   5(R2),0                                                          
         BE    VND05                                                            
         MVC   FVMSGNO,=Y(AE$INVIF)                                             
         B     ERROR                                                            
*                                                                               
VND05    CLI   INPUT,21                                                         
         BE    VND20               TYPE 21 DOES NOT POST TO 2C                  
         CLI   INPUT,24            INSTANT CHECK                                
         BNE   *+8                                                              
         BAS   RE,ANY              MUST HAVE VENDOR TO PRINT ON CHECK           
         CLI   INPUT,23            SKIP STATUS TEST FOR TYPE 23                 
         BE    VND07                                                            
         TM    COMPSTA2,X'04'      COMPANY STATUS TO DEMAND VENDOR              
         BZ    VND07                                                            
         BAS   RE,ANY                                                           
         B     VND09                                                            
*                                                                               
VND07    CLI   5(R2),0             USE IF INPUT-BUT NOT DEMANDED                
         BE    VND20                                                            
*                                                                               
VND09    MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVI   KEY+1,C'S'          READ VENDOR IN EXPENSE SUPPLIERS             
         MVC   KEY+2(1),ACMPSUPX   DEFAULT FOR EXPENSE VENDOR                   
         LA    RF,OPTLVNDR         OPTIONAL LEDGERS FOR TYPE 21                 
         MVC   FVMSGNO,=Y(AE$INACC)                                             
         LA    R4,8(R2)            R4=TO ACCOUNT CODE                           
         SR    R3,R3                                                            
         IC    R3,5(R2)            R3=LENGTH OF INPUT                           
         BCTR  R3,R0                                                            
         LA    RE,KEY+3                                                         
         CLI   0(R4),C'*'          OVERRIDE U/L                                 
         BNE   VND13                                                            
         LA    R4,1(R4)                                                         
         SH    R3,=H'1'            ADJUST LENGTH FOR *                          
         BM    ERROR               MISSING ACCOUNT CODE                         
         LA    RE,KEY+1            RE=UNIT POSITION IN KEY                      
         CLC   0(2,R4),ACMPSUPP    PROD. SUPPLIER IS ALTERNATIVE                
         BE    VND13                                                            
*                                                                               
VND11    SR    R1,R1                                                            
         CLI   1(RF),C' '          ANY LEDGER?                                  
         BE    *+8                                                              
         LA    R1,1                LEDGER IN TABLE                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(RF)       MATCH INPUT CODE TO TABLE                    
         BE    VND13                                                            
         LA    RF,2(RF)                                                         
         CLI   0(RF),X'FF'         END OF TABLE - INVALID ACCOUNT               
         BE    ERROR                                                            
         B     VND11                                                            
*                                                                               
VND13    EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)       CODE TO KEY                                  
*                                                                               
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=Y(AE$INACP)                                             
         BAS   RE,CHECKACC                                                      
         MVC   SVSTAT,ACCTSTAT                                                  
         MVC   VENDNUM,ACCTNUM                                                  
         MVC   VENDNAME,ACCTNAME                                                
         L     R2,AVNDNH                                                        
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         BAS   RE,MOVEFLD                                                       
*                                                                               
         CLI   AGYCTRY,CTRYCAN     TEST FOR CANADA                              
         BNE   VND17               NO                                           
*                                                                               
         MVI   ELCODE,ITCELQ                                                    
         L     R4,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         USING ITCELD,R4                                                        
*                                                                               
VND15    BNE   VND17                                                            
         CLC   SAVEDATE,ITCEFFD    CHECK TRANS DATE VS EFFECTIVE                
         BL    *+14                                                             
         MVC   VENDTYPE,ITCTYPE                                                 
         B     VND17                                                            
*                                                                               
         BAS   RE,NEXTEL           LOOK FOR ANOTHER ELEMENT                     
         B     VND15                                                            
         DROP  R4                                                               
         SPACE 1                                                                
VND17    TM    SVSTAT,X'04'        DOES THE VENDOR DEMAND THAT WE               
         BZ    VND20               MAKE POSTINGS TO '2C'                        
         CLI   INPUT,21                                                         
         BNE   *+10                                                             
         MVC   KEY(15),CRTNUM                                                   
*                                                                               
         MVC   KEY+1(2),=C'2C'                                                  
         LA    R2,PICCRTH                                                       
         BAS   RE,GETACC           MUST HAVE SAME CODE IN '2C'                  
         MVI   CONSULT,C'Y'                                                     
         MVC   V2CNUM,ACCTNUM                                                   
         MVC   V2CNAM,ACCTNAME                                                  
         MVC   KEY+1(14),=CL14'27999'                                           
         BAS   RE,GETACC                                                        
         MVC   CONTROL,ACCTNUM                                                  
         MVC   CONTROLN,ACCTNAME                                                
         B     VND25                                                            
         EJECT                                                                  
*                                                                               
* EXIT TO DUPLICATE PAYMENT CHECK ROUTINE                                       
*                                                                               
VND20    CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   VND20X2             NO                                           
         CLI   PFKEY,7                                                          
         BNE   VND20A                                                           
         CLI   MODE,1                                                           
         BH    VND20A                                                           
         B     INV20                                                            
*                                                                               
VND20A   BAS   RE,EDTAX            READY TO CHECK                               
         BE    VND20B                                                           
         CLI   ERRNUM,OK           ERROR OCCURED                                
         BNE   ERROR                                                            
*                                                                               
VND20B   L     RF,=A(CTAXMOD)                                                   
         L     RE,PRELO                                                         
         AR    RF,RE                                                            
         MVC   MSG,SPACES                                                       
         MVC   TMPMODE,MODE                                                     
         MVI   MODE,4                                                           
         GOTO1 (RF),DMCB,(RC)                                                   
         MVC   MODE,TMPMODE                                                     
         CLI   CTXMODE,C'Z'                                                     
         BNE   VND20X                                                           
         MVI   FVOMTYP,C'E'                                                     
         MVC   FVMSGNO,CTXMSGNO                                                 
         L     R2,FVADDR                                                        
         B     ERROR                                                            
*                                                                               
VND20X   ZAP   TOTNET,TMPNET                                                    
         ZAP   TOTGRS,TMPGRS                                                    
         ZAP   GST,TMPGST                                                       
         ZAP   PST,TMPPST                                                       
         B     DPC01                                                            
*                                                                               
VND20X2  ZAP   TOTNET,NTAMNT                                                    
         ZAP   TOTGRS,GRAMNT                                                    
         B     DPC01                                                            
         EJECT                                                                  
*                                                                               
* EXIT TO POST ROUTINE                                                          
*                                                                               
VND25    CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   VND25X2             NO                                           
         CLI   PFKEY,7                                                          
         BNE   VND25A                                                           
         CLI   MODE,1                                                           
         BH    VND25A                                                           
         B     INV20                                                            
*                                                                               
VND25A   BAS   RE,EDTAX            READY TO CHECK                               
         BE    VND25B                                                           
         CLI   ERRNUM,OK           ERROR OCCURED                                
         BNE   ERROR                                                            
*                                                                               
VND25B   L     RF,=A(CTAXMOD)                                                   
         L     RE,PRELO                                                         
         AR    RF,RE                                                            
         MVC   MSG,SPACES                                                       
         MVI   MODE,4                                                           
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   CTXMODE,C'Z'                                                     
         BNE   VND25X                                                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVI   ERRNUM,SPECIAL                                                   
         L     R2,FVADDR                                                        
         B     ERROR                                                            
*                                                                               
VND25X   ZAP   TOTNET,TMPNET                                                    
         ZAP   TOTGRS,TMPGRS                                                    
         ZAP   GST,TMPGST                                                       
         ZAP   PST,TMPPST                                                       
         B     POST01                                                           
*                                                                               
VND25X2  ZAP   TOTNET,NTAMNT                                                    
         ZAP   TOTGRS,GRAMNT                                                    
         B     POST01                                                           
         EJECT                                                                  
*--------------------------------------------------------------                 
*        SUB-ROUTINE TO EDIT TAX FIELDS AND DISPLAY DATA                        
*        ON EXIT, CC=EQ IF OK, NEQ IF ERROR AND R2 SET TO ERR FLD               
*--------------------------------------------------------------                 
EDTAX    NTR1                                                                   
         MVI   ERRNUM,OK                                                        
         L     R2,ATYPEH                                                        
         CLI   8(R2),C'*'          GST/PST NOT APPLICABLE                       
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
         ZAP   GST,TMPGST                                                       
         ZAP   PST,TMPPST                                                       
*                                                                               
EDTAXX   CLI   ERRNUM,OK           YES-RESET VENDOR AMT=GROSS                   
XITR2    XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              LOOK AT 'SA' OR 'SB' LEDGER AND POSTINGS                         
*                                                                               
CHECKSA  NTR1                                                                   
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),CRTNUM+1                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CHECKNO                                                          
         LA    RF,IOAREA                                                        
         SR    R1,R1                                                            
*                                                                               
CHECK02  CLI   0(RF),0             SEE IF LEDGER IS OPEN ITEM                   
         BE    CHECKNO                                                          
         CLI   0(RF),ACLTELQ                                                    
         BE    CHECK04                                                          
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     CHECK02                                                          
*                                                                               
         USING ACLEDGD,RF                                                       
CHECK04  CLI   ACLTLIKE,C'R'                                                    
         BNE   CHECKYS                                                          
         MVC   KEY(15),CRTNUM                                                   
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
CHECK10  BAS   RE,SEQ                                                           
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CHECKNO                                                          
         LA    RF,IOAREA                                                        
         CLI   0(RF),X'43'                                                      
         BNE   CHECK12                                                          
         USING TRSUBHD,RF          SAVE CONTRA-ACCOUNT DETAILS                  
         SR    R1,R1                                                            
         MVC   SAVNAM,SPACES                                                    
         MVC   SAVNUM,TRSBACNT                                                  
         IC    R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         EX    R1,*+8                                                           
         B     CHECK10                                                          
         MVC   SAVNAM(0),TRSBNAME                                               
*                                                                               
CHECK12  DS    0H                                                               
         CLI   0(RF),TRNSELQ                                                    
         BNE   CHECK10                                                          
         USING TRANSD,RF                                                        
         TM    TRNSSTAT,X'80'      MUST BE DEBIT                                
         BZ    CHECK10                                                          
         CLC   SAVEDATE,TRNSDATE   AND MATCH ON DATE                            
         BNE   CHECK10                                                          
         CLC   SAVDOC,TRNSREF                                                   
         BE    CHECKYS                                                          
         B     CHECK10                                                          
*                                                                               
CHECKNO  MVC   FVMSGNO,=Y(AE$NTFBD)                                             
         B     XIT                                                              
*                                                                               
CHECKYS  MVI   ERRNUM,OK                                                        
         B     XIT                                                              
         EJECT                                                                  
*              FIND NEXT NUMBER FOR AUTO CHECK NUMBERING                        
*                                                                               
NUMFIX   NTR1                                                                   
         CLC   PICDOC(4),=C'NEXT'                                               
         BNE   XIT                                                              
         ZAP   DUB,=P'0'                                                        
         LA    R1,LASTDOC                                                       
         LA    R0,L'LASTDOC                                                     
NUMFIX2  CLI   0(R1),C'0'                                                       
         BL    NUMFIX4                                                          
         CLI   0(R1),C'9'                                                       
         BH    NUMFIX4                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,NUMFIX2                                                       
         PACK  DUB,LASTDOC                                                      
NUMFIX4  AP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  SAVDOC,DUB                                                       
         MVC   LASTDOC,SAVDOC                                                   
         B     XIT                                                              
         EJECT                                                                  
*              DUPLICATE PAYMENT CHECK                                          
*                                                                               
DPC01    TM    COMPSTA2,X'08'       'CHECK-DUPL' OPTION FROM LFM/COMP           
         BZ    POST01                                                           
         CLI   INPUT,21                                                         
         BNE   POST01                                                           
         LA    RF,OPTLDPCK                                                      
*                                                                               
DCP03    CLI   0(RF),X'FF'                                                      
         BE    POST01                                                           
         CLC   CRTNUM+1(2),0(RF)                                                
         BE    DPC05                                                            
         LA    RF,2(RF)                                                         
         B     DCP03                                                            
*                                                                               
DPC05    LA    R2,PICDOCH                                                       
         MVI   FVOMTYP,C'E'                                                     
         MVC   IOKEY,SPACES                                                     
         MVC   IOKEY(15),CRTNUM                                                 
         MVC   IOKEY+17(15),DBTNUM                                              
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST4-BCCPYEL(RF),CPYSOFF2 TEST NEW OFFICES                   
         BNO   *+10                                                             
         MVC   IOKEY+15(2),CRDOFC    OFFICE                                     
         GOTO1 AIO,IOHI+IOACCFIL+IO1                                            
         BL    ERRXIT              IO ERROR                                     
*                                                                               
DPC07    L     RE,AIO1                                                          
         CLC   0(32,RE),IOKEYSAV                                                
         BNE   POST01                                                           
         LA    RF,ACRECORD-ACKEYD(RE)                                           
         CLI   0(RF),TRNSELQ                                                    
         BNE   DPC09                                                            
         TM    ACSTATUS-ACKEYD(RE),X'40' DRAFT                                  
         BO    DPC09                                                            
         USING TRANSD,RF                                                        
         TM    TRNSSTAT,X'80'       SKIP THE DEBITS                             
         BO    DPC09                                                            
         CLC   TRNSREF,SAVDOC                                                   
         BNE   DPC09                                                            
         CP    TRNSAMNT,TOTGRS                                                  
         BNE   DPC09                                                            
         MVC   FVMSGNO,=Y(AE$RECAE)                                             
         B     ERROR                                                            
*                                                                               
DPC09    GOTO1 AIO,IOSQ+IOACCFIL+IO1                                            
         BL    ERRXIT              IO ERROR                                     
         B     DPC07                                                            
         DROP  RF                                                               
         EJECT                                                                  
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
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
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*                   BUILD 64 ELEMENT                                            
*                                                                               
POST01   LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         XC    DLDSEL(20),DLDSEL   CLEAR                                        
         MVI   DLDSEL,DLDSELQ                                                   
         CLI   INPUT,24                                                         
         BNE   POST03                                                           
         CLC   SAVDOC(4),=C'NEXT'                                               
         BNE   POST03                                                           
         BAS   RE,NUMFIX                                                        
*                                                                               
POST03   MVC   DLDSREF,SAVDOC                                                   
         MVC   DLDSDATE,SAVEDATE                                                
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         CLI   PICURG,C'U'         URGENT?                                      
         BNE   POST05                                                           
         OI    DLDSSTAT,X'40'                                                   
         B     POST07                                                           
*                                                                               
POST05   LA    R2,PICURGH                                                       
         MVI   FVOMTYP,C'E'                                                     
         MVC   FVMSGNO,=Y(AE$INVIF)                                             
         CLI   PICURGH+5,0                                                      
         BNE   ERROR                                                            
*                                                                               
POST07   L     R2,ANARH                                                         
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         BAS   RE,NARRSCAN                                                      
         SR    R3,R3                                                            
         LA    R5,DLDSNARR                                                      
         SR    R5,R8               R5 = ELEMENT - NARRATIVE                     
         AR    R5,R6               R6 = L'NARRATIVE                             
         STH   R5,HALF                                                          
         MVC   DLDSLEN,HALF+1                                                   
                                                                                
         XC    ELDB,ELDB                                                        
         OC    CLINUM,CLINUM                                                    
         BZ    POST47                                                           
                                                                                
         LA    R6,ELDB                                                          
         USING FFTELD,R6                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'FFTCLPRA                               
         MVI   FFTTYPE,FFTTCLPR                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'FFTCLPRA                                               
         MVC   FFTCLAC,CLINUM+3                                                 
         OC    PRONUM,PRONUM                                                    
         BZ    *+10                                                             
         MVC   FFTPRAC,PRONUM+6                                                 
         OC    FFTCLPRA,SPACES                                                  
         DROP  R6                                                               
                                                                                
POST47   GOTO1 =A(POSTIT),DMCB,(R5),(R8),(R9),(RC),RR=PRELO                     
                                                                                
         EJECT                                                                  
*              DOUBLE POSTING FOR VENDOR                                        
*              DEBIT VENDOR (2C) - CONTRA IS CONTROL (27)                       
*              CREDIT CONTROL (27) - DEBIT VENDOR (2C)                          
*                                                                               
         USING DLPOSTD,R8                                                       
         CLI   CONSULT,C'Y'        POSTING TO '2C'                              
         BNE   POST49                                                           
         LR    R6,R8               SAVE ADDRESS OF LAST                         
         IC    R3,DLPSLEN                                                       
         LA    R8,0(R3,R8)                                                      
         BRAS  RE,ADDANL           ADD ANALYSED OFFICE ELEMENT                  
         BRAS  RE,ADDADB           CLIENT/PRODUCT ELEMENT                       
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   0(0,R8),0(R6)                                                    
         MVI   DLPSEL,DLPSEDCQ                                                  
         OI    DLPSTYPE,X'80'                                                   
         MVC   DLPSDBAC,V2CNUM                                                  
         MVC   DLPSDBNM,V2CNAM                                                  
         MVC   DLPSCRAC,CONTROL                                                 
         MVC   DLPSCRNM,CONTROLN                                                
         ZAP   DLPSAMNT,EXPOST     MAKE ANALYSIS AMOUNTS SAME                   
*                                  AS POSTING TO EXPENSE ACCOUNT                
         MVC   DLPSANAL,CRDOFC     OFFICE CODE                                  
*                                                                               
POST49   IC    R3,DLPSLEN                                                       
         LA    R8,0(R3,R8)                                                      
         MVI   0(R8),0             END OF RECORD                                
         LA    R5,IOAREA-1                                                      
         SR    R8,R5                                                            
         STH   R8,HALF                                                          
         MVC   IOAREA(2),HALF      FINAL LENGTH                                 
         BAS   RE,PUTDAY                                                        
         EJECT                                                                  
*                   ADD ENTRY TO TWA1                                           
*                                                                               
         XC    WORK,WORK                                                        
         IC    R3,PICDOCH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),PICDOC      REF                                          
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         CLI   INPUT,24                                                         
         BNE   POST51                                                           
         CLC   PICDOC(4),=C'NEXT'                                               
         BNE   POST51                                                           
         MVC   WORK(6),SAVDOC                                                   
POST51   BAS   RE,ADSCRINF                                                      
         MVI   GSTSW,C'N'          RESET CANADA SCREEN                          
         XC    KEEPPROV,KEEPPROV                                                
         MVI   CLEAROK,C'Y'        CLEAR GST/PST NEXT TIME                      
         MVI   USERPROV,C'N'                                                    
         MVI   USERPST,C'N'                                                     
         CLI   AGYCTRY,CTRYCAN     CANADIAN?                                    
         BNE   *+12                                                             
         L     R2,APROVH                                                        
         MVI   4(R2),0                                                          
*                                                                               
         ZAP   TOTCASH,TOTGRS                                                   
         CLI   MODE,1              DEAL WITH ORDER RECORD UPDATE                
         BNE   POST53                                                           
         LA    R3,22                                                            
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,,(R9)                                                  
         CLI   ERRNUM,X'FE'                                                     
         BL    EXIT                                                             
         BE    POST55                                                           
*                                                                               
POST53   LA    R2,PICORDH                                                       
*                                                                               
POST55   MVI   ERRNUM,OK                                                        
         CLI   INPUT,24                                                         
         BNE   EXIT                                                             
         L     RF,=A(WRITCHK)                                                   
         A     RF,PRELO                                                         
         GOTO1 (RF),DMCB,(RC)                                                   
         B     EXIT                                                             
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
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
XIT      XIT1                                                                   
*                                                                               
*              ERROR MESSAGE TABLE                                              
*                                                                               
INDEPMSG DC    C'**ERROR** ACCOUNT NOT FLAGGED FOR DEPT'                        
INCLIMSG DC    C'**ERROR** ACCOUNT NOT FLAGGED FOR ANALYSIS'                    
INSTFMSG DC    C'**ERROR** ACCOUNT NOT FLAGGED FOR STAFF'                       
*                                                                               
*                                                                               
OPTLCRT  DC    C'SY',C'SW',C'SX',C'SA',X'FF'                                    
*                                                                               
OPTLBANK DC    C'SB',C'SF',C'SL',C'SA',C'SD',C'SR',C'G ',X'FF'                  
*                                                                               
OPTLDBT  DC    C'SA',C'SF',C'SL',C'SB',C'SC',C'SD',C'SX',C'SR'                  
         DC    C'G ',X'FF'                                                      
*                                                                               
OPTLVNDR DC    C'SW',C'SY',X'FF'                                                
*                                                                               
OPTLDPCK DC    C'SV',C'SX',C'SY',C'SW',X'FF'                                    
*                                                                               
ADVLDG   DC    C'SA',C'SB',X'FF'                                                
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
NOCTAX   DC    C'** ERROR - CANADIAN SCREEN NOT AVAILABLE'                      
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* US SCREEN TABLE  ( BYTES 0-3=DISP TO FIELD, BYTES 4-7=DISP TO ADCON )         
*                                                                               
USTAB    DS    0D                                                               
         DC    AL4(PICFOFFH-TWAD),AL4(AFOFFH-PROGD)                             
         DC    AL4(PICFDPTH-TWAD),AL4(AFDPTH-PROGD)                             
         DC    AL4(PICFDPNH-TWAD),AL4(AFDPNH-PROGD)                             
         DC    AL4(PICPOFFH-TWAD),AL4(APOFFH-PROGD)                             
         DC    AL4(PICCOFFH-TWAD),AL4(ACOFFH-PROGD)                             
         DC    AL4(PICPDPTH-TWAD),AL4(APDPTH-PROGD)                             
         DC    AL4(PICPDPNH-TWAD),AL4(APDPNH-PROGD)                             
         DC    AL4(PICSTFH-TWAD),AL4(ASTFH-PROGD)                               
         DC    AL4(PICSTFNH-TWAD),AL4(ASTFNH-PROGD)                             
         DC    AL4(PICCLIH-TWAD),AL4(ACLIH-PROGD)                               
         DC    AL4(PICCLINH-TWAD),AL4(ACLINH-PROGD)                             
         DC    AL4(PICPROH-TWAD),AL4(APROH-PROGD)                               
         DC    AL4(PICPRONH-TWAD),AL4(APRONH-PROGD)                             
         DC    AL4(PICVNDH-TWAD),AL4(AVNDH-PROGD)                               
         DC    AL4(PICVNDNH-TWAD),AL4(AVNDNH-PROGD)                             
         DC    AL4(PICSRCH-TWAD),AL4(ASRCH-PROGD)                               
         DC    AL4(PICNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    AL4(PICTABH-TWAD),AL4(ATABH-PROGD)                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* CA SCREEN TABLE  ( BYTES 0-3=DISP TO FIELD, BYTES 4-7=DISP TO ADCON )         
*                                                                               
CANTAB   DS    0D                                                               
         DC    AL4(PITTYPEH-TWAD),AL4(ATYPEH-PROGD)                             
         DC    AL4(PITTYPNH-TWAD),AL4(ATYPNH-PROGD)                             
         DC    AL4(PITGORNH-TWAD),AL4(AGORNH-PROGD)                             
         DC    AL4(PITGSTXH-TWAD),AL4(AGSTXH-PROGD)                             
         DC    AL4(PITGAMTH-TWAD),AL4(AGAMTH-PROGD)                             
         DC    AL4(PITPROVH-TWAD),AL4(APROVH-PROGD)                             
         DC    AL4(PITFOFFH-TWAD),AL4(AFOFFH-PROGD)                             
         DC    AL4(PITCOFFH-TWAD),AL4(ACOFFH-PROGD)                             
         DC    AL4(PITFDPTH-TWAD),AL4(AFDPTH-PROGD)                             
         DC    AL4(PITFDPNH-TWAD),AL4(AFDPNH-PROGD)                             
         DC    AL4(PITPOFFH-TWAD),AL4(APOFFH-PROGD)                             
         DC    AL4(PITPDPTH-TWAD),AL4(APDPTH-PROGD)                             
         DC    AL4(PITPDPNH-TWAD),AL4(APDPNH-PROGD)                             
         DC    AL4(PITSTFH-TWAD),AL4(ASTFH-PROGD)                               
         DC    AL4(PITSTFNH-TWAD),AL4(ASTFNH-PROGD)                             
         DC    AL4(PITCLIH-TWAD),AL4(ACLIH-PROGD)                               
         DC    AL4(PITCLINH-TWAD),AL4(ACLINH-PROGD)                             
         DC    AL4(PITPROH-TWAD),AL4(APROH-PROGD)                               
         DC    AL4(PITPRONH-TWAD),AL4(APRONH-PROGD)                             
         DC    AL4(PITVNDH-TWAD),AL4(AVNDH-PROGD)                               
         DC    AL4(PITVNDNH-TWAD),AL4(AVNDNH-PROGD)                             
         DC    AL4(PITSRCH-TWAD),AL4(ASRCH-PROGD)                               
         DC    AL4(PITNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    AL4(PITTABH-TWAD),AL4(ATABH-PROGD)                               
         DC    X'FF'                                                            
         EJECT                                                                  
*              BUILD 69&6A MAIN ACCOUNTING ELEMENTS                             
*                                                                               
*              DEBIT THE EXPENSE ACCOUNT - CONTRA IS THE CREDIT ACCOUNT         
*                                                                               
POSTIT   DS    0D                                                               
         NMOD1 0,**POST**                                                       
*                                                                               
         L     R5,0(R1)                                                         
         L     R8,4(R1)                                                         
         L     R9,8(R1)                                                         
         L     RC,12(R1)                                                        
*                                                                               
         AR    R8,R5                                                            
         ZAP   TRANSAMT,TOTGRS                                                  
         ZAP   DUB,TOTGRS                                                       
         CP    CDAMNT,=P'0'                                                     
         BE    POST11                                                           
         USING TRCASHD,R8                                                       
         ZAP   TRCSAMNT,CDAMNT     CD ELEMENT FOR EXPENSE A/C                   
         MVI   TRCSTYPE,C'D'                                                    
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         SR    R5,R5                                                            
         IC    R5,TRCSLEN                                                       
         AR    R8,R5                                                            
*                                                                               
POST11   CLI   PICORDH+5,0         EXTRA ORDER NUMBER ELEMENT                   
         BE    POST13                                                           
         CLC   ORDNO,SPACES        MUST HAVE A VALID ORDER NUMBER               
         BNH   POST13                                                           
         USING ACNOD,R8                                                         
         MVC   ACNOEL(2),=X'250A'                                               
         MVC   ACNO(6),ORDNO                                                    
         CLI   PICORD+7,C'P'       PARTIAL ORDER?                               
         BNE   *+8                                                              
         MVI   ACNOSTAT,C'P'       YES, THEN SET PARTIAL FLAG                   
         SR    R5,R5                                                            
         IC    R5,ACNOLEN                                                       
         AR    R8,R5                                                            
*                                                                               
POST13   BRAS  RE,ADDANL           ADD ANALYSED OFFICE ELEMENT                  
         BRAS  RE,ADDADB           ADD CLIENT/PRODUCT                           
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ                                                  
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,DBTNUM     DEBIT ACCOUNT                                
         MVC   DLPSDBNM,DBTNAM                                                  
         MVC   DLPSCRAC,SPACES                                                  
         MVC   DLPSCRNM,SPACES     IF SOUCRE IS PRESENT                         
         TM    SRSW,SRDR                                                        
         BZ    POST15                                                           
         L     R2,ASRCH                                                         
         SR    R3,R3               THE DEBIT IS TO SR                           
         IC    R3,5(R2)            THE CONTRA IS SOURCE                         
         LTR   R3,R3                                                            
         BZ    POST15              NO SOURCE                                    
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     POST17                                                           
         MVC   DLPSCRAC+3(0),8(R2)                                              
*                                                                               
POST15   MVC   DLPSCRAC,CRTNUM     CONTRA IS CREDIT                             
         MVC   DLPSCRNM,CRTNAM                                                  
*        TM    SRSW,SRCR                                                        
*        BNO   *+16                                                             
*        MVC   DLPSCRAC,CPJRNUM                                                 
*        MVC   DLPSCRNM,CPJRNAME                                                
         OC    VENDNUM,VENDNUM                                                  
         BZ    POST17                                                           
         MVC   DLPSCRAC,VENDNUM    USE INPUT VENDOR FOR CONTRA A/C              
         MVC   DLPSCRNM,VENDNAME                                                
*                                                                               
POST17   CLI   INPUT,22            FOR TYPE 22 THERE IS A COMPANY               
         BNE   POST19              OPTION TO MAKE THE CONTRA-AC OF SE           
         TM    COMPSTA3,X'08'      THE CASH ACCOUNT                             
         BZ    POST19                                                           
         CLC   DLPSDBAC+1(2),=C'SE' FOR TYPE 22 DEBIT TO EXPENSE                
         BE    *+14                                                             
         CLC   DLPSDBAC+1(2),=C'SY' OR CANADIAN EXPENSE                         
         BNE   POST19                                                           
         CLC   CRTNUM+1(2),=C'SC'  OPTION TO MAKE CONTRA/AC                     
         BNE   POST19                                                           
         MVC   DLPSCRAC,CRTNUM     THE CASH ACCOUNT                             
         MVC   DLPSCRNM,CRTNAM                                                  
*                                                                               
POST19   MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,CLIOFC     CLIENT OFFICE                                
         ZAP   DLPSAMNT,TOTNET     START WITH NET AMOUNT                        
         SP    DLPSAMNT,CDAMNT     POSTING IS INPUT AMT LESS CD                 
         TM    COMPSTAT,X'08'                                                   
         BZ    *+10                                                             
         AP    DLPSAMNT,CDAMNT     BUT NOT IF THEY TAKE CD AS INCOME            
         ZAP   EXPOST,DLPSAMNT     SAVE EXPENSE POSTING                         
         LA    R2,PICAMTH                                                       
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LA    R1,6(R1,R2)                                                      
         CLC   DLPSDBAC+1(2),=C'SC'                                             
         BE    POST21                                                           
         CLC   DLPSDBAC+1(2),=C'SD'                                             
         BE    POST21                                                           
         CLC   DLPSDBAC+1(2),=C'SX'                                             
         BNE   POST22                                                           
*                                                                               
POST21   MVI   DLPSEL,DLPSECRQ     MAKE CASH POSTING A MINUS CREDIT             
         ZAP   DUB,DLPSAMNT                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   DLPSAMNT,DUB        AND VENDOR                                   
         MVC   WORK(51),DLPSDBAC                                                
         MVC   DLPSDBAC(51),DLPSCRAC                                            
         MVC   DLPSCRAC(51),WORK                                                
         EJECT                                                                  
*              DEBIT POSTING TO GST IF APPLICABLE                               
*                                                                               
POST22   CLI   GSTSW,C'Y'          TEST GST IS APPLICABLE                       
         BNE   POST23                                                           
         ST    R8,ADEBIT           SAVE DEBIT POSITION                          
         IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         L     R2,AIOA                                                          
         ZICM  RF,0(R2),2                                                       
         LTR   RF,RF                                                            
         BZ    POST23                                                           
         SH    RF,=H'2'                                                         
         LA    RE,2(R2)                                                         
         LR    R0,R8                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         LR    R8,R0                                                            
         B     POST23A                                                          
         EJECT                                                                  
*              CREDIT POSTING - CONTRA IS CLIENT                                
*                                                                               
POST23   IC    R3,DLPSLEN                                                       
         AR    R8,R3               READY FOR NEXT                               
POST23A  CP    CDAMNT,=P'0'                                                     
         BE    POST24                                                           
         USING TRCASHD,R8                                                       
         ZAP   TRCSAMNT,CDAMNT     CD FOR SUPPLIER                              
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         SR    R5,R5                                                            
         IC    R5,TRCSLEN                                                       
         AR    R8,R5                                                            
*                                                                               
POST24   CLI   GSTSW,C'Y'          TEST CANADA AND GST APPLIES                  
         BNE   POST25              NO                                           
*&&DO                                                                           
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'T'       'T'=TAX PAID                                 
         ZAP   TRCSAMNT,GST                                                     
         ZIC   R5,TRCSLEN                                                       
         AR    R8,R5                                                            
*&&                                                                             
         USING DLPOSTD,R8                                                       
POST25   TM    COMPSTA3,X'80'      IS SX-CONTRA CLIENT                          
         BZ    POST29              NO                                           
         CLI   INPUT,21                                                         
         BNE   POST29              NOT TYPE 21                                  
         CLC   CRTNUM+1(2),=C'SX'  MUST BE CREDIT TO SX                         
         BNE   POST29                                                           
*                                                                               
         USING TRSDESCD,R8                                                      
POST27   MVI   TRSDEL,X'4C'                                                     
         MVC   TRSDACCS(80),SPACES                                              
         MVC   TRSDACCS(14),DBTNUM+1                                            
*                                                                               
         GOTO1 SQUASHER,DMCB,TRSDACCS,80                                        
         SR    R5,R5                                                            
         IC    R5,DMCB+7                                                        
         LA    R5,2(R5)                                                         
         STC   R5,TRSDLEN                                                       
         AR    R8,R5                                                            
*                                                                               
         BRAS  RE,ADDANL           ADD ANALYSED OFFICE ELEMENT                  
         BRAS  RE,ADDADB           CLIENT/PRODUCT ELEMENT                       
         USING DLPOSTD,R8                                                       
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSEL,DLPSECRQ                                                  
         MVI   DLPSTYPE,0          CREDIT SUPPLIER                              
         MVC   DLPSANAL,CRDOFC     OFFICE CODE                                  
         ZAP   DLPSAMNT,TOTGRS                                                  
         SP    DLPSAMNT,CDAMNT     POST INPUT LESS CD                           
         IC    R3,DLPSLEN                                                       
         MVC   DLPSCRAC,CRTNUM                                                  
         MVC   DLPSCRNM,CRTNAM                                                  
         MVC   DLPSDBAC,SPACES                                                  
         MVC   DLPSDBNM,SPACES                                                  
         MVC   DLPSDBAC(1),COMPANY                                              
         MVC   DLPSDBAC+1(5),=C'SJ999'                                          
         MVC   DLPSDBNM(10),=C'NON-CLIENT'                                      
*                                                                               
         L     R2,ACLIH                                                         
         CLI   5(R2),0                                                          
         BE    POST33              NO CLIENT INPUT                              
*                                                                               
         MVC   DLPSDBAC+3(3),SPACES                                             
         MVC   DLPSDBNM,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLPSDBAC+3(0),8(R2)                                              
         L     R2,ACLINH                                                        
         MVC   DLPSDBNM,8(R2)                                                   
         OC    DLPSDBNM,SPACES                                                  
         B     POST33                                                           
         EJECT                                                                  
*              CREDIT POSTING - CONTRA IS DEBIT OR 'SR' ACCOUNT                 
*                                                                               
POST29   BRAS  RE,ADDANL           ADD ANALYSED OFFICE ELEMENT                  
         BRAS  RE,ADDADB           CLIENT/PRODUCT ELEMENT                       
         USING DLPOSTD,R8                                                       
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSEL,DLPSECRQ                                                  
         MVI   DLPSTYPE,0          CREDIT SUPPLIER                              
         MVC   DLPSANAL,CRDOFC     OFFICE CODE                                  
         ZAP   DLPSAMNT,TOTGRS                                                  
         SP    DLPSAMNT,CDAMNT     POST INPUT LESS CD                           
         MVC   DLPSDBAC,DBTNUM                                                  
         MVC   DLPSDBNM,DBTNAM                                                  
         MVC   DLPSCRAC,CRTNUM                                                  
         MVC   DLPSCRNM,CRTNAM                                                  
         TM    SRSW,SRCR                                                        
         BNO   POST31                                                           
*        MVC   DLPSCRAC,CPJRNUM    FIX UP ACCOUNTS FOR SR POSTING               
*        MVC   DLPSCRNM,CPJRNAME                                                
*        MVC   DLPSDBAC(L'DLPSDBAC+L'DLPSDBNM),SPACES                           
*        MVC   DLPSDBAC+3(12),SRSAVE                                            
         MVC   DLPSDBAC,SPACES                                                  
         MVC   DLPSDBNM,SPACES                                                  
         L     R2,ASRCH                                                         
         SR    R3,R3               THE DEBIT IS TO SR                           
         IC    R3,5(R2)            THE CONTRA IS SOURCE                         
         LTR   R3,R3                                                            
         BZ    POST33              NO SOURCE                                    
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     POST33                                                           
         MVC   DLPSDBAC+3(0),8(R2)                                              
*                                                                               
POST31   CLI   INPUT,21            FOR OPEN ITEM ADVANCE ACCOUNTS               
         BE    POST33                                                           
         OC    SAVNUM,SAVNUM       ACCOUNT AS THE DEBIT WE ARE MATCHING         
         BZ    POST33                                                           
         MVC   DLPSDBAC,SAVNUM                                                  
         MVC   DLPSDBNM,SAVNAM                                                  
         EJECT                                                                  
*              CREDIT CASH DISCOUNT INCOME WHEN NOT PASSED TO CLIENT            
*                                                                               
POST33   TM    COMPSTAT,X'08'      TEST C.D. ON EXPENSE TO INCOME               
         BNO   POST35                                                           
         CP    CDAMNT,=P'0'                                                     
         BE    POST35                                                           
         LR    R6,R8               SAVE ADDRESS OF LAST                         
         IC    R3,DLPSLEN                                                       
         LA    R8,0(R3,R8)                                                      
         BRAS  RE,ADDANL           ADD ANALYSED OFFICE ELEMENT                  
         BRAS  RE,ADDADB           CLIENT/PRODUCT ELEMENT                       
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   0(0,R8),0(R6)                                                    
         ZAP   DLPSAMNT,CDAMNT                                                  
         MVC   DLPSCRAC,CDACC      CREDIT INCOME ACCOUNT                        
         MVC   DLPSCRNM,CDNAME                                                  
         MVC   DLPSDBAC,CRTNUM     CONTRA IS THE CREDIT (SUPPLIER)              
         MVC   DLPSDBNM,CRTNAM                                                  
         MVC   DLPSANAL,CLIOFC                                                  
         EJECT                                                                  
*              DEBIT STAFF (2P) CONTRA IS *EXPENSE-CLIENT (29)                  
*                                                                               
POST35   TM    ANLSW,ANLSTF        PERSONNEL EXPENSE                            
         BNO   POST41                                                           
         LR    R6,R8               SAVE ADDRESS OF LAST                         
         IC    R3,DLPSLEN                                                       
         LA    R8,0(R3,R8)                                                      
         BRAS  RE,ADDANL           ADD ANALYSED OFFICE ELEMENT                  
         BRAS  RE,ADDADB           CLIENT/PRODUCT ELEMENT                       
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   0(0,R8),0(R6)                                                    
         MVI   DLPSEL,DLPSEDRQ     DEBIT STAFF                                  
         OI    DLPSTYPE,X'80'                                                   
         MVC   DLPSDBAC,PERSNUM                                                 
         MVC   DLPSDBNM,PERSNAME                                                
         MVC   DLPSCRNM,PSCLINAM                                                
         MVI   DLPSCRAC,C'*'       CONTRA IS *EXPENSE-CLIENT                    
         ZAP   DLPSAMNT,EXPOST     MAKE ANALYSIS AMOUNTS SAME                   
         MVC   DLPSANAL,CLIOFC                                                  
*                                  AS POSTING TO EXPENSE ACCOUNT                
POST37   SR    R1,R1                                                            
         IC    R1,DBTLEN           LENGTH OF DEBIT  ACCOUNT                     
         BCTR  R1,R0                                                            
         CLI   PICDBT,C'*'                                                      
         BNE   *+8                                                              
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),DBTNUM+3                                           
         CLI   TENO,X'F0'          TAKE 'N' BYTES FROM R.H.                     
         BL    POST39              SIDE OF SE ACCOUNT                           
         PACK  DUB,TENO                                                         
         CVB   R5,DUB                                                           
         MVC   DLPSCRAC+1(14),SPACES                                            
         LA    R2,PICDBTH                                                       
         MVC   FVMSGNO,=Y(AE$INVIF)                                             
         LA    RF,1(R1)            INPUT MUST BE AT LEAST 'N' LONG              
         SR    RF,R5                                                            
         BM    ERROR                                                            
         LA    RE,PICDBT(RF)                                                    
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),0(RE)                                              
         LR    R1,R5                                                            
*                                                                               
POST39   STC   R1,BYTE                                                          
         LA    RF,DLPSCRAC+2(R1)                                                
         MVI   0(RF),C'-'                                                       
         LA    R1,DLPSCRAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),PSCLINUM+3                                               
         EJECT                                                                  
*              CREDIT CLIENT (29) - CONTRA IS *EXPENSE-STAFF (2P)               
*                                                                               
         LR    R6,R8               SAVE ADDRESS OF LAST                         
         IC    R3,DLPSLEN                                                       
         LA    R8,0(R3,R8)                                                      
         BRAS  RE,ADDANL           ADD ANALYSED OFFICE ELEMENT                  
         BRAS  RE,ADDADB           CLIENT/PRODUCT ELEMENT                       
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   0(0,R8),0(R6)                                                    
         IC    R3,DLPSLEN                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT CLIENT                                
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         MVC   DLPSDBAC,DLPSCRAC                                                
         MVC   DLPSCRAC,PSCLINUM                                                
         LA    RF,DLPSDBAC+1(R1)                                                
         MVI   0(RF),C'-'                                                       
         MVC   1(10,RF),SPACES                                                  
         MVC   DLPSDBNM,PERSNAME                                                
         LA    R1,DLPSDBAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),PERSNUM+3                                                
*                                                                               
         CLI   V29SW,C'Y'                                                       
         BNE   POST41                                                           
         SR    RE,RE                                                            
         IC    RE,LEV2P                                                         
         LA    RF,1(RE,RF)                                                      
         SR    R1,RE                                                            
         MVC   DLPSDBNM,VENDNAME   CONTRA ACC IS VENDOR                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),VENDNUM+3                                                
         CLI   INPUT,22                                                         
         BE    POST41                                                           
         MVC   DLPSDBNM,CRTNAM                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),CRTNUM+3                                                 
         EJECT                                                                  
*              BUILD SUBSIDIARY ELEMENTS - IF NECESSARY                         
*                                                                               
*              DOUBLE POSTING FOR DEPARTMENT ANALYSIS                           
*              DEBIT  DEPT. ANALYSIS (2D) - CONTRA DEPT. SUSPENSE (28)          
*              CREDIT DEPT.SUSPENSE (28) - CONTRA DEPT. ANALYSIS (2D)           
*                                                                               
POST41   TM    ANLSW,ANLDPT                                                     
         BZ    POST43                                                           
         LR    R6,R8               SAVE ADDRESS OF LAST                         
         IC    R3,DLPSLEN                                                       
         LA    R8,0(R3,R8)                                                      
         BRAS  RE,ADDANL           ADD ANALYSED OFFICE ELEMENT                  
         BRAS  RE,ADDADB           CLIENT/PRODUCT ELEMENT                       
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   0(0,R8),0(R6)                                                    
         MVI   DLPSEL,DLPSEDCQ     DEPARTMENT POSTINGS                          
         OI    DLPSTYPE,X'80'      SUBSIDIARY                                   
         MVC   DLPSDBAC,DEPACNUM                                                
         MVC   DLPSDBNM,DEPACNAM                                                
         MVC   DLPSCRAC,DSEXPNUM                                                
         MVC   DLPSCRNM,DSEXPNAM                                                
         ZAP   DLPSAMNT,EXPOST     MAKE ANALYSIS AMOUNTS SAME                   
         MVC   DLPSANAL,CLIOFC                                                  
*                                                                               
         EJECT                                                                  
*              COST ACCOUNTING POSTINGS                                         
*                                                                               
*              DEBIT DEPT. EXP. (1P) - CONTRA IS CLIENT ANALYSIS (1C)           
*                                                                               
POST43   TM    COSTSW,COSTACC                                                   
         BNO   POSTXIT                                                          
         CLI   COSTANAL,C' '                                                    
         BNH   POSTXIT                                                          
         LR    R6,R8               SAVE ADDRESS OF LAST                         
         IC    R3,DLPSLEN                                                       
         LA    R8,0(R3,R8)                                                      
         BRAS  RE,ADDANL           ADD ANALYSED OFFICE ELEMENT                  
         BRAS  RE,ADDADB           CLIENT/PRODUCT ELEMENT                       
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   0(0,R8),0(R6)                                                    
         MVI   DLPSEL,DLPSEDRQ                                                  
         OI    DLPSTYPE,X'80'      DEBIT DEPT/EXPENSE                           
         MVC   DLPSDBAC,CNEXPNUM                                                
         MVC   DLPSDBNM,CNEXPNAM                                                
         MVC   DLPSCRAC,CPJNUM                                                  
         MVC   DLPSCRNM,CPJNAME                                                 
         ZAP   DLPSAMNT,EXPOST     MAKE ANALYSIS AMOUNTS SAME                   
         MVC   DLPSANAL,CLIOFC                                                  
*                                  AS POSTING TO EXPENSE ACCOUNT                
         EJECT                                                                  
*              CREDIT CLIENT ANALYSIS (1C) - CONTRA IS COST EXP. (13)           
*                                                                               
POST45   LR    R6,R8               SAVE ADDRESS OF LAST                         
         IC    R3,DLPSLEN                                                       
         LA    R8,0(R3,R8)                                                      
         BRAS  RE,ADDANL           ADD ANALYSED OFFICE ELEMENT                  
         BRAS  RE,ADDADB           CLIENT/PRODUCT ELEMENT                       
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   0(0,R8),0(R6)                                                    
         MVI   DLPSEL,DLPSECRQ     CREDIT CLIENT                                
         MVC   DLPSANAL,CLIOFC     OFFICE CODE                                  
         MVI   DLPSDBAC+2,C'3'                                                  
         MVC   DLPSDBAC+3(12),SPACES                                            
         MVC   DLPSDBAC+3(L'COSTANAL),COSTANAL                                  
         TM    COSTSW,COSTNEW      NEW COSTING                                  
         BNO   *+10                                                             
         MVC   DLPSDBAC,EXANLNUM   13 ACCOUNT                                   
*                                                                               
POSTXIT  XIT1  REGS=(R8)           HAVE TO KEEP R8                              
         EJECT                                                                  
         LTORG                                                                  
ADDANL   NTR1  BASE=*,LABEL=*                                                   
         CLI   ANOELM,0            TEST OFFICE ELEMENT                          
         BE    ADDANLX                                                          
         LA    R1,ANOELM                                                        
         USING ANOELD,R1                                                        
         SR    RF,RF                                                            
         IC    RF,ANOLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R8),ANOEL                                                    
         LA    R8,1(RF,R8)         R8 TO NEXT AREA                              
                                                                                
ADDANLX  XIT1  REGS=(R8)                                                        
         DROP  R1                                                               
         EJECT                                                                  
*------------------------------------------------------------------*            
*        ADD FREEFORM ELEMENT FOR CLIENT & PRODUCT                              
*------------------------------------------------------------------*            
ADDADB   NTR1  BASE=*,LABEL=*                                                   
         CLI   ELDB,0              DO WE HAVE A CLIENT/PRODUCT?                 
         BE    ADDADBX             NO                                           
         LA    R1,ELDB             YES, BUILD ELEMENT WITH IT                   
         USING FFTELD,R1                                                        
         SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R8),ELDB                                                     
         LA    R8,1(RF,R8)         R8 TO NEXT AREA                              
                                                                                
ADDADBX  XIT1  REGS=(R8)                                                        
         DROP  R1                                                               
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
         ZAP   AMTBLK+2(6),GRAMNT  1 AMOUNT                                     
         LA    RF,7                                                             
         LA    RE,AMTBLK+8         CLEAR THE REST                               
CTAX13   MVC   0(2,RE),SPACES                                                   
         ZAP   2(6,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   RF,CTAX13                                                        
*                                                                               
         MVC   CTXACC(1),COMPANY                                                
         MVC   CTXOFF,CRDOFC                                                    
         L     R2,AGORNH                                                        
         MVC   CTXGORN,8(R2)                                                    
         L     R2,APROVH                                                        
         MVC   CTXPROV,8(R2)                                                    
         MVC   CTXDATE,SAVEDATE                                                 
*        MVC   CTXACC,JOBNUM                                                    
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
CTAX15   MVC   CTXCNTRA,VENDNUM     VENDOR                                      
         MVC   CTXCNTRN,VENDNAME                                                
         OC    VENDNUM,VENDNUM                                                  
         BNZ   CTAX50                                                           
         MVC   CTXCNTRA,CRTNUM                                                  
         MVC   CTXCNTRN,CRTNAM                                                  
*                                                                               
CTAX50   LA    R3,X'41'            CTAX SCREEN                                  
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(X'15',CTXDATA),(R9),AMTBLK                            
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
         L     R2,ATYPEH                                                        
         MVC   8(1,R2),CTXGSTT                                                  
         OI    6(R2),X'80'                                                      
         L     R2,ATYPNH                                                        
         MVC   8(21,R2),CTXGSTTN                                                
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
         L     R2,AGSTXH                                                        
         XC    8(L'PITGSTX,R2),8(R2)                                            
         CLI   GSTSW,C'Y'                                                       
         BNE   CTAX69                                                           
         OC    CTXGST,CTXGST       DO WE HAVE GST?                              
         BZ    CTAX70                                                           
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
         LTORG                                                                  
         EJECT                                                                  
OFDPV    DS    0D                  VALIDATE OFFICE / DEPARTMENT FIELDS          
         NMOD1 0,**OFV**                                                        
         L     RC,0(R1)                                                         
         XC    ANOELM,ANOELM       ANALYZED OFFICE ELEMENT                      
         MVC   FVMSGNO,=Y(AE$MISIF)                                             
         LA    R5,COMPEL                                                        
         USING ACCOMPD,R5                                                       
         MVI   OFFSW,C'N'                                                       
         TM    COMPSTAT,X'20'      TEST COMPANY STATUS FOR                      
         BZ    OFDPV03             MANDATORY OFFICE CODE                        
         L     R2,AFOFFH                                                        
         CLI   5(R2),0                                                          
         BE    OFDPVXIT            MISSING OFFICE CODE                          
         MVI   OFFSW,C'Y'                                                       
         MVC   FINOFC,8(R2)        FINANCIAL OFFICE                             
         OC    FINOFC,SPACES                                                    
         GOTO1 AVALOFFC,DMCB,(X'80',FINOFC)                                     
         BNE   OFDPVXIT            INVALID OFFICE                               
*                                                                               
OFDPV03  L     R2,AFDPTH                                                        
         MVC   FVMSGNO,=Y(AE$MISIF)                                             
         CLI   5(R2),0             IS THERE DEPT. INPUT                         
         BNE   OFDPV05             YES, VALIDATE IT                             
         TM    ANLSW,ANLDPT        DEPT=Y, MUST HAVE A DEPARTMENT               
         BO    OFDPV04                                                          
         TM    ANLSW,ANLSTF        STAFF=Y, MUST HAVE DEPARTMENT                
         BZ    OFDPV09                                                          
*                                                                               
         MVC   KEY+1(14),SPACES    CHECKING IF 2P HAVE 1 LEVEL                  
         MVC   KEY+1(2),=C'2P'                                                  
         BAS   RE,READ                                                          
         L     R4,AIOAREA1                                                      
         MVI   ELCODE,ACHRELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACHEIRD,R4                                                       
         LA    RE,ACHRLEVA                                                      
         CLI   0(RE),12            ONE LEVEL?                                   
         BE    OFDPV09             DEPT NOT REQUIRED                            
*                                                                               
OFDPV04  OC    FINDPT,FINDPT       DO I HAVE DEPT. FROM EXP. ACCOUNT            
         BNZ   OFDPV09             EXP. DEPT FROM EXP. ACCT                     
         B     OFDPVXIT            IF NOT IT'S AN ERROR - MISSING INPUT         
*                                                                               
OFDPV05  MVI   ERRNUM,OK           DEPT HAS BEEN INPUT                          
         TM    ANLSW,ANLSTF+ANLDPT CAN ONLY HAVE DEPARTMENT IF REQUIRED         
         BNZ   OFDPV07             FOR DEPT=Y OR                                
         MVC   FVMSGNO,=Y(AE$ANFDP)                                             
         B     OFDPVXIT            ERROR ACCOUNT NOT FLAGGED FOR DEPT.          
*                                                                               
OFDPV07  MVC   FINDPT,8(R2)        EXPENSE DEPT                                 
         OC    FINDPT,SPACES                                                    
*                                                                               
OFDPV09  MVC   ANLOFC,FINOFC       SET ANALYSIS OFFICE FROM FINANCIAL           
         MVC   CLIOFC,FINOFC       DEFAULT IS FINANCIAL                         
         MVC   SPCDPT,FINDPT       AND SPECIAL FROM FINANCIAL DEPT              
         L     R2,APOFFH           TEST ANALYSIS(STAFF) OFFICE                  
         CLI   5(R2),0                                                          
         BE    OFDPV11                                                          
         MVC   ANLOFC,8(R2)        SAVE ANALYSIS OFFICE                         
         OC    ANLOFC,SPACES                                                    
         GOTO1 AVALOFFC,DMCB,(X'80',ANLOFC)                                     
         BNE   OFDPVXIT            INVALID OFFICE                               
         CLC   ANLOFC,FINOFC       ANALYSIS SAME AS FINANCIAL                   
         BE    OFDPV11             NO NEED TO ADD ELEMENT                       
         LA    R1,ANOELM           BUILD ANALYSED OFFICE ELEMENT                
         USING ANOELD,R1                                                        
         MVI   ANOEL,ANOELQ                                                     
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTPER     PERSON OFFICE                                
         MVC   ANOOFFC,ANLOFC                                                   
*                                                                               
OFDPV11  MVC   CRDOFC,FINOFC                                                    
         L     R2,ACOFFH           CREDIT OFFICE                                
         CLI   5(R2),0                                                          
         BE    OFDPV13                                                          
         MVC   CRDOFC,8(R2)        SAVE CREDIT OFFICE                           
         OC    CRDOFC,SPACES                                                    
         GOTO1 AVALOFFC,DMCB,(X'80',CRDOFC)                                     
         BNE   OFDPVXIT            INVALID OFFICE                               
*                                                                               
OFDPV13  L     R2,APDPTH           TEST ANALYSIS - DEPARTMENT                   
         CLI   5(R2),0                                                          
         BE    OFDPV17                                                          
         TM    ANLSW,ANLSTF+ANLDPT STAFF=Y OR DEPT                              
         BNZ   OFDPV15                                                          
         MVC   FVMSGNO,=Y(AE$ANFST)                                             
         B     OFDPVXIT            ERROR ACCOUNT NOT FLAGGED FOR STAFF          
*                                                                               
OFDPV15  MVC   SPCDPT,8(R2)        SPECIAL DEPARTMENT                           
         OC    SPCDPT,SPACES                                                    
*                                                                               
OFDPV17  MVI   ERRNUM,OK                                                        
OFDPVXIT XIT1  REGS=(R2)                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              WRITE A PRINT QUEUE ENTRY                                        
WRITCHK  DS    0D                                                               
         NMOD1 SPOOLEND-SPOOLD,*WRTCK*,CLEAR=YES                                
         LR    R6,RC                                                            
         L     RC,0(R1)                                                         
         USING SPOOLD,R6                                                        
         XC    VPRINT,VPRINT                                                    
         XC    ABOX,ABOX                                                        
         MVC   SPOOLDM,DATAMGR     INITIALIZE SPOOL DSECT                       
         MVC   SPOOLBUF,ATIA                                                    
         MVC   RCDATCON,DATCON                                                  
         L     RF,=A(T61B15SP)                                                  
         A     RF,PRELO                                                         
         ST    RF,SPECS                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   SPOOLKEY+12(3),=C'CHE'                                           
         MVC   SPOOLID,=C'CHE'                                                  
         MVC   SPOOLKEY+1(11),=CL11'EXPENSE'                                    
         MVI   SPMODE,0                                                         
         BAS   RE,MYPRT                                                         
         MVC   SPOOLRPN,SPOOLKEY+19                                             
         L     R2,AVNDNH                                                        
         MVC   FLD,8(R2)           EXTRACT VENDOR NAME DATA                     
         MVC   FLD+30(3),=X'407B7E'                                             
         LA    R3,FLD+33                                                        
         EDIT  (B2,SPOOLRPN),(3,0(R3)),FILL=0                                   
         BAS   RE,MOVEFLD                                                       
         MVI   MAXLINES,80                                                      
*                                                                               
         MVI   FORCEHED,C'Y'       NOW PRINT THE CHECK                          
         MVC   P+1(12),DBTNUM+3                                                 
         GOTO1 CHOPPER,DMCB,(36,DBTNAM),(22,P+14),(C'P',2)                      
         MVI   LINES,C'1'                                                       
         CLI   DMCB+11,1                                                        
         BE    *+8                                                              
         MVI   LINES,C'2'                                                       
         L     R2,ANARH                                                         
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         GOTO1 (RF),(R1),((R3),8(R2)),(25,P+38),(C'P',2)                        
         CLI   DMCB+11,2                                                        
         BNE   *+8                                                              
         MVI   LINES,C'2'                                                       
         EDIT  GRAMNT,(12,P+67),2                                               
         BAS   RE,MYPRT                                                         
*                                                                               
         LA    RF,62                                                            
         CLI   LINES,C'2'                                                       
         BNE   *+6                                                              
         BCTR  RF,0                                                             
         STC   RF,SPACING          SKIP DOWN TO CHECK PORTION                   
         BAS   RE,MYPRT                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(1,SAVEDATE),(8,P+6)                                 
         GOTO1 CHOPPER,DMCB,(36,VENDNAME),(28,P+39),(C'P',2)                    
         EDIT  GRAMNT,(13,P+68),2,COMMAS=YES,FILL=*                             
         BAS   RE,MYPRT                                                         
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),VENDNUM                                                  
         MVC   RKEY,KEY                                                         
         GOTO1 AREAD,AIOAREA1                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,AIOAREA1         MOVE RECORD INTO OVERLAY IOAREA              
         LA    RF,KEY                                                           
         LA    R1,1000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         LA    RF,IOAREA                                                        
*                                                                               
WRIT10   CLI   0(RF),0                                                          
         BE    WRIT20                                                           
         CLI   0(RF),X'22'                                                      
         BE    WRIT12                                                           
         SR    RE,RE                                                            
         IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     WRIT10                                                           
*                                                                               
         USING ACADDD,RF                                                        
WRIT12   IC    RE,ACADLNES                                                      
         LA    R1,P+39                                                          
         LA    RF,ACADADD                                                       
*                                                                               
WRIT14   MVC   0(26,R1),0(RF)                                                   
         LA    R1,132(R1)                                                       
         LA    RF,26(RF)                                                        
         BCT   RE,WRIT14                                                        
         BAS   RE,MYPRT                                                         
*                                                                               
WRIT20   MVI   SPMODE,X'FF'                                                     
         BAS   RE,MYPRT                                                         
         B     WRITX                                                            
         EJECT                                                                  
*              LINKAGE TO SPOOL                                                 
*                                                                               
MYPRT    NTR1                                                                   
         MVC   DMCB+4(4),=X'D9000A0C'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R6)                                                   
WRITX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
T61B15SP CSECT                                                                  
         SSPEC H1,2,C'CATEGORY CODE/NAME'                                       
         SSPEC H1,39,C'DESCRIPTION'                                             
         SSPEC H1,74,C'AMOUNT'                                                  
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
       ++INCLUDE ACBATE9D                                                       
         ORG   TWAHOLE                                                          
ORDAMT   DS    PL6                                                              
LASTDOC  DS    CL6                                                              
AMTBLK   DS    CL64                                                             
GSTSW    DS    CL1                 GST/PST APPLICABLE                           
CTAXBEF  DS    CL1                 FLAG USING CTAX                              
KEEPPROV DS    CL2                 SAVE PROVINCE                                
TMPNET   DS    PL6                                                              
TMPGRS   DS    PL6                                                              
TMPGST   DS    PL6                                                              
TMPPST   DS    PL6                                                              
XTRAELM  DS    CL71                                                             
CLEAROK  DS    CL1                 FLAG FOR CLEAR GST/PST FIELD                 
USERPROV DS    CL1                 FLAG, USER ENTERED PROVINCE                  
USERPST  DS    CL1                 FLAG, USER ENTERED PST TYPE                  
*                                                                               
         EJECT                                                                  
         ORG   CONTABH                                                          
       ++INCLUDE ACBATC8D                                                       
         EJECT                                                                  
PROGD    DSECT                                                                  
PRELO    DS    F                                                                
SAVERE   DS    A                                                                
OFFSW    DS    CL1                                                              
ANLSW    DS    XL1                 ANALYSIS                                     
ANLSTF   EQU   X'80'               STAFF ANALYSIS                               
ANLDPT   EQU   X'40'               DEPARTMNET ANALYSIS                          
*                                                                               
COSTSW   DS    XL1                                                              
COSTACC  EQU   X'80'               COST ACCOUNTING                              
COSTNEW  EQU   X'40'               NEW COSTING                                  
*                                                                               
SRSW     DS    XL1                                                              
SRDR     EQU   X'80'               DEBIT TO SR                                  
SRCR     EQU   X'40'               CREDIT TO SR                                 
*                                                                               
V29SW    DS    CL1                                                              
CONSULT  DS    CL1                                                              
*                                                                               
CRTNUM   DS    CL15                CREDIT ACCOUNT                               
CRTNAM   DS    CL36                                                             
*                                                                               
DBTNUM   DS    CL15                DEBIT ACCOUNT                                
DBTNAM   DS    CL36                                                             
*                                                                               
CPJNUM   DS    CL15                CLIENT ANALYSIS(1C)                          
CPJNAME  DS    CL36                                                             
CNEXPNUM DS    CL15                DEPT/EXPENSES(1P)                            
CNEXPNAM DS    CL36                                                             
DSEXPNUM DS    CL15                DEPT SUSPENSE(28)                            
DSEXPNAM DS    CL36                                                             
PSCLINUM DS    CL15                CLIENT/STAFF(29)                             
PSCLINAM DS    CL36                                                             
DEPACNUM DS    CL15                DEPT ANALYSIS(2D)                            
DEPACNAM DS    CL36                                                             
PERSNUM  DS    CL15                STAFF/CLIENT(2P)                             
PERSNAME DS    CL36                                                             
EXANLNUM DS    CL15                EXPENSE ANALYSIS(13) - NEW COST              
*                                                                               
VENDNUM  DS    CL15                VENDOR(SX)                                   
VENDNAME DS    CL36                                                             
VENDTYPE DS    CL1                                                              
VENDPSTT DS    CL1                                                              
VENDPROV DS    CL2                 VENDOR PROVINCE, VNDR REC (LFM)              
*                                                                               
CONTROL  DS    CL15                CONTROL (27)                                 
CONTROLN DS    CL36                                                             
V2CNUM   DS    CL15                CONSULTANT (2C)                              
V2CNAM   DS    CL36                                                             
SAVNUM   DS    CL15                CONTRA OF ADVANCE LEDGER                     
SAVNAM   DS    CL36                                                             
CDACC    DS    CL15                C.D. INCOME ACCOUNT                          
CDNAME   DS    CL36                                                             
CLINUM   DS    CL15                CLIENT (SJ)                                  
CLINAME  DS    CL36                CLIENT NAME                                  
PRONUM   DS    CL15                PRODUCT (SJ)                                 
PRONAME  DS    CL36                PRODUCT NAME                                 
*                                                                               
COSTANAL DS    CL5                 COST CODE FROM SE ACCOUNT                    
DISC     DS    CL3                                                              
CDAMNT   DS    PL6                                                              
GRAMNT   DS    PL6                                                              
NTAMNT   DS    PL6                                                              
EXPOST   DS    PL6                 EXPENSE POSTING                              
GST      DS    PL6                 GST AMOUNT                                   
PST      DS    PL6                 PST AMOUNT                                   
TOTNET   DS    PL6                 NET AFTER CALCULATIONS                       
TOTGRS   DS    PL6                                                              
SAVEDATE DS    CL3                                                              
SAVDOC   DS    CL6                                                              
BYTE     DS    CL1                                                              
SVSTAT   DS    CL1                                                              
LINES    DS    CL1                                                              
*SRSAVE   DS    CL12                                                            
LEV2P    DS    CL1                                                              
ELCODE   DS    CL1                                                              
*                                                                               
FINOFC   DS    CL2                 FINANCIAL OFFICE                             
FINDPT   DS    CL3                 FINANCIAL DEPT                               
*                                                                               
CRDOFC   DS    CL2                 CREDIT OFFICE                                
*                                                                               
ANLOFC   DS    CL2                 ANALYSIS OFFICE                              
SPCDPT   DS    CL3                 SPECIAL DEPARTMENT                           
*                                                                               
CLIOFC   DS    CL2                 CLIENT OFFICE                                
*                                                                               
GORN     DS    CL1                                                              
GSTINP   DS    CL1                                                              
*                                                                               
ATYPEH   DS    A                                                                
ATYPNH   DS    A                                                                
AGORNH   DS    A                                                                
AGSTXH   DS    A                                                                
AGAMTH   DS    A                                                                
APROVH   DS    A                                                                
*                                                                               
AFOFFH   DS    A                                                                
AFDPTH   DS    A                                                                
AFDPNH   DS    A                                                                
ACOFFH   DS    A                                                                
APOFFH   DS    A                                                                
APDPTH   DS    A                                                                
APDPNH   DS    A                                                                
ASTFH    DS    A                                                                
ASTFNH   DS    A                                                                
*                                                                               
ACLIH    DS    A                                                                
ACLINH   DS    A                                                                
APROH    DS    A                                                                
APRONH   DS    A                                                                
*                                                                               
AVNDH    DS    A                                                                
AVNDNH   DS    A                                                                
ASRCH    DS    A                                                                
ANARH    DS    A                                                                
ATABH    DS    A                                                                
*                                                                               
ADEBIT   DS    A                                                                
*                                                                               
DBTLEN   DS    CL1                                                              
OVPOS    DS    CL1                                                              
OVCODE   DS    CL3                                                              
CATBLK   DS    CL(CATLNQ)          CATEGORY BLOCK                               
ANOELM   DS    XL(ANOLNQ)          ANALYZED OFFICE ELEMENT                      
ELDB     DS    XL(FFTLN1Q+L'FFTDLEN+L'FFTCLPRA)                                 
       ++INCLUDE ACBATCTAX                                                      
TMPMODE  DS    CL1                                                              
KEY      DS    CL49                                                             
IOAREA   DS    4000C                                                            
*                                                                               
PROGDX   DS    0C                                                               
         EJECT                                                                  
SPOOLD   DSECT                     ONLINE PRINT CONTROL DSECT                   
SPLAREA  DS    CL4000                                                           
         ORG   SPLAREA                                                          
         DS    D                                                                
LINE     DS    XL1                 PRESET TO 99                                 
ALLOWLIN DS    XL1                 ENSURE THAT N LINES REMAIN ON PAGE           
MAXLINES DS    XL1                 PRESET TO 60                                 
SPACING  DS    XL1                                                              
HEADHOOK DS    V                   USER SUPPLIED A(HEADLINE ROUTINE)            
MIDHOOK  DS    V                   USER SUPPLIED A(MIDLINE ROUTINE)             
CLEARHED DS    CL1                 OPTION TO CLEAR HEADLINES DEFAULT=Y          
FORCEHED DS    CL1                                                              
FORCEMID DS    CL1                                                              
FORCEFUT DS    CL1                                                              
FORCECLR DS    CL1                                                              
SKIPSPEC DS    CL1                                                              
PAGE     DS    XL2                                                              
SUBPAGE  DS    XL2                                                              
SPOOLIND DS    XL1                                                              
SPNSPACE EQU   X'80'               NO SPACE AFTER HEADLINES                     
SPNGAPS  EQU   SPNSPACE+X'20'      NO GAPS BETEEN HEADS AND MIDS                
SPUINIT  EQU   X'40'               ALLOW USER INITIALIZED FIELDS                
SPHHOOK  EQU   X'04'               APPLICATION CALLED WITH HEADHOOK             
SPMHOOK  EQU   X'02'               APPLICATION CALLED WITH MIDHOOK              
SPFHOOK  EQU   X'01'               APPLICATION CALLED WITH FOOTHOOK             
*                                                                               
         DS    XL1                                                              
         SPACE 1                                                                
SPOOLKEY DS    CL48                                                             
SPOOLPAG DS    H                                                                
SPOOLLIN DS    H                                                                
SPOOLDM  DS    A                                                                
SPOOLBUF DS    A                                                                
SPECS    DS    A                                                                
SPOOLID  DS    CL3                                                              
SPOOLRPN DS    CL2                                                              
SPMODE   DS    CL1                                                              
ACTPAGES DS    H                                                                
MAXPAGES DS    H                                                                
RCDATE   DS    CL8                                                              
RCPROG   DS    CL4                                                              
RCSUBPRG DS    CL1                                                              
SPCONSYS DS    CL1                                                              
RCDATCON DS    A                                                                
VPRINT   DS    A                                                                
BUFFALO  DS    V                                                                
SORTER   DS    V                                                                
WORKER   DS    V                                                                
ABOX     DS    V                                                                
FOOTLNS  DS    X                   NUMBER OF FOOTLINES REQUIRED                 
         ORG   FOOTLNS                                                          
FOOTHOOK DS    V                                                                
SPOOLQLK DS    A                   A(EXTENDED SPOOL KEY) (128 BYTES)            
*                                  FIRST BYTE OF SPOOLQLK MUST BE 0             
RCCOMFAC DS    V                                                                
SPOTPROF DS    CL16                                                             
         DS    CL16                                                             
         SPACE 1                                                                
         DS    D                   HEADLINES                                    
HEAD1    DS    CL132                                                            
HEAD2    DS    CL132                                                            
HEAD3    DS    CL132                                                            
HEAD4    DS    CL132                                                            
HEAD5    DS    CL132                                                            
HEAD6    DS    CL132                                                            
HEAD7    DS    CL132                                                            
HEAD8    DS    CL132                                                            
HEAD9    DS    CL132                                                            
HEAD10   DS    CL132                                                            
HEAD11   DS    CL132                                                            
HEAD12   DS    CL132                                                            
HEAD13   DS    CL132                                                            
HEAD14   DS    CL132                                                            
H1       EQU   HEAD1                                                            
H2       EQU   HEAD2                                                            
H3       EQU   HEAD3                                                            
H4       EQU   HEAD4                                                            
H5       EQU   HEAD5                                                            
H6       EQU   HEAD6                                                            
H7       EQU   HEAD7                                                            
H8       EQU   HEAD8                                                            
H9       EQU   HEAD9                                                            
H10      EQU   HEAD10                                                           
H11      EQU   HEAD11                                                           
H12      EQU   HEAD12                                                           
H13      EQU   HEAD13                                                           
H14      EQU   HEAD14                                                           
         SPACE 1                                                                
         DS    CL8                 MID LINES                                    
MID1     DS    CL132                                                            
MID2     DS    CL132                                                            
         DS    CL8                 PRINT LINES                                  
P        DS    0CL132                                                           
P1       DS    CL132                                                            
P2       DS    CL132                                                            
P3       DS    CL132                                                            
P4       DS    CL132                                                            
         DS    CL132               SPACES FIELD                                 
MONTHS   DS    CL36                MONTH TABLE (JAN-DEC)                        
DAYTABL  DS    CL21                DAY TABLE (MON-SUN)                          
USERNAME DS    CL33                                                             
USERADDR DS    CL33                                                             
USERQSTR DS    CL6                                                              
USERQEND DS    CL6                                                              
USERPROF DS    CL16                                                             
USERLANG DS    XL1                                                              
         DS    CL1                                                              
SPOOLEND DS    D                                                                
         EJECT                                                                  
*        ACCATCALLD                                                             
*        ACGENBOTH                                                              
*        ACGENDAY                                                               
*        DDFLDIND                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'082ACBAT15   02/14/03'                                      
         END                                                                    
