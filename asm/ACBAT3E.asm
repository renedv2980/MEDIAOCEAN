*          DATA SET ACBAT3E    AT LEVEL 072 AS OF 10/16/18                      
*PHASE T61B3EA,+0                                                               
*                                                                               
***********************************************************************         
*        HISTORY                                                                
***********************************************************************         
* ID   LVL DATE    JIRA         DESCRIPTION                                     
* **** *** ******* ************ ***************************************         
* CPAT 072 20JUL18 <SPEC-24508> RESTRICTING UNIT PRICE TO $99,999.99  *         
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'T61B3E - BATCH TYPE 62 -UNIT PRICING'                           
T61B3E   CSECT                                                                  
         PRINT NOGEN                                                            
***********************************************************************         
*        TYPE 62, X'3E' BATCHING BY SCHEME USES SCREEN X'F2'          *         
***********************************************************************         
         NMOD1 PROGDX-PROGD,*BAT3E*,R8,R7,CLEAR=YES,RR=R2                       
         USING PROGD,RC                                                         
         L     R9,4(R1)                                                         
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         ST    R2,RELO2                                                         
         BAS   RE,READSJ           GET LEDGER DATA                              
         EJECT                                                                  
***********************************************************************         
*              HANDLE BATCH GENERATE INITIALIZATION                   *         
***********************************************************************         
*                                                                               
BGEN     DS    0H                                                               
         CLI   CSOMODE,CSOMPLIN    ACTION --> BATCH/GENERATE                    
         BNE   BGENX                                                            
*                                                                               
         USING LIND,R4                                                          
         LA    R4,UNTUNTH                                                       
         MVI   LINE,1                                                           
*                                                                               
BGEN20   CLC   LINE,CSOLINE        CLEAR LINE (# IN CSOLINE)                    
         BNE   *+8                                                              
         BAS   RE,PROTECT                                                       
         ZIC   R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,LINE                                                          
         LA    R4,LINLEN(R4)       NEXT LINE                                    
         LA    R2,UNTENDH                                                       
         CR    R4,R2               BOTTOM OF SCREEN YET?                        
         BNH   BGEN20                                                           
         B     EXIT1               EXIT W/O SETTING CURSOR POSTN                
*                                                                               
BGENX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              SET STATUS FIELD                                       *         
***********************************************************************         
*                                                                               
         USING CPYELD,R4                                                        
STAT     LA    R4,BCCPYEL                                                       
         TM    CPYSTAT1,CPYSCOST                                                
         BZ    *+8                                                              
         OI    STATUS,COST         INDICATE DO COST POSTINGS                    
*                                                                               
         MVI   SCRIPTEX,C'N'                                                    
         L     R1,BCAUTL                                                        
         TM    TSTAT6-UTLD(R1),TST6SCRP     RUNNING UNDER SCRIPT                
         BZ    *+8                          TESTING                             
         MVI   SCRIPTEX,C'Y'                                                    
*                                                                               
STATX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              HANDLE ITEM LIST/CHA/DEL OR ITEM INPUT                 *         
***********************************************************************         
*                                                                               
         DS    0H                                                               
         ZAP   TAXTOT,=P'0'                                                     
         CLI   CSACT,ACTINP        IS ACTION ITEM INPUT?                        
         BNE   INIT20              NO                                           
*                                                                               
         CLI   TWAMODE,2           YES, INPUT TAX?                              
         BE    CALLTAX             YES, GO TO OVERLAY                           
         CLI   BCPFKEY,9           TAX REQUESTED?                               
         BE    CALLTAX             YES                                          
*                                                                               
         MVI   CSOLINE,0           NO, INITIALIZE LINE # FOR ADDITE             
         B     INITX                                                            
*                                                                               
INIT20   CLI   TWASCRN,BTS62U      TAX SCREEN ACTIVE?                           
         BE    CALLTAX             YES, GO TO OVERLAY                           
*                                                                               
         USING LIND,R4                                                          
         LA    R4,UNTUNTH                                                       
         MVI   LINE,1                                                           
*                                                                               
INIT40   CLC   LINE,CSOLINE        LINE TO BE ADDED, CHANGED, ETC?              
         BNE   *+12                NO                                           
         BAS   RE,TRANSMIT         YES, TRANSMIT ITEM TO BE CHANGED             
         B     *+8                                                              
         BAS   RE,PROTECT          CLEAR & PROTECT ALL OTHER ITEMS              
         ZIC   R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,LINE                                                          
         LA    R4,LINLEN(R4)       NEXT LINE                                    
         LA    R2,UNTENDH          GET ADDRESS OF END OF SCREEN                 
         CR    R4,R2               ARE WE AT THE BOTTOM?                        
         BNH   INIT40              NO, KEEP LOOKING                             
*                                                                               
INITX    DS    0H                                                               
         B     CLIVAL              VALIDATE CLIENT                              
         EJECT                                                                  
***********************************************************************         
*              TRANSMIT A LINE DURING ITEM/CHANGE                     *         
***********************************************************************         
*                                                                               
*              R4 = A(START OF LINE)                                            
*                                                                               
         USING LIND,R4                                                          
TRANSMIT NTR1                                                                   
         LA    R0,LINNUMQ          #FIELDS/LINE                                 
*                                                                               
TRAN20   NI    4(R4),X'FF'-X'20'   MARK AS NOT PREVIOUSLY VALIDATED             
         OI    6(R4),X'80'         TRANSMIT                                     
         SR    R1,R1                                                            
         IC    R1,0(R4)            BUMP TO NEXT FIELD                           
         AR    R4,R1                                                            
         BCT   R0,TRAN20                                                        
*                                                                               
TRANSX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CLEAR & PROTECT LINE                                   *         
***********************************************************************         
*                                                                               
*              R4 = A(START OF LINE)                                            
*                                                                               
         USING LIND,R4                                                          
PROTECT  NTR1                                                                   
         LA    R0,LINNUMQ          #FIELDS/LINE                                 
*                                                                               
PROT20   TWAXC 0(R4),0(R4),PROT=Y                                               
         CLI   CSOMODE,CSOMPLIN    BATCH GENERATE                               
         BE    *+8                                                              
         OI    1(R4),X'20'         PROTECT FIELD                                
         OI    4(R4),X'20'         MARK PREVIOUSLY VALIDATED                    
         MVI   5(R4),0             CLEAR INPUT LENGTH                           
         OI    6(R4),X'80'         TRANSMIT                                     
         SR    R1,R1                                                            
         IC    R1,0(R4)            BUMP TO NEXT FIELD                           
         AR    R4,R1                                                            
         BCT   R0,PROT20                                                        
*                                                                               
PROTX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE CLIENT                                        *         
***********************************************************************         
*                                                                               
CLIVAL   DS    0H                                                               
         LA    R2,UNTCLIH          CLIENT HEADER                                
         MVI   FVMINL,1            SET MINIMUM LENGTH                           
         MVI   BOFLAG1,ACIPRCLI    INDICATE - VALIDATE CLIENT                   
         XC    PSCLICOD,PSCLICOD   CLEAR OUT OLD CLIENT                         
         GOTO1 AVALCPJ,UNTCLIH                                                  
         BNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BO    LOCKER              YES                                          
*                                                                               
         MVC   UNTCLIN,ACNAME      PRINT THE NAME                               
         OI    UNTCLINH+6,X'80'                                                 
*                                                                               
         TM    UNTCLIH+4,X'20'     HAS CLIENT BEEN CHANGED?                     
         BO    *+12                NO                                           
         OI    STATUS,RRACCT       YES, RE-READ PRICES/INCOME ACCOUNT           
         OI    UNTCLIH+4,X'20'                                                  
*                                                                               
         MVC   SVCLI,FVIFLD        SAVE ACCOUNT FOR POSTINGS/PRICES             
*                                                                               
CLIX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE PRODUCT                                       *         
***********************************************************************         
*                                                                               
PROVAL   LA    R2,UNTPROH          PRODUCT HEADER                               
         MVI   FVMINL,1            SET MINIMUM LENGTH                           
         MVI   BOFLAG1,ACIPRPRO    INDICATE - VALIDATE PRODUCT                  
         XC    PSPROCOD,PSPROCOD   CLEAR OUT OLD PRODUCT                        
         GOTO1 AVALCPJ,UNTPROH                                                  
         BNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BO    LOCKER              YES                                          
*                                                                               
         MVC   UNTPRON,ACNAME      PRINT THE NAME                               
         OI    UNTPRONH+6,X'80'                                                 
*                                                                               
         TM    UNTPROH+4,X'20'     HAS PRODUCT BEEN CHANGED?                    
         BO    *+12                NO                                           
         OI    STATUS,RRACCT       YES, RE-READ PRICES/INCOME ACCOUNT           
         OI    UNTPROH+4,X'20'                                                  
*                                                                               
         MVC   SVPRO,FVIFLD        SAVE ACCOUNT FOR POSTINGS/PRICES             
*                                                                               
PROX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE JOB                                           *         
***********************************************************************         
*                                                                               
JOBVAL   LA    R2,UNTJOBH          JOB HEADER                                   
         MVI   FVMINL,1            SET MINIMUM LENGTH                           
         MVI   BOFLAG1,ACIPRJOB    INDICATE - VALIDATE JOB                      
         XC    PSJOBCOD,PSJOBCOD   CLEAR OUT OLD JOB                            
         GOTO1 AVALCPJ,UNTJOBH                                                  
         BNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BO    LOCKER              YES                                          
         TM    ACBSTAT,ACBSCLSE    ACCOUNT CLOSED?                              
         BO    CLOSER              YES                                          
*                                                                               
         MVC   UNTJOBN,ACNAME      PRINT THE NAME                               
         OI    UNTJOBNH+6,X'80'                                                 
*                                                                               
         USING ACTRECD,R5                                                       
         L     R5,AIO1                                                          
         GOTO1 AGETJOB,BOPARM,(R5)                                              
*                                                                               
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB?                            
         BO    XJOBER              YES                                          
*                                                                               
         TM    UNTJOBH+4,X'20'     HAS JOB BEEN CHANGED?                        
         BO    *+12                NO                                           
         OI    STATUS,RRACCT       YES, RE-READ PRICES/INCOME ACCOUNT           
         OI    UNTJOBH+4,X'20'                                                  
*                                                                               
         MVC   SVACCT,ACCODE       SAVE CUL, ACCOUNT AND NAME                   
         MVC   SVACCN,ACNAME        FOR POSTINGS/PRICES                         
         MVC   SVJOB,FVIFLD        SAVE JOB NUMBER                              
*                                                                               
JOBX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE DATE                                          *         
***********************************************************************         
*                                                                               
DATEVAL  DS    0H                                                               
         LA    R2,UNTDATH          DATE HEADER                                  
         GOTO1 AFVAL,UNTDATH                                                    
         BNL   DATE20                                                           
         GOTO1 VDATCON,BOPARM,(5,0),(5,BOWORK1)                                 
         MVC   UNTDAT,BOWORK1                                                   
         OI    UNTDATH+6,X'80'                                                  
*                                                                               
DATE20   GOTO1 AVALDAT,UNTDATH                                                  
         BNE   EXIT                                                             
         GOTO1 DATECHK,BOPARM,BCWORK+2                                          
         CLI   BOPARM,X'FF'                                                     
         BE    EXIT                                                             
*                                                                               
         TM    UNTDATH+4,X'20'     HAS DATE BEEN CHANGED?                       
         BO    *+12                NO                                           
         OI    STATUS,RRDATE       YES, RE-READ PRICES                          
         OI    UNTDATH+4,X'20'                                                  
*                                                                               
         MVC   SVDATE,BCWORK+2     SAVE DATE FOR POSTINGS/PRICES                
*                                                                               
DATEX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE REFERENCE NUMBER                              *         
***********************************************************************         
*                                                                               
REFVAL   LA    R2,UNTREFH          REFERENCE HEADER                             
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,UNTREFH       INPUT IS REQUIRED                            
         BNE   EXIT                                                             
*                                                                               
         MVC   SVREFER,FVIFLD      SAVE REFERENCE FOR POSTINGS                  
*                                                                               
REFX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE COMMISSION                                    *         
***********************************************************************         
*                                                                               
COMVAL   LA    R2,UNTCOMH          COMMISSION HEADER                            
         GOTO1 AFVAL,UNTCOMH       WAS IT OVERRIDDEN?                           
         BNL   COM20               YES                                          
         MVI   UNTCOM,C'N'         NO, FORCE IT TO BE NO                        
         OI    UNTCOMH+6,X'80'                                                  
*                                                                               
COM20    CLI   UNTCOM,C'Y'         MUST BE YES OR NO                            
         BE    COM40                                                            
         CLI   UNTCOM,C'N'                                                      
         BNE   COMERR                                                           
*                                                                               
COM40    MVC   SVCOMM,UNTCOM       SAVE COMMISSION FOR POSTINGS                 
*                                                                               
COMX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              PROCESS NARRATIVE                                      *         
***********************************************************************         
*                                                                               
NARVAL   DS    0H                                                               
         LA    R2,UNTNAR1H         NARRATIVE HEADER                             
         GOTO1 AVALNAR,BOPARM,(2,(R2)),SVNARR                                   
         STC   R6,SVNARLEN                                                      
*                                                                               
NARX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE 1C COST ACCOUNT                               *         
***********************************************************************         
*                                                                               
AC1CVAL  DS    0H                                                               
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKEY(L'PPRCOST),PSCOMPPR+(PPRCOST-PPRELD)                      
*                                                                               
         GOTO1 AGETACT,0           VALIDATE 1C ACCT                             
         BNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BO    LOCKER              YES                                          
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BZ    POSTER              NO                                           
*                                                                               
         MVC   SV1CACT,ACCODE      SAVE ACCOUNT AND NAME                        
         MVC   SV1CNAME,ACNAME       FOR POSTINGS                               
*                                                                               
AC1CX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE INPUT LINE                                    *         
***********************************************************************         
*                                                                               
VALIDATE DS    0H                                                               
         LA    R6,UNTLSTH          CLEAR OUT AMOUNTS                            
         LA    R4,UNTUNTH                                                       
         CLI   CSACT,ACTDSP        ARE WE DISPLAYING RECORD?                    
         BE    FSTLINE             YES, DON'T CLEAR AMOUNT THEN                 
*                                                                               
         USING LIND,R4                                                          
CLEAR    TWAXC LINTOTH,LINTOTH,PROT=Y                                           
         TM    STATUS,RRACCT+RRDATE   REFRESH THE TX, PRICE AND INCOME?         
         BZ    CLEAR60                NO                                        
         CLI   LINPRC,C'*'            YES, BUT NOT IF TYPED IN                  
         BNE   CLEAR20                                                          
         TWAXC LINPRCH,LINPRCH                                                  
*                                                                               
CLEAR20  TM    STATUS,RRACCT       TAX AND INCOME DEPEND ON ACCOUNT             
         BZ    CLEAR60                                                          
*                                                                               
         CLI   LINTAX,C'*'                                                      
         BNE   CLEAR40                                                          
         TWAXC LINTAXH,LINTAXH                                                  
*                                                                               
CLEAR40  CLI   LININC,C'*'                                                      
         BNE   CLEAR60                                                          
         TWAXC LININCH,LININCH                                                  
*                                                                               
CLEAR60  LA    R4,LINLEN(R4)                                                    
         CR    R4,R6               AT END?                                      
         BNH   CLEAR               NO                                           
*                                                                               
         USING TABLED,R6           START AT BEGINNING OF TABLE                  
FSTLINE  LA    R6,TABLE                                                         
         USING LIND,R4                                                          
         LA    R4,UNTUNTH                                                       
         MVI   INPUTT,0            CLEAR INPUT INDICATOR                        
*                                                                               
NXTLINE  XC    FLDSTAT,FLDSTAT     CLEAR FIELD INDICATORS                       
*                                                                               
         CLI   CSOMODE,CSOMDCHA    ITEM CHANGE?                                 
         BE    NXTL20              YES, DON'T BUMP LINE NUMBER                  
         SR    R1,R1                                                            
         IC    R1,CSOLINE                                                       
         LA    R1,1(R1)                                                         
         STC   R1,CSOLINE                                                       
*                                                                               
NXTL20   MVC   TBCULINE,CSOLINE    SAVE LINE #                                  
*                                                                               
         CLI   LINLVLH+5,0         PRICE LEVEL                                  
         BE    *+8                                                              
         OI    FLDINPT,FLDLVL                                                   
*                                                                               
         CLI   LINUNTH+5,0         UNITS FIELD                                  
         BE    *+8                                                              
         OI    FLDINPT,FLDUNT                                                   
*                                                                               
         CLI   LINWRKH+5,0         WORKCODE FIELD                               
         BE    *+8                                                              
         OI    FLDINPT,FLDWRK                                                   
*                                                                               
         CLI   LINTAXH+5,0         TAX FIELD                                    
         BE    *+8                                                              
         OI    FLDINPT,FLDTAX                                                   
*                                                                               
         CLI   LINPRCH+5,0         PRICE FIELD                                  
         BE    *+8                                                              
         OI    FLDINPT,FLDPRC                                                   
*                                                                               
         CLI   LININCH+5,0         INCOME FIELD                                 
         BE    *+8                                                              
         OI    FLDINPT,FLDINC                                                   
*                                                                               
         OC    FLDINPT,FLDINPT     WAS ANYTHING INPUT ON THIS LINE?             
         BZ    EOL20               NO, SEE IF AT END OF SCREEN                  
         MVI   INPUTT,X'FF'        YES, INDICATE WE HAVE SOMETHING              
         EJECT                                                                  
***********************************************************************         
*              VALIDATE UNITS                                         *         
***********************************************************************         
*                                                                               
UNTVAL   DS    0H                                                               
         LA    R2,LINUNTH          UNITS HEADER                                 
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LINUNTH       INPUT IS REQUIRED                            
         BNE   EXIT                                                             
*                                                                               
         LA    RF,5                MAXIMUM NUMBER OF DIGITS                     
         LA    RE,LINUNT                                                        
*                                                                               
UNTVAL2  CLI   0(RE),C'.'                                                       
         BNE   *+12                                                             
         OI    TBUNTSTA,TBHOURS                                                 
         B     UNTVAL4                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,UNTVAL2                                                       
*                                                                               
UNTVAL4  GOTO1 AVALAMT,BOPARM,(X'C0',LINUNTH),(L'TBUNT,TBUNT)                   
         ORG   *-2                                                              
         TM    TBUNTSTA,TBHOURS    DECIMALS ENTERED?                            
         BZ    *+8                 NO                                           
         OI    0(R1),X'02'                                                      
         BASR  RE,RF                                                            
         BNE   AMTERR                                                           
*                                                                               
         TM    TBUNTSTA,TBHOURS    DO WE HAVE DECIMALS?                         
         BZ    UNT20               NO                                           
*                                                                               
         ZAP   BODUB4,TBUNT                                                     
         DP    BODUB4,=PL2'25'                                                  
         CP    BODUB4+6(2),=P'0'                                                
         BH    AMTERR                                                           
*                                                                               
UNT20    TM    LINUNTH+4,X'20'     HAVE UNITS CHANGED?                          
         BO    *+12                NO                                           
         OI    STATUS,RRLINE       YES, RE-READ PRICES                          
         OI    LINUNTH+4,X'20'                                                  
*                                                                               
UNTX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE LEVEL                                                   
***********************************************************************         
*                                                                               
LVLVAL   DS    0H                                                               
         LA    R2,LINLVLH          PRICE LEVEL HEADER                           
         GOTO1 AFVAL,LINLVLH                                                    
         BNL   LVL20                                                            
         MVI   LINLVL,C'A'         DEFAULT IS LEVEL A                           
         OI    LINLVLH+6,X'80'                                                  
*                                                                               
LVL20    CLI   LINLVL,C'A'         A,B,C OR D ARE VALID ENTIRES                 
         BE    LVL40                                                            
         CLI   LINLVL,C'B'                                                      
         BE    LVL40                                                            
         CLI   LINLVL,C'C'                                                      
         BE    LVL40                                                            
         CLI   LINLVL,C'D'                                                      
         BNE   LEVLER                                                           
*                                                                               
LVL40    TM    LINLVLH+4,X'20'     HAS LEVEL CHANGED?                           
         BO    *+12                NO                                           
         OI    STATUS,RRLINE       YES, RE-READ PRICES                          
         OI    LINLVLH+4,X'20'                                                  
*                                                                               
         MVC   TBLVL,LINLVL        SAVE LEVEL IN TABLE                          
*                                                                               
LVLX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE WORKCODE                                      *         
***********************************************************************         
*                                                                               
WRKVAL   DS    0H                                                               
         LA    R2,LINWRKH                                                       
         MVI   FVMINL,1            INPUT IS REQUIRED                            
         GOTO1 AFVAL,LINWRKH                                                    
         BNE   EXIT                                                             
*                                                                               
         CLC   LINWRK,=C'99'                                                    
         BE    WCERR                                                            
*                                                                               
         GOTO1 AGETWRK,LINWRK                                                   
         BNE   EXIT                                                             
         MVC   LINDSC,BOWORK1      PRINT THE DESCRIPTION                        
         OI    LINDSCH+6,X'80'                                                  
*                                                                               
         TM    LINWRKH+4,X'20'     HAS WORKCODE CHANGED?                        
         BO    *+8                 NO                                           
         OI    STATUS,RRLINE       YES, RE-READ PRICES                          
         OI    LINWRKH+4,X'20'                                                  
*                                                                               
         MVC   TBWRK,LINWRK        SAVE THE WORK CODE                           
         MVC   TBWRKD,LINDSC         AND THE DESCRIPTION IN TABLE               
*                                                                               
         LA    RE,SVACCT           GETJOB DOES GETOPT CALL AT WC LEVEL          
         LA    R3,TBWRK                                                         
         GOTO1 AGETJOB,BOPARM,(X'80',(RE)),(R3)                                 
*                                                                               
         GOTO1 AVALGOB             VALIDATE THE GOBATCH OPTION                  
         BNE   ERRXIT                                                           
*                                                                               
         BAS   RE,OPTMNT           SAVE OPTIONS                                 
*                                                                               
         USING GOBLOCKD,R1                                                      
         L     R1,AGOPBLK                                                       
         LA    R1,GOUWLIST         CHECK IF W/C IS NON-BILLABLE                 
         LA    R0,6                                                             
*                                                                               
WRK20    CLC   TBWRK,0(R1)                                                      
         BE    WRKER                                                            
         LA    R1,2(R1)                                                         
         BCT   R0,WRK20                                                         
*                                                                               
WRKX     DS    0H                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE TAX INDICATOR                                 *         
***********************************************************************         
*                                                                               
TAXVAL   DS    0H                                                               
         LA    R2,LINTAXH                                                       
         TM    STATUS,RRLINE          REFRESH TAX FIELD?                        
         BZ    TAX20                  NO                                        
         CLI   LINTAX,C'*'            YES, BUT NOT IF TYPED IN                  
         BNE   TAX20                                                            
         TWAXC LINTAXH,LINTAXH                                                  
*                                                                               
TAX20    CLC   LINTAX,BCSPACES     WAS TAX ENTERED ON SCREEN?                   
         BNH   TAX60               NO                                           
         LA    RF,LINTAX                                                        
         CLI   0(RF),C'*'                                                       
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         CLI   0(RF),C'Y'          Y OR N ONLY VALID ENTRIES                    
         BE    TAX40                                                            
         CLI   0(RF),C'N'                                                       
         BNE   TAXERR                                                           
*                                                                               
TAX40    MVC   TBTAX,0(RF)         SAVE TAX INDICATOR                           
         B     TAX80                                                            
*                                                                               
TAX60    MVC   LINTAX+1(1),TBTAX                                                
         MVI   LINTAX,C'*'                                                      
         OI    LINTAXH+6,X'80'                                                  
*                                                                               
TAX80    OI    LINTAXH+4,X'20'                                                  
*                                                                               
TAXX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE PRICE                                         *         
***********************************************************************         
*                                                                               
PRICEVAL DS    0H                                                               
         LA    R2,LINPRCH                                                       
         TWAXC LINTOTH,LINTOTH,PROT=Y                                           
         TM    STATUS,RRLINE          REFRESH THE PRICE?                        
         BZ    PRIC20                 NO                                        
         CLI   LINPRC,C'*'            YES, BUT NOT IF TYPED IN                  
         BNE   PRIC20                                                           
         TWAXC LINPRCH,LINPRCH                                                  
*                                                                               
PRIC20   ZAP   TBPRC,=P'0'          INITIALIZE FOR NO PRICE                     
         ZAP   TBTOT,=P'0'                                                      
         XC    FLAGT,FLAGT                                                      
*                                                                               
         GOTO1 AFVAL,LINPRCH                                                    
         CLI   FVILEN,0            DO WE HAVE A RATE?                           
         BE    PRIC60                                                           
         LA    R5,LINPRC                                                        
         SR    R3,R3                                                            
         ICM   R3,1,FVILEN         YES, SET THE LENGTH                          
*                                                                               
         CLI   LINPRC,C'*'         ADJUST FOR C'*' IN PRICE FIELD               
         BNE   PRIC40                                                           
         LA    R5,LINPRC+1                                                      
         BCTR  R3,0                                                             
         MVI   FLAGT,X'FF'         INDICATE PRICE FROM PRICE RECORD             
*                                                                               
PRIC40   GOTO1 AAMTVAL,BOPARM,(R5),(R3)                                         
         CLI   0(R1),X'FF'         ERROR IN PRICE FIELD                         
         BE    AMTERR                                                           
*                                                                               
         L     RF,BOPARM+4                                                      
         ZAP   INPUTAMT,2(6,RF)                                                 
         CP    INPUTAMT,=P'9999999'                                             
         JH    PRIERR              UNIT PRICE CANT EXCEED $99,999.99            
*                                                                               
         ZAP   TBPRC,4(4,RF)       SAVE THE RATE ENTERED                        
         BM    AMTERR                                                           
         B     PRIC80                                                           
*                                                                               
PRIC60   GOTO1 =A(GETPRICE),BCPARM,(RC),(R9),(R6),RR=RELO2                      
         TM    STATUS,UNITERR      UNITS ENTERED CORRECTLY?                     
         BO    UNTERR              NO                                           
         TM    STATUS,PRCFIND      DID WE FIND A PRICE?                         
         BNO   NOPRCER             NO                                           
         MVI   FLAGT,X'FF'                                                      
*                                                                               
PRIC80   CP    TBPRC,=P'0'         ZERO IS INVALID PRICE                        
         BE    NOPRCER                                                          
         ZAP   BODUB1,TBUNT                                                     
         MP    BODUB1,TBPRC                                                     
         ZAP   TBTOT,BODUB1                                                     
*                                                                               
         TM    TBUNTSTA,TBHOURS    IS THIS HOURS?                               
         BZ    *+10                                                             
         SRP   TBTOT,62,0                                                       
*                                                                               
         MVC   BOWORK1,BCSPACES                                                 
         MVI   BOWORK1,C'*'                                                     
         CURED TBPRC,(8,BOWORK1+1),2,DMCB=BOPARM,FLOAT=-,ALIGN=LEFT             
         LA    R1,BOWORK1                                                       
         CLI   FLAGT,X'FF'                                                      
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MVC   LINPRC,0(R1)                                                     
         OI    LINPRCH+6,X'80'                                                  
*                                                                               
         CURED TBTOT,(12,LINTOT),2,DMCB=BOPARM,FLOAT=-                          
         OI    LINTOTH+6,X'80'                                                  
*                                                                               
         CLI   ACJOBEST,C' '       ANY OPTION?                                  
         BE    PRICEX              NO                                           
*                                                                               
         LA    R2,LINTOTH                                                       
         LA    R3,TBWRK                                                         
         GOTO1 AVALWRK,BOPARM,(R3)                                              
         BH    ESTER                                                            
*                                                                               
PRICEX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE INCOME ACCOUNT                                *         
***********************************************************************         
*                                                                               
INCVAL   DS    0H                                                               
         LA    R2,LININCH                                                       
         TM    STATUS,RRLINE          CLEAR OUT INCOME ACCOUNT ?                
         BZ    INC20                  NO                                        
         CLI   LININC,C'*'            YES, IF NOT TYPED IN                      
         BNE   INC20                                                            
         TWAXC LININCH,LININCH                                                  
*                                                                               
INC20    XC    FLAGT,FLAGT                                                      
         GOTO1 AFVAL,LININCH                                                    
         CLI   FVILEN,0            DO WE HAVE AN INCOME ACCOUNT?                
         BE    INC60               NO, GET DEFAULT                              
         CLC   LININC(2),=C'SI'    YES, VERIFY FOR SI/SK                        
         BE    INC40                                                            
         CLC   LININC(2),=C'SK'                                                 
         BE    INC40                                                            
         CLC   LININC(3),=C'*SI'                                                
         BE    INC40                                                            
         CLC   LININC(3),=C'*SK'                                                
         BNE   INVERR                                                           
*                                                                               
INC40    DS    0H                                                               
         MVC   TBINC,BCSPACES                                                   
         MVC   TBINC(1),CUABIN                                                  
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,FVILEN                                                      
         BCTR  R1,0                                                             
         LA    RF,LININC                                                        
         CLI   LININC,C'*'         DID USER ENTER?                              
         BNE   *+14                YES                                          
         MVI   FLAGT,X'FF'         NO, INDICATE SO                              
         LA    RF,LININC+1         ADJUST FOR '*'                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     INC80                                                            
         MVC   TBINC+1(0),0(RF)                                                 
*                                                                               
INC60    CLC   TBINC,BCSPACES      DO WE HAVE AN INCOME ACCOUNT?                
         BNH   INCERR              NO                                           
         MVI   FLAGT,X'FF'         INDICATE WE GENERATED                        
*                                                                               
INC80    MVC   IOKEY,BCSPACES      YES, READ IT                                 
         MVC   IOKEY(15),TBINC                                                  
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BO    LOCKER              YES                                          
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BZ    POSTER              NO                                           
*                                                                               
         MVC   TBINCN,ACNAME       SAVE THE NAME                                
*                                                                               
         CLC   TBINC+1(2),=C'SI' POSTING TO SI?                                 
         BNE   INC180               NO                                          
*                                                                               
         TM    STATUS,COST          YES, GET COST ACCOUNT                       
         BZ    INC180                                                           
*                                                                               
         L     R5,AIO1                                                          
         AH    R5,=Y(ACTRFST-ACTRECD)                                           
*                                                                               
INC100   CLI   0(R5),0             TEST FOR EOR                                 
         BE    INC140                                                           
*                                                                               
         USING ACSPECD,R5                                                       
         CLI   ACSPTYP,ACSPOAN     OVERRIDE ANALYSIS ACCOUNT?                   
         BNE   INC120              NO                                           
         MVC   TB12ACT(1),CUABIN                                                
         MVC   TB12ACT+1(2),=C'12'                                              
         MVC   TB12ACT+3(12),ACSPACCT                                           
         B     INC160                                                           
*                                                                               
INC120   SR    R1,R1               BUMP TO NEXT ELEMENT                         
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     INC100                                                           
*                                                                               
INC140   CLI   TB12ACT,0           DO WE HAVE A 12 ACCOUNT?                     
         BNE   INC160              YES                                          
         MVC   TB12ACT,BCSPACES    NO, USE COST BYTE                            
         MVC   TB12ACT(1),CUABIN                                                
         MVC   TB12ACT+1(2),=C'12'                                              
         MVC   TB12ACT+3(1),ACCOST                                              
*                                                                               
INC160   MVC   IOKEY,BCSPACES      VALIDATE 12 ACCOUNT                          
         MVC   IOKEY(L'TB12ACT),TB12ACT                                         
         CLC   TB12ACT+3(12),BCSPACES                                           
         BNH   SIERR                                                            
*                                                                               
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BO    LOCKER              YES                                          
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BZ    POSTER              NO                                           
         MVC   TB12NAME,ACNAME     SAVE THE NAME                                
*                                                                               
INC180   MVC   BOWORK1,BCSPACES                                                 
         MVI   BOWORK1,C'*'                                                     
         MVC   BOWORK1+1(L'TBINC-1),TBINC+1                                     
         LA    R1,BOWORK1                                                       
         CLI   FLAGT,X'FF'                                                      
         BE    *+8                                                              
         LA    R1,BOWORK1+1                                                     
         MVC   LININC,0(R1)        DISPLAY INCOME ACCOUNT                       
         OI    LININCH+6,X'80'                                                  
*                                                                               
INCX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              END OF SCREEN                                          *         
***********************************************************************         
*                                                                               
EOLVAL   DS    0H                                                               
         LA    R6,TBLNQ(R6)                                                     
*                                                                               
EOL20    NI    STATUS,X'FF'-RRLINE TURN OFF REFRESH FLAG                        
         LA    R4,LINLEN(R4)       END OF SCREEN YET?                           
         LA    R2,UNTUNTH                                                       
         LA    R1,UNTENDH                                                       
         CR    R4,R1                                                            
         BNH   NXTLINE                                                          
         MVI   0(R6),X'FF'         MARK END OF TABLE                            
*                                                                               
         OC    INPUTT,INPUTT       ERROR IF NOTHING WAS INPUT                   
         BZ    MISERR                                                           
*                                                                               
EOL40    LA    R2,UNTUPDH                                                       
         CLI   UNTUPDH+5,0         ANY INPUT INTO UPDATE FIELD                  
         BNE   EOL60                                                            
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'ENTER CHANGES OR ''Y'' IN UPDATE FIELD'             
         B     EXIT                                                             
*                                                                               
EOL60    DS    0H                                                               
         MVI   TAXPASS,0                                                        
         CLI   UNTUPD,C'Y'                                                      
         BE    EOLX                                                             
         CLI   SCRIPTEX,C'Y'                                                    
         BNE   INVERR                                                           
         CLI   UNTUPD,C'S'         FOR SCRIPT ONLY                              
         BNE   INVERR                                                           
         MVI   TAXPASS,1           DON'T ADD ITEM                               
*                                                                               
EOLX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD POSTINGS ELEMENTS                                *         
***********************************************************************         
*                                                                               
         USING TABLED,R6                                                        
BUILDEL  LA    R6,TABLE                                                         
*                                                                               
BLD100   CLI   0(R6),X'FF'                                                      
         BE    BLD400                                                           
         CLI   CSOMODE,CSOMPCHA    PREPARE SCREEN FOR CHANGE?                   
         BE    BLD500                                                           
*                                                                               
***********************************************************************         
*              BUILD DESCRIPTION ELEMENT                              *         
***********************************************************************         
*                                                                               
         LA    R5,IOAREA+2                                                      
         USING DLDESCD,R5                                                       
         MVI   DLDSEL,X'64'                                                     
         MVC   DLDSDATE,SVDATE                                                  
         MVI   DLDSSBRF,0                                                       
         MVC   DLDSREF,SVREFER                                                  
         XC    DLDSSTAT(7),DLDSSTAT                                             
         XC    DLDSNARR,DLDSNARR                                                
         SR    R1,R1                                                            
         ICM   R1,1,SVNARLEN                                                    
         BZ    *+6                                                              
         BCTR  R1,0                                                             
         MVC   DLDSNARR(0),SVNARR                                               
         EX    R1,*-6                                                           
         AH    R1,=Y(DLDSNARR-DLDSEL)                                           
         LA    R1,1(R1)                                                         
         STC   R1,DLDSLEN                                                       
         AR    R5,R1                                                            
*                                                                               
***********************************************************************         
*              DEBIT TO SJ/ CONTRA OF SI OR SK                        *         
***********************************************************************         
*                                                                               
         USING UNPELD,R5                                                        
         MVI   UNPEL,UNPELQ                                                     
         MVI   UNPLN,UNPLNQ                                                     
         MVC   UNPSTRT,SVDATE                                                   
         ZAP   UNPRICE,TBPRC                                                    
         MVC   UNPLVL,TBLVL                                                     
*                                                                               
         MVC   BOWORK1(1),TBUNTSTA                                              
         NI    BOWORK1,TBHOURS                                                  
         OC    UNPSTAT,BOWORK1                                                  
*                                                                               
         ZAP   UNPUNIT,TBUNT                                                    
         SR    R1,R1                                                            
         IC    R1,UNPLN                                                         
         AR    R5,R1                                                            
*                                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,X'69'                                                     
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,SVACCT                                                  
         MVC   DLPSDBNM,SVACCN                                                  
         MVC   DLPSCRAC,TBINC                                                   
         MVC   DLPSCRNM,TBINCN                                                  
         MVI   DLPSTYPE,0                                                       
         CLI   UNTCOM,C'Y'         COMMISSIONABLE?                              
         BE    *+8                 YES                                          
         OI    DLPSTYPE,X'40'      NON-COMMISSIONABLE                           
         ZAP   DLPSAMNT,TBTOT                                                   
         MVC   DLPSANAL,TBWRK                                                   
         CLI   TBTAX,C'Y'          IF TAXABLE, ADD TO BUCKET                    
         BNE   *+10                                                             
         AP    TAXTOT,TBTOT                                                     
         SR    R1,R1                                                            
         IC    R1,DLPSLEN                                                       
         AR    R5,R1                                                            
*                                                                               
***********************************************************************         
*              MEDIA TRANSFER ELEMENT IF INCOME=SI                    *         
***********************************************************************         
*                                                                               
         CLC   TBINC+1(2),=C'SI'                                                
         BNE   BDL200                                                           
         USING ACMTD,R5                                                         
         XC    ACMTEL(ACMTLNQ),ACMTEL CLEAR ELEMENT AREA                        
         MVI   ACMTEL,ACMTELQ                                                   
         MVI   ACMTLEN,ACMTLNQ                                                  
         MVI   ACMTSYS,C'J'                                                     
         MVC   ACMTMED,SVJOB           MEDIA                                    
         MVC   ACMTCLI(12),SVACCT+3    CLIENT/PRODUCT/JOB                       
         MVC   ACMTMOS,CSLSTCUR+(LSTBMOSP-LSTTABD)                              
         MVC   ACMTDSCP,SVACCN                                                  
*                                                                               
         ZAP   BODUB1,TBTOT                                                     
         CVB   R0,BODUB1                                                        
         STCM  R0,15,ACMTCOM       ITS REVENUE                                  
         STCM  R0,15,ACMTINTL      AND INTERNAL INCOME                          
         SR    R1,R1                                                            
         IC    R1,ACMTLEN                                                       
         AR    R5,R1                                                            
*                                                                               
***********************************************************************         
*              GROSS MEMO ELEMENT                                     *         
***********************************************************************         
*                                                                               
         DS    0H                                                               
         USING TRCASHD,R5                                                       
         MVI   TRCSEL,X'50'                                                     
         MVI   TRCSLEN,X'09'                                                    
         MVI   TRCSTYPE,C'G'       INDICATE GROSS MEMO                          
         ZAP   TRCSAMNT,=P'0'      AMOUNT ALWAYS ZERO                           
         SR    R1,R1                                                            
         IC    R1,TRCSLEN                                                       
         AR    R5,R1                                                            
*                                                                               
***********************************************************************         
*              CREDIT TO SI OR SK/ CONTRA OF JOB                      *         
***********************************************************************         
*                                                                               
BDL200   DS    0H                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,X'6A'                                                     
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSCRAC,TBINC                                                   
         MVC   DLPSCRNM,TBINCN                                                  
         MVC   DLPSDBAC,BCSPACES                                                
         MVC   DLPSDBAC,SVACCT                                                  
         MVC   DLPSDBNM,SVACCN                                                  
*                                                                               
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,TBTOT                                                   
         MVC   DLPSANAL,SVOFF                                                   
         SR    R1,R1                                                            
         IC    R1,DLPSLEN                                                       
         AR    R5,R1                                                            
         TM    STATUS,COST         DO COST POSTINGS?                            
         BZ    BLD300              NO                                           
         CLC   TBINC+1(2),=C'SI'   YES, INCOME=SI?                              
         BNE   BLD300              NO                                           
*                                                                               
***********************************************************************         
*              DEBIT TO 1C/CONTRA 12 - CREDIT TO 12/CONTRA 1C         *         
***********************************************************************         
*                                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,X'68'        DOUBLE POSTING                               
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,SV1CACT    DR=1C A/C                                    
         MVC   DLPSDBNM,SV1CNAME                                                
         MVC   DLPSCRAC,TB12ACT    CR=12 A/C (REVENUE)                          
         MVC   DLPSCRNM,TB12NAME                                                
         MVI   DLPSTYPE,X'80'      SUBSIDIARY POSTINGS                          
         ZAP   DLPSAMNT,TBTOT                                                   
         MVC   DLPSANAL,SVOFF      OFFICE=CLI/PRO OFFICE                        
         SR    R1,R1                                                            
         IC    R1,DLPSLEN                                                       
         AR    R5,R1                                                            
*                                                                               
BLD300   MVI   0(R5),0             END OF RECORD                                
         LA    R5,1(R5)                                                         
         LA    R1,IOAREA                                                        
         SR    R5,R1               PUT OUT ACCDAY RECORD                        
         STH   R5,BOHALF1                                                       
         MVC   IOAREA(2),BOHALF1                                                
*                                                                               
         MVC   CSOLINE,TBCULINE                                                 
*                                                                               
         LA    R1,BCPARM                                                        
         XC    0(4,R1),0(R1)       CLEAR DISK ADDRESS                           
         ST    R1,BOPARM+8                                                      
         ZAP   BOPL61,TBUNT                                                     
         TM    TBUNTSTA,TBHOURS    IS THIS HOURS?                               
         BO    *+10                OK                                           
         MP    BOPL61,=P'100'      NO, ADJUST                                   
*                                                                               
         MVC   BOWORK1(L'DLDSREF),IOAREA+2+(DLDSREF-DLDESCD)                    
         L     R5,BOPARM+8                                                      
         MVC   BOWORK1+10(4),0(R5) DISK ADDRESS                                 
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   TAXPASS,0                                                        
         BNE   BLD350                                                           
         GOTO1 AADDITE,BOPARM,IOAREA,BOPL61,BOWORK1                             
BLD350   CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                                                             
*                                                                               
         LA    R6,TBLNQ(R6)        BUMP TO NEXT TABLE ENTRY                     
         B     BLD100              KEEP POSTING                                 
*                                                                               
BLD400   DS    0H                                                               
         LA    R2,UNTJOBH                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         TWAXC UNTTABH,UNTTABH                                                  
         TWAXC UNTUNTH,UNTLSTH,PROT=Y                                           
         TWAXC UNTUPDH,UNTUPDH                                                  
*                                                                               
BLD500   CLI   CSACT,ACTINP        ARE WE DOING ITEM INPUT?                     
         BNE   EXIT                NO, DON'T CALL TAX AGAIN                     
*                                                                               
*        CALL TAX SCREEN IF PF9 PRESSED (FORCE UPDATE)                          
*                                                                               
CALLTAX  CLI   BCPFKEY,9                                                        
         BNE   CALL60                                                           
         CLI   TWAMODE,2           ALRADY DOING TAX?                            
         BE    CALL60              YES, LET OVERLAY HANDLE PF9                  
*                                                                               
         LA    R2,UNTUNTH          CHECK FOR ANY DATA TO UPDATE                 
         LA    R4,UNTUPDH          END OF SCREEN ADDRESS                        
         SR    R0,R0                                                            
*                                                                               
CALL20   CR    R4,R2               END OF SCREEN?                               
         BE    CALL60              YES, GO TO TAX                               
         TM    1(R2),X'20'         IS THIS A PROTECTED FIELD?                   
         BO    CALL40              YES, SKIP IT                                 
         CLI   5(R2),0             NO, ANY DATA?                                
         BNE   UPDERR              YES, MUST UPDATE IT                          
*                                                                               
CALL40   IC    R0,0(R2)            GET NEXT FIELD                               
         AR    R2,R0                                                            
         B     CALL20                                                           
*                                                                               
CALL60   GOTO1 =A(TAXMOD),BCPARM,(RC),(R9),RR=RELO2                             
         B     EXIT1                                                            
         EJECT                                                                  
***********************************************************************         
*              VERIFY DATE TO BE WITHIN RANGE                         *         
***********************************************************************         
*                                                                               
DATECHK  ST    RE,12(RD)                                                        
         L     RE,0(R1)                                                         
         CLC   BCTDATL,0(RE)                                                    
         BH    DATEC20                                                          
         CLC   BCTDATH,0(RE)                                                    
         BNL   DATECX                                                           
*                                                                               
DATEC20  MVI   0(R1),X'FF'         ERROR                                        
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
*                                                                               
DATECX   L     RE,12(RD)                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*              GET OPTIONS AND MISCELLANEOUS VALUES                   *         
***********************************************************************         
*                                                                               
         USING GOBLOCKD,R2                                                      
OPTMNT   NTR1                                                                   
         L     R2,AGOPBLK                                                       
         MVC   SVOFG,GOEFFOG                                                    
         MVC   SVOFF,GOEFFOFC                                                   
         MVC   SVMGR,GOEFFMG                                                    
         MVC   SVMED,GOEFFMED                                                   
         MVC   SVSCHEME,GOSCHEME                                                
         MVC   TBWGR,GOEFFWG                                                    
*                                                                               
         L     R2,AGOXBLK          GET EXTENSION                                
         USING GOXBLKD,R2                                                       
         MVC   TBINC,GOICA                                                      
         MVC   TBTAX,GOTAX                                                      
         MVC   SVTAXL,GOTXLOC                                                   
         MVC   SVTAXW,GOTXWC                                                    
*                                                                               
OPTX     B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*              READ SJ HIERARCHY                                      *         
***********************************************************************         
*                                                                               
READSJ   NTR1                                                                   
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'SJ'                                                
         GOTO1 AGETLDG                                                          
*                                                                               
READX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              ERROR ROUTINES                                         *         
***********************************************************************         
*                                                                               
WCERR    MVC   FVMSGNO,=AL2(AE$INWRK)    INVALID WORKCODE                       
         B     ERRXIT                                                           
*                                                                               
UPDERR   MVC   FVMSGNO,=AL2(AE$UPDAT)    UPDATE ERROR                           
         LA    R2,UNTUPDH                                                       
         B     ERRXIT                                                           
*                                                                               
SIERR    MVC   FVMSGNO,=AL2(AE$SIMSS)    SI ERROR                               
         B     ERRXIT                                                           
*                                                                               
WRKER    MVC   FVMSGNO,=AL2(AE$INWRK)    WORKCODE ERROR                         
         B     ERRXIT                                                           
*                                                                               
ESTER    MVC   FVMSGNO,=AL2(AE$AEEWC)    ESTIMATE/AMOUNT ERROR                  
         B     ERRXIT                                                           
*                                                                               
LEVLER   MVC   FVMSGNO,=AL2(AE$INVIF)    LEVEL ERROR                            
         B     ERRXIT                                                           
*                                                                               
TAXERR   MVC   FVMSGNO,=AL2(AE$INVIF)    TAX ERROR                              
         B     ERRXIT                                                           
*                                                                               
PRIERR   MVC   FVMSGNO,=AL2(AE$MAXPR)    AMOUNT MUST BE <= $99,999.99           
         B     ERRXIT                                                           
*                                                                               
AMTERR   MVC   FVMSGNO,=AL2(AE$INVAM)    AMOUNT ERROR                           
         B     ERRXIT                                                           
*                                                                               
COMERR   MVC   FVMSGNO,=AL2(AE$INVIF)    INVALID COMMISSION                     
         B     ERRXIT                                                           
*                                                                               
NOPRCER  MVC   FVMSGNO,=AL2(AE$PRICE)    NO PRICE RECORD                        
         B     ERRXIT                                                           
*                                                                               
LOCKER   MVC   FVMSGNO,=AL2(AE$ACTLK)    ACCOUNT IS LOCKED                      
         B     ERRXIT                                                           
*                                                                               
CLOSER   MVC   FVMSGNO,=AL2(AE$JBCLO)    ACCOUNT IS CLOSED                      
         B     ERRXIT                                                           
*                                                                               
POSTER   MVC   FVMSGNO,=AL2(AE$INACP)    NOT VALID ACCOUNT FOR POSTING          
         B     ERRXIT                                                           
*                                                                               
DATERR   MVC   FVMSGNO,=AL2(AE$INVIF)    INVALID DATE                           
         B     ERRXIT                                                           
*                                                                               
XJOBER   MVC   FVMSGNO,=AL2(AE$BXJOB)    X-JOB ERROR                            
         B     ERRXIT                                                           
*                                                                               
MISERR   MVC   FVMSGNO,=AL2(AE$MISIF)    MISSING INPUT FIELD                    
         B     ERRXIT                                                           
*                                                                               
INCERR   MVC   FVMSGNO,=AL2(AE$MINCA)    INCOME ACCOUNT MISSING                 
         B     ERRXIT                                                           
*                                                                               
INVERR   MVC   FVMSGNO,=AL2(AE$INVIF)    INVALID ENTRY                          
         B     ERRXIT                                                           
*                                                                               
*NTERR   MVC   FVMSGNO,=AL2(AE$INVAM)    UNIT NOT NUMERIC                       
UNTERR   MVC   FVMSGNO,=AL2(173)         INVALID UNITS                          
*                                                                               
ERRXIT   MVC   FVXTRA,BCSPACES     COMMON PORTION OF ERROR ROUTINE              
*                                                                               
EXIT     ST    R2,FVADDR                                                        
EXIT1    XIT1                                                                   
*                                                                               
ACCMST   DC    C'ACCMST  '                                                      
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
INPUTAMT DS     D                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*               READ PRICE RECORD                                     *         
***********************************************************************         
*                                                                               
GETPRICE DS    0D                                                               
         NMOD1 0,**PRIC**                                                       
         L     RC,0(R1)                                                         
         L     R9,4(R1)                                                         
         USING TABLED,R6                                                        
         L     R6,8(R1)                                                         
         USING MAJORD,R4                                                        
         ST    R4,SAVER4                                                        
         LA    R4,PMAJOR                                                        
         NI    STATUS,X'FF'-PRCFIND                                             
*                                                                               
         USING PRCRECD,R5                                                       
GETP20   LA    R5,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   PRCKTYP,PRCKTYPQ    X'2C' RECORD                                 
         MVI   PRCKSUB,PRCKSUBQ    X'24' SUB RECORD                             
         MVC   PRCKCPY,CUABIN                                                   
         MVC   PRCKUNT(2),=C'SJ'                                                
*                                                                               
         TM    MAJORK,MOFG                                                      
         BZ    *+10                                                             
         MVC   PRCKOFG,SVOFG       FORMAT HIGHEST LEVEL KEY                     
*                                                                               
         TM    MAJORK,MOFF                                                      
         BZ    *+10                                                             
         MVC   PRCKOFC,SVOFF                                                    
*                                                                               
         TM    MAJORK,MCLI                                                      
         BZ    *+10                                                             
         MVC   PRCKCLI,SVCLI                                                    
*                                                                               
         TM    MAJORK,MPRO                                                      
         BZ    *+10                                                             
         MVC   PRCKPRO,SVPRO                                                    
*                                                                               
         TM    MAJORK,MJOB                                                      
         BZ    *+10                                                             
         MVC   PRCKJOB,SVJOB                                                    
*                                                                               
         MVC   SVMAJK,PRCKEY       SAVE THE KEY FOR MINOR READS                 
         GOTO1 AIO,IOHI+IOACCMST+IO1                                            
*                                                                               
         L     R5,AIO1                                                          
         CLC   PRCKEY(PRCKMGR-PRCKEY),SVMAJK                                    
         BE    GETP60                                                           
*                                                                               
GETP40   LA    R4,1(R4)            NEXT NEXT SEARCH LEVEL                       
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BNE   GETP20              NO                                           
         B     GETPX               YES, EXIT WITHOUT SETTING PRCFIND            
*                                                                               
         USING MINORD,R3                                                        
         USING PRCRECD,R5                                                       
GETP60   LA    R3,PMINOR                                                        
*                                                                               
GETP80   LA    R5,IOKEY            DO MINOR LEVEL SEARCH NOW                    
         XC    IOKEY,IOKEY                                                      
         MVC   PRCKEY,SVMAJK       START WITH MAJOR KEY                         
*                                                                               
         TM    MINORK,MMGR                                                      
         BZ    *+10                                                             
         MVC   PRCKMGR,SVMGR       FORMAT HIGHEST LEVEL KEY                     
*                                                                               
         TM    MINORK,MMED                                                      
         BZ    *+10                                                             
         MVC   PRCKMED,SVMED                                                    
*                                                                               
         TM    MINORK,MWGR                                                      
         BZ    *+10                                                             
         MVC   PRCKWGR,TBWGR                                                    
*                                                                               
         TM    MINORK,MWRK                                                      
         BZ    *+10                                                             
         MVC   PRCKWRK,TBWRK                                                    
*                                                                               
         MVC   SVMINK,PRCKEY       SAVE THE MINOR KEY                           
         GOTO1 AIO,IOHI+IOACCMST+IO1                                            
*                                                                               
         L     R5,AIO1                                                          
         CLC   PRCKEY(PRCKEFF-PRCKEY),SVMINK                                    
         BE    GETP100                                                          
         LA    R3,1(R3)            NEXT NEXT SEARCH LEVEL                       
         CLI   0(R3),X'FF'                                                      
         BNE   GETP80                                                           
*                                                                               
         LA    R5,IOKEY                                                         
         XC    IOKEY,IOKEY         NO MINOR LEVEL, RE-READ MAJOR                
         MVC   PRCKEY,SVMAJK                                                    
         GOTO1 AIO,IOHI+IOACCMST+IO1                                            
         L     R5,AIO1             LOOK FOR MAJOR W/O MINOR                     
         CLC   PRCKEY(PRCKEFF-PRCKEY),SVMAJK                                    
         BE    GETP100             FOUND                                        
         B     GETP40              NO, LOOK AT NEXT LEVEL                       
*                                                                               
GETP100  L     R5,AIO1                                                          
         MVC   BOWORK1(L'SVDATE),SVDATE                                         
         XC    BOWORK1,EFFS                                                     
*                                                                               
GETP120  CLC   BOWORK1(L'PRCKEFF),PRCKEFF                                       
         BNH   GETP140             FOUND AN APPROPRIATE DATE                    
         GOTO1 AIO,IOSQ+IOACCMST+IO1                                            
         L     R5,AIO1                                                          
         CLC   PRCKEY(PRCKEFF-PRCKEY),IOKEYSAV                                  
         BE    GETP120                                                          
         B     GETPX               NO DATE MATCH                                
*                                                                               
GETP140  LA    R5,PRCRFST                                                       
*                                                                               
GETP160  CLI   0(R5),0             NO PRICE ELEMENT FOUND                       
         BE    GETPX                                                            
         CLI   0(R5),PRCELQ                                                     
         BE    GETP200                                                          
*                                                                               
GETP180  SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     GETP160                                                          
*                                                                               
         DROP  R5                                                               
         USING PRCELD,R5                                                        
GETP200  DS    0H                                                               
*MN                                                                             
         USING PRCRECD,R4                                                       
         L     R4,AIO1                                                          
         MVC   BOWORK1(L'TBUNTSTA),TBUNTSTA                                     
         NI    BOWORK1,TBHOURS                                                  
*                                                                               
         MVC   BOWORK1+1(L'PRCRSTA),PRCRSTA                                     
         NI    BOWORK1+1,PRCSQTRH                                               
*                                                                               
         CLC   BOWORK1(1),BOWORK1+1                                             
         BE    GETP250                 OK, FORMATS MATCH                        
*                                                                               
         USING LIND,R4                                                          
         L     R4,SAVER4                                                        
         OC    TBUNTSTA,BOWORK1+1  SET TBUNTSTA FROM PRCRSTA                    
         CLI   BOWORK1+1,PRCSQTRH  IS PRICE HOURS?                              
         BE    TAG                 YES, ADJUST THE UNITS TO MATCH               
         OI    STATUS,UNITERR                                                   
         LA    R2,LINUNTH                                                       
         B     GETPX                                                            
*                                                                               
TAG      ZAP   UNITS,TBUNT         MOVE UNITS                                   
*MN      MP    UNITS,=P'100'                                                    
         SRP   UNITS,2,0                                                        
         ZAP   TBUNT,UNITS                                                      
         CURED UNITS,(6,LINUNT),2,DMCB=BOPARM,FLOAT=-,ALIGN=LEFT                
         STC   R0,LINUNTH+5                                                     
*MN                                                                             
GETP250  DS    0H                                                               
         ZAP   UNITS,TBUNT         MOVE UNITS                                   
         BNM   GETP260             NEGATIVE UNITS?                              
         ZAP   BODUB1,UNITS                                                     
         MP    BODUB1,=P'-1'       YES, MAKE POSITIVE FOR LOOKUP                
         ZAP   UNITS,BODUB1                                                     
GETP260  CP    UNITS,PRCSUNT       MUST BE EQUAL OR > START                     
         BL    GETP180                                                          
         CP    UNITS,PRCEUNT       AND EQUAL OR < END                           
         BH    GETP180                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,TBLVL                                                         
         SLL   R1,26                                                            
         SRL   R1,24                                                            
         LA    R5,PRCPRC1-4                                                     
         LA    R5,0(R5,R1)                                                      
         ZAP   TBPRC,0(4,R5)       SAVE PRICE                                   
         OI    STATUS,PRCFIND      INDICATE A PRICE WAS FOUND                   
GETPX    XMOD1 1                                                                
*                                                                               
EFFS     DC    3X'FF'                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              MAJOR PRICE LOOKUP TABLE                               *         
***********************************************************************         
*                                                                               
PMAJOR   DS    0CL1                                                             
         DC    B'00111000'                         CLI,PRO,JOB                  
         DC    B'00110000'                         CLI,PRO                      
         DC    B'00100000'                         CLI                          
         DC    B'01000000'                     OFF                              
         DC    B'10000000'                 OFG                                  
         DC    B'00000000'             AGY                                      
*                                                                               
         DC    X'FF'           END OF TABLE OF POSSIBLE SEARCHES                
         EJECT                                                                  
***********************************************************************         
*              MINOR PRICE LOOKUP TABLE                               *         
***********************************************************************         
*                                                                               
PMINOR   DS    0CL1                                                             
         DC    B'01010000'                          MED     WRK                 
         DC    B'01100000'                          MED WGR                     
         DC    B'01000000'                          MED                         
         DC    B'10010000'                      MGR        WRK                  
         DC    B'10100000'                      MGR     WGR                     
         DC    B'10000000'                      MGR                             
         DC    B'00010000'                                  WRK                 
         DC    B'00100000'                              WGR                     
*                                                                               
         DC    X'FF'           END OF TABLE OF POSSIBLE SEARCHES                
         EJECT                                                                  
***********************************************************************         
*               TAX MODULE INTERFACE                                  *         
***********************************************************************         
*                                                                               
TAXMOD   DS    0D                                                               
         NMOD1 0,**TAX**                                                        
         L     RC,0(R1)                                                         
         CLI   CSACT,ACTINP        ENTERING FROM ITEM INPUT?                    
         BNE   TAXM20                                                           
         MVI   STXMODE,C'E'        EDIT MODE                                    
         CLI   TWAMODE,2                                                        
         BE    TAXM20                                                           
         MVI   STXMODE,C'B'        BUILD SCREEN                                 
         CLI   BCPFKEY,9                                                        
         BE    TAXM20              GO DIRECTLY TO TAX                           
*                                                                               
         CP    TAXTOT,=P'0'        DO WE HAVE AN TAXABLE DATA?                  
         BE    TAXMX               NO                                           
         MVC   STXACC,SVACCT                                                    
         MVC   STXREF,SVREFER                                                   
         GOTO1 VDATCON,BOPARM,(1,SVDATE),(8,STXDTE)                             
         MVC   STXCOFF,SVOFF                                                    
         MVC   STXAMT,TAXTOT                                                    
         MVC   STXNARR(L'SVNARR),SVNARR                                         
         MVC   STXLOC,SVTAXL                                                    
         MVC   STXWKC,SVTAXW                                                    
*                                                                               
TAXM20   LA    R3,X'46'                                                         
         GOTO1 VCOLY,BOPARM,((R3),0),(0,0)                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)            GO TO TAX OVERLAY                            
         MVI   CSSPROG,1                                                        
         GOTO1 (RF),BOPARM,(TAXPASS,STXDATA),(R9)                               
         CLI   CSACT,ACTCHA        ENTERING FROM ITEM/CHANGE?                   
         BE    TAXMX                                                            
*                                                                               
         MVI   TWAMODE,2                                                        
         CLI   STXMODE,C'E'        STILL IN EDIT MODE                           
         BE    TAXMX               OVERLAY HAS SET-UP CURSOR                    
         MVI   TWAMODE,0           SALES TAX IS DONE                            
         LA    R2,UNTCLIH                                                       
         ST    R2,FVADDR                                                        
*                                                                               
TAXMX    XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
*              DSECT FOR WORKING STORAGE                              *         
***********************************************************************         
*                                                                               
PROGD    DSECT                                                                  
RELO2    DS    A                                                                
SAVER4   DS    A                                                                
UNITS    DS    PL3                 HOLD AREA FOR UNIT PRICE LOOKUP              
*                                                                               
STATUS   DS    XL1                                                              
COST     EQU   X'80'               DO COST POSTINGS                             
RRLINE   EQU   X'40'               REREAD TX, PRICE, INCOME FOR LINE            
RRACCT   EQU   X'20'               REREAD TX, PRICE, INCOME FOR SCREEN          
RRDATE   EQU   X'10'               REREAD PRICE FOR SCREEN                      
PRCFIND  EQU   X'08'               PRICE WAS FOUND                              
UNITERR  EQU   X'04'               UNITS WRONG FORMAT                           
PRCHOUR  EQU   X'02'               PRICES ARE HOURS                             
*                                                                               
*                                  INPUT STATUS                                 
FLDSTAT  DS    0XL3                                                             
FLDINV   DS    XL1                 FIELDS THAT ARE INVALID                      
FLDINPT  DS    XL1                 FIELDS THAT WERE INPUT                       
FLDREQ   DS    XL1                 FIELDS THAT ARE REQUIRED                     
FLDUNT   EQU   X'80'               UNITS FIELD                                  
FLDLVL   EQU   X'40'               LEVEL A/B/C/D                                
FLDWRK   EQU   X'20'               WORKCODE FIELD                               
FLDTAX   EQU   X'10'               TAX FIELD                                    
FLDPRC   EQU   X'08'               PRICE FIELD                                  
FLDINC   EQU   X'04'               INCOME ACCOUNT                               
*                                                                               
*                                  SWITCHES/INDICATORS                          
INPUTT   DS    XL1                 X'00' = NO INPUT, X'FF'= DATA INPUT          
FLAGT    DS    XL1                 TEMP FLAG                                    
LINE     DS    XL1                 LINE INDICATOR                               
*                                                                               
*                                                                               
SV1CACT  DS    CL15                1C ACCOUNT                                   
SV1CNAME DS    CL36                1C ACCOUNT NAME                              
SVOFF    DS    CL2                 OFFICE CODE                                  
SVOFG    DS    C                   OFFICE GROUP                                 
SVACCT   DS    CL15                SJ ACCOUNT                                   
SVACCN   DS    CL36                SJ ACCOUNT NAME                              
SVCLI    DS    CL6                 CLIENT                                       
SVPRO    DS    CL6                 PRODUCT                                      
SVJOB    DS    CL6                 JOB                                          
SVDATE   DS    CL3                 DATE                                         
SVREFER  DS    CL6                 REFERENCE NUMBER                             
SVNARLEN DS    X                   NARRATIVE LENGTH                             
SVNARR   DS    CL100               NARRATIVE                                    
SVCOMM   DS    C                   COMMISSIONABLE INDICATOR                     
SVTAXL   DS    CL8                 LOCALITY CODE                                
SVTAXW   DS    CL2                 TAX WORKCODE                                 
SVMGR    DS    C                   MEDIA GROUP                                  
SVMED    DS    C                   MEDIA CODE                                   
SVSCHEME DS    CL8                 SCHEME CODE                                  
*                                                                               
*                                                                               
SVMAJK   DS    CL42                SAVE AREA FOR MAJOR KEY                      
SVMINK   DS    CL42                SAVE AREA FOR MINOR KEY                      
LINEMAX  EQU   9                                                                
SCRIPTEX DS    CL1                 SCRIPT EXECUTION                             
TAXPASS  DS    XL1                                                              
*                                                                               
TABLE    DS    CL(LINEMAX*TBLNQ+1) SCREEN DATA TABLE                            
*                                                                               
       ++INCLUDE ACBATSTAX                                                      
IOAREA   DS    2000C               BUILD POSTING RECORD HERE                    
PROGDX   DS    0C                                                               
         EJECT                                                                  
***********************************************************************         
*              DSECT FOR MAJOR PRICE SEACH TABLE                      *         
***********************************************************************         
*                                                                               
MAJORD   DSECT                                                                  
MAJORK   DS   XL1                 DESCRIBES HOW TO BUILD KEY                    
MOFG     EQU   X'80'               OFFICE GROUP                                 
MOFF     EQU   X'40'               OFFICE                                       
MCLI     EQU   X'20'               CLIENT                                       
MPRO     EQU   X'10'               PRODUCT                                      
MJOB     EQU   X'08'               JOB                                          
         EJECT                                                                  
***********************************************************************         
*              DSECT FOR MINOR PRICE SEACH TABLE                      *         
***********************************************************************         
*                                                                               
MINORD   DSECT                                                                  
MINORK   DS    XL1                 DESCRIBES HOW TO BUILD KEY                   
MMGR     EQU   X'80'               MEDIA GROUP                                  
MMED     EQU   X'40'               MEDIA                                        
MWGR     EQU   X'20'               WORK GROUP                                   
MWRK     EQU   X'10'               WORK CODE                                    
         EJECT                                                                  
***********************************************************************         
*              DSECT FOR A SCREEN LINE                                *         
***********************************************************************         
*                                                                               
LIND     DSECT                                                                  
LINUNTH  DS    CL(L'UNTUNTH)                                                    
LINUNT   DS    CL(L'UNTUNT)        UNITS                                        
LINUNTX  DS    CL(L'UNTUNTX)                                                    
LINLVLH  DS    CL(L'UNTLVLH)                                                    
LINLVL   DS    CL(L'UNTLVL)        PRICE LEVEL                                  
LINLVLX  DS    CL(L'UNTLVLX)                                                    
LINWRKH  DS    CL(L'UNTWRKH)                                                    
LINWRK   DS    CL(L'UNTWRK)        WORKCODE                                     
LINWRKX  DS    CL(L'UNTWRKX)                                                    
LINDSCH  DS    CL(L'UNTWRKDH)                                                   
LINDSC   DS    CL(L'UNTWRKD)       WORKCODE DESCRIPTION                         
LINTAXH  DS    CL(L'UNTTAXH)                                                    
LINTAX   DS    CL(L'UNTTAX)        TAX STATUS                                   
LINTAXX  DS    CL(L'UNTTAXX)                                                    
LINPRCH  DS    CL(L'UNTPRCH)                                                    
LINPRC   DS    CL(L'UNTPRC)        UNIT PRICE                                   
LINPRCX  DS    CL(L'UNTPRCX)                                                    
LINTOTH  DS    CL(L'UNTTOTH)                                                    
LINTOT   DS    CL(L'UNTTOT)        TOTAL AMOUNT                                 
LINTOTX  DS    CL(L'UNTTOTX)       TOTAL AMOUNT                                 
LININCH  DS    CL(L'UNTINCH)                                                    
LININC   DS    CL(L'UNTINC)        INCOME ACCOUNT                               
LININCX  DS    CL(L'UNTINCX)                                                    
LINLEN   EQU   *-LIND                                                           
LINNUMQ  EQU   8                   NUMBER OF FIELDS PER LINE                    
         EJECT                                                                  
***********************************************************************         
*              DSECT FOR TABLE OF LINE ITEMS                          *         
***********************************************************************         
*                                                                               
TABLED   DSECT                                                                  
TBCULINE DS    XL1                 CURRENT ITEM NUMBER                          
TBUNT    DS    PL3                 UNITS                                        
TBUNTSTA DS    XL1                                                              
TBHOURS  EQU   X'40'                                                            
TBLVL    DS    CL1                 PRICE LEVEL - A,B,C OR D                     
TBWGR    DS    C                   WORK GROUP                                   
TBWRKD   DS    CL15                WORKCODE DESCRIPTION                         
TBTAX    DS    CL2                 TAX STATUS                                   
TBPRC    DS    PL5                 UNIT PRICE                                   
TBWRK    DS    CL2                 WORKCODE                                     
TBTOT    DS    PL6                 TOTAL AMOUNT                                 
TBINC    DS    CL15                INCOME ACCOUNT                               
TBINCN   DS    CL36                INCOME ACCOUNT NAME                          
TB12ACT  DS    CL15                12 ACCOUNT                                   
TB12NAME DS    CL36                12 ACCOUNT NAME                              
TBLNQ EQU      *-TABLED                                                         
         EJECT                                                                  
*ACBATDSECT                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBATDSECT                                                     
         PRINT ON                                                               
         ORG   BASOLY2H                                                         
         EJECT                                                                  
       ++INCLUDE ACBATF2D                                                       
         ORG   OSVALS                                                           
* TAX OVERLAY (ACBAT40) USES OSVALS+200                                         
*                                                                               
USERL    EQU   OSVALSL-(OSVALSL-200)                                            
USERAREA DS    0F                                                               
TAXTOT   DS    PL6                 TAXABLE POSTING AMOUNT                       
         DS    CL(USERL-(*-USERAREA))                                           
         EJECT                                                                  
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*FAUTL                                                                          
       ++INCLUDE FAUTL                                                          
*ACGENDAY                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072ACBAT3E   10/16/18'                                      
         END                                                                    
