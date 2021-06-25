*          DATA SET MONTAXS    AT LEVEL 157 AS OF 04/30/86                      
*PHASE MONTAX,*                                                                 
*INCLUDR ILBOSRV0                                                               
*INCLUDE SCANNER                                                                
MONTAX   CSECT                                                                  
         PRINT NOGEN                                                            
         TITLE 'MONTAX - TAX CALCULATION CONTROLLER'                            
         NMOD1 0,*MONTAX*,R7                                                    
         L     RA,0(R1)             ARRAY                                       
         USING ACWDD,RA                                                         
         ST    RA,ARRAYA                                                        
         L     R9,4(R1)                                                         
         CLC   =C'CARDS',0(R9)      DATA FROM CARDS FOR TESTING                 
         BNE   NORM                                                             
         MVI   CARDS,C'Y'                                                       
         LA    RA,ARRAY             LOAD PHONEY ARRAY FOR CARD                  
         L     R6,8(R1)             ADDRESS OF CARD                             
NORM     CLI   FIRST,C'N'                                                       
         BE    NOTFRST                                                          
         MVI   FIRST,C'N'                                                       
         GOTO1 =V(ILBOSTP0)                                                     
         LOAD  EP=ATSEGUE                                                       
         ST    R0,ATSEGUE                                                       
         PERF  FILAGREE            FILL RECIP AGREEMENT TABLE                   
NOTFRST  CLI   CARDS,C'Y'          GO TO CARD ROUTINE IF REQUESTED              
         BE    PROCCARD                                                         
         B     NORMTAX                                                          
         EJECT                                                                  
*                  NORMAL ROUTINE FOR REAL ARRAY OF TAX DATA....                
         SPACE 2                                                                
NORMTAX  CLC   0(5,R9),=C'CLOSE'   IS IT A CLOSE REQUEST                        
         BE    CLSIT                                                            
         L     RA,ARRAYA                                                        
         MVC   FEDYTDE,ACWDYTDE    SAVE FED YTD FOR FREQRT TEST                 
         PERF  FNDEMP              GET EMPLOYER, SOW, SOR & DEBT STATE          
         L     RA,ARRAYA                                                        
         B     LOOPB               GO PROCESS ENTRY                             
         SPACE 1                                                                
LOOPA    PERF  UPARRAY             GO TO NEXT ENTRY                             
         BNE   PUTBKNTX            ALL DONE, GO PUT BACK THE NONTAX             
LOOPB    MVC   ARRAY,0(RA)         MOVE TO WORK AREA                            
         LA    R8,ARRAY            EVERYTHING IS DONE THERE YOU KNOW            
         SPACE 1                                                                
SEGUE    DS    0H                                                               
         L     RF,ATSEGUE          LOAD ATSEGUE ADDRESS                         
         LA    R8,ARRAY            LOAD WORK AREA ADDRESS                       
         CALL  (15),((8))          CALL ATSEGUE                                 
         CLC   ACWDUNIT,=C'CLOS'   WAS IT A CLOSE REQUEST                       
         BE    DILBOSTT            IF SO WE'RE ALL DONE                         
         MVI   ATOPEN,C'Y'         INDICATE ENTRY INTO ALLTAX                   
         MVC   0(68,RA),ARRAY      MOVE WORK AREA TO REAL ARRAY                 
         CLC   ACWDUNIT,=C'0000'   IF THIS ISN'T FEDERAL                        
         BE    NOXCUN              AND                                          
* ****** PERF  FCBHOOK3                                                         
         CLC   ACWDUNIT,SOW        IF THIS ISN'T SOW                            
         BE    NOXCUN                                                           
         XC    ACWDUNMP,ACWDUNMP   ZERO OUT UNEMP                               
         SPACE 1                                                                
NOXCUN   PERF  FICALOOK                                                         
         PERF  ETROUTE             GO DO RES ALIEN                              
         PERF  FREQRT              CHG IN FREQ CHECK AND RETRY                  
         B     LOOPA                                                            
         SPACE 1                                                                
CLSIT    LA    RA,ARRAY                                                         
         CLI   ATOPEN,C'Y'         WAS ALLTAX EVER ENTERED                      
         BNE   XMOD2               NO..THEN DON'T CLOSE OR YOU'LL BLOW          
         MVC   ACWDUNIT,=C'CLOS'                                                
         B     SEGUE                                                            
         SPACE 3                                                                
PUTBKNTX L     RA,ARRAYA           WE GOTTA PUT BACK THE NON TAX AMOUNT         
PBNT2    CLI   0(RA),X'FF'         JUST LIKE WE TOOK IT OUT, DONTCHA            
         BE    RECIPS                                            KNOW.          
         MVC   FULL,ACWDEARN                                                    
         L     RF,FULL                                                          
         MVC   FULL,ACWDNTAX                                                    
         L     R1,FULL                                                          
         AR    RF,R1                                                            
         ST    RF,FULL                                                          
         MVC   ACWDEARN,FULL                                                    
         LA    RA,68(RA)                                                        
         B     PBNT2                                                            
         EJECT                                                                  
*        THE ROUTINE FINDS EMPLOYER, CHECK DATE, STATES OF WORK AND             
*        RESIDENCE, LOCALITIES OF WORK AND RESIDENCE, CHECKS FOR                
*        DEBT STATE, AND ADJUSTS CURRENT EARNINGS AND YTD EARNINGS              
*        BY THE NONTAXABLE CURRENT PAY BECAUSE TAXES ARE CALCULATED             
*        AGAINST ONLY TAXABLE INCOME REPORTED.  THE NON TAXABLE IS              
*        ADDED BACK IN AFTER WE FINISH THE ENTIRE ARRAY AND BEFORE              
*        THE RECIPROCALS ARE DONE.                                              
         SPACE 1                                                                
FNDEMP   NTR1                                                                   
         MVI   DPS,C'N'                                                         
*        TM    ACWDSWIT,X'01'             IF SWITCH SAYS TREAT LIKE             
*        BNO   *+8                        DPS FOR VARIOUS TAX ADJUSTING         
         MVI   DPS,C'Y'                   TELL THE ROUTINES. (CHANGED           
*                                         TO DO FOR EVERYBODY 9/12/85,          
*                                         PER JD AND TF)                        
         PERF  NTXADJ                                                           
FEUP     LA    RA,68(RA)                                                        
         CLI   0(RA),X'FF'                                                      
         BE    DNX                                                              
         PERF  NTXADJ                                                           
         B     FEUP                                                             
DNX      MVC   SVEMPDT,1(RA)                                                    
         CLC   SVEMPDT+8(3),CHGEMP1       CHANGE EMPLOYER FOR SPECIAL           
         BNE   *+10                       TEST RUN.                             
         MVC   SVEMPDT+8(3),CHGEMP2                                             
         MVC   SOWADD,=F'0'                                                     
         MVC   SORADD,=F'0'                                                     
         MVC   LOWADD,=F'0'                                                     
         MVC   LORADD,=F'0'                                                     
         MVC   SOW,SPACES                                                       
         MVC   SOR,SPACES                                                       
         MVC   SVDEBT,=C'    '                                                  
         MVC   HOOKSTAT,=C'    '                                                
FSOW     SH    RA,=H'68'                GO BACK TO LAST ENTRY                   
         CLC   ACWDUNIT,=C'0000'        AND IF IT'S FEDERAL                     
         BNE   SESOW                    DON'T LOKK FOR SOW, ETC.                
* ****** PERF  FCBHOOK1                 IF IT'S FCB NEED DEBT ST ENTRY          
         B     XIT                                                              
SESOW    CLC   ACWDUNIT+2(2),=C'00'                                             
         BE    TKSOW                                                            
         B     FSOW                                                             
TKSOW    ST    RA,SOWADD                                                        
         ST    RA,SORADD                                                        
         MVC   SOW,ACWDUNIT                                                     
         MVC   SOR,ACWDUNIT                                                     
FSOR     SH    RA,=H'68'                                                        
         CLC   ACWDUNIT,=C'0000'                                                
         BE    LOCALSOW                                                         
         CLC   ACWDUNIT+2(2),=C'00'                                             
         BE    TKSOR                                                            
         B     FSOR                                                             
         SPACE 1                                                                
TKSOR    ST    RA,SORADD                                                        
         MVC   SOR,ACWDUNIT                                                     
FSORL    LA    RA,68(RA)                                                        
         CLI   0(RA),X'FF'                                                      
         BE    LOCALSOW                                                         
         CLC   ACWDUNIT+2(2),=C'00'                                             
         BNE   SORLOCAL                                                         
         B     FSORL                                                            
         SPACE 1                                                                
SORLOCAL ST    RA,LORADD                                                        
LOCALSOW L     RA,SOWADD                                                        
FSOWL    LA    RA,68(RA)                                                        
         CLI   0(RA),X'FF'                                                      
         BE    DSTATE                                                           
         CLC   ACWDUNIT+2(2),=C'00'                                             
         BNE   SOWLOCAL                                                         
         B     FSOWL                                                            
SOWLOCAL ST    RA,LOWADD                                                        
DSTATE   MVC   SVDEBT,=C'    '                                                  
* ****** PERF  FCBHOOK2                                                         
         LA    R8,DEBT                                                          
DSC      CLI   0(R8),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(4,R8),SOW                                                      
         BNE   UDS                                                              
         CLC   4(3,R8),SVEMPDT+8                                                
         BE    TDS                                                              
UDS      LA    R8,7(R8)                                                         
         B     DSC                                                              
TDS      MVC   SVDEBT,0(R8)                                                     
XIT      XIT1                                                                   
         EJECT                                                                  
FCBHOOK1 NTR1                                                                   
         CLC   =C'FCB',SVEMPDT+8   IF IT'S FCB WITH NO STATE ENTRY              
         BNE   XIT                                                              
         MVC   SVDEBT,=C'1200'     MAKE IT LOOK LIKE IL FOR FUT                 
         B     XIT                                                              
         SPACE 2                                                                
FCBHOOK2 NTR1                                                                   
         MVC   HOOKSTAT,=C'    '                                                
         CLC   =C'FCB',SVEMPDT+8   IF FCB ANY ANY STATE OTHER THAN              
         BNE   XIT                 IL, NY OR CA                                 
         CLC   SOW,=C'1200'                                                     
         BE    XIT                                                              
         CLC   SOW,=C'3100'                                                     
         BE    XIT                                                              
         CLC   SOW,=C'0400'                                                     
         BE    XIT                                                              
         MVC   HOOKSTAT,SOW        SAVE SOW FOR REPLACEMENT                     
         MVC   SOW,=C'1200'        AND CALCULATE TAXES ON IL                    
         B     XIT                                                              
         SPACE 2                                                                
FCBHOOK3 NTR1                                                                   
         CLC   =C'FCB',SVEMPDT+8   IF IT'S FCB AND                              
         BNE   XIT                                                              
         CLC   HOOKSTAT,=C'    '   IF THERE'S A HOOKSTAT                        
         BE    XIT                                                              
         CLC   ACWDUNIT,=C'1200'   AND THE TAX UNIT IS IL                       
         BNE   XIT                                                              
         MVC   ACWDUNIT,SOW        PUT BACK SOW AS UNIT                         
         XC    ACWDTAX,ACWDTAX     GET RID OF EVERYTHING BUT SUT                
         XC    ACWDFICA,ACWDFICA                                                
         XC    ACWDFICR,ACWDFICR                                                
         B     XIT                                                              
         EJECT                                                                  
*                NTXADJ REDUCES THE GROSSES WITH NON TAXABLE INCOME             
NTXADJ   NTR1                                                                   
         MVC   FULL,ACWDEARN                                                    
         L     RF,FULL                                                          
         MVC   FULL,ACWDNTAX                                                    
         L     R1,FULL                                                          
         SR    RF,R1                                                            
         ST    RF,FULL                                                          
         MVC   ACWDEARN,FULL                                                    
         B     XIT                                                              
         EJECT                                                                  
         SPACE 2                                                                
*                        ROUTINE UPS ARRAY REGISTER AND SAYS NO AT END          
         SPACE 1                                                                
UPARRAY  NTR1                                                                   
         LA    RA,68(RA)                                                        
         CLI   0(RA),X'FF'                                                      
         BE    NO                                                               
         B     YES                                                              
         ANSR  X=N                                                              
         XIT1  REGS=(RA)                                                        
         EJECT                                                                  
*             R E C I P R O C A L    A G R E E M E N T S                        
*                                                                               
*        IT IS IMPORTANT TO KNOW THAT THE GROSS AMOUNTS HERE ARE                
*        ADJUSTED BACK TO INCLUDE THE NONTAXABLE AMOUNTS.  SO IN                
*        FUTURE RECIPROCALS, SHOULD IT BECOME NECESSARY TO REENTER              
*        THE TAX CALCS, THE GROSS MUST BE ADJUSTED (CURR) BACK                  
*        TO TAXABLE BY SUBTRACTING OUT THE NONTAXABLE AMOUNT AND                
*        ADDING BACK IN WHEN ATSEGUE GIVES IT BACK.  JUST WORRY ABOUT           
*        THE CURRENT EARNINGS, YEAR TO DATE HAS BEEN TAKEN CARE OF IN           
*        THE CALLING PROGRAM AND DOES NOT HAVE TO BE MODIFIED.                  
         SPACE 2                                                                
RECIPS   DS    0H                                                               
         L     RA,ARRAYA                  TIP TOE THRU THE ARRAY AND            
RC0      CLC   ACWDEXS,=C'99'             99 EXEMPTIONS ZEROS TAX               
         BNE   RC01                       INCOME TAX ONLY, NOTHING ELSE         
         MVC   ACWDTAX,=F'0'                                                    
RC01     LA    RA,68(RA)                                                        
         CLI   0(RA),X'FF'                                                      
         BE    RC02                                                             
         B     RC0                                                              
         SPACE 3                                                                
*        CHECK TO SEE IF WE LIVE IN SOW AND SOR AND FOR ANY STATE WE            
*        DO NOT LIVE IN WE MUST CANCEL TAXES IN THE STATE AND MATCHING          
*        LOCAL. WE MUST CANCEL RECIPROCALS TOO SINCE THERE ARE NOT TWO          
*        STATES TO CALCULATE RECIPRICOL AMOUNTS WITH.                           
         SPACE 1                                                                
RC02     MVI   RECIPSW,C'Y'        BE OPTIMISTIC ABOUT RECIPS TO START          
         CLC   SOWADD,=F'0'        IF NO SOW ADDR, GET OUT THERE ARE NO         
         BE    XMOD                STATES OR LOCALS AT ALL.                     
         LA    R9,LIVEIN           GO THRU EMPR/STATE TBL TO SEE IF             
RC02A    CLC   0(3,R9),SVEMPDT+8   THIS EMPLOYER LIVES IN THE SOW               
         BNE   RC02B               NO                                           
         CLC   3(4,R9),SOW                                                      
         BE    RC02C               YES                                          
RC02B    LA    R9,8(R9)                                                         
         CLI   0(R9),X'FF'         NOT ON                                       
         BNE   RC02A                                                            
         MVI   RECIPSW,C'N'        CANCEL RECIPS                                
         L     RA,SOWADD           GO TO SOW ENTRY                              
         XC    ACWDTAX,ACWDTAX     WE DON'T LIVE HERE SO ZERO SWT               
         XC    ACWDFICA,ACWDFICA   DISABILITY                                   
         XC    ACWDUNMP,ACWDUNMP   AND UNEMPLOYMENT                             
         L     RA,LOWADD           GET RID OF LOCALS ALSO                       
         LTR   RA,RA                                                            
         BZ    RC02C                                                            
         XC    ACWDTAX,ACWDTAX                                                  
RC02C    LA    R9,LIVEIN           NOW SOR                                      
         CLC   SOW,SOR             BUT FIRST IS SOW = SOR GET OUT               
         BE    XMOD                'CAUSE TAXES HAVE BEEN ADJUSTED AS           
*                                  SOW AND NO RECIPS ARE NECESSARY.             
RC02D    CLC   0(3,R9),SVEMPDT+8   CHECK THE LIVE-IN TABLE FOR SOR              
         BNE   RC02E               FOR THIS EMPLOYER                            
         CLC   3(4,R9),SOR                                                      
         BE    RC02G                                                            
RC02E    LA    R9,8(R9)                                                         
         CLI   0(R9),X'FF'                                                      
         BNE   RC02D                                                            
RC02F    MVI   RECIPSW,C'N'        CANCEL RECIPS                                
         L     RA,SORADD           CANCEL ALL SOR TAXES                         
         XC    ACWDTAX,ACWDTAX                                                  
         XC    ACWDFICA,ACWDFICA                                                
         XC    ACWDUNMP,ACWDUNMP                                                
         L     RA,LORADD           AND MAYBE LOCALS ALSO                        
         LTR   RA,RA                                                            
         BZ    XMOD                                                             
         XC    ACWDTAX,ACWDTAX                                                  
         B     XMOD                                                             
RC02G    CLI   7(R9),C'W'          IF THIS SWITCH IS A W, WE CAN ONLY           
         BE    RC02F               TAX THIS STATE IF IT IS SOW YOU SEE,         
*                                  SO SINCE THIS IS SOR WE HAVE TO ZERO         
*                                  THEM OUT.                                    
         B     RC03                                                             
         SPACE 2                                                                
RC03     LA    R9,AGREE            NOW RELATIONSHIP OF SOW TO SOR               
         CLI   RECIPSW,C'Y'        WERE RECIPROCALS CREAMED?                    
         BNE   XMOD                YES, GO AWAY                                 
         USING AGRMTD,R9                                                        
RC1      CLC   SOW,AGSOW           NO, GO AHEAD                                 
         BNE   RC2                                                              
         CLC   SOR,AGSOR                                                        
         BNE   RC2                                                              
         B     RC3                                                              
RC2      LA    R9,15(R9)                                                        
         CLI   0(R9),X'FF'                                                      
         BE    XMOD               SOW/SOR COMBO NOT ON EVERYTHING STAYS         
         B     RC1                SOME DO YOU KNOW.                             
RC3      LA    R8,ROUTES                                                        
RC4      CLC   AGRULE,0(R8)                                                     
         BE    RC5                                                              
         LA    R8,12(R8)                                                        
         CLI   0(R8),X'FF'                                                      
         BE    XMOD                RULE NOT ON EVERYTHING STAYS                 
         B     RC4                                                              
RC5      L     RF,8(R8)                                                         
         BR    RF                                                               
         SPACE 3                                                                
DIFF     L     RA,SOWADD                                                        
         MVC   FULL,ACWDTAX        GET SOW TAX                                  
         L     R2,FULL                                                          
         L     RA,SORADD                                                        
         MVC   FULL,ACWDTAX        GET SOR TAX                                  
         L     R3,FULL                                                          
         LTR   R3,R3               MINUS TAX CALC'D                             
         BM    MINUS                                                            
         SR    R3,R2               SOR MINUS SOW                                
         BNM   PLUS                                                             
         SR    R3,R3               MINUS DIFF GETS ZERO THEY DON'T LIKE         
         B     PLUS                NEGATIVE TAXES YOU KNOW                      
         SPACE 1                                                                
MINUS    SR    R3,R2               REVERSE EXCESS OF SOR OVER SOW TEST          
         BNP   PLUS                FOR A NEGATIVE AMOUNT                        
         SR    R3,R3               PLUS DIFFERENCES GET ZERO                    
         SPACE 1                                                                
PLUS     ST    R3,FULL                                                          
         MVC   ACWDTAX,FULL        STICK IT IN YOUR SOR BUCKET                  
         MVC   ACWDFICA,=F'0'      ZERO ALL OTHERS                              
         MVC   ACWDUNMP(12),ACWDFICA                                            
         CLI   AGLOCAL,C'D'        ARE WE TO DELETE LOCAL                       
         BNE   XMOD                                                             
         L     RA,LORADD                                                        
         LTR   RA,RA                                                            
         BZ    XMOD                NO LOCAL                                     
         MVC   ACWDTAX,=F'0'                                                    
         B     XMOD                                                             
         SPACE 3                                                                
NILSOR   L     RA,SORADD           ZERO ALL SOR TAX                             
         MVC   ACWDTAX,=F'0'                                                    
         MVC   ACWDFICA(16),ACWDTAX                                             
         CLI   AGLOCAL,C'D'                                                     
         BNE   XMOD                                                             
         L     RA,LORADD                                                        
         LTR   RA,RA                                                            
         BZ    XMOD                                                             
         MVC   ACWDTAX,=F'0'                                                    
         B     XMOD                                                             
         SPACE 3                                                                
NILSOW   L     RA,SOWADD           ZERO ALL SOW TAX                             
         MVC   ACWDTAX,=F'0'                                                    
         MVC   ACWDFICA(16),ACWDTAX                                             
         CLI   AGLOCAL,C'D'                                                     
         BNE   XMOD                                                             
         L     RA,LOWADD                                                        
         LTR   RA,RA                                                            
         BZ    XMOD                                                             
         MVC   ACWDTAX,=F'0'                                                    
         B     XMOD                                                             
         EJECT                                                                  
*                   ROUTINE FILLS RECIP AGREEMENT AND DEBT STATE TABLES         
*                   ALSO LOADS FICA LIMIT TABLE AND EMPLR/STATE TABLE.          
FILAGREE NTR1                                                                   
         OPEN  (ATRATBL,(INPUT))                                                
         LA    R9,AGREE                                                         
         LA    R8,DEBT                                                          
         LA    R3,FICATB                                                        
         LA    R4,LIVEIN                                                        
GTNXT    GET   ATRATBL,ATTBL                                                    
         CLC   ATTBL(11),=C'FICA LIMIT='                                        
         BE    FICALIM                                                          
         CLC   ATTBL(11),=C'DEBT STATE='                                        
         BE    DEBTST                                                           
         CLC   ATTBL(3),=C'ES='         EMPLR/STATE INST.                       
         BE    ESLOGIC                                                          
         CLC   ATTBL(7),=C'CHANGE='     EMPLOYER ID CHANGE                      
         BE    CHGEM                                                            
         CLC   ATTBL(4),=C'SOW='        RECIPROCAL INST.                        
         BNE   GTNXT                                                            
         SPACE 1                                                                
         GOTO1 =V(SCANNER),DMCB,(=C'C',ATTBL),(4,SBLOCKS),0                     
         ZIC   R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    GTNXT                                                            
         LA    R5,SBLOCKS                                                       
         USING SCAND,R5                                                         
NXB      CLC   =C'SOW',SFLD1                                                    
         BE    SSOW                                                             
         CLC   =C'SOR',SFLD1                                                    
         BE    SSOR                                                             
         CLC   =C'RULE',SFLD1                                                   
         BE    SRULE                                                            
         CLC   =C'LOCAL',SFLD1                                                  
         BE    SLOCAL                                                           
         B     NXTBLK                                                           
SSOW     MVC   AGSOW,SFLD2                                                      
         B     NXTBLK                                                           
SSOR     MVC   AGSOR,SFLD2                                                      
         B     NXTBLK                                                           
SRULE    MVC   AGRULE,SFLD2                                                     
         B     NXTBLK                                                           
SLOCAL   MVI   AGLOCAL,C'K'                                                     
         CLC   =C'KEEP',SFLD2                                                   
         BE    NXTBLK                                                           
         MVI   AGLOCAL,C'D'                                                     
NXTBLK   LA    R5,32(R5)                                                        
         BCT   R6,NXB                                                           
         LA    R9,15(R9)                                                        
         B     GTNXT                                                            
         SPACE 1                                                                
EOFTBL   MVI   0(R9),X'FF'          TOP OFF ALL OF THE TABLES                   
         MVI   0(R8),X'FF'                                                      
         MVI   0(R3),X'FF'                                                      
         MVI   0(R4),X'FF'                                                      
         CLOSE ATRATBL                                                          
         B     XIT                                                              
         SPACE 1                                                                
DEBTST   MVC   0(7,R8),ATTBL+11                                                 
         LA    R8,7(R8)                                                         
         B     GTNXT                                                            
         SPACE 1                                                                
FICALIM  PACK  DUB,ATTBL+11(6)                                                  
         CLI   0(R3),X'FF'          PREVENT OVERFLOW                            
         BE    GTNXT                                                            
         CVB   R1,DUB                                                           
         ST    R1,0(R3)                                                         
         MVC   4(4,R3),ATTBL+18                                                 
         LA    R3,8(R3)                                                         
         B     GTNXT                                                            
         SPACE 1                                                                
ESLOGIC  MVC   0(3,R4),ATTBL+3                                                  
         CLI   ATTBL+6,C','          BAD ENTRY, DON'T USE IT                    
         BNE   GTNXT                                                            
         MVC   3(4,R4),ATTBL+7                                                  
         MVI   7(R4),C' '                                                       
         CLI   ATTBL+11,C','                                                    
         BNE   ESL2                                                             
         CLI   ATTBL+12,C'W'                                                    
         BNE   ESL2                                                             
         MVI   7(R4),C'W'             SOW ONLY PARM                             
ESL2     LA    R4,8(R4)                                                         
         B     GTNXT                                                            
         SPACE 1                                                                
CHGEM    CLI   ATTBL+10,C'='                                                    
         BNE   GTNXT                                                            
         MVC   CHGEMP1,ATTBL+7                                                  
         MVC   CHGEMP2,ATTBL+11                                                 
         B     GTNXT                                                            
         EJECT                                                                  
*               NONRES ALIEN ROUTINE - WORKS ON REAL ARRAY ENTRY                
         SPACE 1                                                                
ETROUTE  NTR1                                                                   
         CLC   ACWDUNIT,=C'0000'      THESE ALIEN GUYS GET 30% TAX              
         BNE   XIT                    ON FEDERAL ONLY                           
         CLI   ACWDRES,C'N'                                                     
         BNE   XIT                                                              
         MVC   FULL,ACWDEARN                                                    
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         MP    DUB,=P'30'                                                       
         ZIC   R1,3                                                             
ET1      AP    DUB,=P'5'                                                        
         MVO   DUB(8),DUB(7)                                                    
         BCT   R1,ET1                                                           
         CVB   R1,DUB                                                           
         ST    R1,FULL                                                          
         MVC   ACWDTAX,FULL                                                     
         B     XIT                                                              
         EJECT                                                                  
*  WE MAY HAVE TO CHANGE FEQUENCY CODE AND RETRY ALLTAX BUT THE DIS.            
*  WILL STAY THE SAME.                                                          
         SPACE 1                                                                
*  THIS ROUTINE WORKS ON THE REAL ENTRY.                                        
         SPACE 1                                                                
FREQRT   NTR1                                                                   
         CLI   DPS,C'Y'            SHOULD WE TREAT THIS LIKE DPS                
         BNE   XIT                 NAH                                          
         CLI   ACWDFREQ,C'1'       ONLY FOR WEEKLY TRIES                        
         BNE   XIT                                                              
         PACK  WORK(3),SVEMPDT+4(2)                                             
         MVC   FULL,ACWDEARN                                                    
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         ZAP   EARN,DUB                                                         
         CP    DUB,=P'99999'       AND ONLY OVER 999.99                         
         BNH   XIT                                                              
         MVC   FULL,FEDYTDE                                                     
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         DP    DUB,WORK(3)                                                      
         CP    DUB(5),EARN         AND ONLY HIGHER THAN MONTHLY AVG             
         BNL   XIT                                                              
         MVI   ACWDFREQ,C'4'       MAKE MONTHLY                                 
         MVC   SVDIS,ACWDFICA      SAVE DIS. FOR STATES                         
         MVC   ARRAY,0(RA)         NEED TO USE ARRAY FOR TALKING TO             
         LA    R8,ARRAY            ALLTAX 'CAUSE OF EMP & DATE AT END           
         L     RF,ATSEGUE                                                       
         CALL  (15),((8))          NOW SNEAK BACK INTO ALLTAX                   
         MVC   0(68,RA),ARRAY      REPLACE THE REAL ENTRY                       
         CLC   ACWDUNIT,=C'0000'   ONLY IF NOT FEDERAL                          
         BE    FICACK                                                           
         MVC   ACWDFICA,SVDIS      PUT BACK THE OLD DISABILITY                  
         CLC   ACWDUNIT,SOW        IF NOT SOW                                   
         BE    XIT                                                              
         XC    ACWDUNMP,ACWDUNMP   ZERO OUT UNEMP                               
         B     XIT                                                              
FICACK   PERF  FICALOOK                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE USES THE FICALIMIT AMOUNT TO CHECK YTD FICA              
*              THAT WE WENT INTO ALLTAX WITH.  IF THE YTD WAS OVER THE          
*              LIMIT THE CURRENT FICA AMOUNT IS ERASED BECAUSE ALLTAX           
*              SCREWS IT UP IN THAT INSTANCE.                                   
         SPACE 1                                                                
FICALOOK NTR1                                                                   
         CLC   ACWDUNIT,=C'0000'                                                
         BNE   XIT                                                              
         LA    R3,FICATB                                                        
FL0      CLI   0(R3),X'FF'       TABLE EMPTY                                    
         BE    XIT               THEN GET OUT                                   
         CLC   SVEMPDT(4),4(R3)  THIS YEAR'S                                    
         BE    FL1               YES                                            
         LA    R3,8(R3)          NO                                             
         B     FL0                                                              
FL1      MVC   FULL,ACWDYTDF                                                    
         L     R1,FULL                                                          
         L     R2,0(R3)                                                         
         CR    R1,R2                                                            
         BL    XIT                                                              
         MVC   ACWDFICA,=F'0'                                                   
         MVC   ACWDFICR,=F'0'                                                   
         B     XIT                                                              
         EJECT                                                                  
*                     FILL INTERFACE DATA FROM CARD AND PERFORM NO              
*                     RECIPROCALS, JUST RETURN RESULTS.                         
         SPACE 1                                                                
PROCCARD MVC   ACWDUNIT,8(R6)                                                   
         CLC   ACWDUNIT,=C'CLOS'                                                
         BE    CLOS1                                                            
         PACK  DUB,12(7,R6)                                                     
         CVB   R1,DUB                                                           
         ST    R1,FULL                                                          
         MVC   ACWDEARN,FULL                                                    
         PACK  DUB,19(7,R6)                                                     
         CVB   R1,DUB                                                           
         ST    R1,FULL                                                          
         MVC   ACWDYTDE,FULL                                                    
         PACK  DUB,26(7,R6)                                                     
         CVB   R1,DUB                                                           
         ST    R1,FULL                                                          
         MVC   ACWDYTDF,FULL                                                    
         MVC   ACWDYTDR,FULL                                                    
CLOS1    MVC   ACWDTAX(24),=6F'0'                                               
         CLC   ACWDUNIT,=C'CLOS'                                                
         BE    CLOS2                                                            
         MVC   ACWDYTDR+4(8),33(R6)    CHECK DATE YYYYMMDD                      
         MVC   ACWDUNIT,8(R6)                                                   
         MVC   ACWDRES,7(R6)                                                    
         MVC   ACWDEXS,5(R6)                                                    
         MVC   ACWDSTAT,4(R6)                                                   
         MVC   ACWDFREQ,44(R6)                                                  
         MVC   SVDEBT,45(R6)                                                    
         MVC   ACWDYTDR+12(3),41(R6)                                            
CLOS2    L     RF,ATSEGUE                                                       
         CALL  (15),((10))                                                      
         CLC   ACWDUNIT,=C'CLOS'                                                
         BE    DILBOSTT                                                         
         PERF  FICALOOK                                                         
         L     R1,ACWDTAX                                                       
         CVD   R1,DUB                                                           
         ZAP   35(7,R6),DUB                                                     
         L     R1,ACWDFICA                                                      
         CVD   R1,DUB                                                           
         ZAP   42(7,R6),DUB                                                     
         L     R1,ACWDUNMP                                                      
         CVD   R1,DUB                                                           
         ZAP   49(7,R6),DUB                                                     
         L     R1,ACWDFICR                                                      
         CVD   R1,DUB                                                           
         ZAP   56(7,R6),DUB                                                     
         B     XMOD                                                             
         EJECT                                                                  
XMOD     XMOD1                                                                  
         SPACE 2                                                                
DILBOSTT DELETE EP=ILBOSTT                                                      
XMOD2    MVI   ATOPEN,C'N'      INDICATE NEW START                              
         B     XMOD                                                             
         EJECT                                                                  
         DC    C'XXXXXXXXXXXXXX ACWDD START XXXXXXXXXXXXXX'                     
ARRAY    DC    CL68' '          PICTURE OF ACDW ELEMENT.                        
SVEMPDT  DC    CL11' '          DATE IS 8 (19YYMMDD) THEN EMP FOR 3.            
SVDEBT   DC    CL4' '           DEBT STATE FOR FUT RATE OVERRIDE.               
         DC    C'XXXXXXXXXXXXXXX ACWDD END XXXXXXXXXXXXXX'                      
DMCB     DS    6F                                                               
FULL     DS    F                                                                
DUB      DS    D                                                                
CARDS    DC    C'N'                                                             
FIRST    DC    C'Y'                                                             
ATOPEN   DC    C'N'                                                             
ATSEGUE  DS    A                                                                
ILBDSET0 DS    A                                                                
ARRAYA   DS    A                                                                
SOWADD   DS    F                ADDRESS OF STATE OF WORK                        
SORADD   DS    F                                    RESIDENCE                   
LOWADD   DS    F                           LOCALITY OF WORK                     
LORADD   DS    F                                       RESIDENCE                
SOW      DS    CL4              STATE OF WORK CODE                              
SOR      DS    CL4              STATE OF RESIDENCE CODE                         
HOOKSTAT DC    CL4' '                                                           
SPACES   DC    CL80' '                                                          
SVDIS    DS    CL4                                                              
EARN     DS    PL6                                                              
RECIPSW  DS    C                                                                
DPS      DS    C                                                                
WORK     DS    CL20                                                             
         DS    0D                                                               
ROUTES   DC    CL8'DIFF    ',A(DIFF)                                            
         DC    CL8'NILSOR  ',A(NILSOR)                                          
         DC    CL8'NILSOW  ',A(NILSOW)                                          
         DC    X'FF'                                                            
FEDYTDE  DC    F'0'                                                             
CHGEMP1  DC    C'SJR'                                                           
CHGEMP2  DC    C'DPS'                                                           
FICATB   DC    6F'0',X'FF'                                                      
ATRATBL  DCB   DSORG=PS,MACRF=GM,LRECL=80,RECFM=F,BLKSIZE=80,          X        
               DDNAME=ATRATBL,EODAD=EOFTBL                                      
ATTBL    DS    CL80                                                             
SBLOCKS  DS    5CL32                                                            
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
DEBT     DS    10CL7                                                            
         SPACE 1                                                                
LIVEIN   DS    300CL7                                                           
         SPACE 1                                                                
AGREE    DS    CL4              SOW                                             
         DS    CL4              SOR                                             
         DS    CL6              RULE                                            
         DS    CL1              LOCAL RULING                                    
         DS    500CL15                                                          
         SPACE 1                                                                
AGRMTD   DSECT                                                                  
AGSOW    DS    CL4                                                              
AGSOR    DS    CL4                                                              
AGRULE   DS    CL6                                                              
AGLOCAL  DS    CL1                                                              
         SPACE 1                                                                
SCAND    DSECT                                                                  
SL1      DS    CL1                                                              
SL2      DS    CL1                                                              
SVALD1   DS    CL1                                                              
SVAL2    DS    CL1                                                              
SVALU1   DS    CL4                                                              
SVALU2   DS    CL4                                                              
SFLD1    DS    CL10                                                             
SFLD2    DS    CL10                                                             
         SPACE 1                                                                
*      INCLOOD ACTALENT                                                         
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACTALENT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'157MONTAXS   04/30/86'                                      
         END                                                                    
