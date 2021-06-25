*          DATA SET PPMBC01S   AT LEVEL 088 AS OF 05/01/02                      
*PHASE T41801A,+0,NOAUTO                                                        
*INCLUDE PUBEDIT                                                                
*INCLUDE PPBYOUT                                                                
*INCLUDE PUBFLOAT                                                               
*INCLUDE GETADVC                                                                
*INCLUDE NUMVALC                   ***  NOTE: A DIFFERENT NUMVAL                
*                                                                               
*        CHANGES                                                                
*                                                                               
*  KWAN  04/00  NEW DATA FIELD KEY WORD, UPID                                   
*                                                                               
*  KWAN  03/00  IN LASTIO, PASS DELETED REC AND PROTECT DATA FIELD              
*               DON'T DISPLAY DELETED RECS WHEN LASTIO IS NONE                  
*                                                                               
*  KWAN  03/00  ADD CODES FOR LAST INSERTION ORDER DISPLAY                      
*                                                                               
*  BPLA  02/00  DO CONTRACT ASR CHECK FOR RATE CHANGES                          
*               ALSO CHECK THAT IF A PRODUCT CONTRACT                           
*               IT MUST BE FOR THE RIGHT PRODUCT                                
*                                                                               
*  KWAN  02/00  COPY BRUCE'S BUG FIX ON NET RATE CHANGES                        
*                                                                               
*  KWAN  01/00  PREVENT RATE CHANGES IF BUY IS FULLY PAID BY CHANGING           
*               DATA FIELD TO PROTECTED (CONTROLLED BY BYPROF+7)                
*                                                                               
*  BPLA  9/99   CHANGES FOR RATE CHANGES                                        
*               COPY OF PPMBC01/18  MADE 9/24/99                                
*               FROZEN RATES ARE DISPLAYED PRECEEDED WITH AN F                  
*               IN THE BUY PROGRAM THIS IS A "*", I CAN'T DO THAT               
*               SINCE AN '*' IN MBC MEANS DON'T CHANGE THIS BUY                 
*                                                                               
*  SMYE  8/99   DISALLOW CHANGE OF COST2 FACTOR TO ZERO (AT CHKCH27.)           
*                                                                               
*  KWAN  6/99   UPDATE CHANGE ELEM (X'24') FOR CT, PV AND IMPS CHANGES          
*                                                                               
*  KWAN  6/99   CHECK DOLSW IS C'0' (NO DOLLAR TYPE IS ENTERED)                 
*                                                                               
*  BPLA  5/99   ACTIVATE IMPS FOR ALL AGYS                                      
*                                                                               
*  SMYE  4/99   USE NEW NUMVALC TO ALLOW COMMAS ON INPUT OF PAGE VIEWS,         
*                 CLICK-THRUS, AND IMPRESSIONS (AT CHKCH47.), AND               
*                 INCLUDE COMMAS IN DISPLAY OF THESE (AT SCRND47.)              
*                                                                               
*  KWAN  4/99   ADD CODES FOR IMPRESSION (LIMIT TO SJ ONLY)                     
*                                                                               
*  SMYE  3/99   CALL TO GETINS CHANGED AT SCRND20 TO DISPLAY                    
*                 (CONDITIONALLY) OPEN RATES COST                               
*                                                                               
*  SMYE  2/99   COST2 FACTOR ADDED TO CHANGEABLE DATA                           
*                                                                               
*  SMYE  4/98   PROTECTED STATUS FOR TEST BUYS OF A "FROZEN" CLIENT             
*               PCLTSTAT SAVED AT SVCLPROF+30 FOR USE IN SCRND67                
*                                                                               
*  BPLA  3/98   CHANGE FOR EXPANDED CONIO TO 6000 BYTES FROM 3000               
*                                                                               
*  SMYE  3/98   IN CHKCHA IF PUB LIST PRESENT IN JOB RECORD CHECK               
*               TO VERIFY BUY PUB IS IN PUBLIST RECORD                          
*                                                                               
*  BPLA  1/98   ESTIMATE FILTER MOVED AGAIN                                     
*                                                                               
*  BPLA  1/98   BILLED AND PAID FILTERS ADDED                                   
*               PLUS MOVE ESTIMATE FILTER FIRST                                 
*                                                                               
*  SMYE  10/97  CHANGES FOR "SPECIAL FINANCIAL HANDLING" (SFH)                  
*               INCLUDING 'P' FOR PAID AND/OR 'B' FOR BILLED                    
*               DISPLAYED BEFORE DATE (AT SCRNDIS - SCRND5...)                  
*                                                                               
*  SMYE  10/97  GETINS MADE CORE-RESIDENT                                       
*                                                                               
*  BPLA  10/96  CHANGES FOR PAGE VIEWS AND CLICKTHRUS (WEBSITES)                
*                                                                               
*  SMYE  9/96   ADD CSECT (PEC) TO DISALLOW A "ZZZ" PRODUCT MAKELIVE            
*               IF ANY OF ITS' ALLOCATED PRODUCTS HAVE TEST ESTIMATES           
*                                                                               
*  BPLA  8/96   CHANGES FOR EXPANED CONIO - USED BY GETADVC                     
*               3000 FROM 2000 BYTES                                            
*                                                                               
*  BPLA  7/95   REDISPLAY COST ON AC OR CD CHANGES                              
*                                                                               
*  BPLA  7/95   ADD WILD CARD (*) LOGIC TO SPACE FILTERING                      
*               SPECIAL COMPARE LENGTH IS IN SVSLEN (X'FF' IN NOT               
*               ENTERED)                                                        
*                                                                               
*  BPLA 6/95  SHIP DATE FILTER AND DATA DISPLAY                                 
*                                                                               
* BPLA 6/95  SPACE FILTERING USING SVSPACE                                      
*                                                                               
* BPLA 3/95  IF GNOPT IS 'Y' (DATA=ACNET) THEN GROSS-UP OR NET DOWN             
*            COST OF INSERTIONS BOUGHT WITH A NET RATE (PBDCTYP=N)              
*            ALSO ONLY DISPLAY THOSE INSERTIONS BOUGHT WITH A NET RATE          
*            DISPLAY 'N' FOR BUYS BOUGHT WITH A NET RATE                        
*                                                                               
*   BPLA 3/95      NEW BYPROFILE OPTION (BYPROF+3)                              
*                  Y - MEANS DON'T SKIP TEST BUYS IN THE COMPETITIVE            
*                  BRAND CHECK                                                  
*                                                                               
*   BPLA 1/95      FIX BUG IN ASC - NOT SWITCHING BACK PROPERLY                 
*                                                                               
*   BPLA 1/95      FIX BUG IN AUTO SCHEDULE CHECKING                            
*                  AND CORRECT IT IN COMPETITIVE BRAND CHK AS WELL              
*                                                                               
*   BPLA 1/95      IN COMPETITIVE BRAND CHECKING CHECK SLAVE CLTS               
*                  IF BY PROFILE SAYS TO. (BYPROF+2 = 'Y')                      
*                                                                               
*   BPLA 1/95      ADD COMPETITIVE BRAND CHECKING FOR MAKING BUYS LIVE          
*                                                                               
*   BPLA 6/94      RPT CHANGES                                                  
*                                                                               
*   BPLA 12/7/93   IN ASC - IGNORE BUYS WHOSE SPACE BEGINS WITH "#"             
*                                                                               
*   BPLA 11/11/93  WHEN MAKING LIVE CHECK SVCLPROF+12 TO SEE IF                 
*                  CONTRACTS ARE REQUIRED - NEEDED SINCE NEW VALUE              
*                 "T" ALLOWS ONLY TEST BUYS WITHOUT CONTRACTS                   
*                                                                               
*   BPLA 7/6/93   FIX BUG IN ASC LOGIC (ASC30)                                  
*                                                                               
*   BPLA 7/6/93   OUT2OPT AND SVOUT2 FILTER CHANGES                             
*                                                                               
*   BPLA 6/9/93   AUTO SCHEDULE CHECKING ACORSS ZONES/EDITIONS                  
*                                                                               
*   BPLA 5/21/93  DISPLAY 'T' BEFORE DATE FOR TEST BUYS                         
*                                                                               
*   BPLA 2/2/93   CHANGES FOR REFERENCE NUMBER - CARRIED                        
*                 IN PBREFEL (X'83')                                            
*                                                                               
*   BPLA 1/21/92   SEND ERROR IF FSI JOB ENTERED AND FSI DATA                   
*            CAN'T BE FOUND - WARNING LOGIC LEFT IN BUT NO-OPED                 
*                                                                               
*   BPLA  1/12/93  FIXES TO FSI LOOK-UP                                         
*                                                                               
*   BPLA 12/14/92  FSI LOGIC ADDED                                              
*                                                                               
*   BPLA 6/11/92   PROTECT BILLABLE DATE IF COMMISSION BILLED                   
*                                                                               
*   BPLA 6/8/92    CHANGE DEC TO DLC                                            
*                                                                               
*   BPLA 6/1/92    DON'T PROTEST CD AND AC IF MATCHED AND PAID                  
*                                                                               
*   BPLA 5/12/92   CHANGES FOR DEC                                              
*                                                                               
*   BPLA 4/15/92   PROTECT CD OR AC IF MATCHED                                  
*                  ALSO SPECIAL CHECK FOR OUTDOOR IN ASC400                     
*                  FOR "NOT REAL" BUYS                                          
*                                                                               
*   BPLA 3/6/92    ADD LOGIC FOR AUTOMATIC SCHEDULE CHECKING                    
*                                                                               
*   BPLA 8/20/91   CODE FOR PLANNED COST                                        
*                  CODE FOR PUBLIST FILTER                                      
*   BPLA 7/30/91   CODE FOR MB PROFILE  - COST DISPLAY SUPPRESSION              
*                  PUB NAME                                                     
*                                                                               
         TITLE 'PPMBC01 - PRINTPAK MULTI-BUY DISP/CHA PROCESS'                  
T41801   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41801,RR=R9                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
PUBCLTER EQU   33                MISSING REQUIRED PUB LINK                      
JOBERR1  EQU   215               CAN'T REMOVE ADCODE IF I/O PRODUCED            
JOBERR2  EQU   194               NO INSTRUCTION RECORD                          
JOBERR3  EQU   195               NOT WITHIN JOB DATES                           
JOBERR4  EQU   225               ADCODE MUST AGREE WITH EST JOB CODE            
JOBERR5  EQU   219               PUB LIST RECORD NOT FOUND                      
JOBERR6  EQU   220               PUBLICATION NOT IN PUB LIST                    
FSIERR   EQU   108               CAN'T FIND FSI DATA                            
CLOSERR  EQU   109               CLOSING DATE MUST PRECEED INSERTION            
BDERR    EQU   146               BILLABLE DATE OUTSIDE RANGE                    
PDERR    EQU   150               PAYABLE DATE OUTSIDE RANGE                     
NOTFND   EQU   53                                                               
MISSERR  EQU   1                 MISSING INPUT FIELD                            
*                                                                               
         ST    R9,RELO01                                                        
*                                                                               
         USING T418FFD,RA                                                       
         LA    R8,T41801+4095                                                   
         LA    R8,1(R8)                                                         
         USING T41801+4096,R8     NOTE USE OF R8 AS SECOND BASE                 
*                             *** DON'T TOUCH R8 IN ANY CSECT ***               
*                             *** IT IS NEEDED TO ACCESS ROUTINES               
*                             *** IN THE MAIN PROGRAM                           
*                                                                               
         L     RF,=V(PPBYOUT)                                                   
         A     RF,RELO01                                                        
         ST    RF,VPPBYOUT                                                      
*                                                                               
         L     RF,=V(NUMVALC)                                                   
         A     RF,RELO01                                                        
         ST    RF,VNUMVAL                                                       
*                                                                               
         LA    R6,BUYOUTA                                                       
         USING PPBYOUTD,R6                                                      
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    R2,PBUYREC                                                       
         L     R3,VDATCON                                                       
         LA    R4,GROSS                                                         
         STM   R2,R4,0(R6)                                                      
         MVI   PBYOCTL,X'28'                                                    
         DROP  R6                                                               
*                                                                               
         ZAP   HALF2,=P'0'            SAVE NO. OF DISC ADDRESSES READ           
         LA    R3,SVDISC                                                        
*                                                                               
         CLI   PVSW,0                                                           
         BH    RDBC                                                             
RDBB     XC    SVLSTKEY,SVLSTKEY                                                
         XC    SVDISC,SVDISC                                                    
         B     RDBUYS                                                           
*                                                                               
RDBC     CLI   ACTION,C'D'                                                      
         BNE   RDBD                                                             
         OC    SVLSTKEY,SVLSTKEY                                                
         BZ    RDBUYS                                                           
         B     RDBH                                                             
*                                                                               
RDBD     CLI   PVSW,1                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   ACTION,C'C'                                                      
         BE    RDBD5                                                            
         CLI   ACTION,C'M'               MULTI-CHANGE                           
         BNE   RDBH                                                             
RDBD5    BAS   RE,CHKCHA                                                        
*                                                                               
         CLI   CHASW,0       "SPECIAL" MESSAGE ERROR FOUND IN CHKCHA ?          
         BNH   RDBD5C              NO - CONTINUE                                
         LR    R2,R6               YES                                          
         B     EXIT                EXIT WITH ERROR IN MBCEMSG                   
*                                                                               
RDBD5C   LTR   R3,R3                                                            
         BZ    RDBE                                                             
         LR    R2,R6                                                            
         B     ERROR                                                            
*                                                                               
RDBE     GOTO1 =A(RECUPDT),DMCB,(RC),(RA),RR=RELO01                             
         OC    SVLSTKEY,SVLSTKEY                                                
         BNZ   RDBF                                                             
         MVC   MBCEMSG(31),=C'DATA CHANGED - ACTION COMPLETED'                  
         CLI   WARN,X'80'                                                       
         BNE   RDBE5                                                            
         MVC   MBCEMSG+32(19),=C'*JOB SPACE WARNING*'                           
         B     RDBF0                                                            
*                                                                               
RDBE5    CLI   WARN,X'40'                                                       
         BNE   RDBE10                                                           
         MVC   MBCEMSG+32(17),=C'*JOB FSI WARNING*'                             
         B     RDBF0                                                            
*                                                                               
RDBE10   CLI   WARN,X'C0'          FSI AND SPACE                                
         BNE   RDBF0                                                            
         MVC   MBCEMSG+32(24),=C'*JOB SPACE+FSI WARNINGS*'                      
         B     RDBF0                                                            
*                                                                               
RDBF     MVC   MBCEMSG(36),=C'DATA CHANGED-HIT ENTER FOR NEXT PAGE'             
         CLI   WARN,X'80'                                                       
         BNE   RDBF5                                                            
         MVC   MBCEMSG+37(19),=C'*JOB SPACE WARNING*'                           
         B     RDBF0                                                            
*                                                                               
RDBF5    CLI   WARN,X'40'                                                       
         BNE   RDBE10                                                           
         MVC   MBCEMSG+37(17),=C'*JOB FSI WARNING*'                             
         B     RDBF0                                                            
*                                                                               
RDBF10   CLI   WARN,X'C0'          FSI AND SPACE                                
         BNE   RDBF0                                                            
         MVC   MBCEMSG+37(23),=C'*JOB SPACE+FSI WARNING*'                       
         B     RDBF0                                                            
*                                                                               
RDBF0    XC    MBCACT,MBCACT                                                    
         FOUT  MBCACTH,=C'DISPLAY',7                                            
         MVI   LACTION,C'D'             TO PERMIT SUCESSIVE CHANGES             
*                                       TO THE SAME BUYS                        
         NI    MBCACTH+4,X'DF'          UNVALIDATE ACTION                       
RDBF1    FOUT  MBCEMSGH                                                         
         LA    R2,MBCACTH                                                       
         OI    6(R2),X'40'            POSITION CURSOR HERE                      
         OI    MBCIDH+6,X'81'         CHANGE TO MODIFIED                        
         B     EXIT                                                             
*                                                                               
*                                                                               
RDBH     MVC   KEY,SVLSTKEY           MOVE LAST KEY READ TO KEY.                
         XC    SVLSTKEY,SVLSTKEY                                                
         XC    SVDISC,SVDISC                                                    
         B     RDB09                  BRANCH TO SEQ READ.                       
*                                                                               
RDBUYS   DS    0H                                                               
         MVC   WORKCLT,SVCLT                                                    
         CLI   SVCLT,C'*'          CHK FOR OFFICE                               
         BE    RDB03                                                            
         CLC   SVCLT,=C'ALL'                                                    
         BNE   RDB05                                                            
RDB03    XC    WORKCLT,WORKCLT                                                  
         BAS   RE,GETNCLT          GET FIRST CLIENT                             
         OC    WORKCLT,WORKCLT                                                  
         BZ    RDBXX               NONE FOUND                                   
*                                                                               
RDB05    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
*****                                                                           
         CLC   SVPUB(3),=C'ALL'       IF PUB IS ALL BUILD 20 REC.               
         BNE   RDB08                                                            
         MVI   KEY+3,X'20'                                                      
         MVC   KEY+4(3),WORKCLT                                                 
         CLC   SVPRD,=C'ALL'                                                    
         BE    RDB09                                                            
         MVC   KEY+7(3),SVPRD                                                   
         B     RDB09                                                            
*****                                                                           
RDB08    MVI   KEY+3,X'21'         USE 21 POINTERS                              
         MVC   KEY+4(3),WORKCLT                                                 
         MVC   DUB(6),SVPUB                                                     
         CLI   SVPUB+4,X'FF'       SEE IF DOING ALL ZONES/EDTS                  
         BNE   *+10                                                             
         XC    DUB+4(2),DUB+4                                                   
         MVC   KEY+7(6),DUB                                                     
*                                                                               
         CLC   SVPRD,=C'ALL'                                                    
         BE    *+10                                                             
         MVC   KEY+13(3),SVPRD                                                  
*                                                                               
RDB09    LA    R7,12               FOR KEY COMP                                 
*                                                                               
         CLI   SVPUB+4,X'FF'       CHK FOR ALL ZONES/EDTS                       
         BNE   *+8                                                              
         BCTR  R7,0                COMPARE ONLY 8 DIGIT PUB                     
         BCTR  R7,0                                                             
RDB2     BAS   RE,HIGH                                                          
         B     RDB4                                                             
RDB3     BAS   RE,SEQ                                                           
*****                                                                           
RDB4     OC    KEY+21(3),KEY+21        NO PASSIVE BUYS                          
         BNZ   RDB3                    BRAND POINTER TO ZZZ BUY                 
         TM    KEY+25,X'C0'            NO X'FF''S OF CLOSED OUT                 
         BO    RDB3                                                             
*                                                                               
RDB4A    DS    0H                                                               
         CLC   SVPUB(3),=C'ALL'                                                 
         BNE   RDB4C                                                            
         CLC   KEYSAVE(7),KEY                                                   
         BNE   RDBX                                                             
         CLC   SVPRD,=C'ALL'                                                    
         BE    RDB6                                                             
         CLC   KEY+7(3),SVPRD                                                   
         BE    RDB6                                                             
         B     RDB3                                                             
*****                                                                           
RDB4C    EX    R7,KEYCOMP                                                       
         BNE   RDBX                END OF PUB                                   
         B     RDB5                                                             
*                                                                               
KEYCOMP  CLC   KEYSAVE(0),KEY      EXECUTED                                     
*                                                                               
RDB5     CLC   SVPRD,=C'ALL'                                                    
         BE    RDB6                                                             
         CLC   KEY+13(3),SVPRD                                                  
         BE    RDB6                                                             
         MVC   KEY+16(3),=3X'FF'   BUMP TO NEXT PRD                             
         B     RDB2                                                             
*                                                                               
RDB6     CLI   DATESW,C'I'         IF DATESW IS I CHECK INSERTION DATES         
         BNE   RDB61X                                                           
         CLC   KEY+16(3),SVSTRTB   CHK DATES                                    
         BNL   RDB61A                                                           
         CLI   PRSW,1              SEE IF DOING PRIOR                           
         BE    RDB61X                                                           
         MVC   KEY+16(3),SVSTRTB   READ HIGH FOR THIS DATE                      
         XC    KEY+19(2),KEY+19    CLEAR EST                                    
         B     RDB2                                                             
*                                                                               
RDB61A   CLC   KEY+16(3),SVENDB                                                 
         BNH   RDB61X                                                           
         CLI   SUBSW,1             SEE IF DOING SUBSEQUENT                      
         BE    RDB61X                                                           
         B     RDB3                                                             
*                                                                               
RDB61X   CLC   SVEST,=C'ALL'                                                    
         BE    RDB6A                                                            
         OC    SVESTB,SVESTB            SEE IF DOING ONE ESTIMATE               
         BZ    RDB6A                                                            
         CLC   KEY+19(2),SVESTB          MUST MATCH                             
         BE    RDB6A                                                            
         B     RDB3                     SEQ READ                                
*                                                                               
RDB6A    MVC   SVLSTKEY,KEY         READ BUY RECORD.                            
         BAS   RE,GETREC                                                        
*                                                                               
         TM    KEY+25,X'80'  SEE IF DELETED (MAY GET IF SVIOSW IS ON)           
         BZ    RDB6A0                                                           
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'70'                                                     
         BAS   RE,NEXTEL              WITH DATE EXISTS.                         
         BNE   RDB6A0                 BRANCH AND GET NEXT REC.                  
         OC    2(3,R2),2(R2)          DATE FIELD IN '70' EL.                    
         BNZ   RDB6A0                 BRANCH AND GET NEXT REC.                  
         B     RDB6T                  SKIP IF NO DATED IO FOUND                 
*                                                                               
RDB6A0   LA    R2,PBUYREC+33                                                    
         CLI   0(R2),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                  BAD BUYREC                                 
*                                                                               
         CLI   GNOPT,C'Y'       DATA TYPE = ACNET                               
         BNE   RDB6A1                                                           
         CLI   PBDCTYP,C'N'                                                     
         BNE   RDB6T            SKIP IF NOT BOUGHT WITH "N" RATE                
*                                                                               
RDB6A1   DS    0H                                                               
**RATES**                                                                       
         CLI   PBUYKMED,C'N'                                                    
         BNE   RDB6A1X                                                          
*                                                                               
*                                                                               
         CLI   DATASW,C'M'         INCH RATES                                   
         BNE   RDB6A1L                                                          
         CLI   PBDRCODE,C' '       SKIP RCODE BUYS                              
         BH    RDB6T               FOR NOW?                                     
         CLI   PBDCOSTY,C'U'       UNIT COST MUST BE GIVEN                      
         BNE   RDB6T               THEN SKIP                                    
         CLI   PBDUIND,C'I'         INCHES                                      
         BE    RDB6A1X                                                          
         CLI   PBDUIND,X'89'        INCHES - 2 DECIMALS                         
         BE    RDB6A1X                                                          
         B     RDB6T                IF NOT THEN SKIP                            
*                                                                               
RDB6A1L  DS    0H                                                               
         CLI   DATASW,C'N'         LINE RATES                                   
         BNE   RDB6A1P                                                          
         CLI   PBDRCODE,C' '       SKIP RCODE BUYS                              
         BH    RDB6T               FOR NOW?                                     
         CLI   PBDCOSTY,C'U'       UNIT COST MUST BE GIVEN                      
         BNE   RDB6T               THEN SKIP                                    
         CLI   PBDUIND,C'L'        LINES                                        
         BE    RDB6A1X                                                          
         B     RDB6T             NO THEN SKIP                                   
*                                                                               
RDB6A1P  DS    0H                                                               
         CLI   DATASW,C'$'       TOTAL RATES                                    
         BNE   RDB6A1X                                                          
         CLI   PBDRCODE,C' '       SKIP RCODE BUYS                              
         BH    RDB6T               FOR NOW?                                     
         CLI   PBDCOSTY,C'U'     CAN'T BE UNIT COST                             
         BE    RDB6T             SKIP IF IS                                     
RDB6A1X  DS    0H                                                               
**RATES**                                                                       
         OC    SVREF,SVREF           CHK REF NUMBER FILTER                      
         BZ    RDB6A2C       NONE - GO CHECK FOR T/S STATUS FILTER              
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'83'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   RDB6A2                                                           
         CLI   SVREF,X'FF'    SEE IF LOOKING FOR BUYS WITHOUT REF               
         BE    RDB6T                SKIP IF HAS REF                             
         MVC   WORK(10),2(R2)                                                   
         OC    WORK(10),=CL10' '                                                
         CLC   SVREF,WORK        SEE IF MATCHES REF FILTER                      
         BE    RDB6A2C                                                          
         B     RDB6T                SKIP                                        
*                                                                               
*        GET HERE IF THERE IS NO REF ELEM                                       
*                                                                               
RDB6A2   CLI   SVREF,X'FF'   SEE IF LOOKING FOR BUYS WITHOUT REF                
         BE    RDB6A2C                                                          
         B     RDB6T        SKIP                                                
*                                                                               
RDB6A2C  DS    0H                                                               
         OC    SVTSTAT,SVTSTAT       T/S STATUS FILTER                          
         BZ    RDB6A4                                                           
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'95'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   RDB6A2E                                                          
         USING PTSHTEL,R2                                                       
         CLC   PTSHSTAT,SVTSTAT                                                 
         BE    RDB6A4          MATCHES                                          
         B     RDB6T                SKIP                                        
*                                                                               
*        GET HERE IF THERE IS NO T/S ELEM                                       
*                                                                               
RDB6A2E  CLI   SVTSTAT,C' '  SEE IF LOOKING FOR BUYS WITH T/S STATUS            
         BE    RDB6A4        BLANK OR MISSING                                   
         B     RDB6T        SKIP                                                
*                                                                               
         DROP  R2                                                               
*                                                                               
RDB6A4   CLI   DATASW,C'C'           SEE IF DOING CU=                           
         BNE   RDB6A5                                                           
         CLI   PBDSPACE,C'*'        SKIP * BUYS                                 
         BE    RDB6T                                                            
*                                                                               
RDB6A5   OC    SVADC,SVADC          CHK ADCODE FILTER                           
*****    BZ    RDB6C                                                            
         BZ    RDB6B                                                            
         CLC   SVADC(6),=C'NONE  '     SEE IF LOOKING FOR NO ADCODES            
*****    BNE   RDB6B                                                            
         BNE   RDB6A6                                                           
         OC    PBDJOB,PBDJOB                                                    
*****    BZ    RDB6C                                                            
         BZ    RDB6B                                                            
         CLC   PBDJOB,=6C' '                                                    
*****    BE    RDB6C                                                            
         BE    RDB6B                                                            
         B     RDB6T                                                            
*                                                                               
*****RDB6B    MVC   WORK(6),SVADC                                               
RDB6A6   MVC   WORK(6),SVADC                                                    
         OC    WORK(6),=6C' '                                                   
         CLC   PBDJOB,WORK                                                      
         BNE   RDB6T                                                            
*                                                                               
RDB6B    DS    0H           CHECK FOR SFH (SPECIAL FINANCIAL HANDLING)          
         CLI   DATASW,C'X'         SFH DISPLAY ?                                
         BNE   RDB6B2              NO                                           
         TM    PBDSTAT,X'04'       BOUGHT AS SFH ?                              
         BZ    RDB6T               NO - SKIP THIS BUY                           
*                                                                               
RDB6B2   OC    SVSFH,SVSFH         SFH FILTER ?                                 
         BZ    RDB6C               NO                                           
         TM    PBDSTAT,X'04'       BOUGHT AS SFH ?                              
         BZ    RDB6T               NO - SKIP THIS BUY                           
         CLI   SVSFH,C'H'          LOOKING FOR (H)OLD ?                         
         BNE   RDB6B4              NO                                           
         TM    PBDSTAT,X'08'       BUY "HELD" ?                                 
         BZ    RDB6T               NO - SKIP THIS BUY                           
         B     RDB6C                                                            
*                                  LOOKING FOR (R)ELEASED                       
RDB6B4   TM    PBDSTAT,X'08'       BUY "RELEASED" ?                             
         BNZ   RDB6T               NO - SKIP THIS BUY                           
*                                                                               
RDB6C    OC    SVSTAT,SVSTAT         CHK FOR STATUS FILTER                      
         BZ    RDB6C1                                                           
         CLI   SVSTAT,C'T'                                                      
         BNE   RDB6C0                                                           
         CLI   PBDBFD,C'T'                                                      
         BE    RDB6C1                                                           
         B     RDB6T                SKIP THIS BUY                               
*                                                                               
RDB6C0   CLI   SVSTAT,C'L'                                                      
         BNE   RDB6C1                                                           
         CLI   PBDBFD,C'T'                                                      
         BE    RDB6T                SKIP TEST BUYS                              
*                                                                               
RDB6C1   OC    SVPUBL,SVPUBL       CHK PUB LIST FILTER                          
         BZ    RDB6C2                                                           
         CLC   PBDLIST,SVPUBL                                                   
         BNE   RDB6T               NO MATCH SKIP THIS BUY                       
*                                                                               
RDB6C2   CLI   SVNV,0              CHK FOR LETTER FILTER                        
         BNE   RDB6C2C                                                          
         CLI   DATASW,C'L'         OR NV DISPLAY                                
         BNE   RDB6C4                                                           
RDB6C2C  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'                                                     
         BAS   RE,NEXTEL                                                        
         OC    2(3,R2),2(R2)       CHK FOR PAY ELEM WITH DATE                   
         BNZ   RDB6T               YES - SKIP THIS BUY                          
         CLI   SVNV,C'N'                                                        
         BNE   RDB6C3                                                           
         TM    PBDLETR,X'01'                                                    
         BO    RDB6C4                                                           
         B     RDB6T                SKIP THIS BUY                               
*                                                                               
RDB6C3   CLI   SVNV,C'Y'                                                        
         BNE   RDB6C4                                                           
         TM    PBDLETR,X'01'                                                    
         BO    RDB6T                SKIP NO-LETTER BUYS                         
*                                                                               
RDB6C4   DS    0H                                                               
         OC    SVOUT2,SVOUT2                                                    
         BZ    RDB6C4S              FIND FIRST COMMENT                          
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'66'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   RDB6T       SKIP THIS BUY                                        
         CLI   1(R2),19    COMMENT MUST BE NO LONGER THAN 17 BYTES              
         BH    RDB6T       SKIP THIS BUY                                        
         CLI   1(R2),2     COMMENT ELEM MUST HAVE DATA                          
         BNH   RDB6T       SKIP THIS BUY                                        
         XC    WORK(20),WORK                                                    
         ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),2(R2)                                                    
         OC    WORK(20),=CL20' '                                                
*                                                                               
         ZIC   R1,SVOUT2L           LENGTH OF FILTER                            
         BCTR  R1,0                                                             
         EX    R1,COMOUT                                                        
         BNE   RDB6T                                                            
         BE    RDB6C4X              CAN SKIP SPACE FILTER CHECK                 
*                                   SINCE I CAN'T HAVE BOTH                     
*                                                                               
COMOUT   CLC   SVOUT2(0),WORK       EXECUTED                                    
*                                                                               
RDB6C4S  DS    0H                                                               
         OC    SVSPACE,SVSPACE      SEE IF I HAVE A SPACE FILTER                
         BZ    RDB6C4X                                                          
*                                                                               
*              GO TO GETINS BEFORE PPBYOUT                                      
*                                                                               
         LA    R4,PBUYREC+7                                                     
*****    GOTO1 =V(GETINS),DMCB,PBUYREC,GROSS,(R4),RR=RELO01                     
         GOTO1 VGETINS,DMCB,PBUYREC,GROSS,(R4)                                  
*                                                                               
         GOTO1 VPPBYOUT,DMCB,BUYOUTA                                            
         LA    R4,BUYOUTA                                                       
         USING PPBYOUTD,R4                                                      
*                                                                               
         OC    PBYOSPC1,=CL20' '   20 TO SAVE A LITERAL                         
*                                                                               
         LHI   R1,16                                                            
         CLI   SVSLEN,X'FF'                                                     
         BE    *+10                                                             
         ZIC   R1,SVSLEN                                                        
         EX    R1,SC1COMP                                                       
         BE    RDB6C4Y                                                          
         EX    R1,UNTCOMP                                                       
         BE    RDB6C4Y                                                          
         B     CKINCH                                                           
*                                                                               
SC1COMP  CLC   SVSPACE+1(0),PBYOSPC1     EXECUTED                               
UNTCOMP  CLC   SVSPACE+1(0),PBYOUNTS     EXECUTED                               
*                                                                               
******   CLC   SVSPACE+1(17),PBYOSPC1 SHOULD MATCH EITHER PBYOSPC1              
******   BE    RDB6C4Y                OR PBYOUNTS                               
******   CLC   SVSPACE+1(7),PBYOUNTS                                            
******   BE    RDB6C4Y                                                          
*                                                                               
CKINCH   CLI   PBDUIND,X'89'       SEE IF INCHES WITH 2 DECIMALS                
         BNE   RDB6C4N             NO THEN NO MATCH                             
         ZAP   MYDUB,PBDUNITS                                                   
         DP    MYDUB,=P'100'                                                    
         CP    MYDUB+6(2),=P'0'       REMAINDER ZERO?                           
         BNE   RDB6C4N              NO THEN NO MATCH                            
*                                                                               
*        SO I CAN MATCH NNNI TO NNN.00I                                         
*                                                                               
         MVC   TEMP+1(17),=CL20' '                                              
         EDIT  (P6,MYDUB),(7,TEMP+1),0,ALIGN=LEFT,TRAIL=C'I'                    
         CLI   PBDSPACE,C'*'                                                    
         BE    RDB6C4S5                                                         
         CLI   PBDSPACE,C'#'                                                    
         BE    RDB6C4S5                                                         
         MVC   TEMP(7),TEMP+1                                                   
         B     RDB6C4S7                                                         
*                                                                               
RDB6C4S5 MVC   TEMP(1),PBDSPACE                                                 
*                                                                               
RDB6C4S7 DS    0H                                                               
         LHI   R1,6                                                             
         CLI   SVSLEN,X'FF'           SEE IF SPECIAL COMPARE LENGTH             
         BE    RDB6C4S8               ENTERED                                   
         ZIC   R1,SVSLEN              USE IT INSTEAD                            
RDB6C4S8 EX    R1,TEMCOMP                                                       
         BE    RDB6C4Y                                                          
         B     RDB6C4N                                                          
*                                                                               
TEMCOMP  CLC   SVSPACE+1(0),TEMP       EXECUTED                                 
*                                                                               
RDB6C4Y  DS    0H                   HERE IF MATCHES FILTER                      
         CLI   SVSPACE,C'-'         CHK FOR NEGATIVE FILTER                     
         BE    RDB6T                                                            
         B     RDB6C4X                                                          
*                                                                               
RDB6C4N  DS    0H                                                               
         CLI   SVSPACE,C'-'         CHECK FOR NEAGTIVE FILTER                   
         BE    RDB6C4X                                                          
         B     RDB6T                                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
RDB6C4X  CLI   DATESW,C'I'          SEE IF INSERTION DATE                       
         BE    RDB6C8                                                           
*                                                                               
         CLI   DATESW,C'O'          IF DATESW IS O CHECK ONSALE DATES           
         BNE   RDB6D                                                            
         CLI   PRSW,1                CHK FOR PRIOR                              
         BE    RDB6C5                                                           
         CLC   PBDSDATE,SVSTRTB                                                 
         BL    RDB6T                                                            
RDB6C5   CLI   SUBSW,1               CHK FOR SUBSEQUENT                         
         BE    RDB6C8                                                           
         CLC   PBDSDATE,SVENDB                                                  
         BH    RDB6T                                                            
RDB6C8   MVC   KEY,SVLSTKEY                                                     
         XC    SVLSTKEY,SVLSTKEY                                                
         B     RDB7                                                             
*                                                                               
RDB6D    CLI   DATESW,C'C'         IF DATESW IS C CHECK CLOSING DATES           
         BNE   RDB6F                                                            
         CLI   PRSW,1              CHK FOR PRIOR                                
         BE    RDB6D5                                                           
         CLC   PBDCDATE,SVSTRTB                                                 
         BL    RDB6T                                                            
*                                                                               
RDB6D5   CLI   SUBSW,1                 CHK FOR SUBSEQUENT                       
         BE    RDB6D8                                                           
         CLC   PBDCDATE,SVENDB                                                  
         BH    RDB6T                                                            
RDB6D8   MVC   KEY,SVLSTKEY                                                     
         XC    SVLSTKEY,SVLSTKEY                                                
         B     RDB7                                                             
*                                                                               
RDB6F    CLI   DATESW,C'B'         IF DATESW IS B CHECK BILLING DATES           
         BNE   RDB6G                                                            
         CLI   PRSW,1                CHK DOING PRIOR                            
         BE    RDB6F5                                                           
         CLC   PBDBDATE,SVSTRTB                                                 
         BL    RDB6T                                                            
RDB6F5   CLI   SUBSW,1               CHK DOING SUBSEQUENT                       
         BE    RDB6F8                                                           
         CLC   PBDBDATE,SVENDB                                                  
         BH    RDB6T                                                            
RDB6F8   MVC   KEY,SVLSTKEY                                                     
         XC    SVLSTKEY,SVLSTKEY                                                
         B     RDB7                                                             
*                                                                               
*                                                                               
RDB6G    CLI   DATESW,C'M'       IF DATESW IS M CHECK MAT CLOSE DATES           
         BNE   RDB6H                                                            
         CLI   PRSW,1                CHK DOING PRIOR                            
         BE    RDB6G5                                                           
         CLC   PBDMDATE,SVSTRTB                                                 
         BL    RDB6T                                                            
RDB6G5   CLI   SUBSW,1               CHK DOING SUBSEQUENT                       
         BE    RDB6G8                                                           
         CLC   PBDMDATE,SVENDB                                                  
         BH    RDB6T                                                            
RDB6G8   MVC   KEY,SVLSTKEY                                                     
         XC    SVLSTKEY,SVLSTKEY                                                
         B     RDB7                                                             
*                                                                               
RDB6H    CLI   DATESW,C'P'         IF DATESW IS P CHECK PAYING DATES            
         BNE   RDB6I                                                            
         CLI   PRSW,1                CHK DOING PRIOR                            
         BE    RDB6H5                                                           
         CLC   PBDPDATE,SVSTRTB                                                 
         BL    RDB6T                                                            
*                                                                               
RDB6H5   CLI   SUBSW,1                CHK DOING SUBSEQUENT                      
         BE    RDB6H8                                                           
         CLC   PBDPDATE,SVENDB                                                  
         BH    RDB6T                                                            
RDB6H8   MVC   KEY,SVLSTKEY                                                     
         XC    SVLSTKEY,SVLSTKEY                                                
         B     RDB7                                                             
*                                                                               
RDB6I    CLI   DATESW,C'S'         IF DATESW IS S CHECK SHIP DATE               
         BE    *+6                                                              
         DC    H'0'                SOMETHING SCREWY                             
         CLI   PRSW,1                CHK DOING PRIOR                            
         BE    RDB6I5                                                           
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'86'           SHIP DATE ELEM                            
         BAS   RE,NEXTEL                                                        
         BNE   RDB6T                 SKIP IF NOT FOUND                          
         CLC   2(3,R2),SVSTRTB                                                  
         BL    RDB6T                                                            
*                                                                               
RDB6I5   CLI   SUBSW,1                CHK DOING SUBSEQUENT                      
         BE    RDB6I8                                                           
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'86'           SHIP DATE ELEM                            
         BAS   RE,NEXTEL                                                        
         BNE   RDB6T                 SKIP IF NOT FOUND                          
         CLC   2(3,R2),SVENDB                                                   
         BH    RDB6T                                                            
RDB6I8   MVC   KEY,SVLSTKEY                                                     
         XC    SVLSTKEY,SVLSTKEY                                                
         B     RDB7                                                             
*                                                                               
RDB6T    MVC   KEY,SVLSTKEY                                                     
         XC    SVLSTKEY,SVLSTKEY                                                
         B     RDB3                DATE NOT BETWEEN, GET NEXT REC.              
***                                                                             
RDB7     DS    0H                                                               
*                                  ESTIMATE CHECK MOVED TO RDB4                 
         CLI   SVBILL,0            SEE IF BILLED FILTER SET                     
         BE    RDB7P                                                            
RDB7B    DS    0H                                                               
         CLI   SVBILL,C'B'         ANY BILLING                                  
         BNE   RDB7P                                                            
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'26'     IF 26 (BILL) ELEMENT WITH DATE EXISTS,          
RDB7B2   BAS   RE,NEXTEL                                                        
         BNE   RDB3             SKIP THIS BUY                                   
         OC    5(3,R2),5(R2)                                                    
         BZ    RDB7B2              PASSING BILLING CHECK                        
         TM    10(R2),X'C0'             IGNORE REVERSALS AND REVERSED           
         BNZ   RDB7B2                                                           
         B     RDB7P            GO CHECK PAID FILTER                            
*                                                                               
***                                                                             
***      CHECK FOR TOTALLY BILLED (SVBILL = "T") COULD GO HERE                  
***                                                                             
RDB7P    DS    0H                                                               
         CLI   SVPAID,0            SEE IF PAID FILTER SET                       
         BE    RDB8                                                             
         CLI   SVPAID,C'P'         ANY PAYMENTS                                 
         BNE   RDB8                                                             
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'     IF 26 (BILL) ELEMENT WITH DATE EXISTS,          
RDB7P2   BAS   RE,NEXTEL                                                        
         BNE   RDB3             SKIP THIS BUY                                   
         OC    2(3,R2),2(R2)                                                    
         BZ    RDB7P2                                                           
***                                                                             
***      CHECK FOR TOTALLY PAID (SVPAID = "T") COULD GO HERE                    
***                                                                             
*                                                                               
RDB8     DS    0H                                                               
*****                                                                           
         AP    HALF2,=P'1'         ADD 1,IS ONE FOR SCREEN                      
         CLI   PUBNOPT,C'Y'        SEE IF DISPLAYING PUB NAME                   
         BNE   RDB8B                                                            
         CP    HALF2,=P'8'        FULL SCREEN (8) YET?                          
         BNH   RDB8E                                                            
         B     RDB8C                                                            
*                                                                               
RDB8B    CP    HALF2,=P'16'        FULL SCREEN (16) YET?                        
         BNH   RDB8E                                                            
RDB8C    MVC   SVLSTKEY,KEY        IF YES SAVE LAST (17TH) KEY READ.            
         XC    MBCEMSG,MBCEMSG                                                  
         MVC   MBCEMSG(23),=C'HIT ENTER FOR NEXT PAGE'                          
         FOUT  MBCEMSGH                                                         
         LA    R3,MBCIDH           CHANGE TO MODIFIED                           
         OI    6(R3),X'81'         AND TRANSMIT.                                
         B     SCRNDISP                                                         
*****                                                                           
RDB8E    MVC   0(4,R3),KEY+27      SAVE DISC ADDRESS                            
*                                                                               
         LA    R3,4(R3)            BUMP UP DISC ADDRESS SAVE AREA               
         B     RDB3                GO DO SEQ READ                               
*                                                                               
RDBX     DS    0H                                                               
         CLI   SVCLT,C'*'          CHK FOR OFFICE                               
         BE    RDBX5                                                            
         CLC   SVCLT,=C'ALL'       SEE IF DOING ALL CLTS                        
         BNE   RDBXX                                                            
RDBX5    BAS   RE,GETNCLT                                                       
         OC    WORKCLT,WORKCLT                                                  
         BZ    RDBXX               END OF CLIENTS                               
         B     RDB05                                                            
*                                                                               
RDBXX    DS    0H                  LESS THAN 16 RECORDS READ.                   
         XC    MBCEMSG,MBCEMSG                                                  
RDBXX1   MVC   MBCEMSG(16),=C'ACTION COMPLETED'                                 
         FOUT  MBCEMSGH                                                         
         OI    MBCIDH+6,X'81'      CHANGE TO MODIFIED                           
         EJECT                                                                  
*                                                                               
*                                                                               
SCRNDISP DS    0H                  PUT OUT COLUMN HEADER                        
*                                                                               
*        IMPRESSIONS NOW FOR ALL AGYS                                           
*                                                                               
SCRN10   GOTO1 =A(SETSCRN),DMCB,(RC),(RA),RR=RELO01                             
         GOTO1 =A(SCRNDIS),DMCB,(RC),(RA),RR=RELO01                             
*                                                                               
EXIT1    LA    R2,MBCACTH                                                       
         OI    6(R2),X'40'            POSITION CURSOR HERE                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
CHKCHA   NTR1                                                                   
         MVI   WARN,0          USED TO SAVE GLOBAL SPACE WARNING                
         MVI   CHASW,0         USED TO INDICATE "SPECIAL" ERROR MSG             
         XC    PUBDTTAB,PUBDTTAB   CLEAR TABLE OF PUBS/DATES                    
         MVI   PUBDTTAB,X'FF'                                                   
*                                  USED IN ASC                                  
         MVC   COUNT,=H'16'                                                     
         LA    R7,SVDISC                                                        
         LA    R6,MBCDT01H                                                      
         SR    R3,R3                                                            
*                                                                               
*                                                                               
CHKLP1   OC    0(4,R7),0(R7)          IS THERE A DISC ADDR.                     
         BZ    CHKXIT                 IF NOT EXIT.                              
*                                                                               
         TM    1(R6),X'20'            HAS THE FIELD BEEN PROTECTED.             
         BO    CHKLP9                 IF YES SKIP TO NEXT.                      
*                                                                               
         TM    4(R6),X'20'            HAS FIELD BEEN ALTERED.                   
         BO    CHKLP9                 IF NO NEXT FIELD.                         
*                                                                               
         CLI   8(R6),C'*'             MEANS DON'T CHANGE THIS BUY               
         BE    CHKLP9                                                           
*                                                                               
         CLI   DATASW,C'A'            IS IT ADCODE.                             
         BNE   CHKCH5                                                           
*                                                                               
         CLI   5(R6),6                MAXIMUM IS 6 CHARS                        
         BH    FLDERR                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+27(4),0(R7)        FROM DISC ADDR GET BUY REC.               
         BAS   RE,GETREC                                                        
         LA    R2,PBUYREC+33                                                    
         MVC   SAVBYKEY,PBUYKEY                                                 
         MVC   SAVSPACE,PBDSPACE                                                
         MVC   SAVUNITS,PBDUNITS                                                
         MVC   SAVCLMS,PBDCLMS                                                  
         MVC   SAVUIND,PBDUIND                                                  
         XC    SAVFSI,SAVFSI                                                    
         MVI   ELCODE,X'82'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   CHKLP1B                                                          
         USING PBFSIELD,R2                                                      
         MVC   SAVFSI,PBFSI                                                     
         DROP  R2                                                               
*                                                                               
CHKLP1B  DS    0H                                                               
         LA    R2,PBUYREC+33                                                    
*****    OC    8(11,R6),=9C' '                                                  
         OC    8(11,R6),=17C' '       TO AVOID ANOTHER LITERAL                  
         CLC   8(11,R6),=C'NONE      '  LIKE REMOVING                           
         BE    CHKLP1D                                                          
         CLI   5(R6),0                HAS ADCODE BEEN REMOVED.                  
         BNE   CHKLP2                                                           
*                                                                               
CHKLP1D  MVI   ELCODE,X'70'           CAN'T BE REMOVED IF '70' EL               
         BAS   RE,NEXTEL              WITH DATE EXISTS.                         
         BNE   CHKLP9                 BRANCH AND GET NEXT REC.                  
         OC    2(3,R2),2(R2)          DATE FIELD IN '70' EL.                    
         BZ    CHKLP9                 BRANCH AND GET NEXT REC.                  
         LA    R3,JOBERR1             ERROR - CAN'T REMOVE ADCODE.              
         B     CHKXIT                 IF I/O PRINTED                            
*                                                                               
CHKLP2   XC    KEY,KEY                FROM BUY REC KEY GET JOBREC.              
         MVC   KEY(10),SAVBYKEY                                                 
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+10(2),SAVBYKEY+19                                            
         BAS   RE,HIGH                                                          
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                  EST NOT ON FILE BAD ERROR                  
         BAS   RE,GETREC                                                        
         OC    PESTJOB,=6C' '                                                   
         CLC   PESTJOB,=6C' '                                                   
         BE    CHKLP2F                EST NOT ASSIGNED JOB                      
         MVC   WORK(6),8(R6)                                                    
         OC    WORK(6),=6C' '                                                   
         CLC   WORK(6),=6C' '                                                   
         BE    CHKLP2F                                                          
         CLC   PESTJOB,WORK                                                     
         BE    CHKLP2F                                                          
         LA    R3,JOBERR4         AD CODE MUST AGREE WITH ESTIMATE              
         B     CHKXIT                                                           
*                                                                               
CHKLP2F  DS    0H                                                               
*                                                                               
CHKLP3   XC    KEY,KEY                FROM BUY REC KEY GET JOBREC.              
         MVC   KEY(10),SAVBYKEY                                                 
         MVI   KEY+3,X'15'                                                      
*                                                                               
CHKLP3F  MVC   KEY+10(6),8(R6)                                                  
         OC    KEY+10(6),=6C' '                                                 
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(16),KEY                                                  
         BE    CHKLP5                                                           
         LA    R3,NOTFND              ADREC NOT FOUND                           
         B     CHKXIT                                                           
*                                                                               
CHKLP5   MVC   FULL,KEY+27            SAVE DISK ADDR                            
         MVI   KEY+16,X'FF'                                                     
         BAS   RE,HIGH                                                          
         CLC   KEY(16),KEYSAVE       MUST FIND INSTRUCTION REC                  
         BE    CHKLP6                                                           
         LA    R3,JOBERR2            NO INSTRUCTION RECORD                      
         B     CHKXIT                                                           
*                                                                               
CHKLP6   MVC   KEY+27,FULL                                                      
         MVC   AREC,AWRKREC           READ INTO WRKREC                          
         BAS   RE,GETREC                                                        
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
*                                                                               
         L     R4,AWRKREC                                                       
         USING PJOBREC,R4                                                       
*                                                                               
         OC    PJOBSTA,PJOBSTA        CHK FOR START/END DATES                   
         BZ    CHKLP6C                                                          
         CLC   SAVBYKEY+16(3),PJOBSTA                                           
         BNL   CHKLP6C                                                          
CHKLP6E  LA    R3,JOBERR3            INS DATE OUTSIDE JOB DATES                 
         B     CHKXIT                                                           
*                                                                               
CHKLP6C  OC    PJOBEND,PJOBEND                                                  
         BZ    CHKLP6H                                                          
         CLC   SAVBYKEY+16(3),PJOBEND                                           
         BNH   CHKLP6H                                                          
         B     CHKLP6E                                                          
*                                                                               
CHKLP6H  DS    0H                  CHECK FOR BUY PUB IN PUBLIST RECORD          
         CLI   PJOBPLIS,C' '       PUB LIST IN JOB RECORD ?                     
         BNH   CHKLP7              NO                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(7),SAVBYKEY     AGY/MED/RC/CLT                               
         MVI   KEY+3,X'17'         PUB LIST RECORD                              
         MVC   KEY+7(3),PJOBPLIS   LIST CODE                                    
         MVI   KEY+10,X'01'        "LINE' NUMBER                                
CHKLP6J  BAS   RE,HIGH                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    CHKLP6N             GET THE LIST RECORD                          
         CLC   KEYSAVE+4(3),=C'ZZZ'   WAS I LOOKING FOR ALL CLIENTS ?           
         BNE   CHKLP6L                NO - DO IT NOW                            
*                                   YES - ERROR                                 
         XC    MBCEMSG,MBCEMSG                                                  
         MVC   MBCEMSG(49),=C'** INVALID - NO RECORD FOUND FOR PUB LISTX        
               = *** **'                                                        
         MVC   MBCEMSG+43(3),PJOBPLIS       LIST CODE FROM JOB REC              
         FOUT  MBCEMSGH                                                         
         MVI   CHASW,C'P'          "SPECIAL" ERROR MESSAGE INDICATOR            
         B     CHKXIT                 RECORD NOT FOUND                          
*                                                                               
CHKLP6L  MVC   KEY,KEYSAVE                                                      
         MVC   KEY+4(3),=C'ZZZ'                                                 
         B     CHKLP6J             TRY FOR "ZZZ" CLIENT CODE LIST REC           
*                                                                               
         DROP  R4                                                               
*                                                                               
CHKLP6N  DS    0H                  GET THE PUB LIST RECORD                      
         MVC   AREC,ACONIO         READ INTO CONIO                              
         BAS   RE,GETREC                                                        
         LA    R0,REC              RESTORE AREC                                 
         ST    R0,AREC                                                          
*                                                                               
         L     R2,ACONIO           PUB LIST RECORD IN CONIO                     
         LA    R2,33(R2)                                                        
         MVI   ELCODE,X'20'                                                     
         CLI   0(R2),X'20'         LIST PUB ELEMENT ?                           
         BE    CHKLP6R             YES - TEST FOR PUB                           
CHKLP6P  BAS   RE,NEXTEL                                                        
         BNE   CHKLP6T             LOOK FOR ANOTHER RECORD                      
CHKLP6R  CLC   SAVBYPUB(6),2(R2)   BUY PUB = LIST PUB ?                         
         BE    CHKLP6X             YES - OK - CONTINUE                          
         B     CHKLP6P             LOOK FOR ANOTHER LIST PUB ELEMENT            
*                                                                               
CHKLP6T  BAS   RE,SEQ              NEXT RECORD                                  
         CLC   KEY(10),KEYSAVE     SAME AGY/MED/RC/CLT/LIST ?                   
         BE    CHKLP6N             YES - GET THE RECORD                         
*                                  NO - ERROR                                   
         XC    MBCEMSG,MBCEMSG                                                  
         MVC   MBCEMSG(48),=C'** INVALID - PUBLICATION NOT IN PUB LIST=X        
                *** **'                                                         
         MVC   MBCEMSG+42(3),KEYSAVE+7   CODE FROM PUB LIST REC KEY             
         FOUT  MBCEMSGH                                                         
         MVI   CHASW,C'P'          "SPECIAL" ERROR MESSAGE INDICATOR            
         B     CHKXIT                                                           
*                                                                               
CHKLP6X  DS    0H                  RESTORE JOBREC POINTER                       
*                                                                               
         L     R4,AWRKREC                                                       
         USING PJOBREC,R4                                                       
*                                                                               
CHKLP7   DS    0H                                                               
         CLI   PJOBFSI,C'Y'           SEE IF FSI AD                             
         BNE   CHKLP7W                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+27(4),0(R7)        MUST REREAD BUY                           
         BAS   RE,GETREC              TO USE FINDFSI                            
*                                                                               
         BAS   RE,FINDFSI                                                       
         CP    DUB(6),=P'0'                                                     
         BNE   CHKLP7W                                                          
         OC    SAVFSI,SAVFSI      SEE IF BUY ALREADY HAS FSI DATA               
         BNZ   CHKLP7W                                                          
         LA    R3,FSIERR         FSI DATA NOT FOUND                             
         B     CHKXIT                                                           
*****    MVI   15(R6),C'F'                                                      
*****    OI    WARN,X'40'         FSI WARNING - FSI DATA NOT FOUND              
*                                                                               
CHKLP7W  DS    0H                     CHK AD SPACE VS. BUY                      
*                                     AND SET WARNING INDICATOR                 
         CLI   MBCMDIA,C'N'           NEWSPAPERS                                
         BE    CHKLP8                                                           
         CLI   MBCMDIA,C'O'           OUTDOOR                                   
         BE    CHKLP9                 NO SPACE CHECKING                         
         MVC   WORK(17),SAVSPACE                                                
         OC    WORK(17),=17C' '                                                 
         CLC   WORK(17),PJOBSPC                                                 
         BE    CHKLP9                                                           
         CLI   15(R6),C'F'                                                      
         BNE   *+12                                                             
         MVI   15(R6),C'B'            FOR BOTH FSI AND SPACE                    
         B     *+8                                                              
*                                                                               
         MVI   15(R6),C'S'            SET SPACE WARNING                         
         OI    WARN,X'80'             FOR GLOBAL WARNING                        
         FOUT  (R6)                                                             
         B     CHKLP9                                                           
*                                                                               
CHKLP8   CLC   SAVSPACE(2),=C'* '         SEE IF BUY HAS SPACE DESC             
         BNH   CHKLP8M                    NO - CHECK UNITS                      
*                                                                               
CHKLP8C  MVC   WORK(8),SAVSPACE                                                 
         OC    WORK(8),=17C' '                                                  
         CLC   WORK(8),PJOBSPC                                                  
         BNE   CHKLP8W                                                          
         CP    PJOBTUNS,=P'0'          SEE IF UNITS ENTERED IN JOB              
         BE    CHKLP9                 NO                                        
*                                                                               
CHKLP8M  CP    SAVUNITS,PJOBTUNS   MATCH UNITS EVEN FOR 'SPACE' BUYS            
         BNE   CHKLP8W                                                          
         CP    SAVCLMS,PJOBCOLS                                                 
         BNE   CHKLP8W                                                          
         CLC   SAVUIND,PJOBUIND                                                 
         BNE   CHKLP8W                                                          
         B     CHKLP9                                                           
*                                                                               
         DROP  R4                                                               
*                                                                               
CHKLP8W  CLI   15(R6),C'F'                                                      
         BNE   *+12                                                             
         MVI   15(R6),C'B'           BOTH FSI AND SPACE WARNING                 
         B     *+8                                                              
*                                                                               
         MVI   15(R6),C'S'                                                      
         OI    WARN,X'80'            GLOBAL SPACE WARNING                       
         FOUT  (R6)                                                             
*                                                                               
CHKLP9   LA    R7,4(R7)               GET NEXT DISC ADDRESS.                    
         LA    R6,LDISP(R6)           BUMP TO NEXT LINE ON SCREEN.              
*                                                                               
         LH    R1,COUNT                                                         
*                                                                               
         CLI   PUBNOPT,C'Y'                                                     
         BNE   CHKLP10                                                          
         LA    R6,LDISP(R6)           TO GET PAST PUB NAME LINE                 
         AHI   R1,-1                                                            
         LTR   R1,R1                                                            
         BNP   CHKXIT                                                           
*                                                                               
CHKLP10  DS    0H                                                               
         AHI   R1,-1                                                            
         STH   R1,COUNT                                                         
         LTR   R1,R1                                                            
         BNP   CHKXIT                                                           
         B     CHKLP1                                                           
*                                                                               
CHKCH5   DS    0H                                                               
         CLI   DATASW,C'L'                                                      
         BNE   CHKCH6                                                           
         CLI   5(R6),0                CHK FOR INPUT                             
         BE    FLDMIS                                                           
         CLI   8(R6),C'N'                                                       
         BE    CHKLP9                                                           
         CLI   8(R6),C'Y'                                                       
         BE    CHKLP9                                                           
         B     FLDERR                 INVALID INPUT  (WAS B CHKCH15E)           
*                                                                               
CHKCH6   DS    0H                 SPECIAL FINANCIAL HANDLING (SFH)              
         CLI   DATASW,C'X'                                                      
         BNE   CHKCH7                                                           
         CLI   5(R6),0                CHK FOR INPUT                             
         BE    FLDMIS                                                           
         CLI   8(R6),C'H'          H FOR (H)OLD                                 
         BE    CHKLP9                                                           
         CLI   8(R6),C'R'          R FOR (R)ELEASED                             
         BE    CHKLP9                                                           
         B     FLDERR                 INVALID INPUT                             
*                                                                               
CHKCH7   DS    0H                                                               
         CLI   DATASW,C'E'            T/S STATUS                                
         BNE   CHKCH10                                                          
         CLI   5(R6),0                CHK FOR INPUT                             
         BE    CHKLP9                 NONE IS OK                                
         CLI   8(R6),C'N'                                                       
         BE    CHKLP9                                                           
         CLI   8(R6),C'A'                                                       
         BE    CHKLP9                                                           
         B     FLDERR                 INVALID INPUT  (WAS B CHKCH15E)           
*                                                                               
CHKCH10  DS    0H                                                               
         CLI   DATASW,C'S'                                                      
         BNE   CHKLP12                                                          
         CLI   5(R6),0                CHK FOR INPUT                             
         BE    FLDMIS                                                           
         CLI   8(R6),C'T'                                                       
         BE    CHKLP9                                                           
         CLI   8(R6),C'L'                                                       
         BNE   FLDERR                              (WAS BNE CHKCH15E)           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+27(4),0(R7)        FROM DISC ADDR GET BUY REC.               
         BAS   RE,GETREC                                                        
         CLI   PBDSPACE,C'*'          SEE IF NOT A REAL INS                     
         BE    CHKLP9                                                           
         CLI   PBDSPACE,C'#'                                                    
         BE    CHKLP9                                                           
*                                                                               
         CLI   PBUYKMED,C'O'         SEE IF OUTDOOR                             
         BNE   CHKCH10C                                                         
         CP    PBDSHOW,=P'0'                                                    
         BNE   CHKCH10C                                                         
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'66'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   CHKCH10C                                                         
         CLI   1(R2),19                                                         
         BH    CHKCH10C                                                         
         CLI   2(R5),C'*'                                                       
         BE    CHKLP9                                                           
         CLI   2(R5),C'#'                                                       
         BE    CHKLP9                                                           
*                                                                               
CHKCH10C LA    R2,PBUYREC+33                                                    
         MVC   SAVBYKEY,PBUYKEY                                                 
*                                                                               
*      WHEN MAKING LIVE CHECK ZZZ ALLOCATED PRODUCTS FOR TEST ESTIMATES         
*                                                                               
         CLC   PBUYKPRD,=C'ZZZ'    "ZZZ" PRODUCT ?                              
         BNE   CHKCH10E            NO - CONTINUE                                
*                                  YES - GO CHECK ESTIMATES                     
         GOTO1 =A(PEC),DMCB,(RC),(RA),RR=RELO01                                 
         CLI   CHASW,C'X'              CHECK FOR ERROR                          
         BE    CHKXIT              ERROR - TEST ESTIMATE FOUND                  
*                                                                               
*                                                                               
*        WHEN MAKING LIVE DO COMPETITIVE BRAND CHECK FIRST                      
*                                                                               
CHKCH10E GOTO1 =A(COM),DMCB,(RC),(RA),RR=RELO01                                 
         LTR   R3,R3                   CHECK FOR ERROR                          
         BNZ   CHKXIT                                                           
*                                                                               
*                                                                               
*        WHEN MAKING LIVE DO AUTOMATIC SCHEDULE CHECKING                        
*                                                                               
         GOTO1 =A(ASC),DMCB,(RC),(RA),RR=RELO01                                 
         LTR   R3,R3                   CHECK FOR ERROR                          
         BNZ   CHKXIT                                                           
*                                                                               
         B     CHKLP9                                                           
*                                                                               
CHKLP12  CLI   DATASW,C'1'            IF 1,2,3,4,5 VALIDATE DATE                
         BL    CHKCH20                                                          
         CLI   DATASW,C'5'            IF 1,2,3,4,5 VALIDATE DATE                
         BH    CHKCH20                                                          
         CLI   5(R6),0                 CHK FOR INPUT                            
         BE    CHKCH16                                                          
*                                                                               
CHKCH15  GOTO1 VDATVAL,DMCB,(0,8(R6)),WORK                                      
         OC    DMCB,DMCB                                                        
         BNZ   CHKCH16                                                          
CHKCH15E LA    R3,DTEINV                                                        
         B     CHKXIT                                                           
*                                                                               
CHKCH16  CLI   DATASW,C'2'                                                      
         BE    CHKCH18                                                          
         CLI   DATASW,C'3'                                                      
         BE    CHKCH18                                                          
         CLI   DATASW,C'4'            CLOSING                                   
         BE    CHKCH17                                                          
         CLI   DATASW,C'5'            OR MATERIALS CLOSING                      
         BNE   CHKCH90                                                          
CHKCH17  CLI   SVAPROF+24,C'B'    BOTH CLOSING AND MAT DATES REQUIRED           
         BE    CHKCH17M                                                         
         IF    DATASW,EQ,C'4',AND,SVAPROF+24,EQ,C'R',CHKCH17M                   
         IF    DATASW,EQ,C'5',AND,SVAPROF+24,EQ,C'M',CHKCH17M                   
         CLI   5(R6),0          CHK FOR INPUT                                   
         BE    CHKCH90          NOT REQUIRED                                    
*                                                                               
CHKCH17C XC    KEY,KEY      IF INPUT CAN'T PRECEED INSERTION DATE               
         MVC   KEY+27(4),0(R7)                                                  
         BAS   RE,GETREC                                                        
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK+10)                                
         CLC   WORK+10(3),PBUYKDAT       CAN'T BE AFTER INSERTION DATE          
         BH    CHKCH17E                                                         
         B     CHKCH90                                                          
*                                                                               
CHKCH17M CLI   5(R6),0          MISSING CLOSING OR MATERIALS DATE               
         BNE   CHKCH17C                                                         
         B     FLDMIS                                                           
*                                                                               
CHKCH17E LA    R3,CLOSERR                                                       
         B     CHKXIT                                                           
*                                                                               
CHKCH18  CLI   5(R6),0                                                          
         BNE   CHKCH18A                                                         
         B     FLDMIS            BILLABLE/PAYABLE DATES REQUIRED                
*                                                                               
CHKCH18A XC    KEY,KEY                                                          
         MVC   KEY+27(4),0(R7)                                                  
         BAS   RE,GETREC                                                        
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK+10)                                
         LA    R4,=AL2(6,3)                                                     
         LA    R3,BDERR                                                         
         CLI   DATASW,C'2'               BILLABLE                               
         BE    CHKCH19                                                          
         LA    R4,=AL2(5,5)               PAYABLE                               
         LA    R3,PDERR                                                         
*                                                                               
CHKCH19  DS    0H                                                               
         SPACE 2                                                                
RTSUBX5  DS    0H                                                               
         SR    RF,RF                                                            
         MVC   WORK(2),PBUYKDAT                                                 
         IC    RF,WORK+1                                                        
         SH    RF,0(R4)                                                         
         STC   RF,WORK+1                                                        
         BP    RTSUBX5B                                                         
         AHI   RF,12                                                            
         STC   RF,WORK+1                                                        
         IC    RF,WORK                                                          
         BCTR  RF,R0                                                            
         STC   RF,WORK                                                          
RTSUBX5B DS    0H                                                               
         CLC   WORK(2),WORK+10                                                  
         BH    CHKXIT              DATE BELOW LIMIT                             
*                                                                               
         MVC   WORK(2),PBUYKDAT                                                 
         IC    RF,WORK+1                                                        
         AH    RF,2(R4)                                                         
         STC   RF,WORK+1                                                        
         CLI   WORK+1,12                                                        
         BNH   RTSUBX5D                                                         
         AHI   RF,-12                                                           
         STC   RF,WORK+1                                                        
         IC    RF,WORK                                                          
         LA    RF,1(RF)                                                         
         STC   RF,WORK                                                          
RTSUBX5D DS    0H                                                               
         CLC   WORK(2),WORK+10                                                  
         BL    CHKXIT              DATE ABOVE LIMIT                             
         SR    R3,R3                                                            
         B     CHKCH90                                                          
*                                                                               
CHKCH20  CLI   DATASW,C'7'                                                      
         BNE   CHKCH25                                                          
         TM    4(R6),X'20'               HAS FIELD NOT BEEN CHANGED             
         BO    CHKCH90                   IF NO BRANCH TO NEXT                   
         ZIC   R4,5(R6)                                                         
         LTR   R4,R4                     IS THERE AN ENTRY IN THE FIELD         
         BZ    CHKCH90                                                          
         GOTO1 VCASHVAL,DMCB,(1,8(R6)),(R4)                                     
         CLI   DMCB,0                                                           
         BNE   FLDERR                                                           
         CLC   DMCB+6(2),=X'03E7'                                               
         BH    FLDERR                                                           
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BM    FLDERR            CAN'T BE MINUS                                 
         B     CHKCH90                                                          
*                                                                               
CHKCH25  CLI   DATASW,C'8'                                                      
         BNE   CHKCH27                                                          
         TM    4(R6),X'20'               HAS FIELD BEEN CHANGED                 
         BO    CHKCH90                   IF NO BRANCH TO NEXT                   
         ZIC   R4,5(R6)                                                         
         LTR   R4,R4                     IS THERE AN ENTRY IN THE FIELD         
         BZ    CHKCH90                                                          
         GOTO1 VCASHVAL,DMCB,(3,8(R6)),(R4)                                     
         CLI   DMCB,0                                                           
         BNE   FLDERR                                                           
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BM    FLDERR                    CAN'T BE MINUS                         
         CLC   DMCB+5(3),=X'0186A0'      MAX IS 100.000                         
         BNH   CHKCH90                                                          
         B     FLDERR                                                           
*                                                                               
CHKCH27  CLI   DATASW,C'B'         COST 2 FACTOR                                
         BNE   CHKCH30                                                          
         TM    4(R6),X'20'               HAS FIELD BEEN CHANGED                 
         BO    CHKCH90                   IF NO BRANCH TO NEXT                   
         ZIC   R4,5(R6)                                                         
         LTR   R4,R4                     IS THERE AN ENTRY IN THE FIELD         
*NOP*    BZ    CHKCH90                                                          
         BZ    FLDERR                    CAN'T BE "DELETED"                     
         GOTO1 VCASHVAL,DMCB,(6,8(R6)),(R4)                                     
         CLI   DMCB,0                                                           
         BNE   FLDERR                                                           
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BNP   FLDERR                    CAN'T BE MINUS OR ZERO                 
         CLC   DMCB+4(4),=X'0098967F'    MAX IS 9.999999                        
         BNH   CHKCH90                                                          
         B     FLDERR                                                           
*                                                                               
CHKCH30  CLI   DATASW,C'C'               CONTRACT UNITS                         
         BNE   CHKCH40                                                          
         TM    4(R6),X'20'               HAS FIELD BEEN CHANGED                 
         BO    CHKCH90                   IF NO BRANCH TO NEXT                   
         ZIC   R4,5(R6)                                                         
         LTR   R4,R4                     IS THERE AN ENTRY IN THE FIELD         
         BZ    CHKCH90                                                          
*                                                                               
         XC    KEY,KEY                   MUST READ BUY                          
         MVC   KEY+27(4),0(R7)           TO CHK PBDSPACE                        
         BAS   RE,GETREC                                                        
*                                                                               
         GOTO1 VCASHVAL,DMCB,(4,8(R6)),(R4)                                     
         CLI   DMCB,0                                                           
         BNE   FLDERR                                                           
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BM    FLDERR                    CAN'T BE MINUS                         
         CLI   PBDSPACE,C'*'             ONLY ALLOW CU=0 FOR * BUYS             
         BNE   CHKCH35                                                          
         OC    DMCB+4(4),DMCB+4          MUST BE ZERO                           
         BNZ   FLDERR                                                           
*                                                                               
CHKCH35  CLC   DMCB+4(4),=X'00FFFFFF'    MAX IS 1677.7215                       
         BNH   CHKCH90                                                          
         B     FLDERR                                                           
*                                                                               
CHKCH40  CLI   DATASW,C'P'               PLANNED COST                           
         BNE   CHKCH45                                                          
         TM    4(R6),X'20'               HAS FIELD BEEN CHANGED                 
         BO    CHKCH90                   IF NO BRANCH TO NEXT                   
         ZIC   R4,5(R6)                                                         
         LTR   R4,R4                     IS THERE AN ENTRY IN THE FIELD         
         BZ    CHKCH90                                                          
         GOTO1 VCASHVAL,DMCB,(2,8(R6)),(R4)                                     
         CLI   DMCB,0                                                           
         BNE   FLDERR                                                           
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BM    FLDERR                    CAN'T BE MINUS                         
         B     CHKCH90                                                          
*                                                                               
CHKCH45  DS    0H                                                               
         CLI   DATASW,C'D'         DLC                                          
         BNE   CHKCH47                                                          
         TM    4(R6),X'20'         TEST FIELD CHANGED                           
         BO    CHKCH90                                                          
         ZIC   R4,5(R6)                                                         
         LTR   R4,R4                     IS THERE AN ENTRY IN THE FIELD         
         BZ    CHKCH90                                                          
         GOTO1 VCASHVAL,DMCB,(2,8(R6)),(R4)                                     
         CLI   DMCB,0                                                           
         BNE   FLDERR                                                           
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BM    FLDERR                    CAN'T BE MINUS                         
         C     R1,=F'99999900'     MAXIMUM                                      
         BH    FLDERR                                                           
         CVD   R1,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET REMAINDER 0                         
         BNE   FLDERR                                                           
         B     CHKCH90                                                          
*                                                                               
CHKCH47  DS    0H                                                               
         CLI   DATASW,C'T'         RPT                                          
         BNE   CHKCH47C                                                         
         TM    4(R6),X'20'         TEST FIELD CHANGED                           
         BO    CHKCH90                                                          
         ZIC   R4,5(R6)                                                         
         LTR   R4,R4                     IS THERE AN ENTRY IN THE FIELD         
         BZ    CHKCH90                                                          
         GOTO1 VCASHVAL,DMCB,(2,8(R6)),(R4)                                     
         CLI   DMCB,0                                                           
         BNE   FLDERR                                                           
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BM    FLDERR                    CAN'T BE MINUS                         
         C     R1,=F'99900'        MAXIMUM 999.00                               
         BH    FLDERR                                                           
         CVD   R1,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET REMAINDER 0                         
         BNE   FLDERR                                                           
         B     CHKCH90                                                          
*                                                                               
CHKCH47C DS    0H                                                               
         CLI   DATASW,C'I'         IMPRESSION?                                  
         BE    CHKCH47D                                                         
         CLI   DATASW,C'V'         PAGE VIEWS?                                  
         BE    CHKCH47D                                                         
         CLI   DATASW,C'K'         CLICK THRUS?                                 
         BNE   CHKCH48                                                          
CHKCH47D DS    0H                                                               
         TM    4(R6),X'20'         TEST FIELD CHANGED                           
         BO    CHKCH90                                                          
         ZIC   R4,5(R6)                                                         
         LTR   R4,R4               IS THERE AN ENTRY IN THE FIELD               
         BZ    CHKCH90                                                          
         GOTO1 VNUMVAL,DMCB,(0,8(R6)),(R4)                                      
         CLI   DMCB,0                                                           
         BNE   FLDERR                                                           
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BM    FLDERR              CAN'T BE NEGATIVE                            
*                                                                               
*NOP*    CLI   DATASW,C'I'         IMPRESSION?                                  
*NOP*    BNE   CHKCH47H                                                         
*NOP*    C     R1,=F'999999999'    MAXIMUM 999,999,999 FOR IMPS                 
         C     R1,=F'999999999'    MAXIMUM 999,999,999                          
         BH    FLDERR                                                           
*NOP*    B     CHKCH47M                                                         
*                                                                               
*NOP*CHKCH47H C     R1,=F'9999999'      MAXIMUM 9,999,999 FOR PV AND CT         
*NOP*    BH    FLDERR                                                           
*                                                                               
CHKCH47M CVD   R1,DUB                                                           
         B     CHKCH90                                                          
*                                                                               
*                                                                               
*                                                                               
CHKCH48  DS    0H                                                               
         CLI   DATASW,C'F'         FSI                                          
         BNE   CHKCH49                                                          
         TM    4(R6),X'20'         TEST FIELD CHANGED                           
         BO    CHKCH90                                                          
*                                                                               
         XC    KEY,KEY                   MUST READ BUY                          
         MVC   KEY+27(4),0(R7)           TO CHK PBDSPACE                        
         BAS   RE,GETREC                                                        
*                                                                               
         ZIC   R4,5(R6)                                                         
         LTR   R4,R4                     IS THERE AN ENTRY IN THE FIELD         
         BZ    CHKCH90                                                          
         CLI   5(R6),1                FIRST CHK FOR 1 CHARACTER                 
         BNE   CHKCH48B                                                         
         CLI   8(R6),C'N'                                                       
         BE    CHKCH90                                                          
         CLI   8(R6),C'Y'                                                       
         BNE   CHKCH48B                                                         
*                                                                               
         XC    KEY,KEY                   MUST READ BUY                          
         MVC   KEY+27(4),0(R7)           TO CHK LOOK-UP OF FSI                  
         BAS   RE,GETREC                                                        
*                                                                               
         BAS   RE,FINDFSI             THIS WILL BE SURE FSI CAN                 
         LA    R3,FSIERR                                                        
         CP    DUB(6),=P'0'           BE LOOKED-UP                              
         BE    CHKXIT                                                           
         SR    R3,R3                                                            
         B     CHKCH90                                                          
*                                                                               
CHKCH48B CLC   8(2,R6),=C'NO'                                                   
         BE    CHKCH90                                                          
         CLC   8(3,R6),=C'YES'                                                  
         BNE   CHKCH48D                                                         
*                                                                               
         XC    KEY,KEY                   MUST READ BUY                          
         MVC   KEY+27(4),0(R7)           TO CHK LOOK-UP OF FSI                  
         BAS   RE,GETREC                                                        
*                                                                               
         BAS   RE,FINDFSI             THIS WILL BE SURE FSI CAN                 
         LA    R3,FSIERR                                                        
         CP    DUB(6),=P'0'           BE LOOKED-UP                              
         BE    CHKXIT                                                           
         SR    R3,R3                                                            
         B     CHKCH90                                                          
*                                                                               
CHKCH48D GOTO1 VCASHVAL,DMCB,(2,8(R6)),(R4)                                     
         CLI   DMCB,0                                                           
         BNE   FLDERR                                                           
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BM    FLDERR                    CAN'T BE MINUS                         
         C     R1,=F'999999900'     MAXIMUM                                     
         BH    FLDERR                                                           
         CVD   R1,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET REMAINDER 0                         
         BNE   FLDERR                                                           
         B     CHKCH90                                                          
*                                                                               
CHKCH49  DS    0H                                                               
         CLI   DATASW,C'R'         REFERENCE NUMBER                             
         BNE   CHKCH49E                                                         
         TM    4(R6),X'20'         TEST FIELD CHANGED                           
         BO    CHKCH90                                                          
         CLI   5(R6),10       MAX IS 10 CHARACTERS                              
         BH    FLDERR                                                           
         CLC   AGYALPHA,=C'BS'    SPECIAL FOR BACKER                            
         BNE   CHKCH49C                                                         
         CLI   5(R6),6        THEY HAVE ONLY 6 CHARACTERS                       
         BH    FLDERR                                                           
*                                                                               
CHKCH49C DS    0H                                                               
         B     CHKCH90                                                          
*                                                                               
CHKCH49E DS    0H                                                               
         CLI   DATASW,C'H'         SHIP DATE                                    
         BNE   CHKCH52                                                          
         TM    4(R6),X'20'       TEST FIELD CHANGED                             
         BO    CHKCH90                                                          
         GOTO1 VDATVAL,DMCB,(0,8(R6)),WORK                                      
         OC    DMCB,DMCB                                                        
         BNZ   CHKCH90                                                          
         B     CHKCH15E            DATE ERROR                                   
*                                                                               
CHKCH52  CLI   DATASW,C'$'        RATE CHANGE                                   
         BE    CHKCH52A                                                         
         CLI   DATASW,C'M'        INCH RATE (NEWS)                              
         BE    CHKCH52A                                                         
         CLI   DATASW,C'N'        LINE RATE (NEWS)                              
         BE    CHKCH52A                                                         
         B     CHKCHERR                                                         
*                                                                               
CHKCH52A DS    0H                                                               
         TM    4(R6),X'20'       TEST FIELD CHANGED                             
         BO    CHKCH90                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+27(4),0(R7)        MUST REREAD BUY                           
         BAS   RE,GETREC                                                        
*                                                                               
*                                                                               
         MVC   SAVFLD(5),PBDCOS     SAVE ORIGINAL COST AND TYPES                
         MVC   SAVFLD+5(1),PBDCOSTY                                             
         MVC   SAVFLD+6(1),PBDCTYP                                              
         MVC   SAVFLD+7(1),PBDRLIND                                             
         MVC   SAVFLD+8(1),PBDCOSIN                                             
*                                                                               
         CLI   PBUYKMED,C'N'                                                    
         BE    CHKCH52C                                                         
         GOTO1 =A(EDTRTM),DMCB,(RC),(RA),(R6),RR=RELO01                         
         LTR   R3,R3        CHECK FOR ERROR                                     
         BNZ   CHKXIT                                                           
         B     CHKCH90                                                          
*                                                                               
CHKCH52C GOTO1 =A(EDTRTN),DMCB,(RC),(RA),(R6),RR=RELO01                         
         LTR   R3,R3        CHECK FOR ERROR                                     
         BNZ   CHKXIT                                                           
         B     CHKCH90                                                          
*                                                                               
CHKCHERR DC    H'0'                INVALID DATASW                               
*                                                                               
CHKCH90  B     CHKLP9              GO DO NEXT FIELD                             
CHKXIT   XIT1  REGS=(R3,R6)                                                     
*                                                                               
FLDMIS   LA    R3,MISSERR                                                       
         B     CHKXIT                                                           
*                                                                               
FLDERR   LA    R3,FLDINV                                                        
         B     CHKXIT                                                           
         EJECT                                                                  
*                                                                               
*                                                                               
*                                  ROUTINE TO GET NEXT CLIENT                   
GETNCLT  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'02'                                                      
GETNC1   MVC   KEY+4(3),WORKCLT                                                 
         MVI   KEY+7,X'FF'                                                      
         BAS   RE,HIGH                                                          
         XC    WORKCLT,WORKCLT     NOT FOUND CLEAR WORKCLT                      
         CLC   KEY(4),KEYSAVE                                                   
         BNE   GETNCX                                                           
         CLI   SVCLT,C'*'          SEE IF DOING OFFICE                          
         BNE   GETNC10                                                          
         BAS   RE,GETREC                                                        
         CLI   SVCLT+1,C'-'          SEE IF DOING ALL BUT                       
         BNE   GETNC3                                                           
         CLC   PCLTOFF(1),SVCLT+2                                               
         BE    GETNC8              BYPASS THIS CLIENT                           
         B     GETNC10                                                          
*                                                                               
GETNC3   CLI   SVCLT+2,C' '        SEE IF DOING RANGE                           
         BNH   GETNC5              NO                                           
         CLC   PCLTOFF(1),SVCLT+1                                               
         BL    GETNC8              LOW SKIP                                     
         CLC   PCLTOFF(1),SVCLT+2                                               
         BH    GETNC8              HIGH SKIP                                    
         B     GETNC10             PROCESS                                      
*                                                                               
GETNC5   CLC   PCLTOFF(1),SVCLT+1  ONE OFFICE - MUST MATCH                      
         BNE   GETNC8              NO SKIP                                      
         B     GETNC10                                                          
*                                                                               
GETNC8   MVC   WORKCLT,KEY+4                                                    
         B     GETNC1              GO CHK NEXT CLIENT                           
*                                                                               
GETNC10  MVC   WORKCLT,KEY+4       PROCESS THIS CLIENT                          
         MVC   SVCLPROF,PCLTPROF   SAVE CLIENT PROFILE                          
         MVC   SVCLPROF+30(1),PCLTSTAT    SAVE CLIENT "STATUS"                  
*                                                                               
         XC    SADVDATA,SADVDATA                                                
         LA    R2,PCLTREC+33                                                    
         MVI   ELCODE,X'15'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   GETNC14                                                          
*                                                                               
         MVC   SADVDATA(18),2(R2)    SAVE ADVERTISER DATA                       
*                                                                               
GETNC14  DS    0H                                                               
*                                                                               
GETNCX   XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
NEXTEL   DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  SET CONDITION CODE NE                        
         EJECT                                                                  
FINDFSI  NTR1                                                                   
*                                  RETURNS FSI NUMBER IN DUB(6)                 
         ZAP   DUB(6),=P'0'                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(3),PBUYREC                                                   
         MVI   KEY+3,X'27'                                                      
         MVC   KEY+4(6),PBUYKPUB                                                
         MVC   KEY+10(3),PBUYKCLT                                               
FINDF5   BAS   RE,HIGH                                                          
         CLC   KEY(8),KEYSAVE       BASE PUB NUMBER                             
         BNE   FINDFX                                                           
         CLC   KEY(10),KEYSAVE      PUB + ZONE/EDT                              
         BE    FINDF10                                                          
         CLI   KEYSAVE+8,X'FF'  WAS I LOOKING FOR "ALL ZONE/EDT" REC            
         BE    FINDFX                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(8),KEYSAVE                                                   
         MVC   KEY+10(3),PBUYKCLT                                               
         MVC   KEY+8(2),=X'FFFF'                                                
         B     FINDF5                                                           
*                                                                               
FINDF10  CLC   KEY+10(3),PBUYKCLT        SEE IF I FOUND MY CLIENT               
         BE    FINDF30                                                          
FINDF15  CLC   KEY+10(3),=X'FFFFFF'      OR ALL CLT REC                         
         BE    FINDF30                                                          
         CLC   KEYSAVE+10(3),=X'FFFFFF'   WAS I LOOKING FOR ALL CLT REC         
         BE    FINDFX                                                           
         XC    KEY,KEY                                                          
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+10(3),=X'FFFFFF'       TRY FOR ALL CLT REC                   
         BAS   RE,HIGH                                                          
         B     FINDF15                                                          
*                                                                               
FINDF30  DS    0H                                                               
         MVC   AREC,ACONIO         READ INTO CONIO                              
         BAS   RE,GETREC                                                        
         L     R2,AREC                                                          
         LA    R2,33(R2)                                                        
         MVI   ELCODE,X'01'                                                     
FINDF35  CLI   0(R2),0                                                          
         BE    FINDFX                                                           
         CLI   0(R2),X'01'                                                      
         BE    FINDF45                                                          
FINDF40  BAS   RE,NEXTEL                                                        
         BNE   FINDFX                                                           
         USING PFSIEL01,R2                                                      
FINDF45  MVC   FULL(3),PFSIDATE                                                 
         XC    FULL(3),=X'FFFFFF'      COMPLEMENT FSI DATE                      
         CLC   PBUYKDAT,FULL                                                    
         BL    FINDF40                 KEEP LOOKING                             
         ZAP   DUB(6),PFSINUM          USE THIS NUMBER                          
*                                                                               
FINDFX   DS    0H                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
MYPK4    DS    PL4                                                              
*                                                                               
SAVSPACE DS    CL17                                                             
SAVUNITS DS    CL3                                                              
SAVCLMS  DS    CL2                                                              
SAVUIND  DS    CL1                                                              
SAVFSI   DS    PL5                                                              
*                                                                               
SAVFLD   DS    CL10               SAVED BUYREC DATA                             
WARN     DS    CL1                                                              
*                                                                               
         SPACE 3                                                                
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (DIRECTORY)                                   
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
*                                                                               
         CLI   SVIOSW,0            PASS DELETED RECS WHEN DOING LASTIO          
         BE    *+8                                                              
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
*                                                                               
         B     DIRCTRY                                                          
*                                                                               
*                                                                               
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         CLI   SVIOSW,0            PASS DELETED RECS WHEN DOING LASTIO          
         BE    *+8                                                              
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
*                                                                               
         B     DIRCTRY                                                          
*                                                                               
*                                                                               
*                                                                               
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
*                                                                               
*                                                                               
*                                                                               
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
*                                                                               
         CLI   SVIOSW,0            PASS DELETED RECS WHEN DOING LASTIO          
         BE    *+8                                                              
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
*                                                                               
         B     FILE                                                             
*                                                                               
*                                                                               
*                                                                               
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
*                                                                               
*                                                                               
*                                                                               
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
*                                                                               
HIGHPUB  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIR                                                           
*                                                                               
SEQPUB   MVC   COMMAND,=C'DMRSEQ'                                               
*                                                                               
PUBDIR   NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
*                                                                               
GETPUB   MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILE                                                          
*                                                                               
PUBFILE  NTR                                                                    
         LA    R2,KEY+27                                                        
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            X        
               (R2),APUBIO,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
*                                                                               
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
ALPHATAB DS    0H                                                               
         DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
*                                                                               
         EJECT                                                                  
BUYOUTA  DS    600C                                                             
         DS    10C                     EXTRA BYTES                              
       ++INCLUDE PPBYOUTD                                                       
         EJECT                                                                  
RECUPDT  CSECT                                                                  
         NMOD1 0,RECUPDT                                                        
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T418FFD,RA                                                       
         LA    R9,RECUPDT+4095                                                  
         LA    R9,1(R9)                                                         
         USING RECUPDT+4096,R9    NOTE USE OF R9 AS SECOND BASE                 
*                             *** DON'T TOUCH R8 IN ANY CSECT ***               
*                             *** IT IS NEEDED TO ACCESS ROUTINES               
*                             *** IN THE MAIN PROGRAM                           
*                                                                               
*                                                                               
         OI    DMINBTS,X'80'           SET READ FOR UPDATE                      
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
*                                                                               
         LA    R7,SVDISC                                                        
         MVC   COUNT,=H'16'                                                     
         LA    R6,MBCDT01H         FIRST CHANGABLE DATA FIELD                   
         LA    R5,MBCOT01H         FIRST BUY DISPLAY FIELD                      
*                                                                               
RECLOOP  OC    0(4,R7),0(R7)                                                    
         BZ    RECUPEX                                                          
*                                                                               
         TM    1(R6),X'20'              IF FIELD HAS BEEN PROTECTED             
         BO    RECUP90                  SKIP REC UPDATE - GOTO NEXT.            
*                                                                               
         TM    4(R6),X'20'              HAS FIELD BEEN ALTERED.                 
         BO    RECUP90                  IF NO SKIP REC UPDATE - NEXT.           
*                                                                               
         CLI   8(R6),C'*'               MEANS SKIP THIS BUY                     
         BE    RECUP90                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+27(4),0(R7)                                                  
*                                                                               
         BAS   RE,GETREC                                                        
         LA    R2,PBUYREC+33                                                    
         CLI   0(R2),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RECUP05  MVI   CHADTIND,0                                                       
         MVI   CHADTIN2,0                                                       
         MVI   CHADTIN3,0                                                       
         MVI   CHADTIN4,0                                                       
         CLI   DATASW,C'A'                                                      
         BNE   RECUP10                                                          
         OC    PBDJOB,PBDJOB                                                    
         BNZ   RECUP08                                                          
         CLI   5(R6),0             CHK FOR INPUT                                
         BE    RECUP90             THERE WAS NO JOB AND STILL ISN'T             
*                                  DON'T NEED TO WRITE BACK RECORD              
         CLC   8(11,R6),=C'NONE       '                                         
         BE    RECUP90                                                          
*                                                                               
         GOTO1 =A(FSILOOK),DMCB,(RC),(RA),RR=RELO01                             
*                                                                               
         MVC   PBDJOB,8(R6)        ADDING NEW JOB IS NOT A CHANGE               
         OC    PBDJOB,=6C' '                                                    
*                                                                               
         OI    CHADTIN3,X'40'         SET AD CODE ADDED FLAG                    
         B     RECUP84                                                          
*                                                                               
RECUP08  DS    0H                                                               
*                                                                               
         MVC   SAVFLD(6),PBDJOB        SAVE OLD JOB                             
         XC    PBDJOB,PBDJOB                                                    
         CLC   8(11,R6),=C'NONE       '                                         
         BE    RECUP09                                                          
         CLI   5(R6),0                NO INPUT                                  
         BE    RECUP09                                                          
         MVC   PBDJOB,8(R6)                                                     
         OC    PBDJOB,=6C' '                                                    
RECUP09  CLC   PBDJOB,SAVFLD                                                    
         BE    RECUP90              NO 'REAL' CHANGE                            
         CLC   8(11,R6),=C'NONE       '                                         
         BE    RECUP09C                                                         
         CLI   5(R6),0                NO INPUT                                  
         BE    RECUP09C                                                         
*                                   DON'T WRITE RECORD BACK                     
*                                   FIND AND STORE FSI DATA                     
*                                   ON JOB CHANGES                              
         GOTO1 =A(FSILOOK),DMCB,(RC),(RA),RR=RELO01                             
         MVC   PBDJOB,8(R6)                                                     
         OC    PBDJOB,=6C' '                                                    
*                                                                               
RECUP09C DS    0H                                                               
*                                                                               
         OI    CHADTIN2,X'08'                                                   
         B     RECUP84                                                          
*                                                                               
RECUP10  CLI   DATASW,C'S'            STATUS CHANGE                             
         BNE   RECUP12                                                          
         MVC   SAVFLD(1),PBDBFD                                                 
         CLI   8(R6),C'T'                                                       
         BNE   RECUP10C                                                         
         MVI   PBDBFD,C'T'                                                      
         B     RECUP10X                                                         
*                                                                               
RECUP10C CLI   8(R6),C'L'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    1(R6),X'20'          MUST PROTECT SO THEY CAN'T MAKE             
*                                   TEST AGAIN                                  
         OI    6(R6),X'80'          ALSO SET FIELD STATUS CHANGE                
*                                                                               
         CLI   SAVFLD,C'T'          SEE IF IT WAS TEST                          
         BNE   RECUP10X                                                         
         GOTO1 =A(ASR),DMCB,(RC),(RA),RR=RELO01                                 
         MVI   PBDBFD,0                                                         
         MVI   PBDDTIND,0                                                       
         MVI   PBDDTIN2,0                                                       
         MVI   PBDDTIN3,X'04'         SET TO MADE LIVE                          
         MVI   CHADTIN3,X'04'         SET TO MADE LIVE                          
         MVC   PBDBUYDT,BTODAY                                                  
*                                  NOW DELETE OLD X'24' PCHGELEMS               
         MVI   ELCODE,X'24'                                                     
RECUP10F LA    R2,PBDELEM                                                       
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP84                                                          
         GOTO1 VRECUP,DMCB,(1,PBUYREC),0(R2),0                                  
         BAS   RE,CLEAREND                                                      
         B     RECUP10F                                                         
*                                                                               
RECUP10X CLC   PBDBFD,SAVFLD                                                    
         BE    RECUP90               NO REAL CHANGE                             
         B     RECUP84                                                          
*                                                                               
RECUP12  CLI   DATASW,C'L'          NV LETTER                                   
         BNE   RECUP14                                                          
         MVC   SAVFLD(1),PBDLETR                                                
         NI    PBDLETR,X'FE'        SET OFF X'01'                               
         CLI   5(R6),0              CHK FOR INPUT                               
         BE    RECUP12C                                                         
         CLI   8(R6),C'Y'                                                       
         BE    RECUP12C                                                         
         OI    PBDLETR,X'01'                                                    
*                                                                               
RECUP12C CLC   SAVFLD,PBDLETR       CHK FOR 'REAL' CHANGE                       
         BE    RECUP90              NONE                                        
         B     RECUP85              MUST WRITE RECORD BACK                      
*                                   SKIPS X'24' ELEM LOGIC                      
*                                                                               
RECUP14  CLI   DATASW,C'X'          SPECIAL FINANCIAL HANDLING (SFH)            
         BNE   RECUP15                                                          
         MVC   SAVFLD(1),PBDSTAT                                                
         NI    PBDSTAT,X'FF'-X'08'  SET OFF X'08' (HELD FLAG)                   
         CLI   5(R6),0              CHK FOR INPUT                               
         BE    RECUP14C                                                         
         CLI   8(R6),C'R'          R FOR RELEASE ?                              
         BE    RECUP14C            YES - X'08' FLAG S/B OFF                     
         OI    PBDSTAT,X'08'                                                    
*                                                                               
RECUP14C CLC   SAVFLD(1),PBDSTAT    CHK FOR 'REAL' CHANGE                       
         BE    RECUP90              NONE                                        
         OI    CHADTIN4,X'80'       SFH STATUS CHANGE                           
         B     RECUP84      GO TO CHANGE ELEMENT (X'24') LOGIC                  
*                                   SKIPS X'24' ELEM LOGIC                      
*                                                                               
RECUP15  DS    0H                                                               
*                                                                               
         CLI   DATASW,C'1'                                                      
         BNE   RECUP16                                                          
         MVC   SAVFLD(3),PBDSDATE                                               
         XC    PBDSDATE,PBDSDATE                                                
         CLI   5(R6),0                                                          
         BE    RECUP15C                                                         
         GOTO1 VDATVAL,DMCB,(0,8(R6)),WORK                                      
         GOTO1 VDATCON,DMCB,(0,WORK),(3,PBDSDATE)                               
RECUP15C CLC   PBDSDATE,SAVFLD                                                  
         BE    RECUP90             NO 'REAL' CHANGE                             
         OI    CHADTIN2,X'40'                                                   
         B     RECUP84                                                          
*                                                                               
RECUP16  DS    0H                                                               
         CLI   DATASW,C'E'            T/S STATUS                                
         BNE   RECUP17                                                          
         XC    SAVFLD(1),SAVFLD                                                 
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'95'                                                     
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP16C                                                         
         USING PTSHTELD,R2                                                      
         MVC   SAVFLD(1),PTSHSTAT    SAVE OLD STATUS                            
*                                                                               
RECUP16C CLI   5(R6),0              CHECK FOR INPUT                             
         BNE   RECUP16E                                                         
RECUP16D CLI   SAVFLD,0             DID ELEM EXIST                              
         BE    RECUP90              NO - THEN NO CHANGE                         
         CLI   PTSHSTAT,C' '        SEE IF IT WAS BLANK                         
         BE    RECUP90              YES THEN NO CHANGE                          
*                                                                               
         MVI   PTSHSTAT,C' '        CHANGE STATUS TO BALNK                      
         MVC   PTSHCDAT,BTODAY      SET TODAY'S DATE AS LAST CHANGE             
*                                   IN OLD ELEMENT                              
         NI    PBDSTAT,X'EF'        ALSO SET OFF T/S BIT                        
         B     RECUP84                                                          
*                                                                               
RECUP16E DS    0H                                                               
         CLI   SAVFLD,0              SEE IF ELEMENT ALREADY EXISTED             
         BE    RECUP16H              NO                                         
         CLC   SAVFLD(1),8(R6)                                                  
         BE    RECUP90              NO CHANGE                                   
         MVC   PTSHSTAT,8(R6)       SET NEW STATUS IN ELEMENT                   
         CLC   PTSHCDAT,BTODAY      SEE IF LAST CHANGE WAS TODAY                
         BE    RECUP16F                                                         
         MVC   PTSHCDAT,BTODAY      SET TODAY'S DATE AS LAST CHANGE             
         XC    PTSHCIN1(2),PTSHCIN1   CLEAR CHANGE INDICATORS                   
RECUP16F OI    PTSHCIN1,X'01'                                                   
         MVC   PTSHBID,=C'   '     SINCE MBC HAS NO BUYER ID                    
         NI    PBDSTAT,X'EF'       SET OFF T/S BIT                              
         CLI   PTSHSTAT,C'A'       SEE IF APPROVED                              
         BNE   *+8                                                              
         OI    PBDSTAT,X'10'       SET T/S BIT ON                               
         B     RECUP16X                                                         
*                                                                               
*                                                                               
RECUP16H LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'95'                                                     
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP16J                                                         
         DC    H'0'                 SOMETHING VERY WRONG                        
*                                   THERE CAN'T BE AN ELEMENT NOW               
RECUP16J XC    WORK(40),WORK                                                    
         MVC   WORK(2),=X'9527'                                                 
         MVC   WORK+PTSHSTAT-PTSHTEL(1),8(R6)                                   
         MVC   WORK+PTSHIDAT-PTSHTEL(3),BTODAY                                  
         MVC   WORK+PTSHBID-PTSHTEL(3),=C'   ' SINCE MBC HAS NO BUY ID          
         GOTO1 VRECUP,DMCB,(1,PBUYREC),WORK,(R2)                                
         NI    PBDSTAT,X'EF'       SET OFF T/S BIT                              
         CLI   PTSHSTAT,C'A'       SEE IF APPROVED * NOT FOR WESTERN*           
         BNE   *+8                                                              
         OI    PBDSTAT,X'10'       SET T/S BIT ON                               
RECUP16X B     RECUP84                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
*                                                                               
RECUP17  DS    0H                                                               
         CLI   DATASW,C'R'            REFERENCE NUMBER                          
         BNE   RECUP18                                                          
         XC    SAVFLD(10),SAVFLD                                                
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'83'                                                     
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP17C                                                         
         MVC   SAVFLD(10),2(R2)                                                 
*                                                                               
RECUP17C CLI   5(R6),0              CHECK FOR INPUT                             
         BNE   RECUP17E                                                         
RECUP17D OC    SAVFLD(10),SAVFLD    DID ELEM EXIST                              
         BZ    RECUP90              NO - THEN NO CHANGE                         
*                                   YES - THE DELETE ELEMENT                    
         GOTO1 VRECUP,DMCB,(1,PBUYREC),(R2) THEN DELETE IT                      
         BAS   RE,CLEAREND                                                      
         B     RECUP84                                                          
*                                                                               
RECUP17E DS    0H                                                               
         ZIC   R1,5(R6)             INPUT LENGTH                                
         XC    WORK+20(10),WORK+20                                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+20(0),8(R6)                                                 
         OC    SAVFLD(10),SAVFLD     SEE IF ELEMENT ALREADY EXISTED             
         BZ    RECUP17H              NO                                         
         CLC   SAVFLD(10),WORK+20                                               
         BE    RECUP90              NO CHANGE                                   
         MVC   2(10,R2),WORK+20                                                 
         B     RECUP84                                                          
*                                                                               
RECUP17H LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'83'                                                     
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP17J                                                         
         DC    H'0'                 SOMETHING VERY WRONG                        
*                                   THERE CAN'T BE AN ELEMENT NOW               
RECUP17J XC    WORK(15),WORK                                                    
         MVC   WORK(2),=X'830F'                                                 
         MVC   WORK+2(10),WORK+20                                               
         GOTO1 VRECUP,DMCB,(1,PBUYREC),WORK,(R2)                                
         B     RECUP84                                                          
*                                                                               
*                                                                               
RECUP18  DS    0H                                                               
         CLI   DATASW,C'D'            DLC                                       
         BNE   RECUP19                                                          
         XC    SAVFLD(6),SAVFLD                                                 
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'81'                                                     
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP18C                                                         
         MVC   SAVFLD(6),2(R2)                                                  
*                                                                               
RECUP18C CLI   5(R6),0              CHECK FOR INPUT                             
         BNE   RECUP18E                                                         
RECUP18D OC    SAVFLD(6),SAVFLD     DID ELEM EXIST                              
         BZ    RECUP90              NO - THEN NO CHANGE                         
*                                   YES - THE DELETE ELEMENT                    
         GOTO1 VRECUP,DMCB,(1,PBUYREC),(R2) THEN DELETE IT                      
         BAS   RE,CLEAREND                                                      
         B     RECUP84                                                          
*                                                                               
RECUP18E DS    0H                                                               
         ZIC   R0,5(R6)             INPUT LENGTH                                
         GOTO1 VCASHVAL,DMCB,(2,8(R6)),(R0)                                     
*                                     NOTE THAT ERRORS ARE CHECKED              
*                                     IN CHKCHA                                 
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB(6),=P'0'         INPUT IS ZERO - DELETE ELEM                 
         BE    RECUP18D             IF I NEED TO                                
         OC    SAVFLD(6),SAVFLD      SEE IF ELEMENT ALREADY EXISTED             
         BZ    RECUP18H              NO                                         
         CLC   SAVFLD(6),DUB                                                    
         BE    RECUP90              NO CHANGE                                   
         MVC   2(6,R2),DUB                                                      
         B     RECUP84                                                          
*                                                                               
RECUP18H LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'81'                                                     
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP18J                                                         
         DC    H'0'                 SOMETHING VERY WRONG                        
*                                   THERE CAN'T BE AN ELEMENT NOW               
RECUP18J XC    WORK(12),WORK                                                    
         MVC   WORK(2),=X'810A'                                                 
         MVC   WORK+2(6),DUB                                                    
         GOTO1 VRECUP,DMCB,(1,PBUYREC),WORK,(R2)                                
         B     RECUP84                                                          
*                                                                               
RECUP19  DS    0H                                                               
         CLI   DATASW,C'F'            FSI                                       
         BNE   RECUP20                                                          
         XC    SAVFLD(5),SAVFLD                                                 
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'82'                                                     
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP19C                                                         
         USING PBFSIELD,R2                                                      
         MVC   SAVFLD(5),PBFSI                                                  
         DROP  R2                                                               
*                                                                               
RECUP19C CLI   5(R6),0              CHECK FOR INPUT                             
         BNE   RECUP19E                                                         
RECUP19D OC    SAVFLD(5),SAVFLD     DID ELEM EXIST                              
         BZ    RECUP90              NO - THEN NO CHANGE                         
*                                   YES - THE DELETE ELEMENT                    
         GOTO1 VRECUP,DMCB,(1,PBUYREC),(R2) THEN DELETE IT                      
         BAS   RE,CLEAREND                                                      
         B     RECUP85               NOT RECUP84 SINCE NO 24 ELEM               
*                                    CHANGE                                     
RECUP19E DS    0H                                                               
*                                    ACCEPT N, NO, Y, YES OR A NUMBER           
         ZAP   DUB(6),=P'0'                                                     
         CLI   5(R6),1                                                          
         BNE   RECUP19F                                                         
         CLI   8(R6),C'N'            ACCEPT N, NO, Y, YES OR A NUMBER           
         BE    RECUP19H                                                         
         CLI   8(R6),C'Y'                                                       
         BNE   RECUP19F                                                         
         BAS   RE,FINDFSI                                                       
*                                                                               
         XC    KEY,KEY           MUST RE-READ BUYREC                            
         MVC   KEY+27(4),0(R7)                                                  
         BAS   RE,GETREC                                                        
*                                                                               
         B     RECUP19H                                                         
*                                                                               
RECUP19F CLC   8(2,R6),=C'NO'                                                   
         BE    RECUP19H                                                         
         CLC   8(3,R6),=C'YES'                                                  
         BNE   RECUP19G                                                         
         BAS   RE,FINDFSI                                                       
*                                                                               
         XC    KEY,KEY           MUST RE-READ BUYREC                            
         MVC   KEY+27(4),0(R7)                                                  
         BAS   RE,GETREC                                                        
*                                                                               
         B     RECUP19H                                                         
*                                                                               
RECUP19G ZIC   R0,5(R6)             INPUT LENGTH                                
         GOTO1 VCASHVAL,DMCB,(2,8(R6)),(R0)                                     
*                                     NOTE THAT ERRORS ARE CHECKED              
*                                     IN CHKCHA                                 
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         DP    DUB,=P'100'                                                      
*                                                                               
RECUP19H OC    SAVFLD(5),SAVFLD      SEE IF ELEMENT ALREADY EXISTED             
         BZ    RECUP19M              NO                                         
         CLC   SAVFLD(5),DUB+1      PL5                                         
         BE    RECUP90              NO CHANGE                                   
         USING PBFSIELD,R2                                                      
         MVC   PBFSI,DUB+1          (PL5)                                       
         MVI   PBFSIIND,X'02'       OVERRIDDEN                                  
         CLI   8(R6),C'Y'          SEE IF Y OR YES ENTERED                      
         BNE   *+8                                                              
         MVI   PBFSIIND,X'01'      LOOKED-UP                                    
         MVC   PBFSILDT,BTODAY                                                  
         B     RECUP85             NOT RECUP84 SINCE NO X'24' ELEM              
         DROP  R2                   CHANGE                                      
*                                                                               
RECUP19M LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'82'                                                     
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP19O                                                         
         DC    H'0'                 SOMETHING VERY WRONG                        
*                                   THERE CAN'T BE AN ELEMENT NOW               
RECUP19O XC    WORK(13),WORK                                                    
         MVC   WORK(2),=X'820D'                                                 
         MVI   WORK+2,X'02'      OVERRIDDEN                                     
         CLI   8(R6),C'Y'        SEE IF Y OR YES ENTERED                        
         BNE   *+8                                                              
         MVI   WORK+2,X'01'      LOOKED-UP                                      
*                                                                               
         MVC   WORK+3(3),BTODAY                                                 
         MVC   WORK+6(5),DUB+1                                                  
         GOTO1 VRECUP,DMCB,(1,PBUYREC),WORK,(R2)                                
         B     RECUP85              NOT RECUP84 SINCE NO X'24' ELEM             
*                                   CHANGE                                      
*                                                                               
RECUP20  CLI   DATASW,C'2'                                                      
         BNE   RECUP25                                                          
         MVC   SAVFLD(3),PBDBDATE                                               
         GOTO1 VDATVAL,DMCB,(0,8(R6)),WORK                                      
         GOTO1 VDATCON,DMCB,(0,WORK),(3,PBDBDATE)                               
         CLC   PBDBDATE,SAVFLD                                                  
         BE    RECUP90              NO 'REAL' CHANGE                            
         OI    CHADTIN2,X'20'                                                   
         B     RECUP84                                                          
*                                                                               
RECUP25  DS    0H                                                               
         CLI   DATASW,C'B'         COST 2 FACTOR                                
         BNE   RECUP30                                                          
         XC    SAVFLD(5),SAVFLD                                                 
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'91'                                                     
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP25C                                                         
         MVC   SAVFLD(5),2(R2)                                                  
*                                                                               
RECUP25C CLI   5(R6),0              CHECK FOR INPUT                             
         BNE   RECUP25E                                                         
RECUP25D OC    SAVFLD(5),SAVFLD     DID ELEM EXIST                              
         BZ    RECUP90              NO - THEN NO CHANGE                         
*                                   YES - THEN DELETE ELEMENT                   
         GOTO1 VRECUP,DMCB,(1,PBUYREC),(R2)      DELETE IT                      
         BAS   RE,CLEAREND                                                      
         B     RECUP84                                                          
*                                                                               
RECUP25E DS    0H                                                               
         ZIC   R0,5(R6)             INPUT LENGTH                                
         GOTO1 VCASHVAL,DMCB,(X'86',8(R6)),(R0)                                 
*                                     NOTE THAT ERRORS ARE CHECKED              
*                                     IN CHKCHA                                 
         MVC   DUB,4(R1)                                                        
         CP    DUB+3(5),=P'0'       INPUT IS ZERO - DELETE ELEM                 
         BE    RECUP25D             IF I NEED TO                                
         OC    SAVFLD(5),SAVFLD      SEE IF ELEMENT ALREADY EXISTED             
         BZ    RECUP25H              NO                                         
         CLC   SAVFLD(5),DUB+3                                                  
         BE    RECUP90              NO CHANGE                                   
         MVC   2(5,R2),DUB+3                                                    
         OI    CHADTIN4,X'40'      COST 2 FACTOR CHANGE                         
         B     RECUP84                                                          
*                                                                               
RECUP25H LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'91'                                                     
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP25J                                                         
         DC    H'0'                 SOMETHING VERY WRONG                        
*                                   THERE CAN'T BE AN ELEMENT NOW               
RECUP25J XC    WORK(8),WORK                                                     
         MVC   WORK(2),=X'9108'                                                 
         MVC   WORK+2(5),DUB+3                                                  
         GOTO1 VRECUP,DMCB,(1,PBUYREC),WORK,(R2)                                
         B     RECUP84                                                          
*                                                                               
RECUP30  CLI   DATASW,C'3'                                                      
         BNE   RECUP40                                                          
         MVC   SAVFLD(3),PBDPDATE                                               
         GOTO1 VDATVAL,DMCB,(0,8(R6)),WORK                                      
         GOTO1 VDATCON,DMCB,(0,WORK),(3,PBDPDATE)                               
         CLC   PBDPDATE,SAVFLD                                                  
         BE    RECUP90              NO 'REAL' CHANGE                            
         OI    CHADTIN2,X'10'                                                   
         B     RECUP84                                                          
*                                                                               
RECUP40  CLI   DATASW,C'4'                                                      
         BNE   RECUP45                                                          
         MVC   SAVFLD(3),PBDCDATE                                               
         XC    PBDCDATE,PBDCDATE                                                
         CLI   SVAPROF+24,C'B'         SEE IF BOTH REQUIRED                     
         BE    RECUP40A                                                         
         CLI   SVAPROF+24,C'R'         SEE IF CLOSING DATE REQUIRED             
         BNE   RECUP40B                                                         
RECUP40A CLI   5(R6),0                                                          
         BE    FLDMIS                                                           
*                                                                               
RECUP40B CLI   5(R6),0                                                          
         BE    RECUP40C                                                         
         GOTO1 VDATVAL,DMCB,(0,8(R6)),WORK                                      
         GOTO1 VDATCON,DMCB,(0,WORK),(3,PBDCDATE)                               
RECUP40C CLC   PBDCDATE,SAVFLD                                                  
         BE    RECUP90            NO 'REAL' CHANGE                              
         OI    CHADTIN2,X'80'                                                   
         B     RECUP84                                                          
*                                                                               
RECUP45  CLI   DATASW,C'5'                                                      
         BNE   RECUP50                                                          
         MVC   SAVFLD(3),PBDMDATE                                               
         XC    PBDMDATE,PBDMDATE                                                
         CLI   SVAPROF+24,C'B'         SEE IF BOTH REQUIRED                     
         BE    RECUP45A                                                         
         CLI   SVAPROF+24,C'M'         SEE IF MAT CLOSING DATE REQUIRED         
         BNE   RECUP45B                                                         
RECUP45A CLI   5(R6),0                                                          
         BE    FLDMIS                                                           
*                                                                               
RECUP45B CLI   5(R6),0                                                          
         BE    RECUP45C                                                         
         GOTO1 VDATVAL,DMCB,(0,8(R6)),WORK                                      
         GOTO1 VDATCON,DMCB,(0,WORK),(3,PBDMDATE)                               
RECUP45C CLC   PBDMDATE,SAVFLD                                                  
         BE    RECUP90            NO 'REAL' CHANGE                              
         OI    CHADTIN3,X'02'     MATERIALS CLOSING DATE CHG                    
         B     RECUP84                                                          
*                                                                               
RECUP50  CLI   DATASW,C'7'                 CASHDISC                             
         BNE   RECUP55                                                          
         MVC   SAVFLD(2),PBDCD                                                  
         LA    R4,PBUYREC+7                                                     
*****    GOTO1 =V(GETINS),DMCB,PBUYREC,GROSS,(R4),RR=RELO01                     
         GOTO1 VGETINS,DMCB,PBUYREC,GROSS,(R4)                                  
         MVC   SVGROSS(12),GROSS  SAVE GROSS,AC,CD                              
         ZIC   R4,5(R6)                                                         
         LTR   R4,R4                                                            
         BNZ   RECUP52                                                          
         ZAP   PBDCD,=P'0'                                                      
         B     RECUP53                                                          
RECUP52  GOTO1 VCASHVAL,DMCB,(1,8(R6)),(R4)                                     
         L     R4,DMCB+4                                                        
         CVD   R4,DUB                                                           
         MVC   PBDCD,DUB+6                                                      
*                                                                               
RECUP53  CLC   PBDCD,SAVFLD                                                     
         BE    RECUP90                  NO 'REAL' CHANGE                        
         LA    R4,PBUYREC+7                                                     
*****    GOTO1 =V(GETINS),DMCB,PBUYREC,GROSS,(R4),RR=RELO01                     
         GOTO1 VGETINS,DMCB,PBUYREC,GROSS,(R4)                                  
         OI    CHADTIN2,X'02'                                                   
         B     RECUP84                                                          
*                                                                               
RECUP55  CLI   DATASW,C'8'                 AGENCY COMMISSION                    
         BNE   RECUP59                                                          
         MVC   SAVFLD(3),PBDACP                                                 
         LA    R4,PBUYREC+7                                                     
*****    GOTO1 =V(GETINS),DMCB,PBUYREC,GROSS,(R4),RR=RELO01                     
         GOTO1 VGETINS,DMCB,PBUYREC,GROSS,(R4)                                  
         MVC   SVGROSS(12),GROSS  SAVE GROSS,AC,CD                              
         ZIC   R4,5(R6)                                                         
         LTR   R4,R4                       IS THERE AN ENTRY IN FIELD           
         BNZ   RECUP57                     IF NOT SET AGYCOM TO 0               
         ZAP   PBDACP,=P'0'                                                     
         B     RECUP58                                                          
*                                                                               
RECUP57  GOTO1 VCASHVAL,DMCB,(3,8(R6)),(R4)                                     
         L     R4,DMCB+4                                                        
         CVD   R4,DUB                                                           
         CP    DUB,=P'100000'                                                   
         BNE   RECUP57C                                                         
         ZAP   PBDACP,=P'-1'          SET 100.000 TO -1                         
         B     RECUP58                                                          
RECUP57C MVC   PBDACP,DUB+5                                                     
*                                                                               
RECUP58  CLC   PBDACP,SAVFLD                                                    
         BE    RECUP90                     NO 'REAL' CHANGE                     
*                                                                               
         CLI   GNOPT,C'Y'                                                       
         BNE   RECUP58D                                                         
         CLI   PBDCTYP,C'N'       MUST BE BOUGHT WITH NET (N) RATE              
         BNE   RECUP58D                                                         
*                                                                               
         CP    SAVFLD(3),=P'0'        SEE IF OLD AC WAS ZERO                    
         BNE   RECUP58B                                                         
*                                                                               
         ZAP   DUB,PBDCOS           GROSS-UP RATE WITH NEW AC                   
         CVB   R1,DUB                                                           
         M     R0,=F'100000'                                                    
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'       =NET PCT                                     
         LCR   RF,RF                                                            
         BNP   RECUP58D                                                         
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         CVD   R1,DUB                                                           
         ZAP   PBDCOS,DUB                                                       
         B     RECUP58D                                                         
*                                                                               
RECUP58B DS    0H              OLD AC WAS NOT ZERO                              
         CP    PBDACP,=P'0'    IS NEW ZERO                                      
         BNE   RECUP58D        NO-THEN DO NOTHING SPECIAL                       
*                                                                               
*        IF NEW AC IS ZERO THEN 'NET DOWN' OLD GROSS USING OLD AC               
*                                                                               
*                                                                               
         ZAP   DUB,PBDCOS                                                       
         CVB   R1,DUB                                                           
*                                                                               
         ZAP   DUB,SAVFLD(3)                                                    
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         CVD   R1,DUB                                                           
         ZAP   PBDCOS,DUB                                                       
*                                                                               
RECUP58D DS    0H                                                               
         LA    R4,PBUYREC+7                                                     
*****    GOTO1 =V(GETINS),DMCB,PBUYREC,GROSS,(R4),RR=RELO01                     
         GOTO1 VGETINS,DMCB,PBUYREC,GROSS,(R4)                                  
         OI    CHADTIN2,X'04'              SET CHANGE INDICATOR                 
*                                                                               
         B     RECUP84                                                          
*                                                                               
RECUP59  CLI   DATASW,C'C'                 CONTRACT UNITS                       
         BNE   RECUP59P                                                         
         MVC   SAVFLD(3),PBDCU                                                  
         ZIC   R4,5(R6)                                                         
         LTR   R4,R4                       IS THERE AN ENTRY IN FIELD           
         BNZ   RECUP59B                    IF NOT SET PBDCU TO 0                
         XC    PBDCU,PBDCU                                                      
         B     RECUP59G                                                         
*                                                                               
RECUP59B GOTO1 VCASHVAL,DMCB,(4,8(R6)),(R4)                                     
         L     R4,DMCB+4                                                        
         CVD   R4,DUB                                                           
         CP    DUB,=P'0'                                                        
         BNE   RECUP59D                                                         
         MVC   PBDCU,=X'000001'       SET 0 TO .0001                            
         B     RECUP59G                                                         
*                                                                               
RECUP59D MVC   PBDCU,DMCB+5             3 BYTES                                 
*                                                                               
RECUP59G CLC   PBDCU,SAVFLD                                                     
         BE    RECUP90                     NO 'REAL' CHANGE                     
         B     RECUP84                                                          
*                                                                               
RECUP59P DS    0H                                                               
         CLI   DATASW,C'P'          PLANNED COST                                
         BNE   RECUP60                                                          
         MVC   SAVFLD(4),PBDPLCOS                                               
         ZIC   R4,5(R6)                                                         
         LTR   R4,R4                       IS THERE AN ENTRY IN FIELD           
         BNZ   RECUP59Q                    IF NOT SET PBDPLCOS TO 0             
         XC    PBDPLCOS,PBDPLCOS                                                
         B     RECUP59S                                                         
*                                                                               
RECUP59Q GOTO1 VCASHVAL,DMCB,(2,8(R6)),(R4)                                     
*                                                                               
RECUP59R MVC   PBDPLCOS,DMCB+4          4 BYTES                                 
*                                                                               
RECUP59S CLC   PBDPLCOS,SAVFLD                                                  
         BE    RECUP90                     NO 'REAL' CHANGE                     
         OI    CHADTIN3,X'10'      PLANNED COST CHANGE                          
         B     RECUP84                                                          
*                                                                               
*                                                                               
RECUP60  DS    0H                                                               
         CLI   DATASW,C'T'            RPT                                       
         BNE   RECUP61                                                          
         XC    SAVFLD(3),SAVFLD                                                 
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'85'                                                     
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP60C                                                         
         MVC   SAVFLD(3),2(R2)      PL3                                         
*                                                                               
RECUP60C CLI   5(R6),0              CHECK FOR INPUT                             
         BNE   RECUP60E                                                         
RECUP60D OC    SAVFLD(3),SAVFLD     DID ELEM EXIST                              
         BZ    RECUP90              NO - THEN NO CHANGE                         
*                                   YES - THE DELETE ELEMENT                    
         GOTO1 VRECUP,DMCB,(1,PBUYREC),(R2) THEN DELETE IT                      
         BAS   RE,CLEAREND                                                      
         B     RECUP84                                                          
*                                                                               
RECUP60E DS    0H                                                               
         ZIC   R0,5(R6)             INPUT LENGTH                                
         GOTO1 VCASHVAL,DMCB,(2,8(R6)),(R0)                                     
*                                     NOTE THAT ERRORS ARE CHECKED              
*                                     IN CHKCHA                                 
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         DP    DUB,=P'100'                                                      
         OC    SAVFLD(3),SAVFLD      SEE IF ELEMENT ALREADY EXISTED             
         BZ    RECUP60H              NO                                         
         CLC   SAVFLD(3),DUB+3                                                  
         BE    RECUP90              NO CHANGE                                   
         MVC   2(3,R2),DUB+3                                                    
         B     RECUP84                                                          
*                                                                               
RECUP60H LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'85'                                                     
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP60J                                                         
         DC    H'0'                 SOMETHING VERY WRONG                        
*                                   THERE CAN'T BE AN ELEMENT NOW               
RECUP60J XC    WORK(12),WORK                                                    
         MVC   WORK(2),=X'8505'                                                 
         MVC   WORK+2(3),DUB+3     PL3                                          
         GOTO1 VRECUP,DMCB,(1,PBUYREC),WORK,(R2)                                
         B     RECUP84                                                          
*                                                                               
RECUP61  DS    0H                                                               
         CLI   DATASW,C'H'          SHIP DATE                                   
         BNE   RECUP62                                                          
         XC    SAVFLD(3),SAVFLD                                                 
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'86'                                                     
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP61C                                                         
         MVC   SAVFLD(3),2(R2)                                                  
*                                                                               
RECUP61C CLI   5(R6),0              CHECK FOR INPUT                             
         BNE   RECUP61E                                                         
RECUP61D OC    SAVFLD(3),SAVFLD     DID ELEM EXIST                              
         BZ    RECUP90              NO - THEN NO CHANGE                         
*                                   YES - THE DELETE ELEMENT                    
         GOTO1 VRECUP,DMCB,(1,PBUYREC),(R2) THEN DELETE IT                      
         BAS   RE,CLEAREND                                                      
         B     RECUP84                                                          
*                                                                               
RECUP61E DS    0H                                                               
         GOTO1 VDATVAL,DMCB,(0,8(R6)),WORK                                      
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK+12)                                
         OC    SAVFLD(3),SAVFLD      SEE IF ELEMENT ALREADY EXISTED             
         BZ    RECUP61H              NO                                         
         CLC   SAVFLD(3),WORK+12                                                
         BE    RECUP90              NO CHANGE                                   
         MVC   2(3,R2),WORK+12                                                  
         B     RECUP84                                                          
*                                                                               
RECUP61H LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'86'                                                     
         BAS   RE,RNEXTEL                                                       
         BNE   RECUP61J                                                         
         DC    H'0'                 SOMETHING VERY WRONG                        
*                                   THERE CAN'T BE AN ELEMENT NOW               
RECUP61J XC    WORK(12),WORK                                                    
         MVC   WORK(2),=X'8607'                                                 
         MVC   WORK+2(3),WORK+12                                                
         GOTO1 VRECUP,DMCB,(1,PBUYREC),WORK,(R2)                                
         B     RECUP84                                                          
*                                                                               
RECUP62  DS    0H                                                               
         CLI   DATASW,C'I'         IMPRESSION                                   
         BE    RECUP62A                                                         
         CLI   DATASW,C'V'         PAGE VIEWS                                   
         BE    RECUP62A                                                         
         CLI   DATASW,C'K'         CLICK THRUS                                  
         BNE   RECUP63                                                          
RECUP62A DS    0H                                                               
         XC    SAVFLD(5),SAVFLD                                                 
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'87'                                                     
         CLI   DATASW,C'V'         PAGE VIEWS                                   
         BE    RECUP62B                                                         
         MVI   ELCODE,X'88'                                                     
         CLI   DATASW,C'K'         CLICK THRUS                                  
         BE    RECUP62B                                                         
         MVI   ELCODE,X'92'        MUST BE IMPRESSION                           
*                                                                               
RECUP62B BAS   RE,RNEXTEL                                                       
         BNE   RECUP62C                                                         
         MVC   SAVFLD(5),2(R2)      PL5                                         
*                                                                               
RECUP62C CLI   5(R6),0              CHECK FOR INPUT                             
         BNE   RECUP62E                                                         
RECUP62D OC    SAVFLD(5),SAVFLD     DID ELEM EXIST                              
         BZ    RECUP90              NO - THEN NO CHANGE                         
*                                                                               
*  DELETE ELEMENT                                                               
*                                                                               
         OI    CHADTIN4,X'20'       CT, PV OR IMPS HAS BEEN CHANGED             
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PBUYREC),(R2)                                     
         BAS   RE,CLEAREND                                                      
         B     RECUP84                                                          
*                                                                               
RECUP62E DS    0H                                                               
         ZIC   R0,5(R6)            INPUT LENGTH                                 
         GOTO1 VNUMVAL,DMCB,(0,8(R6)),(R0)                                      
*                                                                               
* NOTE THAT ERRORS ARE CHECKED IN CHKCHA                                        
*                                                                               
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
*                                                                               
         CP    DUB,=P'0'           SEE IF ZERO ENTERED                          
         BE    RECUP62D            GO REMOVE OLD ELEM (IF NECESSARY)            
*                                                                               
         OC    SAVFLD(5),SAVFLD    SEE IF ELEMENT ALREADY EXISTED               
         BZ    RECUP62H            NO                                           
         CLC   SAVFLD(5),DUB+3                                                  
         BE    RECUP90             NO CHANGE                                    
         MVC   2(5,R2),DUB+3                                                    
*                                                                               
         OI    CHADTIN4,X'20'       CT, PV OR IMPS HAS BEEN CHANGED             
*                                                                               
         B     RECUP84                                                          
*                                                                               
RECUP62H LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'87'                                                     
         CLI   DATASW,C'V'         PAGE VIEWS                                   
         BE    RECUP62I                                                         
         MVI   ELCODE,X'88'                                                     
         CLI   DATASW,C'K'         CLICK THRUS                                  
         BE    RECUP62I                                                         
         MVI   ELCODE,X'92'        MUST BE IMPRESSION                           
*                                                                               
RECUP62I BAS   RE,RNEXTEL                                                       
         BNE   RECUP62J                                                         
         DC    H'0'                THERE CAN'T BE AN ELEMENT NOW                
*                                                                               
RECUP62J XC    WORK(12),WORK                                                    
*                                                                               
         MVC   WORK(2),=X'8707'                                                 
         CLI   DATASW,C'V'         PAGE VIEWS                                   
         BE    RECUP62K                                                         
         MVC   WORK(2),=X'8807'                                                 
         CLI   DATASW,C'K'         CLICK THRUS                                  
         BE    RECUP62K                                                         
         MVC   WORK(2),=X'9207'    MUST BE IMPRESSION                           
*                                                                               
RECUP62K MVC   WORK+2(5),DUB+3     PL5                                          
         GOTO1 VRECUP,DMCB,(1,PBUYREC),WORK,(R2)                                
*                                                                               
         OI    CHADTIN4,X'20'       CT, PV OR IMPS HAS BEEN CHANGED             
*                                                                               
         B     RECUP84                                                          
*                                                                               
RECUP63  DS    0H                                                               
         CLI   DATASW,C'$'    RATES                                             
         BE    RECUP63B                                                         
         CLI   DATASW,C'M'    INCH RATES                                        
         BE    RECUP63B                                                         
         CLI   DATASW,C'N'    LINE RATES                                        
         BNE   RECUP63X                                                         
*                                                                               
RECUP63B DS    0H                                                               
*                                                                               
         MVC   SAVFLD(5),PBDCOS     SAVE ORIGINAL COST AND TYPES                
         MVC   SAVFLD+5(1),PBDCOSTY                                             
         MVC   SAVFLD+6(1),PBDCTYP                                              
         MVC   SAVFLD+7(1),PBDRLIND                                             
         MVC   SAVFLD+8(1),PBDCOSIN                                             
*                                                                               
         LA    R4,PBUYREC+7     PRODUCT                                         
         GOTO1 VGETINS,DMCB,PBUYREC,GROSS,(R4)                                  
         MVC   SVGROSS(12),GROSS      SAVE GROSS,AC,CD                          
*                                                                               
         CLI   PBUYKMED,C'N'      SEE IF NEWSPAPERS                             
         BE    RECUP63N                                                         
*                                                                               
*        NON-NEWSPAPERS DATASW MUST BE $ SINCE THE OTHERS                       
*        ARE DISALLOWED IN PPMBC00                                              
*                                                                               
         GOTO1 =A(EDTRTM),DMCB,(RC),(RA),(R6),RR=RELO01                         
*                                                                               
RECUP63E DS    0H                                                               
         CLC   PBDCOS,SAVFLD                                                    
         BNE   RECUP63G                                                         
         CLC   PBDCOSTY,SAVFLD+5   NOTE:CHANGES ARE PREVENTED FOR NOW           
         BNE   RECUP63G                                                         
         CLC   PBDCTYP,SAVFLD+6                                                 
         BNE   RECUP63G                                                         
         CLC   PBDRLIND,SAVFLD+7                                                
         BNE   RECUP63G                                                         
         CLC   PBDCOSIN,SAVFLD+8   NOTE:CHANGES ARE PREVENTED FOR NOW           
         BE    RECUP90          NO REAL CHANGE                                  
*                                                                               
RECUP63G DS    0H                                                               
         LA    R4,PBUYREC+7      PRODUCT                                        
         GOTO1 VGETINS,DMCB,PBUYREC,GROSS,(R4)                                  
         CLC   PBDCOS,SAVFLD     SEE IF RATE CHANGED                            
         BE    RECUP84           NO - JUST SOME MODIFIER                        
*                                                                               
         MVC   SAVFLD(5),PBDCOS      SAVE CHANGED COST                          
         MVC   SAVFLD+5(1),PBDCOSTY   AND INDICATORS                            
         MVC   SAVFLD+6(1),PBDCTYP                                              
         MVC   SAVFLD+7(1),PBDRLIND                                             
         MVC   SAVFLD+8(1),PBDCOSIN                                             
*                                                                               
*        ABOVE CODE NEEDED SINCE ASR REREADS THE INSERTION                      
*                                                                               
         GOTO1 =A(ASR),DMCB,(RC),(RA),RR=RELO01                                 
*                                                                               
         MVC   PBDCOS,SAVFLD    RESTORE CHANGED COST                            
         MVC   PBDCOSTY(1),SAVFLD+5    AND INDICATORS                           
         MVC   PBDCTYP(1),SAVFLD+6                                              
         MVC   PBDRLIND(1),SAVFLD+7                                             
         MVC   PBDCOSIN(1),SAVFLD+8                                             
*                                                                               
         OI    CHADTIND,X'40'    RATE CHANGE                                    
*                                                                               
         B     RECUP84                                                          
*                                                                               
RECUP63N DS    0H                   NEWSPAPER RATE EDIT                         
         GOTO1 =A(EDTRTN),DMCB,(RC),(RA),(R6),RR=RELO01                         
         B     RECUP63E      REST SAME AS NON-NEWS                              
*                                                                               
*                                                                               
RECUP63X DC    H'0'                 INVALID DATASW                              
*                                                                               
RECUP84  DS    0H                                                               
         GOTO1 =A(UPREC),DMCB,(RC),(RA),RR=RELO01                               
*                                                                               
RECUP85  BAS   RE,PUTREC                                                        
RECUP90  LA    R6,LDISP(R6)                                                     
         LA    R5,LDISP(R5)                                                     
         LH    R1,COUNT                                                         
*                                                                               
         CLI   PUBNOPT,C'Y'           PUB NAME DISPLAY OPTION                   
         BNE   RECUP95                                                          
         LA    R6,LDISP(R6)           MUST BUMP R6                              
         LA    R5,LDISP(R5)                                                     
         AHI   R1,-1                                                            
         LTR   R1,R1                                                            
         BNP   RECUPEX                MUST BE DONE                              
RECUP95  LA    R7,4(R7)                                                         
         AHI   R1,-1                                                            
         STH   R1,COUNT                                                         
         LTR   R1,R1                                                            
         BNP   RECUPEX                                                          
         B     RECLOOP                                                          
*                                                                               
RECUPEX  NI    DMINBTS,X'7F'          SET OFF READ FOR UPDATE                   
*                                                                               
         XMOD1 1                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
RNEXTEL  DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    RNEXTELX                                                         
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     RNEXTEL                                                          
*                                                                               
RNEXTELX LTR   RE,RE                                                            
         BR    RE                  SET CONDITION CODE NE                        
         EJECT                                                                  
*                                                                               
CLEAREND NTR1                                                                   
         SR    R2,R2                                                            
         IC    R2,REC+25            CLEAR END OF REC                            
         SLL   R2,8                                                             
         IC    R2,REC+26                                                        
         SR    RE,RE                                                            
         LA    RE,REC                                                           
         AR    RE,R2                                                            
         LA    RF,REC+999                                                       
         AHI   RF,2000                                                          
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R9                                                               
*                                                                               
         EJECT                                                                  
* SUBROUTINE TO EDIT MAGAZINE RATES                                             
*                                                                               
EDTRTM   CSECT                                                                  
         NMOD1 0,EDTRTM                                                         
*                             *** DON'T TOUCH R8 IN ANY CSECT ***               
*                             *** IT IS NEEDED TO ACCESS ROUTINES               
*                             *** IN THE MAIN PROGRAM                           
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T418FFD,RA                                                       
         L     R2,8(R1)           ADDRESS OF FIELD HEADER                       
         SR    R3,R3              CLEAR ERROR REGISTER                          
*                                                                               
         MVI   PBDCTYP,0                                                        
         MVI   PBDCOSIN,C' '                                                    
         NI    PBDRLIND,X'F7'      SET OFF FROZEN BIT                           
         CLI   5(R2),0                                                          
         BE    EDTRE1              NO INPUT - DISALLOW                          
         SR    R7,R7                                                            
         IC    R7,5(R2)            GET LENGTH                                   
         LA    R6,8(R2)            SET ADDRESS                                  
         TM    4(R2),X'04'         TEST VALID ALPHA                             
         BNZ   EDTR7               (FREE, ETC)                                  
*                                                                               
EDTR3    DS    0H                                                               
         CLI   0(R6),C'S'          GROSS=NET                                    
         BNE   EDTR3C                                                           
******** ZAP   PBDACP,=P'1'           TO PREVENT AC LOOK-UP                     
*                                     WILL BE RESET TO P'0'                     
         MVI   PBDCOSIN,C'S'                                                    
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
*                                                                               
**NEW 6/3/88                                                                    
EDTR3C   DS    0H                                                               
**                                                                              
         CLC   PBUYKAGY,=C'SJ'      ONLY FOR SJ                                 
         BNE   EDTR4                                                            
**                                 SKIP COMMISSION LOGIC FOR NOW                
**                                                                              
         CLI   0(R6),C'C'          COMMISSION ONLY                              
         BNE   EDTR4                                                            
         CLI   PBDCOSIN,C' '                                                    
         BNE   EDTRE2                                                           
         MVI   PBDCOSIN,C'C'                                                    
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
*                                                                               
EDTR4    DS    0H                                                               
         CLI   0(R6),C'N'          NET TO BE GROSSED UP                         
         BNE   EDTR5                                                            
         MVI   PBDCTYP,C'N'                                                     
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
         B     EDTR3                                                            
*                                                                               
EDTR5    DS    0H                                                               
EDTR6    DS    0H                                                               
         CLI   0(R6),C'F'          FROZEN RATE  (* IN BUY)                      
         BNE   EDTR7                                                            
         OI    PBDRLIND,X'08'                                                   
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
         B     EDTR3                                                            
*                                                                               
EDTR7    DS    0H                                                               
*                                                                               
*        CHECK BELOW NOT NEEDED SINCE PBDCOSIN WILL NOT BE                      
*        ALLOWED TO CHANGE                                                      
*                                                                               
******   CLI   SVESPROF+28,C'C'      SEE IF 'C' RATE ESTIMATE                   
******   BNE   EDTR8                                                            
******   CLI   PBDCOSIN,C'C'          'C' INPUT                                 
******   BE    EDTR10                                                           
******   CLI   PBDCOSIN,C' '                                                    
******   BNE   EDTRE2                                                           
******   MVI   PBDCOSIN,C'C'         NOTHING INPUT - SET TO 'C'                 
         B     EDTR10                                                           
*                                                                               
EDTR8    CLI   PBDCOSIN,C'C'        'C' RATE ONLY FOR 'C' RATE EST              
         BE    EDTRE2                                                           
**NEW 3/30/89                                                                   
EDTR10   GOTO1 VCASHVAL,DMCB,(R6),(R7)                                          
         CLI   0(R1),X'FF'                                                      
         BE    EDTRE2                                                           
         L     R0,4(R1)                                                         
         C     R0,=F'999999999'     MAX IS NOW 9,999,999.99                     
         BH    EDTRE2                                                           
*                                                                               
         CVD   R0,DUB                                                           
         ZAP   PBDCOS,DUB                                                       
******** LTR   R0,R0                                                            
******** BNZ   *+10                                                             
******** ZAP   PBDCOS,=P'1'        LIE ABOUT RATE FOR NOW                       
*                                                                               
         B     EDTRXIT                                                          
*                                                                               
EDTRE1   LA    R3,MISSERR                                                       
         B     EDTRXITX       MISSING INPUT                                     
*                                                                               
EDTRE2   LA    R3,FLDINV                                                        
         B     EDTRXITX                                                         
*                                                                               
EDTRXIT  DS    0H                                                               
         LR    R6,R2     RESET R6 TO FIELD HEADER                               
         CLC   PBDCOSIN,SAVFLD+8    PBDCOSIN CAN'T CHANGE                       
         BNE   FLDERR                                                           
*                                                                               
*        SEE IF NET RATE INPUT - MUST BE GROSSED-UP                             
*                                                                               
         CLI   PBDCTYP,C'N'        TEST NET INPUT                               
         BNE   EDTRXIT6                                                         
*                                  GROSS UP COST                                
         CP    PBDACP,=P'0'        UNLESS THERE IS NO AC                        
         BE    EDTRXIT6            THEN LEAVE IT ALONE                          
         CP    PBDACP,=P'1'        OR AC WAS OVERRIDDEN TO 0                    
         BE    EDTRXIT6            THEN LEAVE IT ALONE                          
*                                                                               
         ZAP   DUB,PBDCOS                                                       
         CVB   R1,DUB                                                           
         M     R0,=F'100000'                                                    
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'       =NET PCT                                     
         LCR   RF,RF                                                            
         BNP   EDTRXIT6                                                         
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         CVD   R1,DUB                                                           
         ZAP   PBDCOS,DUB                                                       
*                                                                               
EDTRXIT6 DS    0H                                                               
*                                                                               
EDTRXITX XIT1  REGS=(R3)                                                        
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
SCRNDIS  CSECT                                                                  
         NMOD1 0,SCRNDIS                                                        
*                             *** DON'T TOUCH R8 IN ANY CSECT ***               
*                             *** IT IS NEEDED TO ACCESS ROUTINES               
*                             *** IN THE MAIN PROGRAM                           
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T418FFD,RA                                                       
*                                                                               
**RATES**                                                                       
         LA    R9,SCRNDIS+4095                                                  
         LA    R9,1(R9)                                                         
         USING SCRNDIS+4096,R9   NOTE USE OF R9 AS SECOND BASE REG              
**RATES**                                                                       
*                                                                               
         LA    R7,SVDISC           SAVED DISC ADDRESSES                         
         MVC   COUNT,=H'16'        ONLY 16 PER SCREEN                           
         LA    R5,MBCOT01H         UNCHANGABLE DATA INITIALIZATION              
         LA    R6,MBCDT01H         CHANGABLE DATA INITIALIZATION                
*                                                                               
SCRLOOP  OC    0(4,R7),0(R7)       CHECK FOR A DISC ADDRESS                     
         BZ    SEXIT1              IF NONE EXIT.                                
*                                                                               
         NI    1(R6),X'DF'         UNPROTECT CHANGEABLE FIELD.                  
         OI    4(R6),X'20'         IF NO CHANGE REC WON'T BE UPDATED            
         OI    6(R6),X'80'         TRANSMIT STATUS CHANGE.                      
         STM   R5,R6,MYDUB2        SAVE R5 AND R6                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+27(4),0(R7)     GETREC ASSUMES DISC ADDRESS IN               
*                                                                               
         BAS   RE,GETREC           KEY + 27.                                    
         CLI   PUBNOPT,C'Y'        SEE IF DISPLAYING PUB NAME                   
         BNE   SCRND4                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),PBUYKMED                                                  
         MVC   KEY+1(6),PBUYKPUB                                                
         MVC   KEY+7(2),PBUYKAGY                                                
         MVI   KEY+9,X'81'                                                      
SCRND2   BAS   RE,HIGHPUB                                                       
         CLC   KEY(10),KEYSAVE                                                  
         BE    SCRND3                                                           
         CLI   SVAPROF+16,C'0'                                                  
         BNE   *+6                                                              
         DC    H'0'                MUST FIND PUB                                
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+7(2),=C'ZZ'    NOW TRY FOR ZZ                                
         B     SCRND2                                                           
*                                                                               
SCRND3   DS    0H                                                               
         BAS   RE,GETPUB                                                        
         GOTO1 =V(PUBFLOAT),DMCB,APUBIO,LDISP+12(R5),RR=RELO01                  
*                                                                               
SCRND4   LA    R2,PBUYREC+33                                                    
         CLI   0(R2),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
SCRND5   MVC   8(3,R5),PBUYKPRD                                                 
         ZIC   R4,SVAPROF+12                   DISPLAY PRODUCT NUMBER           
         GOTO1 =V(PUBEDIT),DMCB,((R4),PBUYKPUB),12(R5),RR=RELO01                
*                                                                               
SCRND5D  MVC   HALF,PBUYKEST                      ESTIMATE NUMBER               
         SR    R4,R4                                                            
         LH    R4,HALF                                                          
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         ZAP   FULL,DUB                                                         
         EDIT  (P4,FULL),(3,29(R5)),0,FILL=0                                    
                                                                                
         CLI   PBDBFD,C'T'           SEE IF TEST BUY                            
         BNE   *+8                                                              
         MVI   33(R5),C'T'                                                      
                                                                                
         CLI   DATASW,C'X'    SFH (SPECIAL FINANCIAL HANDLING) BUY ?            
         BNE   SCRND5X         NO - BRANCH AROUND (P)AID AND (B)ILLED           
*                              LOGIC BELOW - ONLY FOR SFH AT PRESENT            
*                                                                               
         LA    R4,32(R5)           POINT R4 TO SPACE AFTER ESTIMATE             
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'      IF 25 (PAY) ELEMENT WITH DATE EXISTS,          
SCRND5DL BAS   RE,NEXTEL                                                        
         BNE   SCRND5K                                                          
         OC    2(3,R2),2(R2)                                                    
         BZ    SCRND5DL                                                         
         MVI   0(R4),C'P'             THEN DISPLAY P FOR PAID                   
         LA    R4,1(R4)          AND MOVE OVER FOR POSSIBLE "B" BELOW           
*                                             AND/OR                            
SCRND5K  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'26'     IF 26 (BILL) ELEMENT WITH DATE EXISTS,          
SCRND5KL BAS   RE,NEXTEL                                                        
         BNE   SCRND5X                                                          
         OC    5(3,R2),5(R2)                                                    
         BZ    SCRND5KL                                                         
         TM    10(R2),X'C0'             IGNORE REVERSALS AND REVERSED           
         BNZ   SCRND5KL                                                         
         MVI   0(R4),C'B'             THEN DISPLAY B FOR BILLED                 
*                                                                               
SCRND5X  CLI   DATESW,C'O'                                                      
         BNE   SCRND10                                                          
         OC    PBDSDATE,PBDSDATE                                                
         BZ    SCRND20                                                          
         GOTO1 VDATCON,DMCB,(3,PBDSDATE),(5,34(R5))   ON-SALE                   
         B     SCRND20                                                          
*                                                                               
SCRND10  CLI   DATESW,C'B'                                                      
         BNE   SCRND12                                                          
         GOTO1 VDATCON,DMCB,(3,PBDBDATE),(5,34(R5))   BILLABLE                  
         B     SCRND20                                                          
*                                                                               
SCRND12  CLI   DATESW,C'P'                                                      
         BNE   SCRND14                                                          
         GOTO1 VDATCON,DMCB,(3,PBDPDATE),(5,34(R5))   PAYABLE                   
         B     SCRND20                                                          
*                                                                               
SCRND14  CLI   DATESW,C'C'                                                      
         BNE   SCRND15                                                          
         OC    PBDCDATE,PBDCDATE                                                
         BZ    SCRND20                                                          
         GOTO1 VDATCON,DMCB,(3,PBDCDATE),(5,34(R5))   CLOSING                   
         B     SCRND20                                                          
*                                                                               
*                                                                               
SCRND15  CLI   DATESW,C'M'        MATERIALS CLOSING                             
         BNE   SCRND15S                                                         
         OC    PBDMDATE,PBDMDATE                                                
         BZ    SCRND20                                                          
         GOTO1 VDATCON,DMCB,(3,PBDMDATE),(5,34(R5))   CLOSING                   
         B     SCRND20                                                          
*                                                                               
SCRND15S CLI   DATESW,C'S'        SHIP DATE                                     
         BNE   SCRND16            THEN USE INSERTION DATE                       
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'86'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   SCRND20                                                          
         GOTO1 VDATCON,DMCB,(3,2(R2)),(5,34(R5))                                
         B     SCRND20                                                          
*                                                                               
SCRND16  GOTO1 VDATCON,DMCB,(3,PBUYKDAT),(5,34(R5))   INSERTION DATE            
         CLI   PBUYKLIN,1                                                       
         BNH   SCRND20                                                          
         MVI   42(R5),C'-'                                                      
         ZIC   R0,PBUYKEY+24           SEE IF OVER 100                          
         CVD   R0,DUB                                                           
         CP    DUB,=P'100'                                                      
         BL    SCRND16C                                                         
         DP    DUB,=P'10'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  44(1,R5),DUB+7(1)                                                
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   R1,DUB                                                           
         LA    R1,ALPHATAB(R1)                                                  
         MVC   43(1,R5),0(R1)                                                   
         B     SCRND20                                                          
*                                                                               
SCRND16C OI    DUB+7,X'0F'                                                      
         UNPK  43(2,R5),DUB                                                     
*                                                     IS DEFAULT.               
SCRND20  DS    0H                                                               
*                                                                               
*              GO TO GETINS BEFORE PPBYOUT                                      
*                                                                               
         LA    R4,PBUYREC+7                                                     
*****    GOTO1 =V(GETINS),DMCB,PBUYREC,GROSS,(R4),RR=RELO01                     
*****    GOTO1 VGETINS,DMCB,PBUYREC,GROSS,(R4)                                  
         GOTO1 VGETINS,DMCB,PBUYREC,(COST2SW,GROSS),(R4)                        
*                                                                               
SCRND20B LA    R4,BUYOUTA                                                       
         USING PPBYOUTD,R4                                                      
*                                                                               
         MVC   WORK(17),PBDSPACE    SAVE REAL PBDSPACE                          
         CLI   MBCMDIA,C'O'         SEE IF OUTDOOR                              
         BNE   SCRND20D                                                         
         CLI   OUT2OPT,C'Y'         SEE IF SHOWING OUTSPC2                      
         BNE   SCRND20D                                                         
         MVC   PBDSPACE,=CL67' '     SO PPBYOUT WILL RETURN OUTSPC2             
*                                                                               
SCRND20D GOTO1 VPPBYOUT,DMCB,BUYOUTA                                            
*                                                                               
         MVC   PBDSPACE,WORK          RESTORE REAL PBDSPACE                     
*                                                                               
         CLI   MBCMDIA,C'N'                                                     
         BNE   SCRND20F                                                         
         CLI   PBYOSPC,C' '                                                     
         BH    SCRND20F                                                         
         MVC   46(7,R5),PBYOUNTS                                                
         B     SCRND22                                                          
*                                                                               
SCRND20F DS    0H                                                               
*                                                                               
SCRND20G MVC   46(17,R5),PBYOSPC                                                
*                                                                               
*                                                                               
*                                                                               
SCRND22  DS    0H                  DISPLAYING LAST I/O INFO                     
*                                                                               
         CLI   SVIOSW,C'Y'         SEE IF DISPLAYING LASTI/O INFO               
         BNE   SCRND25             NO, SO GO AHEAD AND DISPLY COST              
*                                                                               
         XC    TEMP(11),TEMP       USED TEMPORARILY TO STORE ELEM               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'70'        INSERTION ORDER ELEM CODE                    
SCRND22H BAS   RE,NEXTEL                                                        
         BNE   SCRND22M            NO MORE ELEMS                                
         OC    2(3,R2),2(R2)                                                    
         BZ    SCRND22H            NO INSERTION DATE, TRY NEXT X'70'            
         CLC   TEMP+2(3),2(R2)                                                  
         BH    SCRND22H            NOT LAST INSERTION DATE, NEXT X'70'          
         MVC   TEMP(11),0(R2)                                                   
         B     SCRND22H            CHECK NEXT X'70' UNTIL NO MORE               
*                                                                               
SCRND22M DS    0H                                                               
*                                                                               
         OC    TEMP(11),TEMP                                                    
         BZ    SCRND22V            NO INSERTION DATE, GO DISPLAY "NONE"         
*                                                                               
* WIPE OUT 6 SPACES FROM SPACE COLUMN, ONLY 11 OF 17 CHARS WILL BE              
* DISPLAYED.  THIS IS DONE TO SEPARATE SPACE AND LASTIO COLUMNS                 
*                                                                               
         MVC   57(8,R5),=CL67' '   REUSE LITERALS                               
*                                                                               
         MVC   59(1,R5),SVMED      GET MEDIA                                    
*                                                                               
         GOTO1 VDATCON,DMCB,(3,TEMP+2),(0,60(R5))                               
         MVI   60(R5),C'-'                                                      
         MVI   66(R5),C'-'         DONE WITH I/O DATE PART (YMMDD)              
         MVC   HALF,TEMP+5                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  67(4,R5),DUB                                                     
         MVI   71(R5),C' '         DONE WITH RENFERENCE NUMBER PART             
         CLI   TEMP+10,C'N'                                                     
         BNE   *+10                                                             
         MVC   72(3,R5),=C'NEW'                                                 
         CLI   TEMP+10,C'C'                                                     
         BNE   *+10                                                             
         MVC   72(3,R5),=C'CHA'                                                 
         CLI   TEMP+10,C'D'                                                     
         BNE   *+10                                                             
         MVC   72(3,R5),=C'CAN'                                                 
         B     *+10                DONE WITH INSERTION ORDER TYPE               
*                                                                               
SCRND22V MVC   71(4,R5),=C'NONE'   SCR POSITION 64 (71-8=63+1)                  
*                                                                               
         NI    DMINBTS,X'FF'-X'08' TURN OFF DELETED RECS BIT                    
*                                                                               
         LA    RE,PBUYREC                                                       
         TM    27(RE),X'80'                                                     
         BZ    SCRND38             REC IS NOT DELETED                           
*                                                                               
         MVI   33(R5),C'D'         INDICATING DELETED (FRONT OF INS DT)         
*                                                                               
         LR    RE,R5               COPY CURRENT FIELD POINTER                   
         SR    R0,R0                                                            
         IC    R0,0(R5)                                                         
         AR    RE,R0               BUMP TO NEXT FIELD                           
         OI    1(RE),X'20'         PROTECT FIELD TO DISALLOW CHANGES            
*                                                                               
         B     SCRND38             GO DO MBCDT01 DISPLAYS                       
*                                                                               
SCRND25  DS    0H                                                               
*                                                                               
         CLI   PBDCTYP,C'N'            SEE IF BOUGTH WITH A NET RATE            
         BNE   *+8                                                              
         MVI   64(R5),C'N'             DISPLAY 'N' AFTER SPACE                  
*                                                                               
         CLI   DOLSW,0                 DON'T DISPLAY COST                       
         BE    SCRND38                                                          
*                                                                               
         CLI   DOLSW,C'2'              GROSS - CASH DISC. ORDERED               
         BNE   SCRND30                                                          
         EDIT  BLABLE,(10,65(R5)),2,FLOAT=-                                     
         B     SCRND38                                                          
*                                                                               
SCRND30  CLI   DOLSW,C'3'              NET ORDERED                              
         BNE   SCRND32                                                          
SCRND31  L     R4,GROSS                CALCULATE:                               
         CVD   R4,MYDUB                NET=GROSS - AGENCY COMM.                 
         L     R4,AGYCOM                                                        
         CVD   R4,DUB                                                           
         SP    MYDUB,DUB                                                        
         EDIT  (P8,MYDUB),(10,65(R5)),2,FLOAT=-                                 
         B     SCRND38                                                          
*                                                                               
SCRND32  CLI   DOLSW,C'4'              NET - CASH DISC ORDERED                  
         BNE   SCRND34                                                          
         EDIT  PYABLE,(10,65(R5)),2,FLOAT=-                                     
         B     SCRND38                                                          
*                                                                               
SCRND34  CLI   DOLSW,C'5'              CASH DISCOUNT                            
         BNE   SCRND36                                                          
         EDIT  CSHDSC,(10,65(R5)),2,FLOAT=-                                     
         B     SCRND38                                                          
*                                                                               
SCRND36  DS    0H                                                               
*                                                                               
         CLI   DOLSW,C'1'          SEE IF IS GROSS                              
         BE    SCRND37                                                          
         CLI   DOLSW,C'0'          SEE IF NO DOLLAR TYPE IS ENTERED             
         BE    *+6                                                              
         DC    H'0'                SOMETHING IS WRONG WITH DOLSW                
*                                                                               
         CLI   MBPROF+3,C'Y'                                                    
         BNE   SCRND37             DO GROSS                                     
         CLI   PBDCTYP,C'N'                                                     
         BE    SCRND31             DO NET                                       
*                                                                               
SCRND37  EDIT  GROSS,(10,65(R5)),2,FLOAT=-                                      
*                                                                               
SCRND38  DS    0H                  BEGINNING OF MBCDT01                         
*                                                                               
         CLI   DATASW,C'L'             NV LETTER STATUS                         
*****    BNE   SCRND40                                                          
         BNE   SCRND39                                                          
         MVC   8(3,R6),=C'YES'                                                  
         TM    PBDLETR,X'01'                                                    
         BZ    SCRND38X                                                         
         MVC   8(3,R6),=C'NO '                                                  
SCRND38X OI    4(R6),X'20'           SET PREVIOUSLY VALIDATED                   
         B     SCRND70                                                          
*                                                                               
SCRND39  CLI   DATASW,C'X'         SPECIAL FINANCIAL HANDLING (SFH)             
         BNE   SCRND40                                                          
         OI    4(R6),X'20'           SET PREVIOUSLY VALIDATED                   
         MVC   8(4,R6),=C'HOLD'                                                 
         TM    PBDSTAT,X'08'       HELD SFH ?                                   
         BNZ   SCRND70             YES - CAN BE CHANGED TO RELEASE              
         MVC   8(4,R6),=C'REL '                                                 
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'70'        IF A 70 ELEMENT WITH DATE                    
SCRND39C BAS   RE,NEXTEL           EXISTS, CAN'T CHANGE FROM                    
         BNE   SCRND70                 "REL" TO "HOLD"                          
         OC    2(3,R2),2(R2)       ANY DATE ?                                   
         BNZ   SCRND67P            YES - GO PROTECT FIELD                       
         B     SCRND39C            NO - LOOK FOR ANOTHER I/O ELEM               
*                                                                               
SCRND40  CLI   DATASW,C'A'             JOBCODE                                  
         BNE   SCRND45                                                          
         MVC   8(6,R6),PBDJOB                                                   
         OI    4(R6),X'20'             VALIDATED PREVIOUSLY.                    
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'35'            IF A 35 ELEMENT EXISTS, CAN'T            
         BAS   RE,NEXTEL               ADD, DEL, OR CHANGE ADCODE.              
         BNE   SCRND70                                                          
         OI    1(R6),X'20'             CHANGE FIELD TO PROTECTED.               
         OI    6(R6),X'80'             TRANSMIT STATUS CHANGE.                  
         B     SCRND70                                                          
*                                                                               
SCRND45  CLI   DATASW,C'D'             DLC                                      
         BNE   SCRND46                                                          
         OI    4(R6),X'20'             VALIDATED PREVIOUSLY.                    
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'81'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   SCRND70                                                          
         EDIT  (P6,2(R2)),(6,8(R6)),0,ALIGN=LEFT                                
         B     SCRND70                                                          
*                                                                               
SCRND46  CLI   DATASW,C'E'             T/S STATUS                               
         BNE   SCRND47                                                          
         OI    4(R6),X'20'             VALIDATED PREVIOUSLY.                    
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'95'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   SCRND70                                                          
         USING PTSHTELD,R2                                                      
         MVC   8(1,R6),PTSHSTAT                                                 
         B     SCRND70                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
SCRND47  CLI   DATASW,C'T'             RPT                                      
         BNE   SCRND47C                                                         
         OI    4(R6),X'20'             VALIDATED PREVIOUSLY                     
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'85'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   SCRND70                                                          
         EDIT  (P3,2(R2)),(3,8(R6)),0,ALIGN=LEFT,ZERO=NOBLANK                   
         B     SCRND70                                                          
*                                                                               
SCRND47C CLI   DATASW,C'V'             PAGE VIEWS                               
         BNE   SCRND47F                                                         
         OI    4(R6),X'20'             VALIDATED PREVIOUSLY                     
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'87'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   SCRND70                                                          
         EDIT  (P5,2(R2)),(11,8(R6)),0,ALIGN=LEFT,ZERO=NOBLANK,        X        
               COMMAS=YES                                                       
         B     SCRND70                                                          
*                                                                               
SCRND47F CLI   DATASW,C'K'             CLICK THRUS                              
         BNE   SCRND47G                                                         
         OI    4(R6),X'20'             VALIDATED PREVIOUSLY                     
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'88'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   SCRND70                                                          
         EDIT  (P5,2(R2)),(11,8(R6)),0,ALIGN=LEFT,ZERO=NOBLANK,        X        
               COMMAS=YES                                                       
         B     SCRND70                                                          
*                                                                               
SCRND47G CLI   DATASW,C'I'             IMPRESSION                               
         BNE   SCRND48                                                          
         OI    4(R6),X'20'             VALIDATED PREVIOUSLY                     
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   SCRND70                                                          
         EDIT  (P5,2(R2)),(11,8(R6)),0,ALIGN=LEFT,ZERO=NOBLANK,        X        
               COMMAS=YES                                                       
         B     SCRND70                                                          
*                                                                               
SCRND48  CLI   DATASW,C'F'             FSI                                      
         BNE   SCRND49                                                          
         OI    4(R6),X'20'             VALIDATED PREVIOUSLY.                    
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'82'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   SCRND70                                                          
         USING PBFSIELD,R2                                                      
         EDIT  PBFSI,(6,8(R6)),0,ALIGN=LEFT,ZERO=NOBLANK                        
         B     SCRND70                                                          
         DROP  R2                                                               
*                                                                               
SCRND49  CLI   DATASW,C'R'             REFERENCE NUMBER                         
         BNE   SCRND50                                                          
         OI    4(R6),X'20'             VALIDATED PREVIOUSLY.                    
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'83'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   SCRND70                                                          
         USING PBREFELD,R2                                                      
         MVC   8(L'PBREFNO,R6),PBREFNO                                          
         B     SCRND70                                                          
         DROP  R2                                                               
*                                                                               
SCRND50  CLI   DATASW,C'1'             ON-SALE DATE                             
         BNE   SCRND52                                                          
         OC    PBDSDATE,PBDSDATE                                                
         BZ    SCRND70                                                          
         GOTO1 VDATCON,DMCB,(3,PBDSDATE),(5,8(R6))                              
         B     SCRND70                                                          
*                                                                               
SCRND52  CLI   DATASW,C'2'             BILLABLE DATE                            
         BNE   SCRND54                                                          
         GOTO1 VDATCON,DMCB,(3,PBDBDATE),(5,8(R6))                              
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'26'            IF A 26 ELEMENT WITH A                   
SCRND52B BAS   RE,NEXTEL               DATE EXISTS, CAN'T CHANGE                
         BNE   SCRND52D                BILLABLE DATE.                           
         OC    5(3,R2),5(R2)                                                    
         BZ    SCRND52B                                                         
         TM    10(R2),X'C0'             IGNORE REVERSALS AND REVERSED           
         BNZ   SCRND52B                                                         
         OI    1(R6),X'20'             CHANGE FIELD TO PROTECTED.               
         OI    6(R6),X'80'             TRANSMIT STATUS CHANGE.                  
         B     SCRND70                                                          
*                                                                               
SCRND52D DS    0H                                                               
         B     SCRND70                                                          
*                                                                               
SCRND54  CLI   DATASW,C'3'             PAYING DATE                              
         BNE   SCRND56                                                          
         GOTO1 VDATCON,DMCB,(3,PBDPDATE),(5,8(R6))                              
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'            IF 25 ELEMENT WITH DATE EXISTS,          
SCRND54B BAS   RE,NEXTEL               CAN'T CHANGE PAYING DATE.                
         BNE   SCRND70                                                          
         OC    2(3,R2),2(R2)                                                    
         BZ    SCRND54B                                                         
         OI    1(R6),X'20'             CHANGE FIELD TO PROTECTED.               
         OI    6(R6),X'80'             TRANSMIT STATUS CHANGE.                  
         B     SCRND70                                                          
*                                                                               
SCRND56  CLI   DATASW,C'4'             CLOSING DATE                             
         BNE   SCRND58                                                          
         OC    PBDCDATE,PBDCDATE                                                
         BZ    SCRND70                                                          
         GOTO1 VDATCON,DMCB,(3,PBDCDATE),(5,8(R6))                              
         B     SCRND70                                                          
*                                                                               
*                                                                               
SCRND58  CLI   DATASW,C'5'             MATERIALS                                
         BNE   SCRND60                                                          
         OC    PBDMDATE,PBDMDATE                                                
         BZ    SCRND70                                                          
         GOTO1 VDATCON,DMCB,(3,PBDMDATE),(5,8(R6))                              
         B     SCRND70                                                          
*                                                                               
SCRND60  CLI   DATASW,C'7'                                                      
         BNE   SCRND65                                                          
         EDIT  (P2,PBDCD),(8,8(R6)),1,ALIGN=LEFT   EDIT CASH DISCOUNT           
         CLI   PBDCOSIN,C'C'           NO CD OVERRIDE IF                        
         BNE   SCRND60B                PBDCOSIN = 'C' ('C' RATE BUY)            
         OI    1(R6),X'20'             CHANGE FIELD TO PROTECTED.               
         OI    6(R6),X'80'             TRANSMIT STATUS CHANGE.                  
         B     SCRND70                                                          
*                                                                               
SCRND60B DS    0H                                                               
         TM    PBDSTAT,X'40'          SEE IF MATCHED                            
         BNO   SCRND60G                                                         
*                                                                               
         LA    R2,PBUYREC+33          SEE IF PAID                               
         MVI   ELCODE,X'25'                                                     
SCRND60C BAS   RE,NEXTEL                                                        
         BNE   SCRND60D                                                         
         OC    2(3,R2),2(R2)                                                    
         BNZ   SCRND60G                YES-THEN ALLOW CD CHANGE                 
         B     SCRND60C                                                         
*                                                                               
SCRND60D OI    1(R6),X'20'             CHANGE FIELD TO PROTECTED.               
         OI    6(R6),X'80'             TRANSMIT STATUS CHANGE.                  
         B     SCRND70                                                          
*                                                                               
SCRND60G OI    4(R6),X'20'                       VALIDATED PREVIOUSLY           
         B     SCRND70                                                          
*                                                                               
SCRND65  CLI   DATASW,C'8'                                                      
         BNE   SCRND67                                                          
         ZAP   MYPK4,PBDACP                                                     
         CP    MYPK4,=P'-1'                                                     
         BNE   *+10                                                             
         ZAP   MYPK4,=P'100000'                                                 
         EDIT  (P4,MYPK4),(8,8(R6)),3,ALIGN=LEFT    EDIT AGENCY COMM            
         CLI   PBDCOSIN,C' '           NO AC OVERRIDE IF                        
         BE    SCRND65B                PBDCOSIN NOT EQU SPACE                   
         OI    1(R6),X'20'             CHANGE FIELD TO PROTECTED.               
         OI    6(R6),X'80'             TRANSMIT STATUS CHANGE.                  
         B     SCRND70                                                          
*                                                                               
SCRND65B TM    PBDSTAT,X'40'          SEE IF MATCHED                            
         BNO   SCRND65G                                                         
*                                                                               
         LA    R2,PBUYREC+33          SEE IF PAID                               
         MVI   ELCODE,X'25'                                                     
SCRND65C BAS   RE,NEXTEL                                                        
         BNE   SCRND65D                                                         
         OC    2(3,R2),2(R2)                                                    
         BNZ   SCRND65G                YES-THEN ALLOW AC CHANGE                 
         B     SCRND65C                                                         
*                                                                               
SCRND65D OI    1(R6),X'20'             CHANGE FIELD TO PROTECTED.               
         OI    6(R6),X'80'             TRANSMIT STATUS CHANGE.                  
         B     SCRND70                                                          
*                                                                               
SCRND65G OI    4(R6),X'20'                       VALIDATED PREVIOUSLY           
         B     SCRND70                                                          
*                                                                               
SCRND67  CLI   DATASW,C'S'            STATUS                                    
         BNE   SCRND68                                                          
         CLI   PBDBFD,C'T'                                                      
         BNE   SCRND67B                                                         
         MVI   8(R6),C'T'                                                       
         TM    SVCLPROF+30,X'02'   TEST SAVED PCLTSTAT (FROZEN CLIENT?)         
         BNO   SCRND67W            NOT FROZEN - GO SEE IF WSJ BUY               
         MVC   10(6,R6),=C'FROZEN'                                              
         B     SCRND67P            GO PROTECT FIELD                             
*                                                                               
SCRND67B MVI   8(R6),C'L'                                                       
         B     SCRND67P       FOR NOW ALWAYS PROTECT LIVE BUYS                  
*****                                                                           
*****                         BELOW CODE WILL PROTECT STATUS IF                 
*****                         BUY IS BILLED,PAID,OR I/O'ED                      
*****                                                                           
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'70'                                                     
SCRND67C BAS   RE,NEXTEL                                                        
         BNE   SCRND67D                                                         
         OC    2(3,R2),2(R2)                                                    
         BNZ   SCRND67P                                                         
         B     SCRND67C                                                         
*                                                                               
SCRND67D LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'                                                     
SCRND67E BAS   RE,NEXTEL                                                        
         BNE   SCRND67F                                                         
         OC    2(3,R2),2(R2)                                                    
         BNZ   SCRND67P                                                         
         B     SCRND67E                                                         
*                                                                               
SCRND67F LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'26'                                                     
SCRND67G BAS   RE,NEXTEL                                                        
         BNE   SCRND67W                                                         
         OC    5(3,R2),5(R2)                                                    
         BNZ   SCRND67P                                                         
         B     SCRND67G                                                         
*                                                                               
SCRND67W LA    R2,PBUYREC+33         SEE IF WSJ BUY                             
         MVI   ELCODE,X'35'                                                     
         BAS   RE,NEXTEL                                                        
         BE    SCRND67P                                                         
*                                                                               
         CLI   PBDBFD,C'T'          SEE IF TEST BUY                             
         BNE   SCRND70                                                          
*                                 PROTECT IF ON A TEST ESTIMATE                 
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYREC                                                  
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+10(2),PBUYKEST                                               
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'              FATAL ERROR ESTIMATE NOT ON FILE               
         GOTO1 GETREC                                                           
         CLI   PESTELEM+1,X'D8'     SEE IF OLD ESTIMATE                         
         BL    SCRND70              'SHORT' ELEM - CAN'T BE TEST                
         TM    PESTTEST,X'80'                                                   
         BNZ   SCRND67P          PROTECT                                        
         B     SCRND70                                                          
*                                                                               
SCRND67P DS    0H                                                               
*                     PROTECT FIELD IF BILLED,PAID,INS ORDER                    
*                     OR WSJ BUY                                                
         OI    1(R6),X'20'             CHANGE FIELD TO PROTECTED.               
         OI    6(R6),X'80'             TRANSMIT STATUS CHANGE.                  
         B     SCRND70                                                          
*                                                                               
SCRND68  CLI   DATASW,C'C'            CONTRACT UNITS                            
         BNE   SCRND69                                                          
         MVI   8(R6),C'0'                                                       
         CLC   PBDCU,=X'000001'      MEANS ZERO                                 
         BE    SCRND68X                                                         
         XC    8(11,R6),8(R6)                                                   
         OC    PBDCU,PBDCU                                                      
         BZ    SCRND68X                                                         
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),PBDCU                                                  
         EDIT  (B4,FULL),(11,8(R6)),4,ALIGN=LEFT,DROP=3                         
         CH    R0,=H'11'                  MAX OUTPUT LENGHT                     
         BNH   SCRND68X                                                         
         MVC   8(11,R6),=C'TOO BIG    '                                         
         OI    1(R6),X'20'                       PROTECT                        
SCRND68X OI    4(R6),X'20'                       VALIDATED PREVIOUSLY           
         B     SCRND70                                                          
*                                                                               
SCRND69  DS    0H                                                               
         CLI   DATASW,C'P'             PLANNED COST                             
         BNE   SCRND69E                                                         
         XC    8(11,R6),8(R6)                                                   
         OC    PBDPLCOS,PBDPLCOS                                                
         BZ    SCRND69C                                                         
         MVC   FULL,PBDPLCOS                                                    
         EDIT  (B4,FULL),(11,8(R6)),2,ALIGN=LEFT                                
         CH    R0,=H'11'                  MAX OUTPUT LENGHT                     
         BNH   SCRND69C                                                         
         MVC   8(11,R6),=C'TOO BIG    '                                         
         OI    1(R6),X'20'                       PROTECT                        
SCRND69C OI    4(R6),X'20'                       VALIDATED PREVIOUSLY           
         B     SCRND70                                                          
*                                                                               
SCRND69E DS    0H                                                               
         CLI   DATASW,C'H'           SHIP DATE                                  
         BNE   SCRND69G                                                         
         OI    4(R6),X'20'             VALIDATED PREVIOUSLY.                    
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'86'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   SCRND70                                                          
         GOTO1 VDATCON,DMCB,(3,2(R2)),(5,8(R6))                                 
         B     SCRND70                                                          
*                                                                               
SCRND69G CLI   DATASW,C'B'         COST 2 FACTOR                                
         BNE   SCRND69H                                                         
         OI    4(R6),X'20'             VALIDATED PREVIOUSLY.                    
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'91'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   SCRND70                                                          
         EDIT  (P5,2(R2)),(8,8(R6)),6,ALIGN=LEFT      EDIT C2 FACTOR            
         B     SCRND70                                                          
*                                                                               
**RATES**                                                                       
SCRND69H DS    0H                  RATE DISPLAYS                                
         CLI   DATASW,C'$'                                                      
         BE    SCRND69I                                                         
         CLI   DATASW,C'M'         INCH RATE                                    
         BE    SCRND69I                                                         
         CLI   DATASW,C'N'         LINE RATE                                    
         BE    SCRND69I                                                         
         B     SCRN69XX                                                         
*                                                                               
SCRND69I CLI   PBUYKMED,C'N'       SEE IF NEWSPAPERS                            
         BNE   SCRND69K                                                         
         BAS   RE,FMTRTN           TO FORMAT NEWSPAPER RATE ROUTINE             
*                                  DISPLAY RETURNED IN X                        
SCRND69J DS    0H                                                               
         OI    4(R6),X'20'         VAILDATED PREVIOUSLY                         
*                                                                               
* RATE CHANGE IS NOT ALLOWED FOR FULLY PAID BUY                                 
*                                                                               
         CLI   BYPROF+7,C'R'       CKING PROFILE                                
         BE    SC69J3H                                                          
         CLI   BYPROF+7,C'B'                                                    
         BNE   SC69J3X                                                          
SC69J3H  DS    0H                                                               
         LA    R4,PBUYREC+7                                                     
         GOTO1 VGETINS,DMCB,PBUYREC,GROSS,(R4)                                  
*                                                                               
         CLC   PGROSS(12),GROSS    COMPARE UP TO CASH DISCOUNT                  
         BNE   SC69J3X                                                          
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'        PAY ELEM EXISTS?                             
         BAS   RE,NEXTEL                                                        
         BNE   SC69J3X             ALLOW RATE CHANGE                            
         OC    2(3,R2),2(R2)                                                    
         BNZ   SCRND69L            DATE EXIST, PROTECT DATA FIELD               
*                                                                               
*                                                                               
*                                                                               
SC69J3X  DS    0H                                                               
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'35'        IF A 35 (WSJ) ELEMENT EXISTS, CAN'T          
         BAS   RE,NEXTEL           CHANGE RATE                                  
         BE    SCRND69L            PROTECT                                      
*                                                                               
         TM    PBDSTAT,X'40'       MATCHED                                      
         BNO   SCRND70                                                          
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'                                                     
SC69J5   BAS   RE,NEXTEL                                                        
         BNE   SCRND69L                                                         
         OC    2(3,R2),2(R2)                                                    
         BNZ   SCRND70             MATCH AND PAID - ALLOW RATE CHANGE           
         B     SC69J5                                                           
*                                                                               
SCRND69K DS    0H                                                               
         BAS   RE,FMTRTM           TO FORMAT NON-NEWSPAPER RATES                
*                                  DISPLAY RETURNED IN X                        
         B     SCRND69J            GO CHK WSJ AND MATCH STATUS                  
*                                                                               
SCRND69L OI    1(R6),X'20'         PROTECT                                      
         OI    6(R6),X'80'         TRANSMIT                                     
         B     SCRND70                                                          
*                                                                               
**RATES**                                                                       
SCRN69XX DS    0H                  END OF RATE DISPLAY                          
*                                                                               
*                                                                               
*                                                                               
SCUPID   DS    0H                  SCREEN DISPLAY FOR UPID                      
         CLI   DATASW,C'U'                                                      
         BNE   SCRNDERR                                                         
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'90'        SEARCH FOR UPLOAD ELEM                       
         BAS   RE,NEXTEL                                                        
         BNE   SCUPI30             NO FOUND                                     
         USING PIUPEL,R2                                                        
         MVC   8(L'PIUPUSEQ,R6),PIUPUSEQ                                        
         B     SCUPIXX                                                          
         DROP  R2                                                               
*                                                                               
SCUPI30  MVC   8(4,R6),=C'NONE'    NO UPID IS FOUND                             
*                                                                               
SCUPIXX  OI    1(R6),X'20'         PROTECT                                      
         OI    6(R6),X'80'         TRANSMIT                                     
         B     SCRND70                                                          
*                                                                               
*                                                                               
*                                                                               
SCRNDERR DC    H'0'                INVALID DATASW                               
*                                                                               
*                                                                               
*                                                                               
SCRND70  LM    R5,R6,MYDUB2           RESTORE R5,R6                             
         OI    6(R5),X'80'            TRANSMIT                                  
         LA    R5,LDISP(R5)           BUMP UP TO NEXT LINE - UNCHNGBLE          
         OI    6(R6),X'80'            TRANSMIT                                  
         LA    R6,LDISP(R6)           BUMP TO NEXT LINE - CHANGEABLE            
*                                                                               
         LA    R7,4(R7)               BUMP TO NEXT DISC ADDRESS                 
*                                                                               
         LH    R1,COUNT                                                         
*                                                                               
         CLI   PUBNOPT,C'Y'           SEE IF DISPLAYING PUB NAME                
         BNE   SCRND75                                                          
         AHI   R1,-1                                                            
         C     R1,=F'2'               MUST HAVE 2 LINES                         
         BL    SEXIT1                                                           
         OI    6(R5),X'80'            TRANSMIT                                  
         LA    R5,LDISP(R5)           BUMP UP TO NEXT LINE - UNCHNGBLE          
         OI    6(R6),X'80'            TRANSMIT                                  
         OI    1(R6),X'20'            PROTECT PUB NAME LINE                     
         OI    4(R6),X'20'            ALSO SET PREVIOUSLY VALIDATED             
         LA    R6,LDISP(R6)           BUMP TO NEXT LINE - CHANGEABLE            
*                                                                               
SCRND75  DS    0H                                                               
         AHI   R1,-1                                                            
         STH   R1,COUNT                                                         
         LTR   R1,R1                                                            
         BNP   SEXIT1                                                           
         B     SCRLOOP                                                          
*                                                                               
SEXIT1   XMOD1 1                                                                
*                                                                               
         EJECT                                                                  
FMTRTM   NTR1                                                                   
         MVI   X,C' '              BLANK FILL                                   
         MVC   X+1(12),X                                                        
         ZAP   DUB,PBDCOS                                                       
         CVB   R1,DUB                                                           
         CLI   PBDCTYP,C'N'        NET INPUT SO DISPLAY AS NET                  
         BNE   FMTRTM4                                                          
*                                                                               
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
*                                                                               
FMTRTM4  EDIT  (R1),(10,X+5),2,ALIGN=LEFT,FLOAT=-                               
FMTRTX   DS    0H                                                               
         LA    R1,X+5                                                           
         CLI   PBDCOSIN,C' '       TEST DEFAULT COST TYPE                       
         BE    FMTRTX2                                                          
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCOSIN                                                 
*                                                                               
FMTRTX2  DS    0H                                                               
         CLI   PBDCTYP,C'N'        SEE IF NET INPUT                             
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCTYP                                                  
*                                                                               
         TM    PBDRLIND,X'08'      TEST FROZEN                                  
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MVI   0(R1),C'F'       (* IN BUY)                                      
*                                                                               
FMTRTX4  DS    0H                                                               
         MVC   8(11,R6),0(R1)                                                   
*                                                                               
         CP    PBDCOS,=P'0'                                                     
         BNZ   *+16                                                             
         XC    8(11,R6),8(R6)                                                   
         MVC   8(4,R6),=C'FREE'                                                 
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
FMTRTN   NTR1                                                                   
         MVI   X,C' '              BLANK FILL                                   
         MVC   X+1(12),X                                                        
         ZAP   DUB,PBDCOS                                                       
         CVB   R1,DUB                                                           
         CLI   PBDCTYP,C'N'        NET INPUT SO DISPLAY AS NET                  
         BNE   FMTRTN1                                                          
*                                                                               
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
*                                                                               
FMTRTN1  CLI   PBDCOSTY,C'U'       TEST UNIT RATE                               
         BE    FMTRTN2                                                          
*                                                                               
         C     R1,=F'99999999'   SEE IF TOTAL RATE OVER 999,999.99              
         BNH   FMTRTN1D                                                         
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'      HAVE ENTERED PENNIES WHEN BUYING                 
         LTR   R1,R1           (NO ROOM)                                        
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(9,X+5),0,FLOAT=-,ALIGN=LEFT                                
         B     FMTRTNX                                                          
*                                                                               
FMTRTN1D DS    0H                                                               
         EDIT  (R1),(9,X+5),2,FLOAT=-,ALIGN=LEFT                                
         B     FMTRTNX                                                          
*                                                                               
FMTRTN2  EDIT  (R1),(11,X+5),5,FLOAT=-,ALIGN=LEFT                               
*                                                                               
         LA    R2,X+5              START OF OUTPUT                              
         AR    R2,R0               + LENGTH                                     
         AHI   R2,-3               BACK UP TO LAST 3 BYTES                      
         CLC   =C'000',0(R2)                                                    
         BNE   *+10                                                             
         MVC   0(3,R2),X+60        MOVE SOME BLANKS                             
FMTRTNX  DS    0H                                                               
         LA    R1,X+5                                                           
* IF COST TYPE NOT 'U' DISPLAY IT. ELSE DISPLAY COST IND IF NOT C' '.           
         CLI   PBDCOSTY,C'U'       TEST UNIT RATE                               
         BE    *+12                YES - CHECK PBDCOSIN                         
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCOSTY                                                 
         CLI   PBDCOSIN,C' '       TEST DEFAULT COST TYPE                       
         BE    FMTRTNX2                                                         
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCOSIN                                                 
*                                                                               
FMTRTNX2 DS    0H                                                               
         CLI   PBDCTYP,C'N'        NET INPUT                                    
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCTYP                                                  
*                                                                               
         TM    PBDRLIND,X'08'      TEST FROZEN RATE                             
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MVI   0(R1),C'F'      (* IN BUY)                                       
FMTRTNX4 DS    0H              R6 POINTS TO FIELD HEADER                        
         MVC   8(11,R6),0(R1)                                                   
*                                                                               
         CP    PBDCOS,=P'0'                                                     
         BNZ   *+16                                                             
         XC    8(11,R6),8(R6)                                                   
         MVC   8(4,R6),=C'FREE'                                                 
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PIUPLELEM         UPLOAD ID ELEN DSECT                         
*                                                                               
*                                                                               
*                            SUBROUTINE TO EDIT NEWSPAPER RATES                 
EDTRTN   CSECT                                                                  
*                             *** DON'T TOUCH R8 IN ANY CSECT ***               
*                             *** IT IS NEEDED TO ACCESS ROUTINES               
*                             *** IN THE MAIN PROGRAM                           
         NMOD1 0,EDTRAT                                                         
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T418FFD,RA                                                       
*                                                                               
         SR    R3,R3           CLEAR ERROR REGISTER                             
*                                                                               
         L     R2,8(R1)           SET R2 TO FIELD HEADER                        
*                                                                               
         CLI   5(R2),0             MUST HAVE INPUT                              
         BE    EDTNRE2             MISSING ERROR                                
*                                                                               
         NI    PBDRLIND,X'F7'      SET OFF FROZEN BIT                           
*                                                                               
         XC    PBDRCODE,PBDRCODE   CLEAR RATE CODE                              
         MVI   BYTE,5              PRESET FOR 5 DEC.                            
         MVI   PBDCOSTY,C'U'       PRESET RATE TYPE                             
         CLI   PBDUIND,X'89'       LOWER CASE I (INCHES 2 DECIMALS)             
         BE    EDTNR2              SKIP SPACE CHECK                             
*                                                                               
         CLI   PBDUIND,C'L'        MAY ALSO HAVE SPACE (LINES)                  
         BE    EDTNR2                                                           
*                                                                               
         CLC   PBDSPACE(2),=C'* '  UNLESS SPACE BUY                             
         BNH   EDTNR2                                                           
         CLC   PBDSPACE(2),=X'7B00'    # AND BINARY ZERO                        
         BE    EDTNR2                  TREAT AS NON-SPACE BUY                   
         CLC   PBDSPACE(2),=C'# '      # AND SPACE                              
         BE    EDTNR2                  TREAT AS NON-SPACE BUY                   
         MVI   PBDCOSTY,C'T'                                                    
         MVI   BYTE,2                                                           
EDTNR2   MVI   PBDCOSIN,C' '                                                    
         MVI   PBDCTYP,0                                                        
         CLI   5(R2),0                                                          
         BE    EDTRATX                                                          
         SR    R7,R7                                                            
         IC    R7,5(R2)                                                         
         LA    R6,8(R2)                                                         
         TM    4(R2),X'04'         TEST VALID ALPHA                             
         BNZ   EDTNR7              (FREE, ETC)                                  
*                                                                               
EDTNR3   DS    0H                                                               
         CLI   0(R6),C'S'          GROSS=NET                                    
         BNE   EDTNR3C                                                          
*******  ZAP   PBDACP,=P'1'         TO PREVENT AC LOOK-UP                       
*                                    IS RESET TO P'0'                           
         MVI   PBDCOSIN,C'S'                                                    
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
**NEW 6/1/88                                                                    
EDTNR3C  DS    0H                                                               
**COM                                                                           
         CLC   PBUYKAGY,=C'SJ'                                                  
         BNE   EDTNR4                                                           
**COM                         SKIP COMMISSION RATE LOGIC FOR NOW                
**COM                                                                           
         CLI   0(R6),C'C'          COMMISSION RATE                              
         BNE   EDTNR4                                                           
         CLI   PBDCOSIN,C' '                                                    
         BNE   EDTNRE1             CAN'T ALREADY HAVE                           
         MVI   PBDCOSIN,C'C'       GETINS SETS GROSS TO AGYCOM                  
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
*                                                                               
EDTNR4   DS    0H                                                               
         CLI   0(R6),C'N'          NET TO BE GROSSED UP                         
         BNE   EDTNR5                                                           
         MVI   PBDCTYP,C'N'                                                     
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
         B     EDTNR3                                                           
*                                                                               
EDTNR5   DS    0H                                                               
         CLI   0(R6),C'T'          TOTAL RATE                                   
         BNE   EDTNR6                                                           
         MVI   PBDCOSTY,C'T'                                                    
         MVI   BYTE,2              DECIMALS                                     
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
         B     EDTNR3                                                           
*                                                                               
EDTNR6   DS    0H                                                               
         CLI   0(R6),C'F'          FROZEN RATE  (* IN BUY)                      
         BNE   EDTNR6D                                                          
         OI    PBDRLIND,X'08'                                                   
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
         B     EDTNR3                                                           
*                                                                               
EDTNR6D  CLC   0(2,R6),=C'R='        RATE CODE                                  
         BNE   EDTNR7                                                           
***                                                                             
         B     EDTNRE1    DON'T ALLOW RATE CODES                                
***                                                                             
***                       RCODE BUYS ARE SKIPPED IN READ                        
***                                                                             
***      LA    R6,2(R6)                                                         
***      BCTR  R7,R0                                                            
***      BCTR  R7,R0                                                            
***      CLI   0(R6),C' '                                                       
***      BNH   EDTNRE1                                                          
***      CH    R7,=H'3'                 MAX 3 CHARS                             
***      BH    EDTNRE1                                                          
***      MVC   PBDRCODE,0(R6)           SAVE CODE IN PBUYREC                    
***      OC    PBDRCODE,=3C' '                                                  
***                                     NEW RATE LOOK WILL FIND RATE            
***      B     EDTRATX                    RETURN                                
*                                                                               
EDTNR7   DS    0H                                                               
*                                                                               
*        CHECK BELOW NOT NEEDED SINCE PBDCOSIN WILL NOT BE                      
*        ALLOWED TO CHANGE                                                      
*                                                                               
*******  CLI   SVESPROF+28,C'C'          SEE IF 'C' RATE EST                    
*******  BNE   EDTNR8                                                           
*******  CLI   PBDCOSIN,C'C'              'C' INPUT                             
*******  BE    EDTNR10                                                          
*******  CLI   PBDCOSIN,C' '            NOTHING INPUT  - SET TO 'C'             
*******  BNE   EDTNRE1                                                          
*******  MVI   PBDCOSIN,C'C'                                                    
         B     EDTNR10                                                          
*                                                                               
EDTNR8   CLI   PBDCOSIN,C'C'         'C' RATE ON NON 'C' RATE EST               
         BE    EDTNRE1                                                          
*                                                                               
EDTNR10  DS    0H                                                               
         GOTO1 VCASHVAL,DMCB,(BYTE,(R6)),(R7)                                   
         CLI   0(R1),X'FF'                                                      
         BE    EDTNRE1                                                          
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    DUB,=P'999999999'   MAX FOR PBDCOS                               
         BH    EDTNRE1                                                          
         CLI   BYTE,5              SEE IF 5 DECIMALS                            
         BNE   EDTNR10D                                                         
         CP    DUB,=P'299999999'   MAX UNIT RATE IS 2999.99999                  
         BH    EDTNRE1             ** THEY HAVE PROBABLY ENTERED A              
*                                  TOTAL COST BY MISTAKE **                     
EDTNR10D ZAP   PBDCOS,DUB                                                       
*******  LTR   R0,R0               IF RATE IS FREE, SET COST TO                 
*******  BNZ   *+10                  .00001 AND RESET AFTER                     
*******  ZAP   PBDCOS,=P'1'          RATE LOOK-UP                               
*                                                                               
EDTRATX  BAS   R9,CHKGRS                                                        
         CLC   PBDCOSIN,SAVFLD+8    PBDCOSIN CAN'T CHANGE                       
         BNE   EDTNRE1                                                          
         CLC   PBDCOSTY,SAVFLD+5    UNIT OR TOTAL RATE IND                      
         BNE   EDTNRE1              CAN'T CHANGE                                
*                                                                               
*        THE CHECK ABOVE SHOULD INSURE THAT TOTAL RATES                         
*        ARE ONLY CHANGED WITH DATASW '$' AND                                   
*        UNIT RATES ARE ONLY CHANGED WITH DATASW 'M' (INCHES)                   
*        AND DATASW 'N' (LINES)                                                 
*                                                                               
*        SEE IF NET RATE ENTERED - MUST BE GROSSED-UP IN PBDCOS                 
*        DATA SET PPBUY03    AT LEVEL 027 AS OF 08/03/99                        
*                                                                               
         CLI   PBDCTYP,C'N'        TEST NET INPUT                               
         BNE   EDTRATX5                                                         
*                                  GROSS UP COST                                
         CP    PBDACP,=P'0'        UNLESS THERE IS NO AC                        
         BE    EDTRATX5            THEN LEAVE IT ALONE                          
         CP    PBDACP,=P'1'        OR AC WAS OVERRIDDEN TO 0                    
         BE    EDTRATX5            THEN LEAVE IT ALONE                          
*                                                                               
         ZAP   DUB,PBDCOS                                                       
         CVB   R1,DUB                                                           
         M     R0,=F'100000'                                                    
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'       =NET PCT                                     
         LCR   RF,RF                                                            
         BNP   EDTRATX5                                                         
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         CVD   R1,DUB                                                           
         ZAP   PBDCOS,DUB                                                       
*                                                                               
EDTRATX5 DS    0H                  TEST NET PREMIUM CHARGE INPUT                
*                                                                               
EDTRATXX XIT1  REGS=(R3)                                                        
*                                                                               
*                                  CHECK GROSS NOT TOO LARGE                    
CHKGRS   DS    0H                                                               
         CLI   PBDCOSTY,C'U'                                                    
         BNER  R9                                                               
         CP    PBDCOS,=P'300000000'                                             
         BH    CHKGRS8             PROBABLY TOTAL COST                          
         CP    PBDCOS,=P'-300000000'                                            
         BL    CHKGRS8             PROBABLY TOTAL COST                          
*                                  NOT LINE OR INCH RATE                        
*                                  TRY TO PROTECT MULTILPY PACKED INS           
         ZAP   WORK(16),PBDUNITS                                                
         MP    WORK(16),PBDCOS          PBDCOS HAS 5 DECIMALS                   
         DP    WORK(16),=P'100'                                                 
         ZAP   WORK(16),WORK(14)                                                
         DP    WORK(16),=P'10'                                                  
         CP    WORK(14),=P'-2100000000'  MAX FOR FULL WORD                      
         BL    CHKGRS8                                                          
*                                                                               
         CP    WORK(14),=P'2100000000'   MAX FOR FULL WORD                      
         BLR   R9                                                               
CHKGRS8  DS    0H                                                               
         B     EDTNRE1                                                          
*                                                                               
EDTNRE1  LA    R3,FLDINV                                                        
         B     EDTRATXX                                                         
*                                                                               
EDTNRE2  LA    R3,MISSERR                                                       
         B     EDTRATXX                                                         
*                                                                               
         LTORG                                                                  
RATIND   DS    CL1                                                              
         EJECT                                                                  
*                                                                               
*                   THIS ROUTINE UPDATES/ADDS THE CHANGE ELEMENT                
*                   AND BUY CHANGE INDICATORS                                   
UPREC    CSECT                                                                  
         NMOD1 0,UPREC                                                          
*                             *** DON'T TOUCH R8 IN ANY CSECT ***               
*                             *** IT IS NEEDED TO ACCESS ROUTINES               
*                             *** IN THE MAIN PROGRAM                           
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T418FFD,RA                                                       
*                                                                               
UPREC64  CLC   PBDDATE,BTODAY       SEE IF LAST CHA WAS TODAY                   
         BNE   UPREC64A                                                         
         OC    PBDDTIND,CHADTIND                                                
         OC    PBDDTIN2,CHADTIN2                                                
         OC    PBDDTIN3,CHADTIN3                                                
         B     UPREC64B                                                         
*                                                                               
UPREC64A MVC   PBDDATE,BTODAY         SET NEW CHANGE DATE                       
         MVC   PBDDTIND,CHADTIND                                                
         MVC   PBDDTIN2,CHADTIN2                                                
         MVC   PBDDTIN3,CHADTIN3                                                
*                                                                               
*                                                                               
UPREC64B CLI   CHADTIND,0        SEE IF CHA NEEDS X'24' ELEM                    
         BNE   UPREC64E                                                         
         CLI   CHADTIN2,0                                                       
         BNE   UPREC64E                                                         
         CLI   CHADTIN3,0                                                       
         BNE   UPREC64E                                                         
         CLI   CHADTIN4,0                                                       
         BNE   UPREC64E                                                         
         B     UPREC65         NO NEED TO ADD/UPDATE X'24' ELEM                 
*                                                                               
UPREC64E GOTO1 VDATCON,DMCB,(3,PBDDATE),(2,DUB)                                 
         XC    CHGELEM,CHGELEM                                                  
         MVI   ELCODE,X'24'                                                     
         LA    R2,PBUYREC+33                                                    
UPREC64G BAS   RE,UNEXTEL                                                       
         BNE   UPREC64O              GO ADD NEW ELEM                            
         CLC   2(2,R2),DUB                                                      
         BNE   UPREC64G                                                         
         USING PCHGELED,R2           IF I HAVE ONE FOR TODAY                    
         OC    PCHGIND1,CHADTIND                                                
         OC    PCHGIND2,CHADTIN2                                                
         OC    PCHGIND3,CHADTIN3                                                
         OC    PCHGIND4,CHADTIN4                                                
         CLI   DATASW,C'7'           SEE IF CD CHANGED                          
         BE    UPREC64H                                                         
         CLI   DATASW,C'8'           OR AC                                      
         BE    UPREC64H                                                         
         CLI   DATASW,C'$'           RATE CHANGE                                
         BE    UPREC64H                                                         
         CLI   DATASW,C'M'           INCH RATE                                  
         BE    UPREC64H                                                         
         CLI   DATASW,C'N'           LINE RATE                                  
         BE    UPREC64H                                                         
         B     UPREC64W                                                         
*                                                                               
UPREC64H CLC   GROSS(12),SVGROSS    SEE IF GROSS,CD,AC  CHANGED                 
         BE    UPREC64W              NO THEN DONE                               
*                                                                               
UPREC64N DS    0H                                                               
         CLI   PCHGLEN,20           SEE IF OLD $ ALREADY THERE                  
         BE    UPREC64W             DONE                                        
         DROP  R2                                                               
*                                                                               
         MVC   CHGELEM(25),0(R2)    SAVE OLD ELEM                               
         GOTO1 VRECUP,DMCB,(1,PBUYREC),(R2) THEN DELETE IT                      
         BAS   RE,ULEAREND                                                      
*                                                                               
         LA    R3,CHGELEM                                                       
         USING PCHGELED,R3                                                      
*                                                                               
         MVI   PCHGLEN,20                                                       
         MVC   PCHGGRS(12),SVGROSS                                              
*                                                                               
         DROP  R3                                                               
*                                                                               
         CLC   PBUYREC+25(2),=H'2975' MAXIMUM REC SIZE                          
         BNL   UPREC64W               CAN'T ADD 24 ELEM                         
*                                                                               
*     R2 SHOULD STILL BE IN PBUYREC                                             
         GOTO1 VRECUP,DMCB,(1,PBUYREC),CHGELEM,(R2)                             
         B     UPREC64W                                                         
*                                                                               
UPREC64O LA    R3,CHGELEM                                                       
         USING PCHGELED,R3                                                      
         MVI   PCHGELEM,X'24'                                                   
         MVI   PCHGLEN,8                                                        
         MVC   PCHGDAT,DUB                                                      
         MVC   PCHGIND1,CHADTIND                                                
         MVC   PCHGIND2,CHADTIN2                                                
         MVC   PCHGIND3,CHADTIN3                                                
         MVC   PCHGIND4,CHADTIN4                                                
         CLI   DATASW,C'7'           SEE IF CD OR AC CHANGED                    
         BE    UPREC64P                                                         
         CLI   DATASW,C'8'           SEE IF CD OR AC CHANGED                    
         BE    UPREC64P                                                         
         CLI   DATASW,C'$'           SEE IF RATE CHANGE                         
         BE    UPREC64P                                                         
         CLI   DATASW,C'M'           SEE IF INCH RATE                           
         BE    UPREC64P                                                         
         CLI   DATASW,C'N'           SEE IF LINE RATE                           
         BNE   UPREC64R                                                         
UPREC64P CLC   GROSS(12),SVGROSS     SEE IF $ CHANGED                           
         BE    UPREC64R                                                         
         MVI   PCHGLEN,20                                                       
         MVC   PCHGGRS(12),SVGROSS                                              
*                                                                               
UPREC64R CLC   PBUYREC+25(2),=H'2975' MAXIMUM REC SIZE                          
         BNL   UPREC64W               CAN'T ADD 24 ELEM                         
         GOTO1 VRECUP,DMCB,(1,REC),CHGELEM,(R2)                                 
*                                                                               
UPREC64W DS    0H                                                               
         TM    CHADTIND,X'40'     RATE CHANGE                                   
         BO    UPRC64W1                                                         
         TM    CHADTIN2,X'06'     AC OR CD                                      
         BZ    UPREC65                                                          
*                                                                               
UPRC64W1 DS    0H                                                               
*                                                                               
*        REDISPLAY COST                                                         
*                                                                               
         CLI   DOLSW,0                 DON'T DISPLAY COST                       
         BE    UPREC64X                                                         
         MVC   65(10,R5),=CL10' '      CLEAR OLD $                              
*                                                                               
         CLI   DOLSW,C'2'              GROSS - CASH DISC. ORDERED               
         BNE   UPRC64W2                                                         
         EDIT  BLABLE,(10,65(R5)),2,FLOAT=-                                     
         B     UPRC64W9                                                         
*                                                                               
UPRC64W2 CLI   DOLSW,C'3'              NET ORDERED                              
         BNE   UPRC64W4                                                         
         L     R4,GROSS                CALCULATE:                               
         CVD   R4,MYDUB                NET=GROSS - AGENCY COMM.                 
         L     R4,AGYCOM                                                        
         CVD   R4,DUB                                                           
         SP    MYDUB,DUB                                                        
         EDIT  (P8,MYDUB),(10,65(R5)),2,FLOAT=-                                 
         B     UPRC64W9                                                         
*                                                                               
UPRC64W4 CLI   DOLSW,C'4'              NET - CASH DISC ORDERED                  
         BNE   UPRC64W6                                                         
         EDIT  PYABLE,(10,65(R5)),2,FLOAT=-                                     
         B     UPRC64W9                                                         
*                                                                               
UPRC64W6 CLI   DOLSW,C'5'              CASH DISCOUNT                            
         BNE   UPRC64W8                                                         
         EDIT  CSHDSC,(10,65(R5)),2,FLOAT=-                                     
         B     UPRC64W9                                                         
*                                                                               
UPRC64W8 DS    0H                                                               
         EDIT  GROSS,(10,65(R5)),2,FLOAT=-                                      
*                                                                               
UPRC64W9 DS    0H                                                               
         FOUT  (R5)                                                             
         OI    6(R5),X'80'         MUST RE-TRANSMIT                             
*                                                                               
UPREC64X MVC   PBDCHGDT,PBDDATE      SET EST CHANGE DATE                        
UPREC65  XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
UNEXTEL  DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    UNEXTELX                                                         
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     UNEXTEL                                                          
*                                                                               
UNEXTELX LTR   RE,RE                                                            
         BR    RE                  SET CONDITION CODE NE                        
         EJECT                                                                  
ULEAREND NTR1                                                                   
         SR    R2,R2                                                            
         IC    R2,REC+25            CLEAR END OF REC                            
         SLL   R2,8                                                             
         IC    R2,REC+26                                                        
         SR    RE,RE                                                            
         LA    RE,REC                                                           
         AR    RE,R2                                                            
         LA    RF,REC+999                                                       
         AHI   RF,2000                                                          
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
CHGELEM  DS    CL25                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
ASR      CSECT                                                                  
         NMOD1 0,ASR                                                            
*                             *** DON'T TOUCH R8 IN ANY CSECT ***               
*                             *** IT IS NEEDED TO ACCESS ROUTINES               
*                             *** IN THE MAIN PROGRAM                           
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T418FFD,RA                                                       
*                                                                               
         OC    MBCCLT,=C'   '    JUST IN CASE                                   
*                                                                               
ASRL     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),MBCMDIA                                                 
         MVI   KEY+3,X'10'                                                      
         MVC   KEY+4(3),MBCCLT                                                  
         CLI   SVCLPROF+5,C'2'     TEST SLAVE CLIENT                            
         BNE   *+10                                                             
         MVC   KEY+4(3),SVCLPROF+6  READ MASTER CLT CONTRACTS                   
         MVC   KEY+7(6),PBUYKPUB                                                
         BAS   RE,HIGH                                                          
         B     *+8                                                              
ASRL2    BAS   RE,SEQ                                                           
         CLC   KEYSAVE(13),KEY     TEST SAME A/M/REC/CL/PUB                     
         BNE   ASRL2X                                                           
         MVC   AREC,ACONIO         READ INTO CONIO                              
         BAS   RE,GETREC                                                        
         LA    RF,REC              RESTORE AREC                                 
         ST    RF,AREC                                                          
         L     R6,ACONIO                                                        
         USING PCONRECD,R6                                                      
*                                                                               
         CLI   PCONPRD,C'A'        SEE IF A PRODUCT CONTRACT                    
         BL    ASRL2C                                                           
         CLC   PCONPRD,PBUYKPRD       PRODUCTS MUST MATCH                       
         BNE   ASRL2                                                            
*                                                                               
ASRL2C   CLC   PCONEND(3),PBUYKDAT    CONTRACT END BEFORE BUY                   
         BL    ASRL2                                                            
         CLC   PBUYKDAT(3),PCONSTRT   BUY BEFORE CONTRACT START                 
         BL    ASRL2                                                            
         B     ASRUPD5        CON FND GO SEE IF I NEED TO UPDATE                
         DROP  R6                                                               
ASRL2X   DS    0H                                                               
         B     ASRX                 NO CONTRACT                                 
*                                                                               
ASRUPD5  L     R2,ACONIO                                                        
         LA    R2,33(R2)                                                        
         MVI   ELCODE,X'85'       LOOK FOR ASRELEM                              
         BAS   RE,SNEXTEL                                                       
         BNE   ASRX                NO ASR FOR THIS CONTRACT                     
         CLI   SVCLPROF+5,C'2'     SEE IF DOING SLAVE CLIENT                    
         BNE   ASRUPD10            YES                                          
         USING PASRELMD,R2                                                      
         L     R2,ACONIO            LOOK FOR ELEM FOR THIS CLIENT               
         LA    R2,33(R2)                                                        
ASRUPD6  BAS   RE,SNEXTEL                                                       
         BNE   ASRUPD7                                                          
         CLC   PASRCLT,MBCCLT                                                   
         BNE   ASRUPD6                                                          
         B     ASRUPD10               FOUND  - UPDATE                           
*                                                                               
ASRUPD7  XC    NEWEL(20),NEWEL        MUST ADD ELEM FOR THIS CLT                
         MVI   NEWEL,X'85'                                                      
         MVI   NEWEL+1,X'0D'                                                    
         MVC   NEWEL+5(3),BTODAY                                                
         MVC   NEWEL+8(3),MBCCLT                                                
         L     R6,ACONIO                                                        
         GOTO1 VRECUP,DMCB,(1,(R6)),NEWEL,(R2)                                  
         B     ASRUPD12                                                         
*                                                                               
ASRUPD10 CLC   PASRCDAT,BTODAY                                                  
         BE    ASRX                MATCHES TODAY - DONE                         
         MVC   PASRLDAT,PASRCDAT   SAVE LAST RUN DATE                           
         MVC   PASRCDAT,BTODAY     SET TODAY IN CURRENT                         
ASRUPD12 MVC   AREC,ACONIO                                                      
         BAS   RE,PUTREC                                                        
ASRX     LA    RF,REC                                                           
         ST    RF,AREC             RESTORE AREC                                 
         XC    KEY,KEY                                                          
         MVC   KEY+27(4),0(R7)      REREAD BUYREC                               
         BAS   RE,GETREC                                                        
         XIT1                                                                   
*                                                                               
NEWEL    DS    CL20                                                             
*                                                                               
*                                                                               
SNEXTEL DS     0H                                                               
         CLI   0(R2),0                                                          
         BE    SNEXTELX                                                         
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     SNEXTEL                                                          
*                                                                               
SNEXTELX LTR   RE,RE                                                            
         BR    RE                  SET CONDITION CODE NE                        
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        MUST READ JOBREC TO SEE IF IT IS A FSI JOB                             
*                                                                               
FSILOOK  CSECT                                                                  
         NMOD1 0,FSILOOK                                                        
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T418FFD,RA                                                       
*                             *** DON'T TOUCH R8 IN ANY CSECT ***               
*                             *** IT IS NEEDED TO ACCESS ROUTINES               
*                             *** IN THE MAIN PROGRAM                           
*                                                                               
         LA    R2,PBUYREC+33      FIRST SEE IF MY REC ALREADY HAS FSI           
         MVI   ELCODE,X'82'       ELEM                                          
         BAS   RE,FNEXTEL                                                       
         BE    FSIL05Z            IF SO THEN DONE                               
*                                                                               
         CLC   8(6,R6),=6C' '     SEE IF I HAVE A JOB                           
         BNH   FSIL05Z            NO THEN DONE                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYREC                                                  
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+10(6),8(R6)       USE INPUT JOB                              
         OC    KEY+10(6),=6C' '                                                 
         L     R4,AWRKREC                                                       
         CLC   0(16,R1),KEY           SEE IF IT IS IN WRKREC                    
         BE    FSIL05D                                                          
         BAS   RE,HIGH                                                          
         CLC   KEY(16),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                  THIS SHOULD NEVER HAPPEN                   
*                                    SINCE MISSING AD SHOULD BE FOUND           
*                                    IN CHKCHA PASS                             
*                                                                               
         MVC   AREC,AWRKREC                                                     
         BAS   RE,GETREC                                                        
*                                                                               
FSIL05D  DS    0H                                                               
         ZAP   DUB(6),=P'0'                                                     
         USING PJOBREC,R4                                                       
         CLI   PJOBFSI,C'Y'                                                     
         BNE   FSIL05X                                                          
         BAS   RE,FINDFSI                                                       
*                                                                               
         DROP  R4                                                               
*                                                                               
FSIL05X  DS    0H                                                               
*                                                                               
         LA    R0,REC         MUST RESET AREC AND REREAD BUYREC                 
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+27(4),0(R7)                                                  
         BAS   RE,GETREC                                                        
*                                                                               
         CP    DUB(6),=P'0'                                                     
         BE    FSIL05Z                                                          
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'82'                                                     
         BAS   RE,FNEXTEL                                                       
         BNE   FSIL05O                                                          
         DC    H'0'                 SOMETHING VERY WRONG                        
*                                   THERE CAN'T BE AN ELEMENT NOW               
FSIL05O  XC    WORK(12),WORK                                                    
         MVC   WORK(2),=X'820D'                                                 
         MVI   WORK+2,X'01'      LOOKED-UP                                      
         MVC   WORK+3(3),BTODAY                                                 
         MVC   WORK+6(5),DUB+1                                                  
         GOTO1 VRECUP,DMCB,(1,PBUYREC),WORK,(R2)                                
*                                                                               
FSIL05Z  DS    0H                                                               
         XMOD1 1                                                                
*                                                                               
FNEXTEL  DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    FNEXTELX                                                         
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     FNEXTEL                                                          
*                                                                               
FNEXTELX LTR   RE,RE                                                            
         BR    RE                  SET CONDITION CODE NE                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                  INITIALISATION CODE                                          
*                                                                               
SETSCRN  CSECT                                                                  
         NMOD1 0,SETSCRN                                                        
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T418FFD,RA                                                       
*                             *** DON'T TOUCH R8 IN ANY CSECT ***               
*                             *** IT IS NEEDED TO ACCESS ROUTINES               
*                             *** IN THE MAIN PROGRAM                           
*                                                                               
         CLI   DATASW,C'A'                                                      
         BNE   SCRND02                                                          
         MVC   MBCDAN1,=CL11'AD CODE'                                           
         MVC   MBCDAN2,=CL11'-------'                                           
         B     SCRND0X                                                          
*                                                                               
SCRND02  CLI   DATASW,C'L'                                                      
         BNE   SCRND05                                                          
         MVC   MBCDAN1,=CL11'NV LETTER'                                         
         MVC   MBCDAN2,=CL11'---------'                                         
         B     SCRND0X                                                          
*                                                                               
SCRND05  CLI   DATASW,C'1'                                                      
         BNE   SCRND06                                                          
         MVC   MBCDAN1,=CL11'ON SALE'                                           
         MVC   MBCDAN2,=CL11'-------'                                           
         B     SCRND0X                                                          
*                                                                               
SCRND06  CLI   DATASW,C'2'                                                      
         BNE   SCRND07                                                          
         MVC   MBCDAN1,=CL11'BILLABLE'                                          
         MVC   MBCDAN2,=CL11'--------'                                          
         B     SCRND0X                                                          
*                                                                               
SCRND07  CLI   DATASW,C'3'                                                      
         BNE   SCRND08                                                          
         MVC   MBCDAN1,=CL11'PAYABLE'                                           
         MVC   MBCDAN2,=CL11'-------'                                           
         B     SCRND0X                                                          
*                                                                               
SCRND08  CLI   DATASW,C'4'                                                      
         BNE   SCRND09                                                          
         MVC   MBCDAN1,=CL11'CLOSING'                                           
         MVC   MBCDAN2,=CL11'-------'                                           
         B     SCRND0X                                                          
*                                                                               
SCRND09  CLI   DATASW,C'5'                                                      
         BNE   SCRND0A                                                          
         MVC   MBCDAN1,=CL11'MATERIALS'                                         
         MVC   MBCDAN2,=CL11'---------'                                         
         B     SCRND0X                                                          
*                                                                               
SCRND0A  CLI   DATASW,C'7'                                                      
         BNE   SCRND0C                                                          
         MVC   MBCDAN1,=CL11'CASH DISC'                                         
         MVC   MBCDAN2,=CL11'---------'                                         
         B     SCRND0X                                                          
*                                                                               
SCRND0C  CLI   DATASW,C'8'                                                      
         BNE   SCRND0D                                                          
         MVC   MBCDAN1,=CL11'AGY COMM'                                          
         MVC   MBCDAN2,=CL11'--------'                                          
         B     SCRND0X                                                          
*                                                                               
SCRND0D  CLI   DATASW,C'S'                                                      
         BNE   SCRND0E                                                          
         MVC   MBCDAN1,=CL11'STATUS'                                            
         MVC   MBCDAN2,=CL11'------'                                            
         B     SCRND0X                                                          
*                                                                               
SCRND0E  CLI   DATASW,C'C'         CONTRACT UNITS                               
         BNE   SCRND0F                                                          
         MVC   MBCDAN1,=CL11'CU-TIMES'                                          
         MVC   MBCDAN2,=CL11'--------'                                          
         CLI   SVCONL,C'X'                                                      
         BE    SCRND0X                                                          
         MVC   MBCDAN1,=CL11'CU-PAGES'                                          
         MVC   MBCDAN2,=CL11'--------'                                          
         CLI   SVCONL,C'P'                                                      
         BE    SCRND0X                                                          
         MVC   MBCDAN1,=CL11'CU-ISSUES'                                         
         MVC   MBCDAN2,=CL11'---------'                                         
         CLI   SVCONL,C'U'                                                      
         BE    SCRND0X                                                          
         MVC   MBCDAN1,=CL11'CON UNITS'                                         
         MVC   MBCDAN2,=CL11'---------'                                         
         B     SCRND0X                                                          
*                                                                               
SCRND0F  CLI   DATASW,C'P'         PLANNED COST                                 
         BNE   SCRND0G                                                          
         MVC   MBCDAN1,=CL11'PLANNED $'                                         
         MVC   MBCDAN2,=CL11'---------'                                         
         B     SCRND0X                                                          
*                                                                               
SCRND0G  CLI   DATASW,C'D'         DLC                                          
         BNE   SCRND0H                                                          
         MVC   MBCDAN1,=CL11'DLC (000)'                                         
         MVC   MBCDAN2,=CL11'---------'                                         
         B     SCRND0X                                                          
*                                                                               
*                                                                               
SCRND0H  CLI   DATASW,C'F'         FSI                                          
         BNE   SCRND0I                                                          
         MVC   MBCDAN1,=CL11'FREE STAND-'                                       
         MVC   MBCDAN2,=CL11'ING INSERTS'                                       
         B     SCRND0X                                                          
*                                                                               
SCRND0I  CLI   DATASW,C'R'         REFERENCE NUMBER                             
         BNE   SCRND0J                                                          
         MVC   MBCDAN1,=CL11'REFERENCE'                                         
         MVC   MBCDAN2,=CL11' NUMBER  '                                         
         B     SCRND0X                                                          
*                                                                               
SCRND0J  CLI   DATASW,C'T'         RPT                                          
         BNE   SCRND0K                                                          
         MVC   MBCDAN1,=CL11'REPAINTS '                                         
         MVC   MBCDAN2,=CL11'-------- '                                         
         B     SCRND0X                                                          
*                                                                               
SCRND0K  CLI   DATASW,C'H'         SHIP DATE                                    
         BNE   SCRND0L                                                          
         MVC   MBCDAN1,=CL11'SHIP DATE'                                         
         MVC   MBCDAN2,=CL11'---------'                                         
         B     SCRND0X                                                          
*                                                                               
SCRND0L  CLI   DATASW,C'E'         T/S STATUS                                   
         BNE   SCRND0M                                                          
         MVC   MBCDAN1,=CL11'T/S STATUS'                                        
         MVC   MBCDAN2,=CL11'----------'                                        
         B     SCRND0X                                                          
*                                                                               
SCRND0M  CLI   DATASW,C'V'         PAGE VIEWS                                   
         BNE   SCRND0N                                                          
         MVC   MBCDAN1,=CL11'PAGE VIEWS'                                        
         MVC   MBCDAN2,=CL11'----------'                                        
         B     SCRND0X                                                          
*                                                                               
SCRND0N  CLI   DATASW,C'K'         CLICK THRUS                                  
         BNE   SCRND0O                                                          
         MVC   MBCDAN1,=CL11'CLICKTHRUS'                                        
         MVC   MBCDAN2,=CL11'----------'                                        
         B     SCRND0X                                                          
*                                                                               
SCRND0O  CLI   DATASW,C'I'         IMPRESSIONS                                  
         BNE   SCRND0P                                                          
         MVC   MBCDAN1,=CL11'IMPRESSIONS'                                       
         MVC   MBCDAN2,=CL11'-----------'                                       
         B     SCRND0X                                                          
*                                                                               
SCRND0P  CLI   DATASW,C'X'     SFH (SPECIAL FINANCIAL HANDLING) STATUS          
         BNE   SCRND0R                                                          
         MVC   MBCDAN1,=CL11'SFH STATUS'                                        
         MVC   MBCDAN2,=CL11'----------'                                        
         B     SCRND0X                                                          
*                                                                               
SCRND0R  CLI   DATASW,C'B'     COST2 FACTOR                                     
         BNE   SCRND0S                                                          
         MVC   MBCDAN1,=CL11'C2 FACTOR'                                         
         MVC   MBCDAN2,=CL11'---------'                                         
         B     SCRND0X                                                          
*                                                                               
**RATES**                                                                       
SCRND0S  CLI   DATASW,C'$'     RATE                                             
         BNE   SCRND0T                                                          
         MVC   MBCDAN1,=CL11'RATE     '                                         
         MVC   MBCDAN2,=CL11'----     '                                         
         CLI   MBCMDIA,C'N'       SEE IF NEWSPAPERS                             
         BNE   SCRND0X                                                          
         MVC   MBCDAN1,=CL11'TOTAL RATE'                                        
         MVC   MBCDAN2,=CL11'----------'                                        
         B     SCRND0X                                                          
*                                                                               
SCRND0T  CLI   DATASW,C'M'     INCH RATE                                        
         BNE   SCRND0U                                                          
         MVC   MBCDAN1,=CL11'INCH RATE'                                         
         MVC   MBCDAN2,=CL11'---------'                                         
         B     SCRND0X                                                          
*                                                                               
SCRND0U  CLI   DATASW,C'N'     LINE RATES                                       
         BNE   SCRND0V                                                          
         MVC   MBCDAN1,=CL11'LINE RATE'                                         
         MVC   MBCDAN2,=CL11'---------'                                         
         B     SCRND0X                                                          
**RATES**                                                                       
*                                                                               
SCRND0V  CLI   DATASW,C'U'         UPID                                         
         BNE   SCRND1A                                                          
         MVC   MBCDAN1,=CL11'UPLOAD ID'                                         
         MVC   MBCDAN2,=CL11'---------'                                         
         B     SCRND0X                                                          
*                                                                               
SCRND1A  DS    0H                  FUTURE USES                                  
*                                                                               
SCRND0ER DC    H'0'                INVALID DATASW                               
*                                                                               
SCRND0X  FOUT  MBCDAN1H                                                         
         FOUT  MBCDAN2H                                                         
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
ASC      CSECT                     AUTO SCHEDULE CHECKING                       
         NMOD1 0,ASC                                                            
*                             *** DON'T TOUCH R8 IN ANY CSECT ***               
*                             *** IT IS NEEDED TO ACCESS ROUTINES               
*                             *** IN THE MAIN PROGRAM                           
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T418FFD,RA                                                       
ASCL     DS    0H                                                               
*                                                                               
         XC    PUBCNT,PUBCNT       BUYS FOR THIS PUB                            
         XC    ZECNT,ZECNT         BUY COUNT ACROSS ZONES/EDITIONS              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'10'                                                      
         MVC   KEY+4(3),WORKCLT                                                 
         CLI   SVCLPROF+5,C'2'     TEST SLAVE CLIENT                            
         BNE   *+10                                                             
         MVC   KEY+4(3),SVCLPROF+6  READ MASTER CLT CONTRACTS                   
         MVC   KEY+7(6),SAVBYPUB                                                
*                                                                               
*                                                                               
         OC    SADVDATA,SADVDATA      NEW ADV SYSTEM                            
         BZ    ASCL1               NO                                           
         TM    SVAORC,X'10'        ADV SCHEDULE CHECKING                        
         BZ    ASCL1                                                            
         CLC   SVAOR,AGYALPHA      SEE IF AM THE AOR                            
         BE    ASCL1                                                            
*                                                                               
         TM    SVAORC,X'01'        TEST PUB LINK                                
         BZ    ASCL0C                                                           
         BAS   RE,FNDAPUB                                                       
*                                                                               
         MVC   KEY+7(6),SVADVPUB                                                
ASCL0C   MVC   KEY(2),SVAOR        AOR                                          
         MVC   KEY+4(3),SVADV      ADV                                          
*                                                                               
*                                   MUST SWITCH TO AOR                          
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SVAORSE                                                  
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    ASCL1                                                            
         LA    R3,ADVFERR            AOR NOT ACTIVE                             
         B     ASCX                                                             
*                                                                               
ASCL1    DS    0H                                                               
*                                                                               
*        DURING CKECK COUNT INSERTIONS I AM MAKING LIVE                         
*        TOWARD CONTRACT MAXIMUM                                                
*                                                                               
         LA    R1,PUBDTTAB                                                      
ASCL1B   CLI   0(R1),X'FF'        END OF TABLE                                  
         BE    ASCL1D                                                           
         CLC   0(6,R1),KEY+7       CHK PUB                                      
         BNE   ASCL1E                                                           
         CLC   6(3,R1),SAVBYDAT    CHK DATE                                     
         BNE   ASCL1E                                                           
         ZIC   RE,9(R1)                                                         
*                                                                               
         STH   RE,PUBCNT            SET NUMBER IN PUBCNT                        
         STH   RE,ZECNT             AND IN ZECNT                                
*                                                                               
         LA    RE,1(RE)                                                         
         STC   RE,9(R1)                                                         
         B     ASCL1X                                                           
*                                                                               
ASCL1D   MVC   0(6,R1),KEY+7         SAVE PUB                                   
         MVC   6(3,R1),SAVBYDAT      AND DATE                                   
         MVI   9(R1),X'01'                                                      
         MVI   10(R1),X'FF'          SET END OF TABLE                           
         B     ASCL1X                                                           
*                                                                               
ASCL1E   LA    R1,10(R1)                                                        
         B     ASCL1B                                                           
*                                                                               
ASCL1X   BAS   RE,HIGH                                                          
         B     *+8                                                              
ASCL2    BAS   RE,SEQ                                                           
         CLC   KEYSAVE(13),KEY     TEST SAME A/M/REC/CL/PUB                     
         BNE   ASCL2X                                                           
         MVC   AREC,ACONIO         READ INTO CONIO                              
         BAS   RE,GETREC                                                        
*                                                                               
ASCL2C   DS    0H                                                               
         LA    RF,REC              RESTORE AREC                                 
         ST    RF,AREC                                                          
*                                                                               
         L     R6,ACONIO                                                        
         USING PCONRECD,R6                                                      
         CLC   PCONEND(3),SAVBYDAT    CONTRACT END BEFORE BUY                   
         BL    ASCL2                                                            
         CLC   SAVBYDAT(3),PCONSTRT   BUY BEFORE CONTRACT START                 
         BL    ASCL2                                                            
         CLI   PCONPRD,C'A'     SEE IF PRD CONTRACT                             
         BL    ASCUPD5                                                          
         CLC   PCONPRD,SAVBYPRD    PRD CONTRACT - MUST MATCH                    
         BNE   ASCL2                                                            
*                                                                               
         OC    SADVDATA,SADVDATA      NEW ADV SYSTEM                            
         BZ    ASCL2G              NO                                           
         TM    SVAORC,X'10'        ADV SCHEDULE CHECKING                        
         BZ    ASCL2G                                                           
         CLC   SVAOR,AGYALPHA      SEE IF AM THE AOR                            
         BE    ASCL2G                                                           
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                  MUST BE ABLE TO SWITCH BACK                
*                                                                               
ASCL2G   DS    0H                                                               
         B     ASCUPD5        CON FND GO CHECK FOR PCATMAX                      
*                                                                               
ASCL2X   DS    0H                 HERE IF NO CONTRACT FOUND                     
*                                                                               
         OC    SADVDATA,SADVDATA      NEW ADV SYSTEM                            
         BZ    ASCL2X2             NO                                           
         TM    SVAORC,X'10'        ADV SCHEDULE CHECKING                        
         BZ    ASCL2X2                                                          
         CLC   SVAOR,AGYALPHA      SEE IF AM THE AOR                            
         BE    ASCL2X2                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                  MUST BE ABLE TO SWITCH BACK                
*                                                                               
ASCL2X2  DS    0H                                                               
*                                                                               
         CLI   SVCLPROF+12,C'N'    SEE IF CONTRACT REQUIRED                     
         BE    ASCX                                                             
         LA    R3,CONERR                                                        
         CLI   SVMED,C'N'       SEE IF NEWSPAPERS                               
         BNE   ASCX                                                             
*                               MUST CHECK IF SLIDING SCALE                     
*                               IF SO CLEAR R3 (ERROR) AND EXIT                 
         XC    KEY,KEY                                                          
         MVC   KEY(1),SVMED                                                     
         MVC   KEY+1(6),SAVBYPUB    AGENCY'S PUB                                
         MVC   KEY+7(2),AGYALPHA                                                
         MVI   KEY+9,X'81'                                                      
         BAS   RE,HIGHPUB                                                       
ASCL2X4A CLC   KEY(7),KEYSAVE      CHECK THROUGH PUB NUMBER                     
         BNE   ASCX                SHOULD NEVER HAPPEN                          
*                                  BUT DON'T DIE JUST EXIT                      
         CLC   KEY+7(3),KEYSAVE+7  CHK AGY                                      
         BE    ASCL2X8             HAVE FOUND A VALID PUB                       
*                                                                               
ASCL2X6  CLI   SVAPROF+16,C'0'                                                  
         BE    ASCL2X6B                                                         
         CLC   KEY+7(2),=C'ZZ'     SEE IF I FOUND DEFAULT                       
         BE    ASCL2X8                                                          
ASCL2X6B BAS   RE,SEQPUB                                                        
         B     ASCL2X4A                                                         
*                                                                               
ASCL2X8  DS    0H                                                               
         BAS   RE,GETPUB                                                        
         L     R2,APUBIO                                                        
         LA    R2,33(R2)                                                        
         MVI   ELCODE,X'20'       LOOK FOR PRODUCTION ELEM                      
         BAS   RE,ANEXTEL                                                       
         BNE   ASCX                                                             
         USING PUBGEND,R2                                                       
         CLI   PUBFLAT,C'S'      SEE IF SLIDING SCALE NEWSPAPER                 
         BNE   ASCX                                                             
         SR    R3,R3              CLEAR ERROR                                   
         B     ASCX                                                             
         DROP  R2                                                               
*                                                                               
ASCUPD5  L     R2,ACONIO                                                        
         LA    R2,33(R2)                                                        
         MVI   ELCODE,X'50'       LOOK FOR PCATELEM                             
         BAS   RE,ANEXTEL                                                       
         BNE   ASCX                                                             
         USING PCATELEM,R2                                                      
         OC    PCATMAX,PCATMAX                                                  
         BNZ   ASCUPD7                                                          
         OC    PCATMAXZ,PCATMAXZ                                                
         BZ    ASCX                                                             
*                                                                               
ASCUPD7  MVC   SCONPRD,PCONPRD        MUST SAVE PRODUCT                         
         MVC   SCONMAX,PCATMAX      AND MAXIMUM FOR ASC400                      
         MVC   SCONMAXZ,PCATMAXZ    AND MAXIMUM ACROSS Z/E FOR ASC400           
*                                                                               
         DROP  R2                                                               
         DROP  R6                                                               
*                                                                               
         TM    SVAORC,X'10'                                                     
         BZ    ASC60                                                            
*                                                                               
ASC10    DS    0H                                                               
         L     RE,ACONIO                                                        
         LHI   RF,6000                                                          
         XCEF                                                                   
*                                                                               
*        GOTO GETADVC AND RETURN LIST OF AGY/CLTS IN ACONIO                     
*                                                                               
*        MUST SWITCH TO CONTROL                                                 
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'0A'               CONTROL SYSTEM                          
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    ASC20                                                            
         LA    R3,CNTFERR  ** CONTROL SYSTEM NOT ACTIVE **'                     
         LA    R2,MBCMDIAH                                                      
         B     ASCX                                                             
*                                                                               
ASC20    DS    0H                                                               
         XC    WORK(20),WORK                                                    
         MVI   WORK,C'P'                                                        
         MVC   WORK+1(1),SVMED                                                  
         MVC   WORK+2(2),SVAOR                                                  
         MVC   WORK+4(3),SVADV                                                  
         GOTO1 =V(GETADVC),DMCB,WORK,ACONIO,VDATAMGR,RR=RELO01                  
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
ASC25    DS    0H                                                               
         L     R6,ACONIO                                                        
ASC27    CLC   0(2,R6),=X'FFFF'     END OF LIST                                 
         BE    ASC50X                                                           
*                                                                               
         USING GETADVCD,R6                                                      
         CLC   GETVAGY,AGYALPHA     SEE IF I AM THE AOR                         
         BE    ASC30                                                            
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),GETVSE                                                   
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    ASC30                                                            
         LA    R3,AGYFERR   *** AGENCY FILE NOT ACTIVE ***'                     
         LA    R2,MBCMDIAH                                                      
         B     ASCX                                                             
*                                                                               
ASC30    MVC   WKAGY,GETVAGY                                                    
         MVC   WKCLT,GETVACLT                                                   
         MVC   WKPUB,SAVBYPUB    BUY'S PUB                                      
         CLC   SVAOR,AGYALPHA    SEE IF I AM THE AOR                            
         BNE   ASC32                                                            
         TM    SVAORC,X'01'       SEE IF PUB LINK REQUIRED                      
         BZ    ASC32C                                                           
*                                                                               
ASC32    MVC   WKPUB,SVADVPUB                                                   
ASC32C   TM    GETVCNTL,X'01'    SEE IF PUB LINK                                
         BZ    ASC40                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'FD'                                                        
         MVC   KEY+1(1),SVMED                                                   
         MVC   KEY+2(3),SVADV    ADVERTISER                                     
         MVC   KEY+5(2),SVAOR                                                   
         MVC   KEY+7(2),WKAGY                                                   
         MVC   KEY+9(6),SVADVPUB    ADV PUB                                     
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ASC45        LINK NOT FOUND - SKIP                               
         MVC   WKPUB,KEY+15                                                     
*                                                                               
ASC40    BAS   RE,ASC400                                                        
         LTR   R3,R3                                                            
         BNZ   ASCX                                                             
*                                                                               
ASC45    DS    0H                                                               
         CLC   GETVAGY,AGYALPHA                                                 
         BE    ASC48                                                            
*                                                                               
*        MUST SWITCH BACK                                                       
ASC47    DS    0H                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
ASC48    LA    R6,GETVLEN(R6)                                                   
         B     ASC27                                                            
*                                                                               
ASC50X   DS    0H                                                               
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     ASCX                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
ASC60    DS    0H                 NON ADV PROCESSING                            
         MVC   WKAGY,AGYALPHA                                                   
         MVC   WKCLT,WORKCLT                                                    
         MVC   WKPUB,SAVBYPUB                                                   
*                                                                               
         BAS   RE,ASC400      FIRST CHECK FOR BUYS FOR THIS CLIENT              
         LTR   R3,R3          CHECK FOR ERROR                                   
         BNZ   ASCX                                                             
*                                                                               
         CLI   SVCLPROF+5,C'2'     SEE IF SLAVE CLIENT                          
         BNE   ASCX                NO THE DONE                                  
*                                                                               
*        MUST SEARCH THE OTHER SLAVES                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'02'                                                      
         BAS   RE,HIGH                                                          
         B     ASC66                                                            
ASC65    BAS   RE,SEQ                                                           
ASC66    CLC   KEY(4),KEYSAVE                                                   
         BNE   ASCX                                                             
         CLC   KEY+4(3),WORKCLT   SKIP THIS CLIENT                              
         BE    ASC65                                                            
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         BAS   RE,GETREC                                                        
         L     RF,AREC                                                          
         USING PCLTREC,RF                                                       
         CLI   PCLTPROF+5,C'2'      SEE IF SLAVE                                
         BNE   ASC65                                                            
         CLC   SVCLPROF+6(3),PCLTPROF+6                                         
         BNE   ASC65              CHECK FOR RIGHT MASTER                        
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   X(64),KEY         SAVE KEY AND KEYSAVE                           
         MVC   WKCLT,KEY+4                                                      
         BAS   RE,ASC400                                                        
         LTR   R3,R3             CHECK FOR ERROR                                
         BNZ   ASCX                                                             
*                                                                               
         MVC   KEY(64),X          RESTORE KEY AND KEYSAVE                       
         BAS   RE,HIGH                                                          
         B     ASC65                                                            
*                                                                               
ASCX     XIT1  REGS=(R3)                                                        
*                                                                               
*                                                                               
ANEXTEL  DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    ANEXTELX                                                         
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     ANEXTEL                                                          
*                                                                               
ANEXTELX LTR   RE,RE                                                            
         BR    RE                  SET CONDITION CODE NE                        
         EJECT                                                                  
ASC400   NTR1                   LOOK FOR BUYS AND COUNT IN R4                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),WKAGY                                                     
         MVC   KEY+2(1),SVMED           MEDIA                                   
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+4(3),WKCLT           CLIENT CODE                             
         MVC   KEY+7(4),WKPUB           BASE PUB                                
*                                                                               
         OC    SCONMAXZ,SCONMAXZ     SEE IF CHECHING ACROSS ZONES/EDT           
         BNZ   *+10                                                             
         MVC   KEY+11(2),WKPUB+4       ADD ZONE AND EDITION                     
*                                                                               
         CLI   SCONPRD,C'A'             CHK FOR PRODUCT CONTRACT                
         BL    *+10                                                             
         MVC   KEY+13(3),SCONPRD        ONLY SEARCH FOR THIS PRODUCT            
         BAS   RE,HIGH                                                          
         B     ASC406                                                           
ASC405   BAS   RE,SEQ                                                           
*                                                                               
ASC406   CLC   KEY(11),KEYSAVE     CHECK AGY/MED/CLT/BASE PUB                   
         BNE   ASC400X                                                          
         OC    SCONMAXZ,SCONMAXZ    SEE IF CHECKING ACROSS ZONES/EDTS           
         BNZ   ASC406C                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ASC400X                                                          
*                                                                               
ASC406C  DS    0H                                                               
         CLI   SCONPRD,C'A'                                                     
         BL    ASC408                                                           
         CLC   KEY+13(3),KEYSAVE+13   CHECK RIGHT PRODUCT                       
         BE    ASC408                                                           
*                                                                               
         OC    SCONMAXZ,SCONMAXZ    SEE IF CHECKING ACROSS ZONES/EDTS           
         BZ    ASC400X             NO - THEN DONE                               
         B     ASC405              CONTINUE                                     
*                                                                               
ASC408   CLC   KEY+16(3),SAVBYDAT     MUST MATCH DATES                          
         BNE   ASC405                                                           
         TM    KEY+25,X'80'        TEST DELETED                                 
         BNZ   ASC405                                                           
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         BAS   RE,GETREC                                                        
         L     R6,AREC                                                          
*                                                                               
         CLI   PBDBFD-PBUYREC(R6),C'T'     SKIP TEST BUYS                       
         BE    ASC405                                                           
         CLI   PBDSPACE-PBUYREC(R6),C'*'   MUST SEE IF REAL INSERTION           
         BE    ASC405               SKIP                                        
*                                                                               
         CLI   PBDSPACE-PBUYREC(R6),C'#'   SEE IF TO BE IGNORE FOR ASC          
         BE    ASC405               CHECKING - SKIP                             
*                                                                               
         CLI   2(R6),C'O'          SEE IF OUTDOOR                               
         BNE   ASC420                                                           
         CP    PBDSHOW-PBUYREC(R6),=P'0'                                        
         BNE   ASC420                                                           
*                                                                               
         LA    R2,33(R6)                                                        
         MVI   ELCODE,X'66'                                                     
         BAS   RE,ANEXTEL                                                       
         BNE   ASC420                                                           
         CLI   1(R2),19                                                         
         BH    ASC420                                                           
         CLI   2(R2),C'*'                                                       
         BE    ASC405                                                           
*                                                                               
         CLI   2(R2),C'#'      SEE IF TO BE IGNORED FOR ASC CHECK               
         BE    ASC405                                                           
*                                                                               
ASC420   LH    R4,ZECNT                                                         
         LA    R4,1(R4)                                                         
         STH   R4,ZECNT                                                         
         OC    SCONMAXZ,SCONMAXZ      SEE IF I HAVE A MAX ACROSS Z/E            
         BZ    ASC425                                                           
         CLC   ZECNT,SCONMAXZ                                                   
         BNL   ASC430                                                           
*                                                                               
ASC425   CLC   KEY+11(2),WKPUB+4      SEE IF "RIGHT" ZONE/EDT                   
         BNE   ASC405                                                           
         LH    R4,PUBCNT                                                        
         LA    R4,1(R4)                                                         
         STH   R4,PUBCNT                                                        
*                                                                               
         OC    SCONMAX,SCONMAX     SEE IF CHECK MAX/ISSUE - THIS PUB            
         BZ    ASC405                                                           
*                                                                               
         CLC   PUBCNT,SCONMAX                                                   
         BL    ASC405                                                           
*                                                                               
ASC430   LA    R3,CONMERR     CONTRACT MAX BUYS PER ISSUE REACHED *'            
         B     ASC400XX                                                         
*                                                                               
ASC400X  DS    0H                     HERE AT END OF BUY READING                
*                                     MUST STILL CHECK VS MAXIMUMS              
*                                     SINCE PUBCNT AND ZECNT MAY HAVE           
*                                     BEEN SET FROM PUBDTTAB                    
*                                                                               
         OC    SCONMAXZ,SCONMAXZ      SEE IF I HAVE A MAX ACROSS Z/E            
         BZ    ASC400X5                                                         
         CLC   ZECNT,SCONMAXZ                                                   
         BNL   ASC430                                                           
*                                                                               
ASC400X5 OC    SCONMAX,SCONMAX      MAX/ISSUE                                   
         BZ    ASC400XX                                                         
         CLC   PUBCNT,SCONMAX                                                   
         BNL   ASC430                                                           
*                                                                               
ASC400XX XIT1  REGS=(R3,R4)                                                     
*                                                                               
FNDAPUB  NTR1                                                                   
         MVC   X(32),KEY                                                        
         XC    SVADVPUB,SVADVPUB                                                
         TM    SVAORC,X'01'         PUB LINK REQUIRED (NEW ADV)                 
         BZ    FNDA9                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'FE'                                                        
         MVC   KEY+1(1),SVMED            MEDIA                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(3),SVADV                                                   
         MVC   KEY+7(2),SVAOR                                                   
         MVC   KEY+9(6),SAVBYPUB                                                
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(15),KEYSAVE                                                  
         BE    FNDA5                                                            
         LA    R3,PUBCLTER                                                      
*                                                                               
FNDA5    DS    0H                                                               
         MVC   SVADVPUB,KEY+15          SAVE ADV PUB NUMBER                     
*                            NEEDED TO READ ADVERTISER CONTRACT                 
*                                                                               
FNDA9    DS    0H                                                               
         MVC   KEY(32),X                                                        
         XIT1  REGS=(R3)                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                             COMPETITIVE BRAND CHECKING                        
COM      CSECT                                                                  
         NMOD1 0,COM                                                            
*                                                                               
*                             *** DON'T TOUCH R8 IN ANY CSECT ***               
*                             *** IT IS NEEDED TO ACCESS ROUTINES               
*                             *** IN THE MAIN PROGRAM                           
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T418FFD,RA                                                       
*                                                                               
         SR    R3,R3                 CLEAR ERROR CODE                           
*                                                                               
         XC    SVPADJCS,SVPADJCS     CLEAR                                      
*                                                                               
*        FIRST READ PRODUCT TO SEE IF IT HAS ANY ADJACENCY CODES                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),SAVBYCLT                                                
         MVC   KEY+7(3),SAVBYPRD                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'               PRODUCT MUST BE FOUND                         
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         BAS   RE,GETREC                                                        
         MVC   SVPADJCS,PPRDEXCL                                                
*                                                                               
COM10    DS    0H                                                               
         OC    SVPADJCS,SVPADJCS    CHECK FOR ADJACENCY CODES                   
         BZ    COMX                 IF NONE JUST EXIT                           
*                                                                               
         OC    SADVDATA,SADVDATA   SEE IF NEW ADVERTISER SYSTEM                 
         BZ    COM60           USE NORMAL CHECKING                              
*                                                                               
         L     RE,ACONIO                                                        
         LHI   RF,6000                                                          
         XCEF                                                                   
*                                                                               
*                                                                               
*        GOTO GETADVC AND RETURN LIST OF AGY/CLTS IN WRKREC                     
*                                                                               
*        MUST SWITCH TO CONTROL                                                 
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'0A'               CONTROL SYSTEM                          
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    COM20                                                            
         LA    R3,CNTFERR  ** CONTROL SYSTEM NOT ACTIVE **'                     
         LA    R2,MBCMDIAH                                                      
         B     ERROR                                                            
*                                                                               
COM20    MVI   WORK,C'P'                                                        
         MVC   WORK+1(1),SVMED                                                  
         MVC   WORK+2(2),SVAOR                                                  
         MVC   WORK+4(3),SVADV                                                  
         XC    WORK+7(2),WORK+7                                                 
         GOTO1 =V(GETADVC),DMCB,WORK,ACONIO,VDATAMGR,RR=RELO01                  
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
COM25    DS    0H                                                               
         L     R6,ACONIO                                                        
COM27    CLC   0(2,R6),=X'FFFF'     END OF LIST                                 
         BE    COM50X                                                           
*                                                                               
         USING GETADVCD,R6                                                      
         CLC   GETVAGY,AGYALPHA     SEE IF I'M PROCESSING MYSELF                
         BE    COM30                                                            
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),GETVSE                                                   
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    COM30                                                            
         LA    R3,AGYFERR   *** AGENCY FILE NOT ACTIVE ***'                     
         LA    R2,MBCMDIAH                                                      
         B     ERROR                                                            
*                                                                               
COM30    MVC   WKAGY,GETVAGY                                                    
         MVC   WKCLT,GETVACLT                                                   
         MVC   WKPUB,SAVBYPUB      SET TO SAVBYPUB                              
         CLC   SVAOR,AGYALPHA      SEE IF I AM THE AOR                          
         BNE   COM32                                                            
         TM    SVAORC,X'01'        SEE IF PUB LINK REQUIRED                     
         BZ    COM32C                                                           
COM32    MVC   WKPUB,SVADVPUB      SET TO ADV PUB                               
COM32C   TM    GETVCNTL,X'01'    SEE IF PUB LINK                                
         BZ    COM40                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'FD'                                                        
         MVC   KEY+1(1),SVMED                                                   
         MVC   KEY+2(3),SVADV    ADVERTISER                                     
         MVC   KEY+5(2),SVAOR                                                   
         MVC   KEY+7(2),WKAGY                                                   
         MVC   KEY+9(6),WKPUB       FIND LINK FOR WKPUB                         
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(15),KEYSAVE                                                  
         BNE   COM45        LINK NOT FOUND - SKIP                               
         MVC   WKPUB,KEY+15                                                     
*                                                                               
COM40    BAS   RE,COM400                                                        
         LTR   R3,R3                                                            
         BNZ   COMX                                                             
*                                                                               
COM45    DS    0H                                                               
         CLC   GETVAGY,AGYALPHA        SEE IF I/M PROCESSING MYSELF             
         BE    COM48                                                            
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
COM47    L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
COM48    LA    R6,GETVLEN(R6)                                                   
         B     COM27                                                            
*                                                                               
COM50X   DS    0H                                                               
         B     COMX                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
COM60    DS    0H                 NON-ADV PROCESSING                            
*                                                                               
         MVC   WKAGY,AGYALPHA                                                   
         MVC   WKCLT,WORKCLT                                                    
         MVC   WKPUB,SAVBYPUB                                                   
*                                                                               
         BAS   RE,COM400      FIRST CHECK FOR BUYS FOR THIS CLIENT              
         LTR   R3,R3                                                            
         BNZ   COMX                                                             
*                                                                               
         CLI   BYPROF+2,C'Y'       SEE IF CHECKING SLAVES                       
         BNE   COMX                                                             
*                                                                               
         CLI   SVCLPROF+5,C'2'     SEE IF SLAVE CLIENT                          
         BNE   COMX                NO THE DONE                                  
*                                                                               
*        MUST SEARCH THE OTHER SLAVES                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'02'                                                      
         BAS   RE,HIGH                                                          
         B     COM66                                                            
COM65    BAS   RE,SEQ                                                           
COM66    CLC   KEY(4),KEYSAVE                                                   
         BNE   COMX                                                             
         CLC   KEY+4(3),WORKCLT   SKIP THIS CLIENT                              
         BE    COM65                                                            
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         BAS   RE,GETREC                                                        
         L     RF,AREC                                                          
         USING PCLTREC,RF                                                       
         CLI   PCLTPROF+5,C'2'      SEE IF SLAVE                                
         BNE   COM65                                                            
         CLC   SVCLPROF+6(3),PCLTPROF+6                                         
         BNE   COM65              CHECK FOR RIGHT MASTER                        
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   X(64),KEY         SAVE KEY AND KEYSAVE                           
         MVC   WKCLT,KEY+4                                                      
         BAS   RE,COM400                                                        
         LTR   R3,R3             CHECK FOR ERROR                                
         BNZ   COMX                                                             
*                                                                               
         MVC   KEY(64),X          RESTORE KEY AND KEYSAVE                       
         BAS   RE,HIGH                                                          
         B     COM65                                                            
*                                                                               
*                                                                               
COMX     DS    0H                                                               
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         XIT1  REGS=(R3)                                                        
*                                                                               
CNEXTEL  DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    CNEXTELX                                                         
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     CNEXTEL                                                          
*                                                                               
CNEXTELX LTR   RE,RE                                                            
         BR    RE                  SET CONDITION CODE NE                        
         EJECT                                                                  
*              THIS ROUNTINE BUILDS A TABLE OF PRODUCTS                         
*              (READING X'A6' POINTERS)                                         
*              THAT HAVE A PPRDEXCL CODE THAT MATCHES                           
*              THE PRODUCT I'M PROCESSING                                       
*              IT THEN CHECKS FOR BUYS FOR THOSE PRODUCTS FOR                   
*              THE SAME PUB AND DATE                                            
*              AND RETURNS AN ERROR IF ONE IS FOUND                             
COM400   NTR1                                                                   
         L     RE,APRDTAB                                                       
         LH    RF,=Y(L'PRDTAB)                                                  
         XCEF                                                                   
*                                                                               
         LA    R3,SVPADJCS            PRODUCT ADJANCENY CODES                   
         LA    R5,3          FOR BCT                                            
*                                     (SAVED FROM PRODUCT IN PPBUY01)           
         L     R6,APRDTAB                                                       
*                                                                               
COM400A  CLI   0(R3),C' '             SEE IF CODE PRESENT                       
         BNH   COM400U                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),WKAGY                                                     
         MVC   KEY+2(1),SVMED           MEDIA                                   
         MVI   KEY+3,X'A6'                                                      
         MVC   KEY+4(3),WKCLT           CLIENT CODE                             
         MVC   KEY+7(1),0(R3)                                                   
         BAS   RE,HIGH                                                          
         B     COM400T                                                          
*                                                                               
COM400S  BAS   RE,SEQ                                                           
*                                                                               
COM400T  CLC   KEY(8),KEYSAVE        CHECK THROUGH CODE                         
         BNE   COM400U                                                          
         CLC   KEY+8(3),SAVBYPRD     SKIP THIS PRODUCT                          
         BE    COM400S                                                          
         MVC   0(3,R6),KEY+8          SAVE PRODUCT CODE                         
         LA    R6,3(R6)                                                         
         B     COM400S                                                          
*                                                                               
COM400U  LA    R3,1(R3)                                                         
         BCT   R5,COM400A                                                       
*                                                                               
COM400X  DS    0H                                                               
         SR    R3,R3                  CLEAR ERROR                               
*                                                                               
         L     R6,APRDTAB                                                       
         OC    0(3,R6),0(R6)         SEE IF I FOUND ANY PRODUCTS                
         BZ    COM400XX                                                         
*                                                                               
COM402   DS    0H                                                               
         L     R4,APRDTAB                                                       
COM402A  CLI   0(R4),0                SEE IF AT END OF LIST                     
         BE    COM400XX                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),WKAGY                                                     
         MVC   KEY+2(1),SVMED           MEDIA                                   
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+4(3),WKCLT           CLIENT CODE                             
         MVC   KEY+7(6),WKPUB           USE FULL PUB                            
*                                                                               
         MVC   KEY+13(3),0(R4)          ONLY SEARCH FOR THIS PRODUCT            
         BAS   RE,HIGH                                                          
         B     COM406                                                           
COM405   BAS   RE,SEQ                                                           
*                                                                               
COM406   CLC   KEY(16),KEYSAVE     CHECK AGY/MED/CLT/FULL PUB/PRD               
         BE    COM408                                                           
         B     COM460              GO TRY NEXT PRODUCT                          
*                                                                               
COM408   CLC   KEY+16(3),SAVBYDAT   MUST MATCH DATES                            
         BNE   COM405                                                           
         TM    KEY+25,X'80'        TEST DELETED                                 
         BNZ   COM405                                                           
         MVC   AREC,AWRKREC         MUST READ BUY                               
         BAS   RE,GETREC                                                        
         LA    R0,REC                                                           
         ST    R0,AREC               RESET AREC TO REC                          
         L     R6,AWRKREC                                                       
*                                                                               
         CLI   BYPROF+3,C'Y'        SEE IF SKIPPING TEST BUY CHECK              
         BE    COM409                                                           
*                                                                               
         CLI   PBDBFD-PBUYREC(R6),C'T'     TEST BUY                             
         BE    COM405                                                           
*                                                                               
COM409   DS    0H                                                               
         CLI   PBDSPACE-PBUYREC(R6),C'*'   MUST SEE IF REAL INSERTION           
         BE    COM405               SKIP                                        
*                                                                               
         CLI   PBDSPACE-PBUYREC(R6),C'#'   SEE IF BUY IS TO BE IGNORED          
         BE    COM405               FOR SCHEDULE CHECKING                       
*                                                                               
         CLI   2(R6),C'O'         SEE IF OUTDOOR                                
         BNE   COM410                                                           
         CP    PBDSHOW-PBUYREC(3,R6),=P'0'                                      
         BNE   COM410                                                           
         LA    R2,33(R6)                                                        
         MVI   ELCODE,X'66'                                                     
         BAS   RE,CNEXTEL                                                       
         BNE   COM410                                                           
         CLI   1(R2),19                                                         
         BH    COM410                                                           
         CLI   2(R2),C'*'                                                       
         BE    COM405            SKIP THIS BUY                                  
*                                                                               
         CLI   2(R2),C'#'        SEE IF THEI BUY IS TO BE IGNORED               
         BE    COM405            FOR SCHEDULE CHECKING                          
*                                                                               
COM410   DS    0H                                                               
*                                                                               
COM430   LA    R3,COMPERR     COMPETITIVE BRAND ERROR                           
         B     COM400XX                                                         
*                                                                               
COM460   DS    0H                                                               
         LA    R4,3(R4)        BUMP TO NEXT PRODUCT (IN PRDTAB)                 
         B     COM402A                                                          
*                                                                               
COM400XX XIT1  REGS=(R3)                                                        
         SPACE 2                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                    ALLOCATED PRODUCTS ESTIMATE CHECKING ("ZZZ" BUYS)          
PEC      CSECT                                                                  
         NMOD1 0,PEC                                                            
*                                                                               
***                                                                             
***      NOTE - DO NOT USE R8 OR R9 IN THIS CSECT                               
***      THEY ARE NEEDED TO COVER ROUTINES IN THE MAIN CSECT                    
***                                                                             
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T418FFD,RA                                                       
*                                                                               
*             FIRST CHECK FOR PRODUCT ELEMENTS (X'21') IN BUY                   
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'21'        PPRELEM                                      
PEC10    BAS   RE,PNEXTEL                                                       
         BNE   PECX                RETURN - NO ERROR FOUND                      
*                                                                               
*             ELEMENT FOUND - CHECK ESTIMATE FOR PRODUCT IN ELEMENT             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'07'         ESTIMATE RECORD CODE                         
         MVC   KEY+4(3),SAVBYCLT                                                
         MVC   KEY+7(3),2(R2)      PRODUCT CODE FROM PPRELEM                    
         MVC   KEY+10(2),SAVBYEST                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'               ESTIMATE MUST BE FOUND                        
*                                                                               
         MVC   AREC,AWRKREC                                                     
         BAS   RE,GETREC           READ ESTIMATE INTO WRKREC                    
         LA    R0,REC              "RESTORE" AREC                               
         ST    R0,AREC                                                          
         L     R4,AWRKREC                                                       
         USING PESTREC,R4                                                       
         TM    PESTTEST,X'80'      TEST ESTIMATE ?                              
         BNO   PEC10               NO - CHECK NEXT PPRELEM                      
*                                                                               
*                                   YES - ERROR                                 
         XC    MBCEMSG,MBCEMSG                                                  
         MVC   MBCEMSG(56),=C'** INVALID - TEST ESTIMATE FOUND FOR ALLOX        
               CATED PRODUCT ='                                                 
         MVC   MBCEMSG+57(3),PESTKPRD          PRD CODE FROM EST REC            
         FOUT  MBCEMSGH                                                         
         MVI   CHASW,C'X'          ERROR ON ZZZ MAKELIVE FOUND                  
*                                                                               
         DROP  R4                                                               
*                                                                               
PECX     DS    0H                                                               
         XMOD1 1                                                                
*                                                                               
PNEXTEL  DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    PNEXTELX                                                         
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     PNEXTEL                                                          
*                                                                               
PNEXTELX LTR   RE,RE                                                            
         BR    RE                  SET CONDITION CODE NE                        
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
PUBGEND  DSECT                                                                  
       ++INCLUDE PUBGENEL                                                       
         EJECT                                                                  
PTSHTELD DSECT                                                                  
       ++INCLUDE PTSHTEL                                                        
*                                                                               
       ++INCLUDE PPMBCWRK                                                       
*                                                                               
