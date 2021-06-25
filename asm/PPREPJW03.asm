*          DATA SET PPREPJW03  AT LEVEL 112 AS OF 05/01/02                      
*PHASE PPJW02C,*   ****** NOTE PHASE NAME = PPJW02C *****                       
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE QSORT                                                                  
         SPACE 2                                                                
****************************************************************                
*                                                              *                
* PROGRAM PROCESSES A SORTED VERSION OF HISTORY AND ESTIMATE   *                
* TAPES, WITH A 27 BYTE SORT KEY PRECEDING THE ORIGINAL RECORD *                
* THE SORT SEQUENCE IS                                         *                
*                                                              *                
*    MED/CLT/PRD/VENDOR/DATE/EST/SEQ NUM/FILE CODE             *                
*                                                              *                
*** SORTED FILE IS PRESUMED TO CONTAIN 87/88 DATA ONLY ***     *                
*                                                              *                
* TABLES PROVIDED BY JWT PRIOR TO CONVERSION ---               *                
*                                                              *                
*   CPETAB - USED TO CONVERT CLT/PRD/EST CODES                 *                
*   PUBTAB - USED TO CONVERT PUBLICATION NUMBERS               *                
*                                                              *                
* INTERNAL TABLES CONSTRUCTED AT RUN TIME ---                  *                
*                                                              *                
*   CPELST - LIST OF ALL VALID CLIENTS/PRODUCTS/ESTIMATES      *                
*   PUBLST - LIST OF ALL VALID PUBLICATIONS                    *                
*                                                              *                
****************************************************************                
PPJW03   TITLE 'JWT BUY DETAIL CONVERSION PROGRAM'                              
         SPACE 1                                                                
PPJW03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPJW03                                                         
*                                                                               
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING PPJW03+4096,RA                                                   
*                                                                               
         L     R9,0(R1)                                                         
         USING PPWORKD,R9                                                       
*                                                                               
         L     RC,PPFILEC                                                       
         LR    R8,RC                                                            
         AH    R8,=H'4096'                                                      
         USING PPFILED,RC,R8                                                    
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BE    JW10                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
         SPACE 1                                                                
JW10     DS    0H                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         OPEN  (FILEOUT,(OUTPUT))                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         LOAD  EPLOC=JWCPETAB                                                   
         LR    RE,R0                                                            
         MVC   MYCPEPRM(24),0(RE)                                               
*                                                                               
         LOAD  EPLOC=JWESTTAB                                                   
         LR    RE,R0                                                            
         MVC   MYESTPRM(24),0(RE)                                               
*                                                                               
         LOAD  EPLOC=JWPUBTAB                                                   
         LR    RE,R0                                                            
         MVC   MYPUBPRM(24),0(RE)                                               
         B     JW11                                                             
*                                                                               
JWCPETAB DC    CL8'JWCPETAB'                                                    
JWESTTAB DC    CL8'JWESTTAB'                                                    
JWPUBTAB DC    CL8'JWPUBTAB'                                                    
*                                                                               
JW11     DS    0H                                                               
         GOTO1 =A(FIXEDT),DMCB,(RC)   FIX CONV TABLE EDITION CODES              
*                                                                               
         GOTO1 =A(BLDCPE),DMCB,(RC) BUILD CLT/PRD/EST LIST                      
*                                                                               
         CLC   =C'MAX=',QUESTOR                                                 
         BNE   JW13                                                             
         SR    RE,RE                                                            
         LA    R1,QUESTOR+4                                                     
JW12A    CLI   0(R1),C'0'                                                       
         BL    JW12X                                                            
         CLI   0(R1),C'9'                                                       
         BH    JW12X                                                            
         LA    R1,1(R1)                                                         
         BCT   RE,JW12A                                                         
*                                                                               
JW12X    LPR   RE,RE                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  MAXIN,QUESTOR+4(0)                                               
         B     JW14                                                             
*                                                                               
JW13     CLC   =C'SKIP=',QUESTOR                                                
         BNE   JW14                                                             
         SR    RE,RE                                                            
         LA    R1,QUESTOR+5                                                     
JW13A    CLI   0(R1),C'0'                                                       
         BL    JW13X                                                            
         CLI   0(R1),C'9'                                                       
         BH    JW13X                                                            
         LA    R1,1(R1)                                                         
         BCT   RE,JW13A                                                         
*                                                                               
JW13X    LPR   RE,RE                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  SKIPCNT,QUESTOR+5(0)                                             
         B     JW14                                                             
*                                                                               
JW14     XC    PBUYKEY,PBUYKEY     CLEAR TOP OF BUYREC                          
*                                                                               
         CLI   QOPT2,C'X'          TEST SUPPRESS PUB TABLE                      
         BE    JW15                                                             
         GOTO1 =A(BLDPUBS),DMCB,(RC)  BUILD PUBFILE LIST                        
*                                                                               
JW15     LA    R7,JWNEXT                                                        
         B     JW21X                                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,25,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2000'                                  
         EJECT                                                                  
JW20     DS    0H                                                               
JW21     DS    0H                                                               
         CLI   PBUYKEY,0           TEST RECORD IN OUTPUT BUFFER                 
         BNE   JW21A               YES                                          
* IF NO RECORD, SOME ERROR COUNTER MUST CHANGE                                  
         CP    CLTERRS,XCLTERR                                                  
         BNE   JW21X                                                            
         CP    PRDERRS,XPRDERR                                                  
         BNE   JW21X                                                            
         CP    BADESTS,XBADEST                                                  
         BNE   JW21X                                                            
         CP    RATERRS,XRATERR                                                  
         BNE   JW21X                                                            
         DC    H'0'                                                             
*                                                                               
JW21A    CLI   QOPT1,C'Y'          TEST TRACE ON                                
         BNE   JW21B                                                            
         MVC   P(7),=C'JWTRACE'                                                 
         LA    R1,P+10                                                          
         BAS   RE,JWTRACE          PRINT JW REC TRACE                           
         BAS   RE,DDSTRACE         PRINT DDS RECORD TRACE                       
*                                                                               
JW21B    LA    R1,PBUYREC          POINT TO RECORD                              
         BAS   RE,PUTSORT          AND PUT TO SORT                              
*                                                                               
JW21X    LA    R0,8                                                             
         LA    R1,PBUYREC                                                       
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         ZAP   XCLTERR,CLTERRS                                                  
         ZAP   XPRDERR,PRDERRS                                                  
         ZAP   XBADEST,BADESTS                                                  
         ZAP   XRATERR,RATERRS                                                  
*                                                                               
         BAS   RE,GETFILE          GET NEXT FILE RECORD                         
         CLI   EOFSW,C'Y'                                                       
         BE    JW400                                                            
*                                                                               
         LA    R7,JWNEXT                                                        
         USING JWREC,R7                                                         
*                                                                               
         EJECT                                                                  
* CREATE A NEW BUY RECORD *                                                     
         SPACE 1                                                                
JW24     DS    0H                                                               
         MVC   PBUYKAGY(2),=C'JW'  MOVE AGENCY                                  
         MVC   PBUYKMED,JWMEDIA                                                 
         CLI   PBUYKMED,C'I'                                                    
         BNE   *+8                                                              
         MVI   PBUYKMED,C'S'                                                    
* CONVERT CLT/PRD CODES                                                         
         XC    WORK,WORK                                                        
         MVC   WORK(4),JWCLT                                                    
         MVC   WORK+4(3),JWPRD                                                  
         GOTO1 BINSRCH,MYCPEPRM,WORK                                            
         CLI   0(R1),X'01'                                                      
         BNE   JW26                                                             
         BAS   RE,PRDERR                                                        
         XC    PBUYKEY,PBUYKEY                                                  
         B     JW20                                                             
*                                                                               
JW26     L     RE,0(R1)            POINT TO ENTRY                               
         USING CPETABD,RE                                                       
         MVC   PBUYKCLT,CPEDDCLT   MOVE NEW CLT                                 
         MVC   PBUYKPRD,CPEDDPRD            PRD                                 
         DROP  RE                                                               
         SPACE 1                                                                
*===============================*                                               
* SEE IF PUB IS TO BE CONVERTED *                                               
*===============================*                                               
         SPACE 1                                                                
         MVI   PBUYKRCD,X'20'                                                   
         PACK  DUB,JWVENDOR(6)                                                  
         MVC   PBUYKPUB(4),DUB+3   SET PWOS PUB                                 
         XC    WORK,WORK                                                        
         MVC   WORK(1),PBUYKMED                                                 
         MVC   WORK+1(4),PBUYKPUB                                               
         GOTO1 BINSRCH,MYPUBPRM,WORK                                            
         CLI   0(R1),X'01'                                                      
         BE    JW27                                                             
         L     RE,0(R1)            POINT TO ENTRY                               
         USING PBTABD,RE                                                        
         MVC   PBUYKMED,PBTDDMED      SET POSSIBLE NEW MEDIA CODE               
         MVC   PBUYKPUB(6),PBTDDPUB   SET CONVERTED PUB IN RECORD               
         DROP  RE                                                               
*                                                                               
JW27     XC    WORK,WORK                                                        
         MVC   WORK(6),PBUYKCLT    MOVE DDS CLT/PRD                             
         MVC   WORK+6(6),JWEST     MOVE JWT ESTIMATE                            
         CLC   JWEST,=C'000000'                                                 
         BNE   JW27A                                                            
         PACK  DUB,JWINSDAT(2)      USE INSERTION YEAR AS EST                   
         CVB   R0,DUB                                                           
         STCM  R0,3,PBUYKEST                                                    
         B     JW28                                                             
*                                                                               
JW27A    CLI   JWMEDIA,C'I'                                                     
         BNE   *+10                                                             
         MVC   WORK+7(1),PBUYKMED   USE NEW MEDIA CODE (THANKS LISA)            
         GOTO1 BINSRCH,MYESTPRM,WORK                                            
         CLI   0(R1),X'01'         TEST NOT FOUND                               
         BNE   JW27B                                                            
         CLI   WORK+11,C'A'        IF LAST CHAR ALPHA, TRY AGAIN                
         BL    JW27ERR                                                          
         CLI   WORK+11,C'Z'                                                     
         BH    JW27ERR                                                          
         MVI   WORK+11,C' '                                                     
         B     JW27A                                                            
JW27ERR  BAS   RE,BADEST                                                        
         XC    PBUYKEY,PBUYKEY     SUPPRESS BUYREC                              
         B     JW20                                                             
*                                                                               
JW27B    L     RE,0(R1)                                                         
         USING ESTTABD,RE                                                       
         PACK  DUB,ESTDDEST                                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,PBUYKEST                                                    
         DROP  RE                                                               
         EJECT                                                                  
*===================================*                                           
* NOW MAKE SURE ESTIMATE IS ON FILE *                                           
*===================================*                                           
         SPACE 1                                                                
JW28     OC    CPELSCNT,CPELSCNT   TEST LIST BUILT                              
         BZ    JW30                NO - SKIP                                    
         XC    WORK,WORK                                                        
         MVC   WORK(1),PBUYKMED                                                 
         MVC   WORK+1(3),PBUYKCLT                                               
         MVC   WORK+4(3),PBUYKPRD                                               
         MVC   WORK+7(2),PBUYKEST                                               
         GOTO1 BINSRCH,CPELSPRM,WORK                                            
         CLI   0(R1),X'01'                                                      
         BNE   JW30                                                             
         BAS   RE,ESTERR           DON'T STOP ON EST NOT FOUND                  
         SPACE 1                                                                
* NOW MAKE SURE PUB IS ON FILE *                                                
         SPACE 1                                                                
JW30     OC    PBLSTCNT,PBLSTCNT   TEST PUB LIST SUPPRESSED                     
         BZ    JW32                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),PBUYKMED                                                 
         MVC   WORK+1(6),PBUYKPUB                                               
         GOTO1 BINSRCH,PBLSTPRM,WORK                                            
         CLI   0(R1),X'01'                                                      
         BNE   JW32                                                             
         BAS   RE,PUBERR           DON'T STOP ON PUB ERR                        
*                                                                               
JW32     DS    0H                                                               
         GOTO1 DATCON,DMCB,JWINSDAT,(3,PBUYKDAT)                                
         CLI   PBUYKDAT+2,0                                                     
         BNE   *+12                                                             
         MVI   PBUYKDAT+2,1                                                     
         MVI   PBDFREQ,C'M'                                                     
*                                                                               
         MVI   PBUYLEN+1,33+116     KEY+CNTL+PBDELEM                            
*                                                                               
         LA    R6,PBDELEM                                                       
         MVI   PBDELEM,X'20'                                                    
         MVI   PBDELEM+1,116                                                    
*                                                                               
         MVC   PBDBUYDT,=X'010101'  SET VISIBLE CREATION DATE                   
         ZAP   PBDCD,JWDISCRT                                                   
         ZAP   DUB,JWTAXRT         GET TAX RATE                                 
         MP    DUB,=P'100'         CORRECT DECIMAL PLACES                       
         CVB   R0,DUB                                                           
*                                                                               
         CLI   JWTYPE,C'T'                                                      
         BNE   *+6                                                              
         SR    R0,R0                                                            
         STCM  R0,7,PBDTAX                                                      
*                                                                               
         CLC   JWEST+2(4),=C'9999'                                              
         BNE   *+8                                                              
         MVI   PBDBFD,C'T'         SET TEST BUY IND FOR EST 999999              
         SPACE 1                                                                
* MAKE SURE ALL PACKED FIELDS ARE PACKED                                        
         SPACE 1                                                                
         ZAP   PBDUNITS,=P'0'                                                   
         ZAP   PBDCOS,=P'0'                                                     
         MVI   PBDCOSIN,C' '                                                    
         ZAP   PBDPRCOS,=P'0'                                                   
         ZAP   PBDCLMS,=P'0'                                                    
*                                                                               
         CLC   =C'00',JWINSDT2     TEST FOR SECOND INSERTION DATE               
         BE    JW36                NO                                           
         CLI   JWINSDT2,C'Z'       TEST ALPHA                                   
         BH    JW34                NO                                           
         CLI   JWINSDT2,C'A'                                                    
         BL    JW36                DO NOT MOVE NON-ALPHA                        
         MVC   PBDBFD,JWINSDT2                                                  
         B     JW36                                                             
*                                                                               
JW34     MVC   PBDIDAT2,PBUYKDAT   NUMERIC- TREAT AS SECOND DATE                
         PACK  DUB,JWINSDT2                                                     
         CVB   R0,DUB                                                           
         STC   R0,PBDIDAT2+2                                                    
*                                                                               
JW36     MVC   PBDSPACE,JWSPACE                                                 
         LA    R6,PBDELEM                                                       
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         MVI   0(R6),X'99'                                                      
         MVI   1(R6),29                                                         
         BAS   RE,SETLEN                                                        
         MVC   2(27,R6),JWSRTKEY                                                
*                                                                               
         CLC   JWCOMMNT,SPACES                                                  
         BNH   JW38                                                             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         MVI   0(R6),X'66'         CREATE COMMENT ELEMENT                       
         MVI   1(R6),26                                                         
         BAS   RE,SETLEN                                                        
         MVC   2(24,R6),JWCOMMNT                                                
         GOTO1 =V(SQUASHER),DMCB,2(R6),24                                       
*                                                                               
JW38     DS    0H                                                               
         BAS   RE,GETBPDTS         SET BILLABLE/PAYABLE DATES                   
*                                                                               
         BAS   RE,GETDOLS          GET GROSS/NET                                
         BE    JW38X                                                            
         XC    PBUYKEY,PBUYKEY                                                  
         B     JW20                                                             
*                                                                               
JW38X    CLI   JWSRTFIL,C'E'        TEST RECORD FROM EST FILE                   
         BNE   JW38Z                NO BILL/PAY DATA IN REC                     
         ZIC   R0,1(R6)            SO JUST ADD EMPTY ELEMENTS                   
         AR    R6,R0                                                            
         MVI   0(R6),X'25'                                                      
         MVI   1(R6),22                                                         
         BAS   RE,SETLEN                                                        
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         MVI   0(R6),X'26'                                                      
         MVI   1(R6),22                                                         
         BAS   RE,SETLEN                                                        
*                                                                               
JW38Z    CP    JWCSHDSC,=P'0'      TEST ANY ACTUAL CASH DISCOUNT                
         BNE   *+10                                                             
         ZAP   PBDCD,=P'0'         NO - FORCE ZERO RATE                         
*                                                                               
         CLC   JWPAYDT,=C'000000'  TEST PAID                                    
         BNH   JW40                                                             
         LA    R0,6                                                             
         LA    R1,JWPAYDT                                                       
JW39     CLI   0(R1),C'0'                                                       
         BL    JW39ERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    JW39ERR                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,JW39                                                          
         B     JW39X                                                            
JW39ERR  BAS   RE,PDTERR                                                        
         B     JW40                                                             
         SPACE 1                                                                
* CREATE PAID ELEMENT                                                           
         SPACE 1                                                                
JW39X    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         USING PPAYELEM,R6                                                      
         MVI   0(R6),X'25'                                                      
         MVI   1(R6),22                                                         
         BAS   RE,SETLEN                                                        
         GOTO1 DATCON,DMCB,JWPAYDT,(3,PPDDATE)                                  
         MVC   PPGROSS,GROSS                                                    
         MVC   PPAGYCOM,AGYCOM                                                  
         MVC   PPCSHDSC,CSHDSC                                                  
*                                                                               
         MVC   PBDPDATE,PPDDATE    SET PAYABLE DATE=PAID DATE                   
*                                                                               
JW40     CLC   JWBILLDT,=C'000000' TEST BILLED                                  
         BNH   JW50                                                             
         LA    R0,6                                                             
         LA    R1,JWBILLDT                                                      
JW41     CLI   0(R1),C'0'                                                       
         BL    JW41ERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    JW41ERR                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,JW41                                                          
         B     JW41X                                                            
*                                                                               
JW41ERR  BAS   RE,BDTERR                                                        
         B     JW50                                                             
         SPACE 1                                                                
* CREATE BILLED ELEMENT                                                         
         SPACE 1                                                                
JW41X    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         MVI   0(R6),X'26'                                                      
         MVI   1(R6),23                                                         
         BAS   RE,SETLEN                                                        
         USING PBILELEM,R6                                                      
         MVC   PBPRD,PBUYKPRD                                                   
         GOTO1 DATCON,DMCB,JWBILLDT,(3,PBLDATE)                                 
         MVC   PBGROSS,GROSS                                                    
         MVC   PBAGYCOM,AGYCOM                                                  
         MVC   PBCSHDSC,CSHDSC                                                  
*                                                                               
         MVC   PBDBDATE,PBLDATE    SET BILLABLE DATE = BILLED DATE              
*                                                                               
JW50     LA    R6,PBDELEM          RESTORE REG POINTER                          
         SPACE 1                                                                
* MAGAZINES/TRADE/INT'L (SUPPL) *                                               
         SPACE 1                                                                
JW60     L     R0,GROSS                                                         
         S     R0,PREMIUM                                                       
         CVD   R0,DUB                                                           
         ZAP   PBDCOS,DUB                                                       
         MVI   PBDCOSTY,C'T'       INDICATE TOTAL COST                          
*                                                                               
         CLI   JWMEDIA,C'N'                                                     
         BNE   JW20                                                             
         SPACE 1                                                                
* NEWSPAPERS*                                                                   
         SPACE 1                                                                
         CLI   JWLINES,C'0'        TEST PACKED OR ZONE DECIMAL                  
         BL    JW102               PACKED IS INCHES                             
* LINES                                                                         
         MVI   PBDUIND,C'L'                                                     
         B     JW20                                                             
*                                                                               
JW102    MVI   PBDUIND,X'89'       SET UNITS = INCHES (2 DEC)                   
         ZAP   PBDUNITS,JWINCHES                                                
         B     JW20                                                             
*                                                                               
SETLEN   SR    R0,R0                                                            
         ICM   R0,3,PBUYLEN                                                     
         ZIC   RF,1(R6)                                                         
         AR    R0,RF                                                            
         STCM  R0,3,PBUYLEN                                                     
         BR    RE                                                               
         EJECT                                                                  
* RETRIEVE SORT OUTPUT AND WRITE OUTPUT FILE *                                  
         SPACE 1                                                                
JW400    DS    0H                                                               
         BAS   RE,PRTRTERR                                                      
         XC    KEY,KEY                                                          
*                                                                               
JW402    DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R4,15,4(R1)         R4 POINTS TO 4 BYTE RECLEN                   
         BZ    JW410                                                            
         CLC   KEY(24),4(R4)       TEST SAME UP TO LINE                         
         BE    *+6                                                              
         SR    R5,R5               RESET LINE COUNTER                           
         LA    R5,1(R5)            BUMP LINE COUNTER                            
         STC   R5,28(R4)           SET LINE NUMBER IN BUY                       
         MVC   KEY(25),4(R4)       SAVE THIS KEY                                
JW404    DS    0H                                                               
         AP    SORTOUT,=P'1'                                                    
         LR    R0,R4                                                            
         PUT   FILEOUT,(R0)                                                     
         B     JW402                                                            
*                                                                               
JW410    CLOSE FILEOUT                                                          
*                                                                               
         LA    R4,COUNTS                                                        
         LA    R5,NCOUNTS                                                       
*                                                                               
JW420    OI    3(R4),X'0F'                                                      
         UNPK  P(7),0(4,R4)                                                     
         MVC   P+9(20),4(R4)                                                    
         GOTO1 REPORT                                                           
         LA    R4,L'COUNTS(R4)                                                  
         BCT   R5,JW420                                                         
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*==================================*                                            
* CALCULATE BILLABLE/PAYABLE DATES *                                            
*==================================*                                            
         SPACE 1                                                                
GETBPDTS NTR1                                                                   
         MVC   PBDPDATE,PBUYKDAT                                                
**NOP**  LA    R4,PAYCDS           POINT TO APPROPRIATE TABLE                   
**NOP**  CLI   JWMEDIA,C'N'                                                     
**NOP**  BNE   *+8                                                              
**NOP**  LA    R4,PAYCDSN                                                       
**NOP**  ZIC   R5,JWPAYCYC                                                      
**NOP**  N     R5,=X'0000000F'                                                  
**NOP**  BCTR  R5,0                                                             
**NOP**  IC    R5,0(R4,R5)         GET PAY DAY                                  
**NOP**  STC   R5,PBDPDATE+2       SET PAYABLE DAY                              
**NOP**  CLI   PBDPDATE+2,99                                                    
**NOP**  BNE   GETBP10                                                          
**NOP**  LA    R4,DAYS87                                                        
**NOP**  CLI   PBDPDATE,88                                                      
**NOP**  BNE   *+8                                                              
**NOP**  LA    R4,DAYS88                                                        
**NOP**  ZIC   R5,PBDPDATE+1       GET PAYABLE MONTH                            
**NOP**  BCTR  R5,0                                                             
**NOP**  IC    R5,0(R5,R4)         GET DAYS IN MONTH                            
**NOP**  STC   R5,PBDPDATE+2       SET PAYABLE DAY                              
*                                                                               
GETBP10  MVC   PBDBDATE,PBUYKDAT   SET BILLABLE DATE = INSERT DATE              
         CLI   JWBILCYC,C' '       TEST VALID BILL CYCLE CODE                   
         BNE   GETBP20             YES                                          
         CLC   JWBILLDT,=C'000000' TEST BILLED                                  
         BE    GETBP15             NO                                           
         GOTO1 DATCON,DMCB,JWBILLDT,(3,PBDBDATE)                                
         B     GETBP30                                                          
*                                                                               
GETBP15  DS    0H                  IF CYCLE INVALID AND UNBILLED, USE           
* SET BILLABLE ON FIRST OF FOLLOWING MONTH IF INVALID                           
         ZIC   R6,PBDBDATE         GET YEAR                                     
         ZIC   R5,PBDBDATE+1       GET MONTH                                    
         LA    R5,1(R5)            SET PAYABLE 1 MONTH LATER                    
         CH    R5,=H'13'           TEST TOO BIG                                 
         BL    *+12                                                             
         SH    R5,=H'12'                                                        
         LA    R6,1(R6)                                                         
         STC   R6,PBDBDATE         SET YEAR                                     
         STC   R5,PBDBDATE+1       SET MONTH                                    
         B     GETBP30                                                          
         EJECT                                                                  
GETBP20  DS    0H                                                               
         LA    R4,BILLCDS                                                       
         CLI   JWMEDIA,C'N'                                                     
         BNE   *+8                                                              
         LA    R4,BILLCDSN                                                      
*                                                                               
         ZIC   R5,JWBILCYC         GET BILL CYCLE CODE                          
         N     R5,=X'0000000F'                                                  
         BCTR  R5,0                                                             
         AR    R5,R5               ADJUST FOR 2 BYTE ENTRIES                    
         AR    R4,R5               POINT TO MONTH FACTOR                        
         ZIC   R5,PBUYKDAT+1       PICK UP INSERTION MONTH                      
         ZIC   R6,PBUYKDAT         PICK UP INSERTION YEAR                       
         AH    R5,0(R4)            ADD INCREMENT (DECREMENT)                    
*                                                                               
         LTR   R5,R5               TEST POSITIVE                                
         BP    *+10                                                             
         AH    R5,=H'12'                                                        
         BCTR  R6,0                                                             
*                                                                               
         CH    R5,=H'13'           TEST TOO BIG                                 
         BL    *+12                                                             
         SH    R5,=H'12'                                                        
         LA    R6,1(R6)            ADVANCE YEAR                                 
*                                                                               
         STC   R6,PBDBDATE         SET BILLABLE YEAR                            
         STC   R5,PBDBDATE+1       SET BILLABLE MONTH                           
*                                                                               
GETBP30  MVC   PBDPDATE,PBDBDATE   SET BILLABLE IN PAYABLE DATE                 
         ZIC   R6,PBDPDATE         GET YEAR                                     
         ZIC   R5,PBDPDATE+1       GET MONTH                                    
         LA    R5,1(R5)            SET PAYABLE 1 MONTH LATER                    
         CH    R5,=H'13'           TEST TOO BIG                                 
         BL    *+12                                                             
         SH    R5,=H'12'                                                        
         LA    R6,1(R6)                                                         
         STC   R6,PBDPDATE         SET YEAR                                     
         STC   R5,PBDPDATE+1       SET MONTH                                    
         B     EXIT                                                             
*                                                                               
PAYCDS   DC   AL1(10,20,99)        99=LAST                                      
*                  1  2  3                                                      
PAYCDSN  DC   AL1(15,20,99)                                                     
*                  1  2  3                                                      
BILLCDS  DC   H'-3',H'0',H'-2',H'0',H'-1',H'0',H'0',H'0',H'+1'                  
*                1   XXX    3   XXX    5   XXX   7   XXX    9                   
BILLCDSN DC   H'-2',H'0',H'-1',H'0',H'+1',H'0',H'0',H'0',H'0'                   
*                1   XXX    3   XXX    5   XXX   7   XXX  XXX                   
*                                                                               
DAYS87   DC    AL1(31,28,31,30,31,30,31,31,30,31,30,31)                         
DAYS88   DC    AL1(31,29,31,30,31,30,31,31,30,31,30,31)                         
*                 J F M A M J J A S O N D                                       
         EJECT                                                                  
*=============================================================*                 
* SUBROUTINE COMPUTES GROSS/NET/AGYCOMM/CSHDSC AND COMM RATE  *                 
*=============================================================*                 
         SPACE 1                                                                
GETDOLS  NTR1                                                                   
         XC    GROSS(48),GROSS                                                  
         XC    NET,NET                                                          
         ZAP   LINRT,=P'0'                                                      
*                                                                               
         ZAP   PBDACP,=P'15000'    SET NORMAL COMM PCT                          
         CP    JWAGYCOM,=P'0'                                                   
         BNE   GETDOL1                                                          
         ZAP   PBDACP,=P'0'                                                     
         B     GETDOL5                                                          
*                                                                               
GETDOL1  CLI   JWGRSNET,C'G'                                                    
         BNE   GETDOL2                                                          
         CP    JWAGYCOM,=P'0'                                                   
         BE    GETDOL5                                                          
GETDOL1A MVI   PBDCOSIN,C' '       SET COST IND                                 
         BAS   RE,GETPRM                                                        
         BNE   GETDOL1B                                                         
         ZAP   PBDPRCOS,JWCLRCHG   SET CHARGE IN PBDELEM                        
         ZAP   DUB,JWCLRCHG                                                     
         CVB   R0,DUB                                                           
         ST    R0,PREMIUM                                                       
*                                                                               
GETDOL1B ZAP   DUB,JWNET                                                        
         AP    DUB,JWCSHDSC        THEIR NET IS LESS CD                         
         CVB   R1,DUB                                                           
         ST    R1,NET              THIS IS NET PAID/BILLED                      
*                                                                               
         CVB   R1,DUB              SO ADD IT BEFORE GROSSING UP                 
         M     R0,=F'200'          X 100 X 2                                    
         D     R0,=F'85'           GROSS UP TO FULL MEDIA VALUE                 
         LTR   R1,R1                                                            
         BM    *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         ST    R1,GROSS                                                         
         B     GETDOL3                                                          
*                                                                               
GETDOL2  CLI   JWGRSNET,C'N'                                                    
         BNE   GETDOL4                                                          
         CP    JWAGYCOM,=P'0'                                                   
         BE    GETDOL5                                                          
*                                                                               
         BAS   RE,GETPRM                                                        
         BNE   GETDOL2B                                                         
         ZAP   DUB,JWCLRCHG        NEED TO GROSS UP COLOR CHARGE                
         CVB   R1,DUB                                                           
         M     R0,=F'200'          X 100 X 2                                    
         D     R0,=F'85'                                                        
         LTR   R1,R1                                                            
         BM    *+8                                                              
         SRA   R1,1                                                             
         ST    R1,PREMIUM                                                       
         CVD   R1,DUB                                                           
         ZAP   PBDPRCOS,DUB                                                     
*                                                                               
GETDOL2B DS    0H                                                               
         MVI   PBDCTYP,C'N'        SET COST IND                                 
         ZAP   DUB,JWNET                                                        
         AP    DUB,JWCSHDSC        ADD CASH DISCOUNT BACK IN                    
         CVB   R1,DUB                                                           
         ST    R1,NET                                                           
*                                                                               
         CVB   R1,DUB                                                           
         M     R0,=F'200'          X 100 X 2                                    
         D     R0,=F'85'                                                        
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         ST    R1,GROSS                                                         
*                                                                               
         CP    JWLINRT,=P'0'                                                    
         BE    GETDOL3                                                          
* NEED TO GROSS UP LINE RATE TOO                                                
         ZAP   DUB,JWLINRT                                                      
         CVB   RF,DUB                                                           
         M     RE,=F'200'          X 100 X 2                                    
         D     RE,=F'85'                                                        
         LTR   RF,RF                                                            
         BNP   *+8                                                              
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         CVD   RF,DUB                                                           
         ZAP   LINRT,DUB                                                        
*                                                                               
GETDOL3  L     R0,GROSS                                                         
         S     R0,NET                                                           
         ST    R0,AGYCOM                                                        
         B     GETDOLX                                                          
*                                                                               
GETDOL4  CLI   JWGRSNET,C'S'                                                    
         BNE   GETDOL6                                                          
*                                                                               
GETDOL5  ZAP   PBDACP,=P'0'                                                     
         MVI   PBDCOSIN,C'S'                                                    
*                                                                               
         BAS   RE,GETPRM                                                        
         BNE   GETDOL5B                                                         
         ZAP   DUB,JWCLRCHG                                                     
         CVB   R0,DUB                                                           
         ST    R0,PREMIUM                                                       
*                                                                               
GETDOL5B ZAP   DUB,JWGROSS                                                      
         CVB   R0,DUB                                                           
         A     R0,PREMIUM                                                       
         ST    R0,GROSS                                                         
         ST    R0,NET                                                           
         XC    AGYCOM,AGYCOM                                                    
         B     GETDOLX                                                          
*                                                                               
GETDOL6  CLI   JWGRSNET,C'B'       TEST MEMO FOR INV/EST                        
         BE    *+12                                                             
         CLI   JWGRSNET,C'M'       TEST MEMO FOR EST ONLY                       
         BNE   GETDOL8                                                          
         SPACE 1                                                                
*=================================================================*             
* GAYA SAYS IF 'B' OR 'M' SET BILLED/PAID DATES = INSERTION DATE  *             
* IF GROSS = 0                                                    *             
*=================================================================*             
         SPACE 1                                                                
         CP    JWGROSS,=P'0'                                                    
         BNE   EQXIT                                                            
         MVC   JWPAYDT,JWINSDAT                                                 
         MVC   JWBILLDT,JWINSDAT                                                
         B     EQXIT                                                            
*                                                                               
GETDOL8  CLI   JWGRSNET,C'X'       MANUAL RATE TYPE                             
         BNE   GETDOL10                                                         
         CP    JWGROSS,JWNET       TEST NET = GROSS                             
         BE    GETDOL5                                                          
         CLC   JWPAYDT,=C'000000'  TEST PAID                                    
         BNH   GETDOL8X                                                         
         CLC   JWBILLDT,=C'000000' TEST BILLED                                  
         BNH   GETDOL8X                                                         
* WE NEED THIS ONE                                                              
         BAS   RE,RTNOTE           PRINT A NOTE                                 
         B     GETDOL2B            AND TREAT AS NET BUY                         
*                                                                               
GETDOL8X BAS   RE,RATERR           PRINT ERROR MESSAGE                          
         B     NEQXIT              EXIT AND DROP BUY                            
*                                                                               
GETDOL10 CLI   JWGRSNET,C'C'       TEST COMMISSION ONLY                         
         BNE   GETDOL12                                                         
         MVC   PBDSPACE,=CL17'COMMISSION ONLY'                                  
         ZAP   PBDACP,=P'-1'       SET COMM RATE = 100%                         
*******  MVI   PBDCOSIN,C'C'       BRUCE SAID NO                                
         ZAP   DUB,JWGROSS                                                      
         CVB   R0,DUB                                                           
         ST    R0,GROSS                                                         
         ST    R0,AGYCOM                                                        
         XC    NET,NET                                                          
         XC    CSHDSC,CSHDSC       NEVER ANY CASH DISCOUNT                      
         ZAP   PBDCD,=P'0'                                                      
         B     EQXIT                                                            
*                                                                               
GETDOL12 BAS   RE,RATERR              PRINT ERROR                               
         B     NEQXIT              AND GET OUT                                  
*                                                                               
GETDOLX  XC    CSHDSC,CSHDSC                                                    
         CP    JWCSHDSC,=P'0'                                                   
         BE    EQXIT                                                            
         ZAP   DUB,PBDCD                                                        
         CVB   R1,DUB                                                           
         AR    R1,R1                                                            
         M     R0,NET                                                           
         D     R0,=F'1000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         ST    R1,CSHDSC                                                        
         B     EQXIT                                                            
*                                                                               
DUB2     DS    D                                                                
         SPACE 2                                                                
GETPRM   CLI   JWMEDIA,C'N'        PREMIUM CHARGE ONLY ON NEWSPAPER             
         BNE   GETPRM6                                                          
*                                                                               
         CLI   JWLINES,C'0'        TEST ZONE OR PACKED                          
         BNL   GETPRM2                                                          
         ZAP   DUB,JWLINES         PACKED                                       
         B     GETPRM4                                                          
*                                                                               
GETPRM2  PACK  DUB,JWLINES         ZONE DECIMAL                                 
*                                                                               
GETPRM4  CP    DUB,=P'0'           TEST FOR LINES OR INCHES                     
         BNE   GETPRM6                                                          
         CR    RE,RE               EXIT WITH CC EQ IF LINES/INCHES              
         BR    RE                                                               
*                                                                               
GETPRM6  LTR   RE,RE               EXIT WITH CC NEQ IF NO LINES/INCHES          
         BR    RE                                                               
         EJECT                                                                  
*====================================================*                          
* SUBROUTINE PRINTS SEMI-READABLE TRACE OF JW RECORD *                          
*====================================================*                          
         SPACE 1                                                                
JWTRACE  NTR1                                                                   
         ZAP   DUB,INCNT                                                        
         CVB   RE,DUB                                                           
         SRDL  RE,32                                                            
         D     RE,=F'50'                                                        
         LTR   RE,RE                                                            
         BNZ   EXIT                                                             
         B     JWTRCALL                                                         
*                                                                               
JWTRACE2 NTR1                                                                   
*                                                                               
JWTRCALL LR    R2,R1                                                            
         MVC   0(13,R2),JWSRTKEY                                                
         GOTO1 DATCON,DMCB,(3,JWSRTDAT),13(R2)                                  
         MVC   19(7,R2),JWSRTEST                                                
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+10(120),COLS                                                   
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,JWDEL                                                         
         MVC   P+10(120),0(R2)                                                  
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,(R2),HEXBLOCK,120,=C'SEP'                            
         MVC   P+10(120),HEXBLOCK                                               
         GOTO1 REPORT                                                           
         MVC   P+10(120),HEXBLOCK+120                                           
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,JWDEL+120                                                     
         MVC   P+10(120),0(R2)                                                  
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,(R2),HEXBLOCK,120,=C'SEP'                            
         MVC   P+10(120),HEXBLOCK                                               
         GOTO1 REPORT                                                           
         MVC   P+10(120),HEXBLOCK+120                                           
         MVI   SPACING,2           SKIP A LINE AFTER PRINT                      
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
COLS     DC    C'....5...10...15...20...25...30...35...40...45...50...5X        
               5...60...65...70...75...80...85...90...95...00...05...10X        
               ...15...20'                                                      
HEXBLOCK DS    CL240                                                            
         EJECT                                                                  
*============================================*                                  
* SUBROUTINE PRINTS ELEMENT TRACE OF DDS REC *                                  
*============================================*                                  
         SPACE 1                                                                
DDSTRACE NTR1                                                                   
         CLI   QOPT1,C'Y'                                                       
         BNE   EXIT                                                             
*                                                                               
         ZAP   DUB,INCNT                                                        
         CVB   RE,DUB                                                           
         SRDL  RE,32                                                            
         D     RE,=F'50'                                                        
         LTR   RE,RE                                                            
         BNZ   EXIT                                                             
*                                                                               
         MVC   P(25),PBUYREC                                                    
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,PBUYREC,HEXBLOCK,27,=C'SEP'                          
         MVC   P(27),HEXBLOCK                                                   
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(27),HEXBLOCK+27                                                
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(116),PBDELEM                                                   
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,PBDELEM,HEXBLOCK,116,=C'SEP'                         
*                                                                               
         MVC   P(116),HEXBLOCK                                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(116),HEXBLOCK+116                                              
         GOTO1 REPORT                                                           
*                                                                               
         LA    R6,PBDELEM                                                       
*                                                                               
DDSTR2   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    EXIT                                                             
         ZIC   R7,1(R6)            GET CURRENT ELEMENT LENGTH                   
         BCTR  R7,0                SET FOR EX                                   
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R6) *EXECUTED*                                            
         GOTO1 REPORT                                                           
*                                                                               
         LA    R7,1(R7)                                                         
         GOTO1 HEXOUT,DMCB,(R6),HEXBLOCK,(R7),=C'SEP'                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   P(0),HEXBLOCK *EXECUTED*                                         
         GOTO1 REPORT                                                           
*                                                                               
         LA    RE,HEXBLOCK+1(R7)                                                
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(RE) *EXECUTED*                                            
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     DDSTR2                                                           
         EJECT                                                                  
* ON ENTRY R1 POINTS TO RECORD *                                                
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         CLI   3(R1),X'20'         TEST BUYREC                                  
         BNE   PUTSORT2                                                         
*                                                                               
         ZIC   RE,LASTBUY+24       GET PREVIOUS LINE NUM                        
         CLC   LASTBUY,0(R1)       TEST SAME UP TO LINE                         
         BE    *+6                 YES                                          
         SR    RE,RE               RESET LINE NUM                               
         LA    RE,1(RE)                                                         
         STC   RE,24(R1)           SET AS NEW LINE                              
         MVC   LASTBUY,0(R1)       SAVE THIS KEY                                
*                                                                               
PUTSORT2 SR    R0,R0                                                            
         ICM   R0,3,(PBUYLEN-PBUYREC)(R1)                                       
         AH    R0,=H'4'                                                         
         SLL   R0,16                                                            
         SH    R1,=H'4'            BACK UP TO RECLEN                            
         ST    R0,0(R1)            STORE IT IN FRONT OF RECORD                  
         LR    R0,R1               POINT R0 TO RECORD                           
         GOTO1 SORTER,DMCB,=C'PUT',(R0)                                         
         AP    SORTIN,=P'1'                                                     
         B     EXIT                                                             
         SPACE 2                                                                
GETFILE  NTR1                                                                   
         ZAP   DUB,SKIPCNT         SET SKIP COUNT                               
         CP    INCNT,MAXIN                                                      
         BH    ENDIN                                                            
*                                                                               
GETFILE1 DS    0H                                                               
*                                                                               
GETFILE2 GET   FILEIN,JWNEXT                                                    
         AP    INCNT,=P'1'                                                      
         CLC   =C'0538',JWCLT      TEST FOR HYATT                               
         BE    GETFIL2X            SKIP THIS MOTHER                             
         CLC   =C'0540',JWCLT      OR HYATT INT'L                               
         BE    GETFIL2X                                                         
         B     GETFILE3                                                         
GETFIL2X AP    HHCOUNT,=P'1'                                                    
         B     GETFILE1                                                         
         SPACE 1                                                                
*=========================================*                                     
* IGNORE ALL BUYS WITH GROSS AND NET = 0  *                                     
*=========================================*                                     
GETFILE3 CP    JWGROSS,=P'0'              *                                     
         BNE   GETFILE4                   *                                     
         CP    JWNET,=P'0'                *                                     
         BNE   GETFILE4                   *                                     
         AP    GN0COUNT,=P'1'             *                                     
         B     GETFILE1                   *                                     
*=========================================*                                     
         SPACE 1                                                                
GETFILE4 CLI   QOPT2,C' '          TEST PROCESS ALL MEDIA                       
         BE    GETFILEX                                                         
         CLI   QOPT2,C'X'          TEST SUPPRESS PUBLIST                        
         BE    GETFILEX                                                         
         CLC   QOPT2,JWMEDIA       ELSE MATCH MEDIA CODE                        
         BNE   GETFILE2                                                         
*                                                                               
GETFILEX CP    DUB,=P'0'           TEST SKIPPED ENOUGH YET                      
         BE    EXIT                                                             
         SP    DUB,=P'1'           NO- DECR COUNT AND READ NEXT                 
         B     GETFILE1                                                         
         EJECT                                                                  
ENDIN    CLOSE FILEIN                                                           
         MVI   JWNEXT,X'FF'                                                     
         MVC   JWNEXT+1(199),JWNEXT                                             
         MVI   EOFSW,C'Y'                                                       
         B     EXIT                                                             
         EJECT                                                                  
CLTERR   L     R1,=A(MSG1)                                                      
         B     *+8                                                              
NOCLTERR L     R1,=A(MSG5)                                                      
         AP    CLTERRS,=P'1'                                                    
         CLC   SRCHCLT(4),WORK     SAME CLT                                     
         BER   RE                  YES - IGNORE                                 
         MVC   SRCHCLT(4),WORK     ELSE SAVE IT                                 
         B     CONERR              AND PRINT ERROR ONCE                         
*                                                                               
PRDERR   L     R1,=A(MSG2)                                                      
         AP    PRDERRS,=P'1'                                                    
         CLC   SRCHCLT(7),WORK     SAME CLT/PRD                                 
         BER   RE                  YES - IGNORE                                 
         MVC   SRCHCLT(7),WORK     ELSE SAVE IT                                 
         B     CONERR              AND PRINT ERROR ONCE                         
*                                                                               
ESTERR   L     R1,=A(MSG3)                                                      
         AP    ESTERRS,=P'1'                                                    
         CLC   SRCHEST(9),WORK     SAME CLT/PRD                                 
         BER   RE                  YES - IGNORE                                 
         MVC   SRCHEST(9),WORK     ELSE SAVE IT                                 
         MVC   P+90(1),PBUYKMED                                                 
         MVC   P+91(6),PBUYKCLT                                                 
         SR    R0,R0                                                            
         ICM   R0,3,WORK+7                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+97(3),DUB                                                      
         B     CONERR              AND PRINT ERROR ONCE                         
*                                                                               
BADEST   L     R1,=A(MSG6)                                                      
         AP    BADESTS,=P'1'                                                    
         MVC   WORK(4),JWCLT                                                    
         MVC   WORK+4(3),JWPRD                                                  
         MVC   WORK+7(6),JWEST     MOVE ENTIRE JWT EST                          
         CLC   SRCHEST(13),WORK    SAME CLT/PRD/EST                             
         BER   RE                  YES - IGNORE                                 
         MVC   SRCHEST(13),WORK    ELSE SAVE IT                                 
*                                                                               
         NTR1                                                                   
         MVC   P(11),=C'** ERROR **'                                            
         MVC   P+12(40),0(R1)                                                   
         MVC   P+53(4),=C'KEY='                                                 
         MVC   P+57(13),JWSRTKEY                                                
         GOTO1 DATCON,DMCB,(3,JWSRTDAT),P+71                                    
         MVC   P+78(6),JWSRTEST                                                 
         CLI   P+83,0                                                           
         BNE   *+8                                                              
         MVI   P+83,C'.'                                                        
         MVC   P+90(1),PBUYKMED                                                 
         MVC   P+92(3),PBUYKCLT                                                 
         MVC   P+96(3),PBUYKPRD                                                 
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
PUBERR   NTR1                                                                   
         AP    PUBERRS,=P'1'                                                    
         CLC   SRCHPUB(7),WORK     TEST SAME AS LAST TIME                       
         BE    EXIT                                                             
         MVC   SRCHPUB(7),WORK                                                  
         MVC   P+90(1),SRCHPUB                                                  
         GOTO1 HEXOUT,DMCB,SRCHPUB+1,P+91,6,=C'TOG'                             
         L     R1,=A(MSG4)                                                      
         B     CONERR2                                                          
*                                                                               
RATERR   NTR1                                                                   
         AP    RATERRS,=P'1'                                                    
         CLC   RTERRKEY(13),JWSRTKEY        MED/CLT/PRD/PUB                     
         BNE   RATERR2                                                          
         CLC   RTERRKEY+16(3),JWSRTKEY+16   EST                                 
         BE    RATERR4                                                          
*                                                                               
RATERR2  BAS   RE,PRTRTERR                                                      
*                                                                               
         MVC   RTERRKEY,JWSRTKEY                                                
         MVC   RTERRMED,PBUYKMED                                                
         MVC   RTERRCLT,PBUYKCLT                                                
         MVC   RTERRPRD,PBUYKPRD                                                
         MVC   RTERRPUB,PBUYKPUB                                                
         MVC   RTERREST,PBUYKEST                                                
*                                                                               
RATERR4  AP    RTERRGRS,JWGROSS                                                 
         AP    RTERRNET,JWNET                                                   
         AP    RTERRCD,JWCSHDSC                                                 
*                                                                               
         MVC   P(11),=C'** ERROR **'                                            
         L     R1,=A(MSG7)                                                      
         MVC   P+12(40),0(R1)                                                   
         BAS   RE,PRTX                                                          
         B     NEQXIT                                                           
*                                                                               
RTNOTE   NTR1                                                                   
         MVC   P(8),=C'* NOTE *'                                                
         L     R1,=A(MSG12)                                                     
         MVC   P+12(40),0(R1)                                                   
         BAS   RE,PRTX                                                          
         B     EQXIT               KEEP THIS BUY                                
*                                                                               
PRTX     NTR1                                                                   
         CLI   QOPT3,C'N'          TEST TO SUPPRESS MESSAGES                    
         BNE   *+14                                                             
         MVC   P,SPACES                                                         
         B     EXIT                                                             
*                                                                               
         MVC   P+53(4),=C'KEY='                                                 
         MVC   P+57(13),JWSRTKEY                                                
         GOTO1 DATCON,DMCB,(3,JWSRTDAT),P+71                                    
         MVC   P+78(7),JWSRTEST                                                 
         EDIT  JWGROSS,(12,P+87),2,MINUS=YES                                    
         EDIT  JWNET,(12,P+101),2,MINUS=YES                                     
*                                                                               
         MVC   P+115(1),JWGRSNET                                                
         MVC   P+116(1),JWTYPE                                                  
         MVC   P+117(1),JWBPIND                                                 
         CLC   JWBILLDT,=C'000000'                                              
         BNH   *+8                                                              
         MVI   P+118,C'B'                                                       
         CLC   JWPAYDT,=C'000000'                                               
         BNH   *+8                                                              
         MVI   P+119,C'P'                                                       
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
PDTERR   L     R1,=A(MSG10)                                                     
         AP    PDTERRS,=P'1'                                                    
         B     CONERR                                                           
*                                                                               
BDTERR   L     R1,=A(MSG11)                                                     
         AP    BDTERRS,=P'1'                                                    
         B     CONERR                                                           
*                                                                               
BILLERR  L     R1,=A(MSG8)                                                      
         AP    BILLERRS,=P'1'                                                   
         B     CONERR                                                           
*                                                                               
CONERR   NTR1                                                                   
*                                                                               
CONERR2  MVC   P(11),=C'** ERROR **'                                            
         MVC   P+12(40),0(R1)                                                   
         MVC   P+53(4),=C'KEY='                                                 
         MVC   P+57(13),JWSRTKEY                                                
         GOTO1 DATCON,DMCB,(3,JWSRTDAT),P+71                                    
         MVC   P+78(7),JWSRTEST                                                 
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
PRTRTERR NTR1                                                                   
         CLI   QOPT3,C'N'                                                       
         BE    EXIT                                                             
         CP    RTERRGRS,=P'0'                                                   
         BNE   PRTRTER2                                                         
         CP    RTERRNET,=P'0'                                                   
         BNE   PRTRTER2                                                         
         CP    RTERRCD,=P'0'                                                    
         BE    EXIT                                                             
*                                                                               
PRTRTER2 DS    0H                                                               
         GOTO1 REPORT              SKIP A LINE                                  
         MVC   P(10),=C'=========>'                                             
         MVC   P+12(10),=C'* TOTALS *'                                          
         MVC   P+23(1),RTERRMED                                                 
         MVC   P+25(3),RTERRCLT                                                 
         MVC   P+29(3),RTERRPRD                                                 
         GOTO1 HEXOUT,DMCB,RTERRPUB,P+33,6,=C'TOG'                              
         LH    R0,RTERREST                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+46(3),DUB                                                      
*                                                                               
         MVC   P+53(4),=C'KEY='                                                 
         MVC   P+57(13),RTERRKEY                                                
         LA    R0,RTERRKEY+13                                                   
         GOTO1 DATCON,DMCB,(3,(R0)),P+71                                        
         MVC   P+78(7),RTERRKEY+16                                              
         EDIT  RTERRGRS,(12,P+87),2,MINUS=YES                                   
         EDIT  RTERRNET,(12,P+102),2,MINUS=YES                                  
         EDIT  RTERRCD,(12,P+116),2,MINUS=YES                                   
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         ZAP   RTERRGRS,=P'0'                                                   
         ZAP   RTERRNET,=P'0'                                                   
         ZAP   RTERRCD,=P'0'                                                    
         B     EXIT                                                             
*                                                                               
RTERRGRS DC    PL8'0'                                                           
RTERRNET DC    PL8'0'                                                           
RTERRCD  DC    PL8'0'                                                           
RTERRKEY DC    XL27'00'                                                         
RTERRMED DS    CL1                                                              
RTERRCLT DS    CL3                                                              
RTERRPRD DS    CL3                                                              
RTERRPUB DS    CL6                                                              
RTERREST DS    CL2                                                              
         EJECT                                                                  
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=FB,MACRF=GM,EODAD=ENDIN,   X        
               BLKSIZE=2670,LRECL=267                                           
         SPACE 1                                                                
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               BLKSIZE=32760,LRECL=4004                                         
*                                                                               
         DS    0D                                                               
COUNTS   DS    0CL24                                                            
INCNT    DC    PL4'0',CL20'INPUT RECORDS'                                       
SORTIN   DC    PL4'0',CL20'SORT RECORDS IN'                                     
SORTOUT  DC    PL4'0',CL20'SORT RECORDS OUT'                                    
HHCOUNT  DC    PL4'0',CL20'DELETED HH RECS'                                     
GN0COUNT DC    PL4'0',CL20'GROSS/NET=0 DELS'                                    
CLTERRS  DC    PL4'0',CL20'MISSING CLIENTS'                                     
PRDERRS  DC    PL4'0',CL20'MISSING PRODUCTS'                                    
BADESTS  DC    PL4'0',CL20'EST NOT IN CONV TAB'                                 
RATERRS  DC    PL4'0',CL20'DELETED X RATES'                                     
****************************************                                        
PUBERRS  DC    PL4'0',CL20'MISSING PUBS'                                        
ESTERRS  DC    PL4'0',CL20'MISSING ESTIMATES'                                   
BILLERRS DC    PL4'0',CL20'MISSING BILL DATE'                                   
PDTERRS  DC    PL4'0',CL20'BAD PAY DATES'                                       
BDTERRS  DC    PL4'0',CL20'BAD BILL DATES'                                      
NCOUNTS  EQU   ((*-COUNTS)/24)                                                  
*                                                                               
XINCOUNT DC    PL4'0'                                                           
XCLTERR  DC    PL4'0'                                                           
XPRDERR  DC    PL4'0'                                                           
XBADEST  DC    PL4'0'                                                           
XRATERR  DC    PL4'0'                                                           
*                                                                               
MAXIN    DC    PL4'9999999'                                                     
SKIPCNT  DC    PL4'0'                                                           
SVJWKEY  DS    CL21                                                             
ERRFLAG  DC    X'00'                                                            
EOFSW    DC    X'00'                                                            
SRCHPUB  DS    XL20                                                             
SRCHCLT  DS    XL4                                                              
SRCHPRD  DS    XL7                                                              
SRCHEST  DS    XL13                                                             
CMTCOUNT DC    PL2'0'                                                           
*                                                                               
NET      DS    F                                                                
LINRT    DS    PL4                                                              
AGYCMPCT DS    PL3                                                              
LASTBUY  DS    CL25                SAVE AREA FOR LAST KEY                       
*                                                                               
         DS    0D                                                               
         DC    CL8'*JWNEXT*'                                                    
JWNEXT   DS    272C                                                             
*                                                                               
MYCPEPRM DC    6A(0)               PARAMS FOR CL/PRD CONV CODE SEARCH           
*                                                                               
MYESTPRM DC    6A(0)               PARAMS FOR ESTTAB SEARCH                     
*                                                                               
MYPUBPRM DC    6A(0)               SEARCH PARMS FOR PUB CONV TABLE              
*                                                                               
CPELSPRM DC    A(0)                PARAMS FOR CLT/PRD LIST SEARCH               
         DC    A(CPELST)                                                        
CPELSCNT DC    F'0'                                                             
         DC    A(9)                                                             
         DC    AL1(0),AL3(9)       MED(1)/CLT(3)/PRD(3)/EST(2)                  
         DC    A((CPELSTX-CPELST)/9)                                            
*                                                                               
PBLSTPRM DC    A(0)                SEARCH PARMS FOR PUB LIST                    
         DC    A(PUBLST)                                                        
PBLSTCNT DC    A(0)                                                             
         DC    A(7)                                                             
         DC    AL1(0),AL3(7)       MED(1)/PUB(6)                                
         DC    A((PUBLSTX-PUBLST)/7)                                            
         LTORG                                                                  
         DS    0D                                                               
*                                                                               
MSG1     DC    CL40'CLIENT NOT IN CONV TABLE'                                   
MSG2     DC    CL40'PRODUCT NOT IN CONV TABLE'                                  
MSG3     DC    CL40'ESTIMATE NOT ON FILE'                                       
MSG4     DC    CL40'PUB NOT ON FILE'                                            
MSG5     DC    CL40'CLT/PRD/EST NOT ON FILE'                                    
MSG6     DC    CL40'ESTIMATE NOT IN CONV TABLE'                                 
MSG7     DC    CL40'DELETED X RATE'                                             
MSG8     DC    CL40'CANNOT CALCUALTE BILL DATE'                                 
MSG10    DC    CL40'BAD PAID DATE'                                              
MSG11    DC    CL40'BAD BILLED DATE'                                            
MSG12    DC    CL40'CONVERTED X RATE'                                           
         SPACE 1                                                                
         EJECT                                                                  
***************************************                                         
* BUILD TABLE BY MEDIA OF ALL JW PUBS *                                         
***************************************                                         
         SPACE 1                                                                
BLDPUBS  NMOD1 0,BLDPUBS                                                        
         L     RC,0(R1)            RESTORE REG                                  
*                                                                               
         L     R4,=A(PUBLST)                                                    
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),QOPT2        SET MEDIA CODE                               
         GOTO1 HIGHPUB                                                          
         B     BLDPUB4                                                          
*                                                                               
BLDPUB2  DS    0H                                                               
         GOTO1 SEQPUB                                                           
*                                                                               
BLDPUB4  CLI   KEY,X'FF'                                                        
         BE    BLDPUBX                                                          
         CLI   QOPT2,C' '          TEST MEDIA LIMIT                             
         BE    *+14                                                             
         CLC   KEY(1),QOPT2        YES - MATCH MEDIA                            
         BNE   BLDPUBX                                                          
         CLI   KEY+PUBKCOD-PUBREC,X'81'      TEST MASTER RECORD                 
         BNE   BLDPUB2                                                          
         CLC   KEY+PUBKAGY-PUBREC(2),=C'JW'                                     
         BNE   BLDPUB2                                                          
*                                                                               
         C     R4,=A(PUBLSTX)                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
         MVC   0(7,R4),KEY         MOVE MEDIA/PUB/ZON/ED                        
         LA    R4,7(R4)                                                         
         BCT   R5,BLDPUB2                                                       
*                                                                               
BLDPUBX  LPR   R5,R5                                                            
         L     RE,=A(PBLSTCNT)                                                  
         ST    R5,0(RE)            SET NUMBER OF ENTRIES IN PARMS               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*====================================*                                          
* BUILD LIST OF ALL CLT/PRD/EST HDRS *                                          
*====================================*                                          
         SPACE 1                                                                
BLDCPE   NMOD1 0,*BLDCPE*                                                       
         L     RC,0(R1)            RESTORE REG POINTER                          
*                                                                               
         L     R4,=A(CPELST)                                                    
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=C'JW'       SET AGENCY CODE                              
         MVI   KEY+2,C'M'          SET FOR MAGAZINES                            
         MVI   KEY+3,7             SET RECORD CODE                              
*                                                                               
BLDCPE1  GOTO1 HIGH                                                             
         B     BLDCPE4                                                          
*                                                                               
BLDCPE2  DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
BLDCPE4  CLC   KEY(4),KEYSAVE      SAME AG/M/CODE                               
         BNE   BLDCPE10                                                         
*                                                                               
*        LA    R0,PBUYREC                                                       
*        ST    R0,AREC                                                          
*        GOTO1 GETBUY                                                           
*                                                                               
BLDCPE8  C     R4,=A(CPELSTX)                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
         MVC   0(1,R4),KEY+2       MOVE MEDIA                                   
         MVC   1(8,R4),KEY+4       MOVE CLT/PRD/EST                             
         LA    R4,9(R4)                                                         
         BCT   R5,BLDCPE2                                                       
*                                                                               
BLDCPE10 XC    KEY,KEY                                                          
         MVC   KEY(4),KEYSAVE      RESTORE PREV AG/M/CODE                       
         CLI   KEY+2,C'M'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'N'                                                       
         B     BLDCPE1                                                          
         CLI   KEY+2,C'N'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'O'                                                       
         B     BLDCPE1                                                          
         CLI   KEY+2,C'O'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'S'                                                       
         B     BLDCPE1                                                          
         CLI   KEY+2,C'S'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'T'                                                       
         B     BLDCPE1                                                          
*                                                                               
BLDCPEX  LPR   R5,R5                                                            
         L     RE,=A(CPELSCNT)                                                  
         ST    R5,0(RE)            SET NUMBER OF ENTRIES IN PARMS               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***************************************************                             
*                                                 *                             
* TRANSLATION TABLE FOR ONE BYTE EDITION CODES IN *                             
* OM CONVERSION TABLE TO INTERNAL EDITION CODES   *                             
*                                                 *                             
* TABLE ENTRIES ARE  MEDIA     CL1                *                             
*                    OLDPUB    XL4 PWOS           *                             
*                    OLDZONE   XL1                *                             
*                    OLDEDT    CL1                *                             
*                    MEDIA     CL1                *                             
*                    NEWPUB    XL4 PWOS           *                             
*                    NEWZONE   XL1                *                             
*                    NEWEDT    CL1                *                             
***************************************************                             
         SPACE 1                                                                
FIXEDT   NMOD1 0,FIXEDT                                                         
         L     RC,0(R1)            RESTORE REG POINTER                          
*                                                                               
         L     R4,MYPUBPRM+4       GET A(PUB TABLE)                             
         L     R5,MYPUBPRM+8       GET COUNT OF TABLE ITEMS                     
*                                                                               
FIXEDT2  LA    R4,6(R4)            POINT TO EDITION CODE                        
***NOP***BAL   RE,FIXIT ******* NOP - NO OLD JWT EDITION CODES                  
*                                                                               
         LA    R4,7(R4)            POINT TO NEW EDITION CODE                    
         BAS   RE,FIXIT                                                         
*                                                                               
         LA    R4,1(R4)            POINT TO NEXT MEDIA                          
         BCT   R5,FIXEDT2                                                       
         XIT1                                                                   
*                                                                               
FIXIT    CLI   0(R4),0             TEST NO EDITION CODE                         
         BER   RE                                                               
*                                                                               
         LA    R6,EDTABLE          POINT TO TRANSLATE LIST                      
         LA    R7,(EDTABLX-EDTABLE)/L'EDTABLE                                   
*                                                                               
FIXIT2   CLC   0(1,R4),0(R6)       MATCH EXTERNAL CODE                          
         BE    FIXEDT4                                                          
         LA    R6,L'EDTABLE(R6)                                                 
         BCT   R7,FIXIT2                                                        
         DC    H'0'                                                             
*                                                                               
FIXEDT4  MVC   0(1,R4),3(R6)       MOVE INTERNAL CODE TO ENTRY                  
         BR    RE                                                               
         SPACE 1                                                                
EDTABLE  DS    0CL4                                                             
         DC    C'M  A'                                                          
         DC    C'E  B'                                                          
         DC    C'D  C'                                                          
         DC    C'V  D'             INPUT FORMAT IS ME                           
         DC    C'U  I'             INPUT FORMAT IS SU                           
         DC    C'X  L'             INPUT FORMAT IS OD                           
         DC    C'P  P'                                                          
         DC    C'R  R'                                                          
         DC    C'S  S'                                                          
         DC    C'T  T'                                                          
         DC    C'W  W'                                                          
EDTABLX  DS    0C                                                               
***      DC    C'SAME'                                                          
***      DC    C'SAEF'                                                          
***      DC    C'SD G'                                                          
***      DC    C'SMEH'                                                          
***      DC    C'OM J'                                                          
***      DC    C'OE K'                                                          
***      DC    C'JWEM'                                                          
***      DC    C'MONU'                                                          
         LTORG                                                                  
         EJECT                                                                  
*==========================================================*                    
* DSECT FOR JWT ESTIMATE AND FINANCIAL FILE DETAIL RECORDS *                    
*                                                          *                    
* NOTE THAT FIRST BYTE IS NOW FILE CODE, NOT DELETE IND    *                    
*                                                          *                    
*==========================================================*                    
         SPACE 1                                                                
JWREC    DSECT                                                                  
JWSRTKEY DS    0CL27                                                            
JWSRTMED DS    CL1                                                              
JWSRTCLT DS    CL4                                                              
JWSRTPRD DS    CL3                                                              
JWSRTPUB DS    CL5                                                              
JWSRTDAT DS    CL3                                                              
JWSRTEST DS    CL6                                                              
JWSRTSEQ DS    PL4                                                              
JWSRTFIL DS    CL1                 E=ESTIMATE, F=FINANCIAL                      
*                                                                               
JWDEL    DS    CL1                                                              
JWKEY    DS    0CL40                                                            
JWMEDIA  DS    CL1                                                              
JWRECTYP DS    CL1                                                              
JWREPNUM DS    CL2                                                              
JWVENDOR DS    CL5                                                              
JWRELREC DS    CL5                                                              
         DS    CL6                                                              
JWCLT    DS    CL4                                                              
         DS    CL10                                                             
JWPRD    DS    CL3                                                              
*                                                                               
JWGRSNET DS    CL1                                                              
JWOFFLOC DS    CL1                                                              
JWOFFICE DS    CL3                                                              
JWBILTYP DS    CL1                                                              
JWPAYCYC DS    CL1                                                              
JWBILCYC DS    CL1                                                              
JWREVNUM DS    PL2                                                              
JWSEQNUM DS    CL2                                                              
JWINDS   DS    CL2                                                              
JWVNDRNM DS    CL25                                                             
JWSTATE  DS    CL2                                                              
JWDIST   DS    CL2                                                              
JWAREA   DS    CL1                                                              
JWMATSZ  DS    CL1                                                              
JWMAT    DS    CL1                                                              
JWAGYCOM DS    PL3                                                              
JWADCODE DS    CL7                                                              
JWEST    DS    CL6                                                              
JWINSDAT DS    CL6                                                              
JWINSDT2 DS    CL2                                                              
         DS    CL2                                                              
JWTAXRT  DS    PL2                                                              
JWEDITN  DS    CL3                                                              
JWDISCRT DS    PL2                                                              
JWTYPE   DS    CL1                                                              
JWBPIND  DS    CL1                                                              
JWLINES  DS    0CL4                                                             
JWINCHES DS    PL4                                                              
JWLINRT  DS    0PL4                                                             
JWINCHRT DS    PL4                                                              
JWGROSS  DS    PL5                                                              
JWCSHDSC DS    PL4                                                              
JWCLRCHG DS    PL4                                                              
JWNET    DS    PL5                                                              
JWBILCOD DS    CL1                 C'B' IF XFRD TO HISTORY FILE                 
         SPACE 1                                                                
* FOR RECORDS FROM HISTORY FILE ONLY *                                          
         SPACE 1                                                                
JWBILLDT DS    CL6                 YYMMDD                                       
JWINVMO  DS    CL2                                                              
JWINVMED DS    CL1                                                              
JWINVSEQ DS    CL5                                                              
JWPAYCOD DS    CL1                                                              
JWPAYDT  DS    CL6                                                              
JWCHKNUM DS    CL8                                                              
JWSWITCH DS    CL1                                                              
*                                                                               
JWSPACE  DS    CL24                JWCOL=177,DDSCOL=200                         
JWCOMMNT DS    CL24                JWREF1 & REF2 - TREAT AS COMMENT             
*                                                                               
         ORG   JWREC+L'JWREC                                                    
         EJECT                                                                  
PPJW03   CSECT                                                                  
         SPACE 1                                                                
*====================================*                                          
* LIST OF CLT/PRD  CODES ON DDS FILE *                                          
* CONSTRUCTED AT RUN TIME            *                                          
*====================================*                                          
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'*CPELST*'                                                    
CPELST   DS    12000XL9           MED(1)/CLT(3)/PRD(3)/EST(2)                   
CPELSTX  EQU   *                                                                
         SPACE 1                                                                
*=======================================*                                       
* LIST OF PUBLICATION CODES ON DDS FILE *                                       
* CONSTRUCTED AT RUN TIME               *                                       
*=======================================*                                       
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'*PUBLST*'                                                    
PUBLST   DS    20000XL7            MED(1)/PUB(6)                                
PUBLSTX  EQU   *                                                                
         EJECT                                                                  
*=======================================*                                       
* TABLE OF CLIENT CONVERSION CODES      *                                       
*=======================================*                                       
         SPACE 2                                                                
CPETABD  DSECT        * DSECT FOR CLT/PRD/EST CONVERSION TABLE *                
CPEJWCLT DS    CL4                                                              
CPEJWPRD DS    CL3                                                              
CPEDDCLT DS    CL3                                                              
CPEDDPRD DS    CL3                                                              
         SPACE 2                                                                
*=======================================*                                       
* TABLE OF ESTIMATE CONVERSION CODES    *                                       
*=======================================*                                       
         SPACE 2                                                                
ESTTABD  DSECT        * DSECT FOR ESTIMATE CONVERSION TABLE *                   
ESTDDCLT DS    CL3                                                              
ESTDDPRD DS    CL3                                                              
ESTJWEST DS    CL6                                                              
ESTDDEST DS    CL3                                                              
         SPACE 2                                                                
*=======================================*                                       
* TABLE OF PUBLICATION CONVERSION CODES *                                       
*=======================================*                                       
         SPACE 2                                                                
PBTABD   DSECT        * DSECT FOR PUBLICATION CONVERSION TABLE *                
PBTJWMED DS    CL1                                                              
PBTJWPUB DS    CL6                                                              
PBTDDMED DS    CL1                                                              
PBTDDPUB DS    XL4                                                              
PBTDDZON DS    XL1                                                              
PBTDDEDT DS    CL1                                                              
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'112PPREPJW03 05/01/02'                                      
         END                                                                    
