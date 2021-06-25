*          DATA SET ACBIL01    AT LEVEL 006 AS OF 07/23/13                      
*PHASE T60E01C                                                                  
*INCLUDE ACJOBCOL                                                               
         TITLE 'ACBIL01 - CREATIVE BILLING - DISPLAY WHOLE JOB'                 
ACBIL01  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,**BIL1**,RR=R2,CLEAR=YES                               
*                                                                               
         USING GWS,R9              R9=A(GLOBAL W/S)                             
         USING TWAD,R8             R8=A(TWA)                                    
         USING LWSD,RC             RC=A(TEMP W/S)                               
*                                                                               
         ST    R2,MYRELO                                                        
*                                                                               
*                                  TRANSLATE LOCAL DICTIONARY ITEMS             
         GOTO1 VDICTAT,DMCB,C'LL  ',DICTB01,DICLS01                             
*                                                                               
*                                  INITIALIZE DETAIL TABLE LINES                
*                                                                               
         USING TABLD,RF                                                         
         LA    RF,TAB              CLEAR 12 DETAIL LINES                        
         LA    R2,12                                                            
*                                                                               
*                                  INITIALIZE THE DETAIL TABLE LINES            
INIT2    XC    0(L'TPARA+L'TCODE+L'TCNAME,RF),0(RF)                             
         MVC   TESTO(TFLDS#*PLAMTLNQ),ZEROS                                     
         LA    RF,TABLEN(,RF)                                                   
         BCT   R2,INIT2                                                         
*                                                                               
         ST    RF,SAVRF            SAVE RF                                      
         XC    0(L'TPARA+L'TCODE+L'TCNAME,RF),0(RF)                             
         LA    R6,TCNAME           CLEAR THE 'OTHERS' LINE                      
         MVC   0(8,R6),AC$OTHRS    OTHERS                                       
         L     RF,SAVRF            RESTORE RF                                   
         MVC   TESTO(TFLDS#*PLAMTLNQ),ZEROS                                     
*                                                                               
         LA    RF,TABLEN(,RF)      CLEAR THE 'TOTALS' LINE                      
         ST    RF,SAVRF            SAVE RF                                      
         XC    0(L'TPARA+L'TCODE+L'TCNAME,RF),0(RF)                             
         LA    R6,TCNAME                                                        
         MVC   0(6,R6),AC$6TOT     TOTAL                                        
         L     RF,SAVRF            RESTORE RF                                   
         MVC   TESTO(TFLDS#*PLAMTLNQ),ZEROS                                     
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
* INITIALIZE SCREEN                                                   *         
***********************************************************************         
         SPACE 1                                                                
         MVC   DETHED,SPACES                                                    
         MVC   DETHED2,SPACES                                                   
*                                                                               
         MVC   DETHED+0(4),AC$PRGRP     PARA                                    
         MVC   DETHED+5(13),AC$WC       WORK CODE                               
*                                                                               
         MVC   DETHED+25(8),AC$8EST     ESTIMATE (RIGHT)                        
         MVC   DETHED+35(8),AC$CHGS     CHARGES  (RIGHT)                        
*                                                                               
         MVC   DETHED+44(9),AC$PRV      PREVIOUS (RIGHT)                        
         MVC   DETHED2+45(8),AC$BLG     BILLING  (RIGHT)                        
*                                                                               
         MVC   DETHED+56(7),AC$THIS     THIS     (RIGHT)                        
         MVC   DETHED2+56(7),AC$BIL     BILL     (RIGHT)                        
*                                                                               
         MVC   DETHED+65(8),AC$TOTAL    TOTAL    (RIGHT)                        
         MVC   DETHED2+65(8),AC$BLG     BILLING  (RIGHT)                        
*                                                                               
         OI    DETHEDH+6,FVOXMT    TRANSMIT                                     
         OI    DETHED2H+6,FVOXMT   TRANSMIT                                     
         EJECT ,                                                                
***********************************************************************         
* READ COMPANY RECORD                                                 *         
***********************************************************************         
         SPACE 1                                                                
         MVI   INTEXTSW,NO                                                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   EXIT                                                             
         L     RE,AIOAREA                                                       
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
INIT5    CLI   0(RE),0             FIND COMPANY ELEMENT                         
         BE    INIT6                                                            
         CLI   0(RE),CPYELQ        X'10' - COMPANY ELEMENT                      
         BE    INIT5A                                                           
         IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     INIT5                                                            
*                                                                               
         USING CPYELD,RE           MAP  COMPANY ELEMENT                         
INIT5A   TM    CPYSTAT2,CPYSEBIF   AND SEE IF WE WANT TO CHANGE HEADS           
         BZ    INIT6                                                            
         MVI   INTEXTSW,YES                                                     
         DROP  RE                                                               
*                                                                               
INIT6    BAS   RE,INITIAL          DO INITIALIZATION                            
*                                                                               
         OC    BILNUMP,BILNUMP     SWAP 'THIS BILL' & 'BILLED TODAY'            
         BNZ   INIT7               IN HEADS DEPENDING ON BILL NUMBER            
*                                                                               
         MVC   DETHED+56(7),AC$BLD      BILLED   (RIGHT)                        
         MVC   DETHED2+56(7),AC$TODAY   TODAY    (RIGHT)                        
         B     INIT9                                                            
*                                                                               
INIT7    MVC   DETHED+56(7),AC$THIS     THIS     (RIGHT)                        
         MVC   DETHED2+56(7),AC$BIL     BILL     (RIGHT)                        
*                                                                               
INIT9    CLI   TWOES,YES                                                        
         BNE   INIT10                                                           
         MVC   TEMP(28),DETHED+35  SHUFFLE HEADLINES TO THE RIGHT               
         MVC   TEMP+40(28),DETHED2+35                                           
         MVI   DETHED+44,C' '                                                   
         MVC   DETHED+45(28),TEMP                                               
         MVI   DETHED2+44,C' '                                                  
         MVC   DETHED2+45(28),TEMP+40                                           
         MVC   DETHED+24(9),AC$ORGL     ORIGINAL (RIGHT)                        
         MVC   DETHED2+24(9),AC$EST     ESTIMATE (RIGHT)                        
*                                                                               
         MVC   DETHED+35(8),AC$PRS      PRESENT  (RIGHT)                        
         MVC   DETHED2+35(8),AC$8EST    ESTIMATE (RIGHT)                        
*                                                                               
         CLI   INTEXTSW,YES                                                     
         BNE   INIT10                                                           
         MVC   DETHED+24(9),AC$EXT      EXTERNAL (ESTIMATE)                     
         MVC   DETHED+35(8),AC$INT      INTERNAL (ESTIMATE)                     
*                                                                               
INIT10   LA    R1,DETFRSTH         CLEAR DISPLAY AREA                           
         ZIC   R2,0(,R1)                                                        
         LA    R3,DETFINLH                                                      
         LR    R4,R2                                                            
         AHI   R4,-9                                                            
*                                                                               
INIT11   EX    R4,INITOC                                                        
         BZ    INIT12                                                           
         EX    R4,INITXC                                                        
         OI    6(R1),FVOXMT        TRANSMIT                                     
*                                                                               
INIT12   BXLE  R1,R2,INIT11                                                     
         B     READ                                                             
*                                                                               
INITOC   OC    8(0,R1),8(R1)                                                    
INITXC   XC    8(0,R1),8(R1)                                                    
         EJECT ,                                                                
***********************************************************************         
* READ JOB AND FILL IN TABLE FROM JOB RECORD                          *         
***********************************************************************         
         SPACE 1                                                                
READ     MVC   KEY,SPACES                                                       
         MVC   KEY(L'JOBKEY),JOBKEY                                             
         GOTO1 AREAD,AIOAREA3                                                   
         BNE   EXIT                                                             
         XC    TEMP,TEMP                                                        
         MVC   TEMP(3),SPACES      PARA NUM                                     
         MVC   TEMP+TESTO-TABLD(TFLDS#*PLAMTLNQ),ZEROS                          
*                                                                               
         BAS   RE,RDOPT                                                         
         BAS   RE,LOOKUP                                                        
*                                                                               
         USING TABLD,RF                                                         
         USING JBLOCKD,R5                                                       
         LA    RF,TEMP             BUILD A LINE-ENTRY IN TEMP                   
         LH    R1,JBNROWS                                                       
*                                                                               
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   READ2               NO                                           
         USING MJETABD,R3                                                       
*                                                                               
READ0    CLI   MJETTYP,MJETTEQ     ARE WE AT THE END?                           
         BE    READ5                                                            
         CLI   MJETTYP,MJETTWQ     NO, LOOK FOR WORKCODE                        
         BNE   READ1                                                            
         OC    MJET1RA,MJET1RA     BUT NOT 1R-LEVEL DETAILS                     
         BNZ   READ1                                                            
         MVC   TCODE,MJETWCD       ESTIMATE ELEMENT                             
         ZAP   TESTO,MJETVAL                                                    
         ZAP   TEST,MJETVAL+6(6)                                                
         BAS   RE,TABADD           ADD IT TO BIG TABLE                          
*                                                                               
READ1    XR    R1,R1                                                            
         IC    R1,MJETLEN                                                       
         AR    R3,R1                                                            
         B     READ0                                                            
         DROP  R3                                                               
*                                                                               
         USING JBCOLD,R3                                                        
READ2    CLI   JBCOLTYP,JBCOLTWC                                                
         BNE   READ4                                                            
*                                                                               
         MVC   TCODE,JBCOLWC       ESTIMATE ELEMENT                             
         ZAP   TESTO,JBCOLVAL                                                   
         ZAP   TEST,JBCOLVAL+6(6)                                               
         BAS   RE,TABADD           ADD IT TO BIG TABLE                          
*                                                                               
READ4    AH    R3,JBLCOL                                                        
         BCT   R1,READ2                                                         
         DROP  R3,R5                                                            
*                                                                               
READ5    L     R2,AIOAREA                                                       
         AH    R2,DATADISP                                                      
*                                                                               
READ6    CLI   0(R2),0                                                          
         BE    TRANS                                                            
         CLI   0(R2),WPBELQ        X'3B' - CREATIVE BILLING ELEMENT             
         BE    READ10                                                           
*                                                                               
READ8    ZIC   RF,1(,R2)                                                        
         AR    R2,RF                                                            
         B     READ6                                                            
*                                                                               
         USING WPBELD,R2                                                        
READ10   LA    RF,TEMP                                                          
         MVC   TCODE,WPBWORK       CREATIVE BILLING ELEMENT                     
         ZAP   TPREVBIL,WPBSOFA                                                 
         ZAP   TTOTBIL,WPBSOFA                                                  
         OC    BILNUMP,BILNUMP     IF NO BILL NUMBER THIS-BILL CONTAINS         
         BNZ   READ12              TODAY'S BILLING                              
         CLC   WPBDATE,TODAYP                                                   
         BNE   READ12                                                           
         ZAP   TTHISBIL,WPBTODA                                                 
         SP    TPREVBIL,WPBTODA                                                 
*                                                                               
READ12   BAS   RE,TABADD                                                        
         B     READ8                                                            
         DROP  R2,RF                                                            
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO ADD A LINE IN TEMP TO BIG TABLE                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TABLD,RF                                                         
         SPACE 1                                                                
TABADD   NTR1                                                                   
         LA    R2,12               FIRST 12 LINES                               
         LA    RF,TAB                                                           
         LA    RE,TEMP                                                          
*                                                                               
TABAD2   OC    TPARA,TPARA                                                      
         BZ    TABAD4                                                           
*                                  W/C                                          
         CLC   TEMP+TCODE-TABLD(L'TCODE),SPACES                                 
         BE    TABAD3              PARA WITHOUT W/C                             
         CLC   TCODE,TEMP+(TCODE-TABLD)                                         
         BE    TABAD4                                                           
*                                                                               
TABAD3   LA    RF,TABLEN(,RF)                                                   
         BCT   R2,TABAD2           IF WE DROP THROUGH-WE ARE                    
         B     TABAD5              POINTING TO OTHERS-LINE                      
*                                                                               
TABAD4   MVC   TCODE,TCODE-TABLD(RE)                                            
         OC    TCNAME-TABLD(L'TCNAME,RE),TCNAME-TABLD(RE)                       
         BZ    TABAD5                                                           
*                                  MOVE NAME IN - IF WE HAVE ONE                
         MVC   TCNAME,TCNAME-TCNAME(RE)                                         
*                                                                               
TABAD5   CLC   TPARA,SPACES        MOVE IN PARA NUM OR APPEND + IF              
         BNH   TABAD5A             THERE'S ONE ALREADY                          
         CLC   TPARA-TABLD(L'TPARA,RE),SPACES                                   
         BE    TABAD6                                                           
         MVI   TPARA+2,C'+'                                                     
         B     TABAD6                                                           
*                                                                               
TABAD5A  MVC   TPARA,TPARA-TABLD(RE)                                            
*                                                                               
TABAD6   LA    R1,TFLDS#           NUMBER OF TEST FIELDS                        
         LA    R2,TESTO            ->   FIRST PACKED FIELD                      
         LA    RE,TESTO-TABLD(,RE)                                              
*                                                                               
TABAD8   AP    0(L'TEST,R2),0(L'TEST,RE)                                        
         LA    RE,L'TEST(,RE)                                                   
         LA    R2,L'TEST(,R2)                                                   
         BCT   R1,TABAD8                                                        
*                                  NOW ADD TO TOTAL LINE                        
         LA    RF,TAB                                                           
         LA    R0,TABLEN                                                        
         MHI   R0,13               THE TOTAL LINE IS LINE 13                    
         AR    RF,R0               POINT TO TOTAL LINE                          
         LA    R1,TFLDS#                                                        
         LA    R2,TESTO                                                         
         LA    RE,TEMP+TESTO-TABLD                                              
*                                                                               
TABAD10  AP    0(L'TEST,R2),0(L'TEST,RE)                                        
         LA    RE,L'TEST(,RE)                                                   
         LA    R2,L'TEST(,R2)                                                   
         BCT   R1,TABAD10                                                       
*                                                                               
         XC    TEMP,TEMP           RESET FOR NEXT ELEMENT                       
         MVC   TEMP(3),SPACES                                                   
         MVC   TEMP+TESTO-TABLD(TFLDS#*PLAMTLNQ),ZEROS                          
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
* READ TRANSACTIONS AND POST TO TABLE                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R2                                                        
         SPACE 1                                                                
TRANS    L     R2,AIOAREA3                                                      
         AH    R2,DATADISP                                                      
*                                                                               
TRANS2   L     RF,AIOAREA3         SAME JOB                                     
         CLC   JOBKEY,0(RF)                                                     
         BNE   WRKREAD             NO-TRY TO MERGE IN TODAYS BILLING            
         CLI   0(R2),TRNELQ        X'44' - TRANSACTION ELEMENT                  
         BE    TRANS10                                                          
*                                                                               
TRANS4   GOTO1 ASEQ,AIOAREA3                                                    
         B     TRANS2                                                           
*                                                                               
         USING TRNRECD,RF                                                       
TRANS10  TM    TRNRSTAT,TRNSDRFT   IS THIS A DRAFT ?                            
         BO    TRANS4              YES, SKIP IT                                 
         DROP  RF                                                               
*                                                                               
         TM    TRNSTAT,TRNSDR      MUST BE DEBIT TO POST                        
         BZ    WRKREAD             ELSE WE ARE IN BILLS AND CAN FINISH          
         CP    TRNAMNT,=P'0'                                                    
         BE    TRANS4              IGNORE ZERO AMTS, EG UNMATCHED ORDS          
*                                                                               
         USING TABLD,RF                                                         
         LA    RF,TEMP                                                          
         MVC   TCODE,TRNANAL                                                    
         ZAP   TCHARGE,TRNAMNT                                                  
         BAS   RE,TABADD                                                        
         B     TRANS4              GO BACK FOR MORE                             
         DROP  R2,RF                                                            
         EJECT ,                                                                
***********************************************************************         
* READ A LIBRARY BOOK                                                 *         
***********************************************************************         
         SPACE 1                                                                
WRKREAD  OC    BILNUMP,BILNUMP     IF THEY INPUT A BILL NUMBER                  
         BZ    DISPLAY             IOAREA1 CONTAINS LIB BOOK HEADER             
         MVI   FLAG,0              SAVE BILLED STATUS                           
         MVI   LPARA,0                                                          
         GOTO1 AFINDBIL,AIOAREA1                                                
         BNE   EXIT                                                             
*                                                                               
         USING HEADERD,R7                                                       
         L     R7,AIOAREA1                                                      
         CLI   HBILL,NO                                                         
         BE    WRKREAD2                                                         
         MVI   FLAG,BILLED                                                      
*                                                                               
WRKREAD2 LA    R0,1                PREPARE PARA READING BXLE                    
         LR    R4,R0                                                            
         ZIC   R5,WRKLPARA                                                      
*                                                                               
         USING TABLD,R2                                                         
         USING PARAD,R7                                                         
         LA    R2,TEMP                                                          
         LA    R1,AIOAREA1                                                      
         L     RF,AWRKLGET                                                      
*                                                                               
WRKREAD4 STC   R0,WRKPARA          READ AND POST PARA VALS                      
         BASR  RE,RF                                                            
         BNE   EXIT                                                             
         EDIT  (R0),(2,TPARA),FILL=0                                            
         MVC   TCODE,PARAWRK                                                    
         OC    TCODE,SPACES                                                     
         ZAP   TTHISBIL,PARANET                                                 
         CLI   FLAG,BILLED                                                      
         BE    WRKREAD5                                                         
         ZAP   TTOTBIL,PARANET     IF NOT BILLED THIS BILL IS EXTRA             
         B     WRKREAD6                                                         
*                                                                               
WRKREAD5 SP    TPREVBIL,PARANET    IF BILLED THIS BILL IS IN PREVBIL            
*                                                                               
WRKREAD6 BAS   RE,TABADD                                                        
         BXLE  R0,R4,WRKREAD4                                                   
         B     DISPLAY                                                          
         DROP  R2,R7                                                            
         EJECT ,                                                                
***********************************************************************         
* FORMAT TABLE TO SCREEN AND EXIT                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TABLD,R2                                                         
         SPACE 1                                                                
DISPLAY  LA    R0,12                                                            
         LA    R2,TAB                                                           
*                                                                               
DISP2    OC    TCNAME,TCNAME       FILL IN MISSING W-CODE NAMES                 
         BNZ   DISP4                                                            
         OC    TCODE,TCODE                                                      
         BZ    DISP4                                                            
         GOTO1 AGETWC,TCODE                                                     
         MVC   TCNAME,WORK                                                      
*                                                                               
DISP4    LA    R2,TABLEN(,R2)                                                   
         BCT   R0,DISP2                                                         
         DROP  R2                                                               
*                                                                               
         USING LINED,RE                                                         
         USING TABLD,RF                                                         
         LA    RF,TAB                                                           
         LA    RE,DETFRSTH                                                      
         LA    R2,14                                                            
*                                                                               
DISP10   MVC   LPARAGH,TPARA       BUILD FIELDS IN SCREEN LINE                  
*                                  NO ACTIVITY - SKIP LINE                      
         CLC   TESTO(TFLDS#*PLAMTLNQ),ZEROS                                     
         BE    DISP16                                                           
         MVC   LWCODE(2),TCODE                                                  
         MVC   LWCODE+3(L'TCNAME),TCNAME                                        
         LA    R3,LEST                                                          
         LA    R4,TEST             NOW THE NUMBERS                              
         CLI   TWOES,YES           IF SHOWING BOTH EST VALUES                   
         BNE   *+8                                                              
         LA    R4,TESTO            START ONE FIELD EARLIER                      
         LA    R5,LDSP#            NUMBER OF FIELDS TO DISPLAY                  
*                                                                               
DISP12   CP    0(L'TEST,R4),=P'0'                                               
         BE    DISP14                                                           
         ZAP   DIV,0(L'TEST,R4)                                                 
         AP    DIV,=P'50'                                                       
         CP    DIV,=P'0'                                                        
         BH    *+10                                                             
         SP    DIV,=P'100'                                                      
         DP    DIV,=P'100'                                                      
         EDIT  (P8,DIV),(L'LEST,0(R3)),FLOAT=-                                  
*                                                                               
DISP14   LA    R3,L'LEST+1(,R3)                                                 
         LA    R4,L'TEST(,R4)                                                   
         BCT   R5,DISP12                                                        
*                                                                               
         OI    6(RE),FVOXMT        TRANSMIT                                     
         LA    R0,LLEN             BUMP SCREEN REGISTER                         
         AR    RE,R0                                                            
*                                                                               
DISP16   LA    R0,TABLEN           BUMP TABLE REGISTER                          
         AR    RF,R0                                                            
         BCT   R2,DISP10                                                        
         DROP  RE,RF                                                            
*                                                                               
         MVI   FERN,OK                                                          
*                                  JOB DETAILS DISPLAYED -                      
         MVC   FVMSGNO,=AL2(96)      ENTER NEXT ACTION'                         
         MVI   FVMTYPE,FVMINFO                                                  
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                   RETURN TO CALLER                             
         EJECT ,                                                                
         SPACE 1                                                                
INITIAL  NTR1                                                                   
         LA    R2,CORETAB                                                       
         LA    R3,CORETABL                                                      
         LA    R4,COREFACS                                                      
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
INIT020  MVC   DMCB+7(1),0(R2)     SET MODULE NUMBER                            
         GOTO1 VCALLOV,DMCB,0                                                   
         MVC   0(4,R4),0(R1)       SAVE ADDRESS                                 
         LA    R2,1(,R2)                                                        
         LA    R4,4(,R4)                                                        
         BCT   R3,INIT020                                                       
*                                                                               
         L     RF,=V(ACJOBCOL)                                                  
         A     RF,MYRELO                                                        
         ST    RF,VJOBCOL                                                       
*                                                                               
*                                  GET  ACBIL50 - OVERLAY PHASE JOBBER          
         GOTO1 VCALLOV,DMCB,('OVPHJOBR',0),0,0                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)          GET A(TABLES)                                
*                                                                               
         LM    R0,R1,0(RF)         GET DISP TO/LENGTH OF COLUMN TABLE           
         AR    R0,RF                                                            
         STM   R0,R1,ACOLTAB                                                    
*                                                                               
         LM    R0,R1,8(RF)                                                      
         AR    R0,RF                                                            
         STM   R0,R1,AOPVTAB                                                    
*                                                                               
         LA    RF,IO2              SET ADDRESSES                                
         ST    RF,AIO2                                                          
         LA    RF,COLIST                                                        
         ST    RF,ACOLIST                                                       
         LA    RF,JOBLOCKA                                                      
         ST    RF,AJOBLOCK                                                      
*                                                                               
         GOTO1 VJOBCOL,DMCB,LOOKFLDH,ACOLIST,ACOMFACS                           
         CLI   4(R1),0                                                          
         BNE   INITIALX                                                         
         DC    H'0'                                                             
*                                                                               
INITIALX B     EXIT                RETURN TO CALLER                             
         EJECT ,                                                                
         SPACE 1                                                                
         USING GOBLOCKD,R6                                                      
         SPACE 1                                                                
RDOPT    NTR1                                                                   
         L     R6,AGOBLOCK                                                      
         MVC   GOADM,VDATAMGR                                                   
         L     R4,AIOAREA                                                       
         MVC   GOSELCUL,0(R4)                                                   
         LA    R4,3(,R4)                                                        
*                                                                               
         MVC   GOSELCLI,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,PRODHEIR                                                      
         BCTR  R1,0                                                             
         EXMVC R1,GOSELCLI,0(R4)                                                
         LA    R4,1(R1,R4)                                                      
*                                                                               
         MVC   GOSELPRO,SPACES                                                  
         SR    R3,R3                                                            
         IC    R3,PRODHEIR                                                      
         IC    R1,PRODHEIR+1                                                    
         SR    R1,R3                                                            
         BCTR  R1,0                                                             
         EXMVC R1,GOSELPRO,0(R4)                                                
         LA    R4,1(R1,R4)                                                      
*                                                                               
         MVC   GOSELJOB,SPACES                                                  
         IC    R3,PRODHEIR+1                                                    
         IC    R1,PRODHEIR+2                                                    
         SR    R1,R3                                                            
         BCTR  R1,0                                                             
         EXMVC R1,GOSELJOB,0(R4)                                                
*                                                                               
         MVI   GOWHICH,0                                                        
*                                                                               
         GOTO1 VGETOPT,DMCB,AGOBLOCK                                            
*                                                                               
RDOPTX   B     EXIT                RETURN TO CALLER                             
         DROP  R6                                                               
         EJECT ,                                                                
         SPACE 1                                                                
         USING JBLOCKD,R5                                                       
         SPACE 1                                                                
LOOKUP   NTR1                                                                   
         L     R5,AJOBLOCK                                                      
         MVC   JBAJOB,AIOAREA                                                   
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOMFACS                                                  
         MVC   JBAGOBLK,AGOBLOCK                                                
         MVC   JBAIO,AIO2                                                       
         MVC   JBAKEY,AIOAREA                                                   
*                                                                               
         MVC   JBGETOPT,VGETOPT                                                 
*                                                                               
         MVC   JBACOLTB,ACOLTAB                                                 
         MVC   JBLCOLTB,LCOLTAB                                                 
         MVC   JBAOPVTB,AOPVTAB                                                 
         MVC   JBLOPVTB,LOPVTAB                                                 
*                                                                               
         MVI   JBSELFUN,JBGETDE    GET BO DETAILS                               
         LA    RE,LOOKFLDH                                                      
         ST    RE,JBORICLI                                                      
*                                                                               
         GOTO1 VJOBBER,DMCB,AJOBLOCK,0                                          
         CLI   JBERROR,X'00'                                                    
         BE    LOOKUPX                                                          
         DC    H'0'                                                             
*                                                                               
LOOKUPX  L     R3,ACOLTAB                                                       
         XIT1  REGS=(R3,R5)                                                     
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
* CALL DICTATE                                                        *         
*                                                                     *         
* INPUT:                                                              *         
*   R6=  ADDRESS OF FIELD TO BE TRANSLATED                            *         
*        NOTE: THE FIELD MUST HAVE BEEN PREVIOUSLY INITIALIZED WITH   *         
*              AN MVCDD INSTRUCTION.                                  *         
***********************************************************************         
         SPACE 1                                                                
CALLDICT DS    0H                                                               
         ST    RE,SAVRE            SAVE     RE                                  
         GOTO1 VDICTAT,DMCB,C'SL  ',(R6),0                                      
         L     RE,SAVRE            RESTORE  RE                                  
         BSM   0,RE                RETURN                                       
         EJECT ,                                                                
         SPACE 1                                                                
LOOKFLDH DC    AL1(L'LOOKFLD+8),4X'00',AL1(L'LOOKFLD),2X'00'                    
LOOKFLD  DC    C'OE,CE'                                                         
         DC    C' '                BLANK                                        
         SPACE 3                                                                
DICTB01  DS    0X                  LOCAL DICTIONARY ITEMS                       
         DCDDL AC#BIL,7,R          BILL                                         
         DCDDL AC#BLD,7,R          BILLED                                       
         DCDDL AC#BLG,8,R          BILLING                                      
         DCDDL AC#CHGS,8,R         CHARGES                                      
         DCDDL AC#EST,9,R          ESTIMATE                                     
*                                  ESTIMATE                                     
         DCDDL AC#EST,8,R,LABEL=AC@EST8                                         
         DCDDL AC#EXT,9,R          EXTERNAL                                     
         DCDDL AC#INT,8,R          INTERNAL                                     
         DCDDL AC#ORGL,9,R         ORIGINAL                                     
         DCDDL AC#OTHRS,8          OTHERS                                       
         DCDDL AC#PRGRP,4          PARAGRAPH                                    
         DCDDL AC#PRS,8,R          PRESENT                                      
         DCDDL AC#PRV,9,R          PREVIOUS                                     
         DCDDL AC#THIS,7,R         THIS                                         
         DCDDL AC#TOTAL,8,R        TOTAL                                        
*                                  TOTAL                                        
         DCDDL AC#TOTAL,6,LABEL=AC@TOTA6                                        
         DCDDL AC#TODAY,7,R        TODAY                                        
         DCDDL AC#WC,13            WORK CODE                                    
*                                                                               
DICTB01X DC    AL1(EOT)                                                         
         SPACE 3                                                                
CORETAB  DS    0X                  CORE-RESIDENT PHASE NUMBERS                  
         DC    AL1(QJOBBER)                                                     
CORETABL EQU   *-CORETAB                                                        
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* DSECT FOR SCREEN LINE                                               *         
***********************************************************************         
         SPACE 1                                                                
LINED    DSECT                                                                  
LHEAD    DS    CL8                 FIELD HEADER                                 
LPARAGH  DS    CL3                                                              
         DS    CL1                                                              
LWCODE   DS    CL18                                                             
         DS    CL1                                                              
LEST     DS    CL9                                                              
         DS    CL1                                                              
LCHARGE  DS    CL(L'LEST)                                                       
         DS    CL1                                                              
LPREVBIL DS    CL(L'LEST)                                                       
         DS    CL1                                                              
LTHISBIL DS    CL(L'LEST)                                                       
         DS    CL1                                                              
LTOTBIL  DS    CL(L'LEST)                                                       
LDSP#    EQU   (*-LEST+1)/L'LEST                                                
LLEN     EQU   *-LHEAD                                                          
         EJECT ,                                                                
***********************************************************************         
* DSECT FOR LINE IN DETAIL TABLE                                      *         
***********************************************************************         
         SPACE 1                                                                
TABLD    DSECT                                                                  
TPARA    DS    CL3                                                              
TCODE    DS    CL2                                                              
TCNAME   DS    CL15                                                             
*                                                                               
TESTO    DS    PL(PLAMTLNQ)                                                     
TEST     DS    PL(PLAMTLNQ)                                                     
TCHARGE  DS    PL(PLAMTLNQ)                                                     
TPREVBIL DS    PL(PLAMTLNQ)                                                     
TTHISBIL DS    PL(PLAMTLNQ)                                                     
TTOTBIL  DS    PL(PLAMTLNQ)                                                     
TFLDS#   EQU   (*-TESTO)/PLAMTLNQ  NUMBER OF PACKED FIELDS                      
*                                                                               
TABLEN   EQU   *-TPARA                                                          
         EJECT ,                                                                
***********************************************************************         
* DSECT FOR TEMP WORKING STORAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
MYRELO   DS    A                                                                
VJOBCOL  DS    V                                                                
*                                                                               
SAVRE    DS    F                   SAVE AREA FOR  RE                            
SAVRF    DS    F                   SAVE AREA FOR  RF                            
*                                                                               
AIO2     DS    A                   A(IO2 FOR JOBBER)                            
ACOLIST  DS    A                   A(COLUMN LIST)                               
AJOBLOCK DS    A                   A(JOBBER BLOCK)                              
*                                                                               
ACOLTAB  DS    A                   A(COLUMN OUTPUT TABLE)                       
LCOLTAB  DS    F                   L'COLUMN OUTPUT TABLE                        
AOPVTAB  DS    A                   A(OPERAND VALUE TABLE)                       
LOPVTAB  DS    F                   L'OPERAND VALUE TABLE                        
*                                                                               
COREFACS DS    0A                                                               
VJOBBER  DS    A                   A(JOBBER)                                    
*                                                                               
DIV      DS    CL10                                                             
INTEXTSW DS    CL1                                                              
*                                                                               
TAB      DS    14CL(TABLEN)                                                     
*                                                                               
DICLS01  DS    0X                  LOCAL DICTIONARY ITEMS                       
AC$BIL   DS    CL7                 BILL      (RIGHT)                            
AC$BLD   DS    CL7                 BILLED    (RIGHT)                            
AC$BLG   DS    CL8                 BILLING   (RIGHT)                            
AC$CHGS  DS    CL8                 CHARGES   (RIGHT)                            
AC$EST   DS    CL9                 ESTIMATE  (RIGHT)                            
AC$8EST  DS    CL8                 ESTIMATE  (RIGHT)                            
AC$EXT   DS    CL9                 EXTERNAL  (RIGHT)                            
AC$INT   DS    CL8                 INTERNAL  (RIGHT)                            
AC$ORGL  DS    CL9                 ORIGINAL  (RIGHT)                            
AC$OTHRS DS    CL8                 OTHERS                                       
AC$PRGRP DS    CL4                 PARAGRAPH                                    
AC$PRS   DS    CL8                 PRESENT   (RIGHT)                            
AC$PRV   DS    CL9                 PREVIOUS  (RIGHT)                            
AC$THIS  DS    CL7                 THIS      (RIGHT)                            
AC$TOTAL DS    CL8                 TOTAL     (RIGHT)                            
AC$6TOT  DS    CL6                 TOTAL                                        
AC$TODAY DS    CL7                 TODAY     (RIGHT)                            
AC$WC    DS    CL13                WORK CODE                                    
*                                                                               
DICLS01X DS    AL1                 EOT                                          
*                                                                               
IO2      DS    CL2000              IO AREA FOR JOBBER                           
COLIST   DS    CL200                                                            
*                                                                               
JOBLOCKA DS    (JBLOCKL)X                                                       
LWSX     DS    0C                                                               
         EJECT ,                                                                
         SPACE 1                                                                
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT ,                                                                
         SPACE 1                                                                
*ACBILDSECT                                                                     
       ++INCLUDE ACBILDSECT                                                     
*ACJOBBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
*ACBILWORK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACBILWORK                                                      
         PRINT ON                                                               
         EJECT ,                                                                
         SPACE 1                                                                
*ACBILFED                                                                       
       ++INCLUDE ACBILFED                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACBIL01   07/23/13'                                      
         END                                                                    
