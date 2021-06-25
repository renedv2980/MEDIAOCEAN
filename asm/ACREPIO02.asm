*          DATA SET ACREPIO02  AT LEVEL 010 AS OF 06/06/12                      
*PHASE ACIO02A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'INTER-OFFICE BALANCING'                                         
ACIO02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACIO**,R9,R8                                                 
         USING ACWORKD,RA                                                       
         L     RA,0(R1)                                                         
         USING ACIOD,RC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    SETINIT                                                          
         CLI   MODE,PROCOPTS                                                    
         BE    SREQ                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
XIT      XIT1  ,                                                                
*                                                                               
         USING MASTD,R5                                                         
SETINIT  L     R5,ADMASTC                                                       
         MVC   UPSI,MCUPSI                                                      
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAYC)                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAYP)                                
         ZAP   BATCNT,=P'0'                                                     
         ZAP   POSTCASH,=P'0'                                                   
         ZAP   POSTCNT,=P'0'                                                    
         MVI   RCSUBPRG,1                                                       
         MVI   FCRQOPT,FCRQOPTQ    INDICATE MODE OF QOPTS REQUIRED              
         MVI   RCFLAG1,RCFREPLC    LOWER CASE SPECS                             
*                                                                               
SETINTX  B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* BUILD SUPPLEMENTARY REQUEST REPORT                                  *         
*---------------------------------------------------------------------*         
*                                                                               
SREQ     L     R4,ASRQTAB          R4=A(REQUEST REPORT TABLE)                   
         USING SRQTABD,R4                                                       
         XR    R3,R3                                                            
*                                                                               
SREQ2    MVC   P,SPACES            CLEAR PRINT LINE                             
         XR    R2,R2                                                            
         ICM   R2,3,SRQDISP        R2=DISPLACEMENT TO REQUEST FIELD             
         BZ    SREQX                                                            
         LA    R2,ACWORKD(R2)                                                   
         IC    R3,SRQLEN           R3=L'REQUEST FIELD                           
         EX    R3,*+8              TEST FIELD SET                               
         BE    SREQ4                                                            
         CLC   0(0,R2),SPACES                                                   
*                                                                               
         XC    WORK,WORK           CALL DICTATE TO SET LOWER CASE               
         MVC   WORK,SRQDD                                                       
         GOTO1 ADDICTAT,DMCB,C'SL  ',WORK                                       
         MVC   P+24(L'SRQDD),WORK  PRINT DESCRIPTION OF THE FIELD               
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+45(0),0(R2)       FOLLOWED BY CONTENTS OF THE FIELD            
         GOTO1 PRINT,DMCB,P,=C'BL02'                                            
*                                                                               
SREQ4    LA    R4,SRQQ(R4)                                                      
         B     SREQ2                                                            
*                                                                               
SREQX    MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* GET REQUEST CRITERIA                                                *         
*                                                                     *         
* BATCH GROUP = TBAKGRUP = QCOMMENT                                   *         
* BATCH TYPE = TBAKBTYP = QCOMMENT+1                                  *         
* BATCH REFERENCE = TBABREF = QAPPL+8                                 *         
* PERSON = QAPPL                                                      *         
* START DATE = QSTART                                                 *         
* END DATE = QEND                                                     *         
* BATCH MOA = TBAKBMOS = QMOSSTRT(QMOSEND)                            *         
* REVERSAL = QOPT2                                                    *         
* LIVE = QOPT1                                                        *         
* NARRATIVE = QCARD5                                                  *         
*---------------------------------------------------------------------*         
*                                                                               
REQF     MVC   SECCODE,ALPHAID                                                  
         BAS   RE,GETSECA          GET SECURITY ALPHA                           
*                                                                               
         CLI   QOPT1,C'Y'          IS THIS A LIVE RUN?                          
         BE    *+12                YES                                          
         MVI   RCWRITE,NO          NO DON'T WRITE TO THE FILE                   
         MVI   RCPOSTNG,NO          OR MAKE POSTINGS                            
*                                                                               
         BAS   RE,GETINT           GET THE INTER-OFFICE ACCOUNT                 
         BAS   RE,GETNAR           GET THE NARRATIVE                            
         BAS   RE,GETLED           GET THE LEDGER                               
         BAS   RE,OPENWRK          OPEN THE WORKER FILE                         
*                                                                               
         XC    BALOFF,BALOFF                                                    
         XC    BATMOS,BATMOS       SET START/END MOA                            
         MVC   BATMOE,=X'FFFF'                                                  
         XC    BATSTR,BATSTR       SET START/END DATES                          
         MVC   BATEND,=X'FFFF'                                                  
         XC    BATPER,BATPER       SET PERSON                                   
*                                                                               
         CLC   QMOSSTRT,SPACES                                                  
         BNH   REQF02                                                           
         MVC   WORK(4),QMOSSTRT                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   BATMOS,WORK+6                                                    
*                                                                               
REQF02   CLC   QMOSEND,SPACES                                                   
         BNH   REQF04                                                           
         MVC   WORK(4),QMOSEND                                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   BATMOE,WORK+6                                                    
*                                                                               
REQF04   CLC   QSTART,SPACES                                                    
         BNH   REQF06                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(2,BATSTR)                                
*                                                                               
REQF06   CLC   QEND,SPACES                                                      
         BNH   REQF08                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(2,BATEND)                                  
*                                                                               
REQF08   CLC   QAPPL(8),SPACES     FILTERING BY PERSON?                         
         BE    *+8                 NO                                           
         BAS   RE,GETPERS                                                       
*                                                                               
         XC    BATTYP,BATTYP       GET BATCH TYPE                               
         CLC   QCOMMENT+1(2),SPACES                                             
         BE    REQF10                                                           
         CLI   QCOMMENT+2,C' '                                                  
         BNE   *+14                                                             
         MVC   QCOMMENT+2(1),QCOMMENT+1                                         
         MVI   QCOMMENT+1,C' '                                                  
         PACK  DUB,QCOMMENT+1(2)                                                
         CVB   R1,DUB                                                           
         STC   R1,BATTYP                                                        
*                                                                               
REQF10   XC    BATGRP,BATGRP       GET BATCH GROUP                              
         CLI   QCOMMENT,C' '                                                    
         BE    *+10                                                             
         MVC   BATGRP,QCOMMENT                                                  
*                                                                               
         XC    BATREF,BATREF       GET BATCH REFERENCE                          
         CLC   QAPPL+8(4),SPACES                                                
         BE    *+10                                                             
         MVC   BATREF,QAPPL+8                                                   
*                                                                               
*---------------------------------------------------------------------*         
* READ THE BATCH HEADERS AND APPLY CRITERIA                           *         
*---------------------------------------------------------------------*         
*                                                                               
         XC    DKEY,DKEY                                                        
         LA    R5,DKEY                                                          
         USING TBARECD,R5                                                       
         MVI   TBAKTYP,TBAKTYPQ    SET KEY FOR BATCH HEADER RECORDS             
         MVC   TBAKCPY,QCOMPANY                                                 
*                                                                               
REQF20   BAS   RE,HIGH             READ FOR KEY                                 
         B     *+8                                                              
*                                                                               
REQF22   BAS   RE,SEQ              READ NEXT RECORD                             
         MVI   HIT,0                                                            
         LA    R5,DIR                                                           
         CLC   DIR(TBAKUSER-TBAKEY),DKEY                                        
         BNE   REQFX                                                            
*                                                                               
         TM    UPSI,UPSIALL                                                     
         BNO   *+12                                                             
         LA    R6,=C'DDIR'                                                      
         BAS   RE,DMPDIR                                                        
*                                                                               
         OC    TBAKTSEQ,TBAKTSEQ   IS THIS A HEADER?                            
         BNZ   REQF80              NO, GO PROCESS THE DETAIL                    
         MVC   DHEDSV,DIR          SAVE THE KEY OF THE HEADER                   
         TM    TBAKHSTA,TBAHSUPD   SKIP BATCHES NOT UPDATED                     
         BZ    REQF24                                                           
         TM    TBAKHSTA,TBAHSDEL   OR DELETED                                   
         BNO   REQF26                                                           
*                                                                               
REQF24   MVC   DKEY,DHEDSV         RESTORE HEADER KEY                           
         LA    R5,DKEY                                                          
         MVC   TBAKTSEQ,=X'FFFF'                                                
         B     REQF20                                                           
*                                                                               
REQF26   CLC   BATMOS,=X'0000'     FILTERING BY MOA?                            
         BE    REQF28              NO, TEST NEXT                                
         CLC   TBAKBMOS,BATMOS                                                  
         BL    REQF24              NO MATCH, GET NEXT HEADER                    
*                                                                               
REQF28   CLC   BATMOE,=X'0000'                                                  
         BE    REQF30                                                           
         CLC   TBAKBMOS,BATMOE                                                  
         BH    REQF24              NO MATCH, GET NEXT HEADER                    
*                                                                               
REQF30   CLI   BATTYP,0            FILTERING BY TYPE?                           
         BE    REQF32              NO, TEST NEXT                                
         CLC   TBAKBTYP,BATTYP                                                  
         BNE   REQF24              NO MATCH, GET NEXT HEADER                    
*                                                                               
REQF32   CLI   BATGRP,0            FILTERING BY GROUP?                          
         BE    REQF34              NO, TEST NEXT                                
         CLC   TBAKGRUP,BATGRP                                                  
         BNE   REQF24              NO MATCH, GET NEXT HEADER                    
*                                                                               
REQF34   CLI   BATREF,0            FILTERING BY REFERENCE?                      
         BE    REQF36              NO, TEST NEXT                                
         CLC   TBAKBREF,BATREF                                                  
         BNE   REQF24              NO MATCH, GET NEXT HEADER                    
*                                                                               
REQF36   CLI   BATPER,0            FILERING BY PERSON?                          
         BE    REQF38              NO, TEST NEXT                                
         CLC   TBAKBCHR,BATPER                                                  
         BNE   REQF24              NO MATCH, GET NEXT HEADER                    
*                                                                               
REQF38   CLI   BATSTR,0            FILTERING BY START DATE?                     
         BE    REQF40              NO, TEST NEXT                                
*                                                                               
         SR    RF,RF               TEST AGAINST UPDATE DATE                     
         ICM   RF,3,TBAHKUDT                                                    
         CLM   RF,3,BATSTR                                                      
         BL    REQF24              BEFORE START, GET NEXT HEADER                
*                                                                               
REQF40   CLI   BATEND,0            FILTERING BY END DATE?                       
         BE    REQF60              NO, GET THE HEADER                           
         CLM   RF,3,BATEND                                                      
         BH    REQF24              AFTER END, GET NEXT HEADER                   
         DROP  R5                                                               
*                                                                               
*---------------------------------------------------------------------*         
* GET THE BATCH HEADER AND CHECK IF BALANCED                          *         
*---------------------------------------------------------------------*         
*                                                                               
REQF60   L     R3,AIO1                                                          
         BAS   RE,GET              SEE IF BATCH ALREADY BALANCED                
         BE    *+6                                                              
         DC    H'00'                                                            
         USING BHDELD,R2                                                        
         USING TBARECD,R3                                                       
         LA    R2,TBARFST                                                       
*                                                                               
REQF62   CLI   0(R2),0             EOF?                                         
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R2),BHDELQ        LOOK FOR HEADER ELEMENT                      
         BE    REQF64                                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     REQF62                                                           
*                                                                               
REQF64   TM    BHDSTAT2,BHDSBDON   IS BATCH MARKED BALANCED?                    
         BO    REQF24              YES, GET NEXT HEADER                         
         MVC   SAVEKEY,DIR         SAVE THE KEY OF THE HEADER                   
         BAS   RE,CLEARE2          CLEAR THE E2 TABLE                           
         B     REQF22              NO, GET DETAIL HEADER NOW                    
*                                                                               
*---------------------------------------------------------------------*         
* GET THE ITEM RECORD AND SEE IF ELIGIBLE FOR BALANCING               *         
*---------------------------------------------------------------------*         
*                                                                               
REQF80   L     R3,AIO1                                                          
         BAS   RE,GET              GET THE DETAIL RECORD                        
*                                                                               
         L     R5,AIO1                                                          
         USING BIAELD,R2                                                        
         LA    R2,TBARFST                                                       
*                                                                               
REQF82   CLI   0(R2),0             EOF?                                         
         BE    REQF22                                                           
         CLI   0(R2),BIAELQ        LOOK FOR ITEM ELEMENT                        
         BNE   REQF84                                                           
*                                                                               
         CLI   BIALN,BIALN4Q       LONG ENOUGH TO HOLD BALANCE INFO?            
         BL    REQF22              NO                                           
         CLI   BIASTAT,BIAELG      ELIGIBLE FOR BALANCING?                      
         BNE   REQF22              NO                                           
         MVC   BALOFF,BIAIOFF      YES, SAVE BALANCING OFFICE                   
         MVI   HIT,HITBAL          INDICATE WE ARE BALANCING                    
*                                                                               
REQF84   CLI   0(R2),ASKELQ        GET THE KEY ELEMENT NOW                      
         BE    REQF100                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     REQF82                                                           
*                                                                               
*---------------------------------------------------------------------*         
* GO OFF AND READ E2 ELEMENTS AND BUILD NEW POSTINGS                  *         
*---------------------------------------------------------------------*         
*                                                                               
REQF100  BAS   RE,BUILDE2          BUILD TABLE OF E2S                           
         BAS   RE,OFFNAME            GET OFFICE NAME                            
         BAS   RE,READKEYS         READ E2'S AND SAVE DATA                      
         BNE   REQF24              ERROR - BATCH CAN'T BE BALANCED              
         BAS   RE,BLDPOST          BUILD AND MAKE POSTINGS                      
         BAS   RE,UPBATCH          UPDATE BATCH HEADER                          
         MVI   SPACING,3                                                        
         BAS   RE,PRINTIT          SKIP A LINE BEFORE NEXT BATCH                
         AP    BATCNT,=P'1'        YES, ADD TO RECORDS PROCESSED                
         B     REQF24              GET NEXT BATCH HEADER                        
*                                                                               
REQFX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PRINT TOTAL OF RECORDS PROCESSED                             *         
*---------------------------------------------------------------------*         
*                                                                               
RUNL     BAS   RE,PRINTIT          SKIP A LINE                                  
         LA    R6,P                                                             
         USING PLINE,R6                                                         
         MVC   P(11),=C'** TOTAL **'                                            
         EDIT  BATCNT,(7,P+13),0,MINUS=YES,ZERO=NOBLANK                         
         MVC   P+21(16),=C'BATCHES BALANCED'                                    
         BAS   RE,PRINTIT                                                       
         BAS   RE,WRITOTAL                                                      
         BAS   RE,CLOSWRK                                                       
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        CLEAR TABLE OF E2 KEYS FROM BATCH RECORDS                    *         
*---------------------------------------------------------------------*         
*                                                                               
CLEARE2  NTR1                                                                   
         LA    R2,E2MAX                                                         
         USING E2TABD,R5                                                        
         L     R5,AE2TABLE                                                      
CLRE202  XC    0(E2LEN,R5),0(R5)                                                
         LA    R5,E2LEN(R5)                                                     
         BCT   R2,CLRE202                                                       
*                                                                               
CLRE2X   B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        READ OFFICE RECORD FOR NAME                                  *         
*---------------------------------------------------------------------*         
*                                                                               
OFFNAME  NTR1                                                                   
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,L'IO3            CLEAR IOAREA                                 
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING OFFRECD,R5          BUILD KEY OF OFFICE RECORD                   
         MVC   BALOFFN,SPACES                                                   
         MVC   BALOFFN(18),=C'OFFICE NOT DEFINED'                               
*                                                                               
         MVC   SAVEKEY,DKEY                                                     
         LA    R5,DKEY                                                          
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,RCCOMPFL                                                 
         MVC   OFFKOFF,BALOFF                                                   
         BAS   RE,HIGH                                                          
         CLC   DIR(OFFKOFF-OFFKEY),DKEY                                         
         BNE   OFFNX                                                            
*                                                                               
         L     R3,AIO3                                                          
         BAS   RE,GET                                                           
*                                                                               
         L     R5,AIO3                                                          
         LA    R2,OFFRFST                                                       
*                                                                               
OFFN02   CLI   0(R2),0             TEST END-OF-RECORD                           
         BE    OFFNX                                                            
         CLI   0(R2),NAMELQ        R2=NAME ELEMENT                              
         BNE   OFFN04                                                           
         USING NAMELD,R2                                                        
         MVC   BALOFFN,SPACES                                                   
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BALOFFN(0),NAMEREC  NAME TO TABLE                                
         B     OFFNX                                                            
*                                                                               
OFFN04   SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     OFFN02                                                           
*                                                                               
OFFNX    MVC   DKEY,SAVEKEY                                                     
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        BUILD TABLE OF E2 KEYS FROM BATCH RECORDS                    *         
*        IO1 = A(BATCH ITEM RECORD)                                   *         
*---------------------------------------------------------------------*         
*                                                                               
BUILDE2  NTR1                                                                   
         USING E2TABD,R5                                                        
         L     R5,AE2TABLE                                                      
         MVI   0(R5),X'FF'                                                      
*                                                                               
         USING TBARECD,R3             BUMP THROUGH ITEM REC AND                 
BLDE202  L     R3,AIO1                 BUILD A TABLE OF ALL TRANS               
         LA    R2,TBARFST             RECORDS ADDED WITH THIS ITEM REC          
*                                                                               
BLDE204  CLI   0(R2),0                                                          
         BE    BLDE208                                                          
         CLI   0(R2),ASKELQ                                                     
         BNE   BLDE206                                                          
                                                                                
         USING ASKELD,R2                                                        
         MVC   E2SEQ,ASKSEQN          SAVE SEQ NUMBER                           
         MVC   E2KEY,ASKKEY           SAVE 42 BYTE KEY                          
         LA    R5,E2LEN(R5)                                                     
         MVI   0(R5),X'FF'            MARK END OF TABLE                         
                                                                                
BLDE206  ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     BLDE204                                                          
                                                                                
BLDE208  BAS   RE,SEQ                 GET THE NEXT RECORD (ITEM REC)            
         CLC   DIR(TBAKTSEQ-TBAKEY),SAVEKEY                                     
         BNE   BLDE2X                 THE NEXT HEADER                           
         TM    TBARESTA,TBAESLDE      IS THIS ONE LOGICALLY DELETED?            
         BO    BLDE208                IF YES, SKIP AND GET NEXT ITEM            
         BAS   RE,GET                 GET BATCH ITEM RECORD                     
         BE    BLDE202                AND PROCESS IT                            
         DC    H'0'                                                             
                                                                                
BLDE2X   B     XIT                                                              
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        READ TRANSACTIONS POINTED TO IN E2 ELS                       *         
*        SAVE STATUS AND AMOUNT                                       *         
*---------------------------------------------------------------------*         
*                                                                               
READKEYS NTR1                                                                   
         MVC   DKEYSV,IO1          SAVE DIR KEY TO RESET READ                   
         USING E2TABD,R5                                                        
         L     R5,AE2TABLE                                                      
         ZAP   OFFAMT,=P'0'                                                     
         ZAP   OTHAMT,=P'0'                                                     
         NI    HIT,X'FF'-HITOFF                                                 
*                                                                               
READK02  CLI   0(R5),X'FF'                                                      
         BE    READK08             ALL E2'S READ                                
         MVC   DKEY,E2KEY          GET DIR KEY FOR TRANSACTION                  
         BAS   RE,READ                                                          
         CLC   DKEY,DIR                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO1             GET THE RECORD NOW                           
         BAS   RE,GET                                                           
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         USING TRNRECD,R2                                                       
         L     R2,AIO1                                                          
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         CLI   0(R3),TRNELQ        FIND THE 44                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   E2TRNAMT,TRNAMNT                                                 
         MVC   E2STAT,TRNSTAT                                                   
*                                                                               
         LA    R1,OTHAMT                                                        
         CLC   BALOFF,TRNOFFC      IS THIS THE COMPANY OFFICE?                  
         BNE   READK04             NO                                           
         OI    HIT,HITOFF          INDICATE WE HAVE OFFICE POSTING              
         LA    R1,OFFAMT           NO                                           
*                                                                               
READK04  TM    TRNSTAT,TRNSDR                                                   
         BZ    *+14                                                             
         AP    0(L'OFFAMT,R1),TRNAMNT                                           
         B     READK06                                                          
         SP    0(L'OFFAMT,R1),TRNAMNT                                           
*                                                                               
READK06  MVC   AIO,AIO1                                                         
         BAS   RE,PRNTTRAN                                                      
         LA    R5,E2LEN(R5)        BUMP TO NEXT TRANSACTION KEY                 
         B     READK02                                                          
*                                                                               
READK08  MVC   P+1(18),=C'BALANCING OFFICE ='                                   
         MVC   P+20(L'BALOFF),BALOFF                                            
         TM    HIT,HITOFF          ANY POSTINGS TO OFFICE?                      
         BO    READK12             YES                                          
*                                                                               
READK10  MVC   P+25(30),=C'** BATCH CANNOT BE BALANCED **'                      
         BAS   RE,PRINTIT                                                       
         B     READKNG                                                          
*                                                                               
READK12  AP    OFFAMT,OTHAMT       DO POSTINGS BALANCE OUT?                     
         BNE   READK10             NO                                           
         BAS   RE,PRINTIT                                                       
         CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
READKNG  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        READ TRANSACTIONS AGAIN AND BUILD NEW POSTINGS               *         
*        R2=A(POSTING ELEMENTS)                                       *         
*        R3=A(TRANSACTION RECORD)                                     *         
*        R4=A(POSTING HEADER)                                         *         
*        R5=E2TABLE                                                   *         
*---------------------------------------------------------------------*         
*                                                                               
BLDPOST  NTR1                                                                   
         MVC   DKEYSV,IO1          SAVE DIR KEY TO RESET READ                   
         USING E2TABD,R5                                                        
         L     R5,AE2TABLE                                                      
*                                                                               
BPOST02  CLI   0(R5),X'FF'                                                      
         BE    BPOSTX                                                           
*                                                                               
         MVC   DKEY,E2KEY          GET DIR KEY FOR TRANSACTION                  
         BAS   RE,READ                                                          
         CLC   DKEY,DIR                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO1             GET THE RECORD NOW                           
         BAS   RE,GET                                                           
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         USING TRNRECD,R3                                                       
         L     R3,AIO1                                                          
         CLC   TRNKOFF,BALOFF      IF POSTING IS TO BALANCING OFFICE            
         BE    BPOST16                NO BALANCING IS NEEDED                    
*                                                                               
         L     RE,AIO2                                                          
         LA    RF,L'AIO2                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR POSTING BLOCK                          
*                                                                               
         USING PSHEADD,R4          BUILD POSTING HEADER                         
         L     R4,AIO2                                                          
         LA    R4,4(R4)                                                         
         MVI   PSHDEL,PSHDELQ                                                   
         MVI   PSHDLEN,PSHEADL                                                  
         MVC   PSHDACC,SPACES                                                   
         MVC   PSHDACPY,QCOMPANY                                                
         MVC   PSHDAUL(L'BALACCT),BALACCT                                       
         SR    RF,RF                                                            
         IC    RF,LLEVAB                                                        
         LA    R1,PSHDAACT(RF)                                                  
         MVC   0(L'BALOFF,R1),TRNKOFF                                           
*                                                                               
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC,PSHDACC                                                 
         LA    R1,PSHDSACT(RF)                                                  
         MVC   0(L'BALOFF,R1),BALOFF                                            
         MVC   PSHDANAL,BALOFF                                                  
*                                                                               
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         CLI   0(R3),TRNELQ        FIND THE 44 AND SAVE IT                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVE44,TRNEL                                                     
*                                                                               
BPOST04  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         XC    SAVE60,SAVE60       CLEAR ELEMENT                                
         USING TRSELD,R3                                                        
         CLI   0(R3),0             FIX THE TRSEL AND SAVE IT                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),TRSELQ                                                     
         BNE   BPOST04                                                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,TRSLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SAVE60(0),TRSEL                                                  
         DROP  R3                                                               
*                                                                               
         USING TRNELD,R2                                                        
         LR    R2,R4                                                            
         AHI   R2,PSHEADL                                                       
         MVC   TRNEL(L'SAVE44),SAVE44                                           
         MVI   TRNLN,TRNLN1Q+1     RESET THE LENGTH                             
         MVI   TRNTYPE,83          CHANGE THE TYPE                              
         MVI   TRNNARR,C' '        SET BLANK NARRATIVE                          
         MVC   TRNOFFC,BALOFF      SET THE CORRECT OFFICE                       
*                                                                               
         CLC   BALNARR,SPACES      ANY NARRATIVE?                               
         BE    BPOST06             NO                                           
*                                                                               
         LA    RF,BALNARR                                                       
         LA    RE,BALNARR+L'BALNARR-1                                           
         CLI   0(RE),C' '          FIND LAST CHARACTER                          
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
*                                                                               
         SR    RE,RF               GET LENGTH OF NARRATIVE                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TRNNARR(0),BALNARR                                               
*        AHI   RE,1                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         AR    RE,RF                                                            
         STC   RE,TRNLN            SET NEW LENGTH                               
*                                                                               
BPOST06  LR    RF,R2                                                            
         SR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         AR    RF,RE                                                            
*                                                                               
         USING TRSELD,RF                                                        
         SR    RE,RE                                                            
         IC    RE,SAVE60+1                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TRSEL(0),SAVE60                                                  
         XC    TRSDATE,TRSDATE                                                  
*                                                                               
         AHI   RE,1                ADD BACK THE ONE                             
         AR    RF,RE                                                            
         MVI   0(RF),0             MARK END JUST IN CASE                        
*                                                                               
         BAS   RE,VALACCT          READ FOR POSTING ACCOUNT                     
*                                                                               
         CLC   ACCNAME,SPACES      DID WE FIND IT?                              
         BE    BPOST08             YES, FINISH THE POSTING THEN                 
*                                                                               
         USING NAMELD,RF                                                        
         MVI   NAMEL,NAMELQ        ADD NAME ELEMENT                             
         MVC   NAMEREC(4),=C'I/O-'                                              
         MVC   NAMEREC+4(L'BALOFFN-4),BALOFFN                                   
         LA    R6,NAMEREC+L'NAMEREC-1                                           
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         SR    R6,RF                                                            
         LA    R6,1(R6)                                                         
         STC   R6,NAMLN                                                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         AR    RF,RE                                                            
         MVI   0(RF),0                                                          
*                                                                               
BPOST08  BAS   RE,PRNTPOST                                                      
         BAS   RE,ADDPOST                                                       
*                                                                               
         TM    UPSI,UPSIALL                                                     
         BNO   BPOST10                                                          
         L     R3,AIO2                                                          
         LA    R6,=C'POST'                                                      
         BAS   RE,DMPOST                                                        
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        REVERSE OFFICE CODES AND AMOUNTS AND ADD AGAIN               *         
*---------------------------------------------------------------------*         
BPOST10  SR    RF,RF                                                            
         IC    RF,LLEVAB                                                        
         LA    R1,PSHDAACT(RF)                                                  
         LA    RE,PSHDSACT(RF)                                                  
         MVC   0(L'BALOFF,RE),0(R1)                                             
         MVC   PSHDANAL,0(R1)                                                   
         MVC   0(L'BALOFF,R1),BALOFF                                            
*                                                                               
         XI    TRNSTAT,TRNSDR      REVERSE THE STATUS                           
         MVC   TRNOFFC,PSHDANAL    CHANGE THE OFFICE                            
*                                                                               
         LR    RF,R2               GET PAST TRNEL                               
         SR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         AR    RF,RE                                                            
*                                                                               
         SR    RE,RE               GET PAST TRSEL                               
         IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         MVI   0(RF),0             MARK END JUST IN CASE                        
*                                                                               
         BAS   RE,VALACCT                                                       
*                                                                               
         CLC   ACCNAME,SPACES      DID WE FIND IT?                              
         BE    BPOST14             YES, FINISH THE POSTING THEN                 
*                                                                               
         USING NAMELD,RF                                                        
         MVI   NAMEL,NAMELQ        ADD NAME ELEMENT                             
         MVC   NAMEREC(4),=C'I/O-'                                              
         MVC   NAMEREC+4(L'BALOFFN-4),BALOFFN                                   
         LA    R6,NAMEREC+L'NAMEREC-1                                           
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         SR    R6,RF                                                            
         LA    R6,1(R6)                                                         
         STC   R6,NAMLN                                                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         AR    RF,RE                                                            
         MVI   0(RF),0                                                          
*                                                                               
BPOST14  BAS   RE,PRNTPOST                                                      
         BAS   RE,ADDPOST                                                       
*                                                                               
         TM    UPSI,UPSIALL                                                     
         BNO   BPOST16                                                          
         L     R3,AIO2                                                          
         LA    R6,=C'POST'                                                      
         BAS   RE,DMPOST                                                        
*                                                                               
BPOST16  LA    R5,E2LEN(R5)        BUMP TO NEXT TRANSACTION KEY                 
         B     BPOST02                                                          
*                                                                               
BPOSTX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        READ FOR SECURITY                                            *         
*---------------------------------------------------------------------*         
*                                                                               
GETSECA  NTR1                                                                   
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,L'IO3            CLEAR IOAREA                                 
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING SAPEREC,R5                                                       
         LA    R5,IOKEY                                                         
         USING CT5REC,R5           SYSTEM ACCESS RECORD                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ    C'5'                                         
         MVC   CT5KALPH,ALPHAID                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY,AIO3                            
         L     R3,AIO3                                                          
         CLC   IOKEY(L'CT5KEY),0(R3)                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,CT5DATA                                                       
         XR    R0,R0                                                            
*                                                                               
         USING CTSEAD,R2                                                        
GETS02   CLI   0(R2),0                                                          
         BE    GETSECX                                                          
         CLI   0(R2),CTSEAELQ    SECURITY AGENCY ALPHA ID ELEMENT               
         BNE   *+14                                                             
         MVC   SECCODE,CTSEAAID                                                 
         B     GETSECX                                                          
*                                                                               
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GETS02                                                           
*                                                                               
GETSECX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        GET THE INTER-OFFICE BALANCING ACCOUNT                       *         
*---------------------------------------------------------------------*         
*                                                                               
GETINT   NTR1                                                                   
         MVC   BALACCT,SPACES                                                   
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,L'IO3            CLEAR IOAREA                                 
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING CPYRECD,R5          READ COMPANY RECORD                          
         MVC   SAVEKEY,DKEY                                                     
         LA    R5,DKEY                                                          
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,RCCOMPFL                                                 
         BAS   RE,HIGH                                                          
         CLC   DIR(OFFKOFF-OFFKEY),DKEY                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO3                                                          
         BAS   RE,GET                                                           
*                                                                               
         GOTO1 GETEL,DMCB,('SPAELQ',(R3)),0                                     
         CLI   12(R1),0                                                         
         BNE   GETIERR                                                          
*                                                                               
         USING SPAELD,R2                                                        
         L     R2,12(R1)                                                        
GETI02   CLI   SPAEL,0                                                          
         BE    GETIERR                                                          
         CLI   SPATYPE,SPATIBAC                                                 
         BNE   *+14                                                             
         MVC   BALACCT,SPAAULA    SAVE THE ACCOUNT                              
         B     GETINTX                                                          
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GETI02                                                           
*                                                                               
GETIERR  MVC   P(39),=C'ERROR - NO INTER-OFFICE ACCOUNT DEFINED'                
         BAS   RE,PRINTIT                                                       
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVI   ACMMODE,RUNLAST     FORCE RUN LAST                               
*                                                                               
GETINTX  MVC   DKEY,SAVEKEY                                                     
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        GET THE NARRATIVE FROM THE BALACCT                           *         
*---------------------------------------------------------------------*         
*                                                                               
GETNAR   NTR1                                                                   
         MVC   BALNARR,SPACES                                                   
*                                                                               
         LHI   R1,ACQCARD5-ACQD                                                 
         A     R1,ADQSTACK                                                      
         CLC   0(L'BALNARR,R1),SPACES                                           
         BNH   GETN02                                                           
         MVC   BALNARR,0(R1)       USE OVERRIDE IF THERE                        
         B     GETNARX                                                          
*                                                                               
GETN02   L     RE,AIO3                                                          
         LA    RF,L'IO3            CLEAR IOAREA                                 
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING ACTRECD,R5          READ ACCOUNT RECORD                          
         MVC   SAVEKEY,DKEY                                                     
         LA    R5,DKEY                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKULA,BALACCT                                                  
         BAS   RE,HIGH                                                          
         CLC   DIR(OFFKOFF-OFFKEY),DKEY                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO3                                                          
         BAS   RE,GET                                                           
*                                                                               
         GOTO1 GETEL,DMCB,('OMEELQ',(R3)),0                                     
         CLI   12(R1),0                                                         
         BNE   GETNARX              NO NARRATIVE TO ADD                         
*                                                                               
         USING OMEELD,R2                                                        
         L     R2,12(R1)                                                        
         CLC   OMEMO(3),=C'IO='                                                 
         BNE   GETNARX                                                          
         IC    R1,OMELN                                                         
         SH    R1,=H'6'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BALNARR(0),OMEMO+3   SAVE THE NARRATIVE                          
*                                                                               
GETNARX  MVC   DKEY,SAVEKEY                                                     
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        READ LEDGER FOR HIERARCHY                                    *         
*---------------------------------------------------------------------*         
*                                                                               
GETLED   NTR1                                                                   
         L     RE,AIO3                                                          
         LA    RF,L'IO3            CLEAR IOAREA                                 
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING LDGRECD,R5          READ SB LEDGER                               
         MVC   SAVEKEY,DKEY                                                     
         LA    R5,DKEY                                                          
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,RCCOMPFL                                                 
         MVC   LDGKUNT(2),=C'SB'                                                
         BAS   RE,HIGH                                                          
         CLC   DIR(OFFKOFF-OFFKEY),DKEY                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    LLEVA(LLEVLNQ),LLEVA                                             
*                                                                               
         L     R3,AIO3                                                          
         BAS   RE,GET                                                           
*                                                                               
         GOTO1 GETEL,DMCB,('ACLELQ',(R3)),0                                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,12(R1)                                                        
*                                                                               
         USING ACLELD,R2                                                        
         MVC   LLEVA,ACLELLVA                                                   
         MVC   LLEVAB,ACLELLVB                                                  
         MVC   LLEVABC,ACLELLVC                                                 
         MVC   LLEVABCD,ACLELLVD                                                
*                                                                               
GETLX    MVC   DKEY,SAVEKEY                                                     
         B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* READ FOR BALANCING ACCOUNT AND ADD IF NOT FOUND                     *         
* AIO2 = POSTING RECORD                                               *         
* AIO3 = RECORD ADD AREA (IF NEEDED)                                  *         
*---------------------------------------------------------------------*         
*                                                                               
         USING PSHEADD,R2                                                       
VALACCT  NTR1                                                                   
*                                                                               
         MVC   ACCNAME,SPACES      CLEAR NAME FIELD                             
*                                                                               
         L     R2,AIO2             A(BALANCING ACCOUNT)                         
         LA    R2,4(R2)                                                         
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(L'PSHDACC),PSHDACC                                          
         BAS   RE,READ                                                          
         CLC   DKEY,DIR                                                         
         BE    VALACCX             OK, RECORD ON FILE                           
*                                                                               
         MVC   ACCNAME(4),=C'I/O-'                                              
         MVC   ACCNAME+4(L'BALOFFN-4),BALOFFN                                   
*                                                                               
VALACCX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        READ FOR PERSON                                              *         
*---------------------------------------------------------------------*         
*                                                                               
GETPERS  NTR1                                                                   
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,L'IO3            CLEAR IOAREA                                 
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING SAPEREC,R3                                                       
         LA    R3,IOKEY                                                         
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,SECCODE     USE SECURITY AGENCY ALPHA ID                 
         MVC   SAPEPID,QAPPL                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY,AIO3                            
         L     R3,AIO3                                                          
         CLC   IOKEY(SAPEDEF-SAPEKEY),0(R3)                                     
         JNE   GETPERX                                                          
*                                                                               
         LA    R2,SAPEDATA                                                      
         XR    R0,R0                                                            
*                                                                               
         USING SAPWDD,R2                                                        
GETP02   CLI   0(R2),0                                                          
         BE    GETPERX                                                          
         CLI   0(R2),SAPWDELQ      PERSON PASSWORD ELEMENT                      
         BE    *+14                                                             
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GETP02                                                           
*                                                                               
         MVC   BATPER,SAPWDNUM                                                  
GETPERX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        RE-READ BATCH HEADER AND MARK BALANCED                       *         
*---------------------------------------------------------------------*         
*                                                                               
UPBATCH  NTR1                                                                   
         MVC   DKEY,DHEDSV         RESET DIRECTORY FOR BATCH HEADER             
         BAS   RE,READ             REREAD IT                                    
         CLC   DKEY,DIR                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO1                                                          
         BAS   RE,GET              GET BATCH HEADER                             
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TBARECD,R3                                                       
         USING BHDELD,R2                                                        
         LA    R2,TBARFST                                                       
*                                                                               
UPBAT02  CLI   0(R2),0             EOF?                                         
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R2),BHDELQ        LOOK FOR HEADER ELEMENT                      
         BE    UPBAT04                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     UPBAT02                                                          
*                                                                               
UPBAT04  OI    BHDSTAT2,BHDSBDON   MARK BATCH BALANCED                          
         BAS   RE,PUT                                                           
*                                                                               
         TM    UPSI,UPSIALL                                                     
         BNO   *+12                                                             
         LA    R6,=C'PBAT'                                                      
         BAS   RE,DMPREC                                                        
*                                                                               
UPBATX   B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PRINT THE TRANSACTIONS                                       *         
*---------------------------------------------------------------------*         
*                                                                               
PRNTTRAN NTR1                                                                   
         L     R5,AIO                                                           
         USING TRNRECD,R5                                                       
         LA    R6,P                                                             
         USING PLINE,R6                                                         
         MVC   PACCOUNT,TRNKULA                                                 
         MVC   POFFICE,TRNKOFF                                                  
         MVC   PCONTRA,TRNKULC                                                  
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(5,PDATE)                               
         MVC   PREF,TRNKREF                                                     
         MVC   WORK(2),TRNRSMOS                                                 
         MVC   WORK+2(1),=X'01'                                                 
         GOTO1 DATCON,DMCB,(1,WORK),(9,PMOA)                                    
         CURED (B1,TRNRSTYP),(2,PTYPE),0                                        
         LA    R2,TRNRFST                                                       
         CLI   0(R2),X'44'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TRNELD,R2                                                        
         MVC   PBREF,TRNBREF                                                    
         LA    R3,PDEBITS                                                       
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R3,PCREDITS                                                      
         EDIT  (P6,TRNAMNT),(14,(R3)),2,MINUS=YES                               
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PRINT THE NEW POSTINGS                                       *         
*---------------------------------------------------------------------*         
*                                                                               
PRNTPOST NTR1                                                                   
         LA    R6,P                                                             
         USING PLINE,R6                                                         
         USING PSHEADD,R4                                                       
         MVC   PACCOUNT,PSHDAUNT                                                
         MVC   POFFICE,PSHDANAL                                                 
         MVC   PCONTRA,PSHDSUNT                                                 
*                                                                               
         USING TRNELD,R2                                                        
         GOTO1 DATCON,DMCB,(1,TRNDATE),(5,PDATE)                                
*                                                                               
         MVC   PREF,TRNREF                                                      
*                                                                               
         CURED (B1,TRNTYPE),(2,PTYPE),0                                         
*                                                                               
         MVC   PBREF,TRNBREF                                                    
*                                                                               
         LA    R3,PDEBITS                                                       
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R3,PCREDITS                                                      
         EDIT  (P6,TRNAMNT),(14,(R3)),2,MINUS=YES                               
*                                                                               
         LR    RF,R2                                                            
         SR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         AR    RF,RE                                                            
*                                                                               
         USING TRSELD,RF                                                        
         MVC   WORK(2),TRSPMOS                                                  
         MVC   WORK+2(1),=X'01'                                                 
         GOTO1 DATCON,DMCB,(1,WORK),(9,PMOA)                                    
*                                                                               
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              PRINT FROM HERE                                        *         
*---------------------------------------------------------------------*         
*                                                                               
PRINTIT  NTR1                                                                   
         CLI   QOPT1,C'Y'          IS THIS A LIVE RUN?                          
         BE    *+10                YES, SKIP THIS                               
         MVC   HEAD2+63(7),=C'(DRAFT)'                                          
         GOTO1 ACREPORT                                                         
*                                                                               
PRINTITX B     XIT                                                              
*                                                                               
*---------------------------------------------------------------------*         
*              OPEN POSTING FILE                                      *         
*---------------------------------------------------------------------*         
*                                                                               
OPENWRK  DC    0H'0'                                                            
         OI    WKID+13,X'01'       ALLOW DUP KEYS                               
         MVC   WKID(2),ORIGINUM                                                 
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   WKID+6(1),DUB                                                    
         MVI   WKID+7,C'P'                                                      
         MVC   WKID+2(3),=C'AIO'                                                
         MVC   COMMAND,=CL6'OPEN'                                               
         B     POST                                                             
*                                                                               
*---------------------------------------------------------------------*         
*              WRITE LAST POSTING FILE RECORD AND CLOSE               *         
*---------------------------------------------------------------------*         
*                                                                               
WRITOTAL DS    0H                                                               
         L     RE,AIO2                                                          
         LA    RF,L'AIO2                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R3,AIO2                                                          
         MVC   0(4,R3),=X'00210000'                                             
         LA    R3,4(R3)                                                         
         USING PSSUBFD,R3                                                       
         MVI   PSSBEL,PSSBELQ                                                   
         MVI   PSSBLEN,PSSUBFL                                                  
         MVC   PSSBDESC,=C'INTER-BALANCING'                                     
         ZAP   PSSBRECS,POSTCNT+2(6)                                            
         ZAP   PSSBCASH,POSTCASH+2(6)                                           
         BAS   RE,ADDWRK                                                        
         BAS   RE,CLOSWRK                                                       
         CLI   RCFFPARM,C'T'       IS THIS A TEST RUN?                          
         BNE   *+8                 NO                                           
         BAS   RE,KEEPWRK          YES, PUT FILE ON KEEP                        
         B     POSTX                                                            
         DROP  R3                                                               
*                                                                               
*---------------------------------------------------------------------*         
*        WORKER INTERFACE                                             *         
*---------------------------------------------------------------------*         
*                                                                               
         USING TRNELD,R2                                                        
ADDPOST  NTR1                                                                   
         TM    TRNSTAT,X'80'                                                    
         BZ    *+10                                                             
         AP    POSTCASH,TRNAMNT                                                 
         AP    POSTCNT,=P'1'                                                    
         SR    R3,R3                                                            
         IC    R3,1(R2)                                                         
*                                                                               
ADD02    AR    R2,R3                                                            
         CLI   0(R2),0                                                          
         BE    ADD04                                                            
         IC    R3,1(R2)                                                         
         LTR   R3,R3               CHECK ZERO LENGTH ELEMENT                    
         BNZ   ADD02                                                            
         MVI   0(R2),0                                                          
*                                                                               
ADD04    LA    R2,1(R2)                                                         
         L     R3,AIO2                                                          
         SR    R2,R3                                                            
         STH   R2,0(R3)                                                         
         BAS   RE,ADDWRK                                                        
         B     POSTX                                                            
*                                                                               
ADDWRK   MVC   COMMAND,=CL6'ADD'                                                
         B     POST                                                             
*                                                                               
CLOSWRK  MVC   COMMAND,=CL6'CLOSE'                                              
         B     POST                                                             
*                                                                               
KEEPWRK  MVC   COMMAND,=CL6'KEEP'                                               
         B     POST                                                             
*                                                                               
POST     NTR1                                                                   
         L     R3,AIO2                                                          
         L     R4,AWKBUFF                                                       
         CLI   RCPOSTNG,C'Y'       SHOULD WE CREATE A POSTING FILE?             
         BNE   POSTX               NO                                           
*                                                                               
         GOTO1 WORKER,DMCB,COMMAND,(R4),WKID,(R3)                               
         TM    DMCB+8,X'C0'                                                     
         BZ    POSTX                                                            
*                                  ERROR ON ADD TO WORKER FILE                  
         MVC   P,SPACES                                                         
         MVC   P(42),=C'*WORKER FILE FULL - STOP ALL FILE MARKERS*'             
         TM    DMCB+8,X'80'                                                     
         BO    *+10                                                             
         MVC   P(42),=C'*WORKER FILE DISK ERROR - CANNOT ADD ID = '             
         L     R2,DMCB+8           ADDRESS OF KEY                               
         MVC   DOUBLE(2),0(R2)                                                  
         LH    R3,DOUBLE                                                        
         CVD   R3,DOUBLE                                                        
         UNPK  DOUBLE+2(6),DOUBLE                                               
         OI    DOUBLE+7,X'F0'                                                   
         MVC   P+42(3),DOUBLE+5                                                 
         MVC   P+45(23),=C', REPLY = ''OK'' FOR DUMP'                           
*                                                                               
POSTER   GOTO1 LOGIO,WORK,1,(68,P)                                              
         GOTO1 (RF),(R1),0,(2,DUB)                                              
         CLC   DUB(2),=C'OK'                                                    
         BNE   POSTER                                                           
         DC    H'0'                NOW DIE                                      
*                                                                               
POSTX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        DATA MANAGER ROUTINES                                        *         
*---------------------------------------------------------------------*         
*                                                                               
HIGH     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
READ     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SEQ      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
WRT      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GET      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,(R3),DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ADD      LR    R0,RE                                                            
         XC    DA,DA                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R3),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PUT      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R3),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        DUMP SOME RECORDS - USE R6 FOR LABEL                         *         
*---------------------------------------------------------------------*         
*                                                                               
DMPDIR   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    DMPX                                                             
         LA    RF,L'DIR                                                         
         LA    R3,DIR                                                           
         GOTO1 PRNTBL,DMCB,(4,(R6)),(R3),C'DUMP',(RF),=C'2D'                    
         B     DMPX                                                             
*                                                                               
DMPREC   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   DMPX                                                             
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DMPX                                                             
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(R3)                                         
         GOTO1 PRNTBL,DMCB,(4,(R6)),(R3),C'DUMP',(RF),=C'2D'                    
         B     DMPX                                                             
*                                                                               
DMPOST   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   DMPX                                                             
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DMPX                                                             
         SR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         GOTO1 PRNTBL,DMCB,(4,(R6)),(R3),C'DUMP',(RF),=C'2D'                    
*                                                                               
DMPX     B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ROUTINE TO GET AN ELEMENT                                    *         
*   P1   BYTE 0    ELEMENT CODE                                       *         
*        BYTE 1-3  A(RECORD)                                          *         
*   P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                          *         
*        BYTE 1-3  A(SEARCH ARGUMENT)                                 *         
*---------------------------------------------------------------------*         
*                                                                               
GETEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'G',ACCMST),((R4),(R2)),((R5),(R3))                 
         B     XIT                                                              
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
*        ROUTINE TO DELETE AN ELEMENT                                 *         
*          P1   BYTE 0    ELEMENT CODE                                *         
*               BYTE 1-3  A(RECORD)                                   *         
*          P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                   *         
*               BYTE 1-3  A(SEARCH ARGUMENT)                          *         
*---------------------------------------------------------------------*         
*                                                                               
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',ACCMST),((R4),(R2)),((R5),(R3))                 
         B     XIT                                                              
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
*        ROUTINE TO ADD AN ELEMENT                                    *         
*          P1   A(RECORD)                                             *         
*          P2   A(ELEMENT)                                            *         
*---------------------------------------------------------------------*         
*                                                                               
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R3)                               
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        LITERALS                                                     *         
*---------------------------------------------------------------------*         
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
*                                                                               
*---------------------------------------------------------------------*         
*        STORAGE                                                      *         
*---------------------------------------------------------------------*         
*                                                                               
BATCNT   DC    PL4'0'                                                           
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'2000'                                                        
POSTCNT  DS    PL8                                                              
POSTCASH DS    PL8                                                              
WRKREC   DC    PL6'0'              WORKER RECORDS                               
*                                                                               
OFFAMT   DS    PL8                 POSTINGS TO BALANCING OFFICE                 
OTHAMT   DS    PL8                 POSTINGS TO NON-BALANCING OFFICE             
*                                                                               
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
*                                                                               
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
AIO3     DC    A(IO3)                                                           
ASRQTAB  DC    A(SRQTAB)                                                        
AE2TABLE DC    A(E2TABLE)                                                       
AWKBUFF  DC    A(WKBUFF)                                                        
*                                                                               
DKEY     DS    CL(L'ACCKEY)                                                     
DKEYSV   DS    CL(L'ACCKEY)                                                     
DHEDSV   DS    CL(L'ACCKEY)                                                     
DIR      DS    CL64                KEY RETURNED FROM READS                      
DA       DS    F                                                                
IOKEY    DS    CL64                                                             
SAVEKEY  DS    CL42                                                             
SAVE44   DS    CL(TRNLN1Q+1)       SAVE TRNEL                                   
SAVE60   DS    CL(TRSLN2Q)         SAVE TRSEL                                   
         LTORG                                                                  
         EJECT                                                                  
IO1      DS    CL2000                                                           
IO2      DS    CL2000                                                           
IO3      DS    CL2000                                                           
*                                                                               
E2MAX    EQU   200                                                              
E2TABLE  DS    (E2MAX)CL(E2LEN)                                                 
*                                                                               
*                                  POSTING BUFFER FOR WORKER FILE               
WKBUFF   DS    0D                                                               
         DC    4500X'00'                                                        
*---------------------------------------------------------------------*         
*        SUPPLEMENATARY REQUEST DETAILS                               *         
*---------------------------------------------------------------------*         
*                                                                               
SRQTAB   DS    0C                                                               
         DC    AL2(QCOMMENT+1-ACWORKD),AL1(L'QCOMMENT-5)                        
         DCDD  AC#BATTY,20                                                      
         DC    AL2(QCOMMENT-ACWORKD),AL1(L'QCOMMENT-6)                          
         DCDD  AC#BATGP,20                                                      
         DC    AL2(QAPPL+8-ACWORKD),AL1(L'QAPPL-9)                              
         DCDD  AC#BATRF,20                                                      
         DC    AL2(QAPPL-ACWORKD),AL1(L'QAPPL-5)                                
         DCDD  AC#PRSN,20                                                       
         DC    AL2(QOPT1-ACWORKD),AL1(L'QOPT1-1)                                
         DCDD  AC#LRUN,20                                                       
         DC    AL2(QOPT2-ACWORKD),AL1(L'QOPT2-1)                                
         DCDD  AC#RVRSL,20                                                      
SRQTABX  DC    AL2(0)                                                           
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        SUPPLEMENTARY DSECTS                                         *         
*---------------------------------------------------------------------*         
*                                                                               
ACIOD    DSECT                                                                  
BATTYP   DS    XL1                 BATCH TYPE FILTER                            
BATGRP   DS    XL1                 BATCH GROUP FILTER                           
BATREF   DS    XL4                 BATCH REFERENCE FILTER                       
BATMOS   DS    PL2                 BATCH MOA START                              
BATMOE   DS    PL2                 BATCH MOA END                                
BATSTR   DS    XL2                 BATCH START DATE                             
BATEND   DS    XL2                 BATCH END DATE                               
BATPER   DS    XL2                 BATCH PERSON CODE                            
*                                                                               
SECCODE  DS    CL2                 AGENCY ALPHA CODE FROM CTFILE                
*                                                                               
BALOFF   DS    CL2                 COMPANY OFFICE                               
BALOFFN  DS    CL36                COMPANY OFFICE NAME                          
*                                                                               
BALACCT  DS    CL(L'SPAAULA)       FROM COMPANY RECORD                          
BALNARR  DS    CL62                FROM ACCOUNT RECORD                          
ACCNAME  DS    CL(L'NAMEREC)       ACCOUNT NAME FOR ADD                         
*                                                                               
UPSI     DS    X                   UPSI FOR DUMPS                               
UPSIALL  EQU   X'80'               DUMP EVERYTHING                              
*                                                                               
*                                  DATA FROM LEDGER HEIRARCHY                   
LLEVA    DS    XL1                 L'LEVEL A                                    
LLEVAB   DS    XL1                 L'LEVEL A + B                                
LLEVABC  DS    XL1                 L'LEVEL A + B + C                            
LLEVABCD DS    XL1                 L'LEVEL A + B + C + D                        
LLEVLNQ  EQU   *-LLEVA                                                          
*                                                                               
ELCODE   DS    XL1                                                              
ELEMENT  DS    CL255                                                            
REPORTSW DS    CL1                                                              
HIT      DS    XL1                                                              
HITBAL   EQU   X'80'                                                            
HITOFF   EQU   X'40'                                                            
NO       EQU   C'N'                                                             
*                                                                               
AIO      DS    A                                                                
TODAYC   DS    XL2                 COMPRESSED                                   
TODAYP   DS    PL3                 PACKED                                       
*                                                                               
COMMAND  DS    CL6                                                              
WKID     DS    XL16                WORKER FILE ID - POSTING FILE                
         EJECT                                                                  
PLINE    DSECT                                                                  
         DS    CL1                                                              
PBREF    DS    CL4                 BATCH REFERENCE                              
         DS    CL1                                                              
PTYPE    DS    CL2                 TYPE                                         
         DS    CL1                                                              
PMOA     DS    CL6                 MOA                                          
         DS    CL1                                                              
PACCOUNT DS    CL14                ACCOUNT                                      
         DS    CL1                                                              
POFFICE  DS    CL2                 OFFICE                                       
         DS    CL1                                                              
PCONTRA  DS    CL14                CONTRA                                       
         DS    CL1                                                              
PDATE    DS    CL8                 DATE                                         
         DS    CL2                                                              
PREF     DS    CL6                 REFERENCE                                    
         DS    CL1                                                              
PDEBITS  DS    CL14                DR                                           
         DS    CL1                                                              
PCREDITS DS    CL14                CR                                           
PLNQ     EQU   *-PLINE                                                          
*                                                                               
*                                                                               
E2TABD   DSECT                                                                  
E2SEQ    DS    XL1                                                              
E2KEY    DS    CL42                                                             
E2STAT   DS    XL1                                                              
E2DEBIT  EQU   X'80'                                                            
E2TRNAMT DS    PL6                                                              
E2LEN    EQU   *-E2TABD                                                         
*                                                                               
*                                                                               
SRQTABD  DSECT                     ** REQUEST REPORT TABLE **                   
SRQDISP  DS    CL2                 DISP TO FIELD IN REQUEST CARD                
SRQLEN   DS    CL1                 EXECUTABLE LENGTH OF FIELD                   
SRQDD    DS    CL20                DATA DICTIONARY REFERENCE                    
SRQQ     EQU   *-SRQDISP           ENTRY LENGTH                                 
         EJECT                                                                  
*  ACREPWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*  CTGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*  ACGENMODES                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*  DMDTFIS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
*  ACGENPOST                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
*  ACDDEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
*  DDMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*  SEACSFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*  ACQD                                                                         
         PRINT OFF                                                              
       ++INCLUDE ACQD                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACREPIO02 06/06/12'                                      
         END                                                                    
