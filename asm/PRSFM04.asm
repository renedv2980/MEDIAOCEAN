*          DATA SET PRSFM04    AT LEVEL 019 AS OF 02/16/21                      
*PHASE T41C04A                                                                  
*INCLUDE SRCHCALL                                                               
*INCLUDE PUBEDIT                                                                
         TITLE 'T41C04  CLEARANCE LIST'                                         
*        CHANGE LOG                                                             
*                                                                               
* KWAN  01/28/21 SPEC-53127 FIX LIST ENTRIES                                    
*                                                                               
* BOBY  05/23/12 FIX DISPLAY CD ONLY PAYMENTS                                   
*                                                                               
* BOBY  04/10/12 FIX DISPLAY OF LARGE NUMBERS - DROP PENNIES                    
*                                                                               
* SMYE  01/24/02 ADDED "CLEARED" OPTION AND PUT "C" BEFORE CK NUM IF            
*                CHECK HAS A CLEARED DATE ENTRY                                 
*                                                                               
T41C04   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C04,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         OI    GLSTSTAT,NOSELFLD   TAKE OUT THE SELECT FIELDS                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* *********************************************************************         
* VALIDATE KEY                                                                  
* *********************************************************************         
VK       DS    0H                                                               
*                                                                               
         CLI   ACTEQU,ACTLIST      ONLY VALID FOR LIST ACTION                   
         BNE   VKKEY                                                            
*                                                                               
VKLST    DS    0H                  LIST ACTION                                  
*---------------------------------------------------------------                
*                    VALIDATE THE MEDIA AND CLIENT                              
*---------------------------------------------------------------                
         LA    R2,CLRMEDH          VALIDATE MEDIA - REQUIRED FIELD              
         GOTO1 VALIMED                                                          
         LA    R2,CLRCLTH          VALIDATE CLIENT - REQUIRED FIELD             
         GOTO1 VALICLT                                                          
*---------------------------------------------------------------                
*                    VALIDATE THE PUB - REQUIRED                                
*---------------------------------------------------------------                
VKPUB    DS    0H                                                               
         MVI   ALLZE,C'N'          INITIALIZE                                   
         MVI   ERROR,MISSING                                                    
         LA    R2,CLRPUBH                                                       
         CLI   5(R2),0                                                          
         BE    ERRX                                                             
         CLI   8(R2),C'='          PUB NAME SEARCH                              
         BNE   VKPUB10                                                          
         SR    R2,RA                                                            
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,QMED                                                    
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
         B     VKPUB30                                                          
         DROP  R3                                                               
*                                                                               
VKPUB10  DS    0H                                                               
         LA    R2,CLRPUBH                                                       
         MVI   ERROR,INVALID                                                    
         XC    SCANBLK,SCANBLK                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,SCANBLK)                                    
         CLI   DMCB+4,0                                                         
         BE    ERRX                                                             
         LA    R1,SCANBLK                                                       
         CLC   =C'ALL',44(R1)                                                   
         BNE   VKPUB30                                                          
         MVI   ALLZE,C'Y'          WANT ONLY ACROSS ALL ZONES AND ED            
         ZIC   R3,0(R1)                                                         
         STC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),12(R1)                                                   
VKPUB30  LA    R2,CLRPUBH                                                       
         GOTO1 VALIPUB                                                          
         MVC   CLRPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    CLRPUBNH+6,X'80'                                                 
         CLI   ALLZE,C'Y'                                                       
         BNE   VKLST16                                                          
         MVC   BPUB+4(2),=X'FFFF'  ALL ZONES/EDTNS                              
         ZIC   R1,5(R2)                                                         
         AH    R1,=H'4'            PUT BACK THE ORIGINAL LEN                    
         STC   R1,5(R2)                                                         
         EJECT                                                                  
*---------------------------------------------------------------                
*                    VALIDATE THE PERIOD                                        
*---------------------------------------------------------------                
VKLST16  XC    STARTDAT,STARTDAT   CLEAR START AND END DATE FILTERS             
         XC    ENDDATE,ENDDATE                                                  
         CLI   CLRDATEH+5,0        ANYTHING INPUT?                              
         BNE   VKLST17             YES                                          
*                                                                               
*                DEFAULTS TO PERIOD OF PREVIOUS YEAR FROM THIS MONTH            
         GOTO1 DATCON,DMCB,(5,STARTDAT),(8,WRK2)  --DEFAULT=TODAY               
         XC    CLRDATE,CLRDATE                                                  
         MVC   CLRDATE(3),WRK2     MOVE IN MONTH ONLY                           
         MVI   CLRDATEH+5,3                                                     
         GOTO1 PERVAL,DMCB,(CLRDATEH+5,CLRDATE),WORK                            
         LA    R5,WORK             PERVAL OUTPUT AREA                           
         USING PERVALD,R5                                                       
         MVC   STARTDAT,PVALBSTA   SAVE BINARY START DATE                       
         MVC   ENDDATE,PVALBEND    SAVE BINARY END DATE                         
         ZIC   R1,STARTDAT         GRAB THE YEAR (BITS 1-7 ONLY)                
         SH    R1,=H'1'            PREVIOUS YEAR                                
         STC   R1,STARTDAT                                                      
         B     VKLST35                                                          
         DROP  R5                                                               
*                                                                               
VKLST17  LA    R2,CLRDATEH         DATE FIELD                                   
         MVI   ERROR,INVDATE                                                    
         XC    WORK,WORK                                                        
         GOTO1 PERVAL,DMCB,(CLRDATEH+5,CLRDATE),WORK                            
         CLI   DMCB+4,PVRCINV1     DATE 1 INVALID?                              
         BE    ERRX                NO                                           
         CLI   DMCB+4,PVRCINV2     DATE 2 INVALID?                              
         BE    ERRX                NO                                           
*                                                                               
         LA    R5,WORK             PERVAL OUTPUT AREA                           
         USING PERVALD,R5                                                       
         MVC   STARTDAT,PVALBSTA   SAVE BINARY START DATE                       
         MVC   ENDDATE,PVALBEND    SAVE BINARY END DATE                         
         DROP  R5                                                               
         B     VKLST35                                                          
*                                                                               
VKLST35  MVC   CLRDATE,SPACES      DISPLAY                                      
         OI    CLRDATEH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(3,STARTDAT),(8,CLRDATE)                             
         MVI   CLRDATE+8,C'-'                                                   
         GOTO1 DATCON,DMCB,(3,ENDDATE),(8,CLRDATE+9)                            
         EJECT                                                                  
*---------------------------------------------------------------                
*                    VALIDATE THE PAYEE                                         
* VALID PAYEE: 'DIR' FOR ALL DIRECT PAYMENTS                                    
*              'S####' FOR A SPECIAL REP                                        
*              'P####' FOR A PAYING REP                                         
*---------------------------------------------------------------                
VKLST40  XC    PAYFLG,PAYFLG       PAYEE FILTER                                 
         MVI   ERROR,INVALID                                                    
         LA    R2,CLRPAYH                                                       
         CLI   5(R2),0                                                          
         BE    VKOPT                                                            
         GOTO1 ANY                                                              
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXCLC R1,WORK,=C'DIRECT'                                               
         BNE   *+14                                                             
         MVC   PAYFLG,=C'DIR '                                                  
         B     VKOPT                                                            
         CLI   WORK,C'P'                                                        
         BE    *+12                                                             
         CLI   WORK,C'S'                                                        
         BNE   ERRX                                                             
         MVC   PAYFLG(1),WORK      MOVE P OR S                                  
         MVC   WORK+8(4),WORK+1    SET UP FAKE FIELD HEADER                     
         MVC   WORK(8),0(R2)                   FOR SCANNER                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,WORK+5           NEW LENGTH                                   
         LA    R3,WORK                                                          
         XC    SCANBLK,SCANBLK                                                  
         GOTO1 SCANNER,DMCB,(R3),(2,SCANBLK)                                    
         CLI   DMCB+4,0                                                         
         BE    ERRX                                                             
         LA    R3,SCANBLK                                                       
         TM    2(R3),X'80'         VALID NUMERIC                                
         BNO   ERRX                                                             
         EDIT  (B4,4(R3)),(4,PAYFLG+1),FILL=0                                   
         SPACE 3                                                                
*---------------------------------------------------------------                
*        VALIDATE OPTION (S)                                                    
*---------------------------------------------------------------                
VKOPT    DS    0H                                                               
*                                                                               
         LA    R2,CLROPTH                                                       
         MVI   DATFLG,0                                                         
         XC    SCANBLK,SCANBLK                                                  
         GOTO1 SCANNER,DMCB,(R2),(1,SCANBLK)                                    
*                                                                               
         LA    R4,SCANBLK                                                       
*                                                                               
         CLI   0(R4),0                                                          
         BNE   VKOPT10                                                          
*                                                                               
         FOUT  CLRHDAH,=C'CK DATE  ',8                                          
         B     VKOPTX                                                           
*                                                                               
VKOPT10  DS    0H                                                               
*                                                                               
         CLC   12(3,R4),=C'CLE'      ONLY CHECK FOR 3 CHARS                     
         BNE   VKOPCLEN                                                         
*                                                                               
         MVI   DATFLG,C'C'         INDICATE CHECK CLEARANCE DATE                
         FOUT  CLRHDAH,=C'CLRD BNK',8                                           
         B     VKOPTX                                                           
*                                                                               
VKOPCLEN DS    0H                                                               
*                                                                               
         CLC   12(3,R4),=C'EST'      ONLY CHECK FOR 3 CHARS                     
         BNE   VKOPESTN                                                         
*                                                                               
         MVI   DATFLG,C'E'         INDICATE ESTIMATE                            
         FOUT  CLRHDAH,=C'ESTIMATE',8                                           
         B     VKOPTX                                                           
*                                                                               
VKOPESTN DS    0H                                                               
*                                                                               
         CLC   12(3,R4),=C'PID'      ONLY CHECK FOR 3 CHARS                     
         BNE   VKOPPIDN                                                         
*                                                                               
         MVI   DATFLG,C'P'         INDICATE PID                                 
         FOUT  CLRHDAH,=C'  PID   ',8                                           
         B     VKOPTX                                                           
*                                                                               
VKOPPIDN DS    0H                                                               
*                                                                               
         MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*                                                                               
VKOPTX   DS    0H                                                               
*                                                                               
*---------------------------------------------------------------                
*                    BUILD KEY                                                  
*---------------------------------------------------------------                
VKKEY    DS    0H                  BUILD CLEARANCE STATUS RECD KEY              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PPCLRST,R4                                                       
         MVC   PPCLAGY,AGENCY                                                   
         MVC   PPCLMED,QMED                                                     
         MVI   PPCLTYPE,X'25'                                                   
         MVC   PPCLCLT,QCLT                                                     
         MVC   PPCLPUB(4),BPUB                                                  
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* *******************************************************************           
* ON-SCREEN LIST                                                                
* *******************************************************************           
LR       LA    R4,KEY                                                           
         LA    R2,CLRLST1H         SETUP THIS LIST SO IT DOESN'T GO FOR         
         ST    R2,ATHISLST         ...THE FIRST UNPROTECTED FIELD               
         MVI   NLISTS,13            #LIST LINES IS < THE DEFAULT                
*                                                                               
         USING PPCLRST,R4                                                       
*                                                                               
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                NO                                           
*                                                                               
         MVC   KEY,ORIGKEY         RESET TO ORIG KEY BUILT IN VALKEY            
*                                                                               
         XC    ELM01DSP,ELM01DSP   INITIALIZE ELM 01  DISPLACEMENT              
         XC    ELM03DSP,ELM03DSP   INITIALIZE ELM 03  DISPLACEMENT              
*                                                                               
         XC    LASTKEY,LASTKEY                                                  
*                                                                               
         B     LR10HI                                                           
*                                                                               
LR10     OC    ELM01DSP,ELM01DSP   HAD WE READ ALL OF LAST REC'S ELEMS?         
         BZ    *+10                YES                                          
         MVC   KEY,LASTKEY         NO, RESTORE KEY OF LAST RECD                 
*                                                                               
LR10HI   GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LASTKEY,KEY         SAVE KEY                                     
*                                                                               
*                                  SAME RECD AGY/MED/TYP/CLT/PUB?               
         CLC   PPCLKEY(PPCLDATE-PPCLKEY),ORIGKEY                                
         BNE   LRX                 NO MORE RECDS TO LIST                        
*                                                                               
         OC    ELM01DSP,ELM01DSP   DO WE HAVE TO BUMP TO RT ELEMENT?            
         BZ    LR30                NO,                                          
*                                                                               
         GOTO1 GETREC              YES. GET THE RECORD                          
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO              START OF RECORD ADDRESS                      
         A     R6,ELM01DSP         R6 PTS TO NEXT ELEMENT                       
*                                                                               
         MVC   DATADSP,=Y(PPCLELEM-PPCLRST)   INIT FOR NEXTEL LATER ON          
         MVI   ELCODE,X'01'                                                     
*                                                                               
         CLI   0(R6),0             END OF RECORD?                               
         BNE   LR40                NO,                                          
*                                                                               
LR20     LA    R4,KEY              GET NEXT RECORD                              
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELM01DSP,ELM01DSP   NEW RECD->INITIALIZE ELMNT PNTR              
         XC    ELM03DSP,ELM03DSP   NEW RECD->INITIALIZE ELMNT PNTR              
         MVC   LASTKEY,KEY         SAVE KEY                                     
*                                                                               
LR30     DS    0H                   SAME RECD AGY/MED/TYP/CLT/PUB?              
         CLC   PPCLKEY(PPCLDATE-PPCLKEY),ORIGKEY                                
         BNE   LRX                 NO - NO MORE RECDS TO LIST                   
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LR40     L     R4,AIO                                                           
*                                                                               
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         LA    R7,LISTAR                                                        
         USING LISTD,R7            ESTABLISH LIST LINE                          
*                                                                               
         OC    ELM01DSP,ELM01DSP   IN THE MIDDLE OF A RECD?                     
         BNZ   LR50                YES                                          
*                                                                               
         L     R6,AIO                                                           
         USING PPCLEL01,R6         NAME ELEMENT'S DSECT                         
*                                                                               
         MVC   DATADSP,=Y(PPCLELEM-PPCLRST)                                     
         MVI   ELCODE,X'01'                                                     
*                                                                               
         BAS   RE,GETEL            FIND CLERANCE DETAILS ELEMENTS               
         BE    *+6                 IF THERE, CONTINUE, ELSE BUG                 
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
*                                                                               
         L     RF,AIO                                                           
         LR    R1,R6                                                            
         SR    R1,RF                                                            
         ST    R1,ELM01DSP         SAVE DISP TO 1ST 01 ELEMENT                  
*                                                                               
LR50     CLC   PPCLCLRD,ENDDATE    FILTER ON PERIOD                             
         BH    LRNXTEL                                                          
*                                                                               
         CLC   PPCLCLRD,STARTDAT                                                
         BL    LRNXTEL             DATE NO GOOD                                 
*                                                                               
         OC    PAYFLG,PAYFLG       ANY PAYEE FILTER?                            
         BZ    LR70                                                             
*                                                                               
         CLC   PAYFLG,=C'DIR '     DIRECT FLAG (PYEE=X'00')                     
         BNE   LR60                                                             
*                                                                               
         OC    PPCLPYEE,PPCLPYEE   DIRECT IF PYEE=X'00'                         
         BZ    LR70                YES, IT'S GOOD                               
*                                                                               
         CLC   PPCLPYEE,=C'0000'   DIRECT IF PYEE=C'00' (X'F0F0')               
         BE    LR70                YES, IT'S GOOD                               
*                                                                               
         B     LRNXTEL             NO,  NO   GOOD                               
*                                                                               
LR60     CLC   PPCLREPT(5),PAYFLG                                               
         BNE   LRNXTEL                                                          
*                                                                               
LR70     DS    0H                                                               
*                                                                               
         OC    BPUB+4(1),BPUB+4    ZONE SPECIFIED                               
         BZ    LR75                                                             
*                                                                               
         CLC   PPCLZONE,BPUB+4     MATCH                                        
         BNE   LRNXTEL             NO                                           
*                                                                               
LR75     OC    BPUB+5(1),BPUB+5    AND EDITION SPECIFIED                        
         BZ    LR80                                                             
*                                                                               
         CLC   PPCLEDIT,BPUB+5     MATCH                                        
         BNE   LRNXTEL             NO                                           
*                                                                               
         B     LR80                ELEM IS GOOD- GO PRINT IT                    
*                                                                               
LR80     MVC   LISTAR,SPACES                                                    
         MVC   KPUB,BPUB                                                        
*                                                                               
         OC    ELM03DSP,ELM03DSP   SKIP IF DOING 2ND OR HIGHER INVOICE          
         BNZ   LRINV                                                            
*                                                                               
         CLC   PPCLZONE(2),=X'FFFF' SKIP IF PAID ACROSS ZONES/EDITIONS          
         BE    LR83                                                             
*                                                                               
         MVC   KPUB+4(2),PPCLZONE    GET ZONE/EDT                               
         XC    PUBEXP,PUBEXP                                                    
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,(8,KPUB),(C'S',PUBEXP),RR=RELO                  
*                                                                               
         MVC   LPUB,PUBEXP+9                                                    
*                                                                               
         B     LR85                                                             
*                                                                               
LR83     MVC   LPUB(3),=C'ALL'                                                  
*                                                                               
LR85     DS    0H                                                               
*                                                                               
         MVC   LPRD(3),PPCLPRD     DISPLAY PRODUCT                              
*                                                                               
         MVC   LREPT,PPCLREPT      1 CHAR REPT TYPE CODE                        
         MVC   LPYEE,PPCLPYEE      PAYEE                                        
*                                                                               
         OC    PPCLPYEE,PPCLPYEE   DIRECT IF PYEE=X'00'                         
         BZ    *+14                                                             
         CLC   PPCLPYEE,=C'0000'   DIRECT IF PYEE=C'0000'                       
         BNE   *+10                                                             
         MVC   LPYEE(4),=C'DIR '                                                
*                                                                               
*        DISPLAY SEQUENCE NUMBER                                                
*                                                                               
         LLC   RF,PPCLCLSQ         GET SEQUENCE NUMBER                          
*                                                                               
         CHI   RF,X'FF'            IF SEQ # IS 255                              
         BNE   *+12                                                             
         SR    RE,RE                                                            
         ICM   RE,3,PPCLCLS2       GET SECONDARY SEQUENCE NUMBER                
         AR    RF,RE               ADD TO PRIMARY SEQUENCE NUMBER               
*                                                                               
         ST    RF,FULL             SAVE SEQUENCE NUMBER                         
*                                                                               
         EDIT  FULL,(4,WRK)                                                     
*                                                                               
         OC    WRK,=4X'F0'                                                      
*                                                                               
         MVC   LSEQ,WRK+1                                                       
*                                                                               
*        DISPLAY CLEARANCE DATE                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(3,PPCLCLRD),(8,LCLRDT)                              
*                                                                               
*        DISPLAY INVOICE NUMBER                                                 
*                                                                               
LRINV    DS    0H                                                               
*                                                                               
         OC    ELM03DSP,ELM03DSP   IF 2ND OR HIGHER INVOICE                     
         BZ    LRINV10                                                          
*                                                                               
         L     R2,AIO              POINT TO START OF RECORD                     
         A     R2,ELM03DSP         POINT TO NEXT 03 ELM                         
         B     LRINV20                                                          
*                                                                               
LRINV10  DS    0H                                                               
*                                                                               
         LLC   RF,PPCLEL01+1       GET 01 ELEMENT LENGTH                        
         LA    R2,0(RF,R6)         POINT TO NEXT ELEMENT                        
         USING PPCLEL03,R2         ESTABLISH 03 ELEMENT                         
*                                                                               
LRINV20  DS    0H                                                               
*                                                                               
         CLI   PPCLEL03,X'03'      SKIP IF NOT AN 03 ELEMENT                    
         BNE   LRINVX                                                           
*                                                                               
         MVC   LINV,PPCLINV        INVOICE NUMBER                               
*                                                                               
         LLC   RF,PPCLLN03         GET ELEMENT LENGTH                           
         LA    R2,0(RF,R2)         BUMP TO NEXT ELEMENT                         
*                                                                               
LRINVX   DS    0H                                                               
*                                                                               
*        DISPLAY INVOICE AMOUNT                                                 
*                                                                               
         USING PPCLEL05,R2         ESTABLISH 05 ELEMENT                         
*                                                                               
         CLI   PPCLEL05,X'05'      SKIP IF NOT AN X'05' ELEMENT                 
         BNE   LRAMT10                                                          
*                                                                               
         TM    PPCLSTAT,X'20'      IF NOT NET CLEARED                           
         BO    *+12                                                             
         ICM   RF,15,PCL5GRS          USE GROSS AMOUNT                          
         B     LRAMT20                                                          
*                                                                               
         ICM   RF,15,PCL5NET       ELSE USE NET                                 
         BNZ   *+10                                                             
         ICM   R0,15,PCL5CD        IF NET IS ZERO THEN ITS TRUE NET             
         SR    RF,R0               NEED NET LESS CD                             
*                                                                               
         B     LRAMT20                                                          
*                                                                               
LRAMT10  DS    0H                  USE MASTER ELEMENT AMOUNTS                   
*                                                                               
         ICM   RF,15,PPCLGRS       DEFAULT TO GROSS                             
*                                                                               
         OC    PPCLNET,PPCLNET     EITHER NET OR GROSS WILL HAVE VALUE          
         BZ    *+8                                                              
         ICM   RF,15,PPCLNET                                                    
*                                                                               
LRAMT20  DS    0H                                                               
*                                                                               
         LPR   RE,RF               GET ABSOLUTE VALUE                           
*                                                                               
         C     RE,=F'10000000'     IF GE 100,000.00 DROP PENNIES                
         BNL   LRAMT25                                                          
*                                                                               
         EDIT  (RF),(9,LAMT),2,MINUS=YES                                        
*                                                                               
         B     LRAMTX                                                           
*                                                                               
LRAMT25  DS    0H                                                               
*                                                                               
         CVD   RF,WRKDUB           CVD                                          
         SRP   WRKDUB,64-2,5       DIVIDE OUT PENNIES                           
*                                                                               
         EDIT  (P8,WRKDUB),(9,LAMT),0,MINUS=YES                                 
*                                                                               
LRAMTX   DS    0H                                                               
*                                                                               
*        DISPLAY CD                                                             
*                                                                               
LRCD     DS    0H                                                               
*                                                                               
         CLI   PPCLEL05,X'05'      IF NO X'05' ELEMENT                          
         BE    LRCD10                                                           
*                                                                               
         EDIT  (B4,PPCLCD),(7,LCD),2,MINUS=YES                                  
*                                                                               
         B     LRCDX                                                            
*                                                                               
LRCD10   DS    0H                                                               
*                                                                               
         ICM   R1,15,PPCLGRS       ASSUME PAYING GROSS - TOTAL                  
         BNZ   *+8                 IF NO GROSS                                  
         ICM   R1,15,PPCLNET          USE NET                                   
*                                                                               
         ICM   RF,15,PCL5GRS       ASSUME PAYING GROSS - THIS INVOICE           
         BNZ   *+8                 IF NO GROSS                                  
         ICM   RF,15,PCL5NET          USE NET                                   
         BNZ   *+10                                                             
         ICM   R0,15,PCL5CD        IF NET IS ZERO THEN ITS TRUE NET             
         SR    RF,R0               NEED NET LESS CD                             
*                                                                               
         EDIT  (B4,PCL5CD),(7,LCD),2,MINUS=YES                                  
*                                                                               
         B     LRCDX                                                            
*                                                                               
LRCDX    DS    0H                                                               
*                                                                               
*        DISPLAY CHECK NUMBER/DATE                                              
*                                                                               
LRCHK    DS    0H                                                               
*                                                                               
         CLI   PPCLEL05,X'05'      SKIP IF NOT AN X'05' ELEMENT                 
         BNE   LRCHK50                                                          
*                                                                               
         MVC   LCKNUM,PCL5CHK      CHECK NUMBER                                 
*                                                                               
*        ESTIMATE NUMBER AND PID PRESENT ONLY IF X'05'                          
*              ELEMENT IS PRESENT - ADDED DURING SAME UPDATE                    
*                                                                               
         TM    PCL5STAT,PCL5STAT_RECON RECONCILED?                              
         BNO   *+8                                                              
         MVI   LCKRCN,C'*'                                                      
*                                                                               
         CLI   DATFLG,C'E'         SHOW ESTINMTE NUMBER ?                       
         BNE   LRCHKESN                                                         
*                                                                               
         CLI   PPCLEL01+1,PPCLELL2 SKIP IF DATA NOT IN 01 ELEMENT               
         BL    LRCHKESX                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,PPCLEST        GET ESTIMATE NUMBER                          
         BZ    LRCHKESX                                                         
*                                                                               
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  LCKDT+2(3),DUB+6(2) DISPLAY ESTIMATE #                           
*                                                                               
LRCHKESX DS    0H                                                               
*                                                                               
         B     LRCHK99                                                          
*                                                                               
LRCHKESN DS    0H                                                               
*                                                                               
         CLI   DATFLG,C'P'         IF DISPLAYING PID                            
         BNE   LRCHKPIN                                                         
*                                                                               
         OC    PPCLPID,PPCLPID        SKIP IF NO PID FOUND                      
         BZ    LRCHKPIX                                                         
*                                                                               
*        READ PERSON AUTH REC ON CTFILE                                         
*                                                                               
         ICM   R0,15,AIO           SAVE CURRENT AIO                             
         MVC   CURRKEY,KEY         SAVE CURRENT KEY                             
*                                                                               
         LA    R3,KEY                                                           
         USING CT0REC,R3           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
*                                                                               
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
*                                                                               
         GOTO1 CGETFACT,DMCB,(2,0),0,0   RETURN TIME IN TUS                     
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
*                                                                               
         MVC   CT0KAGY,FATAGYSC    SECURITY AGENCY                              
*                                                                               
         CLC   CT0KAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   CT0KAGY,AGENCY         USE BUYREC'S AGENCY                       
*                                                                               
         MVC   CT0KNUM,PPCLPID     SET PID                                      
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
*                                                                               
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
*                                                                               
         L     R3,AIO              POINT TO FOUND RECORD                        
*                                                                               
         CLC   CT0KEY,KEYSAVE      SKIP IF RECORD NOT FOUND                     
         BNE   LRCHKPIX                                                         
*                                                                               
*        FIND USER'S ID                                                         
*                                                                               
*        FIND PERSON'S ID ELEMENT                                               
*                                                                               
         LA    RE,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
LRCHKPLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    LRCHKPDN                                                         
*                                                                               
         CLI   0(RE),X'C3'         - MATCH ON ELEMENT CODE                      
         BE    LRCHKPFD                                                         
*                                                                               
LRCHKPCN DS    0H                                                               
*                                                                               
         IC    RF,1(RE)            GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     LRCHKPLP            GO FIND NEXT ELEMENT                         
*                                                                               
LRCHKPDN DS    0H                  NO PERSON ID FOUND                           
*                                                                               
         B     LRCHKPIX                                                         
*                                                                               
LRCHKPFD DS    0H                                                               
*                                                                               
         MVC   LCKDT,2(RE)         DISPLAY PID                                  
*                                                                               
LRCHKPIX DS    0H                                                               
*                                                                               
         STCM  R0,15,AIO           RESTORE CURRENT AIO                          
         MVC   KEY,CURRKEY         RESTORE CURRENT KEY                          
         GOTO1 HIGH                RESTORE FILE POINTERS                        
*                                                                               
         B     LRCHK99                                                          
*                                                                               
LRCHKPIN DS    0H                                                               
*                                                                               
         CLI   DATFLG,C'C'         SHOW CLEARED BANK DATE ?                     
         BE    LRCHK10             YES - DO NOT SHOW "CHECK DATE"               
*                                                                               
         GOTO1 DATCON,DMCB,(2,PCL5CHDT),(8,LCKDT)                               
*                                                                               
LRCHK10  DS    0H                                                               
*                                                                               
         OC    PCL5BKDT,PCL5BKDT   CLEARED BANK DATE ?                          
         BZ    LRCHK49             NO                                           
*                                                                               
         MVI   LCKCLR,C'C'         "CLEARED CHECK" INDICATOR                    
*                                                                               
         CLI   DATFLG,C'C'         SHOW CLEARED BANK DATE ?                     
         BNE   LRCHK49             NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(2,PCL5BKDT),(8,LCKDT)  REPLACE CHECK DATE           
*                                                                               
LRCHK49  DS    0H                                                               
*                                                                               
         B     LRCHKX                                                           
*                                                                               
LRCHK50  DS    0H                                                               
*                                                                               
         MVC   LCKNUM,PPCLCHK      CHECK NUMBER                                 
*                                                                               
         TM    PPCLSTAT,X'80'      RECONCILED?                                  
         BNO   *+8                                                              
         MVI   LCKRCN,C'*'                                                      
*                                                                               
         CLI   DATFLG,C'E'         SKIP IF ESTIMATE                             
         BE    *+8                                                              
         CLI   DATFLG,C'P'         OR PID DISPLAY - NO DATA PRESENT             
         BE    LRCHKX                                                           
*                                                                               
         CLI   DATFLG,C'C'         SHOW CLEARED BANK DATE ?                     
         BE    LRCHK60             YES - DO NOT SHOW "CHECK DATE"               
*                                                                               
         GOTO1 DATCON,DMCB,(2,PPCLCHDT),(8,LCKDT)                               
*                                                                               
LRCHK60  DS    0H                                                               
*                                                                               
         OC    PPCLBKDT,PPCLBKDT   CLEARED BANK DATE ?                          
         BZ    LRCHK99             NO                                           
*                                                                               
         MVI   LCKCLR,C'C'         "CLEARED CHECK" INDICATOR                    
*                                                                               
         CLI   DATFLG,C'C'         SHOW CLEARED BANK DATE ?                     
         BNE   LRCHK99             NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(2,PPCLBKDT),(8,LCKDT)  REPLACE CHECK DATE           
*                                                                               
LRCHK99  DS    0H                                                               
*                                                                               
LRCHKX   DS    0H                                                               
*                                                                               
LR89     DS    0H                                                               
*                                                                               
         CLI   PPCLEL05,X'05'      IF WE HAVE 05 ELEMENT                        
         BNE   LRCONT10                                                         
*                                                                               
         LLC   RF,PPCLLN05            GET ELEMENT LENGTH                        
         LA    R2,PPCLEL05(RF)        BUMP TO NEXT ELEMENT                      
*                                                                               
         CLI   PPCLEL05,X'03'         IF 03 - INVOICE ELEMENT                   
         BNE   LRCONT10                                                         
*                                                                               
         L     RF,AIO                    POINT TO START OF RECORD               
         SR    R2,RF                     CALC DISPLACEMENT TO 03 ELM            
         ST    R2,ELM03DSP               SAVE DISPLACEMENT                      
         B     LRCONT20                                                         
*                                                                               
LRCONT10 DS    0H                                                               
*                                                                               
         LR    R1,R6               SAVE CURRENT ELM 01 PTR                      
         XC    ELM03DSP,ELM03DSP   FORCE NEXT INVOICE ELM                       
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT 01 ELEMENT                         
*                                                                               
         L     RF,AIO                                                           
         SR    R6,RF                                                            
         ST    R6,ELM01DSP         IN CASE WE HIT END OF SCREEN                 
         LR    R6,R1               RESTORE 01 ELM POINTER                       
*                                                                               
LRCONT20 DS    0H                                                               
*                                                                               
         GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
*                                                                               
         OC    ELM03DSP,ELM03DSP   IF WE HAVE ANOTHER 03 ELEMENT                
         BNZ   LR50                   GO PROCESS                                
*                                                                               
         B     LR90                ELSE NEXT 01 ELEMENT                         
*                                                                               
LRNXTEL  DS    0H                                                               
*                                                                               
LR90     DS    0H                                                               
*                                                                               
         XC    ELM03DSP,ELM03DSP   FORCE NEXT INVOICE ELM                       
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT 01 ELEMENT                         
         BNE   LR20                NO MORE, READ NEXT RECORD                    
*                                                                               
*                                  GOT ANOTHER                                  
         L     RF,AIO                                                           
         LR    RE,R6                                                            
         SR    RE,RF                                                            
         ST    RE,ELM01DSP         SAVE DISPLACEMENT                            
*                                                                               
         B     LR50                PROCESS NEXT 01 ELEMENT                      
*                                                                               
LRX      XIT1                                                                   
*                                                                               
         DROP  R6,R2                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
         SPACE 5                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
ERRX     GOTO1 ERREX                                                            
         B     XIT                                                              
**                                                                              
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMF4D          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    F                                                                
PAYFLG   DS    CL5                                                              
PUBFLG   DS    CL1                                                              
DATFLG   DS    CL1    C=SHOW CLEARED BANK DATE (INSTEAD OF CHECK DATE)          
KPUB     DS    CL6                                                              
ORIGKEY  DS    XL(L'KEY)                                                        
SAVEKEY  DS    XL(L'KEY)                                                        
LASTKEY  DS    XL(L'KEY)                                                        
STARTDAT DS    XL3                                                              
ENDDATE  DS    XL3                                                              
DATADSP  DS    H                                                                
WRK      DS    CL4                                                              
WRK2     DS    CL80                                                             
ELM01DSP DS    F                                                                
ELM03DSP DS    F                                                                
PUBEXP   DS    CL18                                                             
ALLZE    DS    CL1                                                              
SCANBLK  DS    CL70                                                             
WRKDUB   DS    D                   WORK DOUBLE WORD                             
CURRKEY  DS    XL(L'KEY)           CURRENT KEY SAVEAREA                         
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
*                                                                               
LISTD    DSECT                                                                  
LPUB     DS    CL7                 PUBLICATION ZONE AND EDITION                 
         DS    CL1                                                              
LREPT    DS    CL1                 REP TYPE                                     
LPYEE    DS    CL4                 PAYEE                                        
         DS    CL1                                                              
LCLRDT   DS    CL8                 CLEARANCE DATE                               
         DS    CL1                                                              
LSEQ     DS    CL3                 SEQUENCE                                     
         DS    CL1                                                              
LPRD     DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
LINV     DS    CL11                INVOICE NUMBER                               
         DS    CL1                                                              
LAMT     DS    CL9                 AMOUNT                                       
         DS    CL1                                                              
LCD      DS    CL7                 AMOUNT OF CASH DISCOUNT                      
         DS    CL1                                                              
LCKCLR   DS    CL1                 CHECK CLEARED INDICATOR                      
LCKNUM   DS    CL6                 CHECK NUMBER                                 
LCKRCN   DS    CL1                 RECONCILIATION INDICATOR                     
         DS    CL1                                                              
LCKDT    DS    CL8                 CHECK DATE                                   
         EJECT                                                                  
       ++INCLUDE PPCLRST                                                        
         ORG                                                                    
*DDSPOOLD                                                                       
*DDCOMFACS                                                                      
*DDFLDIND                                                                       
*DDPERVAL                                                                       
*PPSRCHPARM                                                                     
*FAFACTS                                                                        
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE PPSRCHPARM                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019PRSFM04   02/16/21'                                      
         END                                                                    
