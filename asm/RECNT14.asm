*          DATA SET RECNT14    AT LEVEL 007 AS OF 05/01/02                      
*PHASE T80214A,+0                                                               
         TITLE 'T80214 - REPPAK REMOTE CONTRACT COVERSHEET PRINT'               
*                                                                               
*********************************************************************           
*                                                                   *           
*        RECNT14 --- VALIDATE CONTRACT LIST AND PRODUCE CONTRACT    *           
*                     COVER SHEET REPORT                            *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* MAR13/89 (MRR) --- INITIAL RELEASE                                *           
* APR26/89 (SNS) LEVEL 85 - FINAL RELEASE                           *           
*                                                                   *           
* CNT HISTORY:                                                      *           
*                                                                   *           
* 23SEP95 SKU  2K CONTRACT SUPPORT                                 *            
* 08APR96 RHV  SUPPORT 34 BYTE AGY ADDRESS FIELDS                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
T80214   CSECT                                                                  
         NMOD1 0,T80214,RR=R5,R9                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R8,RA                                                            
         AH    R8,=H'4096'         4K                                           
         USING TWAWORK,R8                                                       
*                                                                               
*        MAIN CONTROL                                                           
*                                                                               
         BAS   RE,DOSCRN          VALIDATE/GET INPUT SCREEN                     
         SPACE                                                                  
         LH    R1,RECCNT          CONTRACT RECORD COUNTER                       
         LTR   R1,R1                                                            
         BNZ   MAIN30                                                           
         LA    R2,CONCACTH                                                      
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,43                                                  
         B     EXXMOD             NO RECORDS                                    
*                                                                               
MAIN30   BAS   RE,INIT            INITIALIZE PRINT QUEUE                        
         SPACE                                                                  
         BAS   RE,PUTSREC         GET CON INFO/PUT RECS INTO STAREA             
         SPACE                                                                  
         LH    R0,RECCNT          SORT RECORDS IN AREA BY MKT/STA               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4        FOR ADDRESS TO XSORT                          
         GOTO1 CXSORT,DMCB,STAREA,(R0),L'SLEN,25,8                              
         SPACE                                                                  
         DROP  R4                                                               
         BAS   RE,PRTCON           PRINT CONTRACT COVERSHEET                    
*                                                                               
* GO TO PRTQUE WITH LAST TIME                                                   
*                                                                               
LAST     EQU   *                                                                
         MVI   MYP-1,X'FF'         LAST TIME                                    
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 DATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                  
*                                                                               
         LA    R2,CONCACTH         CURSOR                                       
         CLI   TWAACCS,C'$'        IF STATION, IT'S 'ACE'                       
         BE    EXXMOD              DON'T SHOW THIS CONMSG                       
         SPACE 1                                                                
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,51                                                  
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* DOSCRN --- PROCESS THE INPUT SCREEN                                           
*                                                                               
DOSCRN   EQU   *                                                                
         NTR1                                                                   
         XC    RECCNT,RECCNT      RECORD COUNTER FOR XSORT                      
         LA    R4,COVLIST         PT TO COVERLIST                               
         MVI   DOCONVAL,0         SET DEFAULT VALUE                             
         LA    R2,COVVALH         EDIT HEADLINES                                
         CLI   5(R2),0                                                          
         BE    DSCN10                                                           
         CLI   8(R2),C'Y'                                                       
         BE    DSCN10                                                           
         LA    R3,INVINP          ERROR MSG- INVALID INPUT                      
         CLI   8(R2),C'N'                                                       
         BNE   ERROR                                                            
         MVI   DOCONVAL,1         BLANK/Y/N ONLY VALID ANSWERS                  
         SPACE                                                                  
DSCN10   LA    R2,COVCON1H        EDIT CONTRACT LIST                            
         SPACE                                                                  
         MVI   NOINPUT,0          DEFAULT VALUE                                 
         LA    R7,42              # OF FIELDS ON SCREEN                         
DSCN100  BAS   RE,DOFIELD                                                       
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0              NEXT CONTRACT FIELD                           
         CLI   NOINPUT,1          ANY INPUT?                                    
         BE    DSCN120            NO -INPUT DON'T INCREMENT TABLE               
         LA    R4,24(R4)                                                        
         LH    R1,RECCNT                                                        
         LA    R1,1(R1)                                                         
         STH   R1,RECCNT          INCREMENT AND STORE RECORD COUNTER            
*                                                                               
DSCN120  MVI   NOINPUT,0          RESET FLAG                                    
         BCT   R7,DSCN100         LOOP                                          
         XIT                                                                    
         EJECT                                                                  
*                                                                               
*        DOFIELD --- VALIDATE A SCREEN FIELD POINTED TO BY R2                   
*                                                                               
DOFIELD  EQU   *                                                                
         NTR1                                                                   
         CLI   5(R2),0            I/P?                                          
         BNE   DFLD20             YES                                           
         MVI   NOINPUT,1          SET NO INPUT                                  
         B     DFLDEXIT           JUST GO TO NEXT FIELD                         
         SPACE                                                                  
DFLD20   EQU   *                                                                
         LA    R3,117             FIELD NOT NUMERIC                             
         TM    4(R2),X'08'        IS IT?                                        
         BZ    ERROR              ERROR - CONTRACT # MUST BE NUMERIC            
         XC    WORK,WORK                                                        
         GOTO1 VPACK              R2 POINTING TO CONTRACT #                     
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)                                               
         XC    RCONREC(32),RCONREC                                              
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,REPALPHA                                                
         MVC   RCONPCON,WORK                                                    
         MVC   KEY,RCONREC                                                      
         GOTO1 VHIGH               READ FOR CONTRACT                            
         LA    R3,CONERR           CONTRACT NOT FOUND                           
         CLC   KEY(27),KEYSAVE                                                  
         BE    DFLD40              MUST EXIST                                   
         B     ERROR                                                            
         SPACE                                                                  
DFLD40   GOTO1 VGETREC,DMCB,RCONREC                                             
         TM    RCONCNTL,X'80'                                                   
         BO    ERROR              CONTRACT DELETED                              
         CLI   DOCONVAL,1         CHECK ADV/AGY/PROD ?                          
         BE    DFLD100            NO DON'T VALIDATE HEADLINES                   
         OC    CONADV,MYSPACES                                                  
         CLC   RCONKADV(4),CONADV                                               
         BE    DFLD60                                                           
         LA    R3,162             ADVERTISER DOESN'T MATCH                      
         B     ERROR                                                            
         SPACE                                                                  
DFLD60   OC    CONAGY,MYSPACES                                                  
         CLC   RCONKAGY(4),CONAGY                                               
         BE    DFLD70                                                           
         LA    R3,163             AGENCY DOESN'T MATCH                          
         B     ERROR                                                            
         SPACE                                                                  
DFLD70   CLI   RCONPRD,0                                                        
         BE    DFLD100                                                          
         CLI   RCONPRD,C' '                                                     
         BE    DFLD100                                                          
         CLC   CONPRD(2),=C'C='                                                 
         BNE   DFLD100                                                          
         OC    CONPRD+2(3),MYSPACES                                             
         CLC   RCONPRD(3),CONPRD+2                                              
         BE    DFLD100                                                          
         LA    R3,164             PRODUCTS DOESN'T MATCH                        
         B     ERROR                                                            
         SPACE                                                                  
DFLD100  DS    0H                 CHECK FOR DUPLICATE CONTRACT                  
         LA    RE,COVLIST                                                       
DFLD110  CLC   0(4,RE),KEY+28     CHECK D/A AGAINST PREVIOUS ENTRIES            
         LA    R3,165              DUPLICATE CONTRACT NUMBER                    
         BE    ERROR                                                            
         LA    RE,24(RE)                                                        
         CR    RE,R4              END OF TABLE?                                 
         BL    DFLD110                                                          
*                                                                               
         MVC   0(4,R4),KEY+28          SAVE D/A                                 
         SPACE                                                                  
* READ STATION RECORD FOR MARKET                                                
         XC    KEY(32),KEY                                                      
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONREC+6        STATION CALL LETTERS                  
         GOTO1 VHIGH                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VGETREC,DMCB,RSTAREC                                             
*                                                                               
         LA    R6,RSTAELEM                                                      
         CLI   0(R6),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   4(20,R4),2(R6)             MARKET                                
*                                                                               
DFLDEXIT EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        INIT --- PROGRAM AND REPORT START-UP STUFF                             
*                                                                               
INIT     NTR1                                                                   
INIT20   EQU   *                                                                
         MVI   MAXLNES,78          MAX LINES                                    
         XC    WORK3,WORK3                                                      
         MVC   WORK3(2),=H'2'                                                   
         MVI   PG,1                PG COUNT                                     
* SET 1ST TIME FOR PRTQUE                                                       
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         MVI   MYP-1,0                                                          
         XC    MYP,MYP                                                          
         MVC   MYP(3),=C'CON'      ID                                           
         MVC   MYP+3(8),CONCNUM                                                 
         MVC   MYP+11(3),CONSAL    SET SUB-KEY                                  
         MVI   MYP+24,C'C'         CLASS 'C'                                    
*                                                                               
         LA    RE,MYP-1                                                         
         USING PQPLD,RE            PRINT QUEUE PRINT LNE                        
*                                                                               
         OC    SENDID,SENDID       IF SENDID, IT'S ACE/GRAPHNET                 
         BZ    INIT30                                                           
         MVC   PLUSER(2),SENDID                                                 
         XC    SENDID,SENDID       CLEAR SENDID                                 
*                                                                               
INIT30   EQU   *                                                                
         MVI   QLEXTRA,X'FF'       INDICATE NEW STYLE LIST                      
         MVC   QLSRCID,PLUSER                                                   
         MVC   QLSUBID,PLSUBID                                                  
         CLI   QLCLASS,0                                                        
         BNE   *+10                                                             
         MVC   QLCLASS,PLCLASS                                                  
         CLI   QLSTAT,0                                                         
         BNE   *+10                                                             
         MVC   QLSTAT,PLSTAT                                                    
         CLI   QLLPP,0                                                          
         BNE   *+10                                                             
         MVC   QLLPP,PLLPP                                                      
         MVC   QLDESC,PLDESC                                                    
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'       KEEP PRINTED CONTRACT 2 HOURS                
         DROP  RE                                                               
*                                                                               
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 DATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                  
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        END OF INIT                                                            
INITEXIT EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* READ EACH CONTRACT FROM D/A LIST FOR MORE INFO -                              
* THEN PUT RECORDS INTO STAREA FOR LATER SORTING BY MKT/STA                     
*                                                                               
PUTSREC  NTR1                                                                   
         LA    R5,STAREA           STORE AREA FOR SRECS                         
         USING SRECD,R5                                                         
         LA    R4,COVLIST          CONTRACT D/A LIST                            
*                                                                               
PUT10    XC    KEY(32),KEY                                                      
         MVC   KEY+28(4),0(R4)     MOVE IN D/A                                  
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
         MVC   SCON,RCONKCON      MOVE INFO INTO STAREA                         
         MVC   SMKT,4(R4)                                                       
         MVC   SSTA,RCONKSTA                                                    
*                                                                               
         LA    R3,RCONDATE         DEFAULT TO DISPLAY NORMAL DATES              
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   PUT15                                                            
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT   HAVE REVISED DATE?                           
         BZ    PUT15                                                            
         LA    R3,RCONRFLT         YES - USE IT                                 
         DROP  R6                                                               
PUT15    GOTO1 DATCON,DMCB,(3,0(R3)),(4,SSDATE)                                 
         GOTO1 DATCON,DMCB,(3,3(R3)),(4,SEDATE)                                 
         LA    R1,0               DEFAULT AMOUNT                                
         LA    R6,RCONCMEL                                                      
PUT30    CLI   0(R6),X'03'                                                      
         BH    PUT50              NO MORE X'03' ELEMENTS                        
         BL    PUT40              GET NEXT ELEMENT                              
*                                                                               
         A     R1,6(R6)           ADD TO STATION AMT TOTAL                      
PUT40    ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     PUT30              NEXT STATION AMT BUCKET                       
*                                                                               
PUT50    STCM  R1,15,SAMT                                                       
         LA    R4,24(R4)          NEXT CONTRACT ENTRY                           
         OC    0(24,R4),0(R4)                                                   
         BZ    END                                                              
         LA    R5,L'SLEN(R5)      NEXT SLOT IN STAREA                           
         B     PUT10              PROCESS NEXT RECORD                           
*                                                                               
END      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        PRTCON --- PRODUCE CONTRACT COVER SHEET DETAIL LINES                   
*                   BY READING SORTED RECORDS FROM STAREA                       
*                                                                               
PRTCON   NTR1                                                                   
         BAS   RE,HEADLINE        PRNT FIRST CONTRACT INFO                      
         MVI   MYP-1,C'-'         SPACE 3 LINES                                 
         BAS   RE,PRINT                                                         
         MVI   MYP-1,C'-'         SPACE 3 LINES                                 
         BAS   RE,PRINT                                                         
         MVI   MYP-1,C'-'         SPACE 3 LINES                                 
         BAS   RE,PRINT                                                         
         MVI   MYP-1,C'-'         SPACE 3 LINES                                 
         BAS   RE,PRINT                                                         
*                                                                               
         MVI   FRSTIME,1          FIRST TIME AROUND                             
         SR    R6,R6              STATION TOTAL AMOUNT                          
         SR    R7,R7              MARKET TOTAL AMOUNT                           
         SR    RE,RE                                                            
         SR    R3,R3              GRAND TOTAL AMOUNT                            
         STH   RE,STALCNT         CLEAR OUT STATION LNE CNT                     
         STH   RE,MKTCNT          CLEAR OUT # STATIONS IN MARKET                
         LA    R5,MYP+3           PRINT LNE                                     
         USING PLINED,R5                                                        
         LA    R4,STAREA          SORTED RECORDS                                
         USING SRECD,R4                                                         
         OC    0(43,R4),0(R4)     ANY RECORDS?                                  
         BZ    END                                                              
*                                                                               
PRT10    CLI   FRSTIME,1          FIRST TIME?                                   
         BNE   PRT11                                                            
         MVI   FRSTIME,0          RESET                                         
         B     PRT15                                                            
*                                                                               
PRT11    CLC   SVMKT,SMKT         SAME MARKET?                                  
         BE    PRT12                                                            
         MVI   MYP-1,X'40'        SPACE 1                                       
         BAS   RE,PRINT                                                         
         LH    RE,STALCNT                                                       
         C     RE,=F'1'           MORE THAN ONE STATION?                        
         BNH   PRT11E             NO- CHECK ABOUT MARKET TOTAL?                 
*                                                                               
         MVI   TOTIND,1           STATION TOTAL                                 
         BAS   RE,PRTTOT                                                        
*                                                                               
PRT11E   LH    RE,MKTCNT          # OF STATIONS IN MARKET                       
         C     RE,=F'1'           ONLY ONE DON'T DO MKT TOTAL                   
         BL    PRT11F                                                           
         MVI   TOTIND,0           DO MARKET TOTAL ONLY                          
         BAS   RE,PRTTOT                                                        
*                                                                               
PRT11F   SR    R7,R7              CLEAR MARKET TOTAL                            
         SR    R6,R6              CLEAR STATION TOTAL                           
         STH   R6,STALCNT         CLEAR OUT STATION LNE CTR                     
         STH   R6,MKTCNT          CLEAR STATIONS IN MARKET                      
         MVC   SVMKT,SMKT         SAVE NEW MARKET FOR BREAK                     
         B     PRT15                                                            
*                                                                               
PRT12    CLC   SSTA,SVSTA        SAME STATION?                                  
         BE    PRT15                                                            
         LH    RE,STALCNT                                                       
         C     RE,=F'1'          MORE THAN ONE STATION?                         
         BE    PRT14             NO - DON'T NEED STATION TOTAL                  
         MVI   TOTIND,1          1=STATION TOTAL                                
         BAS   RE,PRTTOT                                                        
PRT14    MVC   SVSTA,SSTA        SAVE NEW STATION FOR BREAK                     
         SR    R6,R6             CLEAR STATION TOTAL                            
         STH   R6,STALCNT        CLEAR STATION DETAIL LNE CNTR                  
         MVI   MYP-1,X'40'       SKIP LNE                                       
         BAS   RE,PRINT                                                         
*                                                                               
         LH    RE,MKTCNT                                                        
         LA    RE,1(RE)                                                         
         STH   RE,MKTCNT          INCREMENT MARKET TOTAL CNTR                   
*                                                                               
PRT15    ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),SCON                                                    
         EDIT  (P5,DUB+3),(8,PCON),ALIGN=LEFT                                   
*                                                                               
         MVC   PSTA,SSTA          STATION INTO PRINT                            
         MVC   SVSTA,SSTA         SAVE STATION FOR NEXT COMPARE                 
         MVC   PMKT,SMKT          PRINT MARKET                                  
         MVC   SVMKT,SMKT         SAVE MARKET FOR NEXT COMPARE                  
         MVC   PSDATE,SSDATE      PRINT START DATE                              
         MVC   PEDATE,SEDATE      PRINT END  DATE                               
         SR    RE,RE                                                            
         ICM   RE,15,SAMT         PRINT STATION AMT                             
         EDIT  (RE),(17,PAMT),2,COMMAS=YES,FLOAT=$                              
         AR    R6,RE              ADD TO STATION TOTAL                          
         AR    R7,RE              ADD TO MARKET TOTAL                           
         AR    R3,RE              ADD TO GRAND TOTAL                            
         LH    RE,STALCNT                                                       
         LA    RE,1(RE)                                                         
         STH   RE,STALCNT         INCREMENT STATION LNE CNT                     
*                                                                               
         MVI   MYP-1,X'40'        SPACE 1 BEFORE PRINT                          
         BAS   RE,PRINT           PRINT ONE DETAIL LNE                          
         LA    R4,L'SLEN(R4)      NEXT RECORD ENTRY IN STAREA                   
         OC    0(43,R4),0(R4)                                                   
         BZ    PRT30                                                            
*                                                                               
         CLI   LNE,75             FINISHED FIRST PG?                            
         BL    PRT10              NEXT DETAIL LNE                               
*                                                                               
         BAS   RE,HEADLINE        PRINT HEADER                                  
         MVI   MYP-1,C'-'         SPACE 3 LINES                                 
         BAS   RE,PRINT                                                         
         MVI   MYP-1,C'-'         SPACE 3 LINES                                 
         BAS   RE,PRINT                                                         
         MVI   MYP-1,C'-'         SPACE 3 LINES                                 
         BAS   RE,PRINT                                                         
         MVI   MYP-1,C'-'         SPACE 3 LINES                                 
         BAS   RE,PRINT                                                         
         B     PRT10                                                            
*                                                                               
PRT30    MVI   TOTIND,1           INDICATE STATION TOTAL                        
         LH    RE,STALCNT                                                       
         C     RE,=F'1'           MORE THAN ONE STATION                         
         BNH   PRT50              NO                                            
PRT40    BAS   RE,PRTTOT                                                        
*                                                                               
PRT50    MVI   TOTIND,0           INDICATE MARKET TOTAL                         
         LH    RE,MKTCNT          STATIONS IN MARKET                            
         C     RE,=F'1'                                                         
         BL    PRT60              NO ONLY ONE STATION                           
         BAS   RE,PRTTOT                                                        
*                                                                               
PRT60    MVI   MYP-1,X'40'                                                      
         MVC   MYP(15),=C'GRAND TOTAL   -'                                      
         EDIT  (R3),(17,MYP+16),2,COMMAS=YES,FLOAT=$                            
         BAS   RE,PRINT                                                         
         B     END                                                              
         EJECT                                                                  
PRTTOT   NTR1                                                                   
         DS    0H                                                               
         CLI   TOTIND,2           STATION AND MARKET?                           
         BE    TOT20                                                            
*                                                                               
         CLI   TOTIND,1                                                         
         BNE   TOT40              ONLY STATION                                  
*                                                                               
TOT20    MVI   MYP-1,X'40'        PRINT 1 BLANK LNE                             
         MVC   MYP(15),=C'STATION TOTAL -'                                      
         EDIT  (R6),(17,MYP-16),2,COMMAS=YES,FLOAT=$                            
         BAS   RE,PRINT                                                         
*                                                                               
         CLI   TOTIND,1                                                         
         BE    TOT60              ONLY STATION                                  
*                                                                               
TOT40    XC    MYP,MYP                                                          
         MVI   MYP-1,X'40'        PRINT 1 BLANK LNE                             
         BAS   RE,PRINT                                                         
*                                                                               
         MVI   MYP-1,X'40'        PRINT 1 BLANK LNE                             
         MVC   MYP(15),=C'MARKET TOTAL  -'                                      
         EDIT  (R7),(17,MYP+16),2,COMMAS=YES,FLOAT=$                            
         BAS   RE,PRINT                                                         
*                                                                               
TOT60    XC    MYP,MYP                                                          
         MVI   MYP-1,C'0'           PRINT 2 BLANK LINES                         
         MVI   TOTIND,0                                                         
         BAS   RE,PRINT                                                         
         B     END                                                              
         EJECT                                                                  
         TITLE 'T80214- CONTRACT COVERSHEET HEADLINE ROUTINE'                   
HEADLINE NTR1                                                                   
* BUILD AND PRINT CONTRACT HEADLINE                                             
         MVI   LNE,0               LNE CTR                                      
         XC    MYP,MYP                                                          
         MVI   MYP-1,X'F1'           SKIP TO NEW PG                             
         MVC   MYP+10(33),TWAREPNM   REP NAME                                   
         BAS   RE,PRINT                                                         
*                                                                               
         MVI   MYP-1,X'40'           SPACE 1 BEFORE PRINT                       
         MVC   MYP+10(20),TWAOFAD1   OFF ADDR LN 1                              
*                                                                               
         BAS   RE,PRINT                                                         
         MVI   MYP-1,X'40'                                                      
         MVC   MYP+10(18),TWAOFAD2   OFF ADDR LN 2                              
* FLOAT STATE AND ZIP                                                           
         LA    R4,MYP+27                                                        
         LA    R5,MYP+10                                                        
HL3      CR    R4,R5                                                            
         BL    HL3K                                                             
         OI    0(R4),X'40'                                                      
         CLI   0(R4),X'40'                                                      
         BNE   HL3K                                                             
         BCT   R4,HL3                                                           
HL3K     MVC   3(2,R4),TWAOFSTT                                                 
         MVC   7(10,R4),TWAOFZIP                                                
         SPACE 1                                                                
         BAS   RE,PRINT                                                         
         BAS   RE,PRINT            SPACE 1 LNE                                  
*                                                                               
         MVI   MYP-1,C'-'            SPACE 3 LINES BEFORE PRINT                 
*                                                                               
         MVC   MYP+5(4),RCONKADV     ADV CODE                                   
         MVC   MYP+13(20),CONADVN                                               
         BAS   RE,PRINT                                                         
*                                                                               
         EDIT  (1,PG),(3,MYP+76)     PG                                         
         IC    RE,PG                                                            
         LA    RE,1(RE)                                                         
         STC   RE,PG                                                            
*                                                                               
         MVI   MYP-1,X'40'                                                      
         BAS   RE,PRINT                                                         
*                                                                               
         MVI   MYP-1,C'0'            SPACE 2 LINES                              
*                                                                               
* GET PRODUCT NAME                                                              
         MVC   MYP+5(3),RCONPRD      PRODUCT CODE                               
         MVC   MYP+13(20),CONPRD                                                
         CLC   RCONPRD,MYSPACES                                                 
         BE    *+10                                                             
         MVC   MYP+13(20),TWAPRDNM                                              
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MYP+51(7),CONAGY                                                 
         MVI   MYP-1,C'0'                                                       
         BAS   RE,PRINT                                                         
         MVI   MYP-1,X'40'                                                      
*                                                                               
         MVC   MYP+5(3),RCONSAL                                                 
         MVC   MYP+13(20),CONSALN    SALESMAN                                   
         BAS   RE,PRINT                                                         
*                                                                               
         MVI   MYP-1,X'40'                                                      
         MVC   MYP+13(12),TWASALTL   SAL TELEPHONE                              
         BAS   RE,PRINT                                                         
*                                                                               
         MVI   MYP-1,C'-'            SPACE 3 LINES THEN PRINT                   
         MVC   MYP+46(33),TWAAGNM2   AGENCY NAME                                
         BAS   RE,PRINT                                                         
         MVC   MYP+5(2),RCONKOFF     OFFICE                                     
         MVC   MYP+13(20),CONOFFN                                               
         MVC   MYP+46(13),=C'MEDIA BUYER- '                                     
         MVC   MYP+59(20),CONBUY                                                
HL7      BAS   RE,PRINT                                                         
         MVI   MYP-1,X'40'                                                      
         MVC   MYP+46(34),TWAAGAD1   AGY ADDR                                   
*                                                                               
HL9      BAS   RE,PRINT                                                         
         MVI   MYP-1,X'40'                                                      
         MVC   MYP+46(34),TWAAGAD2                                              
         BAS   RE,PRINT                                                         
         OC    TWAAGAD3,MYSPACES                                                
         CLC   TWAAGAD3,MYSPACES                                                
         BE    HL13                                                             
         MVI   MYP-1,X'40'                                                      
         MVC   MYP+46(34),TWAAGAD3                                              
*                                                                               
HL13     EQU   *                                                                
         BAS   RE,PRINT                                                         
         XIT1                                                                   
         TITLE 'T80214-MISCELLANEOUS'                                           
* PRINT ROUTINE                                                                 
PRINT    NTR1                                                                   
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                  
         TM    DMCB+8,X'FF'            ERROR?                                   
         BZ    *+6                                                              
         DC    H'0'                PRINT ERROR                                  
         XC    MYP,MYP                                                          
         IC    RE,LNE                                                           
         LA    RE,1(RE)            BUMP LNE COUNT                               
         CLI   MYP-1,C'0'            2 LINES                                    
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         B     PRTXIT                                                           
         CLI   MYP-1,C'-'            3 LINES                                    
         BNE   PRTXIT                                                           
         LA    RE,2(RE)                                                         
*                                                                               
PRTXIT   STC   RE,LNE                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        LOCAL WORKING STORAGE                                                  
*                                                                               
DOCONVAL DS    F                   CHECK CONTRACT FIELDS?                       
RECCNT   DS    H                   RECORD COUNTER FOR XSORT                     
STALCNT  DS    H                   STATION LNE COUNTER                          
MKTCNT   DS    H                   MARKET  LNE COUNTER                          
COVLIST  DS    0CL24               MAX OF 42 ENTRIES                            
         DS    CL1008              EACH COVER ENTRY IS:                         
*                                    -  4 BYTE DISK ADDRESS                     
*                                    - 20 BYTE MARKET                           
STAREA   DS    0CL43              SORT RECORD 42X43=1806                        
         DS    CL2000                                                           
SVMKT    DS    CL20               SAVE MARKET                                   
SVSTA    DS    CL5                SAVE STATION CALL LETTERS                     
FRSTIME  DS    XL1                FLAG FIRST TIME THROUGH                       
NOINPUT  DS    XL1                FLAG NO INPUT FOR THAT FIELD                  
TOTIND   DS    XL1                STATION OR MARKET TOTAL INDICATOR             
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTEFD                                                       
         EJECT                                                                  
* DSECT TO COVER INVENTORY (TEXT) RECORDS                                       
*                                                                               
RINVD    DSECT                                                                  
       ++INCLUDE REGENINV                                                       
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
         SPACE                                                                  
SRECD    DSECT                                                                  
SLEN     DS    0CL43                                                            
SCON     DS    CL4                                                              
SMKT     DS    CL20                                                             
SSTA     DS    CL5                                                              
SSDATE   DS    CL5                                                              
SEDATE   DS    CL5                                                              
SAMT     DS    XL4                                                              
*                                                                               
         SPACE                                                                  
PLINED   DSECT                                                                  
PCON     DS    CL8                                                              
         DS    CL3                                                              
PSTA     DS    CL5                                                              
         DS    CL3                                                              
PMKT     DS    CL20                                                             
         DS    CL1                                                              
PSDATE   DS    CL5                                                              
         DS    CL2                                                              
PEDATE   DS    CL5                                                              
         DS    CL3                                                              
PAMT     DS    CL17                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007RECNT14   05/01/02'                                      
         END                                                                    
