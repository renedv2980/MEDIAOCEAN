*          DATA SET ACBIL07    AT LEVEL 008 AS OF 02/23/15                      
*PHASE T60E07A                                                                  
         TITLE 'ACBIL07 - CREATIVE BILLING - AUTO BILL'                         
ACBIL07  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,**BIL7**,RA,CLEAR=YES,RR=R2                            
*                                                                               
         USING GWS,R9                                                           
         USING TWAD,R8                                                          
         USING LWSD,RC                                                          
*                                                                               
         ST    R2,MYRELO                                                        
         LA    R1,TABLE                                                         
         ST    R1,ATABLE                                                        
         A     R1,=AL4(L'TABLE)                                                 
         ST    R1,AWKSTBLE                                                      
         A     R1,=AL4(L'WKSTBLE)                                               
         ST    R1,APARLN                                                        
         A     R1,=AL4(MAXPAR*LINELNQ)                                          
         ST    R1,AIOAREA4                                                      
*                                                                               
         LA    R1,MAXPAR                                                        
         STH   R1,MAXPARH                                                       
*                                                                               
*                                  INIT TOT  AND  JOB  COUNTERS                 
         MVC   TOTAMT(TOTAMT#*PLAMTLNQ),ZEROS                                   
         MVC   JOBNET(JOBAMT#*PLAMTLNQ),ZEROS                                   
*                                                                               
         USING ABLD,R5             INIT TABLE     ENTRIES                       
         L     R5,ATABLE                                                        
         LA    R2,MAXCNT           GET  MAX  NUM  OF   ENTRIES                  
*                                                                               
*                                  ZERO PACKED    COUNTERS                      
INIT10   MVC   ABLBUCK(ABLBUCK#*PLAMTLNQ),ZEROS                                 
         LA    R5,ABLNQ(,R5)                                                    
         BCT   R2,INIT10           NEXT ENTRY                                   
         DROP  R5                                                               
*                                                                               
         USING WKSD,R5             INIT WKSTBLE   ENTRIES                       
         L     R5,AWKSTBLE                                                      
         LA    R2,WKSMAX           GET  MAX  NUM  OF   ENTRIES                  
*                                                                               
*                                  ZERO PACKED    COUNTERS                      
INIT20   MVC   WKSNET(WKSN#*PLAMTLNQ),ZEROS                                     
         LA    R5,WKSLNQ(,R5)                                                   
         BCT   R2,INIT20           NEXT ENTRY                                   
         DROP  R5                                                               
*                                                                               
         GOTO1 ABLDCHR             BUILD TABLE OF ALLOCATIONS                   
         CLI   CHRCNT,0            ANYTHING TO BILL ?                           
         BE    NOCHR               NO,  EXIT NOW                                
         CLI   BILFORM,0           HAVE THEY INPUT A NUMBER ?                   
         BNE   *+10                YES, USE IT                                  
         MVC   BILFORM,PFFORMT     ELSE USE PROFILE                             
         CLI   BILFORM,0                                                        
         BNE   *+8                                                              
         MVI   BILFORM,1           DEFAULT IS FORMAT 1                          
         EJECT ,                                                                
***********************************************************************         
* FORMAT 1 LABOR AND COSTS                                            *         
***********************************************************************         
         SPACE 1                                                                
AUTO1    CLI   BILFORM,1                                                        
         BE    AUTO1A                                                           
         CLI   BILFORM,6                                                        
         BNE   AUTO2                                                            
         MVI   RTSUMH,C'Y'                                                      
*                                                                               
AUTO1A   BAS   RE,BLDTAB           BUILD TABLE OF CHARGES                       
         SR    RF,RF               CLEAR RF SO GOAKEY ISN'T SET                 
         BAS   RE,GETRUL           ADD  COMMISSION                              
         BAS   RE,GETRATE          GET  GST/PST                                 
*                                                                               
         BAS   RE,SORTABLE         SORT TABLE SO DIRECT LABOR IS FIRST          
*                                                                               
         BAS   RE,HEADUP           ADD  HEADER RECORD TO WORKER                 
*                                                                               
         BAS   RE,LABOR            ADD  LABOR  PARAGRAPH                        
         BAS   RE,COSTS            ADD  COSTS PARAGRAPH                         
         B     AUTO50                                                           
         EJECT ,                                                                
***********************************************************************         
* FORMAT 2 IS PROFESSIONAL SERVICE FEE - DETAIL                       *         
***********************************************************************         
         SPACE 1                                                                
AUTO2    CLI   BILFORM,2                                                        
         BNE   AUTO3                                                            
*                                                                               
         MVI   HRSUMH,C'Y'         NEED SUMMARY OF HOURS                        
         BAS   RE,BLDTAB           BUILD TABLE OF CHARGES                       
         SR    RF,RF               CLEAR RF SO GOAKEY ISN'T SET                 
         BAS   RE,GETRUL           ADD  COMMISSION                              
         BAS   RE,GETRATE          GET  GST/PST                                 
*                                                                               
         BAS   RE,SORTABLE         SORT TABLE SO DIRECT LABOR IS FIRST          
*                                                                               
         BAS   RE,HEADUP           ADD  HEADER RECORD TO WORKER                 
*                                                                               
         BAS   RE,PROFEE           PROFESSIONAL SERVICE FEE                     
         BAS   RE,COSTS            ADD  COSTS PARAGRAPH                         
         B     AUTO50                                                           
         EJECT ,                                                                
***********************************************************************         
* FORMAT 3 IS PROFESSIONAL SERVICE FEE - ONE LINE                     *         
***********************************************************************         
         SPACE 1                                                                
AUTO3    CLI   BILFORM,3                                                        
         BNE   AUTO4                                                            
*                                                                               
         MVI   HRSUMH,C'Y'         NEED SUMMARY OF HOURS                        
         BAS   RE,BLDTAB           BUILD TABLE OF CHARGES                       
         SR    RF,RF               CLEAR RF SO GOAKEY ISN'T SET                 
         BAS   RE,GETRUL           ADD  COMMISSION                              
         BAS   RE,GETRATE          GET  GST/PST                                 
*                                                                               
         BAS   RE,SORTABLE         SORT TABLE SO DIRECT LABOR IS FIRST          
*                                                                               
         BAS   RE,HEADUP           ADD  HEADER RECORD TO WORKER                 
*                                                                               
         BAS   RE,SUMFEE           SUMMARY PROFESSIONL SERVICE FEE              
         BAS   RE,COSTS            ADD  COSTS PARAGRAPH                         
         B     AUTO50                                                           
         EJECT ,                                                                
***********************************************************************         
* FORMAT 4 LABOR AND DETAIL COSTS                                     *         
***********************************************************************         
         SPACE 1                                                                
AUTO4    CLI   BILFORM,4                                                        
         BE    AUTO4A                                                           
         CLI   BILFORM,5                                                        
         BNE   AUTO9                                                            
         MVI   RTSUMH,C'Y'                                                      
*                                                                               
AUTO4A   BAS   RE,BLDTAB           BUILD TABLE OF CHARGES                       
         SR    RF,RF               CLEAR RF SO GOAKEY ISN'T SET                 
         BAS   RE,GETRUL           ADD  COMMISSION                              
         BAS   RE,GETRATE          GET  GST/PST                                 
*                                                                               
         CLI   TCNT,0                                                           
         BE    NOCHR               NO   BILLABLE CHARGES                        
         BAS   RE,SORTABLE         SORT TABLE SO DIRECT LABOR IS FIRST          
*                                                                               
         BAS   RE,HEADUP           ADD  HEADER RECORD TO WORKER                 
*                                                                               
         BAS   RE,LABOR            ADD  LABOR  PARAGRAPH                        
         CLI   LABRSW,C'N'                                                      
         BE    *+8                 NO   DIRECT LABOR                            
         MVI   NEXTPARA,1          SEED THE PARAGRAPH NUMBER                    
*                                                                               
         MVI   DTLINEH,C'Y'        HOOK FOR DETAIL LINE                         
         MVI   WKLINEH,C'Y'        HOOK FOR WORK CODE LINE                      
         MVI   HRSUMH,C'X'         EXCLUDE HOURS                                
*                                                                               
         L     R2,APARLN                                                        
         ST    R2,NEXTP                                                         
         MVI   TCNT,0                                                           
         BAS   RE,BLDTAB           LOOK FOR ALLOCATED AMOUNTS                   
*                                  HOOKS WILL HANDLE PRINTING                   
         CLI   NEXTPARA,0                                                       
         BE    NOCHR               NOTHING ADDED TO FILE                        
         B     AUTO50                                                           
         EJECT ,                                                                
***********************************************************************         
* FORMAT 3 LIKE A PROGRESSIVE BILL                                    *         
***********************************************************************         
         SPACE 1                                                                
AUTO9    CLI   BILFORM,9                                                        
         BNE   BADFORM             UNKNOWN TYPE                                 
*                                                                               
         MVC   PARDSC,SPACES                                                    
         MVI   PARCNT,0                                                         
         L     R2,APARLN                                                        
         ST    R2,NEXTP            SAVE ADDRESS OF NEXT PARLN                   
*                                                                               
         MVI   DTLINEH,C'Y'        HOOK FOR DETAIL LINE                         
         MVI   WKLINEH,C'Y'        HOOK FOR WORK CODE LINE                      
*                                                                               
         MVI   TCNT,0                                                           
         BAS   RE,BLDTAB           LOOK FOR ALLOCATED AMOUNTS                   
*                                  HOOKS WILL HANDLE PRINTING                   
         CLI   NEXTPARA,0                                                       
         BE    NOCHR               NOTHING ADDED TO FILE                        
         B     AUTO50              OK   TO CLOSE AND GO AWAY                    
         EJECT ,                                                                
         SPACE 1                                                                
AUTO50   GOTO1 AWRKCLO,AIOAREA1    CLOSE THE FILE                               
         BNE   WRKERR                                                           
*                                                                               
         GOTO1 =A(DSPL),DMCB,(RC),RR=MYRELO   DISPLAY PARAGRAPH                 
*                                                                               
         CLI   BYTE,C'W'           DSPL PASSES ERRORS BACK IN BYTE              
         BE    WRKERR                                                           
         CLI   BYTE,C'C'                                                        
         BE    NOCHR                                                            
*                                                                               
         MVI   FERN,OK             ALL  DONE                                    
         B     EXIT                RETURN TO CALLER                             
*                                                                               
OKXIT    SR    RB,RB               CC = EQU                                     
*                                                                               
ERRXIT   LTR   RB,RB               CC = NEQ                                     
*                                                                               
EXIT     XIT1  ,                   RETURN TO CALLER                             
         EJECT ,                                                                
***********************************************************************         
* SORT TABLE SO DIRECT LABOR IS FIRST                                 *         
***********************************************************************         
         SPACE 1                                                                
SORTABLE ST    RE,SAVRE            SAVE    RE                                   
         CLI   TCNT,1                                                           
         BE    SORTBLEX            ONLY    ON ENTRY                             
         L     RF,ATABLE           TABLE                                        
         ZIC   R2,TCNT             NUMBER  OF ENTRIES                           
         LA    R3,ABLNQ            LENGTH  OF ENTRY                             
         LA    R4,ABLNQ            LENGTH  OF SORT KEY                          
         SR    R5,R5               DISP.   TO KEY                               
         GOTO1 VXSORT,DMCB,(0,(RF)),(R2),(R3),(R4),(R5)                         
*                                                                               
SORTBLEX L     RE,SAVRE            RESTORE RE                                   
         BSM   0,RE                RETURN                                       
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO BUILD TABLE OF ALLOCATED CHARGES                         *         
***********************************************************************         
         SPACE 1                                                                
BLDTAB   NTR1                                                                   
         BAS   RE,CLEARHED         CLEAR ANY OLD HEADLINES                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),JOBKEY      READ JOB RECORD                              
         SR    R0,R0               COUNT ENTRIES                                
         GOTO1 AREAD,AIOAREA3                                                   
         BE    *+6                                                              
         DC    H'0'                CAN  NOT READ JOB RECORD                     
*                                                                               
BLDTAB2  GOTO1 ASEQ,AIOAREA3                                                    
*                                                                               
         USING TRNRECD,R2          MAP  TRANSACTION RECORD                      
BLDTAB2A L     R2,AIOAREA3                                                      
         CLC   JOBKEY,TRNKCULA                                                  
         BNE   BLDTAB23            END  OF JOB RECORDS                          
         LR    R4,R2                                                            
         AH    R4,DATADISP                                                      
         CLI   0(R4),CACELQ        X'43' - CONTRA-ACCOUNT HEADER EL ?           
         BNE   BLDTAB3                                                          
         MVC   CONTRA,SPACES                                                    
*                                                                               
         USING CACELD,R4                                                        
         ZIC   R1,CACLN            SAVE CONTRA ACCOUNT NAME                     
         SH    R1,=AL2(CACLN1Q)    LENGTH = 0 ?                                 
         BZ    BLDTAB2                                                          
         BCTR  R1,0                NO,  SUBTRACT ON FOR EXECUTE                 
         EXMVC R1,CONTRA,CACNAME                                                
         B     BLDTAB2                                                          
*                                                                               
         USING TRNELD,R4           MAP  TRANSACTION ELEMENT                     
BLDTAB3  LR    R4,R2                                                            
         AH    R4,DATADISP                                                      
         CLI   0(R4),TRNELQ        X'44' - TRANSACTION ELEMENT ?                
         BNE   BLDTAB2             ONLY WANT TRANSACTIONS                       
         CLC   TRNANAL,=C'99'      ELIMINATE BILLING                            
         BE    BLDTAB2                                                          
         CLC   TRNANAL,=C'**'      AND  OTHER FUNNIES                           
         BE    BLDTAB2                                                          
         CLC   TRNANAL,SPACES                                                   
         BE    BLDTAB2                                                          
         TM    TRNSTAT,TRNSDR      DEBIT ?                                      
         BZ    BLDTAB2             ONLY DEBITS                                  
*                                                                               
         TM    TRNRSTAT,TRNSDRFT                                                
         BO    BLDTAB2             NO   DRAFTS                                  
         TM    TRNRSTAT,TRNSREVS                                                
         BO    BLDTAB2             NO   REVERSALS                               
*                                                                               
         CLI   WKLINEH,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,WKLINE           WORK CODE TOTAL IF REQUIRED                  
*                                                                               
         MVC   BYTE,TRNSTAT                                                     
*                                                                               
         USING ABLD,R5                                                          
         LA    R5,ABLWRK                                                        
         XC    ABLWRK,ABLWRK                                                    
*                                  ZERO PACKED    COUNTERS                      
         MVC   ABLBUCK(ABLBUCK#*PLAMTLNQ),ZEROS                                 
         MVC   ABLCNTR,CONTRA                                                   
*        OC    ABLCNTR,SPACES                                                   
*                                                                               
BLDTAB4  ZIC   R1,1(,R4)           NOW  GET ALLOCATED AMOUNT                    
         AR    R4,R1                                                            
         CLI   0(R4),PRTELQ        X'40' - PERSONAL RATE ELEMENT ?              
         BE    BLDTAB5             NOTE: NO WAY THAT WE CAN EVER GET =!         
         CLI   0(R4),0                                                          
         BE    BLDTAB7             END  OF RECORD GET NEXT                      
         B     BLDTAB4                                                          
*                                                                               
         USING PRTELD,R4           MAP  PERSONAL RATE ELEMENT                   
BLDTAB5  ZAP   DUB,PRTRATE         HOURS                                        
         CVB   R1,DUB                                                           
         STCM  R1,15,ABLRTE                                                     
         B     BLDTAB4                                                          
*                                                                               
BLDTAB7  GOTO1 VPRORATA,DMCB,(X'C0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
*                                                                               
         USING PRORATAD,R4                                                      
         L     R4,APROBLK                                                       
*                                                                               
         LA    R1,ABLNET           COMMISSIONABLE                               
         TM    BYTE,X'01'                                                       
         BZ    *+8                                                              
         LA    R1,ABLNON           OR   NON                                     
*                                                                               
         ZAP   DUB,PP$AALLO        AMOUNT ALLOCATED TO BILL                     
         AP    DUB,PP$ADSCB        DISCOUNT ALLOCATED TO BILL                   
         ZAP   0(PLAMTLNQ,R1),DUB                                               
*                                                                               
         ZAP   ABLCSD,PP$ADSCB     DISCOUNT ALLOCATED TO BILL                   
*                                                                               
         LA    R1,ABLHRS                                                        
         ZAP   DUB,PP$HRSB         HOURS ALLOCATED TO BILL                      
         CVB   RF,DUB                                                           
         STCM  RF,15,0(R1)                                                      
         DROP  R4                                                               
*                                                                               
BLDTAB9  CP    ABLNET,=P'0'                                                     
         BNE   BLDTAB10                                                         
         CP    ABLNON,=P'0'                                                     
         BNE   BLDTAB10                                                         
         OC    ABLHRS,ABLHRS                                                    
         BNZ   BLDTAB10                                                         
         B     BLDTAB2             NOTHING GOOD TO ADD                          
*                                                                               
BLDTAB10 MVC   ABLCDE,TRNKWORK                                                  
         MVI   ABLTYP,1            ASSUME DIRECT COSTS                          
         OC    ABLHRS,ABLHRS                                                    
         BZ    BLDTAB12            NO   HOURS                                   
         MVI   ABLTYP,0            IF   HOURS, IT'S DIRECT LABOR                
         CLI   HRSUMH,C'X'         EXCLUDE HOURS                                
         BE    BLDTAB2                                                          
*                                                                               
*              NOW TRY TO FIND A MATCH IN TABLE                                 
BLDTAB12 ZIC   R0,TCNT             NUMBER SO FAR                                
         L     R6,ATABLE                                                        
*                                                                               
BLDTAB13 LTR   R0,R0                                                            
         BZ    BLDTAB18            NO   MATCH ADD NEW ENTRY                     
         CLC   ABLCDE,ABLCDE-ABLD(R6)                                           
         BNE   BLDTAB14            NOT  SAME WORK CODE                          
         CLC   ABLTYP,ABLTYP-ABLD(R6)                                           
         BNE   BLDTAB14            NOT  SAME STATUS                             
         CLC   ABLRTE,ABLRTE-ABLD(R6)                                           
         BNE   BLDTAB14            NOT  SAME RATE                               
         B     BLDTAB15                                                         
*                                                                               
BLDTAB14 LA    R6,ABLNQ(,R6)       BUMP TO NEXT ENTRY                           
         BCTR  R0,0                SUBTRACT 1 FROM COUNT                        
         B     BLDTAB13                                                         
*                                                                               
*                                  ADD  THIS ENTRY TO TABLE ENTRY               
BLDTAB15 AP    ABLNET-ABLD(PLAMTLNQ,R6),ABLNET                                  
         AP    ABLNON-ABLD(PLAMTLNQ,R6),ABLNON                                  
         AP    ABLCSD-ABLD(PLAMTLNQ,R6),ABLCSD                                  
*                                                                               
         L     RE,ABLHRS                                                        
         A     RE,ABLHRS-ABLD(,R6)                                              
         ST    RE,ABLHRS-ABLD(,R6)                                              
         B     BLDTAB19                                                         
*                                                                               
BLDTAB18 CLI   TCNT,MAXCNT                                                      
         BE    TABFULL             TABLE IS FULL                                
         MVC   0(ABLNQ,R6),ABLWRK  NEW  ENTRY TO TABLE                          
         ZIC   R0,TCNT                                                          
         AH    R0,=H'1'                                                         
         STC   R0,TCNT                                                          
*                                                                               
BLDTAB19 CLI   ABLTYP,0            IS   IT DIRECT LABOR ?                       
         BNE   BLDTAB21            NO,  NO NEED TO CHECK TABLE                  
         CLI   HRSUMH,C'Y'         DO   WE NEED HOUR SUMMARY ?                  
         BNE   *+8                                                              
         BAS   RE,HRSUM            ADD  TO HOUR SUMMARY                         
         CLI   RTSUMH,C'Y'         SUMMARIZE BY RATE ?                          
         BNE   *+8                 NO                                           
         BAS   RE,RTSUM                                                         
*                                                                               
BLDTAB21 CLI   DTLINEH,C'Y'                                                     
         BNE   BLDTAB2                                                          
         BAS   RE,DTLINE           1ST  DETAIL LINE                             
         B     BLDTAB2                                                          
*                                                                               
BLDTAB23 CLI   WKLINEH,C'Y'                                                     
         BNE   BLDTAB25                                                         
         BAS   RE,WKLINE           WORK CODE TOTAL                              
         B     BLDTABEX            RETURN                                       
*                                                                               
BLDTAB25 CLI   TCNT,0              NUMBER OF ENTRIES                            
         BE    NOCHR                                                            
*                                                                               
BLDTABEX B     EXIT                RETURN TO CALLER                             
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO ADD LABOR COSTS                                          *         
***********************************************************************         
         SPACE 1                                                                
LABOR    NTR1                                                                   
         BAS   RE,CLEARHED         CLEAR     ANY  OLD  HEADLINES                
*                                  INITIALIZE     PARAGRAPH HEADERS             
         LH    R1,=H'6000'         .    PARDSC=DIRECT LABOR                     
         BAS   RE,INITPARH                                                      
         MVI   PARCNT,0                                                         
         MVI   WKSCNT,0                                                         
*                                                                               
         USING LINED,R2            MAP  LINE FORMAT                             
         USING HOURSUM,R4          MAP  HOUR SUMMARY   TABLE                    
         L     R2,APARLN                                                        
         L     R4,AIOAREA4                                                      
         MVI   LABRSW,C'N'                                                      
         CLI   BILFORM,5                                                        
         BE    LABR20              SPECIAL   ROUTINE   FOR  FORMAT    5         
         CLI   BILFORM,6                                                        
         BE    LABR20              SAME      ROUTINE   FOR  FORMAT    6         
*                                                                               
         L     R5,ATABLE           SET  UP   FOR  LOOP THRU TABLE               
         USING ABLD,R5                                                          
         ZIC   R3,TCNT                                                          
*                                                                               
LABR3    CLI   ABLTYP,0            IS   IT   DIRECT    LABOR ?                  
         BE    LABR5               YES, GO   PROCESS                            
*                                                                               
LABR4    LA    R5,ABLNQ(,R5)       ELSE GO   TO   NEXT                          
         BCT   R3,LABR3                                                         
*                                                                               
LABR4E   CLI   PARCNT,0            DIRECT    LABOR ?                            
         BE    LABREX              NO,  RETURN                                  
         MVC   LINSE1,=C'S=1'                                                   
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
*                                  SUBTOTAL  DIRECT    LABOR                    
         GOTO1 ATXTGET,DMCB,6001,(L'LINPHDR,LINPHDR),0,(PLANG,0)                
         ZAP   DUB,TOTAMT                                                       
         AP    DUB,TOTNON                                                       
         CLI   PFCOMM,C'W'                                                      
         BNE   *+10                                                             
         AP    DUB,TOTCOMM         ADD  COMMISSION     TO   W/C  LINE           
*                                                                               
         CLI   PFDOL,C'Y'          SUPPRESS  THE  DOLLAR    SIGN ?              
         BE    LABR5A              YES                                          
         EXTED (P8,DUB),(L'LINAMT,LINAMT),2,MINUS=YES,CURSYMB=S,       X        
               LANG=PLANG                                                       
         B     LABR5B                                                           
*                                                                               
LABR5A   EXTED (P8,DUB),(L'LINAMT,LINAMT),2,MINUS=YES,LANG=PLANG                
*                                                                               
LABR5B   ZIC   RF,PARCNT                                                        
         LA    RF,2(,RF)                                                        
         CH    RF,MAXPARH                                                       
         BH    TOOPAR              TOO  MANY LINES     ON   PARAGRAPH           
         STC   RF,PARCNT                                                        
         BAS   RE,PUTPARA          OUTPUT    THE  PARAGRAPH                     
         MVI   LABRSW,C'Y'                                                      
         B     LABREX              RETURN                                       
*                                                                               
LABR5    OC    ABLHRS,ABLHRS                                                    
         BZ    LABR4               HOURS     NETTED    TO   ZERO                
         ZIC   RF,PARCNT                                                        
         LA    RF,1(,RF)                                                        
         CH    RF,MAXPARH                                                       
         BH    TOOPAR              TOO  MANY LINES     ON   PARAGRAPH           
         STC   RF,PARCNT                                                        
         CLI   PARCNT,1                                                         
         BNE   LABR8                                                            
         MVI   PARCNT,3            FIRST     TIME DO   MIDLINES                 
         MVCDD LINRATE1,AC#RATES,R RATES                                        
         LA    R1,LINRATE1                                                      
         GOTO1 AGETPFLD                                                         
*                                  HOURS                                        
         MVCDD LINHRS1(L'LINHRS1-1),AC#HOURS,R                                  
         LA    R1,LINHRS1                                                       
         GOTO1 AGETPFLD                                                         
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
*                                  RATES     UNDERLINED                         
         MVCDD LINRATE1,AC#RATES,RU                                             
         LA    R1,LINRATE1                                                      
         GOTO1 AGETPFLD                                                         
*                                  HOURS     UNDERLINED                         
         MVCDD LINHRS1(L'LINHRS1-1),AC#HOURS,RU                                 
         LA    R1,LINHRS1                                                       
         GOTO1 AGETPFLD                                                         
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
*                                                                               
LABR8    GOTO1 AGETWC,ABLCDE                                                    
         MVI   LINBLANK,C' '                                                    
         MVC   LINWCDSC,WORK       WORK CODE DESCRIPTION                        
*                                  RATE AND  HOURS                              
         EXTED ABLHRS,LINHRS1,2,MINUS=YES,LANG=PLANG                            
         ZAP   DUB,ABLNET                                                       
         AP    DUB,ABLNON                                                       
         CLI   PFCOMM,C'W'                                                      
         BNE   *+10                                                             
         AP    DUB,ABLCOM          OPTION    TO   ADD  COMMISSION               
         ZAP   PK2D(16),DUB        DISPLAY   AMOUNT    COLUMN                   
*                                                                               
         CLI   PFDOL,C'Y'          SUPPRESS  THE  DOLLAR    SIGN ?              
         BE    LABR8A              YES                                          
         EXTED (P8,DUB),LINAMT,2,MINUS=YES,CURSYMB=S,LANG=PLANG                 
         B     LABR8B                                                           
*                                                                               
LABR8A   EXTED (P8,DUB),LINAMT,2,MINUS=YES,LANG=PLANG                           
*                                                                               
LABR8B   MP    PK2D(16),=P'1000'                                                
         L     RE,ABLHRS                                                        
         CVD   RE,DUB                                                           
         DP    PK2D(16),DUB+4(4)   COST/HOURS                                   
         SRP   PK2D(12),64-1,5                                                  
         ZAP   DUB,PK2D(12)                                                     
*                                                                               
         CLI   PFDOL,C'Y'          SUPPRESS  THE  DOLLAR    SIGN ?              
         BE    LABR9               YES                                          
         EXTED (P8,DUB),LINRATE1,2,CURSYMB=S,LANG=PLANG                         
         B     LABR9A                                                           
*                                                                               
LABR9    EXTED (P8,DUB),LINRATE1,2,LANG=PLANG                                   
*                                                                               
*                                  ADD  TO   PARAGRAPH TOTALS                   
LABR9A   BAS   RE,ADDUP                                                         
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
         B     LABR4                                                            
         EJECT ,                                                                
         SPACE 1                                                                
*                                  SPECIAL   CODE FOR  FORMAT    5 & 6          
LABR20   CLI   RTCNT,0                                                          
         BE    LABREX              RETURN                                       
*                                                                               
         ZIC   R0,RTCNT            NUMBER    OF   ENTRIES                       
         SR    R3,R3                                                            
*                                                                               
LABR22   CP    HRSHOURS,=P'0'      NO   HOURS,    SKIP                          
         BE    LABR24E                                                          
         LA    R3,1(,R3)           BUMP NUMBER    OF   LINES                    
         CH    R3,=H'1'            1ST  LINE ?                                  
         BNE   LABR24              NOT  1ST  ENTRY,    SKIP                     
         LA    R3,3                                                             
*                                  HOURS                                        
         MVCDD LINHRS2(L'LINHRS2-1),AC#HOURS,R                                  
         LA    R1,LINHRS2                                                       
         GOTO1 AGETPFLD                                                         
         MVCDD LINRATE2,AC#RATES,R RATES                                        
         LA    R1,LINRATE2                                                      
         GOTO1 AGETPFLD                                                         
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
*                                  HOURS     UNDERLINED                         
         MVCDD LINHRS2(L'LINHRS2-1),AC#HOURS,RU                                 
         LA    R1,LINHRS2                                                       
         GOTO1 AGETPFLD                                                         
*                                  RATES     UNDERLINED                         
         MVCDD LINRATE2,AC#RATES,RU                                             
         LA    R1,LINRATE2                                                      
         GOTO1 AGETPFLD                                                         
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
*                                                                               
LABR24   MVI   LINBLANK,C' '                                                    
         MVC   LINCNTRA,HRSCNTR    CONTRA    NAME      TO   PRINT LINE          
*                                  HOURS               TO   PRINT LINE          
         EXTED HRSHOURS,LINHRS2,2,MINUS=YES,LANG=PLANG                          
*                                                                               
         ZAP   DUB,HRSNET          NET                                          
         AP    DUB,HRSNON          PLUS NON-COMMISSION                          
         CLI   PFCOMM,C'W'                                                      
         BNE   *+10                                                             
         AP    DUB,HRSCOM          OPTION    TO   ADD  COMMISSION               
         ZAP   PK2D(16),DUB        DISPLAY   AMOUNT    COLUMN                   
*                                                                               
         CLI   PFDOL,C'Y'          SUPPRESS  THE  DOLLAR    SIGN ?              
         BE    LABR24A             YES                                          
         EXTED (P8,DUB),LINAMT,2,MINUS=YES,CURSYMB=S,LANG=PLANG                 
         B     LABR24B                                                          
*                                                                               
LABR24A  EXTED (P8,DUB),LINAMT,2,MINUS=YES,LANG=PLANG                           
*                                                                               
LABR24B  MP    PK2D(16),=P'1000'                                                
         ZAP   DUB,HRSHOURS                                                     
         DP    PK2D(16),DUB+4(4)   COST/HOURS                                   
         SRP   PK2D(12),64-1,5                                                  
         ZAP   DUB,PK2D(12)                                                     
*                                                                               
         CLI   PFDOL,C'Y'          SUPPRESS  THE  DOLLAR    SIGN ?              
         BE    LABR24C             YES                                          
         EXTED (P8,DUB),LINRATE2,2,CURSYMB=S,LANG=PLANG                         
         B     LABR24D                                                          
*                                                                               
LABR24C  EXTED (P8,DUB),LINRATE2,2,LANG=PLANG                                   
*                                                                               
LABR24D  LA    R2,LINELNQ(,R2)     NEXT LINE                                    
*                                                                               
LABR24E  LA    R4,HRSLNQ(,R4)                                                   
         BCT   R0,LABR22                                                        
         DROP  R4                                                               
*                                                                               
         MVC   LINHRS2+2(L'LINHRS2-3),=C'------'                                
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
         LA    R3,1(,R3)           KEEP NUMBER    OF   LINES                    
         STC   R3,PARCNT                                                        
*                                                                               
         USING ABLD,R5                                                          
         L     R5,ATABLE           SET  UP   FOR  LOOP THRU TABLE               
         ZIC   R3,TCNT             NUMBER    IN   TABLE                         
         SR    RF,RF               TOTAL     HOURS                              
*                                                                               
LABR26   CLI   ABLTYP,0            IS   IT   DIRECT    LABOR ?                  
         BNE   LABR28              YES, GO   PROCESS                            
         A     RF,ABLHRS           COUNT     NUMBER    OF   HOURS               
         BAS   RE,ADDUP            ADD  TO   TOTALS                             
*                                                                               
LABR28   LA    R5,ABLNQ(,R5)       ELSE GO   TO   NEXT                          
         BCT   R3,LABR26                                                        
         ST    RF,SAVEHRS                                                       
         DROP  R5                                                               
*                                                                               
         MVC   LINSE1,=C'S=1'                                                   
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
         MVCDD LINTOTC1,AC#TOTAL,R TOTAL                                        
         LA    R1,LINTOTC1                                                      
         GOTO1 AGETPFLD                                                         
         EXTED SAVEHRS,LINHRS2,2,MINUS=YES,LANG=PLANG                           
         ZAP   DUB,TOTAMT                                                       
         AP    DUB,TOTNON                                                       
         CLI   PFCOMM,C'W'                                                      
         BNE   *+10                                                             
         AP    DUB,TOTCOMM         ADD  COMMISSION     TO   W/C  LINE           
*                                                                               
         CLI   PFDOL,C'Y'          SUPPRESS  THE  DOLLAR    SIGN ?              
         BE    LABR30              YES                                          
         EXTED (P8,DUB),LINAMT,2,MINUS=YES,CURSYMB=S,LANG=PLANG                 
         B     LABR32                                                           
*                                                                               
LABR30   EXTED (P8,DUB),LINAMT,2,MINUS=YES,LANG=PLANG                           
*                                                                               
LABR32   ZIC   RF,PARCNT                                                        
         LA    RF,2(,RF)                                                        
         CH    RF,MAXPARH                                                       
         BH    TOOPAR                                                           
         STC   RF,PARCNT                                                        
         BAS   RE,PUTPARA          OUTPUT    THE  PARAGRAPH                     
         MVI   LABRSW,C'Y'                                                      
*                                                                               
LABREX   B     EXIT                RETURN    TO   CALLER                        
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO ADD DIRECT COSTS                                         *         
***********************************************************************         
         SPACE 1                                                                
COSTS    NTR1                                                                   
         BAS   RE,CLEARHED         CLEAR ANY OLD HEADLINES                      
*                                  INITIALIZE PARAGRAPH HEADERS                 
         LH    R1,=H'6004'         .    PARDSC=DIRECT COSTS                     
         BAS   RE,INITPARH                                                      
         MVI   PARCNT,0                                                         
         MVI   WKSCNT,0                                                         
*                                                                               
         USING LINED,R2            MAP  LINE FORMAT                             
         USING ABLD,R5                                                          
         L     R2,APARLN                                                        
         L     R5,ATABLE           SET  UP FOR LOOP THRU TABLE                  
         ZIC   R3,TCNT                                                          
*                                                                               
COST3    CLI   ABLTYP,1            IS   IT DIRECT COST ?                        
         BE    COST5               YES, GO PROCESS                              
*                                                                               
COST4    LA    R5,ABLNQ(,R5)       ELSE GO TO NEXT                              
         BCT   R3,COST3                                                         
         CLI   PARCNT,0            ANY  DIRECT COSTS ?                          
         BE    COSTEX              NO,  RETURN                                  
         MVC   LINSE1,=C'S=1'                                                   
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
*                                  SUBTOTAL DIRECT COSTS                        
         GOTO1 ATXTGET,DMCB,6005,(L'LINPHDR,LINPHDR),0,(PLANG,0)                
         ZAP   DUB,TOTAMT                                                       
         AP    DUB,TOTNON                                                       
         CLI   PFCOMM,C'W'                                                      
         BNE   *+10                                                             
         AP    DUB,TOTCOMM         ADD  COMMISSION TO W/C LINE                  
*                                                                               
         CLI   PFDOL,C'Y'          SUPPRESS THE DOLLAR SIGN ?                   
         BE    COST4A              YES                                          
         EXTED (P8,DUB),LINAMT,2,MINUS=YES,CURSYMB=S,LANG=PLANG                 
         B     COST4B                                                           
*                                                                               
COST4A   EXTED (P8,DUB),LINAMT,2,MINUS=YES,LANG=PLANG                           
*                                                                               
COST4B   ZIC   RF,PARCNT                                                        
         LA    RF,2(,RF)                                                        
         CH    RF,MAXPARH                                                       
         BH    TOOPAR                                                           
         STC   RF,PARCNT                                                        
         BAS   RE,PUTPARA          OUTPUT THE PARAGRAPH                         
         B     COSTEX              RETURN                                       
*                                                                               
COST5    ZIC   RF,PARCNT           COUNT NUMBER OF PARAGRAPHS                   
         LA    RF,1(,RF)                                                        
         CH    RF,MAXPARH                                                       
         BH    TOOPAR                                                           
         STC   RF,PARCNT                                                        
*                                                                               
COST8    GOTO1 AGETWC,ABLCDE                                                    
         MVI   LINBLANK,C' '                                                    
         MVC   LINWCDSC,WORK       WORK CODE DESCRIPTION                        
         ZAP   DUB,ABLNET                                                       
         AP    DUB,ABLNON                                                       
         CLI   PFCOMM,C'W'                                                      
         BNE   *+10                                                             
         AP    DUB,ABLCOM          ADD  COMMISSION TO W/C LINE                  
*                                                                               
         CLI   PFDOL,C'Y'          SUPPRESS THE DOLLAR SIGN ?                   
         BE    COST8A              YES                                          
         EXTED (P8,DUB),LINAMT,2,MINUS=YES,CURSYMB=S,LANG=PLANG                 
         B     COST8B                                                           
*                                                                               
COST8A   EXTED (P8,DUB),LINAMT,2,MINUS=YES,LANG=PLANG                           
*                                                                               
*                                  ADD  TO PARAGRAPH TOTALS                     
COST8B   BAS   RE,ADDUP                                                         
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
         B     COST4                                                            
*                                                                               
COSTEX   B     EXIT                RETURN TO CALLER                             
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO ADD ITEMS TO HOURS SUMMARY                               *         
*   25 BYTES NAME / 5 BYTES HOURS                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING ABLD,R5                                                          
         SPACE 1                                                                
HRSUM    NTR1                                                                   
         LA    R5,ABLWRK                                                        
         L     R1,ABLHRS           GET  CURRENT ENTRY HOURS                     
         CVD   R1,DUB                                                           
*                                                                               
         USING HOURSUM,R4          MAP  HOUR SUMMARY   TABLE                    
         L     R4,AIOAREA4         KEEP TABLE IN IOAREA4                        
         ZIC   R0,HRCNT            COUNT NUMBER OF ENTRIES                      
*                                                                               
HRSUM2   LTR   R0,R0                                                            
         BZ    HRSUM9              END  OF TABLE                                
         CLC   HRSCNTR,CONTRA                                                   
         BNE   HRSUM5              NOT  SAME PERSON                             
         AP    HRSHOURS,DUB        ADD  HOURS TO TABLE AMOUNT                   
         B     HRSUMEX             RETURN                                       
*                                                                               
HRSUM5   LA    R4,HRSLNQ(,R4)      BUMP TO NEXT ENTRY                           
         BCTR  R0,0                                                             
         B     HRSUM2                                                           
*                                                                               
HRSUM9   MVC   HRSCNTR,CONTRA      NEW  ENTRY - ADD NAME                        
         ZAP   HRSHOURS,DUB        ADD  HOURS AMOUNT                            
         ZAP   HRSRATE,=P'0'       CLEAR RATE                                   
         ZAP   HRSNET,=P'0'        CLEAR NET                                    
         ZAP   HRSNON,=P'0'        CLEAR NON-COMMISSION                         
         ZAP   HRSCOM,=P'0'        CLEAR COMMISSION                             
         ZIC   RF,HRCNT            UPDATE ITEM COUNT                            
         LA    RF,1(,RF)                                                        
         STC   RF,HRCNT                                                         
*                                                                               
HRSUMEX  B     EXIT                RETURN TO CALLER                             
         DROP  R4,R5                                                            
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO ADD ITEMS TO HOURS SUMMARY BY RATE AND CONTRA NAME       *         
***********************************************************************         
         SPACE 1                                                                
         USING ABLD,R5                                                          
         SPACE 1                                                                
RTSUM    NTR1                                                                   
         LA    R5,ABLWRK                                                        
         L     R1,ABLRTE           PACK ALL THE FIELDS WE'LL NEED               
         CVD   R1,DUB                                                           
*                                                                               
         L     R1,ABLHRS                                                        
         CVD   R1,DUB2                                                          
*                                                                               
         ZAP   PACK1,ABLNET                                                     
         ZAP   PACK2,ABLNON                                                     
         ZAP   PACK3,ABLCOM                                                     
*                                                                               
         USING HOURSUM,R4          MAP  HOUR SUMMARY   TABLE                    
         L     R4,AIOAREA4         KEEP TABLE IN IOAREA4                        
         ZIC   R0,RTCNT            COUNT NUMBER OF ENTRIES                      
*                                                                               
RTSUM2   LTR   R0,R0               END  OF TABLE ?                              
         BZ    RTSUM9              YES                                          
         CLC   HRSCNTR,ABLCNTR     SAME PERSON ?                                
         BNE   RTSUM5              NO                                           
         CP    HRSRATE,DUB         YES, IS RATE THE SAME ?                      
         BNE   RTSUM5              NO                                           
         AP    HRSHOURS,DUB2       YES, ADD HOURS                               
         AP    HRSNET,PACK1        NET                                          
         AP    HRSNON,PACK2        NON-COMMISSIONABLE                           
         AP    HRSCOM,PACK3        COMMISSION                                   
         B     RTSUMEX             RETURN                                       
*                                                                               
RTSUM5   LA    R4,HRSLNQ(,R4)      BUMP TO NEXT ENTRY                           
         BCTR  R0,0                                                             
         B     RTSUM2                                                           
*                                                                               
RTSUM9   MVC   HRSCNTR,ABLCNTR     NEW  ENTRY - ADD NAME                        
         ZAP   HRSRATE,DUB         RATE                                         
         ZAP   HRSHOURS,DUB2       HOURS                                        
         ZAP   HRSNET,PACK1        NET                                          
         ZAP   HRSNON,PACK2        NON                                          
         ZAP   HRSCOM,PACK3        COM                                          
         ZIC   RF,RTCNT            UPDATE ITEM COUNT                            
         LA    RF,1(,RF)                                                        
         STC   RF,RTCNT                                                         
*                                                                               
RTSUMEX  B     EXIT                RETURN TO CALLER                             
         DROP  R4,R5                                                            
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO PRINT PROFESSIONAL SERVICE FEE                           *         
***********************************************************************         
         SPACE 1                                                                
PROFEE   NTR1                                                                   
         CLI   HRCNT,0             ANY  HOURS ?                                 
         BE    PROFEX              NO,  RETURN                                  
         BAS   RE,CLEARHED         CLEAR     ANY  OLD  HEADLINES                
*                                  INITIALIZE     PARAGRAPH HEADERS             
         LH    R1,=H'6006'         .    PARDSC=PROFESSIONAL SERVICES            
         BAS   RE,INITPARH                                                      
         MVI   PARCNT,0                                                         
         MVI   WKSCNT,0                                                         
*                                                                               
*              SET-UP DETAIL LINES                                              
*                                                                               
         USING LINED,R2            MAP  LINE FORMAT                             
         USING HOURSUM,R4          MAP  HOUR SUMMARY   TABLE                    
         L     R2,APARLN                                                        
         L     R4,AIOAREA4         HOUR SUMMARY   TABLE                         
         ZIC   R0,HRCNT            NUMBER    OF   ENTRIES                       
         SR    R3,R3                                                            
*                                                                               
PROF3    LA    R3,1(,R3)           1ST  LINE ?                                  
         CH    R3,=H'1'            BUMP NUMBER    OF   LINES                    
         BNE   PROF4               NOT  1ST  LINE SKIP                          
         LA    R3,3                                                             
*                                  HOURS                                        
         MVCDD LINHRS2(L'LINHRS2-1),AC#HOURS,R                                  
         LA    R1,LINHRS2                                                       
         GOTO1 AGETPFLD                                                         
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
*                                  HOURS     UNDERLINED                         
         MVCDD LINHRS2(L'LINHRS2-1),AC#HOURS,RU                                 
         LA    R1,LINHRS2                                                       
         GOTO1 AGETPFLD                                                         
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
*                                                                               
PROF4    MVI   LINBLANK,C' '                                                    
         MVC   LINCNTRA,HRSCNTR    NAME      TO   PRINT LINE                    
*                                  HOURS     TO   PRINT LINE                    
         EXTED HRSHOURS,LINHRS2,2,MINUS=YES,LANG=PLANG                          
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
         LA    R4,HRSLNQ(,R4)                                                   
         BCT   R0,PROF3                                                         
         DROP  R4                                                               
*                                                                               
         MVC   LINHRS2+2(L'LINHRS2-3),=C'------'                                
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
         LA    R3,1(,R3)           KEEP NUMBER    OF   LINES                    
         STC   R3,PARCNT                                                        
*                                                                               
         USING ABLD,R5                                                          
         L     R5,ATABLE           SET  UP   FOR  LOOP THRU TABLE               
         ZIC   R3,TCNT             NUMBER    IN   TABLE                         
         SR    RF,RF               TOTAL     HOURS                              
*                                                                               
PROF5    CLI   ABLTYP,0            IS   IT   DIRECT    LABOR ?                  
         BNE   PROF9               YES, GO   PROCESS                            
*                                                                               
         A     RF,ABLHRS           COUNT     NUMBER    OF   HOURS               
         BAS   RE,ADDUP            ADD  TO   TOTALS                             
*                                                                               
PROF9    LA    R5,ABLNQ(,R5)       ELSE GO   TO   NEXT                          
         BCT   R3,PROF5                                                         
         ST    RF,SAVEHRS          SAVE HOURS                                   
         DROP  R5                                                               
*                                                                               
         MVC   LINSE1,=C'S=1'                                                   
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
         MVCDD LINTOTC1,AC#TOTAL,R TOTAL                                        
         LA    R1,LINTOTC1                                                      
         GOTO1 AGETPFLD                                                         
         EXTED SAVEHRS,LINHRS2,2,MINUS=YES,LANG=PLANG                           
         ZAP   DUB,TOTAMT                                                       
         AP    DUB,TOTNON                                                       
         CLI   PFCOMM,C'W'                                                      
         BNE   *+10                                                             
         AP    DUB,TOTCOMM         ADD  COMMISSION     TO   W/C  LINE           
*                                                                               
         CLI   PFDOL,C'Y'          SUPPRESS  THE  DOLLAR    SIGN ?              
         BE    PROF9A              YES                                          
         EXTED (P8,DUB),LINAMT,2,MINUS=YES,CURSYMB=S,LANG=PLANG                 
         B     PROF9B                                                           
*                                                                               
PROF9A   EXTED (P8,DUB),LINAMT,2,MINUS=YES,LANG=PLANG                           
*                                                                               
PROF9B   ZIC   RF,PARCNT                                                        
         LA    RF,2(,RF)                                                        
         CH    RF,MAXPARH                                                       
         BH    TOOPAR                                                           
         STC   RF,PARCNT                                                        
         BAS   RE,PUTPARA          OUTPUT    THE  PARAGRAPH                     
*                                                                               
PROFEX   B     EXIT                RETURN    TO   CALLER                        
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO PRINT SUMMARY PROFESSIONAL SERVICE FEE                   *         
***********************************************************************         
         SPACE 1                                                                
SUMFEE   NTR1                                                                   
         CLI   HRCNT,0             ANY  HOUR SUMMARY ?                          
         BE    SUMFEX              NO,  RETURN                                  
         BAS   RE,CLEARHED         CLEAR ANY OLD HEADLINES                      
*                                  INITIALIZE PARAGRAPH HEADERS                 
         LH    R1,=H'6007'         .    PARDSC=PROFESSIONAL SERVICES            
         BAS   RE,INITPARH         .           FOR THE PERIOD                   
         MVI   PARCNT,0                                                         
         MVI   WKSCNT,0                                                         
*                                                                               
         USING LINED,R2            MAP  LINE FORMAT                             
         USING ABLD,R5                                                          
         L     R2,APARLN                                                        
         L     R5,ATABLE           SET  UP FOR LOOP THRU TABLE                  
         ZIC   R3,TCNT             NUMBER IN TABLE                              
*                                                                               
SUMF5    CLI   ABLTYP,0            IS   IT DIRECT LABOR ?                       
         BNE   SUMF9               YES, GO PROCESS                              
*                                                                               
SUMF8    BAS   RE,ADDUP            ADD  TO TOTALS                               
         MVI   PARCNT,1            FOUND SOME LABOR                             
*                                                                               
SUMF9    LA    R5,ABLNQ(,R5)       ELSE GO TO NEXT                              
         BCT   R3,SUMF5                                                         
         DROP  R5                                                               
*                                                                               
         CLI   PARCNT,0                                                         
         BE    SUMFEX                                                           
         ZAP   DUB,TOTAMT                                                       
         AP    DUB,TOTNON                                                       
         CLI   PFCOMM,C'W'                                                      
         BNE   *+10                                                             
         AP    DUB,TOTCOMM         ADD  COMMISSION TO W/C LINE                  
         CLI   PFDOL,C'Y'          SUPPRESS THE DOLLAR SIGN ?                   
         BE    SUMF9A              YES                                          
         EXTED (P8,DUB),LINAMT,2,MINUS=YES,CURSYMB=S,LANG=PLANG                 
         B     SUMF9B                                                           
*                                                                               
SUMF9A   EXTED (P8,DUB),LINAMT,2,MINUS=YES,LANG=PLANG                           
*                                                                               
SUMF9B   BAS   RE,PUTPARA          OUTPUT THE PARAGRAPH                         
*                                                                               
SUMFEX   B     EXIT                RETURN TO CALLER                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO HANDLE DETAIL LINE                                       *         
***********************************************************************         
         SPACE 1                                                                
DTLINE   NTR1                                                                   
         CLI   PARCNT,0                                                         
         BNE   DTLIN04             NOT  FIRST LINE                              
         ZIC   R0,NEXTPARA         COUNT NUMBER OF PARAGRAPHS                   
         AH    R0,=H'1'                                                         
         STC   R0,NEXTPARA                                                      
         CH    R0,=H'1'                                                         
         BH    DTLIN02                                                          
         L     R2,AIOAREA3                                                      
         MVC   NEXTKEY,0(R2)       SAVE CURRENT KEY                             
         BAS   RE,HEADUP           FIRST TIME SET-UP HEADER RECORD              
         MVC   KEY,NEXTKEY                                                      
         GOTO1 AREAD,AIOAREA3      READ LAST RECORD                             
         OI    PARXTRA,X'80'       PRECEDE FIRST PARA WITH DIRECT COSTS         
*                                                                               
*                                  INITIALIZE PARAGRAPH HEADERS                 
DTLIN02  SR    R1,R1               .    PARDSC=0 (NOT SET)                      
         BAS   RE,INITPARH                                                      
*                                                                               
DTLIN04  ZIC   R0,PARCNT                                                        
         AH    R0,=H'1'                                                         
         CH    R0,MAXPARH          MORE THAN FIFTY PER PARAGRAPH ?              
         BH    TOOPAR              YES, ERROR                                   
         STC   R0,PARCNT                                                        
*                                                                               
         USING LINED,R2            MAP  LINE FORMAT                             
         L     R2,NEXTP                                                         
         MVI   LINBLANK,C' '                                                    
         MVC   LINCNTRA,CONTRA     VENDOR NAME/REF/DATE                         
*                                                                               
         USING TRNELD,R4           MAP  TRANSACTION ELEMENT                     
         L     R4,AIOAREA3                                                      
         AH    R4,DATADISP                                                      
*                                                                               
         CLI   PROGPROF+12,C'Y'    PRINTING LONG INVOICE NUMBER?                
         BNE   DTLIN14             NO                                           
         MVC   LINREFL,SPACES      YES, CLEAR IT                                
         MVI   LINREFL-1,C' '      MOVE IN SHORT INVOICE IN CASE                
         MVC   LINREFL(L'TRNREF),TRNREF                                         
         SR    RF,RF                                                            
         LR    R5,R4                                                            
*                                                                               
         USING FFTELD,R5                                                        
DTLIN06  CLI   0(R5),0             LOOK FOR LONG INVOICE                        
         BE    DTLIN12                                                          
         CLI   FFTTYPE,FFTTINVN    44 - SUPPLIER INVOICE NUMBER                 
         BE    DTLIN10                                                          
*                                                                               
DTLIN08  IC    RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     DTLIN06                                                          
*                                                                               
DTLIN10  SR    R1,R1                                                            
         ICM   R1,1,FFTDLEN        NO DATA, SKIP IT                             
         BZ    DTLIN08                                                          
         BCTR  R1,0                SUBTRACT 1 FOR MVC                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINREFL(0),FFTDATA  MAX LENGTH IS 20                             
         DROP  R5                                                               
*                                                                               
DTLIN12  GOTO1 VDATCON,DMCB,(1,TRNDATE),(8,LINDATL)                             
         B     DTLIN16                                                          
*                                                                               
DTLIN14  MVC   LINREFID,TRNREF                                                  
         GOTO1 VDATCON,DMCB,(1,TRNDATE),(8,LINDATE)                             
*                                                                               
         USING ABLD,R5                                                          
DTLIN16  LA    R5,ABLWRK                                                        
         ZAP   DTLINDUB,ABLNET                                                  
         AP    DTLINDUB,ABLNON                                                  
         CLI   PFCOMM,C'W'                                                      
         BNE   DTLIN18                                                          
         ST    R5,ONERUL                                                        
         L     RF,AIOAREA3                                                      
         BAS   RE,GETRUL           GET  COMMISSION FOR ONE ITEM                 
         BAS   RE,GETRATE          GET  GST/PST                                 
         XC    ONERUL,ONERUL                                                    
         AP    DTLINDUB,ABLCOM     ADD  COMMISSION TO W/C LINE                  
*                                                                               
DTLIN18  EXTED (P8,DTLINDUB),LINAMT,2,MINUS=YES,LANG=PLANG                      
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
         ST    R2,NEXTP                                                         
         CLI   PFNARR,C'Y'         OPTION TO PRINT DETAIL NARRATIVE             
         BNE   DTLINEX                                                          
         BAS   RE,DTLNAR           PRINT NARRATIVE                              
*                                                                               
DTLINEX  B     EXIT                RETURN TO CALLER                             
         DROP  R2,R4,R5                                                         
         EJECT ,                                                                
***********************************************************************         
* PRINT DETAIL NARRATIVE                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R4           MAP  TRANSACTION ELEMENT                     
         SPACE 1                                                                
DTLNAR   NTR1                                                                   
         MVI   CHOPWRK,C' '                                                     
         MVC   CHOPWRK+1(L'CHOPWRK-1),CHOPWRK                                   
         ZIC   R3,TRNLN                                                         
         SH    R3,=H'29'                                                        
         BM    DTLNAREX            NO   NARRATIVE, RETURN                       
         CLC   TRNNARR(2),=C'**'   DO   NOT PRINT NARRATIVE                     
         BE    DTLNAREX                                                         
         EXMVC R3,CHOPWRK,TRNNARR                                               
         LA    R0,L'CHOPWRK                                                     
         GOTO1 VSQUASHR,DMCB,CHOPWRK,(R0)                                       
         L     R3,DMCB+4           NUMBER OF CHARACTERS AFTER SQUASH            
         LTR   R3,R3                                                            
         BZ    DTLNAREX                                                         
         GOTO1 VCHOPPER,DMCB,((R3),CHOPWRK),(50,TEMP),3                         
         L     R3,DMCB+8           NUMBER OF LINES AFTER CHOP                   
         LTR   R3,R3                                                            
         BZ    DTLNAREX                                                         
         ZIC   R0,PARCNT                                                        
         AR    R0,R3                                                            
         AH    R0,=H'1'            PLUS ONE FOR THE S=1                         
         CH    R0,MAXPARH          MORE THAN MAX PER PARAGRAPH ?                
         BH    TOOPAR              YES, ERROR                                   
         STC   R0,PARCNT                                                        
*                                                                               
         USING LINED,R2            MAP  LINE FORMAT                             
         L     R2,NEXTP                                                         
         LA    R4,TEMP                                                          
         MVC   0(LINELNQ,R2),SPACES                                             
*                                                                               
*                                  MOVE FROM CHOP AREA                          
DTLNAR5  MVC   LINNARR,0(R4)       TO   PARAGRAPH LINE                          
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
         LA    R4,50(,R4)                                                       
         BCT   R3,DTLNAR5                                                       
*                                                                               
         MVC   0(LINELNQ,R2),SPACES                                             
         MVC   LINSE1,=C'S=1'                                                   
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
         ST    R2,NEXTP            SAVE ADDRESS OF NEXT PARAGRAPH LINE          
*                                                                               
DTLNAREX B     EXIT                RETURN TO CALLER                             
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO HANDLE WORK CODE TOTAL LINE                              *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION RECORD                      
         SPACE 1                                                                
WKLINE   NTR1                                                                   
         L     R2,AIOAREA3                                                      
         OC    WKCLAST,WKCLAST                                                  
         BNZ   WKLIN1                                                           
         MVC   WKCLAST,TRNKWORK    SAVE FIRST WORK CODE                         
         B     WKLINEX             RETURN                                       
*                                                                               
WKLIN1   CLC   TRNKWORK,WKCLAST    SAME WORK CODE ?                             
         BE    WKLINEX             YES, RETURN                                  
         MVC   WKCLAST,TRNKWORK    SAVE NEXT WORK CODE                          
         MVC   NEXTKEY,0(R2)       SAVE KEY OF NEXT DETAIL RECORD               
         CLI   PARCNT,0                                                         
         BE    WKLINX              NO   DETAILS                                 
         CLI   TCNT,0                                                           
         BE    WKLINX                                                           
         L     RF,AIOAREA3                                                      
         BAS   RE,GETRUL           GET  COMMISSION                              
         BAS   RE,GETRATE          GET  GST/PST                                 
         MVI   WKSCNT,0                                                         
         DROP  R2                                                               
*                                                                               
         USING ABLD,R5             TOTAL FOR WORK CODE                          
         L     R5,ATABLE                                                        
         ZIC   R0,TCNT                                                          
*                                                                               
WKLIN2   BAS   RE,ADDUP            ADD  EACH ENTRY                              
         LA    R5,ABLNQ(,R5)                                                    
         BCT   R0,WKLIN2                                                        
*                                                                               
         L     R5,ATABLE                                                        
         GOTO1 AGETWC,ABLCDE       GET  WORK CODE NAME                          
         DROP  R5                                                               
*                                                                               
         MVC   PARDSC(15),WORK                                                  
         CLI   PFUPPER,C'M'        MIXED     CASE DATA ?                        
         BE    WKLIN5              YES, SKIP                                    
         L     R1,AUPPERCT         ->   UPPER     CASE TABLE                    
         TR    PARDSC(15),0(R1)    TRANSLATE TO   UPPER     CASE                
*                                                                               
WKLIN5   ZIC   R0,PARCNT                                                        
         AH    R0,=H'1'                                                         
         CH    R0,MAXPARH          MORE THAN MAX PER PARAGRAPH ?                
         BH    TOOPAR              YES, ERROR                                   
         STC   R0,PARCNT                                                        
*                                                                               
         BAS   RE,PUTPARA          OUTPUT THE PARAGRAPH                         
*                                                                               
         MVC   KEY,NEXTKEY                                                      
         GOTO1 AREAD,AIOAREA3      READ NEXT WORK CODE                          
*                                                                               
WKLINX   MVI   TCNT,0              RESET ACCUMS                                 
         MVC   TOTAMT(TOTAMT#*PLAMTLNQ),ZEROS                                   
         MVC   PARDSC,SPACES       AND  PARAGRAPH DETAILS                       
         MVI   PARCNT,0                                                         
         L     R2,APARLN                                                        
         ST    R2,NEXTP            SAVE ADDRESS OF NEXT PARLN                   
*                                                                               
WKLINEX  B     EXIT                RETURN TO CALLER                             
         EJECT ,                                                                
***********************************************************************         
* ADD TABLE ENTRY TO PARAGRAPH TOTALS                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING ABLD,R5             TOTAL FOR WORK CODE                          
         SPACE 1                                                                
ADDUP    NTR1                                                                   
         LA    R1,ABLNET           ADD  CURRENT   TO                            
         LA    R2,TOTAMT                PARAGRAPH AND                           
         LA    R3,JOBNET                JOB  TOTALS                             
         LA    R0,ABLBUCK#                                                      
*                                                                               
ADDUP4   AP    0(PLAMTLNQ,R2),0(PLAMTLNQ,R1)                                    
         AP    0(PLAMTLNQ,R3),0(PLAMTLNQ,R1)                                    
*                                                                               
         LA    R1,PLAMTLNQ(,R1)                                                 
         LA    R2,PLAMTLNQ(,R2)                                                 
         LA    R3,PLAMTLNQ(,R3)                                                 
         BCT   R0,ADDUP4                                                        
*                                                                               
         USING WKSD,R7                                                          
         L     R7,AWKSTBLE         TABLE OF PARAGRAPH WORK CODES                
         ZIC   R0,WKSCNT           NUMBER IN TABLE                              
*                                                                               
ADDUP6   LTR   R0,R0                                                            
         BNZ   ADDUP7                                                           
         XC    0(WKSLNQ,R7),0(R7)  INITIALIZE NEW ENTRY AND                     
         MVC   WKSNET(WKSN#*PLAMTLNQ),ZEROS                                     
         ZIC   R1,WKSCNT           UPDATE COUNT                                 
         LA    R1,1(,R1)                                                        
         STC   R1,WKSCNT                                                        
         CLI   WKSCNT,WKSMAX       TOO  MANY WORK CODES ?                       
         BH    TOOWRK              YES, ERROR                                   
         B     ADDUP8                                                           
*                                                                               
ADDUP7   CLC   ABLCDE,WKSCODE                                                   
         BE    ADDUP8                                                           
         LA    R7,WKSLNQ(,R7)                                                   
         BCTR  R0,0                                                             
         B     ADDUP6                                                           
*                                                                               
ADDUP8   MVC   WKSCODE,ABLCDE                                                   
         AP    WKSNET,ABLNET       ADD  NET                                     
         AP    WKSNON,ABLNON       AND  NON COMMISSIONABLE                      
         B     EXIT                RETURN TO CALLER                             
         DROP  R5,R7                                                            
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO GET COMMISSION RULES                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING GOBLOCK,R7                                                       
         SPACE 1                                                                
GETRUL   NTR1                                                                   
         L     R7,AGOBLOCK                                                      
         LTR   RF,RF               DO WE NEED TO RE-READ LAST RECORD?           
         BZ    *+8                 NO                                           
         ST    RF,GOAKEY           YES, PASS THE ADDRESS                        
*                                                                               
         MVC   GOADM,VDATAMGR      SETUP GOBLOCK                                
         MVC   GOSELCUL,JOBKEY                                                  
         MVC   GOSELCLI,LCLI                                                    
         MVC   GOSELPRO,LPRO                                                    
         MVC   GOSELJOB,LJOB                                                    
         MVC   GOACOMP,ADCMP                                                    
         MVC   GOACLI,ADCLI                                                     
         MVC   GOAPRO,ADPRD                                                     
         MVC   GOAJOB,ADJOB                                                     
         MVI   GOWHICH,0                                                        
         MVI   GOANYMED,0                                                       
         MVI   GOSELLEV,0                                                       
*                                                                               
         USING ABLD,R5                                                          
         L     R5,ATABLE                                                        
         ZIC   R0,TCNT                                                          
         OC    ONERUL,ONERUL                                                    
         BZ    NR06                                                             
         L     R5,ONERUL           ONLY ONE CODE (IF DETAIL LINE)               
         LA    R0,1                                                             
*                                                                               
NR06     MVC   GOSELWC,ABLCDE                                                   
         GOTO1 VGETOPT,DMCB,AGOBLOCK                                            
         ZAP   DUB,GOAGYCOM                                                     
         MVC   ABLCODE,GOTAXCOD                                                 
         MVC   ABLPCODE,GOPSTCOD   PST  CODE                                    
         MVC   ABLPRV,GOPSTPRV     PROVINCE                                     
*                                                                               
         ZAP   PK2D(16),ABLNET     NET                                          
         MP    PK2D(16),DUB+2(6)   NET  *  RATE                                 
         SRP   PK2D(16),64-6,5     RATE HAS 4DP SO ROUND                        
         ZAP   ABLCOM,PK2D+8(8)    SAVE COMMISSION                              
         LA    R5,ABLNQ(,R5)                                                    
         BCT   R0,NR06                                                          
         XC    GOAKEY,GOAKEY                                                    
         B     EXIT                RETURN TO CALLER                             
         DROP  R5,R7                                                            
         EJECT ,                                                                
         SPACE 1                                                                
         USING VTCD,R2                                                          
         USING TWA1D,R3                                                         
         USING SVTABD,R4                                                        
         USING ABLD,R5                                                          
         USING GOBLOCKD,R7                                                      
         SPACE 1                                                                
GETRATE  NTR1                                                                   
         L     R2,AVATBUFF                                                      
         L     R3,ATWA1                                                         
         L     R5,ATABLE                                                        
         L     R7,AGOBLOCK                                                      
         ZIC   R0,TCNT                                                          
         OC    ONERUL,ONERUL                                                    
         BZ    GETR00                                                           
         L     R5,ONERUL           ONLY ONE CODE(IF DETAIL LINE)                
         LA    R0,1                                                             
*                                                                               
GETR00   TM    RUNOPT,NEEDGST      DOING GST LOGIC ?                            
         BZ    GETRX               NO,  RETURN                                  
*                                                                               
GETR01   LA    R4,SVTABLE                                                       
         LA    R6,VATNUM                                                        
         XC    ABLRATE,ABLRATE     CLEAR RATE AND                               
         XC    ABLPRATE,ABLPRATE   .     PST  RATE                              
         OC    ABLCODE,ABLCODE     DO    WE   HAVE A VAT CODE ?                 
         BZ    GETR08              NO,   TRY  NEXT                              
*                                                                               
GETR02   CLI   SVTYPE,X'00'        NOTHING IN TABLE OR END                      
         BE    GETR04                                                           
         CLI   SVTYPE,SVT_GST      IS   THIS A GST ENTRY ?                      
         BNE   GETR03              NO,  GET NEXT                                
         CLC   ABLCODE,SVCODE      FIND CODE IN TABLE                           
         BE    GETR06                                                           
*                                                                               
GETR03   LA    R4,SVTABLNQ(,R4)                                                 
         BCT   R6,GETR02                                                        
         DC    H'0'                VATNUM IS TOO SMALL IN ACBILWORK             
*                                                                               
GETR04   MVI   VTCACTN,VTCALOOK    LOOK FOR RATES                               
         MVC   VTCLANG,PLANG       USE  PRINT LANGUAGE                          
         MVC   VTCCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   VTCCPY,JOBKEY       COMPANY CODE                                 
         MVC   VTCOFFC,GOEFFOFC    OFFICE                                       
         MVC   VTCINVD,TODAYP      INVOICE/UNBILL DATE                          
         MVC   VTCTYPE,ABLCODE     PASS VAT TYPE CODE                           
         GOTO1 VVATICAN,(R2)                                                    
         TM    VTCINDS,VTCINA      IS   TAX APPLICABLE ?                        
         BO    GETR08              NO                                           
         CLI   VTCERR,X'00'        YES, FIND TYPE ?                             
         BNE   GETR08              NO                                           
         MVC   SVRATE,VTCRATE      YES, PUT RATE,                               
         MVC   SVINDS,VTCINDS               DECIMALS,                           
         MVC   SVACCT,VTCACT       .        ACCOUNT,                            
         MVC   SVCODE,ABLCODE      .        CODE AND                            
         MVC   SVDATE,VTCEFFD      .        EFFECTIVE DATE IN TABLE             
*                                                                               
GETR06   MVC   ABLRATE,SVRATE      SAVE RATE                                    
         DROP  R2                                                               
*                                                                               
         ZAP   DUB,ABLNET          NET  TO DUB                                  
         AP    SVNET,ABLNET        NET  ADDED TO SVNET                          
*                                                                               
         AP    SVNON,ABLNON        NON-COMMISSIONABLE ADDED TO SVNON            
         AP    DUB,ABLNON          NON-COMMISSIONABLE ADDED TO DUB              
*                                                                               
         AP    SVCOMM,ABLCOM       +    COMMISSION                              
         AP    DUB,ABLCOM                                                       
*                                                                               
         AP    SVDISC,ABLCSD       +    DISCOUNT                                
         SP    DUB,ABLCSD          -    DISCOUNT                                
*                                                                               
         AP    SVBASE,DUB          ADD  INTO SVBASE                             
*                                                                               
         ZAP   PK16,DUB                                                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,ABLRATE                                                     
         CVD   RE,DUB                                                           
         MP    PK16,DUB            NET  + COMMISSION - CSD * GST RATE           
*                                                                               
         TM    SVINDS,VBIIDC3      DOES RATE HAVE 3 DECIMALS?                   
         BZ    *+14                NO                                           
         SRP   PK16,64-5,5         YES, SHIFT ONE MORE DIGIT                    
         B     *+10                                                             
         SRP   PK16,64-4,5         RATE HAS 4DP SO ROUND                        
*                                                                               
         ZAP   ABLGST,PK16+8(8)    SAVE GST AMOUNT                              
         AP    SVGST,PK16                                                       
*                                  NOTE: PL16 USED BELOW AS PST BASE            
*                                                                               
         TM    RUNOPT,NEEDPST      ARE  WE DOING PST LOGIC ?                    
         BZ    GETR08              NO,  PROCESS NEXT ABLTAB ENTRY               
*                                                                               
         LA    R4,SVTABLE                                                       
         LA    R6,VATNUM                                                        
         OC    ABLPCODE,ABLPCODE   DO   WE HAVE A PST CODE ?                    
         BZ    GETR08              NO,  TRY NEXT                                
*                                                                               
GETR07   CLI   SVTYPE,X'00'        NOTHING IN TABLE OR END                      
         BE    GETR07D                                                          
         CLI   SVTYPE,SVT_PST      IS   THIS A PST ENTRY                        
         BNE   GETR07B             NO,  GET NEXT                                
         CLC   ABLPCODE,SVCODE     FIND CODE IN TABLE                           
         BNE   GETR07B             NO                                           
         CLC   ABLPRV,SVPRVNCE     YES, CHECK PROVINCE                          
         BE    GETR07F             YES                                          
*                                                                               
GETR07B  LA    R4,SVTABLNQ(,R4)                                                 
         BCT   R6,GETR07                                                        
         DC    H'0'                VATNUM IS TOO SMALL IN ACBILDSECT            
*                                                                               
         USING VTCD,R2                                                          
GETR07D  L     R2,AVATBUFF                                                      
         MVI   VTCACTN,VTCALOOK    LOOK FOR RATES                               
         MVC   VTCLANG,PLANG       USE  PRINT LANGUAGE                          
         MVC   VTCCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   VTCCPY,JOBKEY       COMPANY CODE                                 
         MVC   VTCOFFC,GOEFFOFC    OFFICE                                       
         MVC   VTCINVD,TODAYP      INVOICE/UNBILL DATE                          
         MVC   VTCTYPE,ABLPCODE    PASS VAT TYPE CODE                           
         MVC   VTCPRV,ABLPRV                                                    
         GOTO1 VVATICAN,(R2)                                                    
         TM    VTCINDS,VTCINA      IS   TAX APPLICABLE ?                        
         BO    GETR08              NO                                           
         CLI   VTCERR,X'00'        YES, FIND TYPE ?                             
         BNE   GETR08              NO                                           
         MVC   SVRATE,VTCRATE      YES, PUT RATE,                               
         MVC   SVINDS,VTCINDS               DECIMALS,                           
         MVC   SVACCT,VTCACT       .        ACCOUNT,                            
         MVC   SVCODE,ABLPCODE     .        PST CODE,                           
         MVC   SVPRVNCE,ABLPRV     .        PROVINCE AND                        
         MVC   SVDATE,VTCEFFD      .        EFFECTIVE DATE IN TABLE             
*                                                                               
GETR07F  MVC   ABLPRATE,SVRATE     SAVE PST RATE                                
         SR    RE,RE                                                            
         ICM   RE,3,ABLPRATE                                                    
         CVD   RE,DUB                                                           
         MP    PK16,DUB            PK16 IS PST BASE, FROM GST CALC              
*                                                                               
         TM    SVINDS,PBIIDC3      DOES RATE HAVE 3 DECIMALS?                   
         BZ    *+14                NO                                           
         SRP   PK16,64-5,5         YES, SHIFT ONE MORE DIGIT                    
         B     *+10                                                             
         SRP   PK16,64-4,5         RATE HAS 4DP SO ROUND                        
*                                                                               
         ZAP   ABLPST,PK16+8(8)    SAVE PST AMOUNT                              
*                                                                               
GETR08   LA    R5,ABLNQ(,R5)                                                    
         BCT   R0,GETR01                                                        
*                                                                               
GETRX    B     EXIT                RETURN TO CALLER                             
         DROP  R2,R3,R4,R5,R7                                                   
         EJECT ,                                                                
***********************************************************************         
* ADD HEADER RECORD TO WORKER LIBRARY BOOK                            *         
***********************************************************************         
         SPACE 1                                                                
HEADUP   NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         GOTO1 AREADL,AIOAREA2     GET  COMPANY RECORD                          
         BNE   HEADUPEX            NONE, RETURN                                 
*                                                                               
HEADUP1  L     RE,AIOAREA2                                                      
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
HEADUP2  CLI   0(RE),0             LOCATE COMPANY ELEMENT                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),CPYELQ        X'10' - COMPANY ELEMENT                      
         BE    HEADUP3                                                          
         IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     HEADUP2                                                          
*                                                                               
         USING CPYELD,RE           MAP  COMPANY ELEMENT                         
HEADUP3  OC    CPYSBILL,CPYSBILL   GET  BILL NUMBER                             
         BNZ   *+10                                                             
         ZAP   CPYSBILL,=P'0'      1ST  CLIENT BILL                             
         AP    CPYSBILL,=P'1'                                                   
         ZAP   BILNUMP,CPYSBILL    SAVE FOR DISPLAY                             
         DROP  RE                                                               
*                                                                               
         GOTO1 AWRITE,AIOAREA                                                   
         BNE   HEADUPEX                                                         
*                                                                               
         GOTO1 AWRKID              GET  WORKER ID INTO WRKEY                    
         GOTO1 AFINDBIL,AIOAREA1   IS   IT ALREADY IN USE ?                     
         BE    HEADUP1             YES, TRY AGAIN                               
         GOTO1 VDATAMGR,DMCB,DMUNLK,ACCOUNT,AIOAREA                             
         MVI   WRKLPARA,0                                                       
*                                                                               
         USING UKRECD,RF                                                        
         LA    RF,WRKEY            NOW  ADD A NEW BOOK                          
         L     R7,AIOAREA1                                                      
         LA    R7,28(,R7)                                                       
*                                                                               
         USING WKRECD,R7                                                        
         MVI   UKFLAG,X'1A'        SET  RETN/SET COMMENT/LIBRARY-TYPE           
         XC    WKCOMNT,WKCOMNT                                                  
         MVC   WKCOMNT(L'JOBKEY-3),JOBKEY+3                                     
         MVC   WKRETN,=H'7'                                                     
         GOTO1 AWRKOPE,AIOAREA1                                                 
         BE    HEADUP12                                                         
         TM    DMCB+8,X'20'        DUPLICATE                                    
         BZ    HEADUPEX                                                         
         B     HEADUP1             TRY  AGAIN                                   
         DROP  R7,RF                                                            
*                                                                               
         USING HEADERD,R7                                                       
HEADUP12 L     R7,AIOAREA1         ADD  HEADER    RECORD                        
         XC    0(HEND,R7),0(R7)                                                 
         MVI   HTYPE,C'A'                                                       
         MVC   HCLI,BILCLI                                                      
         OC    HCLI,SPACES                                                      
         MVC   HCLIN,BILCLIN                                                    
         MVC   HPRO,BILPRO                                                      
         OC    HPRO,SPACES                                                      
         MVC   HPRON,BILPRON                                                    
         MVC   HJOB,BILJOB                                                      
         OC    HJOB,SPACES                                                      
         MVC   HJOBN,BILJOBN                                                    
         MVI   HBILL,C'N'          NOT  YET  BILLED                             
         MVC   HINVNUM,SPACES                                                   
         MVC   HPLANG,PLANG        SAVE PRINT     LANGUAGE                      
         CLI   PLANG,0             ANY  PRINT     LANGUAGE ?                    
         BNE   *+10                YES, SKIP                                    
         MVC   HPLANG,CULANG       USE  CURRENT   LANGUAGE                      
         LA    R0,HEND                                                          
         STH   R0,HLEN                                                          
         GOTO1 AWRKADD,AIOAREA1                                                 
         BNE   WRKERR                                                           
*                                                                               
HEADUPEX B     EXIT                RETURN TO CALLER                             
         DROP  R7                                                               
         EJECT ,                                                                
***********************************************************************         
* CLEAR HEADLINES                                                     *         
***********************************************************************         
         SPACE 1                                                                
CLEARHED L     R2,APARLN           CLEAR UNUSED HEADLINES                       
         LA    R0,MAXPAR                                                        
*                                                                               
CLEARHD5 MVC   0(LINELNQ,R2),SPACES                                             
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
         BCT   R0,CLEARHD5                                                      
         MVC   TOTAMT(TOTAMT#*PLAMTLNQ),ZEROS                                   
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
* INITIALIZE PARAGRAPH HEADERS                                        *         
*                                                                     *         
*   INPUT:                                                            *         
*     R1 CONTAINS THE MESSAGE NUMBER FOR PARDSC OR ZERO (IF NONE)     *         
***********************************************************************         
         SPACE 1                                                                
INITPARH NTR1                                                                   
         LR    R2,R1               SAVE MESSAGE   NUMBER                        
         MVI   PARHEADS,2                                                       
         MVC   PARHED1,SPACES      CLEAR     HEAD LINE 1                        
         MVC   PARHED2,SPACES      CLEAR     HEAD LINE 2                        
*                                  AMOUNT                                       
         MVCDD PARHED1+LINAMT-LINED(L'LINAMT-1),AC#AMT,R                        
         LA    R1,PARHED1+LINAMT-LINED                                          
         GOTO1 AGETPFLD                                                         
*                                  AMOUNT    UNDERLINED                         
         MVCDD PARHED2+LINAMT-LINED(L'LINAMT-1),AC#AMT,RU                       
         LA    R1,PARHED2+LINAMT-LINED                                          
         GOTO1 AGETPFLD                                                         
*                                                                               
         LTR   R2,R2               ANY  DESCRIPTION ?                           
         BZ    INITPARX            NO,  RETURN                                  
         MVC   PARDSC,SPACES       CLEAR     DESCRIPTION                        
*                                  DESCRIPTION                                  
         GOTO1 ATXTGET,DMCB,(R2),(L'PARDSC,PARDSC),0,(PLANG,0)                  
*                                                                               
INITPARX B     EXIT                RETURN    TO   CALLER                        
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO OUTPUT PARAGRAPH RECORDS                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING PARAD,R5                                                         
         SPACE 1                                                                
PUTPARA  NTR1                                                                   
*                                  BUILD BLOCK FOR UNSCAN                       
         GOTO1 =A(BLOCK),DMCB,(RC),RR=MYRELO                                    
         CLI   PARCNT,0            ANY  PARAGRAPHS ?                            
         BE    NOPAR               NO,  ERROR                                   
         L     R2,APARLN                                                        
         L     R5,AIOAREA4                                                      
*                                                                               
PUTPAR2  BAS   RE,PARINIT          INITIALIZE PARAGRAPH                         
*                                  UNSCAN THE WORK CODE LINE                    
         GOTO1 =A(UNSCAN),DMCB,(RC),RR=MYRELO                                   
         CLC   PARAWRK,SPACES      ANY  WORK CODES ?                            
         BE    PUTPAR3             NO,  SKIP                                    
         LA    RF,L'PARAWRK                                                     
         GOTO1 AWKDCODE,DMCB,((RF),PARAWRK)                                     
         GOTO1 APARCHR             GET  CHARGES FOR PARAGRAPH                   
         ZAP   PARANET,PNET                                                     
         ZAP   PARACOM,PCOM                                                     
         ZAP   PARANON,PNON                                                     
         ZAP   PARACSD,PCSD                                                     
         ZAP   PARAGST,PGST                                                     
         ZAP   PARAPST,PPST                                                     
*                                                                               
PUTPAR3  CLI   PARCNT,0            ANY  PARAGRAPH INFO TO WRITE ?               
         BE    PUTPAR9             NO,  SKIP PARAGRAPHS                         
*                                                                               
PUTPAR7  BAS   RE,PUTEL            ADD  PARAGRAPHS TO RECORD                    
         ZIC   R1,PARCNT           REDUCE NUMBER OF PARAGRAPHS TO WRITE         
         BCTR  R1,0                                                             
         STC   R1,PARCNT                                                        
         LA    R2,LINELNQ(,R2)     NEXT WORK LINE                               
         ZIC   R1,PCNT                                                          
         LA    R1,1(,R1)                                                        
         STC   R1,PCNT             COUNT NUMBER ADDED TO RECORD                 
         CLI   PCNT,10                                                          
         BE    PUTPAR9             CORRENT RECORD IS FULL                       
         CLI   PARCNT,0            ANY  MORE PARAGRAPH LINES ?                  
         BNE   PUTPAR7             YES, ADD  THEM TO RECORD                     
*                                                                               
PUTPAR9  MVI   CONTIN,C'N'                                                      
         CLI   PARCNT,0            ANY  MORE PARAGRAPH LINES ?                  
         BE    *+8                                                              
         MVI   CONTIN,C'Y'         YES, SAY  PARAGRAPH IS CONTINUED             
         CLI   SCOUNT,0            ANY  WORK CODES ?                            
         BE    *+8                                                              
         MVI   CONTIN,C'Y'         YES, SAY  PARAGRAPH IS CONTINUED             
         CLI   CONTIN,C'N'         IS   THE  PARAGRAPH CONTINUED ?              
         BE    PUTPAR12            NO,  SKIP                                    
         LA    R1,PARAWRK+(L'PARAWRK-1)                                         
         LA    R0,L'PARAWRK                                                     
*                                                                               
PUTPAR10 CLC   0(1,R1),WCCOMMA     SO   MAKE LAST A ','                         
         BE    PUTPAR12            ALREADY SET                                  
         CLI   0(R1),X'40'                                                      
         BH    PUTPAR11                                                         
         BCTR  R1,0                                                             
         BCT   R0,PUTPAR10                                                      
*                                                                               
PUTPAR11 MVC   1(1,R1),WCCOMMA                                                  
*                                                                               
PUTPAR12 GOTO1 AWRKADD,AIOAREA4    WRITE THE RECORD                             
         BNE   PUTPAREX                                                         
*                                                                               
         CLI   CONTIN,C'Y'         ANY  MORE ?                                  
         BE    PUTPAR2             YES, GO PUT THEM OUT                         
*                                  NO,  ALL DONE WITH THIS PARAGRAPH            
*                                                                               
PUTPAREX B     EXIT                RETURN TO CALLER                             
         DROP  R5                                                               
         EJECT ,                                                                
         SPACE 1                                                                
         USING PARAD,R5                                                         
         SPACE 1                                                                
PARINIT  NTR1                      BUILD SKELETON PARA REC                      
         XC    PARAD(PARATXT-PARAD),PARAD                                       
         MVI   PARAEL,1                                                         
         MVI   PARALEN,PARALNQ                                                  
         MVI   PARATYPE,C'B'                                                    
         MVC   PARAHLNS,PARHEADS   NUMBER OF HEADLINES AND                      
         MVC   PARADSC,PARDSC      DESCRIPTION                                  
         MVC   PARAXTRA,PARXTRA                                                 
         MVI   PARXTRA,0                                                        
         MVC   PARAWRK,SPACES                                                   
         MVC   PARDSC,SPACES       DESCRIPTION ONLY ON FIRST                    
         ZAP   PARACOM,=P'0'                                                    
         ZAP   PARANET,=P'0'                                                    
         ZAP   PARANON,=P'0'                                                    
         ZAP   PARACSD,=P'0'                                                    
         ZAP   PARAGST,=P'0'                                                    
         ZAP   PARAPST,=P'0'                                                    
*                                                                               
         MVI   PARATXT,0                                                        
         LA    R1,PARALNQ+3                                                     
         STC   R1,PARARLEN+1                                                    
         LA    R4,PARATXT                                                       
         ST    R4,ANXTCOM                                                       
         MVC   PCNT,PARAHLNS                                                    
         ZIC   R0,PARAHLNS         NUMBER OF HEAD LINES                         
         LTR   R0,R0               ANY  HEAD LINES ?                            
         BZ    PARINITX            NO,  RETURN                                  
         LA    R2,PARHED1          ADD  HEAD LINES TO PARAGRAPH                 
*                                                                               
PARINIT8 BAS   RE,PUTEL                                                         
         LA    R2,LINELNQ(,R2)     NEXT LINE                                    
         BCT   R0,PARINIT8                                                      
*                                                                               
PARINITX B     EXIT                RETURN    TO   CALLER                        
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
* ADD COMMENT ELEMENT                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING SCMELD,R4           MAP  STANDARD COMMENT ELEMENTS               
         USING PARAD,R5                                                         
         SPACE 1                                                                
PUTEL    NTR1                                                                   
         L     R4,ANXTCOM                                                       
         MVI   SCMEL,SCMELQ        X'3E' - STANDARD COMMENT ELEMENT             
*                                  LENGTH =70+4                                 
         MVI   SCMLN,LINELNQ+SCMLN1Q                                            
         LA    R3,1(,R3)                                                        
         STC   R3,SCMSEQ           SEQUENCE                                     
         MVI   SCMTYPE,0           TYPE                                         
         MVC   SCMNARR(LINELNQ),0(R2)                                           
         MVC   0(LINELNQ,R2),SPACES                                             
         CLC   SCMNARR(2),=C'S='                                                
         BNE   *+8                                                              
         MVI   SCMLN,7                                                          
         ZIC   R1,1(,R4)                                                        
         AR    R4,R1               NEXT COMMENT AREA                            
         MVI   0(R4),0                                                          
         ST    R4,ANXTCOM                                                       
         SR    RF,RF                                                            
         ICM   RF,3,PARARLEN                                                    
         AR    RF,R1                                                            
         STCM  RF,3,PARARLEN       UPDATE PARAGRAPH LENGTH                      
         B     EXIT                RETURN TO CALLER                             
         DROP  R4,R5                                                            
         EJECT ,                                                                
***********************************************************************         
* ERROR ROUTINES - THE TEXT OF ALL OF THESE MESSAGES IS: ERROR - ...  *         
***********************************************************************         
         SPACE 1                                                                
BADFORM  MVC   FVMSGNO,=AL2(2201)  INVALID BILL FORMAT                          
         LA    R1,BILACTH                                                       
         B     SETERR1                                                          
*                                                                               
NOCHR    MVC   FVMSGNO,=AL2(2202)  NO ALLOCATED CHARGES ON BILL                 
         B     SETERR                                                           
*                                                                               
TABFULL  MVC   FVMSGNO,=AL2(2203)  TOO MANY ITEMS IN DETAIL TABLE               
         B     SETERR                                                           
*                                                                               
TOOPAR   MVC   FVMSGNO,=AL2(2204)  TOO MANY LINES IN PARAGRAPH                  
         B     SETERR                                                           
*                                                                               
TOOWRK   MVC   FVMSGNO,=AL2(2205)  TOO MANY WORK CODES IN PARAGRAPH             
         B     SETERR                                                           
*                                                                               
NOPAR    MVC   FVMSGNO,=AL2(2206)  UNABLE TO BUILD PARAGRAPH                    
         B     SETERR                                                           
*                                                                               
WRKERR   MVC   FVMSGNO,=AL2(2207)  UNABLE TO GENERATE AUTO BILL                 
         B     SETERR                                                           
*                                                                               
SETERR   LA    R1,BILJOBH                                                       
*                                                                               
SETERR1  ST    R1,FADR                                                          
         MVI   WRKSTAT,0                                                        
         MVI   FNDX,0                                                           
         GOTO1 AERRORX2                                                         
         EJECT ,                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DROP  RA                                                               
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO DISPLAY FIRST PARAGRAPH                                  *         
***********************************************************************         
         SPACE 1                                                                
DSPL     NMOD1 0,*DSPL*                                                         
         L     RC,0(,R1)                                                        
         TWAXC BLPWRKH,BLPJOBTH    CLEAR SCREEN                                 
         GOTO1 AFINDBIL,AIOAREA2   FIND THE BILL                                
         BNE   DSWKER                                                           
         MVI   PARAHEX,1                                                        
         MVC   WRKPARA,PARAHEX     AND  GET FIRST                               
         GOTO1 AWRKLGET,AIOAREA4                                                
         BNE   DSWKER                                                           
*                                                                               
         USING PARAD,R6                                                         
         L     R6,AIOAREA4                                                      
         MVC   BLPWRK,PARAWRK      DISPLAY WORK CODES                           
         OC    BLPWRK,SPACES                                                    
         OI    BLPWRKH+6,FVOXMT    TRANSMIT                                     
         MVC   BLPDSC,PARADSC      DESCSRIPTON                                  
*        OC    BLPDSC,SPACES                                                    
         OI    BLPDSCH+6,FVOXMT    TRANSMIT                                     
*                                                                               
         GOTO1 ANARRDIS,AIOAREA4   NARRATIVE                                    
*                                                                               
         GOTO1 ABLDCHR             BUILD TABLE OF CHARGES                       
         CLI   CHRCNT,0                                                         
         BE    DSNOCH                                                           
         GOTO1 ABILCHR             GET  BILL TOTALS                             
*                                                                               
         USING CHRD,R5                                                          
         LA    R5,CHRTOT                                                        
         LA    R1,JOBNET                                                        
         LA    R2,CHRANET                                                       
         LA    R0,5                JOB  TOTALS TO CHARGES LINE                  
*                                                                               
DSPL3    ZAP   0(PLAMTLNQ,R2),0(PLAMTLNQ,R1)                                    
         LA    R1,PLAMTLNQ(,R1)                                                 
         LA    R2,PLAMTLNQ(,R2)                                                 
         BCT   R0,DSPL3                                                         
*                                                                               
         ZAP   PNET,PARANET                                                     
         ZAP   PCOM,PARACOM                                                     
         ZAP   PNON,PARANON                                                     
         ZAP   PCSD,PARACSD                                                     
         ZAP   PGST,PARAGST                                                     
         ZAP   PPST,PARAPST                                                     
         GOTO1 ADISTOT             DISPLAY PARAGRAPH TOTALS                     
*                                                                               
         OI    BILACTH+6,X'81'                                                  
         MVC   BILACT,SPACES                                                    
         MVC   BILACT(12),=C'DISPLAY,NEXT'                                      
*                                  DISPLAY,NEXT                                 
*        GOTO1 ATXTGET,DMCB,6008,BILACTH,0,0                                    
         MVI   LACTION,DID         CHANGE ACTION                                
         MVC   BILNUM,SPACES                                                    
         OI    BILNUMH+6,FVOXMT    TRANSMIT                                     
         EDIT  BILNUMP,(6,BILNUM),ALIGN=LEFT                                    
         MVC   BILPARA,SPACES                                                   
         MVI   BILPARA,C'1'                                                     
         MVI   LPARA,1                                                          
*                                  BILL NUMBER NNNN - ADDED TO FILE             
         GOTO1 ATXTGET,DMCB,6009,(L'MSG,MSG),(6,BILNUM),0                       
         OI    BILPARAH+6,FVOXMT   TRANSMIT                                     
         LA    R2,BLPJOBTH                                                      
         ST    R2,FADR                                                          
         XC    BYTE,BYTE                                                        
         B     DSPLX                                                            
*                                                                               
DSWKER   MVI   BYTE,C'W'                                                        
         B     DSPLX                                                            
*                                                                               
DSNOCH   MVI   BYTE,C'C'                                                        
*        B     DSPLX                                                            
*                                                                               
DSPLX    XMOD1 ,                   EXIT                                         
*                                                                               
         DROP  R5,R6                                                            
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO BUILD BLOCK OF CODE=AMOUNT FOR UNSCAN                    *         
***********************************************************************         
         SPACE 1                                                                
         USING WKSD,R7                                                          
         SPACE 1                                                                
BLOCK    NMOD1 0,**BLCK**                                                       
         L     RC,0(,R1)                                                        
         SR    R6,R6               COUNT     NUM  OF   UNSCAN   ENTRIES         
         MVI   SCOUNT,0                                                         
         L     R7,AWKSTBLE         TABLE     OF   CODES                         
         ZIC   R3,WKSCNT           NUMBER    IN   TABLE                         
         LTR   R3,R3               ANY  WORK CODES ?                            
         BZ    BLOCKEX             NO,  RETURN                                  
         L     R4,AIOAREA1         USE  IOAREA1   FOR  UNSCAN    BLOCK          
*                                                                               
BLOCK2   MVC   0(23,R4),SPACES                                                  
         MVC   0(2,R4),WKSCODE     WORK CODE ON   LEFT                          
         CP    WKSNET,=P'0'        ANY  COMMISSIONABLE ?                        
         BE    BLOCK5              NO,  SKIP                                    
         EXTED WKSNET,(12,10(R4)),2,FLOAT=-,ALIGN=LEFT,CURSYMB=S,      X        
               LANG=PLANG                                                       
         LA    R2,10(,R4)          FIND 1ST  AVAILABLE BYTE                     
         AR    R2,R0                                                            
*                                                                               
         MVI   0(R2),C'C'          TURN ON   COMMISSIONABLE                     
         LA    R6,1(,R6)                                                        
         LA    R4,23(,R4)                                                       
*                                                                               
BLOCK5   MVC   0(23,R4),SPACES                                                  
         MVC   0(2,R4),WKSCODE     WORK CODE ON   LEFT                          
         CP    WKSNON,=P'0'        ANY  NON-COMMISSIONABLE ?                    
         BE    BLOCK9              NO,  SKIP                                    
         EXTED WKSNON,(12,10(R4)),2,FLOAT=-,ALIGN=LEFT,CURSYMB=S,      X        
               LANG=PLANG                                                       
         LA    R2,10(,R4)          FIND 1ST  AVAILABLE BYTE                     
         AR    R2,R0                                                            
*                                                                               
         MVI   0(R2),C'N'          TURN ON   NON-COMMISSIONABLE                 
         LA    R6,1(,R6)                                                        
         LA    R4,23(,R4)                                                       
*                                                                               
BLOCK9   LA    R7,WKSLNQ(,R7)                                                   
         BCT   R3,BLOCK2                                                        
         STC   R6,SCOUNT           NUMBER    IN   UNSCAN    TABLE               
         MVC   ANXTSCN,AIOAREA1    SET  ADDRESS   OF   NEXT SCAN                
*                                                                               
BLOCKEX  XMOD1 ,                   RETURN TO CALLER                             
*                                                                               
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* UNSCAN CODES/AMOUNTS TO WORK CODE FIELDS                            *         
***********************************************************************         
         SPACE 1                                                                
         USING PARAD,R5                                                         
         SPACE 1                                                                
UNSCAN   NMOD1 0,**UNSC**                                                       
         L     RC,0(,R1)                                                        
         XC    PARWK,PARWK                                                      
         MVI   PARWK,(L'PARAWRK+7) DUMMY     HEADER                             
         ZIC   R3,SCOUNT                                                        
         LTR   R3,R3               ANY  MORE CODES ?                            
         BZ    UNSCANX             NO,  RETURN                                  
         MVI   DMCB+8,COMMA        CHANGE    C',='     DELIMITERS               
         MVI   DMCB+9,EQUALSGN     BASED     ON        PLANG                    
         MVC   DMCB+10(1),WCCOMMA                                               
         MVI   DMCB+11,EQUALSGN                                                 
         GOTO1 VUNSCAN,DMCB,((R3),ANXTSCN),(13,PARWK),,(14,=C'$LT')             
         MVC   SCOUNT,DMCB         NUMBER    REMAINING                          
         MVC   ANXTSCN,DMCB        ADDRESS   OF   NEXT                          
         CLI   SCOUNT,0            ANY  MORE CODES                              
         BE    UNSCANX             NO,  RETURN                                  
         LA    R1,L'PARAWRK        FIND 1ST  AVAILABLE BYTE                     
         LA    R2,PARWK+8          FIND LAST                                    
         BAS   RE,FINDFREE                                                      
*                                                                               
UNSCAN4  MVC   0(1,R2),WCCOMMA     ADD  TRAILING  COMMA                         
*                                                                               
UNSCANX  MVC   PARAWRK,PARWK+8                                                  
         OC    PARAWRK,SPACES                                                   
         XMOD1 ,                   RETURN    TO   CALLER                        
*                                                                               
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* FIND FIRST AVAILABLE (BYTE)                                         *         
*                                                                     *         
*   INPUT:                                                            *         
*     R1 = NUMBER  OF BYTES TO CONSIDER                               *         
*     R2 = ADDRESS OF FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
FINDFREE AR    R2,R1               FIND LAST BYTE IN   FIELD                    
         BCTR  R2,0                                                             
*                                                                               
FINDFR10 CLI   0(R2),C' '          FIND LAST CHARACTER                          
         BH    FINDFR20                                                         
         BCTR  R2,0                                                             
         BCT   R1,FINDFR10                                                      
*                                                                               
FINDFR20 LA    R2,1(,R2)           1ST  FREE BYTE                               
         BSM   0,RE                RETURN    TO   CALLER                        
         EJECT ,                                                                
***********************************************************************         
* DSECT TO COVER ENTRY IN TABLE OF CHARGES                            *         
***********************************************************************         
         SPACE 1                                                                
ABLD     DSECT                                                                  
ABLTYP   DS    CL1                 TYPE 0 DIRECT LABOR                          
*                                       1 DIRECT COSTS                          
ABLCDE   DS    CL2                 WORK-CODE                                    
ABLCODE  DS    CL1                 GST  CODE                                    
ABLRATE  DS    XL2                 GST  RATE                                    
ABLPCODE DS    CL1                 PST  CODE                                    
ABLPRATE DS    XL2                 PST  RATE                                    
ABLPRV   DS    CL2                 PROVINCE                                     
         DS    CL1                 SPARE (FOR ALIGNMENT)                        
*                                                                               
ABLBUCK  DS    0PL(PLAMTLNQ)                                                    
ABLNET   DS    PL8                 NET  AMOUNT                                  
ABLCOM   DS    PL8                 COMMISSION                                   
ABLNON   DS    PL8                 NON-COMMISSIONABLE                           
ABLCSD   DS    PL8                 CASH DISCOUNT                                
ABLGST   DS    PL8                 GST  AMOUNT                                  
ABLPST   DS    PL8                 PST  AMOUNT                                  
ABLBUCK# EQU   (*-ABLBUCK)/PLAMTLNQ                                             
*                                                                               
ABLRTE   DS    BL4                 HOURLY RATE (IF LABOR)                       
ABLHRS   DS    BL4                 HOURS (IF LABOR)                             
ABLCNTR  DS    CL25                CONTRA ACCOUNT NAME                          
ABLNQ    EQU   *-ABLD                                                           
*                                                                               
MAXCNT   EQU   100                 MAXIMUM NUMBER OF ENTRIES                    
         EJECT ,                                                                
***********************************************************************         
* DSECT TO COVER ENTRY IN TABLE OF PARAGRAPH CHARGES                  *         
***********************************************************************         
         SPACE 1                                                                
WKSD     DSECT                                                                  
WKSCODE  DS    CL2                 WORK CODE                                    
         DS    CL2                 SPARE                                        
WKSNET   DS    PL8                 NET  COMMISSIONABLE                          
WKSNON   DS    PL8                 NON  COMMISSIONABLE                          
WKSN#    EQU   (*-WKSNET)/PLAMTLNQ                                              
WKSLNQ   EQU   *-WKSD                                                           
*                                                                               
WKSMAX   EQU   50                  MAX IN TABLE                                 
         EJECT ,                                                                
***********************************************************************         
* DSECT TO COVER ENTRY IN HOUR SUMMARY TABLE                          *         
***********************************************************************         
         SPACE 1                                                                
HOURSUM  DSECT                                                                  
HRSCNTR  DS    CL25                CONTRA NAME                                  
HRSRATE  DS    PL8                 RATE                                         
HRSHOURS DS    PL8                 HOURS                                        
HRSNET   DS    PL8                 NET                                          
HRSNON   DS    PL8                 NON-COMMISION                                
HRSCOM   DS    PL8                 COMMISSION                                   
*                                                                               
HRSLNQ   EQU   *-HOURSUM           LENGTH                                       
*                                                                               
HRSMAX   EQU   150                                                              
         EJECT ,                                                                
***********************************************************************         
* DSECT TO COVER 70 CHARACTER LINE FORMATTING                         *         
*                                                                     *         
*   NOTES:                                                            *         
*        L =   JUSTIFIED LEFT                                         *         
*        R =   JUSTIFIED RIGHT                                        *         
*        - =   POSSIBLE  MINUS SIGN ON RIGHT                          *         
***********************************************************************         
         SPACE 1                                                                
LINED    DSECT                                                                  
*                                        L                                      
*                                        /                                      
*                                  COLS  R - DESCRIPTION                        
*                                  ----- - - --------------------------         
LINSE1   DS    0CL3                00-02 L   S=1                                
*                                                                               
LINBLANK DS    CL1                           GENERAL   START                    
LINNARR  DS    0CL50               01-50 L   NARRATIVE                          
LINPDSC  DS    0CL25               01-25 L   PARAGRAPH DESCRIPTION              
LINCNTRA DS    CL25                01-25 L   CONTRA    NAME                     
LINBLKS1 DS    CL1                 26        SPACE                              
LINPHDR  DS    CL25                27-51 L   PARAGRAPH HEADER                   
*                                                                               
         ORG   LINSE1+1                      FORMATS:  ALL                      
LINWCDSC DS    CL15                01-15 L   WORK-CODE DESCRIPTION              
         DS    CL3                 16-18     SPACES                             
*                                                                               
*                                            FORMAT    1,9                      
LINBLKS2 DS    CL6                 19-24     SPACES                             
LINRATE1 DS    CL9                 25-33 R   RATE                               
         DS    CL8                 34-41     SPACES                             
LINHRS1  DS    CL7                 42-48 R - HOURS                              
*                                                                               
         ORG   LINBLKS2                      FORMAT    2,5,6                    
LINTOTC1 DS    CL6                 19-24 R   TOTAL     CONSTANT                 
         DS    CL1                 25-25     SPACE                              
LINHRS2  DS    CL9                 26-34 R - HOURS                              
         DS    CL4                 35-38     SPACES                             
LINRATE2 DS    CL9                 39-47 R   RATE                               
*                                                                               
         ORG   LINBLKS1                      FORMAT    4,5,9                    
LINREFID DS    CL6                 26-31 L   REFERENCE ID                       
         DS    CL10                32-41     SPACES                             
LINDATE  DS    CL8                 42-49 L   DATE                               
         ORG   LINCNTRA+19                                                      
         DS    CL1                 SPACES                                       
LINREFL  DS    CL20                                                             
         DS    CL1                 SPACES                                       
LINDATL  DS    CL8                                                              
*                                                                               
         ORG   ,                             FORMATS:  ALL                      
         DS    CL2                 53-54     SPACES                             
LINAMT   DS    CL13                55-67 R - AMOUNT                             
         DS    CL2                 68-69     SPACES                             
*                                                                               
LINLNQ   EQU   *-LINED                       MUST BE   70 (LINELNQ)             
         EJECT ,                                                                
***********************************************************************         
* LOCAL W/S                                                           *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
PK2D     DS    2D                                                               
PACK1    DS    D                                                                
PACK2    DS    D                                                                
PACK3    DS    D                                                                
DTLINDUB DS    D                   DTLINE DUB SAVE AREA                         
*                                                                               
MYRELO   DS    A                                                                
*                                                                               
ATABLE   DS    A                                                                
AWKSTBLE DS    A                                                                
APARLN   DS    A                                                                
AIOAREA4 DS    A                                                                
ANXTSCN  DS    A                   ADDRESS OF NEXT UNSCAN ENTRY                 
ANXTCOM  DS    A                   ADDRESS OF COMMENT ELEMENT                   
ONERUL   DS    A                   ADDRESS OF ONE ENTRY IN ABL TABLE            
*                                                                               
SAVRE    DS    F                   SAVE AREA FOR RE                             
*                                                                               
NEXTP    DS    F                   NEXT PARLN                                   
*                                                                               
SAVEHRS  DS    F                   SAVE HOURS                                   
*                                                                               
MAXPARH  DS    H                   MAXIMUM NUMBER OF LINES PER PARA             
MAXPAR   EQU   100                                                              
*                                                                               
TOTAMT   DS    PL(PLAMTLNQ)        PARAGRAPH TOTALS - NET                       
TOTCOMM  DS    PL(PLAMTLNQ)                         - COMMISSION                
TOTNON   DS    PL(PLAMTLNQ)                         - NON                       
TOTCSD   DS    PL(PLAMTLNQ)                         - C.D.                      
TOTGST   DS    PL(PLAMTLNQ)                         - GST                       
TOTPST   DS    PL(PLAMTLNQ)                         - PST                       
TOTAMT#  EQU   (*-TOTAMT)/PLAMTLNQ PARAGRAPH TOTALS NUMBER                      
*                                                                               
JOBNET   DS    PL(PLAMTLNQ)              JOB TOTALS - NET                       
JOBCOM   DS    PL(PLAMTLNQ)                         - COMMISSION                
JOBNON   DS    PL(PLAMTLNQ)                         - NON                       
JOBCSD   DS    PL(PLAMTLNQ)                         - C.D.                      
JOBGST   DS    PL(PLAMTLNQ)                         - GST                       
JOBPST   DS    PL(PLAMTLNQ)                         - PST                       
JOBAMT#  EQU   (*-JOBNET)/PLAMTLNQ       JOB TOTALS LENGTH                      
*                                                                               
ABLWRK   DS    CL(ABLNQ)                                                        
*                                                                               
WKSCNT   DS    CL1                 NUMBER IN WKSTABLE                           
SCOUNT   DS    CL1                 NUMBER IN UNSCAN TABLE                       
PCNT     DS    CL1                 NUMBER OF LINES IN CURRENT RECORD            
CONTIN   DS    CL1                 CONTINUATION                                 
*                                                                               
LABRSW   DS    CL1                                                              
NEXTPARA DS    CL1                                                              
NEXTKEY  DS    CL42                                                             
WKCLAST  DS    CL2                                                              
CONTRA   DS    CL50                                                             
TCNT     DS    CL1                 NUMBER IN TABLE SO FAR                       
HRCNT    DS    CL1                 NUMBER IN HOUR TABLE                         
RTCNT    DS    CL1                 NUMBER IN RATE/HOUR TABLE                    
BILGOWRK DS    CL100                                                            
PARWK    DS    CL100               WORK-CODES                                   
PARDSC   DS    CL36                DESCRIPTION                                  
PARCNT   DS    CL1                 NUMBER OF WORKLINES                          
PARHEADS DS    CL1                 NUMBER OF HEADLINES                          
PARHED1  DS    CL70                HEADLINE 1                                   
PARHED2  DS    CL70                HEADLINE 2                                   
PARXTRA  DS    CL1                 X'80' PRECEDE WITH 'DIRECT COSTS'            
*                                                                               
DTLINEH  DS    CL1                 DETAIL LINE HOOK                             
WKLINEH  DS    CL1                 WORK CODE TOTAL HOOK                         
HRSUMH   DS    CL1                 Y=HOURS SUMMARY HOOK                         
*                                  X=EXCLUDE HOURS                              
RTSUMH   DS    C                   Y=SUMMARY BY RATE/CONTRA                     
CHOPWRK  DS    CL250                                                            
*                                                                               
         SPACE 2                                                                
         DS    0F                                                               
TABLE    DS    CL(ABLNQ*MAXCNT)    TABLE OF ALLOCATED CHARGES                   
*                                                                               
         DS    0F                                                               
WKSTBLE  DS    CL(WKSLNQ*WKSMAX)   TABLE OF PARAGRAPH CHARGES                   
*                                                                               
PARLN    DS    0CL70               WORK LINES                                   
LINELNQ  EQU   L'PARLN             LINE LENGTH                                  
         DS    CL(MAXPAR*LINELNQ)                                               
*                                                                               
IOAREA4  DS    CL(HRSMAX*HRSLNQ)   EXPANDED FOR HOURS SUMMARY                   
         ORG   IOAREA4                                                          
         DS    CL2048              SPACE FOR PARAGRAPH RECORD                   
         ORG   ,                                                                
*                                                                               
LWSX     DS    0C                                                               
         EJECT ,                                                                
         SPACE 1                                                                
* ACBILDSECT                                                                    
       ++INCLUDE ACBILDSECT                                                     
* ACBILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBILWORK                                                      
         PRINT ON                                                               
         EJECT ,                                                                
         SPACE 1                                                                
* ACBILFDD                                                                      
       ++INCLUDE ACBILFDD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACBIL07   02/23/15'                                      
         END                                                                    
