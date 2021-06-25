*          DATA SET PRSFM36    AT LEVEL 212 AS OF 07/08/13                      
*PHASE T41C36A                                                                  
*INCLUDE SRCHCALL                                                               
*        TITLE 'PRSFM36 PURCHASE ORDER NUMBER (PO#) RECORDS'                    
         TITLE 'PRSFM36 PURCHASE ORDER NUMBER (PO#) RECORDS'                    
***********************************************************************         
*                                                                     *         
*         CHANGE LOG                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
* SMYE  11/06    BIG BANG                                                       
*                                                                               
         TITLE 'PRSFM36 PURCHASE ORDER NUMBER (PO#) RECORDS - INIT'             
***********************************************************************         
*                                                                     *         
*        INITIALISATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T41C36   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C36,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         L     R7,=A(SUBROUTS)     SET UP ADDRESSABILITY TO SUBROUTINES         
         A     R7,RELO             RELOCATE ADDRESS                             
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
*===>                                                                           
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*===>                                                                           
         MVI   IPSTAT,0            INIT INPUT STATISTICS                        
         MVI   SAVMSGNO,0          INIT MESSAGE NUMBER SAVEAREA                 
         MVI   ERROR,0             INIT MESSAGE NUMBER                          
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENT                          
*                                                                               
         OI    GENSTAT4,NODELLST   DELETE FROM LIST NOT ALLOWED                 
         OI    GENSTAT2,DISTHSPG   STAY ON SAME PAGE OF LIST                    
*                                                                               
         LA    RF,PAGE#Q           NUMBER OF ENTRIES ON A half PAGE             
*                                                                               
         GOTO1 DATCON,DUB,(5,0),(3,DATENOW) SET TODAY'S DATE                    
*                                                                               
         BRAS  RE,PIDGET           FIND AND SAVE PID IN SVPID                   
*                                                                               
         TITLE 'PRSFM36 PURCHASE ORDER NUMBER (PO#) RECORDS - ANAL'             
***********************************************************************         
*                                                                     *         
*        ANALIZE CALLING MODE                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ANAL     DS    0H                                                               
*                                                                               
         CLI   MODE,PROCPFK        PROCESS PF KEYS                              
         BNE   ANAL0                                                            
*                                                                               
         CLI   ACTNUM,ACTSEL       SKIP IF NOT SELECT FROM LIST                 
         BNE   ANAL0                                                            
*                                                                               
         CLI   PFAID,12            IF PFKEY 12 HIT                              
         BE    *+8                                                              
         CLI   PFAID,24            IF PFKEY 24 HIT                              
         BNE   *+16                                                             
         OI    GENSTAT2,NEXTSEL          GO TO NEXT SELECT                      
         NI    GENSTAT2,X'FF'-RETEQSEL   GO BACK TO LIST SCREEN                 
         B     ANALX                                                            
*                                                                               
ANAL0    DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   *+12                                                             
         BRAS  RE,VK                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BRAS  RE,VR                                                            
         B     ANAL90                                                           
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     ANAL90                                                           
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   *+12                                                             
         BRAS  RE,PR                                                            
         B     ANALX                                                            
*                                                                               
         B     ANALX                                                            
*                                                                               
ANAL90   DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTSEL        SKIP IF NOT SELECT FROM LIST                
         BNE   ANALX                                                            
*                                                                               
         CLI   PFAID,0              IF NO PF KEY HIT                            
         BNZ   ANAL95                                                           
*                                                                               
         BRAS  RE,TSTNTRY             TEST IF ANY FIELD ENTERED                 
         BZ    *+12                   NO                                        
         OI    GENSTAT2,RETEQSEL      YES RETURN TO THIS SCREEN                 
         B     ANAL95                                                           
*                                                                               
         OI    GENSTAT2,NEXTSEL          GO TO NEXT SELECT                      
*                                                                               
ANAL95   DS    0H                                                               
*                                                                               
ANALX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM36 PURCHASE ORDER NUMBER (PO#) RECORDS - VK'               
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
VK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      DIFFERENT IF LIST                            
         BE    VKL                                                              
         CLI   ACTNUM,ACTREP       OR REPORT                                    
         BE    VKL                                                              
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
VKMED    DS    0H                                                               
*                                                                               
         MVI   USEIONUM,2          VALIDATE USING IOA2                          
*                                                                               
         MVC   SCRMEDN,SPACES      INIT                                         
         MVC   MEDNM,SPACES        INIT                                         
         OI    SCRMEDNH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
         LA    R2,SCRMEDH         VALIDATE MEDIA                                
*                                                                               
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   SCRMEDN(L'MEDNM),MEDNM  DISPLAY MEDIA NAME                       
*                                                                               
VKMEDX   DS    0H                                                               
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
VKCLT    DS    0H                                                               
*                                                                               
         LA    R2,SCRCLTH          CLIENT                                       
         MVC   CLTNM,SPACES                                                     
*                                                                               
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    VKERR                                                            
*                                                                               
         GOTO1 VALICLT             VALIDATE CLIENT ENTRY                        
*                                                                               
         MVC   SCRCLTN(L'CLTNM),CLTNM  DISPLAY CLT NAME                         
         OI    SCRCLTNH+6,X'80'                                                 
*                                                                               
         L     R4,AIO2             GET PO PROFILE                               
         USING PCLTRECD,R4                                                      
*                                                                               
         XC    SVPROF,SVPROF       CLEAR PROFILE AREA                           
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,C'P'           SET SYSTEM                                   
         MVC   WORK+2(2),=C'PO'    SET PROFILE ID                               
         MVC   WORK+4(3),PCLTKAGY  AGY/MED                                      
         MVC   WORK+7(3),PCLTKCLT                                               
*                                                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         XC    SVCLTPO,SVCLTPO                                                  
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,PCLTPOEQ     FIND PURCHASE ORDER ELM                      
         BRAS  RE,GETEL                                                         
         BNE   VKCLTX                                                           
*                                                                               
         USING PCLTPOEL,R6         ESTABLISH PURCHASE ORDER ELM                 
*                                                                               
         LLC   RF,PCLTPOLN         SAVE PURCHASE ORDER ELM                      
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVCLTPO(0),PCLTPOEL                                              
*                                                                               
         MVC   SCRPOTL,=CL12'PO#'  SET DEFAULT PO# TITLE                        
*                                                                               
         TM    PCLTPOF1,P_POLOVQ   IF THERE IS PO LABEL OVERRIDE                
         BNO   *+10                                                             
         MVC   SCRPOTL(L'PCLTPONM),PCLTPONM  DISPLAY PO TITLE                   
*                                                                               
         OI    SCRPOTLH+6,X'80'    FORCE REWRITE                                
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
VKCLTX   DS    0H                                                               
*                                                                               
*        VALIDATE PRODUCT                                                       
*                                                                               
VKPRD    DS    0H                                                               
*                                                                               
         XC    QPRD,QPRD           INIT SAVEAREA                                
*                                                                               
         LA    R2,SCRPRDH          PRODUCT                                      
         MVC   PRDNM,SPACES                                                     
*                                                                               
         MVI   ERROR,MISSING                                                    
*                                                                               
         LA    R6,SVCLTPO          POINT TO SAVED CLT PO ELM                    
         USING PCLTPOEL,R6         ESTABLISH CLIENT PO ELEMENT                  
*                                                                               
         CLI   PCLTPOLV,P_POLVCQ   IF CLIENT LEVEL PO'S                         
         BNE   VKPRD10                                                          
*                                                                               
         CLI   5(R2),0                OKAY IF NO PRODUCT                        
         BE    VKPRD30                                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         B     VKERR                                                            
*                                                                               
VKPRD10  DS    0H                                                               
*                                                                               
         CLI   5(R2),0             ELSE MUST HAVE PRODUCT                       
         BE    VKERR                                                            
*                                                                               
VKPRD20  DS    0H                                                               
*                                                                               
         CLC   =C'ZZZ',8(R2)       PRODUCT CODE ZZZ ?                           
         BE    VKZZZERR            YES - INVALID                                
*                                                                               
         GOTO1 VALIPRD             VALIDATE PRODUCT ENTRY                       
*                                                                               
VKPRD30  DS    0H                                                               
*                                                                               
         MVC   SCRPRDN(L'PRDNM),PRDNM  DISPLAY PRD NAME                         
         OI    SCRPRDNH+6,X'80'    RE-TRANSMIT                                  
*                                                                               
VKPRDX   DS    0H                                                               
*                                                                               
*        VALIDATE ESTIMATE                                                      
*                                                                               
VKEST    DS    0H                                                               
*                                                                               
         XC    QEST,QEST           INIT SAVEAREA                                
         XC    BEST,BEST                                                        
*                                                                               
         LA    R2,SCRESTH          ESTIMATE                                     
         MVC   ESTNM,SPACES                                                     
*                                                                               
         MVI   ERROR,MISSING                                                    
*                                                                               
         CLI   PCLTPOLV,P_POLVCQ   IF CLIENT  LEVEL PO'S                        
         BE    *+8                                                              
         CLI   PCLTPOLV,P_POLVPQ   OR PRODUCT LEVEL PO'S                        
         BNE   VKEST10                                                          
*                                                                               
         CLI   5(R2),0             OK IF NO ESTIMATE ALLOWED                    
         BE    VKEST30                                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         B     VKERR                                                            
*                                                                               
VKEST10  DS    0H                                                               
*                                                                               
         CLI   5(R2),0             ELSE MUST HAVE ESTIMATE                      
         BE    VKERR                                                            
*                                                                               
         GOTO1 VALIEST             VALIDATE ESTIMATE ENTRY                      
*                                                                               
         L     R6,AIO2                                                          
         USING PESTRECD,R6                                                      
*                                  CHECK FOR ACCEPTABLE ESTIMATE                
         CLI   PESTELEM,X'07'      ESTIMATE ELEMENT CODE                        
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         MVI   ERROR,0                                                          
         TM    PESTTEST,X'40'      STEWARDSHIP ESTIMATE ?                       
         BO    VKESTERR            YES - CANNOT USE                             
         CLI   ACTNUM,ACTADD       ACTION ADD ?                                 
         BNE   VKEST20             NO                                           
         CLI   PESTSTAT,C'2'       PERMANENT LOCKOUT ?                          
         BE    VKESTERR            YES - CANNOT USE                             
*                                                                               
         DROP  R6                                                               
*                                                                               
VKEST20  DS    0H                                                               
*                                                                               
*              SAVE ESTIMATE START AND END DATES IN YMD BINARY FORMAT           
         GOTOR DATCON,DMCB,(0,ESTDTST),(3,ESTSTRT)                              
         GOTOR DATCON,DMCB,(0,ESTDTEND),(3,ESTEND)                              
*                                                                               
VKEST30  DS    0H                                                               
*                                                                               
         MVC   SCRESTN(L'ESTNM),ESTNM  DISPLAY EST NAME                         
         OI    SCRESTNH+6,X'80'    RE-TRANSMIT                                  
*                                                                               
VKESTX   DS    0H                                                               
*                                                                               
*        BUILD STARTING KEY                                                     
*                                                                               
VKKEY    DS    0H                                                               
*                                                                               
         MVI   USEIONUM,1          USE IOAREA 1 FOR VALIDATION                  
         MVC   AIO,AIO1            USE AIO1                                     
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING PPO#RECD,R4         ESTABLISH PO# KEY                            
*                                                                               
         MVC   PPO#KAGY,AGENCY     AGENCY                                       
         MVC   PPO#KMED,QMED       MEDIA                                        
         MVI   PPO#KRCD,PPO#KIDQ   RECORD TYPE                                  
         MVC   PPO#KCLT,QCLT       CLIENT                                       
         MVC   PPO#KPRD,QPRD       PRODUCT                                      
         MVC   PPO#KEST,BEST       ESTIMATE                                     
*                                                                               
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
*                                                                               
         B     VKX                                                              
*                                                                               
         TITLE 'PRSFM36 PURCHASE ORDER NUMBER (PO#) RECORDS - VKL'              
***********************************************************************         
*                                                                     *         
*        VALIDATE LIST KEY                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKL      DS    0H                                                               
*                                                                               
         MVI   USEIONUM,2          USE IOAREA 2 FOR VALIDATION                  
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
VKLMED   DS    0H                                                               
*                                                                               
         MVC   P#LMEDN,SPACES      INIT                                         
         MVC   MEDNM,SPACES                                                     
         OI    P#LMEDNH+6,X'80'    FORCE RE-TRANSMISSION                        
*                                                                               
         LA    R2,P#LMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   P#LMEDN(L'MEDNM),MEDNM   DISPLAY MEDIA NAME                      
*                                                                               
VKLMEDX  DS    0H                                                               
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
VKLCLT   DS    0H                                                               
*                                                                               
         XC    QCLT,QCLT           FOR LISTING CLEAR QCLT                       
         XC    P#LCLTN,P#LCLTN                                                  
         OI    P#LCLTNH+6,X'80'                                                 
*                                                                               
         LA    R2,P#LCLTH          CLIENT                                       
*                                                                               
         CLI   5(R2),0             NO ENTRY - NO FILTERING                      
         BE    VKLCLTX             DONE WITH CLIENT                             
*                                                                               
         GOTO1 VALICLT             VALIDATE CLIENT                              
*                                                                               
         MVC   P#LCLTN,CLTNM                                                    
         OI    P#LCLTNH+6,X'80'                                                 
*                                                                               
VKLCLTX  DS    0H                                                               
*                                                                               
*        VALIDATE PRODUCT                                                       
*                                                                               
VKLPRD   DS    0H                                                               
*                                                                               
         XC    QPRD,QPRD           FOR LISTING CLEAR QPRD                       
         XC    P#LPRDN,P#LPRDN                                                  
         OI    P#LPRDNH+6,X'80'                                                 
*                                                                               
         LA    R2,P#LPRDH          POINT TO PRODUCT FIELD                       
         CLI   5(R2),0             NO ENTRY EQUIVALENT TO ALL PRODUCTS          
         BE    VKLPRDX             SO DONE                                      
*                                                                               
         GOTO1 VALIPRD             VALIDATE PRODUCT                             
*                                                                               
         MVC   P#LPRDN,PRDNM                                                    
         OI    P#LPRDNH+6,X'80'                                                 
*                                                                               
VKLPRDX  DS    0H                                                               
*                                                                               
*        VALIDATE ESTIMATE                                                      
*                                                                               
VKLEST   DS    0H                                                               
*                                                                               
         XC    BEST,BEST           FOR LISTING CLEAR BEST                       
         XC    P#LESTN,P#LESTN                                                  
         OI    P#LESTNH+6,X'80'                                                 
*                                                                               
         LA    R2,P#LESTH          POINT TO ESTIMATE FIELD                      
         CLI   5(R2),0             NO ENTRY EQUIVALENT TO ALL ESTIMATES         
         BE    VKLESTX             SO DONE                                      
*                                                                               
         GOTO1 VALIEST             VALIDATE ESTIMATE                            
*                                                                               
         MVC   P#LESTN,ESTNM                                                    
         OI    P#LESTNH+6,X'80'                                                 
*                                                                               
VKLESTX  DS    0H                                                               
*                                                                               
         MVI   USEIONUM,1          BACK TO IOAREA 1                             
         MVC   AIO,AIO1            USE AIO1                                     
*                                                                               
VKX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
NEEDCLT  EQU   85             SPECIFIC CLIENT ENTRY REQUIRED (SECURITY)         
USDINBY  EQU   295            CANNOT CHANGE - USED IN BUY                       
DTOVRLAP EQU   296            OVERLAPPING DATES NOT ALLOWED                     
NOCUTBK  EQU   297            CANNOT CUT BACK DATES - USED IN BUY               
USDNODEL EQU   298            CANNOT DELETE - USED IN BUY                       
INVESTPO EQU   299            ESTIMATE CAN'T BE USED FOR PURCHASE ORDER         
ACTVNG   EQU   300            CANNOT "RE-ACTIVATE" IF PO# USED IN BUY           
INVESTZZ EQU   303            PRODUCT CODE ZZZ ESTIMATES NOT ALLOWED            
EFFDTEER EQU   322            PO# PERIOD STARTS BEFORE EFFECTIVE DATE           
*                                                                               
VKCLTER  DS    0H                                                               
         MVI   ERROR,NEEDCLT       SECURITY - CLIENT REQUIRED                   
         B     VKERR                                                            
*                                                                               
VKERR    DS    0H                                                               
         GOTOR ERREX                                                            
*                                                                               
*                                                                               
VKESTERR DS    0H                  ESTIMATE NG FOR PO#S                         
         MVI   ERROR,0             OLD STYLE MESSAGE                            
         LHI   RF,INVESTPO         SET ERROR CODE                               
         B     VKERR2                                                           
*                                                                               
VKZZZERR DS    0H                  PRODUCT CODE ZZZ ESTIMATES NG                
         MVI   ERROR,0             OLD STYLE MESSAGE                            
         LHI   RF,INVESTZZ         SET ERROR CODE                               
         B     VKERR2                                                           
*                                                                               
VKERR2   DS    0H                                                               
         STCM  RF,3,ERROR2CD       SET ERROR EQUATE                             
*                                                                               
VKERRX   DS    0H                                                               
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO ERROR2CD IF REQUIRED                     
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
*                                                                               
         CLI   ERROR,0             IF OLD STYLE MESSAGE NUMBER                  
         BE    *+10                                                             
         MVC   ERROR2CD+1(1),ERROR      PUT IN NEW STYLE                        
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         MVC   GTMSGNO,ERROR2CD    MESSAGE NUMBER                               
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM36 PURCHASE ORDER NUMBER (PO#) RECORDS - DK'               
***********************************************************************         
*                                                                     *         
*        DISPLAY KEY                                                  *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
DK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
*        SET PAGE NUMBER TO 1 ON NEW KEY                                        
*                                                                               
DKPAGE   DS    0H                                                               
*                                                                               
         CLC   SVPO#KEY(L'PPO#KEY),KEY IF NEW PO# RECORD                        
         BE    DKPAGE9                                                          
*                                                                               
         MVC   SVPO#KEY,KEY        SAVE NEW KEY                                 
*                                                                               
         ZAP   SVPAGE,=P'1'           DEFAULT TO ONE                            
*                                                                               
         FOUT  SCRPAGEH,SPACES,3   INIT PAGE FIELD                              
*                                                                               
         EDIT  (P8,SVPAGE),SCRPAGE,0,ALIGN=LEFT                                 
*                                                                               
         MVI   SCRPAGEH+5,1        SET LENGTH                                   
*                                                                               
DKPAGE9  DS    0H                                                               
*                                                                               
         OI    SCRPAGEH+4,X'80'    SET AS ENTERED THIS TIME                     
*                                  ACTS LIKE A FIRST TIME SEITCH                
*                                                                               
DKPAGEX  DS    0H                                                               
*                                                                               
         L     R4,AIO              ESTABLISH PO# RECORD                         
         USING PPO#RECD,R4                                                      
*                                                                               
*        DISPLAY MEDIA                                                          
*                                                                               
DKMED    DS    0H                                                               
*                                                                               
         XC    SCRMED,SCRMED                                                    
         MVC   SCRMED(L'PPO#KMED),PPO#KMED   MEDIA                              
         MVI   SCRMEDH+5,L'PPO#KMED          SET INPUT LENGTH                   
         OI    SCRMEDH+6,FOUTTRN             TRANSMIT                           
*                                                                               
DKMEDX   DS    0H                                                               
*                                                                               
*        DISPLAY CLIENT                                                         
*                                                                               
DKCLT    DS    0H                                                               
*                                                                               
         XC    SCRCLT,SCRCLT                                                    
         MVC   SCRCLT(L'PPO#KCLT),PPO#KCLT      CLIENT                          
         MVI   SCRCLTH+5,L'PPO#KCLT SET INPUT LENGTH                            
*                                                                               
         CLI   SCRCLT+2,C' '       IF 2 CHARACTER CLIENT                        
         BH    *+8                                                              
         MVI   SCRCLTH+5,2            ADJUST INPUT LENGTH                       
*                                                                               
         OI    SCRCLTH+6,FOUTTRN   TRANSMIT                                     
*                                                                               
DKCLTX   DS    0H                                                               
*                                                                               
*        DISPLAY PRODUCT                                                        
*                                                                               
DKPRD    DS    0H                                                               
*                                                                               
         XC    SCRPRD,SCRPRD                                                    
         MVI   SCRPRDH+5,0         DEFAULT TO NO INPUT                          
*                                                                               
         OC    PPO#KPRD,PPO#KPRD   SKIP IF NO PRODUCT                           
         BZ    DKPRD10                                                          
*                                                                               
         MVC   SCRPRD(L'PPO#KPRD),PPO#KPRD      PRODUCT                         
         MVI   SCRPRDH+5,L'PPO#KPRD SET INPUT LENGTH                            
*                                                                               
         CLI   SCRPRD+2,C' '       IF 2 CHARACTER PRODUCT                       
         BH    *+8                                                              
         MVI   SCRPRDH+5,2            ADJUST INPUT LENGTH                       
*                                                                               
DKPRD10  DS    0H                                                               
*                                                                               
         OI    SCRPRDH+6,FOUTTRN   TRANSMIT                                     
*                                                                               
DKPRDX   DS    0H                                                               
*                                                                               
*        DISPLAY ESTIMATE                                                       
*                                                                               
DKEST    DS    0H                                                               
*                                                                               
         XC    SCREST,SCREST                                                    
         MVI   SCRESTH+5,0         DEFAULT TO NO INPUT                          
*                                                                               
         ZICM  R0,PPO#KEST,2       2-BYTE BINARY ESTIMATE                       
         CVD   R0,DUB                                                           
         BZ    DKEST10                NO EST                                    
*                                                                               
         OI    DUB+7,X'0F'                                                      
         UNPK  SCREST,DUB                                                       
*                                                                               
         MVI   SCRESTH+5,3         SET INPUT LENGTH                             
*                                                                               
DKEST10  DS    0H                                                               
*                                                                               
         OI    SCRESTH+6,FOUTTRN   TRANSMIT                                     
*                                                                               
DKESTX   DS    0H                                                               
*                                                                               
         BRAS  RE,VK               VALIDATE KEY                                 
*                                                                               
DKX      DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM36 PURCHASE ORDER NUMBER (PO#) RECORDS - VR'               
***********************************************************************         
*                 VALIDATE RECORD                                     *         
***********************************************************************         
*      *** NOTE ***        *** NOTE ***        *** NOTE ***           *         
*   ACTUAL UPDATING OF RECORD DONE IN ROUTINES BEGINNING AT URBEGIN   *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         XC    SVACHGS,SVACHGS     INIT CHANGE INDICATORS                       
         XC    SVMAXID,SVMAXID     INIT MAXIMUM INTERNAL ID                     
*                                                                               
         MVI   ELEMCNT,0           INIT DETAIL ELEMENT COUNTER                  
*                                                                               
         L     RE,AIO2         CLEAR FIRST 4000 BYTES OF AIO2 WHICH             
         ST    RE,SORTNXT    WILL BE USED FOR SORTING DETAIL ELEMENTS           
         LA    RF,4000          PRIOR TO UPDATING OF RECORD                     
         XCEFL                                                                  
*                                                                               
         SR    R0,R0               INIT COUNTER                                 
         L     R3,AIO2             START OF SORT TABLE                          
*                                                                               
         CLI   ACTNUM,ACTADD       RECORD ACTION ADD ?                          
         BE    VRSRTDN             YES - MAX INTERNAL ID IS ZERO                
*                                                                               
*        FILL SORT TABLE WITH ALL PO# ELEMENTS                                  
*                                  FIND AND SAVE MAXIMUM INTERNAL ID            
         L     R6,AIO1                                                          
         MVI   ELCODE,PO#DLIDQ     LOOK FOR DETAIL ELM                          
         BRAS  RE,GETEL            READ FIRST DETAIL ELM                        
*                                                                               
VRSRTLP  DS    0H                                                               
*                                                                               
         BNE   VRSRTDN             END OF RECORD                                
*                                                                               
         USING PO#DELMD,R6                                                      
*                                                                               
         LLC   RF,PO#DLEN          GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),PO#DELM     ADD ELM TO SORT TABLE                        
*                                                                               
         CLC   SVMAXID,PO#DID      RECORD ID HIGH ?                             
         BH    *+10                NO                                           
         MVC   SVMAXID,PO#DID      SAVE NEW MAX ID                              
*                                                                               
         LA    R3,SORTLNTH(R3)     BUMP SORT TABLE POINTER                      
         AHI   R0,1                BUMP TABLE ENTRY COUNTER                     
*                                                                               
VRSRTCN  DS    0H                                                               
         BRAS  RE,NEXTEL           FIND NEXT PO# ELM                            
         B     VRSRTLP                                                          
*                                                                               
VRSRTDN  DS    0H                                                               
*                                                                               
         ST    R3,SORTNXT          SAVE A(NEXT SLOT IN TABLE)                   
         STC   R0,ELEMCNT          BUMP ELEMENT COUNTER                         
*                                                                               
         DROP  R6                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*        VALIDATE DETAIL INPUT                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRBEGIN  DS    0H                                                               
*                                                                               
         LA    R2,SCRACT1H         POINT TO FIRST FIELD ON LINE                 
         USING FLDHDRD,R2          ESTABLISH INPUT FIELD                        
*                                                                               
         LA    RE,SCRACTLH         POINT TO FINAL ACTION FIELD                  
         ST    RE,SCRNEND          SAVE ADDRESS FOR END-OF-SCREEN TEST          
*                                                                               
VRLINLP  DS    0H                                                               
*                                                                               
         MVI   SVLNACTV,0          CLEAR ACTIVITY INDICATOR                     
*                                                                               
         L     R6,SORTNXT          BUILD NEW ELEMENT HERE                       
         STCM  R2,15,SORTROWA(R6)  SAVE A(ROW)                                  
*                                                                               
         USING PO#DELMD,R6         ESTABLISH PO# DETAIL ELM                     
*                                                                               
         MVI   PO#DELID,PO#DLIDQ   SET ELEMENT ID                               
         MVI   PO#DLEN,PO#DHDLQ    SET BASIC LENGTH                             
*                                                                               
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE ACTION (A,C,D) FIELD                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRACT    DS    0H                                                               
*                                                                               
         CLC   FLDDATA(3),=3X'00'                                               
         BE    VRACTPT                                                          
         CLI   FLDDATA,C' '                                                     
         BE    VRACTPT                                                          
*****    CLI   FLDDATA,C'A'                                                     
*****    BE    VRACTPT                                                          
*****    CLI   FLDDATA,C'C'                                                     
*****    BE    VRACTPT                                                          
         CLI   FLDDATA,C'D'                                                     
         BNE   NVALERR                                                          
*                                                                               
VRACTPT  DS    0H              FIND AND ESTABLISH "CURRENT" ELEMENT             
*                                                                               
         XC    SV#DID,SV#DID   CLEAR 2-BYTE SAVE PO#DID AREA                    
*                                                                               
         CLI   ACTNUM,ACTADD       RECORD ACTION ADD ?                          
         BE    VRACTX              YES - NO ELEMENT TO ESTABLISH                
*                                                                               
         LR    R3,R2                                                            
         AH    R3,LNDSPID      POINT R3 TO "HIDDEN" PO#DID ON SCREEN            
*                                                                               
         CLC   8(3,R3),=C'000' NEW PO# ENTRY OR UNUSED LINE ?                   
         BNH   VRADELCK        YES - CONTINUE                                   
*                                                                               
         PACK  WORK(3),8(3,R3)   CONVERT SCREEN ID TO 2-BYTE BINARY             
         ZAP   DUB,WORK(3)                                                      
         CVB   R0,DUB                                                           
         STH   R0,SV#DID                                                        
*                                                                               
*        LOOK FOR DETAIL ELEM IN SORT TABLE                                     
*                                                                               
         L     R6,AIO2             POINT TO SORT TABLE                          
*                                                                               
VRELMLP  DS    0H                                                               
*                                                                               
         OC    0(SORTLNTH,R6),0(R6)   MUST FIND ELEMENT                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SV#DID,PO#DID       ELEMENT WE WANT ?                            
         BE    VRELMFD             YES - CONTINUE                               
*                                                                               
VRELMCN  DS    0H                                                               
*                                                                               
         LA    R6,SORTLNTH(R6)     NEXT ELEMENT IN TABLE                        
         B     VRELMLP                                                          
*                                                                               
VRELMFD  DS    0H                                                               
*                                                                               
         STCM  R2,15,SORTROWA(R6)  SAVE A(ROW)                                  
*                                                                               
*   R6 NOW POINTING TO PROPER ELEM (IF SV#DID NOT NULLS)                        
*        OR TO NEW ELEMENT BUILD AREA                                           
*                                                                               
VRADELCK DS    0H                                                               
*                                                                               
         OC    SV#DID,SV#DID       ANY EXISTING PO# TO CHECK ?                  
         BZ    VRACTX              NO                                           
*                                                                               
         CLI   FLDDATA,C'D'        DELETE PO# ?                                 
         BNE   VRACTX              NO                                           
*                                                                               
         TM    PO#DACTV,PO#DUSDQ   USED IN BUY ?                                
         BO    VRDELERR            YES - CANNOT DELETE                          
*                                                                               
         MVI   PO#DELID,X'FF'      FLAG ELEMENT FOR DELETE                      
         OI    SVACH1,PO#ADELQ     SET ACTIVITY INDICATOR                       
*                                                                               
VRACTX   DS    0H                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE PURCHASE ORDER NUMBER (PO#)              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRPO#    DS    0H                                                               
*                                                                               
         XC    NOPO#,NOPO#         CLEAR "NOTHING ON LINE" FLAG                 
*                                                                               
         BAS   RE,BUMPU            BUMP TO PO# FIELD                            
*                                                                               
         ST    R2,FULL             SAVE POINTER TO PO#                          
*                                                                               
         CLI   FLDDATA,C' '        SKIP IF INPUT IN PO# FIELD                   
         BH    VRPO#TST                                                         
*                                  NO PO# ENTERED                               
         OC    SV#DID,SV#DID       ANY EXISTING PO# FOR THIS LINE ?             
         BNZ   VRPO#NG             YES - INVALID                                
*                                                                               
         BAS   RE,BUMPU            BUMP TO DATES FIELD                          
*                                                                               
         CLI   FLDDATA,C' '        IF INPUT IN DATES FIELD                      
         BH    VRPO#NG             INVALID                                      
*                                                                               
         BAS   RE,BUMPU            BUMP TO AMOUNT FIELD                         
*                                                                               
         CLI   FLDDATA,C' '        IF INPUT IN AMOUNT FIELD                     
         BH    VRPO#NG             INVALID                                      
*                                                                               
         BAS   RE,BUMPU            BUMP TO STATUS FIELD                         
*                                                                               
         CLI   FLDDATA,C' '        IF INPUT IN STATUS FIELD                     
         BH    VRPO#NG             INVALID                                      
*                                                                               
         MVI   NOPO#,C'X'          NOTHING ENTERED ON LINE                      
*                                                                               
         MVI   PO#DELID,0          CLEAR NEW ELEMENT BUILD AREA                 
         MVI   PO#DLEN,0                                                        
*                                                                               
         B     VRPO#X                                                           
*                                                                               
VRPO#TST DS    0H                  CHECK FOR CHANGE TO PO#                      
*                                                                               
         L     R2,FULL             RESTORE TO PO# FIELD                         
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         CLC   FLDILEN,SVPROF+1    LENGTH MUST EXCEED MINIMUM                   
         BL    VRPOINV                                                          
*                                                                               
         CLI   SVPROF+2,0          IF MAX LENGTH GIVEN                          
         BE    *+14                                                             
         CLC   FLDILEN,SVPROF+2       LENGTH MUST NOT EXCEED MAXIMUM            
         BH    VRPOINV                                                          
*                                                                               
         CLI   SVPROF+3,C'A'       IF PO MUST BE ALPHA                          
         BNE   *+12                                                             
         TM    FLDIIND,FINPALF        TEST FOR ALPHA INPUT                      
         BNO   VRPONTAL                                                         
*                                                                               
         CLI   SVPROF+3,C'N'       IF PO MUST BE NUMERIC                        
         BNE   *+12                                                             
         TM    FLDIIND,FINPNUM        TEST FOR NUMERIC INPUT                    
         BNO   VRPONTNM                                                         
*                                                                               
         LLC   RE,5(R2)            GET INPUT LENGTH                             
         BCTR  RE,0                PREP FOR EXECUTE                             
*                                                                               
         OC    SV#DID,SV#DID       SKIP IF NO PREVIOUS ELEMENT                  
         BNZ   *+12                                                             
         OI    SVACH1,PO#DADDQ        INDICATE ADDING NEW ELEMENT               
         B     VRPO#20                                                          
*                                                                               
         LLC   RF,PO#DLEN          GET ELEMENT LENGTH                           
         SHI   RF,PO#DHDLQ         DECREMENT BY HEADER LENGTH                   
*                                                                               
         CLM   RF,1,5(R2)          SKIP IF PO# LENGTH CHANGED                   
         BNE   VRPO#10                                                          
*                                                                               
         LR    RE,RF               COPY LENGTH                                  
*                                                                               
         BCTR  RE,0                DECREMENT FOR EXECUTE                        
         EX    RE,*+8              SKIP IF PO# UNCHANGED                        
         B     *+10                                                             
         CLC   PO#DPO#,8(R2)                                                    
         BE    VRPO#X                                                           
*                                                                               
VRPO#10  DS    0H                  NEW PO#                                      
*                                                                               
         TM    PO#DACTV,PO#DUSDQ   IF USED IN BUY                               
         BO    VRPO#NG2               ERROR                                     
*                                                                               
         OI    SVACH1,PO#ACHGQ     INDICATE PO# CHANGED                         
         OI    SVLNACTV,PO#DCHGQ   INDICATE PO# CHANGED                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    PO#DPO#(0),PO#DPO#  CLEAR OLD PO#                                
*                                                                               
VRPO#20  DS    0H                  REPLACE WITH ENTERED PO#                     
*                                                                               
         LLC   RF,5(R2)            GET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PO#DPO#(0),8(R2)    MOVE IN NEW PO#                              
*                                                                               
         LA    RF,1+PO#DHDLQ(RF)   ELEMENT NEW LENGTH                           
         STC   RF,PO#DLEN                                                       
*                                                                               
         B     VRPO#X                                                           
*                                                                               
VRPO#NG  DS    0H                                                               
         L     R2,FULL             RESTORE TO PO# FIELD                         
         B     VRMISS              FIELD MISSING                                
*                                                                               
VRPO#NG2 DS    0H                                                               
         L     R2,FULL             RESTORE TO PO# FIELD                         
         B     VRUSED              USED IN BUY - CANNOT CHANGE                  
*                                                                               
VRPO#X   DS    0H                                                               
*                                                                               
         L     R2,FULL             RESTORE TO PO# FIELD                         
*                                                                               
         TITLE 'PRSFM36 - VALIDATE PO# DATES - VRDAT'                           
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE PO# DATES                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRDAT    DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU            BUMP TO DATES FIELD                          
*                                                                               
         CLI   NOPO#,C'X'          ANYTHING ON LINE ?                           
         BE    VRDATX              NO                                           
*                                                                               
         CLI   FLDILEN,0           ANYTHING IN FLD?                             
         BE    VRMISS              NO - MISSING FIELD ERROR                     
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK             PERVAL OUTPUT AREA                           
         USING PERVALD,R4                                                       
         MVC   BYTE,FLDILEN        INPUT DATA LENGTH                            
         OI    BYTE,X'80'          PASSING TODAY'S DATE                         
*                                  PASS TODAY'S DATE                            
         GOTOR DATCON,DMCB,(3,DATENOW),(6,PVALPERD)                             
*                                                                               
         GOTOR PERVAL,DMCB,(BYTE,FLDDATA),(X'80',WORK)                          
*                                                                               
         CLI   4(R1),0                                                          
         BNE   DATERR              NO ERRORS TOLERATED                          
*                                                                               
         TM    PCLTPOF1-PCLTPOEL+SVCLTPO,P_POBILQ  IF BILLED BY PO              
         BNO   VRDAT05                                                          
         OC    PCLTPOED-PCLTPOEL+SVCLTPO,PCLTPOED-PCLTPOEL+SVCLTPO              
         BZ    VRDAT05             AND EFFECTIVE DATE KNOWN                     
         CLC   PVALBSTA,PCLTPOED-PCLTPOEL+SVCLTPO                               
         BL    VREFFDER            PO MUST START ON/AFTER EFF DATE              
*                                                                               
VRDAT05  DS    0H                                                               
*                                                                               
         CLI   PCLTPOLV-PCLTPOEL+SVCLTPO,P_POLVCQ  SKIP IF CLT LVL PO#          
         BE    *+8                                                              
         CLI   PCLTPOLV-PCLTPOEL+SVCLTPO,P_POLVPQ  SKIP IF PRD LVL PO#          
         BE    VRDAT10                                                          
*                                                                               
         CLC   PVALBSTA,ESTSTRT    START DATE LESS THAN EST START ?             
         BL    EDATERR             YES - ERROR                                  
         CLC   PVALBSTA,ESTEND     START DATE GREATER THAN EST END ?            
         BH    EDATERR             YES - ERROR                                  
         CLC   PVALBEND,ESTSTRT    END DATE LESS THAN EST START ?               
         BL    EDATERR             YES - ERROR                                  
         CLC   PVALBEND,ESTEND     END DATE GREATER THAN EST END ?              
         BH    EDATERR             YES - ERROR                                  
*                                                                               
VRDAT10  DS    0H                                                               
*                                                                               
         OC    SV#DID,SV#DID       ANY EXISTING PO# TO CHECK ?                  
         BZ    VRDATOK             NO - DONE WITH DATES                         
*                                                                               
         TM    PO#DACTV,PO#DUSDQ   USED IN BUY ?                                
         BNO   VRDATOK             NO - DONE WITH DATES                         
*                                  TEST AGAINST PO# RECORD                      
         CLC   PVALBSTA,PO#DSTRT   SCREEN START DATE GT PO# START DATE?         
         BH    VRNOCUT             YES - ERROR                                  
         CLC   PVALBEND,PO#DEND    SCREEN END   DATE LT PO# END   DATE?         
         BL    VRNOCUT             YES - ERROR                                  
*                                                                               
VRDATOK DS     0H                                                               
*                                                                               
         MVC   FLDDATA(L'PVALCPER),PVALCPER    RE-DISPLAY DATES                 
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         CLC   PO#DSTRT,PVALBSTA   IF NEW START DATE                            
         BE    *+28                                                             
         MVC   PO#DSTRT,PVALBSTA      UPDATE ELM START DATE                     
         OC    SV#DID,SV#DID          IF DEALING WITH OLD ELEMENT               
         BZ    *+12                                                             
         OI    SVACH1,PO#ACHGQ           FLAG PO# ELEM CHANGED                  
         OI    SVLNACTV,PO#DCHGQ         INDICATE PO# CHANGED                   
*                                                                               
         CLC   PO#DEND,PVALBEND    IF NEW END   DATE                            
         BE    *+28                                                             
         MVC   PO#DEND,PVALBEND       UPDATE ELM END   DATE                     
         OC    SV#DID,SV#DID          IF DEALING WITH OLD ELEMENT               
         BZ    *+12                                                             
         OI    SVACH1,PO#ACHGQ           FLAG PO# ELEM CHANGED                  
         OI    SVLNACTV,PO#DCHGQ         INDICATE PO# CHANGED                   
*                                                                               
         DROP  R4                                                               
*                                                                               
VRDATX   DS    0H                                                               
*                                                                               
         TITLE 'PRSFM36 - VALIDATE AMOUNT FIELD - VRAMT'                        
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE AMOUNT FIELD                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRAMT    DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU            BUMP TO AMOUNT FIELD                         
*                                                                               
         CLI   NOPO#,C'X'          ANYTHING ON LINE ?                           
         BE    VRAMTX              NO                                           
*                                                                               
         CLI   FLDILEN,0                                                        
         BE    VRMISS              MISSING ERROR                                
*                                                                               
VRAMT10 DS     0H                                                               
*                                                                               
         ZIC   RF,FLDILEN          FIELD LENGTH                                 
*                                                                               
         LA    R4,FLDDATA                                                       
*                                                                               
VRAMLUP DS     0H                                                               
*                                                                               
         CLI   0(R4),C' '          ANYTHING THERE ?                             
         BH    VRAMT20             YES                                          
         LA    R4,1(R4)            NEXT POSITION                                
         BCT   RF,VRAMLUP          TEST NEXT                                    
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
VRAMT20 DS     0H                                                               
*                                                                               
         CLI   0(R4),C'N'          NET ?                                        
         BNE   VRAMT30                                                          
*                                                                               
         OC    SV#DID,SV#DID       IF DEALING WITH OLD ELEMENT                  
         BZ    *+10                                                             
         CLC   PO#DGORN,0(R4)      IF TYPE CHANGED                              
         BE    *+8                                                              
         OI    SVACH1,PO#DCHGQ       INDICATE ELEMENT CHANGED                   
*                                                                               
         MVC   PO#DGORN,0(R4)      SET NUMBER TYPE IN ELEMENT                   
*                                                                               
         LA    R4,1(R4)            BUMP PAST "N" TO "DOLLARS"                   
         BCTR  RF,0                REDUCE LENGTH OF FIELD FOR "N"               
*                                                                               
VRAMT30 DS     0H                                                               
*                                                                               
         GOTOR CASHVAL,DMCB,(X'80',0(R4)),(RF),0                                
         CLI   0(R1),X'FF'         INVALID ?                                    
         BE    AMTERR              YES                                          
*                                                                               
         OC    SV#DID,SV#DID       IF DEALING WITH OLD ELEMENT                  
         BZ    *+10                                                             
         CP    PO#D$,4(8,R1)       IF DOLLARS CHANGED                           
         BE    *+12                                                             
         OI    SVACH1,PO#ACHGQ        INDICATE ELEMENT CHANGED                  
         OI    SVLNACTV,PO#DCHGQ         INDICATE PO# CHANGED                   
*                                                                               
         ZAP   PO#D$,4(8,R1)       SET DOLLARS IN ELEMENT                       
*                                                                               
VRAMTX   DS    0H                                                               
*                                                                               
         TITLE 'PRSFM36 - VALIDATE STATUS FIELD - VRSTA'                        
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE STATUS FIELD                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRSTA    DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU            BUMP TO STATUS FIELD                         
*                                                                               
         CLI   NOPO#,C'X'          ANYTHING ON LINE ?                           
         BE    VRSTAX              NO                                           
*                                                                               
         CLI   FLDDATA,C'I'        INACTIVE ?                                   
         BE    VRSTA30             YES                                          
*                                  NO - ACTIVE                                  
         OC    SV#DID,SV#DID       ANY EXISTING PO# TO CHECK ?                  
         BZ    VRSTA60             NO - CONTINUE                                
*                                                                               
         TM    PO#DACTV,PO#DUSDQ   USED IN BUY ?                                
         BNO   VRSTA60             NO - ANY CHANGE OKAY                         
*                                  YES                                          
         TM    PO#DACTV,PO#DINAQ   RECORD CURRENTLY INACTIVE ?                  
         BO    VRACTERR            YES - CANNOT MAKE ACTIVE                     
*                                                                               
         B     VRSTA60                                                          
*                                                                               
VRSTA30 DS     0H                                                               
*                                                                               
         OC    SV#DID,SV#DID       IF OLD PO# ELM                               
         BZ    *+20                                                             
         TM    PO#DACTV,PO#DINAQ   AND NOT CURRENTLY INACTIVE                   
         BO    *+16                                                             
         OI    SVACH1,PO#ACHGQ       FLAG AS CHANGED ELEMENT                    
         OI    SVLNACTV,PO#DCHGQ     FLAG AS CHANGED ELEMENT                    
         OI    PO#DACTV,PO#DINAQ     INDICATE PO# INACTIVE                      
*                                                                               
         B     VRSTAX                                                           
*                                                                               
VRSTA60  DS    0H                  STATUS IS ACTIVE                             
*                                                                               
         OC    SV#DID,SV#DID       IF OLD PO# ELM                               
         BZ    *+20                                                             
         TM    PO#DACTV,PO#DINAQ   AND CURRENTLY INACTIVE                       
         BNO   *+12                                                             
         OI    SVACH1,PO#DCHGQ       FLAG AS CHANGED ELEMENT                    
         OI    SVLNACTV,PO#DCHGQ     FLAG AS CHANGED ELEMENT                    
*                                                                               
         NI    PO#DACTV,X'FF'-PO#DINAQ   FLAG AS ACTIVE                         
*                                                                               
VRSTAX   DS    0H                                                               
*                                                                               
VRLINCN  DS    0H                                                               
*                                                                               
         OC    SV#DID,SV#DID       IF NEW PO# ELM                               
         BNZ   VRLINCN1                                                         
*                                                                               
         CLI   NOPO#,C'X'          ANYTHING ON LINE ?                           
         BE    VRLINCN1            NO                                           
*                                                                               
         OI    SVLNACTV,PO#DADDQ   ASSUME ADDED THIS TIME                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,SVMAXID        BUMP MAX ID NUMBER                           
         AHI   RF,1                BUMP TO NEW MAX                              
         STCM  RF,3,SVMAXID                                                     
*                                                                               
         MVC   PO#DID,SVMAXID      SET IN NEW ELEMENT                           
         MVC   PO#DPID,SVPID         SET CHANGER'S PID                          
         MVC   PO#DCDTE,DATENOW      CHANGE DATE IS TODAY'S DATE                
         NI    PO#DACTV,X'FF'-PO#DADDQ-PO#DCHGQ  CLEAR ACTV IND                 
         OC    PO#DACTV,SVLNACTV   ADD IN NEW ACTIVITY                          
*                                                                               
         LA    R6,SORTLNTH(R6)     BUMP NEXT TABLE SLOT                         
         ST    R6,SORTNXT          SAVE ADDRESS                                 
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,ELEMCNT                                                     
         AHI   RF,1                BUMP ELEMENT COUNTER                         
         STCM  RF,1,ELEMCNT                                                     
*                                                                               
         B     VRLINCN2                                                         
*                                                                               
VRLINCN1 DS    0H                                                               
*                                                                               
         CLI   SVLNACTV,0          IF ACTIVITY FOR ELM                          
         BE    *+26                                                             
         MVC   PO#DPID,SVPID         SET CHANGER'S PID                          
         MVC   PO#DCDTE,DATENOW      CHANGE DATE IS TODAY'S DATE                
         NI    PO#DACTV,X'FF'-PO#DADDQ-PO#DCHGQ  CLEAR ACTV IND                 
         OC    PO#DACTV,SVLNACTV   ADD IN NEW ACTIVITY                          
*                                                                               
VRLINCN2 DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             USED IN BUY FIELD                            
         BAS   RE,BUMP             HIDDEN ID FIELD                              
         BAS   RE,BUMP             NEXT ACTION FIELD                            
*                                                                               
         C     R2,SCRNEND          AT END OF SCREEN ?                           
         BNH   VRLINLP             NO - BACK TO ACTION FIELD                    
*                                                                               
VRLINDN  DS    0H                                                               
*                                                                               
         XC    0(SORTLNTH,R6),0(R6) CLEAR NEXT SLOT IN TABLE                    
*                                                                               
         DROP  R6                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*        ROUTINE TO CHECK FOR DUPLICATE PO#S AND PERIOD OVERLAPS      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRPO#DUP DS    0H                                                               
*                                                                               
         L     R6,AIO2             POINT TO TABLE OF ELEMENTS                   
         USING PO#DELM,R6          ESTABLISH PO# ELEMENT                        
*                                                                               
VRDUPLP  DS    0H                                                               
*                                                                               
         OC    0(SORTLNTH,R6),0(R6)  DONE AT END OF TABLE                       
         BZ    VRDUPDN                                                          
*                                                                               
         L     R3,AIO2             POINT TO TABLE OF ELEMENTS                   
*                                                                               
VRDUP1LP DS    0H                                                               
*                                                                               
         OC    0(SORTLNTH,R3),0(R3)  DONE AT END OF TABLE                       
         BZ    VRDUP1DN                                                         
*                                                                               
         CR    R3,R6               SKIP IF SAME ELEMENT                         
         BE    VRDUP1CN                                                         
*                                                                               
         CLC   PO#DLEN,PO#DLEN-PO#DELM(R3) OK IF DIFFERENT PO# LENGTHS          
         BNE   VRDUP110                                                         
*                                                                               
         LLC   RF,PO#DLEN          GET ELEM LENGTH                              
         SHI   RF,PO#DHDLQ         DECREMENT BY HEADER LENGTH                   
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   PO#DPO#(0),PO#DPO#-PO#DELM(R3) PROBLEM IF SAME PO#               
         BE    VRDUPER                                                          
*                                                                               
VRDUP110 DS    0H                                                               
*                                                                               
         CLI   SVPROF,C'Y'         ALLOW OVERLAPPING PERIODS ?                  
         BE    VROVLX              YES                                          
*                                  NO - CHECK THEM                              
         TM    PO#DACTV,PO#DINAQ   SKIP IF INACTIVE                             
         BO    VROVLX                                                           
*                                                                               
         TM    PO#DACTV-PO#DELM(R3),PO#DINAQ SKIP IF INACTIVE                   
         BO    VROVLX                                                           
*                                                                               
         CLC   PO#DSTRT,PO#DEND-PO#DELM(R3) DATES CAN'T OVERLAP                 
         BH    VROVLX                                                           
*                                                                               
         CLC   PO#DEND,PO#DSTRT-PO#DELM(R3)                                     
         BL    VROVLX                                                           
*                                                                               
         B     VROVLER             THERE IS AN OVERLAP                          
*                                                                               
VROVLX   DS    0H                                                               
*                                                                               
VRDUP1CN DS    0H                                                               
*                                                                               
         LA    R3,SORTLNTH(R3)     BUMP TO NEXT ENTRY                           
         B     VRDUP1LP                                                         
*                                                                               
VRDUP1DN DS    0H                                                               
*                                                                               
VRDUPCN  DS    0H                                                               
*                                                                               
         LA    R6,SORTLNTH(R6)     NEXT ELEMENT IN TABLE                        
         B     VRDUPLP                                                          
*                                                                               
VRDUPDN  DS    0H                                                               
*                                                                               
         B     VRDUPX                                                           
*                                                                               
VRDUPER  DS    0H                                                               
*                                                                               
         ICM   R2,15,SORTROWA(R3)  POINT R2 TO DUPLICATE ENTRY                  
*                                                                               
         LTR   R2,R2               SKIP IF WE HAVE A(ROW)                       
         BNZ   *+8                                                              
         ICM   R2,15,SORTROWA(R6)  POINT R2 TO DUPLICATE ENTRY                  
*                                                                               
         LTR   R2,R2               IF WE KNOW LINE                              
         BZ    *+8                                                              
         AH    R2,LNDSPPO#            POINT TO PO#                              
*                                                                               
         B     DUPERR                                                           
*                                                                               
VROVLER  DS    0H                                                               
*                                                                               
         ICM   R2,15,SORTROWA(R3)  POINT R2 TO OVERLAPPING ENTRY                
*                                                                               
         LTR   R2,R2               SKIP IF WE HAVE A(ROW)                       
         BNZ   *+8                                                              
         ICM   R2,15,SORTROWA(R6)  POINT R2 TO DUPLICATE ENTRY                  
*                                                                               
         LTR   R2,R2               IF WE KNOW LINE                              
         BZ    *+8                                                              
         AH    R2,LNDSPDAT            POINT TO DATES                            
*                                                                               
         B     VROVLAP             OVERLAPPING PERIODS NOT PERMITTED            
*                                                                               
VRDUPX   DS    0H                                                               
*                                                                               
**********************************************************************          
*        SORT ALL DETAIL ELEMENTS NOW IN AIO2                        *          
**********************************************************************          
*                                                                               
VRSORT   DS     0H                                                              
*                                                                               
         ZIC   R0,ELEMCNT          ELEMENT COUNT                                
         GOTO1 QSORT,DMCB,AIO2,(R0),SORTLNTH,6,4                                
*                                  DONE WITH SORT                               
*                                                                               
*        ON ADD, BUILD SKELETON RECORD                                          
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING A RECORD                           
         BNE   VRADDX                                                           
*                                                                               
         L     R4,AIO1             BUILD RECORD IN IOA1                         
         USING PPO#RECD,R4         ESTABLISH PO# RECORD                         
*                                                                               
         XC    0(256,R4),0(R4)     CLEAR RECORD BUILD AREA                      
*                                                                               
         MVC   PPO#KEY,KEY         SET RECORD KEY                               
         LA    RF,PO#FIRST-PPO#REC SET MINIMUM RECORD LENGTH                    
         STCM  RF,3,PPO#LEN                                                     
*                                                                               
         B     VRDELX                                                           
*                                                                               
VRADDX   DS    0H                                                               
*                                                                               
*        NOW DELETE ALL DETAIL ELEMENTS FROM RECORD IN AIO1                     
*                                                                               
         L     R6,AIO1             GET RECORD ADDRESS                           
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,X'FE'          INDICATE SPECIAL SYSTEM                      
*                                                                               
         LA    R6,33(R6)           POINT TO FIRST ELEMENT IN REC                
*                                                                               
         XC    DMCB+8(4),DMCB+8    SET FOR DELETE                               
*                                                                               
         LA    R1,DMCB             POINT TO PARAMETER LIST                      
         L     RF,VRECUP           POINT TO UPDATING ROUTINE                    
*                                                                               
VRDELL   DS    0H                                                               
*                                                                               
         USING PO#DELMD,R6         ESTABLISH DETAIL ELEMENT                     
*                                                                               
         CLI   PO#DELID,0          DONE IF EOR REACHED                          
         BE    VRDELX                                                           
*                                                                               
         CLI   PO#DELID,PO#DLIDQ   DELETE IF DETAIL ELEMENT                     
         BNE   VRDELC                                                           
*                                                                               
         GOTO1 (RF),(R1),,(R6),,=AL2(33,25,4000)    DELETE ELEMENT              
*                                                                               
         B     VRDELC1             BECAUSE R6 ==> NEXT ELM NOW                  
*                                                                               
VRDELC   DS    0H                                                               
*                                                                               
         ZIC   R0,PO#DLEN          ELEMENT LENGTH                               
         AR    R6,R0               NEXT ELEMENT                                 
*                                                                               
VRDELC1  DS    0H                                                               
*                                                                               
         B     VRDELL                                                           
*                                                                               
VRDELX   DS    0H                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
VRADDEL  DS    0H                  ALL ELEMENTS DELETED                         
*                                                                               
*        NOW ADD ALL DETAIL ELEMENTS TO RECORD IN AIO1                          
*        FROM TABLE OF SORTED DETAIL ELEMENTS IN AIO2                           
*                                                                               
         ZIC   R3,ELEMCNT          *** NOTE - R3 USED AS LOOP COUNTER           
*                                                                               
         L     R6,AIO2             START OF TABLE                               
         CLI   0(R6),0             SKIP IF NO ENTRIES                           
         BNH   VRWTDONE                                                         
*                                                                               
         L     R4,AIO1             GET RECORD ADDRESS                           
         ST    R4,DMCB             PASS TO RECUP                                
         MVI   DMCB,X'FE'          INDICATE SPECIAL SYSTEM                      
                                                                                
         LA    R4,33(R4)           POINT TO FIRST ELEMENT IN REC                
*                                                                               
         LA    R1,DMCB             POINT TO PARAMETER LIST                      
*                                                                               
VRWTLOOP DS    0H                                                               
*                                                                               
         CLI   0(R6),PO#DLIDQ      SKIP IF NOT A PO# DETAIL ELM                 
         BNE   VRWTCONT                                                         
*                                                                               
         GOTO1 VRECUP,(R1),,(R6),(C'R',(R4)),=AL2(33,25,4000) ADD ELEM          
*                                                                               
         CLI   DMCB+8,0            ONLY ERROR CAN BE NO ROOM IN REC             
         BE    RECFULER            NO ERROR TOLERATED                           
*                                                                               
         ZIC   R0,1(R4)            ELEMENT LENGTH                               
         AR    R4,R0               NEXT INSERTION POINT                         
*                                                                               
VRWTCONT DS    0H                                                               
*                                                                               
         LA    R6,SORTLNTH(R6)     BUMP TO NEXT ENTRY                           
*                                                                               
         BCT   R3,VRWTLOOP         LOOP IF NOT LAST ENTRY                       
*                                                                               
VRWTDONE DS    0H                                                               
*                                                                               
         OC    SVACH1,SVACH1       ANYTHING IN ACTIVITY ?                       
         BZ    VRWTX               NO                                           
*                                                                               
         BRAS  RE,ACTPUT           OUTPUT ACTIVITY ELEMENT                      
*                                                                               
VRWTX    DS    0H                                                               
*                                                                               
         BRAS  RE,DR               DISPLAY SORTED UPDATED RECORD                
*                                    AND EXIT                                   
VRX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
VRMISS   MVI   ERROR,MISSING                                                    
         B     VRERR0X                                                          
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         B     VRERR0X                                                          
*                                                                               
VRPONTNM DS    0H                                                               
         MVI   ERROR,NOTNUM        INPUT NOT NUMERIC                            
         B     VRERR0X                                                          
*                                                                               
VRPONTAL DS    0H                                                               
         MVI   ERROR,NOTALPHA      INPUT NOT ALPHABETIC                         
         B     VRERR0X                                                          
*                                                                               
EDATERR  MVI   ERROR,NOTINEST      DATES OUTSIDE ESTIMATE PERIOD                
         B     VRERR0X                                                          
*                                                                               
VRPOINV  DS    0H                  INVALID PURCHASE ORDER NUMBER                
AMTERR   MVI   ERROR,INVALID       AMOUNT FIELD NOT VALID                       
         B     VRERR0X                                                          
*                                                                               
DUPERR   MVI   ERROR,DUPEDATA      DUPLICATE DATA                               
         B     VRERR0X                                                          
*                                                                               
NVALERR  MVI   ERROR,INVALID       INVALID FIELD                                
         B     VRERR0X                                                          
*                                                                               
RECFULER MVI   ERROR,RECFULL       TOO MANY DETAILS                             
         B     VRERR0X                                                          
*                                                                               
VRERR0X  DS    0H                                                               
         GOTOR ERREX                                                            
*                                                                               
*                                                                               
VRUSED   DS    0H                  USED IN BUY - CANNOT CHANGE                  
         LHI   RF,USDINBY          SET ERROR CODE                               
         B     VRERR                                                            
*                                                                               
VREFFDER LHI   RF,EFFDTEER         PO PERIOD STARTS BEFORE EFF DATE             
         B     VRERR                                                            
*                                                                               
VROVLAP  DS    0H                  OVERLAPPING DATES NOT ALLOWED                
         LHI   RF,DTOVRLAP         SET ERROR CODE                               
         B     VRERR                                                            
*                                                                               
VRNOCUT  DS    0H                  USED IN BUY - CANNOT CUT BACK DATES          
         LHI   RF,NOCUTBK          SET ERROR CODE                               
         B     VRERR                                                            
*                                                                               
VRDELERR DS    0H                  USED IN BUY - CANNOT DELETE                  
         LHI   RF,USDNODEL         SET ERROR CODE                               
         B     VRERR                                                            
*                                                                               
VRACTERR DS    0H                  USED IN BUY - CANNOT "RE-ACTIVATE"           
         LHI   RF,ACTVNG           SET ERROR CODE                               
         B     VRERR                                                            
*                                                                               
VRERR    DS    0H                                                               
         STCM  RF,3,ERROR2CD       SET ERROR EQUATE                             
*                                                                               
VRERRX   DS    0H                                                               
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO ERROR2CD IF REQUIRED                     
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
*                                                                               
         CLI   ERROR,0             IF OLD STYLE MESSAGE NUMBER                  
         BE    *+10                                                             
         MVC   ERROR2CD+1(1),ERROR      PUT IN NEW STYLE                        
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         MVC   GTMSGNO,ERROR2CD    MESSAGE NUMBER                               
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
***********************************************************************         
*           VARIOUS SCREEN DISPLACEMENTS                              *         
***********************************************************************         
*                                                                               
LNDSPID  DC    Y(SCRID1H-SCRACT1H)       FROM ACTIVITY TO HIDDEN ID             
LNDSPDAT DC    Y(SCRDAT1H-SCRACT1H)      FROM ACTIVITY TO PERIOD                
LNDSPPO# DC    Y(SCRPO#1H-SCRACT1H)      FROM ACTIVITY TO PO#                   
LNPRDSTA DC    Y(SCRSTA1H-SCRDAT1H)      FROM PERIOD TO STATUS                  
*                                                                               
LINLNTH  DC    Y(SCRACT2H-SCRACT1H)      SCREEN "LINE" LENGTH                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM36 PURCHASE ORDER NUMBER (PO#) RECORDS - DR'               
***********************************************************************         
*                                                                     *         
*        DISPLAY  RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
DR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         CLC   SVPO#KEY(L'PPO#KEY),KEY IF NEW PO# RECORD                        
         BE    DRPAGE10                                                         
*                                                                               
         MVC   SVPO#KEY,KEY        SAVE NEW KEY                                 
*                                                                               
         MVI   SCRPAGEH+5,1        SET PAGE COUNTER TO 1                        
         MVC   SCRPAGE,=C'1  '                                                  
         ZAP   SVPAGE,=P'1'           DEFAULT TO ONE                            
         B     DRPAGEX                                                          
*                                                                               
DRPAGE10 DS    0H                                                               
*                                                                               
         BRAS  RE,PFK                                                           
*                                                                               
*        VALIDATE PAGE NUMBER                                                   
*                                                                               
DRPAGE   DS    0H                                                               
*                                                                               
         CLI   SCRPAGEH+5,0        IF NO PAGE ENTERED                           
         BNZ   *+14                                                             
         ZAP   SVPAGE,=P'1'           DEFAULT TO ONE                            
         B     DRPAGEX                                                          
*                                                                               
         LLC   RF,SCRPAGEH+5       GET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SCRPAGE(0)      PACK PAGE NUMBER                             
*                                                                               
         TP    DUB                 IF NOT A NUMBER                              
         BZ    *+14                                                             
         ZAP   SVPAGE,=P'1'           DEFAULT TO ONE                            
         B     DRPAGEX                                                          
*                                                                               
         ZAP   SVPAGE,DUB          SAVE ENTERED PAGE NUMBER                     
*                                                                               
DRPAGEX  DS    0H                                                               
*                                                                               
         FOUT  SCRPAGEH,SPACES,3   INIT PAGE FIELD                              
*                                                                               
         EDIT  (P8,SVPAGE),SCRPAGE,0,ALIGN=LEFT                                 
*                                                                               
         STC   R0,SCRPAGEH+5       SET FIELD LENGTH                             
*                                                                               
***********************************************************************         
*                                                                     *         
*  DISPLAY DETAILS                                                    *         
*                                                                     *         
***********************************************************************         
*                                                                               
         MVI   RECDONE,0           CLEAR                                        
*                                                                               
*        FIND STARTING PO# FOR DISPLAY                                          
*                                                                               
         CVB   R0,SVPAGE           GET PAGE NUMBER                              
         BCTR  R0,0                DECREMENT FOR INDEXING                       
         MHI   R0,PAGE#Q           STARTING PO#                                 
         AHI   R0,1                                                             
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,PO#DLIDQ     LOOK FOR 1ST DETAIL ELM                      
         BRAS  RE,GETEL                                                         
*                                                                               
DRPO#LP  DS    0H                                                               
*                                                                               
         USING PO#DELMD,R6         ESTABLISH DETAIL ELEMENT                     
*                                                                               
         BNE   DRPO#DN             NO MORE DETAIL ELEMENTS                      
*                                                                               
         BCT   R0,*+8              DECREMENT COUNTER                            
         B     DRPO#FD             STARTING PO# FOUND                           
*                                                                               
DRPO#CN  DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT DETAIL ELM                         
         B     DRPO#LP                                                          
*                                                                               
DRPO#DN  DS    0H                                                               
*                                                                               
         MVI   RECDONE,C'X'        NOTHING (OR NOTHING LEFT) IN RECORD          
*                                                                               
DRPO#FD  DS    0H                                                               
*                                                                               
         LA    R2,SCRACT1H         A(FIRST FIELD)                               
         USING FLDHDRD,R2          ESTABLISH GENERIC FIELD                      
*                                                                               
         LA    RE,SCRACTLH         POINT TO FINAL ACTION FIELD                  
         ST    RE,SCRNEND          SAVE THE ADDRESS                             
*                                                                               
*        CLEAR ACTIVITY FIELD                                                   
*                                                                               
DRACT    DS    0H                                                               
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY PURCHASE ORDER NUMBER (PO#)                                    
*                                                                               
DRPO#    DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU                                                         
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         CLI   RECDONE,C'X'        MORE TO DISPLAY ?                            
         BE    DRPO#X              NO                                           
*                                                                               
         XC    WORK,WORK                                                        
         ZIC   RE,PO#DLEN          GET ELEMENT LENGTH                           
         LA    RF,PO#DHDLQ         GET ELEMENT HEADER LENGTH                    
         SR    RE,RF               LENGTH OF VARIABLE PO# TO RE                 
         BCTR  RE,0                PREP FOR EXECUTED MOVE                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),PO#DPO#                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
DRPO#X   DS    0H                                                               
*                                                                               
*        DISPLAY DATES                                                          
*                                                                               
DRDAT    DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU                                                         
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         CLI   RECDONE,C'X'        MORE TO DISPLAY ?                            
         BE    DRDATX              NO                                           
*                                                                               
*                                  DISPLAY DATES AS MMMDD/YY-MMMDD/YY           
         GOTOR DATCON,DMCB,(X'13',PO#DSTRT),(5,FLDDATA),0                       
*                                                                               
DRDATX   DS    0H                                                               
*                                                                               
*              DISPLAY AMOUNT                                                   
*                                                                               
DRAMT    DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU                                                         
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         CLI   RECDONE,C'X'        MORE TO DISPLAY ?                            
         BE    DRAMTX              NO                                           
*                                                                               
         LA    R5,FLDDATA                                                       
         CLI   PO#DGORN,C'N'       NET INDICATED ?                              
         BNE   DRAMT40             NO                                           
         EDIT  (P8,PO#D$),(13,0(R5)),2,COMMAS=YES,FLOAT=N                       
         B     DRAMTOK                                                          
*                                                                               
DRAMT40 DS     0H                                                               
*                                                                               
         EDIT  (P8,PO#D$),(13,0(R5)),2,COMMAS=YES                               
*                                                                               
DRAMTOK DS     0H                                                               
*                                                                               
DRAMTX   DS    0H                                                               
*                                                                               
*        DISPLAY STATUS                                                         
*                                                                               
DRSTA    DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU                                                         
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         CLI   RECDONE,C'X'        MORE TO DISPLAY ?                            
         BE    DRSTAX              NO                                           
*                                                                               
         MVC   FLDDATA(6),=C'ACTIVE'                                            
         TM    PO#DACTV,PO#DINAQ                                                
         BNO   *+10                                                             
         MVC   FLDDATA(6),=C'INACTV'                                            
*                                                                               
DRSTAX   DS    0H                                                               
*                                                                               
*        DISPLAY WHETHER USED IN BUY                                            
*                                                                               
DRBUY    DS    0H                                                               
*                                                                               
         BAS   RE,BUMP                                                          
         TWAXC (R2),(R2),PROT=Y    CLEAR FIELD                                  
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         CLI   RECDONE,C'X'        MORE TO DISPLAY ?                            
         BE    DRBUYX              NO                                           
*                                                                               
         MVC   FLDDATA(3),=C'YES'  ASSUME USED IN A BUY                         
*                                                                               
         TM    PO#DACTV,PO#DUSDQ   ELSE DISPLAY 'NO'                            
         BO    *+10                                                             
         MVC   FLDDATA(3),=C'NO '                                               
*                                                                               
DRBUYX   DS    0H                                                               
*                                                                               
*        SAVE THE PO# INTERNAL ID ON THE SCREEN                                 
*                                                                               
DRPOID   DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             PROTECTED FIELD                              
         MVC   FLDDATA(3),=C'000'  SET INITIALLY AS ZEROS                       
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         CLI   RECDONE,C'X'        MORE TO DISPLAY ?                            
         BE    DRPOIDX             NO                                           
*                                                                               
         EDIT  (B2,PO#DID),(3,FLDDATA)                                          
*                                                                               
         CLI   FLDDATA,C'0'        ZERO-FILL HIGH ORDER BYTES                   
         BNL   *+8                                                              
         MVI   FLDDATA,C'0'                                                     
         CLI   FLDDATA+1,C'0'                                                   
         BNL   *+8                                                              
         MVI   FLDDATA+1,C'0'                                                   
*                                                                               
DRPOIDX  DS    0H                                                               
*                                                                               
DRLX     DS    0H                                                               
*                                                                               
         CLI   RECDONE,C'X'        NO MORE TO DISPLAY ?                         
         BE    DRLBACK             CLEAR REST OF SCREEN                         
*                                                                               
         BRAS  RE,NEXTEL           MORE ELEMENTS ?                              
         BNE   DRRECEND            NO - MARK "NO MORE TO DISPLAY"               
         CLI   PO#DELID,PO#DLIDQ   DETAIL ELEMENT  ?                            
         BNE   DRLX                CHECK MORE                                   
         B     DRLBACK                                                          
*                                                                               
DRRECEND DS    0H                                                               
         MVI   RECDONE,C'X'                                                     
*                                                                               
DRLBACK  DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU            R2 TO NEXT ACTION FIELD                      
*                                                                               
         C     R2,SCRNEND          AT END OF SCREEN ?                           
         BNH   DRACT               NO - NEXT LINE                               
**********************************************************************          
*                   DISPLAY ACTIVITY                                            
**********************************************************************          
*                                                                               
         XC    SCRLUPD,SCRLUPD     CLEAR "LAST UPDATED" FIELDS                  
         XC    SCRDTEU,SCRDTEU                                                  
         XC    SCRPIDU,SCRPIDU                                                  
         XC    SCRNAMU,SCRNAMU                                                  
         FOUT  SCRLUPDH            XMIT FIELDS                                  
         FOUT  SCRDTEUH                                                         
         FOUT  SCRPIDUH                                                         
         FOUT  SCRNAMUH                                                         
*                                                                               
         MVI   ELCODE,X'90'        SET ACTIVITY ELEMENT CODE                    
         L     R6,AIO              POINT TO RECORD                              
         BRAS  RE,GETEL            FIND ACTIVITY ELEMENT                        
         BNE   DRACTVX             SKIP IF NONE FOUND                           
*                                                                               
DRACTV10 DS    0H                                                               
         ST    R6,FULL             SAVE ELEMENT ADDRESS                         
         BRAS  RE,NEXTEL                                                        
         BE    DRACTV10                                                         
         L     R6,FULL             MOST RECENT ACTIVITY ELEMENT                 
*                                                                               
         USING PO#AELMD,R6         ESTABLISH ACTIVITY ELEMENT                   
*                                                                               
         OC    PO#ADTE,PO#ADTE     SKIP IF NO DATE AVAILABLE                    
         BZ    DRACTVX                                                          
*                                                                               
         LA    R2,SCRLUPDH                                                      
         MVC   8(13,R2),=CL13'Last Updated:'                                    
         OI    6(R2),X'80'         TRANSMIT FLD                                 
*                                                                               
         LA    R3,PO#ADTE          DATE OF LAST ACTIVITY (RECORD)               
*                                                                               
         LA    R2,SCRDTEUH         DATE OF LAST ACTIVITY (SCREEN)               
*                                                                               
         GOTOR DATCON,DMCB,(3,0(R3)),(17,8(R2)) DISP DATE                       
         OI    6(R2),X'80'         TRANSMIT FLD                                 
*                                                                               
         OC    PO#APID,PO#APID     IF NO LAST CHANGED PID                       
         BZ    DRACTVX             DONE                                         
*                                                                               
         LA    R3,PO#APID          LAST CHANGED PID (RECORD)                    
*                                                                               
         LA    R2,SCRPIDUH         LAST CHANGED PID (SCREEN)                    
*                                                                               
         GOTOR TRNPID,DMCB,0(R3)   DISPLAY PID                                  
*                                                                               
DRACTVX  DS    0H                                                               
*                                  END OF DISPLAY REC                           
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
*                                                                               
         MVI   GTMSGNO1,3          MESSAGE NUMBER (1 BYTE)                      
         MVI   GTMSYS,X'FF'        SYSTEM ZERO (GENERAL) MESSAGES               
*                                                                               
         DROP  R1                                                               
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM36 PURCHASE ORDER NUMBER (PO#) RECORDS - RDEL'             
***********************************************************************         
*                                                                     *         
*        DELETE   RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
RDEL     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
*        SEE IF ANY PO#S EXIST OR HAVE BEEN USED IN A BUY                       
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,PO#DLIDQ     LOOK FOR DETAIL ELM                          
         BRAS  RE,GETEL                                                         
         BNE   RDELACT             NOTHING IN RECORD - OKAY TO DELETE           
         USING PO#DELMD,R6                                                      
*                                                                               
RDELTST  DS    0H                                                               
         TM    PO#DACTV,PO#DUSDQ   PO# USED IN BUY ?                            
         BO    RDDELERR            YES - CANNOT DELETE RECORD                   
*                                                                               
RDELLUP  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   RDELACT             OKAY TO DELETE                               
         CLI   PO#DELID,PO#DLIDQ   DETAIL ELEMENT ?                             
         BNE   RDELLUP             NO - LOOK FOR MORE                           
         B     RDELTST             YES - TEST IT                                
*                                                                               
         DROP  R6                                                               
*                                                                               
*        ADD ACTIVITY ELEMENT VIA ACTPUT                                        
*                                                                               
RDELACT  DS    0H                                                               
         OI    SVACH1,PO#ADELQ     SET RECORD DELETED                           
*                                                                               
         BRAS  RE,ACTPUT           ADD ACTIVITY ELEMENT                         
*                                                                               
RDELX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
RDDELERR DS    0H                  USED IN BUY - CANNOT DELETE                  
         LHI   RF,USDNODEL         SET ERROR CODE                               
         B     RDERR                                                            
*                                                                               
RDERR    DS    0H                                                               
         STCM  RF,3,ERROR2CD       SET ERROR EQUATE                             
*                                                                               
RDERRX   DS    0H                                                               
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO ERROR2CD IF REQUIRED                     
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
*                                                                               
         CLI   ERROR,0             IF OLD STYLE MESSAGE NUMBER                  
         BE    *+10                                                             
         MVC   ERROR2CD+1(1),ERROR      PUT IN NEW STYLE                        
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         MVC   GTMSGNO,ERROR2CD    MESSAGE NUMBER                               
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM36 PURCHASE ORDER NUMBER (PO#) RECORDS - RR'               
***********************************************************************         
*                                                                     *         
*        RESTORE  RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
RR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
*        ADD ACTIVITY ELEMENT                                                   
*                                                                               
         NI    SVACH1,(X'FF'-PO#ADELQ)    RECORD UNDELETED                      
         OI    SVACH1,PO#ACHGQ            RECORD RESTORED                       
*                                                                               
         BRAS  RE,ACTPUT           ADD ACTIVITY ELEMENT                         
*                                                                               
RRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM36 PURCHASE ORDER NUMBER (PO#) RECORDS - LR'               
***********************************************************************         
*                                                                     *         
*        LIST RECORDS                                                 *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
LR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         OI    GLSTSTAT,RETEXTRA   RETURN AFTER LAST ON SCREEN                  
*                                                                               
         XC    SVPO#KEY,SVPO#KEY   INIT SAVED KEY                               
*                                                                               
         LA    R6,KEY              ESTABLISH RECORD KEY                         
         USING PPO#RECD,R6                                                      
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR1STX              KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
*        BUILD STARTING KEY                                                     
*                                                                               
         MVC   PPO#KAGY,AGENCY     AGENCY                                       
         MVC   PPO#KMED,QMED       MEDIA CODE                                   
         MVI   PPO#KRCD,PPO#KIDQ   TYPE                                         
*                                                                               
         MVC   PPO#KCLT,QCLT       CLIENT                                       
*                                                                               
         MVC   PPO#KPRD,QPRD       PRODUCT                                      
*                                                                               
         CLI   P#LESTH+5,0         IF EST ENTERED                               
         BE    *+10                                                             
         MVC   PPO#KEST,BEST       EST                                          
*                                                                               
         GOTO1 HIGH                READ FIRST RECORD                            
*                                                                               
LR1STX   DS    0H                                                               
*                                                                               
         MVC   AIO,AIO1            USE AIO1                                     
*                                                                               
LRLOOP   DS    0H                                                               
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
*                                                                               
         TM    PPO#CNTL,X'80'      SKIP DELETED RECORDS                         
         BO    LRCONT                                                           
*                                                                               
         CLC   PPO#KEY(PPO#KCLT-PPO#KEY),KEYSAVE  DONE AT END OF MEDIA          
         BNE   LRDONE                                                           
*                                                                               
         OC    QCLT,QCLT           CLIENT GIVEN ?                               
         BZ    LRCLTX              NO                                           
         CLC   PPO#KCLT,QCLT                                                    
         BNE   LRCONT              FILTER ON CLIENT                             
*                                                                               
LRCLTX   DS    0H                                                               
*                                                                               
         OC    QPRD,QPRD           PRODUCT GIVEN ?                              
         BZ    LRPRDX              NO                                           
         CLC   PPO#KPRD,QPRD                                                    
         BNE   LRCONT              FILTER ON PRODUCT                            
*                                                                               
LRPRDX   DS    0H                                                               
*                                                                               
         OC    BEST,BEST          ESTIMATE GIVEN ?                              
         BZ    LRESTX             NO                                            
         CLC   PPO#KEST,BEST                                                    
         BNE   LRCONT             FILTER ON ESTIMATE                            
*                                                                               
LRESTX   DS    0H                                                               
*                                                                               
         GOTO1 GETREC              GET PO# RECORD                               
*                                                                               
         MVC   MYDSKADD,DMDSKADD   SAVE D/A FOR LIST                            
*                                                                               
         L     R6,AIO              POINT TO FOUND RECORD                        
*                                                                               
         USING LISTD,R5            ESTABLISH LIST DISPLAY                       
*                                                                               
         LA    R5,LISTAR           OR LIST AREA                                 
         MVC   LISTAR,SPACES                                                    
*                                                                               
*        DISPLAY CLIENT                                                         
*                                                                               
         MVC   LCLT,PPO#KCLT       CLIENT                                       
*                                                                               
*        DISPLAY PRODUCT                                                        
*                                                                               
         MVC   LPRD,PPO#KPRD       PRODUCT                                      
*                                                                               
*        DISPLAY ESTIMATE                                                       
*                                                                               
         EDIT  PPO#KEST,(3,LEST)   ESTIMATE                                     
*                                                                               
*                                  LIST NEXT RECORD                             
         MVC   DMDSKADD,MYDSKADD   RESTORE D/A                                  
*                                                                               
         GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
*                                                                               
LRCONT   DS    0H                                                               
*                                                                               
         LA    R6,KEY              MUST RESET R6 TO KEY                         
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
         B     LRLOOP                                                           
*                                                                               
LRDONE   DS    0H                                                               
*                                                                               
         XC    KEY,KEY             TELL GENCON ALL DONE                         
*                                                                               
LRX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
         TITLE 'PRSFM36 PURCHASE ORDER NUMBER (PO#) RECORDS - PR'               
***********************************************************************         
*                                                                     *         
*        PRINT REPORT                                                 *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
PR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R6,KEY              ESTABLISH RECORD KEY                         
         USING PPO#RECD,R6                                                      
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   PR1STX              KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*        BUILD STARTING KEY                                                     
*                                                                               
         MVC   PPO#KAGY,AGENCY     AGENCY                                       
         MVC   PPO#KMED,QMED       MEDIA CODE                                   
         MVI   PPO#KRCD,PPO#KIDQ   TYPE                                         
*                                                                               
         MVC   PPO#KCLT,QCLT       CLIENT                                       
*                                                                               
         MVC   PPO#KPRD,QPRD       PRODUCT                                      
*                                                                               
         CLI   P#LESTH+5,0         IF EST ENTERED                               
         BE    *+10                                                             
         MVC   PPO#KEST,BEST       EST                                          
*                                                                               
         GOTO1 HIGH                READ FIRST RECORD                            
*                                                                               
PR1STX   DS    0H                                                               
*                                                                               
         MVC   AIO,AIO1            USE AIO1                                     
*                                                                               
PRLOOP   DS    0H                                                               
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
*                                                                               
         TM    PPO#CNTL,X'80'      SKIP DELETED RECORDS                         
         BO    PRCONT                                                           
*                                                                               
         CLC   PPO#KEY(PPO#KCLT-PPO#KEY),KEYSAVE  DONE AT END OF MEDIA          
         BNE   PRDONE                                                           
*                                                                               
         CLC   PPO#KEY(PPO#KEST-PPO#KEY),KEYSAVE  SAME THRU PRODUCT ?           
         BE    *+8                 YES                                          
         MVI   FORCEHED,C'Y'       NO - FORCE NEXT PAGE                         
*                                                                               
         OC    QCLT,QCLT           CLIENT GIVEN ?                               
         BZ    PRCLTX              NO                                           
*                                                                               
         CLC   PPO#KCLT,QCLT                                                    
         BNE   PRCONT              FILTER ON CLIENT                             
*                                                                               
PRCLTX   DS    0H                                                               
*                                                                               
         OC    QPRD,QPRD           PRODUCT GIVEN ?                              
         BZ    PRPRDX              NO                                           
         CLC   PPO#KPRD,QPRD                                                    
         BNE   PRCONT              FILTER ON PRODUCT                            
*                                                                               
PRPRDX   DS    0H                                                               
*                                                                               
         OC    BEST,BEST          ESTIMATE GIVEN ?                              
         BZ    PRESTX             NO                                            
         CLC   PPO#KEST,BEST                                                    
         BNE   PRCONT             FILTER ON ESTIMATE                            
*                                                                               
PRESTX   DS    0H                                                               
*                                                                               
         GOTO1 GETREC              GET PO# RECORD                               
*                                                                               
         MVC   MYDSKADD,DMDSKADD   SAVE D/A                                     
*                                                                               
         L     R6,AIO              POINT TO FOUND RECORD                        
*                                                                               
*                                  SEE IF NEED NEW "HEADER" NAMES               
         XC    PRTRDSW,PRTRDSW                                                  
*                                                                               
         CLC   PCLT,PPO#KCLT       HAVE CLIENT ?                                
         BE    PRGPRD              YES                                          
*                                                                               
         MVC   PCLT,PPO#KCLT                                                    
*                                                                               
         MVC   AIO,AIO2            USE AIO2                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(PPO#KPRD-PPO#KEY),PPO#KEY                                    
         MVI   KEY+3,X'02'         CLIENT RECORD CODE                           
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'PPO#KEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                MUST BE FOUND                                
*                                                                               
         GOTO1 GETREC              GET THE CLIENT RECORD                        
*                                                                               
         L     R5,AIO2                                                          
         LA    R5,33(R5)           ESTABLISH CLIENT HEADER ELEMENT              
         USING PCLTELEM,R5                                                      
         MVC   CLTNM,PCLTNAME      SAVE CLIENT NAME                             
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,PCLTPOEQ     FIND PURCHASE ORDER ELM                      
         BRAS  RE,GETEL                                                         
         BNE   PRCLTXX                                                          
*                                                                               
         USING PCLTPOEL,R6         ESTABLISH PURCHASE ORDER ELM                 
*                                                                               
         LLC   RF,PCLTPOLN         SAVE PURCHASE ORDER ELM                      
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVCLTPO(0),PCLTPOEL                                              
*                                                                               
         CLI   PCLTPOLV,P_POLVCQ  SKIP IF CLIENT LEVEL PO'S                     
         BE    PRCLTXX                                                          
*                                                                               
         MVI   PRTRDSW,C'P'        NO - MUST ALSO READ PRODUCT RECORD           
*                                                                               
         CLI   PCLTPOLV,P_POLVPQ  SKIP IF PRD LEVEL PO'S                        
         BE    PRCLTXX                                                          
*                                                                               
         MVI   PRTRDSW,C'E'        MUST ALSO READ ESTIMATE RECORD               
*                                                                               
         DROP  R5                                                               
*                                                                               
PRCLTXX  DS    0H                                                               
*                                                                               
         L     R6,AIO1             ESTABLISH RECORD KEY                         
         USING PPO#RECD,R6                                                      
*                                                                               
PRGPRD   DS    0H                                                               
*                                                                               
         CLI   PRTRDSW,C'P'        READ PRODUCT  RECORD ?                       
         BE    *+8                                                              
         CLI   PRTRDSW,C'E'        READ ESTIMATE RECORD ?                       
         BE    PRGPRD20            YES                                          
*                                                                               
         CLC   PPRD,PPO#KPRD       HAVE PRODUCT ?                               
         BE    PRGEST              YES                                          
*                                                                               
PRGPRD20 DS    0H                  READ PRODUCT RECORD                          
*                                                                               
         MVC   PPRD,PPO#KPRD                                                    
*                                                                               
         MVC   AIO,AIO2            USE AIO2                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(PPO#KEST-PPO#KEY),PPO#KEY                                    
         MVI   KEY+3,X'06'         PRODUCT RECORD CODE                          
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'PPO#KEY),KEYSAVE                                           
         BNE   PRPRDXX                                                          
*                                                                               
         GOTO1 GETREC              GET THE PRODUCT RECORD                       
*                                                                               
         L     R5,AIO2                                                          
         LA    R5,33(R5)           ESTABLISH PRODUCT HEADER ELEMENT             
         USING PPRDELEM,R5                                                      
         MVC   PRDNM,PPRDNAME      SAVE PRODUCT NAME                            
*                                                                               
PRPRDXX  DS    0H                                                               
*                                                                               
         DROP  R5                                                               
*                                                                               
PRGEST   DS    0H                                                               
*                                                                               
         CLI   PRTRDSW,C'E'        READ ESTIMATE RECORD ?                       
         BE    PRGEST20            YES                                          
         CLC   PEST,PPO#KEST       HAVE ESTIMATE ?                              
         BE    PRGESTX             YES                                          
*                                                                               
PRGEST20 DS    0H                  READ ESTIMATE RECORD                         
*                                                                               
         MVC   PEST,PPO#KEST                                                    
         ZICM  R0,PEST,2                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPEST,DUB                                                        
*                                                                               
         MVC   AIO,AIO2            USE AIO2                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'PPO#KEY),PPO#KEY                                           
         MVI   KEY+3,X'07'         ESTIMATE RECORD CODE                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'PPO#KEY),KEYSAVE                                           
         BNE   PRGESTX                                                          
*                                                                               
         GOTO1 GETREC              GET THE ESTIMATE RECORD                      
*                                                                               
         L     R5,AIO2                                                          
         LA    R5,33(R5)           ESTABLISH ESTIMATE HEADER ELEMENT            
         USING PESTELEM,R5                                                      
         MVC   ESTNM,PESTNAME      SAVE ESTIMATE NAME                           
*                                                                               
         DROP  R5                                                               
*                                                                               
PRGESTX  DS    0H                                                               
*                                                                               
         XC    PRTRDSW,PRTRDSW                                                  
         MVC   AIO,AIO1            RESTORE AIO TO PO# RECORD                    
         MVC   DMDSKADD,MYDSKADD   RESTORE D/A                                  
*                                                                               
         MVC   P1,SPACES                                                        
         LA    R4,P1               USE P LINES                                  
         USING PLINED,R4           ESTABLISH PRINTED LINE                       
*                                                                               
         CLI   FORCEHED,C'Y'       FORCE NEXT PAGE ?                            
         BE    PRGLINE             YES - NO SKIP NEEDED                         
         GOTO1 SPOOL,DMCB,SPOOLD   SKIP A LINE                                  
*                                                                               
*                                  PRINT THE ELEMENT (S)                        
*                                                                               
         DROP  R6                                                               
*                                                                               
PRGLINE  DS    0H                                                               
*                                                                               
         MVC   PLEST,PPEST         ESTIMATE CODE                                
         MVC   PLESTNM,ESTNM       ESTIMATE NAME                                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PO#DLIDQ     PO# ELEMENT ID                               
         BRAS  RE,GETEL                                                         
         BE    PRTRLUP                                                          
         MVC   P1+32(31),=C'NO PURCHASE ORDER NUMBERS EXIST'                    
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE                                   
         B     PRENDREC                                                         
*                                                                               
         USING PO#DELMD,R6                                                      
*                                                                               
PRTRLUP  DS    0H                                                               
*                                                                               
         ZIC   RE,PO#DLEN          GET ELEMENT LENGTH                           
         LA    RF,PO#DHDLQ         GET ELEMENT HEADER LENGTH                    
         SR    RE,RF               LENGTH OF VARIABLE PO# TO RE                 
         BCTR  RE,0                PREP FOR EXECUTED MOVE                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PLPPO#(0),PO#DPO#                                                
*                                  DISPLAY DATES AS MMMDD/YY-MMMDD/YY           
         GOTOR DATCON,DMCB,(X'13',PO#DSTRT),(5,PLPDAT),0                        
*                                                                               
         LA    R5,PLPAMT                                                        
         CLI   PO#DGORN,C'N'       NET INDICATED ?                              
         BNE   PRAMT40             NO                                           
         EDIT  (P8,PO#D$),(15,0(R5)),2,COMMAS=YES,FLOAT=N                       
         B     PRAMTX                                                           
*                                                                               
PRAMT40 DS     0H                                                               
*                                                                               
         EDIT  (P8,PO#D$),(15,0(R5)),2,COMMAS=YES                               
*                                                                               
PRAMTX   DS    0H                                                               
*                                                                               
         MVC   PLPSTA,=C'ACTIVE'                                                
         TM    PO#DACTV,PO#DINAQ                                                
         BNO   *+10                                                             
         MVC   PLPSTA,=C'INACTV'                                                
*                                                                               
         TM    PO#DACTV,PO#DUSDQ   USED IN BUY ?                                
         BNO   *+10                                                             
         MVC   PLPUSED(3),=C'YES'                                               
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE- WILL FORCE SPACING               
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    PRTRLUP             NEXT ELEMENT                                 
*                                                                               
         DROP  R6                                                               
*                                                                               
PRENDREC DS    0H                                                               
*                                                                               
         MVC   P1,SPACES                                                        
*****    GOTO1 SPOOL,DMCB,SPOOLD   SKIP A LINE                                  
*                                                                               
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
*                                                                               
PRCONT   DS    0H                                                               
*                                                                               
         LA    R6,KEY              MUST RESET R6 TO KEY                         
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
         B     PRLOOP                                                           
*                                                                               
PRDONE   DS    0H                                                               
*                                                                               
         XC    KEY,KEY             TELL GENCON ALL DONE                         
*                                                                               
PRX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM36 PURCHASE ORDER NUMBER (PO#) RECORDS - HOOK'             
***********************************************************************         
*                                                                     *         
*        HEADLINE ROUTINES                                            *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
HOOK     NTR1  BASE=*,LABEL=*      HEADLINE ROUTINES                            
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         MVC   HEAD3(5),=C'MEDIA'                                               
         MVC   HEAD3+9(1),QMED                                                  
         MVC   HEAD3+13(10),MEDNM                                               
*                                                                               
         MVC   HEAD4(6),=C'CLIENT'                                              
         MVC   HEAD4+9(3),PCLT                                                  
         MVC   HEAD4+13(20),CLTNM                                               
*                                                                               
         MVC   HEAD5(7),=C'PRODUCT'                                             
         MVC   HEAD5+09(3),PPRD                                                 
         MVC   HEAD5+13(20),PRDNM                                               
*                                                                               
         LA    R4,P1                                                            
         USING PLINED,R4                                                        
         MVC   PLEST,PPEST         ESTIMATE CODE                                
         MVC   PLESTNM,ESTNM       ESTIMATE NAME                                
         DROP  R4                                                               
*                                                                               
HOOKX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
*                                  LINES IN WINDOW                              
NLINS    EQU   ((SCRPO#LH-SCRPO#1H)/(SCRPO#2H-SCRPO#1H))+1                      
NFLDS    EQU   6            6 FIELDS PER LINE                                   
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM36 - PURCHASE ORDER NUMBER (PO#) RECORDS - ACTPUT'         
***********************************************************************         
*                                                                     *         
*        ROUTINE TO ADD ACTIVITY DATA TO RECORD                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
ACTPUT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         BRAS  RE,ACTLMT           SEE IF MUST DELETE EXCESS ELEMENT(S)         
*                                                                               
*        ADD ACTIVITY ELEMENT                                                   
*                                                                               
         XC    SVACTELM,SVACTELM   INIT WORK AREA                               
         LA    R3,SVACTELM                                                      
         USING PO#AELMD,R3         ESTABLISH ACTIVITY ELM                       
*                                                                               
         MVI   PO#AELID,PO#ALIDQ   SET ELEMENT CODE                             
         MVI   PO#ALEN,PO#ALNGQ    SET ELEMENT LENGTH                           
*                                                                               
         GOTO1 DATCON,DUB,(5,0),(3,PO#ADTE) SET TODAY'S DATE                    
*                                                                               
         BRAS  RE,PIDGET           FIND PID                                     
*                                                                               
*SMY*    MVC   PO#ATIM,???????     SET TIME OF CHANGE                           
*                                                                               
         MVC   PO#APID,SVPID       SET CHANGER'S PID                            
*                                                                               
         MVC   PO#AACTV,SVACHGS    SET ACTIVITY INDICATOR                       
*                                                                               
         L     R6,AIO              POINT TO RECORD                              
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,X'FE'          INDICATE SPECIAL                             
*                                                                               
*        CHECK OLD ACTIVITY FOR DUPLICATES                                      
*                                                                               
         MVI   ELCODE,PO#ALIDQ     LOOK FOR ACTIVITY ELM                        
         BRAS  RE,GETEL            FIND IN RECORD                               
*                                                                               
ACTPUTLP DS    0H                                                               
*                                                                               
         BNE   ACTPUTDN            SKIP IF NONE FOUND                           
*                                                                               
         CLC   PO#APID,PO#APID-PO#AELM(R6)  IF SAME PERSON                      
         BNE   ACTPUTCN                                                         
*SMY*    CLC   CCAASCID,CCAASCID-CCAAELM(R6) IF SAME SCID                       
*SMY*    BNE   ACTPUTCN                                                         
         CLC   PO#ADTE,PO#ADTE-PO#AELM(R6)  AND DATE                            
         BNE   ACTPUTCN                                                         
*                                                                               
         OC    PO#AACTV-PO#AELM(L'PO#AACTV,R6),SVACHGS UPDATE CHANGES           
*                                                                               
         B     ACTPUTDX               SKIP ADD OF ELM                           
*                                                                               
ACTPUTCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL                                                        
*                                                                               
         B     ACTPUTLP                                                         
*                                                                               
ACTPUTDN DS    0H                                                               
*                                  ADD ELEMENT                                  
         GOTO1 VRECUP,DMCB,,(R3),(C'R',(R6)),=AL2(33,25,4000)                   
*                                                                               
         CLI   DMCB+8,0            ONLY ERROR CAN BE NO ROOM IN REC             
         BE    ACTPUTER            NO ERROR TOLERATED                           
*                                                                               
ACTPUTDX DS    0H                                                               
*                                                                               
ACTPUTX  DS    0H                                                               
         MVC   ERROR,SAVMSGNO      RESTORE MESSAGE NUMBER                       
         XIT1                                                                   
*                                                                               
ACTPUTER MVI   ERROR,RECFULL       NO ROOM IN RECORD OR TABLE                   
         LA    R2,SCRACT1H                                                      
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM36 - PURCHASE ORDER NUMBER (PO#) RECORDS - ACTLMT'         
***********************************************************************         
*                                                                     *         
*       ROUTINE TO LIMIT NUMBER OF ACTIVITY ELEMENTS TO FIVE          *         
*       (FOUR MOST RECENT PLUS THE FIRST ACTIVITY ELEMENT)            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
ACTLMT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         L     R6,AIO              POINT TO RECORD                              
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,X'FE'          INDICATE SPECIAL                             
*                                                                               
*        COUNT NUMBER OF EXISTING ACTIVITY ELEMENTS                             
*                                                                               
         LA    R4,1                USE R4 FOR BCT LOOP COUNTER                  
*                                                                               
         MVI   ELCODE,PO#ALIDQ     LOOK FOR ACTIVITY ELEM                       
         BRAS  RE,GETEL            FIND IN RECORD                               
         BNE   ACTLMTX             NOTHING FOUND                                
*                                                                               
ACTLMTC  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   ACTLMTCX            NO MORE ACTIVITY ELEMS                       
         AHI   R4,1                                                             
         B     ACTLMTC             LOOK FOR MORE                                
*                                                                               
ACTLMTCX DS    0H                                                               
         CHI   R4,5                FIVE OR MORE ACTIVITY ELEMS ?                
         BL    ACTLMTX             NO - OK - NO DELETION NEEDED                 
*                                                                               
*        DELETE ELEMENTS - LEAVE FIRST ONE AND LAST THREE                       
*                                                                               
         AHI   R4,-4               SET R4 TO NUMBER OF ELEMS TO DELETE          
         L     R6,AIO              POINT TO RECORD                              
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,X'FE'          INDICATE SPECIAL                             
*                                                                               
         MVI   ELCODE,PO#ALIDQ     LOOK FOR ACTIVITY ELEM                       
         BRAS  RE,GETEL            FIND IN RECORD                               
         BE    *+6                                                              
         DC    H'0'                MUST FIND ELEMENT                            
*                                                                               
ACTLMTD  DS    0H                                                               
         BRAS  RE,NEXTEL           GO TO SECOND ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND ELEMENT                            
*                                                                               
ACTLMTDD DS    0H                                                               
         GOTO1 VRECUP,DMCB,,(R6),,=AL2(33,25,4000)     DELETE ELEMENT           
*                                                                               
         BCT   R4,ACTLMTDD         DELETE ANOTHER                               
*                                                                               
ACTLMTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM36 - PURCHASE ORDER NUMBER (PO#) RECORDS - PIDGET'         
***********************************************************************         
*                                                                     *         
*   PIDGET - THIS ROUTINE WILL GET TWO BYTES FROM FATBLOCK            *         
*         WHICH ARE "PERSONAL ID"                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
PIDGET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         XC    SVPID,SVPID         PASSWORD ID NUMBER CLEARED                   
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
*                                                                               
         GOTO1 CGETFACT,DMCB,(2,0),0,0   RETURN TIME IN TUS                     
         DROP  RF                                                               
*                                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
*                                                                               
         MVC   SVSECAGY,FATAGYSC   ALSO NEEDED TO GET CORRECT PID               
*                                                                               
         TM    FATFLAG,X'08'       CHECK IF SECRET CODE IS THERE                
         BZ    *+10                                                             
         MVC   SVPID,FAPASSWD      SAVE PASSWORD ID NUMBER                      
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C25 - INVOICE COMMENTS MAINT/LIST - TRNPID'                  
***********************************************************************         
*                                                                     *         
*        TRANSLATE PID TO A NAME                                      *         
*                                                                     *         
*NTRY    R2 ==>   SCREEN FIELD                                        *         
*        P1       A(PID)                                              *         
*                                                                     *         
*EXIT    PUTS PERSONAL ID IN FIRST FIELD                              *         
*        BUMPS TO NEXT FIELD                                          *         
*        PUTS NAME IN THIS FIELD                                      *         
*                                                                     *         
*NOTE    MORE EXTENSIVE THAN ROUTINE IN OTHER MODULES                 *         
*        THEY JUST DISPLAY THE NAME                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TRNPID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         L     R5,0(R1)            POINT TO PID                                 
*                                                                               
         OC    0(2,R5),0(R5)       SKIP IF NO PID FOUND                         
         BZ    TPIDNOTF                                                         
*                                                                               
*        READ PERSON AUTH REC ON CTFILE                                         
*                                                                               
         LA    R4,KEY                                                           
         USING CT0REC,R4           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
*                                                                               
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   CT0KAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   CT0KAGY,AGENCY         USE BUYREC'S AGENCY                       
*                                                                               
         MVC   CT0KNUM,0(R5)       SET PID                                      
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
         MVC   AIO,AIO1            RESET IOAREA                                 
*                                                                               
         L     R4,AIO2             POINT TO FOUND RECORD                        
*                                                                               
         CLC   CT0KEY,KEYSAVE      SKIP IF RECORD NOT FOUND                     
         BNE   TPIDNOTF                                                         
*                                                                               
*        FIND USER'S ID                                                         
*                                                                               
*        FIND PERSON'S ID ELEMENT                                               
*                                                                               
         LA    RE,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
TPIDCTLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDCTDN                                                         
*                                                                               
         CLI   0(RE),X'C3'         - MATCH ON ELEMENT CODE                      
         BE    TPIDCTFD                                                         
*                                                                               
TPIDCTCN DS    0H                                                               
*                                                                               
         IC    RF,1(RE)            GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDCTLP            GO FIND NEXT ELEMENT                         
*                                                                               
TPIDCTDN DS    0H                  NO PERSON ID FOUND                           
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDCTFD DS    0H                                                               
*                                                                               
*        DISPLAY PERSON ID                                                      
*                                                                               
         MVC   FLDDATA(8),2(RE)    USER'S PID                                   
*                                                                               
         LR    R0,RE               SAVE POINTER                                 
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         LR    RE,R0               RESTORE POINTER                              
*                                                                               
         LA    R3,FLDDATA          USE DATAAREA OF SCREEN FIELD                 
*                                                                               
*        FIND PERSON RECORD                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING SAPEREC,R4          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
*                                                                               
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ    SET RECORD SUB TYPE                          
*                                                                               
         MVC   SAPEAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   SAPEAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   SAPEAGY,AGENCY         USE BUYREC'S AGENCY                       
*                                                                               
         MVC   SAPEPID,2(RE)       SET USERID FROM PREVIOUS RECORD              
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
         MVC   AIO,AIO1            RESET IOAREA                                 
*                                                                               
         L     R4,AIO2             POINT TO FOUND RECORD                        
*                                                                               
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),KEYSAVE SKIP IF REC NOT FOUND           
         BNE   TPIDNOTF                                                         
*                                                                               
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
*        FIND NAME ELEMENT                                                      
*                                                                               
TPIDNMLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDNMDN                                                         
*                                                                               
         USING SANAMD,RE           ESTABLISH AS NAME ELEMENT                    
*                                                                               
         CLI   SANAMEL,SANAMELQ    LOOKING FOR NAME ELEMENT                     
         BE    TPIDNMFD                                                         
*                                                                               
TPIDNMCN DS    0H                                                               
*                                                                               
         IC    RF,SANAMLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDNMLP            GO PROCESS NEXT ELEMENT                      
*                                                                               
TPIDNMDN DS    0H                  NAME ELEMENOT FOUND                          
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDNMFD DS    0H                                                               
*                                                                               
         SR    R0,R0               GET ELEMENT LENGTH                           
         IC    R0,SANAMLN                                                       
         AHI   R0,-SANAMLNQ        DECRMENT BY FIXED LENGTH                     
         BNP   TPIDNOTF            NO NAME IN ELEMENT                           
*                                                                               
         LA    RE,SANAMES          POINT TO START OF PERSON'S NAME              
         SR    RF,RF                                                            
         LA    R1,WORK             BUILD NAME IN WORKAREA                       
         XC    WORK,WORK                                                        
*                                                                               
TPIDFMLP DS    0H                  FORMAT PERSON'S NAME                         
*                                                                               
         USING SANAMES,RE          ESTABLISH NAMES SECTION                      
*                                                                               
         IC    RF,SANAMELN         GET LENGTH OF THIS PART OF NAME              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SANAME      MOVE OUT PART OF NAME                        
*                                                                               
         LA    R1,1(RF,R1)         BUMP TO NEXT OUTPUT AREA                     
*                                                                               
TPIDFMCN DS    0H                                                               
*                                                                               
         SR    R0,RF               DECREMENT REMAINING ELEMENT LENGTH           
         AHI   R0,-2               FOR NAME LENGTH BYTE & EX LENGTH             
         BNP   TPIDFMDN              END OF ELEMENT REACHED                     
*                                                                               
         LA    RE,2(RF,RE)         POINT TO NEXT PART OF NAME                   
         LA    R1,1(R1)            ADD IN A SPACING CHARACTER                   
*                                                                               
         B     TPIDFMLP                                                         
*                                                                               
TPIDFMDN DS    0H                                                               
*                                                                               
         B     TPIDSQSH                                                         
*                                                                               
TPIDNOTF DS    0H                  PRINT 'UNKNOWN' IF NO PID                    
*                                                                               
         MVC   WORK(7),=CL7'UNKNOWN'                                            
         LA    R1,WORK+7           POINT TO NEXT OUTPUT POSITION                
*                                                                               
TPIDSQSH DS    0H                                                               
*                                                                               
         LR    R0,R1               END OF OUTPUT MINUS START                    
         LA    RF,WORK             START OF WORKAREA                            
         SR    R0,RF               EQUALS OUTPUT LENGTH                         
*                                                                               
         GOTO1 SQUASHER,DMCB,WORK,(R0) SQUASH NAME                              
*                                                                               
*        MOVE NAME TO SCREEN                                                    
*                                                                               
         LTR   R2,R2               IF NO SCREEN FIELD GIVEN                     
         BNZ   TPIDSCR                                                          
*                                                                               
         LR    RF,R6                  GET RETURN AREA LENGTH                    
         BCTR  RF,0                   DECREMENT FOR EXECUTE                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES         INIT OUT PUT AREA                         
*                                                                               
         L     RF,4(R1)               SAVE SQUASHED LENGTH                      
*                                                                               
         CR    RF,R6                  IF NAME TOO LONG                          
         BNH   *+6                                                              
         LR    RF,R6                     USE MAX FOR RETURN AREA                
*                                                                               
         B     TPIDMVC                                                          
*                                                                               
TPIDSCR  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           GET TOTAL LENGTH OF FIELD                    
         AHI   RF,-(FLDDATA-FLDHDRD)  DECREMENT BY HEADER LENGTH                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         AHI   RF,-8                  DECREMENT BY EXTENDED HDR LENGTH          
*                                                                               
         STC   RF,FLDOLEN          SET MAX OUTPUT LENGTH                        
*                                                                               
TPIDMVC  DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK        DISPLAY NAME                                 
*                                                                               
TRNPIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T41C36  PRINT PO# RECORD - TSTNTRY'                             
***********************************************************************         
*                                                                     *         
*        TEST IF ANY FIELD ENTERED THIS TIME                          *         
*                                                                     *         
***********************************************************************         
*                                                                               
         DS    0D                  ALIGNMENT                                    
TSTNTRY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,SCRPAGEH         POINT TO FIRST FIELD ON SCREEN               
*                                                                               
TNTRLOOP DS    0H                                                               
*                                                                               
         CLI   0(R2),0             DONE AT END OF SCREEN                        
         BE    TNTRDONE                                                         
*                                                                               
         TM    1(R2),X'20'         SKIP IF PROTECTED FIELD                      
         BO    TNTRCONT                                                         
*                                                                               
         TM    4(R2),X'80'         TEST IF FIELD ENTERED THIS TIME              
         BO    TNTRNTRD                                                         
*                                                                               
TNTRCONT DS    0H                                                               
*                                                                               
         LLC   RF,0(R2)            FIELD LENGTH                                 
         LA    R2,0(RF,R2)         BUMP TO NEXT FIELD                           
         B     TNTRLOOP                                                         
*                                                                               
TNTRDONE DS    0H                  NO FIELD ENTERED THIS TIME                   
         CR    RB,RB               SET EQ CC                                    
         B     TSTNTRYX                                                         
*                                                                               
TNTRNTRD DS    0H                  SOME FIELD ENTERED                           
         LTR   RB,RB               SET NEQ CC                                   
*                                                                               
TSTNTRYX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T41C36  PRINT PO# RECORD - PFK'                                 
***********************************************************************         
*                                                                     *         
*        PF KEY HIT                                                   *         
*                                                                     *         
***********************************************************************         
*                                                                               
PFK      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PHSCREEN,X'DD'      DONE IF NOT DETAIL SCREEN                    
         BNE   PFKX                                                             
*                                                                               
         CLI   PFAID,7             ONLY PF7 & PF8 RECOGNIZED                    
         BE    *+8                                                              
         CLI   PFAID,19                                                         
         BE    *+8                                                              
         CLI   PFAID,8                                                          
         BE    *+8                                                              
         CLI   PFAID,20                                                         
         BNE   PFKX                                                             
*                                                                               
         TP    SVPAGE              IF PAGE # NOT KNOWN                          
         BZ    *+14                                                             
         ZAP   SVPAGE,=P'1'           DEFAULT TO PAGE 1                         
         B     PFKPGOUT               GO DISPLAY PAGE NUMBER                    
*                                                                               
         CLI   PFAID,7             IF PAGE UP                                   
         BE    *+8                                                              
         CLI   PFAID,19            IF PAGE UP                                   
         BNE   *+14                                                             
         SP    SVPAGE,=P'1'           DECREMENT PAGE NUMBER                     
         B     PFKPGOUT                                                         
*                                                                               
         CLI   PFAID,8             IF PAGE DOWN                                 
         BE    *+8                                                              
         CLI   PFAID,20            IF PAGE DOWN                                 
         BNE   PFKPGOUT                                                         
*                                                                               
         CLC   SCRID8,=3C'0'       AND SECOND COLUMN HAS DATA                   
         BNH   *+14                                                             
         AP    SVPAGE,=P'1'           INCREMENT PAGE NUMBER                     
         B     PFKPGOUT                                                         
*                                                                               
         ZAP   SVPAGE,=P'1'        ELSE REVERT TO PAGE 1                        
*                                                                               
PFKPGOUT DS    0H                                                               
*                                                                               
         FOUT  SCRPAGEH,SPACES,3   INIT PAGE ON SCREEN                          
*                                                                               
*        DISPLAY NEW PAGE NUMBER                                                
*                                                                               
         EDIT  (P8,SVPAGE),SCRPAGE,0,ALIGN=LEFT                                 
*                                                                               
         STC   R0,SCRPAGEH+5       SET FIELD LENGTH                             
*                                                                               
         OI    GENSTAT2,RETEQSEL   RETURN TO THIS SCREEN                        
*                                                                               
PFKX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPCON45 - PUB LIST POSTING - LNTBL'                             
***********************************************************************         
*                                                                     *         
*        TABLE OF LINE DISPLACEMENTS INTO SCREEN                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
         DC    XL8'00'                                                          
LNTBL    DS    0D                                                               
         DC    Y(SCRPO#1H-T41CFFD)  PURCHASE ORDER 1                            
         DC    Y(SCRPO#2H-T41CFFD)  PURCHASE ORDER 2                            
         DC    Y(SCRPO#3H-T41CFFD)  PURCHASE ORDER 3                            
         DC    Y(SCRPO#4H-T41CFFD)  PURCHASE ORDER 4                            
         DC    Y(SCRPO#5H-T41CFFD)  PURCHASE ORDER 5                            
         DC    Y(SCRPO#6H-T41CFFD)  PURCHASE ORDER 6                            
         DC    Y(SCRPO#7H-T41CFFD)  PURCHASE ORDER 7                            
         DC    Y(SCRPO#8H-T41CFFD)  PURCHASE ORDER 8                            
         DC    Y(SCRPO#9H-T41CFFD)  PURCHASE ORDER 9                            
         DC    Y(SCRPO#AH-T41CFFD)  PURCHASE ORDER 10                           
         DC    Y(SCRPO#BH-T41CFFD)  PURCHASE ORDER 11                           
         DC    Y(SCRPO#CH-T41CFFD)  PURCHASE ORDER 12                           
         DC    Y(SCRPO#DH-T41CFFD)  PURCHASE ORDER 13                           
LNTBLLS  DC    Y(SCRPO#LH-T41CFFD)  PURCHASE ORDER LAST                         
         DC    4X'FF'               END OF TABLE                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         TITLE 'PRSFM36 - PURCHASE ORDER NUMBER (PO#) RECS - SUBROUTS'          
***********************************************************************         
* COMMONLY ADDRESSABLE ROUTINES                                       *         
***********************************************************************         
         SPACE 1                                                                
SUBROUTS DS    0D                                                               
***********************************************************************         
*                                                                     *         
* HANDLE FIELD IN ERROR - R1 -POINTS TO FIELD                         *         
*         HIGHLIGHT FIELD                                             *         
*         ERROR MESSAGE IS IN ERROR                                   *         
*         IF SAVMSGNO IS NOT FVFOK THEN THIS IS NOT FIRST ERROR       *         
*            ROUTINE RESTORES ERROR TO SAVMSGNO                       *         
*         ELSE                                                        *         
*            ROUTINE SETS CURSOR TO THIS FIELD                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         USING FLDHDRD,R1          ESTABLISH HEADER                             
ERRFLD   OI    IPSTAT,LUWVERRQ     INDICATE VALIDATION ERROR                    
         OI    FLDATB,FATBHIGH     HIGHLIGHT FIELD                              
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         NI    FLDIIND,X'FF'-FINPVAL TURN OFF VALID INDICATOR                   
         CLI   SAVMSGNO,0          IF NOT FIRST ERROR                           
         JE    *+14                                                             
         MVC   ERROR,SAVMSGNO      RESTORE PRIOR MESSAGE                        
         J     ERRFLDX                                                          
*                                                                               
         ST    R1,ACURFORC         PUT CURSOR HERE                              
*                                                                               
ERRFLDX  DS    0H                                                               
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER LITTLE ROUTINES                                               *         
***********************************************************************         
         SPACE 2                                                                
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
         SPACE 2                                                                
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         JNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
NEEDDATA EQU   83                  AT LEAST ONE ENTRY REQUIRED                  
DUPEDATA EQU   179                 DUPLICATE DATA                               
RECFULL  EQU   180                 RECORD FULL                                  
*                                                                               
         EJECT                                                                  
*                                                                               
HEDSPECS DS    0H                                                               
         SPROG 0,1                                                              
         PSPEC H1,51,C'PURCHASE ORDER NUMBER (PO#) REPORT'                      
         PSPEC H2,51,C'----------------------------------'                      
         PSPEC H1,95,AGYNAME                                                    
         PSPEC H2,95,AGYADD                                                     
         PSPEC H4,95,RUN                                                        
         PSPEC H5,95,REPORT                                                     
         PSPEC H5,115,PAGE                                                      
         PSPEC H1,1,REQUESTOR                                                   
         PSPEC H7,01,C'EST'                                                     
         PSPEC H8,01,C'---'                                                     
         PSPEC H7,06,C'ESTIMATE NAME'                                           
         PSPEC H8,06,C'--------------------'                                    
         PSPEC H7,29,C'PO#'                                                     
         PSPEC H8,29,C'-------------------------'                               
         PSPEC H7,57,C'EFF. INS. DATES'                                         
         PSPEC H8,56,C'-----------------'                                       
         PSPEC H7,81,C'AMOUNT'                                                  
         PSPEC H8,75,C'---------------'                                         
         PSPEC H7,92,C'STATUS'                                                  
         PSPEC H8,92,C'------'                                                  
         PSPEC H7,100,C'USED IN BUY ?'                                          
         PSPEC H8,100,C'-------------'                                          
         DC    X'00'                                                            
PUBZNM   DS    CL20                                                             
MYPUB    DS    XL6                                                              
MYDSKADD DS    XL4                                                              
         EJECT                                                                  
         PRINT GEN                                                              
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMDED          LIST SCREEN                                  
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMDDD          RECORD SCREEN                                
*                                                                               
         DS    XL64                AREA FOR BOOK= NOTICE                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    F                                                                
ORIGKEY  DS    XL(L'KEY)                                                        
MYKEY    DS    XL(L'KEY)                                                        
SCANBLK  DS    CL70                                                             
SAVMSGNO DS    XL1                 CURRENT MESSAGE NUMBER SAVEAREA              
SAVCURI  DS    XL1                 INDEX OF ERROR INTO FIELD                    
IPSTAT   DS    XL1                 CUMULATIVE INPUT STATISTICS                  
NEWKEY   DS    XL1                 C'Y' - BASIC KEY HAS CHENGED                 
QPID     DS    XL2                 USER PID                                     
*                                                                               
RECDONE  DS    C                   "X" = NO MORE ELEMENTS                       
NOPO#    DS    C                   "X" = NOTHING ON SCREEN LINE                 
*                                                                               
SVMAXID  DS    H                   HIGHEST CURRENT PO# INTERNAL ID              
SCRNEND  DS    F                   LAST SCREEN ACTION FIELD ADDRESS             
SV#DID   DS    XL2                 PO# INTERNAL ID                              
ELEMCNT  DS    X                   NO OF DETAIL ELEMENTS IN RECORD              
SVLNACTV DS    XL1                 ACTIVITY FOR LINE                            
*                                                                               
SORTNXT  DS    F                   A(NEXT SLOT IN AIO2 FOR SORT ELEM)           
SVPAGE   DS    PL8                 PAGE NUMBER                                  
*                                  # OF PO#'S ON HALF PAGE                      
PAGE#Q   EQU   ((SCRACTLH-SCRACT1H)/(SCRACT2H-SCRACT1H))/2                      
*                                                                               
SVPID    DS    XL2                 CHANGER'S PID                                
SVSCID   DS    XL8                 CHANGER'S SECURITY PID                       
SVSECAGY DS    XL(L'FATAGYSC)      SECURITY AGENCY                              
ALINCUR  DS    A                   A(CURSOR)                                    
SVACTELM DS    XL(PO#ALNGQ)        ACTIVITY ELM SAVEAREA                        
*                                                                               
SVPO#KEY DS    XL48                KEY SAVEAREA                                 
SVCLTPO  DS    CL32                CLIENT PO ELM SAVEAREA                       
*                                                                               
DATENOW  DS    XL3                 TODAY'S DATE (YMD)                           
*                                                                               
DELFLAG  DS    C                   "D"=ELEMENT IS TO BE DELETED                 
*                                                                               
PRTRDSW  DS    C                   "X"=READ THIS RECORD                         
*                                                                               
PCLT     DS    CL3                 CLIENT CODE FOR REPORT                       
PPRD     DS    CL3                 PRODUCT CODE FOR REPORT                      
PEST     DS    XL2                 ESTIMATE CODE FOR REPORT                     
PPEST    DS    CL3                 EBCDIC ESTIMATE CODE FOR REPORT              
*                                                                               
ESTSTRT  DS    XL3                 YMD ESTIMATE START DATE                      
ESTEND   DS    XL3                 YMD ESTIMATE END   DATE                      
*                 BELOW TWO FIELDS USED TO TEST FOR "OVERLAPPING" DATES         
TSTDTES1 DS    XL6                 YMDYMD START-END DATES FROM SCREEN           
TSTDTES2 DS    XL6                 YMDYMD START-END DATES FROM SCREEN           
*                                                                               
SVACHGS  DS    0XL6                ACTIVITY BYTES                               
SVACH1   DS    XL1                 ACTIVTY INDICATOR                            
SVACH2   DS    XL1                 ACTIVTY INDICATOR                            
SVACH3   DS    XL1                 ACTIVTY INDICATOR                            
SVACH4   DS    XL1                 ACTIVTY INDICATOR                            
SVACH5   DS    XL1                 ACTIVTY INDICATOR                            
SVACH6   DS    XL1                 ACTIVTY INDICATOR                            
         DS    0F                                                               
*                                                                               
         DS    0F                                                               
       ++INCLUDE DDBSRPRMD                                                      
ELTMAX   EQU   15                  MAX NUMBER OF ELEMENTS IN TABLE              
*                                                                               
SORTROWA EQU   65             DISPLACEMENT TO A(ROW) IN SORT ELM                
SORTLNTH EQU   69             FIXED LENGTH OF ELEMENTS TO BE SORTED             
*                             (MAX DETAIL ELEM LENGTH OF 63 PLUS 2+4)           
         DS    0F                                                               
*                                                                               
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
*                                                                               
LISTD    DSECT                                                                  
         DS    CL1                                                              
LCLT     DS    CL3                 CLIENT                                       
         DS    CL2                                                              
         DS    CL3                                                              
LPRD     DS    CL3                 PRODUCT                                      
         DS    CL2                                                              
         DS    CL4                                                              
LEST     DS    CL3                 ESTIMATE                                     
         DS    CL2                                                              
*                                                                               
*        REPORT LINE                                                            
*                                                                               
PLINED   DSECT                     PRINT LINE                                   
PLINE    DS    0CL132              PRINT/LIST LINE                              
PLEST    DS    CL3                 ESTIMATE NUMBER                              
         DS    CL2                                                              
PLESTNM  DS    CL20                ESTIMATE NAME                                
         DS    CL3                                                              
PLPPO#   DS    CL25                PO#                                          
         DS    CL2                                                              
PLPDAT   DS    CL17                PO# PERIOD                                   
         DS    CL2                                                              
PLPAMT   DS    CL15                PO# AMOUNT                                   
         DS    CL2                                                              
PLPSTA   DS    CL6                 PO# STATUS                                   
         DS    CL2                                                              
PLPUSED  DS    CL1                 Y OR N (USED IN BUY)                         
         DS    CL1                                                              
*                                                                               
         EJECT                                                                  
PPO#RECD DSECT                                                                  
       ++INCLUDE PPGENPO#                                                       
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
PPRDRECD DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
PESTRECD DSECT                                                                  
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
*DDSPOOLD                                                                       
*DDCOMFACS                                                                      
*DDFLDIND                                                                       
*DDFLDHDR                                                                       
*PPSRCHPARM                                                                     
*FAGETTXTD                                                                      
*DDLINUPD                                                                       
*FAFACTS                                                                        
*FATIOB                                                                         
*PRWRIEQUS                                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE PPSRCHPARM                                                     
         EJECT                                                                  
       ++INCLUDE DDLINUPD                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE PRWRIEQUS                                                      
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE SEACSFILE                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'212PRSFM36   07/08/13'                                      
         END                                                                    
*                                                                               
