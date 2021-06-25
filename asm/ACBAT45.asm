*          DATA SET ACBAT45    AT LEVEL 015 AS OF 12/03/01                      
*PHASE T61B45A                                                                  
         TITLE 'OVERLAY FOR SALE/USE TAX POSTINGS-TIME SHEET VERSION'           
T61B45   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,T61B45,R8,CLEAR=YES                                    
         USING LWSD,RC                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9              R9=GLOBAL W/S                                
         L     RA,ATWA0                                                         
         USING TWAD,RA             RA=TWA                                       
         L     RE,0(R1)            A(SALES TAX BLOCK)                           
         ST    RE,ADTAXB                                                        
*                                                                               
         OI    BASSRVH+1,X'01'     MODIFY SERVICE REQUEST SO YOU                
*                                  ALWAYS REVALIDATE                            
*                                                                               
*                                                                               
         CLI   CSACT,ACTINP        ACTION ITEM/INPUT                            
         BE    MAIN                                                             
*                                                                               
         USING TXD,R4                                                           
         LA    R4,TAXJOBH                                                       
         LA    R5,MXLNES                                                        
         MVI   LINE,1                                                           
IC10     CLC   LINE,CSOLINE        IS THIS THE LINE TO CHANGE                   
         BNE   IC40                NO                                           
         BAS   RE,TRANSMIT         TRANSMIT IT                                  
         LR    R2,R4               SET CURSOR                                   
         B     *+8                                                              
IC40     BAS   RE,PROTECT          CLEAR AND PROTECT ALL OTHERS                 
         ZIC   R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,LINE                                                          
         LA    R4,TXLNQ(R4)        DO NEXT LINE                                 
         BCT   R5,IC10                                                          
*                                                                               
         CLI   CSOMODE,CSOMPCHA    FIRST ITEM CHANGE CALL                       
         BE    INIT15              JUST XMIT SCREEN                             
         B     VAL                                                              
*                                                                               
MAIN     MVI   CSOLINE,1           INIT FOR ADDITE                              
         CLI   0(RE),C'B'                                                       
         BE    INIT10              FIRST TIME - MUST INITIALIZE                 
         CLI   0(RE),C'D'                                                       
         BE    INIT10              CALLED VIA PF9                               
         CLI   PFKEY,11                                                         
         BE    END10               PF=11 NO MORE INPUT  - RETURN                
         CLI   PFKEY,10                                                         
         BE    NEXT10              PF=10 REFRESH SCREEN FOR NEXT                
         CLI   PFKEY,0                                                          
         BE    VAL                 PF=0 'ENTER' - OK TO EDIT                    
BADPFK   LA    R2,CONACTH                                                       
         MVI   ERRNUM,251          ERROR INVALID PF KEY                         
         B     EXIT                                                             
         EJECT                                                                  
*              SAVE THE CURRENT SCREEN TWA0 IN TWA3                             
*                                                                               
INIT10   EQU   *                                                                
         GOTO1 ANTRSES,0                                                        
*                                                                               
*              GET SALES TAX SCREEN                                             
*                                                                               
         MVI   CSSPROG,1                                                        
         GOTO1 AOVRSCR,BOPARM,(X'ED',BASOLY2H)                                  
         MVC   TAXPASS,ADTAXB                                                   
*                                                                               
INIT15   LA    RE,CONTABH              TRANSMIT SCREEN                          
         SR    R1,R1                                                            
INIT20   OI    6(RE),X'80'                                                      
         OI    7(RE),X'80'                                                      
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BNE   INIT20                                                           
         XC    1(2,RE),1(RE)                                                    
         CLI   CSOMODE,CSOMPCHA    FIRST ITEM CHANGE CALL                       
         BE    EXIT                DATA SUPPLIED BY ITE/DIS º ITE/CHA           
*                               FILL IN ANY INFO PASSED FROM CALLER             
*                                                                               
         L     RE,ADTAXB          MAKE SURE YOU WERE PASSED DATA                
         CLI   0(RE),C'D'                                                       
         BE    *+8                CALLED VIA PF9 IN BT49                        
         BAS   RE,SETSCRN                                                       
*                                                                               
INIT72   MVI   ERRNUM,X'FE'                                                     
         MVI   MSG,C' '                                                         
         MVC   MSG+1(L'MSG-1),MSG                                               
         MVC   MSG(25),=CL25'INPUT REQUIRED FIELDS'                             
         L     RE,ADTAXB          SCREEN LOADED- OK TO EDIT(NEXT TIME)          
         MVI   0(RE),C'E'                                                       
*                                                                               
         LA    R2,TAXDATEH         FIRST DATA FIELD                             
         XR    R1,R1                                                            
         LA    R3,TAXLSTH                                                       
*                                                                               
         CLI   PFKEY,10            SCREEN REFRESHED                             
         BE    EXIT                LEAVE CURSOR AT FIRST FIELD                  
*                                                                               
INIT80   TM    1(R2),X'20'         SET R2 TO FIRST EMPTY FIELD                  
         BO    INIT90                                                           
         CLI   5(R2),0                                                          
         BE    EXIT                                                             
*                                                                               
INIT90   IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3                                                            
         BNL   EXIT                                                             
         B     INIT80                                                           
         EJECT                                                                  
*              RESTORE SAVED SCREEN                                             
*                                                                               
END10    DS    0H                                                               
         L     RE,ADTAXB                                                        
         MVI   0(RE),C'X'          SET STXMODE TO END                           
         GOTO1 AXITSES                                                          
         MVI   CSSPROG,0           RESET THE PF KEYS                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              CLEAR SCREEN FOR NEXT INPUT                                      
*                                                                               
NEXT10   DS    0H                                                               
         TWAXC TAXDATEH,TAXDATEH                                                
         TWAXC TAXJOBH,TAXTOTLH,PROT=Y     CLEAR THE SCREEN                     
         B     INIT72                       SET-UP FOR NEXT                     
         EJECT                                                                  
*              EDIT THE INPUT                                                   
*                                                                               
*                                                                               
VAL      DS    0H                                                               
         BAS   RE,XMITSCRN         SET TO TRANSMIT SCREEN ON EXIT               
*                                                                               
         LA    R2,TAXDATEH                                                      
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,TAXDATEH                                                   
         BNE   ERRXIT                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+12                                                             
         MVI   ERRNUM,13                                                        
         B     ERRXIT                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,PDATE)                                   
*                                                                               
         EJECT                                                                  
         USING PSTD,R5                                                          
VAL15    ZAP   SCRTOT,=P'0'        SCREEN TOTAL                                 
         LA    R5,PSTABLE                                                       
         LA    R0,4*MXLNES         NUMBER PER LINE * MAX NUMBER                 
*                                                                               
VAL17    MVC   PSTACC,SPACES       INITIALIZE POSTING ENTRY                     
         MVC   PSTNME,SPACES                                                    
         XC    PSTPCT,PSTPCT                                                    
         ZAP   PSTAMT,=P'0'                                                     
         LA    R5,PSTLNQ(R5)                                                    
         BCT   R0,VAL17                                                         
*                                                                               
         USING TXD,R4                                                           
         LA    R4,TAXJOBH          ADDRESS START OF FIRST I/P LINE              
         LA    R5,PSTABLE                                                       
         MVI   GOTDATA,C'N'                                                     
*                                                                               
VAL20    TWAXC TXNAMH,TXNAMH,PROT=Y                                             
         BAS   RE,NEEDVAL                                                       
         BNE   VAL50                                                            
*                                                                               
         BAS   RE,VALJOB                                                        
         BAS   RE,SETGOPS                                                       
         BAS   RE,VALREF                                                        
         BAS   RE,VALBAS           VALIDATE BASIS                               
         BAS   RE,VALOC            VALIDATE LOCALITY                            
         BAS   RE,VALWC            VAL WORK CODE                                
*                                                                               
*              COMPUTE THE TAX AMOUNT FOR EACH ENTRY                            
*              PROPAGATE JOB, BASIS, W/C AND NARRATIVE                          
*                                                                               
         USING PSTD,R5                                                          
VAL40    LA    R0,4                MAX OF 4 POSTING PER LINE                    
VAL43    CLC   PSTACC,SPACES       NO POSTING ACCOUNT- OK TO SKIP               
         BE    VAL45                                                            
*                                                                               
         MVC   PSTJOB,JOB                                                       
         MVC   PSTJBNM,JBNME                                                    
         MVC   PSTCLI,CLI                                                       
         MVC   PSTCLNM,CLNME                                                    
         MVC   PSTOFF,OFFICE                                                    
         ZAP   PSTBAS,THISBASE     SET IN VALBAS                                
         ZAP   PL13,THISBASE       AMOUNT                                       
         MP    PL13,PSTPCT         X PERCENT 4DP                                
         SRP   PL13,64-6,5                                                      
         ZAP   PSTAMT,PL13         TAX AMOUNT                                   
         MVC   PSTWC,THISWC        WORKCODE                                     
         LA    R2,TXNARH                                                        
         XC    PSTNALN,PSTNALN                                                  
         XC    PSTNAR,PSTNAR                                                    
         XR    R6,R6                                                            
         ICM   R6,1,5(R2)                                                       
         BZ    VAL45                                                            
         MVC   PSTNALN,5(R2)                                                    
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   PSTNAR(0),TXNAR                                                  
*                                                                               
VAL45    LA    R5,PSTLNQ(R5)                                                    
         BCT   R0,VAL43                                                         
*                                                                               
         MVI   GOTDATA,C'Y'                                                     
*                                                                               
VAL50    BAS   RE,XMITLINE         SEND FIELDS                                  
*                                                                               
         CLI   CSACT,ACTINP        ACTION ITEM/INPUT                            
         BNE   VAL60                                                            
*                                                                               
         ZIC   R1,CSOLINE          NEED TO KEEP CSOLINE IN SYNCH                
         LA    R1,1(R1)                                                         
         STC   R1,CSOLINE                                                       
*                                                                               
VAL60    LA    R4,TXLNQ(R4)        GET R4 TO NEXT LINE                          
         LA    R3,TAXLSTH                                                       
         CR    R4,R3                                                            
         BL    VAL20               GET NEXT LINE                                
*                                                                               
         CLI   GOTDATA,C'Y'        ANYTHING HAPPEN                              
         BNE   INIT72              NO, ASK FOR INPUT                            
*                                                                               
VAL70    BAS   RE,POSTIT           MAKE THE POSTINGS                            
*                                                                               
         MVC   WORK1,SPACES          SET-UP THE TOTAL LINE                      
         LA    R2,WORK1                                                         
         CP    SCRTOT,=P'0'                                                     
         BE    VAL75                                                            
         MVC   0(13,R2),=C'SCREEN TOTAL='                                       
         LA    R2,13(R2)                                                        
         EDIT  SCRTOT,(11,0(R2)),2,ALIGN=LEFT,FLOAT=-                           
*                                                                               
*                                                                               
VAL75    LA    R3,L'WORK1                                                       
         GOTO1 SQUASHER,DMCB,WORK1,(R3)                                         
         MVC   TAXTOTL,WORK1                                                    
         OI    TAXTOTLH+6,X'80'                                                 
*                                                                               
         MVC   WORK(L'TAXJOB),TAXJOB          AVOID                             
         MVC   TAXJOB+1(L'TAXJOB-1),WORK      DOUBLE INPUT                      
         MVI   TAXJOB,C'*'                                                      
         OI    TAXJOBH+6,X'80'                                                  
*                                                                               
         MVI   ERRNUM,X'FE'                                                     
         MVC   MSG,=CL60'INPUT COMPLETE ENTER NEXT'                             
         LA    R2,TAXLOCH                                                       
         ST    R2,FADR                                                          
*                                                                               
         CLI   CSACT,ACTCHA        ACTION ITEM/CHANGE                           
         BNE   EXIT                                                             
         GOTO1 AXITSES                                                          
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        SEE IF THE LINE AT 0(R4) NEEDS TO BE VALIDATED               *         
*---------------------------------------------------------------------*         
         USING TXD,R4                                                           
NEEDVAL  NTR1                                                                   
         TM    TXJOBH+1,X'20'      JOB FIELD PROTECTED                          
         BO    NEEDNO              YES, DON'T (RE) VAL                          
*                                                                               
         CLI   TXJOBH+5,0          IF ANY INPUT VALIDATE LINE                   
         BNE   NEEDYES                                                          
         CLI   TXLOCH+5,0                                                       
         BNE   NEEDYES                                                          
         CLI   TXBASH+5,0                                                       
         BNE   NEEDYES                                                          
         CLI   TXWCH+5,0                                                        
         BNE   NEEDYES                                                          
NEEDNO   LTR   RB,RB                                                            
         B     NEEDVX                                                           
NEEDYES  CR    RB,RB                                                            
NEEDVX   B     CURSIT                                                           
*---------------------------------------------------------------------*         
*        VALIDATE AN SJ ACCOUNT, GET THE EFFECTIVE SJ OFFICE          *         
*        ASSUMES R4 SET TO START OF LINE COVERED BY TXD               *         
*---------------------------------------------------------------------*         
         USING TXD,R4                                                           
VALJOB   NTR1                                                                   
         LA    R2,TXJOBH                                                        
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,TXJOBH                                                     
         BNE   ERRXIT                                                           
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY            VALIDATE JOB                           
         MVC   KEY+1(2),=C'SJ'                                                  
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)                                                   
         LA    R6,JOBPROF                                                       
         BAS   RE,GETACC                                                        
*                                                                               
*                                                                               
         MVI   BOFLAG1,ACIPRCLI+ACIPRPRO+ACIPRJOB                               
         XC    PSCLICOD,PSCLICOD   CLEAR OUT OLD DATA                           
         XC    PSPROCOD,PSPROCOD                                                
         XC    PSJOBCOD,PSJOBCOD                                                
         GOTO1 AVALCPJ,TXJOBH                                                   
         BNE   ERRXIT                                                           
*                                                                               
         BAS   RE,CHECKACC                                                      
         MVC   JOB,KEY                                                          
         MVC   JBNME,ACCTNAME                                                   
         MVC   TXNAM,JBNME                                                      
         OI    TXNAMH+6,X'80'                                                   
*                                                                               
         MVC   KEY,SPACES                BUILD FOR CLIENT RECORD                
         MVC   KEY(6),JOB                                                       
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVC   CLI,KEY                                                          
         MVC   CLNME,ACCTNAME                                                   
*                                                                               
         MVC   KEY,SPACES                 BUILD FOR PRODUCT RECORD              
         MVC   KEY(9),JOB                                                       
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
         MVC   PRD,KEY                                                          
         MVC   PRNME,ACCTNAME                                                   
*                                                                               
         BAS   RE,PROFMERG                                                      
         LA    R4,PROFILE          GET THE OFFICE CODE                          
         USING ACPROFD,R4                                                       
         MVC   OFFICE,ACPROFFC                                                  
         OC    OFFICE,SPACES                                                    
*                                                                               
         LA    R4,JOB                                                           
         GOTO1 ASETJOB,DMCB,(R4)                                                
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    EXIT                NO                                           
         MVI   ERRNUM,47           YES, ERROR                                   
         B     ERRXIT                                                           
         EJECT                                                                  
*        VALIDATE 4 LEVELS OF LOCALITY                                          
*              ON I/P R4 IS A(CURRENT LINE)                                     
*                     R5 IS A(AREA TO BUILD POSTING INTERFACE)                  
*                                                                               
         USING TXD,R4                                                           
         USING PSTD,R5                                                          
VALOC    NTR1                                                                   
         ZIC   R3,TXLOCH+5                                                      
         LA    R2,TXLOCH                                                        
         OC    TXLOC,SPACES                                                     
*                                    ALLOW DUPLICATES FOR TIMESHEETS            
*                                                                               
VALOC04  MVC   LWKEY,SPACES                                                     
         LA    R7,LWKEY            BUILD TAX KEY                                
         USING ACKEYD,R7                                                        
         MVI   ACUTTYPE,ACUTEQU                                                 
         MVI   ACUTSREC,ACUTSEQU                                                
         MVC   ACUTCMP,COMPANY                                                  
         MVC   ACUTLOC(2),TXLOC                                                 
         BAS   RE,RDHIGH                                                        
         CLC   LWKEY,LWIO                                                       
         BNE   BADLOC               MISSING LEVEL ONE                           
         LA    R6,LWIO                                                          
         BAS   RE,GETAX             POST ACCOUNT/NAME/PCT                       
         CLC   TXLOC+2(2),SPACES                                                
         BNH   EXIT                ONE LEVEL CODE                               
*                                                                               
         LA    R5,PSTLNQ(R5)        R5 TO NEXT POST SAVE AREA                   
         MVC   ACUTLOC(4),TXLOC                                                 
         BAS   RE,RDHIGH                                                        
         CLC   LWKEY,LWIO                                                       
         BNE   BADLOC              MISSING LEVEL TWO                            
         LA    R6,LWIO                                                          
         BAS   RE,GETAX             POST ACCOUNT/NAME/PCT                       
         CLC   TXLOC+4(2),SPACES                                                
         BNH   EXIT                TWO LEVEL CODE                               
*                                                                               
         LA    R5,PSTLNQ(R5)        R5 TO NEXT POST SAVE AREA                   
         MVC   ACUTLOC(6),TXLOC                                                 
         BAS   RE,RDHIGH                                                        
         CLC   LWKEY,LWIO                                                       
         BNE   BADLOC              MISSING LEVEL THREE                          
         LA    R6,LWIO                                                          
         BAS   RE,GETAX            POST ACCOUNT/NAME/PCT                        
         CLC   TXLOC+6(2),SPACES                                                
         BNH   EXIT                THREE LEV CODE                               
*                                                                               
         LA    R5,PSTLNQ(R5)        R5 TO NEXT POST SAVE AREA                   
         MVC   ACUTLOC(8),TXLOC     SET FOR FULL KEY                            
         BAS   RE,RDHIGH                                                        
         CLC   LWKEY,LWIO                                                       
         BNE   BADLOC              MISSING LEVEL FOUR                           
         LA    R6,LWIO                                                          
         BAS   RE,GETAX             POST ACCOUNT/NAME/PCT                       
         B     EXIT                                                             
         EJECT                                                                  
*              VALIDATE WORKCODE                                                
*                                                                               
VALWC    NTR1                                                                   
         LA    R1,TXWCH                                                         
         LA    R2,TXWCH                                                         
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL                                                            
         BNE   ERRXIT                                                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
         LA    R2,TXWCH                                                         
         MVI   ERRNUM,INVWC                                                     
         ST    R2,FVADDR                                                        
         GOTO1 AGETWC,TXWC         VALIDATE WORK CODE                           
         BNE   ERRXIT                                                           
         MVC   THISWC,TXWC                                                      
*                                                                               
         MVI   ERRNUM,INVWC                                                     
         USING GOBLOCKD,R1                                                      
         L     R1,AGOBLOCK                                                      
         LA    R1,GOUWLIST         CHECK IT'S A BILLABLE W/C                    
         LA    R0,6                                                             
         DROP  R1                                                               
*                                                                               
VWC50    CLC   THISWC,0(R1)                                                     
         BE    ERRXIT                                                           
         LA    R1,2(R1)                                                         
         BCT   R0,VWC50                                                         
         B     EXIT                                                             
*                                                                               
*              VALIDATE BASIS                                                   
*                                                                               
VALBAS   NTR1                                                                   
         LA    R2,TXBASH                                                        
         ZIC   R0,5(R2)                                                         
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         LTR   R0,R0                                                            
         BZ    ERRXIT                                                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 AMTVAL,DMCB,8(R2),(R0)  VALID RATE BASIS                         
         MVI   ERRNUM,25                                                        
         CLI   0(R1),0                                                          
         BNE   ERRXIT                                                           
         L     R1,4(R1)                                                         
         LA    R1,0(R1)                                                         
         ZAP   THISBASE,0(8,R1)                                                 
         B     EXIT                                                             
*                                                                               
*              VALIDATE REF NUMBER                                              
*                                                                               
VALREF   NTR1                                                                   
         MVC   PSTREF,SPACES                                                    
         LA    R1,TXREFH                                                        
         LA    R2,TXREFH                                                        
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   ERRXIT                                                           
         ZIC   R1,5(R2)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PSTREF,8(R2)                                                     
         OC    PSTREF,SPACES       ENSURE SPACE FILLED                          
         B     EXIT                                                             
         EJECT                                                                  
*               POST ACCOUNT NAMES/CODES/ RATE TO TABLE                         
*                                                                               
         USING PSTD,R5                                                          
GETAX    NTR1                                                                   
         BAS   RE,GETNAME                                                       
         MVC   TXNAM,WORK     DISPLAY LOCALITY NAME                             
         OI    TXNAMH+6,X'80'                                                   
         MVC   PSTLCNM,WORK        AND SAVE IN POSTING TABLE                    
*                                                                               
         MVI   ELCODE,X'5F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
         B     NORATE                                                           
*                                                                               
         USING ACTAXEL,R6                                                       
GETAX03  CLI   ACTAXLEN,ACTAXLQ2                                                
         BL    *+16                                                             
         MVC   PSTACC(1),COMPANY                                                
         MVC   PSTACC+1(14),ACTAXACC    SAVE THE POSTING ACCOUNT                
         MVC   PSTLOC,LWKEY             SAVE LOCALITY KEY                       
         CLC   ACTAXEFF,PDATE                                                   
         BH    *+16                 RATE NOT YET IN EFFECT                      
         MVC   PSTEFF,ACTAXEFF      SAVE EFFECTIVE DATE                         
         ZAP   PSTPCT,ACTAXRTE      AND RATE                                    
         BAS   RE,NEXTEL                                                        
         BE    GETAX03                                                          
         OC    PSTPCT,PSTPCT                                                    
         BZ    NORATE              NO EFFECTIVE RATE                            
*                                                                               
         MVC   KEY,SPACES          VALIDATE THE CREDIT                          
         MVC   KEY(15),PSTACC                                                   
         SR    R6,R6               NO PROFILES                                  
         BAS   RE,GETACC           GET ACCOUNT NAME                             
         BAS   RE,CHECKACC         CHECK VALID FOR POSTING                      
         MVC   PSTNME,ACCTNAME     SAVE THE NAME                                
*                                                                               
         MVC   PSTLNNUM,CSOLINE    SAVE LINE NUMBER GENERATING THIS             
*                                  INPUT                                        
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
GETNAME  NTR1                                                                   
         MVC   WORK,SPACES                                                      
         MVI   ELCODE,X'20'        GET ACCOUNT NAME INTO WORK                   
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         USING ACNAMED,R6                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     EXIT                                                             
         MVC   WORK(0),ACNMNAME                                                 
         DROP  R6                                                               
         EJECT                                                                  
*              BUILD POSTING RECORDS                                            
*                                                                               
         USING DLDESCD,R2                                                       
         USING PSTD,R5                                                          
POSTIT   NTR1                                                                   
         LA    R5,PSTABLE          POSTING DATA                                 
         LA    R3,MXLNES           MAXIMUM NUMBER OF LINES                      
*                                                                               
POST10   LA    R2,IOAREA+2         SPACE TO BUILD POSTING ELEMENTS              
         MVC   CSOLINE,PSTLNNUM    ADDITE SAVES THIS                            
*                                                                               
         USING DLDESCD,R2                                                       
         MVI   DLDSEL,X'64'                                                     
         MVC   DLDSREF,PSTREF                                                   
         MVC   DOCNO,PSTREF        SAVE FOR DAY FILE                            
         MVC   DLDSDATE,PDATE                                                   
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         XC    DLDSSTAT+1(6),DLDSSTAT+1                                         
         XC    DLDSNARR,DLDSNARR                                                
         XR    R6,R6                                                            
         ICM   R6,1,PSTNALN                                                     
         BZ    POST15                                                           
*                                                                               
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   DLDSNARR(0),PSTNAR                                               
         LA    R6,1(R6)                                                         
*                                                                               
POST15   LA    R1,DLDSNARR                                                      
         SR    R1,R2               R1 = ELEMENT - NARRATIVE                     
         AR    R1,R6               R6 = L'NARRATIVE                             
         STC   R1,DLDSLEN                                                       
         AR    R2,R1               R2 TO NEXT ELEMENT                           
*                                                                               
         LA    R0,4                NUMBER PER LINE                              
         MVI   ACTIVITY,C'N'                    UNT                             
         ZAP   TRANSAMT,=P'0'                                                   
*                                                                               
POST20   CLC   PSTACC,SPACES                                                    
         BE    POST40              NO ENTRY                                     
         MVI   ACTIVITY,C'Y'                                                    
*                                                                               
         USING ACTAXEL,R2                                                       
         XC    0(ACTAXLQ1,R2),0(R2)   ADD TAX ELEMENT                           
         MVI   ACTAXEL,X'5F'                                                    
         MVI   ACTAXLEN,ACTAXLQ2                                                
         MVC   ACTAXEFF,PSTEFF        EFFECTIVE DATE                            
         ZAP   ACTAXRTE,PSTPCT        RATE                                      
         ZAP   ACTAXBAS,PSTBAS        BASIS                                     
         MVC   ACTAXLOC,PSTLOC        LOCALITY KEY                              
         ZIC   R1,ACTAXLEN                                                      
         AR    R2,R1                                                            
*                                                                               
         CLC   PSTACC+1(2),=C'SV'                                               
         BE    POST24                                                           
         CLC   PSTACC+1(2),=C'SX'                                               
         BNE   POST26                                                           
*                                                                               
         USING PAKELD,R2                                                        
POST24   MVI   PAKEL,PAKELQ                                                     
         MVI   PAKLN,PAKLNQ                                                     
         MVC   PAKACC,PSTACC                                                    
         MVC   PAKOFF,PSTOFF                                                    
         MVC   PAKCON,PSTCLI                                                    
         MVC   PAKDATE,PDATE                                                    
         MVC   PAKREF,PSTREF                                                    
         ZIC   R1,PAKLN                                                         
         AR    R2,R1                                                            
*                                                                               
         USING DLPOSTD,R2                                                       
POST26   MVI   DLPSEL,X'69'        DEBIT POSTING                                
         MVI   DLPSLEN,X'71'       LENGTH                                       
         MVC   DLPSDBAC,PSTJOB     JOB ACCOUNT                                  
         MVC   DLPSDBNM,PSTJBNM    JOB NAME                                     
         MVC   DLPSCRAC,PSTACC     CREDIT ACCOUNT  (TAX)                        
         MVC   DLPSCRNM,PSTNME     CREDIT ACCOUNT NAME                          
         MVI   DLPSTYPE,0          MAIN ACCOUNTING ENTRY                        
         ZAP   DLPSAMNT,PSTAMT     AMOUNT                                       
         MVC   DLPSANAL,PSTOFF     OFFICE                                       
         CLC   DLPSDBAC+1(2),=C'SJ'                                             
         BNE   *+10                                                             
         MVC   DLPSANAL,PSTWC      FOR PRODUCTION USE WORKCODE                  
         AP    TRANSAMT,PSTAMT     FOR ITEM TOTAL                               
         AP    SCRTOT,PSTAMT       SCREEN TOTAL                                 
         ZIC   R1,DLPSLEN                                                       
         AR    R2,R1                                                            
         EJECT                                                                  
*                                     SET-UP FOR CREDIT POSTING                 
*                                                                               
*                                                                               
         USING ACOTHERD,R2                                                      
POST30   MVC   ACOTEL(2),=X'230F'       BUILD 'OTHERS' ELEMENT FOR              
         MVC   ACOTNUM(13),SPACES       PRODUCT AND JOB                         
         MVC   ACOTNUM(3),PSTJOB+6      PRODUCT+3 SPACES                        
         MVC   ACOTNUM+6(6),PSTJOB+9    JOB                                     
         ZIC   R1,ACOTLEN                                                       
         AR    R2,R1                                                            
*                                                                               
         USING ACTAXEL,R2                                                       
         XC    0(ACTAXLQ1,R2),0(R2)   ADD TAX ELEMENT                           
         MVI   ACTAXEL,X'5F'                                                    
         MVI   ACTAXLEN,ACTAXLQ2                                                
         MVC   ACTAXEFF,PSTEFF        EFFECTIVE DATE                            
         ZAP   ACTAXRTE,PSTPCT        RATE                                      
         ZAP   ACTAXBAS,PSTBAS        BASIS                                     
         MVC   ACTAXLOC,PSTLOC        LOCALITY                                  
         ZIC   R1,ACTAXLEN                                                      
         AR    R2,R1                                                            
*                                                                               
         USING DLPOSTD,R2                                                       
         MVI   DLPSEL,X'6A'        CREDIT POSTING                               
         MVI   DLPSLEN,X'71'       LENGTH                                       
         MVC   DLPSDBAC,PSTCLI     CONTRA ACCOUNT IS CLIENT                     
         MVC   DLPSDBNM,PSTCLNM                                                 
*                                                                               
         MVC   DLPSCRAC,PSTACC     CREDIT ACCOUNT                               
         MVC   DLPSCRNM,PSTNME     CREDIT ACCOUNT NAME                          
         MVI   DLPSTYPE,0          MAIN ACCOUNTING ENTRY                        
         ZAP   DLPSAMNT,PSTAMT     AMOUNT                                       
         MVC   DLPSANAL,PSTOFF     OFFICE                                       
         ZIC   R1,DLPSLEN                                                       
         AR    R2,R1                                                            
         MVI   0(R2),0                                                          
*                                                                               
POST40   LA    R5,PSTLNQ(R5)       UP TO 4 POSTINGS PER LINE                    
         BCT   R0,POST20                                                        
         CLI   ACTIVITY,C'Y'                                                    
         BNE   POST50                                                           
         LA    RF,IOAREA-1         GET LENGTH OF IOAREA                         
         SR    R2,RF                                                            
         STH   R2,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         CLI   TAXPASS,0                                                        
         BNE   POST45                                                           
         BAS   RE,PUTDAY           ADD THE ACCDAY FILE                          
*                                                                               
*              ADD ENTRY TO TWA1                                                
*                                                                               
POST45   XC    WORK(20),WORK                                                    
         MVC   WORK(6),DOCNO                                                    
         MVC   WORK+10(4),DISKADDR                                              
         OI    WORK+10,X'80'       TAX ITEM - TURN 80 OF DISKSDDR               
         CLI   TAXPASS,0                                                        
         BNZ   POST50                                                           
         BAS   RE,ADTWA1                                                        
*                                                                               
POST50   EQU   *                   SET R2 FOR NEXT ITEM                         
         BCT   R3,POST10           UP TO MAX ON SCREEN                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
SETSCRN  NTR1                                                                   
         USING TXD,R4                                                           
         USING STXD,R3                                                          
         L     R3,ADTAXB                                                        
         LA    R4,TAXJOBH          FIRST DATA FIELD                             
         ZIC   R5,1(R3)            NUMBER OF LINES SENT                         
         LA    R3,2(R3)            BUMP INTO DATA                               
*                                                                               
         MVC   TAXDATE,STXDTE      DATE TO SCREEN                               
*                                                                               
SS10     MVC   TXJOB(12),STXACC+3  JOB TO SCREEN                                
         LA    R2,TXJOBH                                                        
         MVI   5(R2),12                                                         
         OI    1(R2),X'80'         DON'T DELETE TRAILING BLANKS                 
         OI    4(R2),X'80'         INPUT THIS TIME                              
         OC    TXJOB,SPACES                                                     
         BAS   RE,VALJOB           VALIDATE JOB FIELD (CALL SETJOB, ETC         
*                                                                               
         BAS   RE,SETGOPS          SET LOCACTIO AND W/C FROM GETOPT             
*                                                                               
         MVC   TXREF,STXREF          REFERENCE                                  
         MVI   5(R2),L'TXREF                                                    
         OI    1(R2),X'80'         DON'T DELETE TRAILING BLANKS                 
         OI    4(R2),X'80'         INPUT THIS TIME                              
*                                                                               
         MVC   IOAREA(L'TXNAR),SPACES                                           
         OC    STXNARR(L'SPACES),SPACES                                         
         CLC   STXNARR(L'SPACES),SPACES                                         
         BE    SS40                                                             
*                                  CHOP AND DISPLAY NARRATIVE                   
         LA    R2,L'TXNAR                                                       
*                                                                               
         GOTO1 CHOPPER,DMCB,((R2),STXNARR),(L'TXNAR,IOAREA),1                   
         L     R2,DMCB+8                                                        
         LTR   R2,R2                                                            
         BZ    SS40                                                             
         MVC   TXNAR,IOAREA                                                     
*                                                                               
SS40     OC    STXAMT,STXAMT                                                    
         BNZ   *+10                                                             
         ZAP   STXAMT,=P'0'         NO TAX AMOUNT PASSED                        
         MVC   WORK1(11),SPACES                                                 
         CP    STXAMT,=P'0'                                                     
         BE    SS70                                                             
         EDIT  STXAMT,(11,WORK1),2,ALIGN=LEFT,FLOAT=-                           
         LA    R2,TXBASH                                                        
         STC   R0,5(R2)                                                         
         OI    4(R2),X'80'         INPUT THIS TIME                              
*                                                                               
SS70     MVC   TXBAS,WORK1        DEFAULT IS TRANSACTION AMOUNT                 
         BAS   RE,XMITLINE         TRANSMIT LINE                                
         LA    R4,TXLNQ(R4)                                                     
         LA    R3,STXLNQ(R3)                                                    
         BCT   R5,SS10                                                          
*                                                                               
         BAS   RE,XMITSCRN                                                      
         OI    CSINDSL2,CSIACFRM                                                
*                                                                               
         B     EXIT                                                             
*-------------------------------------------------------------------            
*        AFTER VALJOB, SET LOCATION AND WC, IF NEEDED                           
*-------------------------------------------------------------------            
*                                                                               
SETGOPS  NTR1                                                                   
         USING GOBLOCKD,R3                                                      
         L     R3,AGOBLOCK                                                      
         L     R3,AGOXBLK                                                       
         USING GOXBLKD,R3                                                       
         LA    R2,TXWCH                                                         
*                                                                               
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,TXWCH                                                      
         CLI   FVILEN,0                                                         
         BNE   SG20                                                             
*                                                                               
         CLC   GOTXWC,SPACES                                                    
         BNH   SG20                                                             
         MVI   5(R2),L'TXWC                                                     
         OI    1(R2),X'80'         DON'T DELETE TRAILING BLANKS                 
         OI    4(R2),X'80'         INPUT THIS TIME                              
         CLC   TXWC,SPACES                                                      
         BH    *+10                                                             
         MVC   TXWC,GOTXWC                                                      
*                                                                               
SG20     LA    R2,TXLOCH                                                        
*                                                                               
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,TXLOCH                                                     
         CLI   FVILEN,0            ANY THING IN FIELD                           
         BNE   SGX                 YES, USE IT                                  
*                                                                               
         MVI   5(R2),0                                                          
         CLC   GOTXLOC,SPACES                                                   
         BNH   SGX                                                              
*                                                                               
         LA    RE,GOTXLOC                                                       
         LA    RF,TXLOC                                                         
         XR    R1,R1               LENGTH COUNTER                               
SG22     CLI   0(RE),C' '                                                       
         BNH   SG25                                                             
         MVC   0(1,RF),0(RE)                                                    
         LA    R1,1(R1)                                                         
         C     R1,=F'8'            MAX LEN IS 8                                 
         BE    SG25                                                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     SG22                                                             
*                                                                               
SG25     STC   R0,5(R2)                                                         
         OI    4(R2),X'80'         INPUT THIS TIME                              
         CLC   TXLOC,SPACES                                                     
         BH    *+10                                                             
         MVC   TXLOC,GOTXLOC                                                    
*                                                                               
*        OI    CSINDSL2,CSIACFRM   SET AWAIT CONFIRMATION BIT                   
SGX      B     EXIT                                                             
         EJECT                                                                  
XMITLINE EQU   *                   TRANSMIT A LIMES WORTH OF DATA               
         LA    R2,TXJOBH                                                        
         OI    6(R2),X'80'                                                      
         LA    R2,TXLOCH                                                        
         OI    6(R2),X'80'                                                      
         LA    R2,TXREFH                                                        
         OI    6(R2),X'80'                                                      
         LA    R2,TXNARH                                                        
         OI    6(R2),X'80'                                                      
         LA    R2,TXWCH                                                         
         OI    6(R2),X'80'                                                      
         LA    R2,TXNAMH                                                        
         OI    6(R2),X'80'                                                      
         BR    RE                                                               
*                                                                               
XMITSCRN LA    R2,BASOLY2H                                                      
         SR    RF,RF                                                            
XS40     IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   XS40                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT WHOLE SCREEN           
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
RDHIGH   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCOUNT',LWKEY,LWIO               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
BADLOC   MVI   ERRNUM,NOTFOUND        RECORD NOT FOUND                          
         MVC   BADACCNT(8),LWKEY+3    LOCALITY CODE                             
         OC    BADACCNT(8),SPACES                                               
         B     ERRXIT                                                           
*                                                                               
NORATE   MVI   ERRNUM,X'FE'                                                     
         MVC   MSG(20),=CL30'NO TAX RATE FOR'                                   
         MVC   XTRAMESS(8),LWKEY+3    LOCALITY CODE                             
         OC    XTRAMESS(8),SPACES                                               
         B     ERRXIT                                                           
*                                                                               
DUPERR   MVI   ERRNUM,35                                                        
         B     ERRXIT                                                           
         EJECT                                                                  
***********************************************************************         
*              TRANSMIT A LINE DURING ITEM/CHANGE                     *         
***********************************************************************         
*                                                                               
*              R4 = A(START OF LINE)                                            
*                                                                               
TRANSMIT NTR1                                                                   
         LA    R0,NFIELDS          #FIELDS/LINE                                 
*                                                                               
TRAN100  NI    4(R4),X'FF'-X'20'   MARK AS NOT PREVIOUSLY VALIDATED             
         OI    6(R4),X'80'         TRANSMIT                                     
         ZIC   R1,0(R4)            BUMP TO NEXT FIELD                           
         AR    R4,R1                                                            
         BCT   R0,TRAN100                                                       
*                                                                               
TRANSX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CLEAR & PROTECT LINE                                   *         
***********************************************************************         
*                                                                               
*              R4 = A(START OF LINE)                                            
*                                                                               
PROTECT  NTR1                                                                   
         LA    R0,NFIELDS          #FIELDS/LINE                                 
*                                                                               
PROT100  TWAXC 0(R4),0(R4),PROT=Y                                               
         CLI   CSOMODE,CSOMPLIN    BATCH GENERATE                               
         BE    *+8                                                              
         OI    1(R4),X'20'         PROTECT FIELD                                
         OI    4(R4),X'20'         MARK PREVIOUSLY VALIDATED                    
         MVI   5(R4),0             CLEAR INPUT LENGTH                           
         OI    6(R4),X'80'         TRANSMIT                                     
         ZIC   R1,0(R4)            BUMP TO NEXT FIELD                           
         AR    R4,R1                                                            
         BCT   R0,PROT100                                                       
*                                                                               
PROTX    B     EXIT                                                             
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*              CONSTANTS                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR LOCAL W/S                                              
*                                                                               
LWSD     DSECT                                                                  
ADTAXB   DS    A                   A(USERS INPUT BLOCK)                         
SAVRE    DS    F                                                                
ELCODE   DS    CL1                                                              
ACTIVITY DS    CL1                                                              
SCRTOT   DS    PL6                                                              
PL13     DS    PL13                                                             
*                                                                               
OFFICE   DS    CL2                                                              
DOCNO    DS    CL6                                                              
PDATE    DS    CL3                                                              
LINE     DS    CL1                                                              
GOTDATA  DS    CL1                                                              
*                                                                               
JOB      DS    CL15                JOB     CODE                                 
JBNME    DS    CL36                        NAME                                 
PRD      DS    CL15                PRODUCT CODE                                 
PRNME    DS    CL36                        NAME                                 
CLI      DS    CL15                CLIENT  CODE                                 
CLNME    DS    CL36                        NAME                                 
KEY      DS    CL49                                                             
         SPACE 2                                                                
*                                                                               
LWKEY    DS    CL42                                                             
LWIO     DS    1000C                                                            
MXLNES   EQU   6                   MAXIMUM NUMBER OF LINES                      
         DS    0F                                                               
IOAREA   DS    2000C                                                            
PSTABLE  DS    (MXLNES*PSTLINE)C  TABLE OF POSTING ACCOUNTS (14 LINES)          
LWSX     DS    0C                                                               
         EJECT                                                                  
*                                                                               
*              DSECT TO COVER DATA IN TAX BUFFER                                
STXD     DSECT                                                                  
       ++INCLUDE ACBATSTAX                                                      
         EJECT                                                                  
*              DSECT TO COVER INPUT LINE                                        
TXD      DSECT                                                                  
TXJOBH   DS    CL(L'TAXJOBH)       JOB CODE                                     
TXJOB    DS    CL(L'TAXJOB)                                                     
TXJOBX   DS    CL(L'TAXJOBX)                                                    
TXREFH   DS    CL(L'TAXREFH)       REFERENCE NUMBER                             
TXREF    DS    CL(L'TAXREF)                                                     
TXREFX   DS    CL(L'TAXREFX)                                                    
TXBASH   DS    CL(L'TAXBASH)       HEADER FOR TAX BASIS                         
TXBAS    DS    CL(L'TAXBAS)        TAX BASIS                                    
TXBASX   DS    CL(L'TAXBASX)       TRAILER FOR TAX BASIS                        
TXLOCH   DS    CL(L'TAXLOCH)       LOCALITY                                     
TXLOC    DS    CL(L'TAXLOC)                                                     
TXLOCX   DS    CL(L'TAXLOCX)                                                    
TXWCH    DS    CL(L'TAXWCH)        HEADER FOR WORKCODE                          
TXWC     DS    CL(L'TAXWC)         WORKCODE                                     
TXWCX    DS    CL(L'TAXWCX)        TRAILER FOR WORKCODE                         
TXNAMH   DS    CL(L'TAXNAMH)       PROTECTED ACCOUNT NAME                       
TXNAM    DS    CL(L'TAXNAM)        NAME FIELD                                   
TXNARH   DS    CL(L'TAXNARH)       NARATIVE                                     
TXNAR    DS    CL(L'TAXNAR)                                                     
TXNARX   DS    CL(L'TAXNARX)                                                    
TXLNQ    EQU   *-TXD               LENGTH OF INPUT LINE                         
NFIELDS  EQU   7                                                                
*                                                                               
*              DSECT TO COVER POSTING DATA FOR A LINE                           
*                                                                               
PSTD     DSECT                                                                  
PSTLNNUM DS    CL1                 LINE NUMBER                                  
PSTACC   DS    CL15                CREDIT ACCOUNT                               
PSTNME   DS    CL36                ACCOUNT NAME                                 
PSTJOB   DS    CL15                DEBIT ACCOUNT                                
PSTJBNM  DS    CL36                ACCOUNT NAME                                 
PSTCLI   DS    CL15                CA FOR CREDIT POSTING                        
PSTCLNM  DS    CL36                ACCOUNT NAME                                 
PSTOFF   DS    CL2                 OFFICE                                       
PSTREF   DS    CL6                 REF NUMBER                                   
PSTLOC   DS    CL14                LOCALITY                                     
PSTLCNM  DS    CL36                LOCALITY NAME                                
PSTWC    DS    CL2                 WORKCODE                                     
PSTEFF   DS    CL3                 EFFECTIVE DATE                               
PSTPCT   DS    PL4                 PERCENT                                      
PSTBAS   DS    PL6                 BASIS                                        
PSTAMT   DS    PL6                 POSTING AMOUNT                               
PSTNALN  DS    CL1                 NARRATIVE LENGTH                             
PSTNAR   DS    CL78                POSTING AMOUNT                               
PSTLNQ   EQU   *-PSTD                                                           
PSTLINE  EQU   PSTLNQ*4            4 POSSIBLE ENTRIES PER LINE                  
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
         ORG   CONTABH                                                          
       ++INCLUDE ACBATEDD                                                       
         ORG   OSVALS+200                                                       
* CALLING OVERLAYS (ACBAT01, ACBAT03) USE FIRST 200 BYTES                       
*                                                                               
USERL    EQU   OSVALSL-200                                                      
USERAREA DS    0F                                                               
THISBASE DS    PL6                 BASIS                                        
THISWC   DS    CL2                 WORKCODE                                     
TAXPASS  DS    XL1                                                              
         DS    CL(USERL-(*-USERAREA))   SPARE                                   
         EJECT                                                                  
* ACGENBOTH                                                                     
* ACGENDAY                                                                      
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACBAT45   12/03/01'                                      
         END                                                                    
