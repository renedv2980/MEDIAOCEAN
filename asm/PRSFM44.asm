*          DATA SET PRSFM44    AT LEVEL 004 AS OF 05/13/20                      
*PHASE T41C44B                                                                  
*INCLUDE SRCHCALL                                                               
*INCLUDE NUMED                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE PPBROWSE                                                               
         TITLE 'T41C44 AUTOPAY RECORDS'                                         
***********************************************************************         
*                                                                               
***********************************************************************         
T41C44   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C44,RR=R3                                                   
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
*                                                                               
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*                                                                               
         MVI   IPSTAT,0            INIT INPUT STATISTICS                        
         MVI   SAVMSGNO,0          INIT MESSAGE NUMBER SAVEAREA                 
         MVI   ERROR,0             INIT MESSAGE NUMBER                          
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENT                          
*                                                                               
         OI    GENSTAT4,NODELLST   DELETE FROM LIST NOT ALLOWED                 
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AB9'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APUBEDIT,DMCB       STORE PUBEDIT ADDRESS                        
*                                                                               
         CLC   =C'CHA',CONACT                                                   
         BE    INVACTER                                                         
         CLC   =C'ADD',CONACT                                                   
         BE    INVACTER                                                         
         CLC   =C'DEL',CONACT                                                   
         BE    INVACTER                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   *+12                                                             
         BRAS  RE,VK                                                            
         B     MAINX                                                            
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
         B     MAINX                                                            
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     MAINX                                                            
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     MAINX                                                            
*                                                                               
MAINX    DS    0H                                                               
EXIT     XIT1                                                                   
         LTORG                                                                  
*                                                                               
LR       NTR1  BASE=*,LABEL=*                                                   
         OC    KEY,KEY                                                          
         JNZ   LR10                                                             
*                                                                               
         LA    R4,KEY                                                           
         USING PAPYREC,R4                                                       
         MVC   PAPYKAGY,AGENCY     AGENCY                                       
         MVC   PAPYKMED,QMED       MEDIA                                        
         MVI   PAPYKTYP,PAPYKRCD                                                
LRHI     GOTO1 HIGH                                                             
         J     LR10                                                             
LRSEQ    GOTO1 SEQ                                                              
LR10     LA    R4,KEY                                                           
         CLC   PAPYKAGY,AGENCY                                                  
         JNE   LRX                                                              
         CLI   KEY+3,PAPYKRCD      AUTOPAY RECORD?                              
         JE    LR10M                                                            
         XC    KEY+3(L'KEY-3),KEY+3                                             
         MVI   KEY+3,PAPYKRCD                                                   
         LLC   RE,KEY+2                                                         
         CLC   KEY+2(1),KEYSAVE+2  SAME MEDIA?                                  
         JNE   *+8                                                              
         AHI   RE,1                READ NEXT MEDIA                              
         STC   RE,KEY+2                                                         
         J     LRHI                                                             
*                                                                               
LR10M    OC    QMED,QMED           ANY MEDIA FILTER?                            
         JZ    *+14                                                             
         CLC   QMED,PAPYKMED                                                    
         JNE   LRSEQ                                                            
         OC    FILTCLT,FILTCLT     ANY CLIENT FILTER?                           
         JZ    *+14                                                             
         CLC   FILTCLT,PAPYKCLT                                                 
         JNE   LRSEQ                                                            
         OC    FILTDATR,FILTDATR  ANY RUN DATE FILTER?                          
         JZ    *+14                                                             
         CLC   FILTDATR,PAPYKDAT                                                
         JNE   LRSEQ                                                            
         DROP  R4                                                               
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         USING PLINED,R2                                                        
         LA    R2,LISTAR                                                        
         MVC   LISTAR,SPACES                                                    
*                                                                               
         L     R6,AIO                                                           
         USING PAPYREC,R6                                                       
         MVC   PRMED(1),PAPYKMED   MEDIA                                        
         MVC   PRCLT,PAPYKCLT      CLIENT                                       
         DROP  R6                                                               
*                                                                               
         AHI   R6,33                                                            
         USING PAP1D,R6                                                         
*                                                                               
         OC    FILTDATP,FILTDATP   ANY PAID DATE FILTER?                        
         JZ    *+14                                                             
         CLC   FILTDATP,PAP1ADTE                                                
         JNE   LRSEQ                                                            
*                                                                               
         OC    FILTPRD,FILTPRD     ANY PRODUCT FILTER?                          
         JZ    *+14                                                             
         CLC   FILTPRD,PAP1PRD                                                  
         JNE   LRSEQ                                                            
*                                                                               
         MVC   PRPRD,PAP1PRD       PRODUCT                                      
*                                                                               
         EDIT  PAP1EST,PREST,0,ALIGN=LEFT         ESTIMATE                      
         OC    FILTEST,FILTEST     ANY ESTIMATE FILTER?                         
         JZ    *+14                                                             
         CLC   FILTEST,PREST                                                    
         JNE   LRSEQ                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,PAP1DATE),(8,PRPER)                               
*                                                                               
         CLI   PAP1REF,X'01'       REFERENCE LINE 1?                            
         JE    LR20                                                             
         MVI   PRPER+8,C'-'                                                     
*!!!     EDIT  PAP1REF,(3,PRPER+9),0,ALIGN=LEFT                                 
         ZIC   R0,PAP1REF                                                       
         CVD   R0,DUB                                                           
         CP    DUB,=P'100'                                                      
         BL    LR12                                                             
*                                  DISPLAY 100 - 239 AS A0 - N9                 
         DP    DUB,=P'10'                                                       
         UNPK  PRPER+10(1),DUB+7(1)                                             
         OI    PRPER+10,X'F0'                                                   
*                                                                               
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   RF,DUB                                                           
         LA    RF,SBUYATAB(RF)                                                  
         MVC   PRPER+9(1),0(RF)                                                 
         B     LR20                DONE                                         
*                                                                               
LR12     OI    DUB+7,X'0F'                                                      
         UNPK  PRPER+9(2),DUB                                                   
         CLI   PRPER+9,C'0'                                                     
         BNE   *+10                                                             
         MVC   PRPER+9(2),PRPER+10                                              
*                                                                               
LR20     MVC   PRINV#,PAP1INV#     INVOICE NUMBER                               
         GOTOR APUBEDIT,DMCB,(8,PAP1PUB),(C'S',PRPUB)                           
*                                                                               
         CLI   KEY+25,PAPYPRCQ     AUTOPAY PROCESSED?                           
         JNE   LR30                                                             
*                                                                               
         OC    PAP1ADTE,PAP1ADTE                                                
         JNZ   *+12                                                             
         MVI   PRIND,C'*'                                                       
         J     LR30                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,PAP1ADTE),(8,PRADAT)                              
*                                                                               
LR30     GOTO1 LISTMON                                                          
         J     LRSEQ                                                            
*                                                                               
LRX      J     EXIT                                                             
         DROP  R2,R6                                                            
*                                                                               
SBUYATAB DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
*                                                                               
*        VALIDATE KEY                                                 *         
*                                                                               
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
         MVI   USEIONUM,2          VALIDATE USING IOA2                          
*                                                                               
         LA    R2,SCRMEDH           VALIDATE MEDIA                              
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         GOTO1 VALIMED                                                          
         OI    6(R2),X'80'                                                      
         MVC   SCRMEDN,MEDNM       MEDIA NAME                                   
         OI    SCRMEDNH+6,X'80'                                                 
*                                                                               
         LA    R2,SCRCLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
*                                                                               
         MVC   QCLT,=X'FFFFFF'     SET TO DEFAULT                               
         GOTO1 VALICLT             VALIDATE CLIENT ENTRY                        
         OI    6(R2),X'80'                                                      
         MVC   SCRCLTN,CLTNM       MEDIA NAME                                   
         OI    SCRCLTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SCRDTEH          DATE                                         
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,SCRDTE,QSTART                                        
         GOTO1 DATCON,DMCB,QSTART,(2,HALF)                                      
         XC    HALF,=X'FFFF'                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,SCRSNH           SERIAL #                                     
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
*                                                                               
         ZIC   R1,SCRSNH+5                                                      
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SCRSN(0)                                                     
         OI    6(R2),X'80'                                                      
*                                                                               
*        BUILD STARTING KEY                                                     
*                                                                               
         MVI   USEIONUM,1          USE IOAREA 1 FOR VALIDATION                  
         MVC   AIO,AIO1            USE AIO1                                     
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING PAPYREC,R4          ESTABLISH AGENCY CONTACT LIST KEY            
*                                                                               
         MVC   PAPYKAGY,AGENCY     AGENCY                                       
         MVC   PAPYKMED,QMED       MEDIA                                        
         MVI   PAPYKTYP,PAPYKRCD                                                
         MVC   PAPYKDAT,HALF       AUTOPAY DATE                                 
         MVC   PAPYKCLT,QCLT       CLIENT                                       
         ZAP   PAPYKSN,DUB+3(5)    SERIAL #                                     
*                                                                               
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
*                                                                               
         CLC   TWAKEYSV(L'SRAKEY),KEY    CHECK FOR NEWKEY                       
         BE    *+8                                                              
         MVI   NEWKEY,C'Y'                                                      
*                                                                               
         B     VKX                                                              
*                                                                               
*        VALIDATE LIST KEY                                            *         
*                                                                     *         
VKL      DS    0H                                                               
         MVI   USEIONUM,1          USE IOAREA 1 FOR VALIDATION                  
         MVC   AIO,AIO1            USE AIO1                                     
*                                                                               
         XC    FILTVALS(FILTVALQ),FILTVALS                                      
*                                                                               
         LA    R2,LISMEDH           VALIDATE MEDIA                              
         OI    6(R2),X'80'                                                      
         CLI   5(R2),0                                                          
         JE    VKL05                                                            
         GOTO1 VALIMED                                                          
         J     VKL10                                                            
*                                                                               
VKL05    LA    R2,TEMPH            CREATE DUMMY FIELD TO LIST ALL RECS          
         XC    TEMPH,TEMPH                                                      
         MVI   0(R2),9                                                          
         MVI   5(R2),1                                                          
         MVI   8(R2),C'B'          DUMMY MEDIA B - FIRST MEDIA CODE             
         GOTO1 VALIMED                                                          
         XC    QMED,QMED                                                        
*                                                                               
VKL10    LA    R2,LISCLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         JE    VKL20                                                            
*                                                                               
         MVC   QCLT,=X'FFFFFF'     SET TO DEFAULT                               
         GOTO1 VALICLT             VALIDATE CLIENT ENTRY                        
         MVC   FILTCLT,QCLT                                                     
         OI    6(R2),X'80'                                                      
*                                                                               
VKL20    LA    R2,LISDTEH          RUN DATE                                     
         CLI   5(R2),0                                                          
         JE    VKL25                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,LISDTE,QSTART                                        
         GOTO1 DATCON,DMCB,QSTART,(2,FILTDATR)                                  
         XC    FILTDATR,=X'FFFF'                                                
         OI    6(R2),X'80'                                                      
*                                                                               
VKL25    LA    R2,LISPAIDH         PAID DATE                                    
         CLI   5(R2),0                                                          
         JE    VKL30                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,LISPAID,QSTART                                       
         GOTO1 DATCON,DMCB,QSTART,(3,FILTDATP)                                  
         OI    6(R2),X'80'                                                      
*                                                                               
VKL30    LA    R2,LISPRDH          PRODUCT                                      
         CLI   5(R2),0                                                          
         JE    VKL40                                                            
         MVC   FILTPRD,8(R2)                                                    
         OC    FILTPRD,SPACES                                                   
*                                                                               
VKL40    LA    R2,LISESTH          PRODUCT                                      
         CLI   5(R2),0                                                          
         JE    *+16                                                             
         MVC   FILTEST,LISEST                                                   
         OC    FILTEST,SPACES                                                   
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING PAPYREC,R4          ESTABLISH AGENCY CONTACT LIST KEY            
*                                                                               
         MVC   PAPYKAGY,AGENCY     AGENCY                                       
         MVC   PAPYKMED,QMED       MEDIA                                        
         MVI   PAPYKTYP,PAPYKRCD                                                
*                                                                               
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
*                                                                               
         CLC   TWAKEYSV(L'SRAKEY),KEY    CHECK FOR NEWKEY                       
         BE    *+8                                                              
         MVI   NEWKEY,C'Y'                                                      
*                                                                               
VKX      J     EXIT                                                             
         DROP  R4                                                               
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     VKERR                                                            
INVACTER MVI   ERROR,INVACT                                                     
         B     VKERR                                                            
*                                                                               
VKERR    GOTOR ERREX                                                            
         LTORG                                                                  
*                                                                               
*        DISPLAY KEY                                                  *         
*                                                                     *         
DK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         L     R6,AIO                                                           
         USING PAPYREC,R6                                                       
*                                                                               
         OI    SCRMEDH+6,X'80'                                                  
         OI    SCRCLTH+6,X'80'                                                  
         OI    SCRDTEH+6,X'80'                                                  
         OI    SCRSNH+6,X'80'                                                   
*                                                                               
         MVC   SCRMED,PAPYKMED     MEDIA                                        
         MVC   SCRCLT,PAPYKCLT     CLIENT                                       
         MVC   HALF,PAPYKDAT       DATE                                         
         XC    HALF,=X'FFFF'                                                    
         GOTO1 DATCON,DMCB,(2,HALF),(8,SCRDTE)                                  
         EDIT  PAPYKSN,SCRSN,0,ALIGN=LEFT      SERIAL #                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'PAPYKEY),PAPYKEY                                           
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
DKX      J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
*        VALIDATE RECORD                                              *         
*                                                                     *         
VR       NTR1  BASE=*,LABEL=*                                                   
VRX      J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*        DISPLAY  RECORD                                              *         
*                                                                     *         
DR       NTR1  BASE=*,LABEL=*                                                   
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         L     R6,AIO              POINT TO RECORD FOR DISPLAY                  
         AHI   R6,33                                                            
         USING PAP1D,R6            ESTABLISH AS E-MAIL BASE ELEMENT             
*                                                                               
         XC    SCRPRD,SCRPRD                                                    
         OI    SCRPRDH+6,X'80'                                                  
         XC    SCREST,SCREST                                                    
         OI    SCRESTH+6,X'80'                                                  
         XC    SCRDAT,SCRDAT                                                    
         OI    SCRDATH+6,X'80'                                                  
         XC    SCRPUB,SCRPUB                                                    
         OI    SCRPUBH+6,X'80'                                                  
         XC    SCRPAYE,SCRPAYE                                                  
         OI    SCRPAYEH+6,X'80'                                                 
         XC    SCRINVN,SCRINVN                                                  
         OI    SCRINVNH+6,X'80'                                                 
         XC    SCRINVA,SCRINVA                                                  
         OI    SCRINVAH+6,X'80'                                                 
         XC    SCRRATE,SCRRATE                                                  
         OI    SCRRATEH+6,X'80'                                                 
         XC    SCRPAID,SCRPAID                                                  
         OI    SCRPAIDH+6,X'80'                                                 
         XC    SCRMSG,SCRMSG                                                    
         OI    SCRMSGH+6,X'80'                                                  
*                                                                               
         TM    KEY+25,PAPYPRCQ     ALREADY PROCESSED?                           
         JZ    *+10                                                             
         MVC   SCRPAID(5),=C'ERROR'                                             
         OC    PAP1ADTE,PAP1ADTE   ANY AUTOPAY DATE?                            
         JZ    DR10                                                             
         GOTO1 DATCON,DMCB,(3,PAP1ADTE),(8,SCRPAID)                             
*                                                                               
DR10     MVC   SCRPRD(L'PAP1PRD),PAP1PRD        PRODUCT                         
         EDIT  PAP1EST,(3,SCREST),0,ALIGN=LEFT         ESTIMATE                 
         GOTO1 DATCON,DMCB,(3,PAP1DATE),(8,SCRDAT)                              
         CLI   PAP1REF,X'01'       REFERENCE LINE 1?                            
         JE    DR20                                                             
         MVI   SCRDAT+8,C'-'                                                    
*!!!     EDIT  PAP1REF,(3,SCRDAT+9),0,ALIGN=LEFT                                
         ZIC   R0,PAP1REF                                                       
         CVD   R0,DUB                                                           
         CP    DUB,=P'100'                                                      
         BL    DR12                                                             
*                                  DISPLAY 100 - 239 AS A0 - N9                 
         DP    DUB,=P'10'                                                       
         UNPK  SCRDAT+10(1),DUB+7(1)                                            
         OI    SCRDAT+10,X'F0'                                                  
*                                                                               
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   RF,DUB                                                           
         LA    RF,REFATAB(RF)                                                   
         MVC   SCRDAT+9(1),0(RF)                                                
         B     DR20                DONE                                         
*                                                                               
DR12     OI    DUB+7,X'0F'                                                      
         UNPK  SCRDAT+9(2),DUB                                                  
         CLI   SCRDAT+9,C'0'                                                    
         BNE   *+10                                                             
         MVC   SCRDAT+9(2),SCRDAT+10                                            
*                                                                               
DR20     MVC   SCRPAYE,PAP1PAYE+1  PAYEE                                        
         MVC   SCRINVN,PAP1INV#    INVOICE NUMBER                               
         MVC   SCRINVA,PAP1INV$    INVOICE $                                    
*                                                                               
         MVC   SCRRATE,PAP1RATE    RATE                                         
         OI    SCRRATEH+6,X'80'                                                 
*                                                                               
         CLI   PAP1LEN,PAP1LNQ                                                  
         JE    *+14                                                             
         MVC   SCRMSG,PAP1MSG      MESSAGE                                      
         OI    SCRMSGH+6,X'80'                                                  
*                                                                               
         GOTOR APUBEDIT,DMCB,(8,PAP1PUB),(C'S',SCRPUB)                          
*                                                                               
         XC    SCRMEDN,SCRMEDN                                                  
         MVI   SCRMEDH+5,1                                                      
         LA    R2,SCRMEDH                                                       
         GOTO1 VALIMED                                                          
         MVC   SCRMEDN,MEDNM                                                    
         OI    SCRMEDNH+6,X'80'                                                 
*                                                                               
         XC    SCRCLTN,SCRCLTN                                                  
         MVI   SCRCLTH+5,3                                                      
         LA    R2,SCRCLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   SCRCLTN,CLTNM                                                    
         OI    SCRCLTNH+6,X'80'                                                 
*                                                                               
         XC    SCRPRDN,SCRPRDN                                                  
         MVI   SCRPRDH+5,3         INPUT LENGTH                                 
         LA    R2,SCRPRDH                                                       
         GOTO1 VALIPRD                                                          
         MVC   SCRPRDN,PRDNM                                                    
         OI    SCRPRDNH+6,X'80'    TRANSMIT PRD NAME                            
*                                                                               
         XC    SCRESTN,SCRESTN                                                  
         MVI   SCRESTH+5,3         INPUT LENGTH                                 
         LA    R2,SCRESTH                                                       
         GOTO1 VALIEST                                                          
         MVC   SCRESTN,ESTNM                                                    
         OI    SCRESTNH+6,X'80'    TRANSMIT EST NAME                            
*                                                                               
DRX      J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
REFATAB  DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
         LTORG                                                                  
***********************************************************************         
* COMMONLY ADDRESSABLE ROUTINES                                       *         
***********************************************************************         
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
ERRFLDX  BR    RE                                                               
         DROP  R1                                                               
         LTORG                                                                  
*                                                                               
         PRINT GEN                                                              
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFM9AD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFM9BD                                                       
*                                                                               
         DS    XL64                AREA FOR BOOK= NOTICE                        
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    F                                                                
ORIGKEY  DS    XL(L'KEY)                                                        
SCANBLK  DS    CL70                                                             
SAVMSGNO DS    XL1                 CURRENT MESSAGE NUMBER SAVEAREA              
IPSTAT   DS    XL1                 CUMULATIVE INPUT STATISTICS                  
NEWKEY   DS    XL1                 C'Y' - BASIC KEY HAS CHENGED                 
APUBEDIT DS    A                   A(PUBEDIT)                                   
TEMPH    DS    XL9                                                              
SVAPYKEY DS    XL25                                                             
*                                                                               
FILTVALS DS    0X                                                               
FILTCLT  DS    CL3                 CLIENT                                       
FILTPRD  DS    CL3                 PRODUCT                                      
FILTEST  DS    CL3                 ESTIMATE                                     
FILTDATR DS    XL2                 AUTOPAY RUN DATE                             
FILTDATP DS    XL3                 AUTOPAY PAID DATE BINARY                     
FILTVALQ EQU   *-FILTVALS                                                       
*                                                                               
         DS    0F                                                               
       ++INCLUDE DDBSRPRMD                                                      
         DS    0D                                                               
LUBLK    DS    XL(LUBLKL)          LINUP CONTROL BLOCK                          
         DS    0F                                                               
SAVEBASE DS    XL64                DOMAIN NAME SAVEAREA                         
SAVEBASL DS    XL1                 DOMAIN NAME LENGTH                           
*                                                                               
       ++INCLUDE PPGENCCL                                                       
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE PPSRCHPARM                                                     
       ++INCLUDE DDLINUPD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE PRWRIEQUS                                                      
       ++INCLUDE PPGENAPY                                                       
PLINED   DSECT                                                                  
PRMED    DS    CL3                 MEDIA                                        
         DS    CL1                                                              
PRCLT    DS    CL3                 CLIENT                                       
         DS    CL2                                                              
PRPRD    DS    CL3                 PRODUCT                                      
         DS    CL2                                                              
PREST    DS    CL3                 ESTIMATE                                     
         DS    CL2                                                              
PRPUB    DS    CL15                PUBLICATION                                  
         DS    CL1                                                              
PRPER    DS    CL12                INVOICE DATE                                 
         DS    CL2                                                              
PRADAT   DS    CL8                 PAID DATE                                    
         DS    CL2                                                              
PRINV#   DS    CL12                INVOICE NUMBER                               
         DS    CL1                                                              
PRIND    DS    CL1                 INDICATOR                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PRSFM44   05/13/20'                                      
         END                                                                    
