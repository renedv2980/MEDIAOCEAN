*          DATA SET ACREG00    AT LEVEL 019 AS OF 01/22/14                      
*PHASE T61200C,*                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE ACWKSCAN                                                               
         TITLE 'CHECK REGISTER GENERATION'                                      
ACREG00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 GWSX-GWS,**REG0**,RA,R7,RR=R5                                    
         LR    R9,RC                                                            
         USING GWS,R9              R9=A(GLOBAL W/S)                             
         ST    R5,RELO                                                          
         L     R8,4(R1)                                                         
         USING TWAD,R8             R8=A(TWA)                                    
         ST    R8,ATWA                                                          
         MVC   ATIA,12(R1)                                                      
*                                  INITIALIZE STANDARD W/S VALUES               
         LA    RE,GWS                                                           
         LA    RF,GWSINITX                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
         ST    RB,ABASE1                                                        
         ST    RA,ABASE2                                                        
         ST    RD,AWORK                                                         
         MVC   ACOMFACS,16(R1)                                                  
         MVC   COMPANY,0(R1)                                                    
                                                                                
         USING COMFACSD,RE                                                      
         L     RE,ACOMFACS                                                      
         MVC   VCALLOV,CCALLOV     BUILD EXTERNAL DIRECTORY                     
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VDATCON,CDATCON                                                  
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VGETPROF,CGETPROF                                                
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VGETFACT,CGETFACT                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VXTRAINF,CXTRAINF                                                
         MVC   DMCB+4(4),=X'D9000A25'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSPOON,0(R1)                                                     
         DROP  RE                                                               
                                                                                
         MVI   IOAREAS,0                                                        
         LA    RE,IOAREAS          SET A(2 I/O AREAS)                           
         ST    RE,AIOAREA1                                                      
         LA    RE,2048(RE)                                                      
         ST    RE,AIOAREA2                                                      
         LA    RE,2048(RE)                                                      
         LA    RE,ACRECORD-ACKEYD                                               
         STH   RE,DATADISP         SET DISP TO FIRST ELEMENT                    
*                                  BUILD INTERNAL/EXTERNAL DIRECTORY            
         LA    R1,AROUTINE                                                      
         LA    RF,ROUTTAB                                                       
INIT2    ICM   RE,7,0(RF)                                                       
         LA    RE,0(RE)            RELOCATE A/V TYPE                            
         A     RE,RELO                                                          
         LA    RF,3(RF)                                                         
INIT4    ICM   RE,8,0(RF)                                                       
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,1(RF)                                                         
         CLI   0(RF),X'FF'         END OF SUB-LIST                              
         BNE   INIT4                                                            
         LA    RF,1(RF)                                                         
         CLI   0(RF),X'FF'         END OF LIST                                  
         BNE   INIT2                                                            
*                                                                               
         L     RF,=V(ACWKSCAN)                                                  
         A     RF,RELO                                                          
         ST    RF,VCWKSCAN                                                      
*                                  SET OTHER FIELDS                             
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   MSG,SPACES                                                       
         MVC   XMSG,SPACES                                                      
                                                                                
         USING FACTSD,RE                                                        
         GOTO1 VGETFACT,DMCB,0                                                  
         L     RE,0(,R1)                                                        
         MVC   TERMTYPE,FATTYPE    TERMINAL TYPE                                
         MVC   TODAYB,FADATEB                                                   
         MVC   BOOKNM,=CL10'ACV56 '    SET FOR VSPOON                           
         DROP  RE                                                               
                                                                                
         USING XTRAINFD,RE                                                      
         L     RE,VXTRAINF                                                      
         TM    XIFLAG1,XIROMODE+XIROSYS+XIWRONGF                                
         BZ    INIT6               OKAY TO KEEP GOING                           
         LA    R1,REGMSGH                                                       
         ST    R1,FADR                                                          
         MVC   FERR#,=AL2(NOTAUTH)                                              
         TM    XIFLAG1,XIROMODE    READ ONLY MODE                               
         BO    ERROR               USER NOT AUTHORIZED                          
         MVC   FERR#,=AL2(SYSNOTUP)                                             
         TM    XIFLAG1,XIWRONGF    WRONG ADV#                                   
         BZ    ERROR               SYSTEM MUST BE SET TO READ ONLY              
         MVC   FERR#,=AL2(WRNADV#) ON WRONG ADV                                 
         MVC   XMSG(4),XIUPDFAC                                                 
         J     ERROR                                                            
         DROP  RE                                                               
                                                                                
*        GOTO1 VGETFACT,DMCB,(X'80',BYTE),F#SEIND                               
*        TM    BYTE,SEIRONLY       READ ONLY ADV FOR ACC FILE ?                 
*        BO    WRGADV                                                           
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        FIRST TIME INITIALIZATION CODE                                         
*-----------------------------------------------------------------*             
INIT6    MVC   KEY,SPACES          GET COMPANY RECORD FOR START OF              
         MVC   KEY(1),COMPANY      FINANCIAL YEAR                               
         GOTO1 AREAD,AIOAREA2                                                   
         BNE   ERROR                                                            
         L     RE,AIOAREA                                                       
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
INIT7    CLI   0(RE),0             LOCATE COMPANY ELEMENT                       
         BNE   *+6                 IF NO '10' EL, PROBLEM                       
         DC    H'0'                                                             
         CLI   0(RE),X'10'                                                      
         BE    *+14                                                             
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     INIT7                                                            
         USING ACCOMPD,RE                                                       
         DROP  RE                                                               
*                                                                               
INIT8    XC    WORK,WORK           SAVE AGENCY-LEVEL PROGRAM PROFILE            
         MVC   WORK(4),=C'AREG'                                                 
         NI    WORK,X'BF'                                                       
         MVC   WORK+4(1),COMPANY                                                
         GOTO1 VGETPROF,DMCB,WORK,WORK1,VDATAMGR                                
         MVC   PROGPROF,WORK1                                                   
         EDIT  TWAUSRID,(6,ORIGIN),FILL=0                                       
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        CHECK ACTION                                                           
*-----------------------------------------------------------------*             
ACT10    DS    0H                                                               
         CLC   REGACT(5),=C'MAINT'                                              
         BE    ACT20                                                            
         CLC   LASTACT,SPACES                                                   
         BH    ACT30                                                            
         MVC   REGACT(5),=C'MAINT'                                              
         OI    REGACTH+6,TRANSMIT                                               
ACT20    MVC   LASTACT,REGACT                                                   
         B     FILE10                                                           
*                                                                               
ACT30    CLC   REGACT(5),=C'DRAFT'                                              
         BE    FILE10                                                           
         CLC   REGACT(6),=C'UPDATE'                                             
         BE    FILE10                                                           
         MVC   FERR#,=AL2(INVALID)     ELSE IS INVALID INPUT                    
         LA    R1,REGACTH                                                       
         ST    R1,FADR                                                          
         B     ERROR                                                            
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        CHECK FILE TYPE INPUT                                                  
*-----------------------------------------------------------------*             
FILE10   DS    0H                                                               
         TM    REGTYPH+4,VALPREV        HAS DATE BEEN VALIDATED                 
         BO    LEDG00                   PREVIOUSLY                              
         BAS   RE,CLRSCRN               CLEAR ALL SUBSEQUENT FIELDS             
         LA    RE,TWASV                 IF I'M CLEARING SCREEN, MUST BE         
         LA    RF,TWASVEN               KEY CHANGE, SO CLEAR ALL SAVED          
         SR    RF,RE                    CHECK NUMBERS AND TOTALS                
         XCEF                                                                   
         MVI   KEYCH,0                  CLEAR ALL OLD SETTINGS                  
         OI    KEYCH,FRSTTIM                                                    
         LA    R2,REGTYPH                                                       
         ST    R2,FADR                                                          
         GOTO1 AFVAL,REGTYPH                                                    
         BE    ERROR                                                            
         OI    REGTYPNH+6,TRANSMIT                                              
*        MVC   REGTYPN,=C'DDS  '                                                
*        CLI   REGTYP,C'C'                                                      
*        BE    FILE50                                                           
         MVC   REGTYPN,=C'SOON '                                                
         CLI   REGTYP,C'S'                                                      
         BE    FILE50                                                           
         MVC   REGTYPN,=C'LOCAL'                                                
         CLI   REGTYP,C'L'                                                      
         BE    FILE50                                                           
         MVC   REGTYPN,SPACES                                                   
         MVC   FERR#,=AL2(INVALID)                                              
         B     ERROR                                                            
                                                                                
FILE50   OI    REGTYPH+4,VALPREV        IF DATE HAS BEEN VALIDATED              
         OI    REGTYPH+6,TRANSMIT                                               
         NI    REGLEGH+4,NOTVALP        TURN OFF VALIDITY TO SIGNAL             
         NI    REGDATH+4,NOTVALP        REVALIDATION                            
         EJECT                          PREVIOUSLY                              
*-----------------------------------------------------------------*             
*        PROCESS LEDGER FIELD                                                   
*-----------------------------------------------------------------*             
LEDG00   DS    0H                                                               
         TM    REGLEGH+4,VALPREV        IF LEDGER HAS BEEN VALIDATED            
         BO    DATE02                   PREVIOUSLY, WORKER FILE HAS             
         NI    REGDATH+4,NOTVALP        REVALIDATION                            
         BAS   RE,CLRSCRN               CLEAR ALL SUBSEQUENT FIELDS             
         LA    RE,TWASV                 IF I'M CLEARING SCREEN, MUST BE         
         LA    RF,TWASVEN               KEY CHANGE, SO CLEAR ALL SAVED          
         SR    RF,RE                    CHECK NUMBERS AND TOTALS                
         XCEF                                                                   
         MVI   KEYCH,0                  CLEAR ALL OLD SETTINGS                  
         OI    KEYCH,FRSTTIM            SET KEY CHANGE ON                       
*                                                                               
         LA    R2,REGLEGH               ALREADY BEEN READ-SKIP TO               
         GOTO1 AFVAL,REGLEGH            CHECK NUMBER VALIDATION                 
         BE    ERROR                                                            
         MVC   LEDGER,FLD                                                       
         MVI   KEY+1,C'S'                                                       
         MVC   KEY+2(1),FLD                                                     
*                                                                               
         OI    REGLEGH+6,TRANSMIT                                               
         OI    REGLEGNH+6,TRANSMIT                                              
         LA    RF,LEDGTAB               VALIDATE THAT LEDGER INPUT              
LEDG04   CLI   0(RF),X'FF'              ON SCREEN IS IN TABLE OF VALID          
         BE    LEDG45                   LEDGERS                                 
         CLC   0(1,RF),LEDGER                                                   
         BE    LEDG08                                                           
         LA    RF,1(RF)                                                         
         B     LEDG04                                                           
*                                                                               
LEDG08   MVC   REGLEGN,SPACES           CLEAR LEDGER NAME FIELD                 
         GOTO1 AREAD,AIOAREA1           READ RECORD                             
         BNE   ERROR                                                            
         GOTO1 AGETNAME,AIOAREA1        GET LEDGER NAME                         
         MVC   REGLEGN,WORK             MOVE TO SCREEN                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,CHAKTYPQ                                                     
         MVC   KEY+1(L'COMPANY),COMPANY                                         
         MVI   KEY+2,C'S'                                                       
         MVC   KEY+3(L'LEDGER),LEDGER                                           
         MVC   LASTKEY,KEY                                                      
         L     R2,AIOAREA2                                                      
         GOTO1 ARDHI,AIOAREA2           READ RECORD                             
         CLC   KEYSAVE(4),KEY                                                   
         BNE   LGERR                                                            
         AH    R2,DATADISP                                                      
LEDG10   CLI   0(R2),0                  IF NO '54' EL FOUND THAT                
         BNE   LEDG13                                                           
         L     R2,AIOAREA2                                                      
         GOTO1 ASEQ,AIOAREA2            READ RECORD                             
         CLC   LASTKEY(4),0(R2)                                                 
         BNE   LEDG50                                                           
*                                                                               
LEDG13   CLI   0(R2),OCNELQ             ERROR AND EXIT                          
         BE    LEDG20                                                           
LEDG15   ZIC   R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     LEDG10                                                           
*                                                                               
         USING OCNELD,R2                                                        
LEDG20   CLC   TWAUSRID,OCNOFFID        TRY NEXT 54 EL FOR MATCH                
         BNE   LEDG15                                                           
         CLI   1(R2),OCNLN2Q                                                    
         BL    LEDG25                                                           
         TM    OCNSTAT2,OCNSMICR                                                
         BZ    *+8                                                              
         OI    MICROSW,MICROCH                                                  
LEDG25   MVC   LDLETTER,OCNNTYP                                                 
         OI    REGLEGH+4,VALPREV                                                
*                                                                               
         XC    SAVPSR,SAVPSR                                                    
         XC    SAVPLR,SAVPLR                                                    
         OC    OCNDPSR,OCNDPSR                                                  
         BZ    LEDG30                                                           
         GOTO1 VDATCON,DMCB,(2,OCNDPSR),(1,SAVPSR)                              
LEDG30   OC    OCNDPLR,OCNDPLR                                                  
         BZ    DATE02                                                           
         GOTO1 VDATCON,DMCB,(2,OCNDPLR),(1,SAVPLR)                              
         B     DATE02                                                           
                                                                                
LEDG45   MVC   REGLEGN,SPACES                                                   
LEDG50   MVC   FERR#,=AL2(LEDGNVAL)                                             
         B     ERROR                                                            
                                                                                
LGERR    MVC   FERR#,=AL2(SPECIAL)                                              
         MVC   MSG,SPACES                                                       
         MVC   MSG(38),=C'**ERROR - LEDGER NOT SET UP FOR CHECKS'               
         NI    REGLEGH+4,NOTVALP        REVALIDATION                            
         B     ERROR                                                            
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        PROCESS DATE FIELD                                                     
*-----------------------------------------------------------------*             
DATE02   DS    0H                                                               
         TM    REGDATH+4,VALPREV        HAS DATE BEEN VALIDATED                 
         BO    RDIND02                  PREVIOUSLY                              
         BAS   RE,CLRSCRN               CLEAR ALL SUBSEQUENT FIELDS             
         LA    RE,TWASV                 IF I'M CLEARING SCREEN, MUST BE         
         LA    RF,TWASVEN               KEY CHANGE, SO CLEAR ALL SAVED          
         SR    RF,RE                    CHECK NUMBERS AND TOTALS                
         XCEF                                                                   
         MVI   KEYCH,0                  CLEAR ALL OLD SETTINGS                  
         OI    KEYCH,FRSTTIM                                                    
*                                                                               
         LA    R2,REGDATH                                                       
         ST    R2,FADR                                                          
         CLI   REGDATH+5,0                                                      
         BH    DATE10                                                           
         GOTO1 VDATCON,DMCB,(3,TODAYB),(10,REGDAT)                              
                                                                                
DATE10   MVC   FERR#,=AL2(INVDATE)                                              
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK                                      
         GOTO1 VDATCON,DMCB,(0,WORK),(1,WORK+6)                                 
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         MVC   MONTH,WORK+2             SAVE MONTH                              
         MVC   DAY,WORK+4               SAVE DAY                                
         MVC   YEAR,WORK                SAVE YEAR                               
DATE20   OI    REGDATH+4,VALPREV        IF DATE HAS BEEN VALIDATED              
         OI    REGDATH+6,TRANSMIT                                               
         EJECT                          PREVIOUSLY                              
*-----------------------------------------------------------------*             
*        READ INDEX OF WORKER FILE FOR EXISTENCE OF IT                          
*        READ THROUGH WORKER FILE TO TOTAL GOODS AND VOIDS                      
*-----------------------------------------------------------------*             
RDIND02  DS    0H                       READ WORKER FOR POSTING FILE            
         LA    R1,REGLEGH                                                       
         ST    R1,FADR                                                          
         TM    KEYCH,FRSTTIM                                                    
         BZ    WRKT195                                                          
         TM    KEYCH,FILREAD                                                    
         BO    WRKT195                                                          
         MVC   FERR#,=AL2(OK)                                                   
         XC    REMSTR,REMSTR                                                    
         BAS   RE,GETINDX                GET WORK FILE INDEX                    
         LA    R2,WRKEY                                                         
         USING UKRECD,R2                                                        
*                                                                               
         CLI   REGTYP,C'L'               IF LOCAL RUN YOU MUST HAVE             
         BNE   RDIND50                   RUN ALL SOON REGISTERS THAT            
         OC    SAVPSR,SAVPSR             ARE PRIOR TO THIS LOCAL RUN            
         BZ    RDIND90                                                          
         CLC   WORK+6(3),SAVPSR                                                 
         BL    RDIND90                                                          
         MVC   FERR#,=AL2(SPECIAL)                                              
         MVC   MSG,SPACES                                                       
         MVC   MSG(37),=C'**ERROR - MUST FIRST RUN PENDING SOON'                
         B     ERROR                                                            
*                                                                               
RDIND50  CLI   REGTYP,C'S'               IF SOON RUN YOU MUST RUN ALL           
         BNE   RDIND90                   LOCAL REGISTERS OF THIS DAY            
         OC    SAVPLR,SAVPLR             AND BEFORE                             
         BZ    RDIND90                                                          
         CLC   WORK+6(3),SAVPLR                                                 
         BNH   RDIND90                                                          
         MVC   FERR#,=AL2(SPECIAL)                                              
         MVC   MSG,SPACES                                                       
         MVC   MSG(38),=C'**ERROR - MUST FIRST RUN PENDING LOCAL'               
         B     ERROR                                                            
*                                                                               
RDIND90  DS    0H                                                               
         OI    KEYCH,FILREAD             WORKER FILE WAS READ                   
         OI    REGLEGH+4,VALPREV                                                
         XC    SEQNUM,SEQNUM                                                    
         SR    R3,R3                                                            
         ICM   R3,3,UKFILNO                                                     
         CVD   R3,DUB                                                           
         UNPK  SEQNUM,DUB                                                       
         OI    SEQNUM+(L'SEQNUM-1),X'F0'                                        
*                                                                               
*              GET TOTALS FROM WORKER FILE                                      
         ZAP   TOTCHKS,=P'0'             INITIALIZE ALL TOTAL FIELDS            
         ZAP   REMCHKS,=P'0'                                                    
         ZAP   VOIDCKS,=P'-2'            DON'T COUNT FIRST 2 VOIDS              
         ZAP   LINEVDS,=P'-2'            NEED NUMBER OF LINEUP VOIDS            
         ZAP   TOTINC,=P'0'                                                     
         MVC   WRKCOMD,=CL8'READ'                                               
*                                                                               
WRKT100  GOTO1 VDATAMGR,DMCB,WRKCOMD,WRKFILE,WRKEY,AIOAREA2,ATIA                
         TM    DMCB+8,X'10'                                                     
         BZ    WRKT120                                                          
         MVC   FERR#,=AL2(SPECIAL)                                              
         MVC   MSG,SPACES                                                       
         MVC   MSG(34),=C'PROBLEM WTH WORKER FILE - CALL DDS'                   
         B     ERROR                                                            
*                                                                               
WRKT120  L     R2,AIOAREA2                                                      
         LA    R2,4(R2)                                                         
         USING PSHEADD,R2                                                       
         CLI   PSHDEL,PSSBELQ            TRAILER RECORD (52) SIGNALS            
         BE    WRKT190                   END OF WORKER FILE                     
         CLI   PSHDEL,PSHDELQ            REMITTANCE RECORD (50)                 
         BE    WRKT130                                                          
         CLI   PSHDEL,CRDELQ             CHECK REG DETAIL ELEMENT               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                        SPECIAL ELS TO STORE INFO              
         USING CRDELD,R2                 PROCESSED ON LAST DRAFT RUN            
         CLC   CRDDATA,SPACES                                                   
         BE    WRKT100                                                          
         CLI   CRDTYPE,CRDFRST           STARTING CHECK NUMBER SAVED            
         BNE   WRKT122                   FROM LAST DRAFT RUN                    
         CLC   CRDDATA(6),SPACES                                                
         BE    WRKT100                                                          
         MVC   RUNSTR,CRDDATA                                                   
         PACK  RUNSTRP,RUNSTR                                                   
         OI    KEYCH,SECPASS                                                    
         OI    KEYCH,RANDRFT                                                    
         B     WRKT100                                                          
*                                                                               
WRKT122  CLI   CRDTYPE,CRDVOID           SAVED VOIDS FROM LAST DRAFT            
         BNE   WRKT124                   RUN                                    
         MVC   REGVOID,CRDDATA                                                  
         BAS   RE,CHARCNT                                                       
         MVC   REGVOIDH+5(1),BYTE                                               
         B     WRKT100                                                          
*                                                                               
WRKT124  CLI   CRDTYPE,CRDSKIP           SAVED SKIPS FROM LAST DRAFT            
         BE    *+6                       RUN                                    
         DC    H'0'                                                             
         MVC   REGSKIP,CRDDATA                                                  
         BAS   RE,CHARCNT                                                       
         MVC   REGSKIPH+5(1),BYTE                                               
         B     WRKT100                                                          
*                                                                               
         USING PSHEADD,R2                                                       
WRKT130  CLC   PSHDACC+1(4),=C'BANK'     IS THIS A CHECK                        
         BE    WRKT140                                                          
         OI    KEYCH,LINEUPS             SIGNAL FIRST REMIT CHECK -             
         B     WRKT100                                                          
WRKT140  CLC   PSHDSBAC+3(7),=C'***VOID'     IS THIS A VOIDED CHECK             
         BNE   WRKT150                                                          
         AP    VOIDCKS,=P'1'             INCREMENT TOTAL VOID COUNT             
         TM    KEYCH,LINEUPS                                                    
         BO    WRKT100                                                          
         AP    LINEVDS,=P'1'             INCREMENT LEAD VOID COUNT              
         B     WRKT100                                                          
WRKT150  AP    REMCHKS,=P'1'             INCREMENT REMITTANCE COUNT             
         OI    KEYCH,LINEUPS             SIGNAL FIRST REMIT CHECK -             
*                                        STOP TOTALLING LINEUPS                 
         USING TRANSD,R2                                                        
WRKT160  SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                   SHOULD FIND TRANS ELEM                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),TRNSELQ                                                    
         BNE   WRKT160                                                          
         OC    REMSTR,REMSTR                                                    
         BNZ   WRKT170                                                          
         ZAP   LEADVDS,VOIDCKS                                                  
         SP    LEADVDS,LINEVDS                                                  
         MVC   REMSTR,TRNSREF            ALL CHECK NUMBERS FROM HERE            
         CLI   REMSTR,C'0'                                                      
         BNL   *+8                                                              
         MVI   REMSTR,C'0'                                                      
         PACK  REMSTRP,REMSTR            GET CHECK NUMBER FROM FIRST            
WRKT170  AP    TOTINC,TRNSAMNT           TOTAL DISBURSEMENTS                    
         B     WRKT100                                                          
*                                                                               
WRKT190  DS    0H                                                               
         EDIT  (P6,REMCHKS),(8,REGRGDC),0        PUT ALL TOTALS FROM            
         OI    REGRGDCH+6,TRANSMIT               WORKER FILE TO SCREEN          
*                                                                               
         EDIT  (P6,VOIDCKS),(8,REGRVDC),0                                       
         OI    REGRVDCH+6,TRANSMIT                                              
         ZAP   MIDVOIDS,VOIDCKS                                                 
         SP    MIDVOIDS,LINEVDS                                                 
         SP    MIDVOIDS,=P'1'                                                   
*                                                                               
         AP    TOTCHKS,REMCHKS                                                  
         AP    TOTCHKS,VOIDCKS                                                  
         EDIT  (P6,TOTCHKS),(8,REGRTOT),0                                       
         OI    REGRTOTH+6,TRANSMIT                                              
*                                                                               
         EDIT  (P10,TOTINC),(12,REGREV),2                                       
         OI    REGREVH+6,TRANSMIT                                               
*                                                                               
WRKT195  CP    REMCHKS,=P'0'                                                    
         BNE   WRKT200                                                          
         MVC   FERR#,=AL2(SPECIAL)                                              
         MVC   MSG,SPACES                                                       
         MVC   MSG(42),=C'ZERO RUN - NO CHECKS CREATED FROM THIS RUN'           
         B     ERROR                                                            
*                                                                               
WRKT200  DS    0H                                                               
         TM    KEYCH,RANDRFT                                                    
         BO    WRKT230                                                          
         SP    REMSTRP,LEADVDS                                                  
         ZAP   RUNSTRP,REMSTRP                                                  
         SP    RUNSTRP,LINEVDS                                                  
         UNPK  RUNSTR,RUNSTRP                                                   
         OI    RUNSTR+(L'RUNSTR-1),X'F0'                                        
         B     WRKT250                                                          
WRKT230  ZAP   REMSTRP,RUNSTRP                                                  
         AP    REMSTRP,LINEVDS                                                  
WRKT250  UNPK  REMSTR,REMSTRP                                                   
         OI    REMSTR+(L'REMSTR-1),X'F0'                                        
         ZAP   REMENDP,REMSTRP            CALC CHECK END NUMBERS                
         AP    REMENDP,REMCHKS                                                  
         SP    REMENDP,=P'1'              THAT START/END THING                  
         AP    REMENDP,MIDVOIDS                                                 
         UNPK  REMEND,REMENDP                                                   
         OI    REMEND+(L'REMEND-1),X'F0'                                        
         ZAP   RUNENDP,REMENDP            CALC CHECK END NUMBERS                
         AP    RUNENDP,=P'1'                                                    
         UNPK  RUNEND,RUNENDP                                                   
         OI    RUNEND+(L'RUNEND-1),X'F0'                                        
         MVC   STAKSTR,RUNSTR                                                   
*                                                                               
         ZAP   DUB,RUNSTRP                                                      
         CVB   R6,DUB                                                           
         ST    R6,RUNSTRB                                                       
         ZAP   DUB,REMSTRP                                                      
         CVB   R6,DUB                                                           
         ST    R6,REMSTRB                                                       
         ZAP   DUB,REMENDP                                                      
         CVB   R6,DUB                                                           
         ST    R6,REMENDB                                                       
         ZAP   DUB,RUNENDP                                                      
         CVB   R6,DUB                                                           
         ST    R6,RUNENDB                                                       
*                                                                               
         B     FRUNCK                                                           
*                                                                               
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        VALIDATE AND DISPLAY CHECK NUMBER FIELDS                               
*-----------------------------------------------------------------*             
FRUNCK   DS    0H                                                               
         TM    KEYCH,FRSTTIM            IF FIRST TIME THROUGH, PUT OUT          
         BNO   FRUN05                   NUMBER I GOT FROM LEDGER REC            
         MVC   REGFRUN,RUNSTR                                                   
         CLI   LDLETTER,C' '                                                    
         BNH   *+10                                                             
         MVC   REGFRUN(L'LDLETTER),LDLETTER                                     
         OI    REGFRUNH+6,TRANSMIT                                              
         B     FRUN30                                                           
*                                                                               
FRUN05   LA    R1,REGFRUNH              ELSE VALIDATE INPUT IS NUMERIC          
         ST    R1,FADR                                                          
         BAS   RE,NUMVAL                                                        
         BNE   ERROR                                                            
         OI    REGFRUNH+4,VALPREV       IF NEW CHECK NUMBERS INPUT              
         MVI   SCLETTER,0                                                       
         MVC   CHARWK,REGFRUN                                                   
         CLI   REGFRUNH+5,6                                                     
         BL    FRUN20                                                           
         CLI   REGFRUN,C'0'                                                     
         BNL   FRUN20                                                           
         MVI   CHARWK,C'0'                                                      
         MVC   SCLETTER,REGFRUN                                                 
FRUN20   XC    PKWORK,PKWORK           NEED TO PACK FIELD OF VARIABLE           
         LA    R2,L'REGFRUN            LENGTH                                   
         ZIC   R3,REGFRUNH+5                                                    
         SR    R2,R3                                                            
         LA    R3,PKWORK                                                        
         AR    R3,R2                                                            
         ZIC   R2,REGFRUNH+5                                                    
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         BE    *+10                                                             
         MVC   0(0,R3),CHARWK                                                   
         PACK  RUNSTRP,PKWORK                                                   
         UNPK  RUNSTR,RUNSTRP                                                   
         OI    RUNSTR+(L'RUNSTR-1),X'F0'                                        
         ZAP   REMSTRP,RUNSTRP                                                  
         AP    REMSTRP,LINEVDS                                                  
         UNPK  REMSTR,REMSTRP                                                   
         OI    REMSTR+(L'REMSTR-1),X'F0'                                        
         ZAP   REMENDP,REMSTRP            CALC CHECK END NUMBERS                
         AP    REMENDP,REMCHKS                                                  
         SP    REMENDP,=P'1'                                                    
         AP    REMENDP,MIDVOIDS                                                 
         UNPK  REMEND,REMENDP                                                   
         OI    REMEND+(L'REMEND-1),X'F0'                                        
         ZAP   RUNENDP,REMENDP            CALC CHECK END NUMBERS                
         AP    RUNENDP,=P'1'                                                    
         UNPK  RUNEND,RUNENDP                                                   
         OI    RUNEND+(L'RUNEND-1),X'F0'                                        
         MVC   STAKSTR,RUNSTR                                                   
*                                                                               
FRUN30   TM    KEYCH,FRSTTIM            IF FIRST TIME THROUGH, USE              
         BNO   LREMCON                  NUMBER I GOT FROM LEDGER REC            
         MVC   REGFREM,REMSTR                                                   
         CLI   LDLETTER,C' '                                                    
         BNH   *+10                                                             
         MVC   REGFREM(L'LDLETTER),LDLETTER                                     
         OI    REGFREMH+6,TRANSMIT                                              
*                                                                               
         MVC   REGLREM,REMEND                                                   
         OI    REGLREMH+6,TRANSMIT                                              
         CLI   LDLETTER,C' '                                                    
         BNH   *+10                                                             
         MVC   REGLREM(L'LDLETTER),LDLETTER                                     
*                                                                               
         MVC   REGLRUN,RUNEND                                                   
         OI    REGLRUNH+6,TRANSMIT                                              
         CLI   LDLETTER,C' '                                                    
         BNH   *+10                                                             
         MVC   REGLRUN(L'LDLETTER),LDLETTER                                     
*                                                                               
LREMCON  DS    0H                                                               
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        VALIDATE VOIDS                                                         
*-----------------------------------------------------------------*             
CHKVOID  DS    0H                                                               
         MVC   FERR#,=AL2(OK)                                                   
         XC    SCANVDS(160),SCANVDS                                             
         XC    SCANVDS+160(160),SCANVDS+160                                     
         XC    VOIDS,VOIDS                                                      
         MVI   VOIDLNS,0                                                        
         CLI   REGVOIDH+5,0        NO VOIDS ENTERED                             
         BE    CHKSKIP                                                          
         GOTO1 VSCANNER,DMCB,(0,REGVOIDH),(10,SCANVDS),C',=,-'                  
         LA    R3,SCANVDS                                                       
         BAS   RE,FILSCAN                                                       
         MVC   VOIDLNS,BYTE                                                     
*                                                                               
CHKV05   ZIC   R2,VOIDLNS                                                       
         LA    R3,SCANVDS                                                       
         USING SCAND,R3                                                         
         CLC   BIN1,RUNENDB                                                     
         BH    CHKV80                                                           
CHKV10   TM    VALID1,X'80'        VALID NUMERIC FIELD ONE                      
         BNO   CHKV50                                                           
         TM    VALID2,X'80'        VALID NUMERIC FIELD TWO                      
         BNO   CHKV50                                                           
         SR    R4,R4               CLEAR REGISTERS                              
         SR    R5,R5                                                            
         CLI   LEN1,6              INPUT LENGTH CAN'T BE GREATER THAN 6         
         BH    CHKV70                                                           
         CLI   LEN2,6              INPUT LENGTH CAN'T BE GREATER THAN 6         
         BH    CHKV70                                                           
         ICM   R4,15,BIN1          CHECK NUMBERS TO REGISTERS FOR               
         LTR   R4,R4                                                            
         BZ    CHKV50                                                           
         ICM   R5,15,BIN2          SUBTRACTION                                  
         LTR   R5,R5                                                            
         BZ    CHKV50                                                           
         CR    R4,R5               START CANNOT BE HIGHER THAN END              
         BH    CHKV60                                                           
         SR    R5,R4               SUBTRACT AND ADD ONE TO GET NUMBER           
CHKV15   LA    R5,1(R5)            OF VOIDS                                     
         L     R4,VOIDS                                                         
         AR    R4,R5                                                            
         ST    R4,VOIDS                                                         
         LA    R3,SCANLEN(R3)                                                   
         BCT   R2,CHKV10                                                        
*                                                                               
         ZIC   R2,VOIDLNS          VERIFY THAT NO VOID RANGES OVERLAP           
         MVC   SCANSRT1(160),SCANVDS                                            
         MVC   SCANSRT1+160(160),SCANVDS+160                                    
         ZAP   DUB1,RUNSTRP                                                     
         ZAP   DUB2,REMENDP                                                     
         BAS   RE,SINOVLP                                                       
         CLC   FERR#,=AL2(OK)                                                   
         BNE   CHKV91                                                           
*                                                                               
         L     R4,VOIDS                                                         
         C     R4,=F'5500'                                                      
         BH    CHKV75                                                           
         B     CHKSKIP                                                          
*                                                                               
CHKV50   MVC   FERR#,=AL2(INVALID) ERROR MESSAGES                               
         B     CHKV91                                                           
*                                                                               
CHKV60   MVC   FERR#,=AL2(STGTREND)                                             
         B     CHKV91                                                           
*                                                                               
CHKV70   MVC   FERR#,=AL2(SPECIAL)                                              
         MVC   MSG,SPACES                                                       
         MVC   MSG(40),=C'ERROR - CANNOT BE MORE THAN 6 CHARACTERS'             
         B     CHKV91                                                           
*                                                                               
CHKV75   MVC   FERR#,=AL2(SPECIAL)                                              
         MVC   MSG,SPACES                                                       
         MVC   MSG(42),=C'ERROR - NUMBER OF VOIDS CANNOT EXCEED 5500'           
         B     CHKV91                                                           
*                                                                               
CHKV80   MVC   FERR#,=AL2(SPECIAL)                                              
         MVC   MSG,SPACES                                                       
         MVC   MSG(40),=C'ERROR - VOIDS OUTSIDE CHECK BOUNDARIES  '             
CHKV91   LA    R1,REGVOIDH                                                      
         ST    R1,FADR                                                          
         B     ERROR                                                            
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        VALIDATE SKIPS                                                         
*-----------------------------------------------------------------*             
CHKSKIP  DS    0H                                                               
         MVC   FERR#,=AL2(OK)                                                   
         XC    SCANSKP(160),SCANSKP                                             
         XC    SCANSKP+160(160),SCANSKP+160                                     
         XC    SKIPS,SKIPS                                                      
         MVI   SKIPLNS,0                                                        
         CLI   REGSKIPH+5,0                                                     
         BE    CHKS20                                                           
         GOTO1 VSCANNER,DMCB,(0,REGSKIPH),(10,SCANSKP),C',=,-'                  
         LA    R3,SCANSKP                                                       
         BAS   RE,FILSCAN                                                       
         MVC   SKIPLNS,BYTE                                                     
*                                                                               
CHKS05   ZIC   R2,SKIPLNS                                                       
         LA    R3,SCANSKP                                                       
         USING SCAND,R3                                                         
         CLC   BIN1,REMSTRB                                                     
         BL    CHKS80                                                           
         CLC   BIN1,RUNENDB                                                     
         BH    CHKS80                                                           
CHKS10   TM    VALID1,X'80'        VALID NUMERIC FIELD ONE                      
         BNO   CHKS50                                                           
         TM    VALID2,X'80'        VALID NUMERIC FIELD TWO                      
         BNO   CHKS50                                                           
         SR    R4,R4               CLEAR REGISTERS                              
         SR    R5,R5                                                            
         CLI   LEN1,6              INPUT LENGTH CAN'T BE GREATER THAN 6         
         BH    CHKS70                                                           
         CLI   LEN2,6              INPUT LENGTH CAN'T BE GREATER THAN 6         
         BH    CHKS70                                                           
         ICM   R4,15,BIN1                                                       
         LTR   R4,R4                                                            
         BZ    CHKS50                                                           
         ICM   R5,15,BIN2                                                       
         LTR   R5,R5                                                            
         BZ    CHKS50                                                           
         CR    R4,R5               START CANNOT BE HIGHER THAN END              
         BH    CHKS60                                                           
         SR    R5,R4               SUBTRACT AND ADD ONE TO GET NUMBER           
CHKS15   LA    R5,1(R5)            OF SKIPS                                     
         L     R4,SKIPS                                                         
         AR    R4,R5                                                            
         ST    R4,SKIPS                                                         
         LA    R3,SCANLEN(R3)                                                   
         BCT   R2,CHKS10                                                        
*                                                                               
         ZIC   R2,SKIPLNS          VERIFY THAT NO SKIP RANGES OVERLAP           
         MVC   SCANSRT1(160),SCANSKP                                            
         MVC   SCANSRT1+160(160),SCANSKP+160                                    
         ZAP   DUB1,REMSTRP                                                     
         ZAP   DUB2,REMENDP                                                     
         BAS   RE,SINOVLP                                                       
         CLC   FERR#,=AL2(OK)                                                   
         BNE   CHKS100                                                          
CHKS20   DS    0H                                                               
*                                                                               
         L     R4,SKIPS                                                         
         CH    R4,=H'100'                                                       
         BH    CHKS75                                                           
         B     CHKBOTH                                                          
*                                                                               
CHKS50   MVC   FERR#,=AL2(INVALID) ERROR MESSAGES                               
         B     CHKS100                                                          
*                                                                               
CHKS60   MVC   FERR#,=AL2(STGTREND)                                             
         B     CHKS100                                                          
*                                                                               
CHKS70   MVC   MSG,SPACES                                                       
         MVC   MSG(40),=C'ERROR - CANNOT BE MORE THAN 6 CHARACTERS'             
         B     CHKS90                                                           
*                                                                               
CHKS75   MVC   MSG,SPACES                                                       
         MVC   MSG(40),=C'ERROR - NUMBER OF SKIPS CANNOT EXCEED 20'             
         B     CHKS90                                                           
*                                                                               
CHKS80   MVC   MSG,SPACES                                                       
         MVC   MSG(40),=C'ERROR - SKIPS OUTSIDE CHECK BOUNDARIES  '             
*                                                                               
CHKS90   MVC   FERR#,=AL2(SPECIAL)                                              
CHKS100  LA    R1,REGSKIPH                                                      
         ST    R1,FADR                                                          
         B     ERROR                                                            
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        CONFIRM NO OVERLAPS CROSS OVER BETWEEN VOIDS AND SKIPS                 
*-----------------------------------------------------------------*             
CHKBOTH  DS    0H                                                               
         BAS   RE,DBLOVLP                                                       
         CLC   FERR#,=AL2(OK)                                                   
         BE    SP020                                                            
         LA    R1,REGVOIDH                                                      
         ST    R1,FADR                                                          
         B     ERROR                                                            
         SPACE 2                                                                
*-----------------------------------------------------------------*             
*        GET WORKER FILE INDEX RECORD                                           
*-----------------------------------------------------------------*             
GETINDX  LR    R0,RE                                                            
         XC    WRKEY,WRKEY                                                      
         LA    R2,WRKEY                  BUILD INDEX                            
         USING UKRECD,R2                                                        
         MVC   UKUSRID,TWAUSRID                                                 
         MVC   UKSYSPRG,=C'A55'                                                 
         MVC   UKSUBPRG,LEDGER                                                  
         PACK  HALF,DAY(3)                                                      
         MVC   UKDAY,HALF                                                       
         MVC   UKCLASS(1),REGTYP                                                
GETINDX3 GOTO1 AWRKIND                                                          
         BNE   GETINDXX                                                         
         CLC   UKUSRID,TWAUSRID          CHECK USER ID                          
         BNE   GETINDX3                                                         
         TM    UKSTAT,X'08'              INDICATING FILE IS KEPT                
         BO    GETINDX3                                                         
         CLC   UKCLASS(L'REGTYP),REGTYP  CHECK RUN TYPE                         
         BNE   GETINDX3                                                         
         CLC   UKSUBPRG,LEDGER           CHECK LEDGER                           
         BNE   GETINDX3                                                         
         CLC   UKDAY,HALF                                                       
         BNE   GETINDX3                                                         
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GETINDXX MVC   MSG,SPACES                                                       
         MVC   MSG(22),=C'POSTING FILE NOT FOUND'                               
         B     ERROR                                                            
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        CONFIRM RUN AND DOUBLE CHECK CHECK NUMBERS                             
*        THEN BUILD REQUEST CARD STACK                                          
*-----------------------------------------------------------------*             
SP020    DS    0H                      CHECK DRAFT RUN FIELD                    
         MVI   DRFTRUN,C' '                                                     
         CLC   REGACT(5),=C'UPDAT'                                              
         BE    SP021                                                            
         MVI   DRFTRUN,C'D'                                                     
*                                                                               
SP021    BAS   RE,NUMCHK               VERIFICATION SINCE NONE WOULD            
         CLC   FERR#,=AL2(OK)          HAVE BEEN INPUT                          
         BNE   ERROR                                                            
*                                                                               
SP022    CLC   REGACT(5),=C'MAINT'     DO I WANT TO SUBMIT REQUEST?             
         BNE   SP025                   IF NOT YET PUT OUT MESSAGE AND           
         MVC   REGMSG,SPACES           EXIT                                     
         MVC   REGMSG(18),=C'CHECK TOTALS AGREE'                                
         OI    REGMSGH+6,TRANSMIT                                               
         OI    REGFRUNH+6,CURSOR                                                
         NI    KEYCH,X'FE'                                                      
         B     EXIT                                                             
*                                                                               
SP025    DS    0H                                                               
         BAS   RE,GETINDX          GET WORKER FILE INDEX                        
         BAS   RE,UPWORK               UPDATE CHECK REG DETAIL ELEMS            
         LA    R1,REGREPH              MUST ENTER RUN ID                        
         ST    R1,FADR                                                          
         CLI   REGREPH+5,0                                                      
         BH    SP026                                                            
         MVC   FERR#,=AL2(NOINPUT)                                              
         BE    ERROR                                                            
SP026    CLI   REGREP,C' '             MAKE SURE THEY HAVEN'T JUST              
         BH    SP027                   HIT SPACE BAR                            
         MVC   FERR#,=AL2(INVALID)                                              
         BE    ERROR                                                            
*                                                                               
*        BUILD REQUEST CARD STACK                                               
SP027    GOTO1 VHEXOUT,DMCB,COMPANY,HEXCMP,1                                    
         LA    RE,STACK                                                         
         LA    RF,2000                                                          
         XCEF                                                                   
         LA    RF,STACK                                                         
         MVC   0(80,RF),SPACES         BUILD ACTUAL MONACC REQUEST              
         MVC   0(2,RF),=C'56'          CARD                                     
         MVI   10(RF),C'S'                                                      
         MVC   11(1,RF),LEDGER                                                  
         MVC   36(2,RF),YEAR                                                    
         MVC   38(2,RF),MONTH                                                   
         MVC   40(2,RF),DAY                                                     
         MVC   50(3,RF),SEQNUM                                                  
         MVC   59(1,RF),DRFTRUN                                                 
         MVC   60(1,RF),REGTYP                                                  
         LA    RF,TABW(RF)                                                      
*                                                                               
         MVC   0(6,RF),=C'FIRST='      FIRST RUN,REMIT CHECK NUMBERS            
         MVC   6(6,RF),REGFRUN                                                  
         CLI   6(RF),C'0'              CANNOT SEND OVERRIDE LETTERS             
         BNL   *+8                     IN PARM CARDS                            
         MVI   6(RF),C'0'                                                       
         MVI   12(RF),C','                                                      
         MVC   13(6,RF),REGFREM                                                 
         CLI   13(RF),C'0'             CANNOT SEND OVERRIDE LETTERS             
         BNL   *+8                     IN PARM CARDS                            
         MVI   13(RF),C'0'                                                      
         LA    RF,TABW(RF)                                                      
*                                                                               
         MVC   0(5,RF),=C'LAST='       LAST RUN REMIT CHECK NUMBERS             
         MVC   5(6,RF),REGLRUN                                                  
         CLI   5(RF),C'0'              CANNOT SEND OVERRIDE LETTERS             
         BNL   *+8                     IN PARM CARDS                            
         MVI   5(RF),C'0'                                                       
         MVI   11(RF),C','                                                      
         ZAP   REMENDP,RUNENDP                                                  
         TM    MICROSW,MICROCH                                                  
         BO    *+10                                                             
         SP    REMENDP,=P'1'                                                    
         UNPK  REMEND,REMENDP                                                   
         OI    REMEND+(L'REMEND-1),X'F0'                                        
         MVC   12(6,RF),REMEND                                                  
         CLI   12(RF),C'0'             CANNOT SEND OVERRIDE LETTERS             
         BNL   *+8                     IN PARM CARDS                            
         MVI   12(RF),C'0'                                                      
         LA    RF,TABW(RF)                                                      
*                                                                               
         ST    RF,FULL                                                          
         MVI   BYTE,0                                                           
         LA    R3,SCANVDS          SCAN OF VOIDS DATA BLOCK                     
         CLI   VOIDLNS,0           WERE THERE ANY VOIDS                         
         BE    SP100                                                            
         CLI   VOIDLNS,5                                                        
         BNH   SP028                                                            
         ZIC   R6,VOIDLNS                                                       
         SH    R6,=H'5'                                                         
         STC   R6,BYTE                                                          
         LA    R6,5                                                             
         B     SP029                                                            
SP028    ZIC   R6,VOIDLNS          NUMBER OF LINES FROM SCANNER                 
SP029    MVC   0(5,RF),=C'VOID='                                                
         LA    RF,4(RF)            BUMP CARD                                    
         USING SCAND,R3                                                         
SP030    LA    RF,1(RF)            BUMP CARD                                    
         ZIC   R5,LEN1             LENGTH OF FIRST FIELD                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DATA1       MOVE DATA TO CARD                            
         AR    RF,R5               BUMP CARD                                    
         LA    RF,1(RF)                                                         
         MVI   0(RF),C'-'          INDICATE RANGE                               
         LA    RF,1(RF)            BUMP CARD                                    
*                                                                               
         ZIC   R5,LEN2             LENGTH OF DATA FIELD TWO                     
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DATA2       MOVE DATA TO CARD                            
         AR    RF,R5                                                            
         LA    RF,1(RF)                                                         
SP050    MVI   0(RF),C','                                                       
         LA    R3,SCANLEN(R3)      BUMP TO NEXT SCAN LINE                       
         BCT   R6,SP030            PROCESS NEXT SCAN LINE                       
         MVI   0(RF),C' '          BLANK OUT LAST COMMA                         
         L     RF,FULL                                                          
         LA    RF,TABW(RF)         BUMP TO NEXT CARD                            
         ST    RF,FULL                                                          
*                                                                               
         CLI   BYTE,0                                                           
         BE    SP100                                                            
         ZIC   R6,BYTE                                                          
         MVI   BYTE,0                                                           
         B     SP029                                                            
*                                                                               
*                                                                               
SP100    DS    0H                                                               
         ST    RF,FULL                                                          
         MVI   BYTE,0                                                           
         LA    R3,SCANSKP          SCANNER SKIP DATA BLOCK                      
         CLI   SKIPLNS,0           WERE THERE ANY SKIPS                         
         BE    SP200                                                            
         CLI   SKIPLNS,5           WERE THERE ANY SKIPS                         
         BNH   SP120                                                            
         ZIC   R6,SKIPLNS                                                       
         SH    R6,=H'5'                                                         
         STC   R6,BYTE                                                          
         LA    R6,5                                                             
         B     SP121                                                            
SP120    ZIC   R6,SKIPLNS          NUMBER OF SKIP LINES FROM SCANNER            
SP121    MVC   0(5,RF),=C'SKIP='   SET UP CARD                                  
         LA    RF,4(RF)            BUMP CARD                                    
         USING SCAND,R3                                                         
SP130    LA    RF,1(RF)            BUMP BLOCK                                   
         ZIC   R5,LEN1             LENGTH OF FIRST DATA FIELD                   
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DATA1       MOVE IT TO CARD                              
         AR    RF,R5               BUMP CARD                                    
         LA    RF,1(RF)                                                         
         MVI   0(RF),C'-'          MARK RANGE                                   
         LA    RF,1(RF)            BUMP CARD                                    
*                                                                               
         ZIC   R5,LEN2             LENGTH OF DATA FIELD TWO                     
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DATA2       MOVE DATA TO CARD                            
         AR    RF,R5               BUMP CARD                                    
         LA    RF,1(RF)                                                         
SP150    MVI   0(RF),C','          SEPARATE INPUT                               
         LA    R3,SCANLEN(R3)      BUMP TO NEXT SCAN LINE                       
         BCT   R6,SP130            PROCESS NEXT SCAN CARD                       
         MVI   0(RF),C' '          BLANK OUT ENDING COMMA                       
         L     RF,FULL                                                          
         LA    RF,TABW(RF)         BUMP TO NEXT CARD                            
         ST    RF,FULL                                                          
*                                                                               
         CLI   BYTE,0                                                           
         BE    SP200                                                            
         ZIC   R6,BYTE                                                          
         MVI   BYTE,0                                                           
         B     SP121                                                            
*                                                                               
*                                                                               
SP200    DS    0H                                                               
         MVC   0(2,RF),=C'/*'                                                   
         LA    RF,TABW(RF)                                                      
         MVI   0(RF),X'FF'          MARK END OF LIST                            
         B     SP500                                                            
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        WRITE OFF TO SPOOK STACK OF REQUEST CARDS FOR SOON                     
*        REQUEST                                                                
*-----------------------------------------------------------------*             
SP500    DS    0H                                                               
         LA    R2,REGLEGH                                                       
         ST    R2,FADR                                                          
         CLC   REGACT(6),=C'UPDATE' IF USER HAS CHANGED FIRST CHAR              
         BNE   SP508                OVERRIDE, UPDATE LEDGER WITH                
         CLI   SCLETTER,0           IF USER HAS CHANGED FIRST CHAR              
         BE    SP505                OVERRIDE, UPDATE LEDGER WITH                
         CLC   LDLETTER,SCLETTER    IF OVERRIDE HASN'T CHANGED DON'T            
         BE    SP505                BOTHER TO UPDATE                            
**       BAS   RE,UPLEDG            NEW CHARACTER                               
**       CLC   FERR#,=AL2(OK)       PROBLEM WRITING LEDGER RECORD BACK?         
**       BNE   ERROR                                                            
*                                                                               
SP505    L     R4,VDATAMGR          CHECK TO SEE IF FACWK HAS AN                
         GOTO1 VCWKSCAN,DMCB,(R4)   AVAILABLE INDEX                             
         CLI   0(R1),0              IF ZERO FILE IS NOT FULL                    
         BE    SP506                                                            
         MVC   REGMSG,SPACES                                                    
         MVC   REGMSG(31),=C'** FILE FULL - STOP CALL DDS **'                   
         OI    REGMSGH+6,TRANSMIT                                               
         OI    REGFRUNH+6,CURSOR                                                
         B     EXIT                                                             
*                                                                               
SP506    BAS   RE,LEGLOCK                                                       
         CLC   FERR#,=AL2(OK)                                                   
         BNE   ERROR                                                            
*                                                                               
SP508    LA    R4,TEMP              BUILD SPOOK BLOCK                           
         USING SPOOK,R4                                                         
         XC    SPOOK(SPOOKXL),SPOOK                                             
         MVC   SPOOKXT,=C'XT='      TELL SPOOK I'M USING EXTENDED BLOCK         
         MVC   SPOOKUID,TWAUSRID    USER ID NUMBER                              
         MVC   SPOOKTID,TWATRM      TERMINAL NUMBER                             
         MVC   SPOOKAGY,TWAAGY      AGENCY ALPHA CODE                           
         MVC   SPOOKAGX,COMPANY     AGENCY HEX CODE                             
         MVC   SPOOKSYS,=C'AC'      SYSTEM CODE (FOR JCL BOOKS)                 
         MVC   SPOOKJCL,=C'56'      JCL ID                                      
         MVC   SPOOKEOD,=C'56'      EOD ID                                      
         MVI   SPOOKWEN,2           SOON STATUS                                 
         MVC   SPOOKDID,REGREP                                                  
*                                                                               
         CLC   REGACT(6),=C'UPDATE' IF NOT UPDATING SKIP THIS                   
         BNE   SP509                                                            
         MVI   SPOOKWEN,5           SET UPDATIVE SOON                           
         OI    SPOOKSTA,X'02'       SET REPORT STATUS INVISIBLE                 
*                                                                               
SP509    CLI   SPOOKDID+1,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+1,C'*'                                                  
         CLI   SPOOKDID+2,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+2,C'*'                                                  
SP510    MVI   FLAG,1                                                           
         LA    R2,STACK                                                         
SP520    MVC   WORK(80),SPACES                                                  
         MVC   WORK(TABW),0(R2)                                                 
         OC    WORK(TABW),SPACES                                                
         CLI   TABW(R2),X'FF'                                                   
         BNE   *+8                                                              
         MVI   FLAG,X'FF'                                                       
         GOTO1 VSPOON,DMCB,(X'FF',ATWA),(FLAG,WORK),(C'A',ACOMFACS),   X        
               BOOKNM,TEMP                                                      
*                                                                               
         CLI   0(R1),1                                                          
         BNE   *+6                                                              
         DC    H'0'                JCL BOOK NOT FOUND                           
*                                                                               
         CLI   0(R1),2                                                          
         BNE   *+6                                                              
         DC    H'0'                JOB TABLE FULL                               
*                                                                               
         CLI   FLAG,1                                                           
         BNE   *+8                                                              
         MVI   FLAG,2                                                           
         CLI   TABW(R2),X'FF'                                                   
         BE    GOODX                                                            
         LA    R2,TABW(R2)                                                      
         B     SP520                                                            
*                                                                               
GOODX    DS    0H                                                               
         L     R5,DMCB+20                  PUT OUT SOON MESSAGE                 
         SR    R3,R3                                                            
         ICM   R3,3,6(R5)                                                       
         MVC   MSG(9),=C'REGISTER '                                             
         MVC   MSG+9(L'SPOOKDID),SPOOKDID                                       
         MVI   MSG+9+L'SPOOKDID,C','                                            
         LA    R5,MSG+15                                                        
         EDIT  (R3),(5,(R5)),ALIGN=LEFT                                         
         MVC   MSG+21(26),=C'WILL RUN SOON - ENTER NEXT'                        
         LA    R2,L'MSG                                                         
         GOTO1 =V(SQUASHER),DMCB,MSG,(R2),RR=RB                                 
         LA    R1,REGDATH                                                       
         ST    R1,FADR                                                          
         MVC   REGACT,SPACES              SINCE REG WAS JUST REQUESTED          
         OI    REGACTH+6,TRANSMIT         CLEAR CONTROL FIELDS TO               
         MVC   REGREP,SPACES              AVOID ACCIDENTAL RE-REQUEST           
         OI    REGREPH+6,TRANSMIT                                               
         NI    REGTYPH+4,X'FF'-VALPREV    HAS DATE BEEN VALIDATED               
         MVC   FERR#,=AL2(SPECIAL)                                              
         J     ERROR                                                            
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        ROUTINE TO VALIDATE A CHECK TOTALS AGREE                               
*-----------------------------------------------------------------*             
NUMCHK   NTR1                                                                   
NUMC030  DS    0H                                                               
         LA    R1,REGFRUNH                                                      
         ST    R1,FADR                                                          
*                                                                               
         L     R2,VOIDS                                                         
         CVD   R2,RNVOID                                                        
         L     R2,SKIPS                                                         
         CVD   R2,RNSKIP                                                        
*                                                                               
         ZAP   DUB,RUNSTRP                                                      
         CVB   R6,DUB                                                           
         ST    R6,RUNSTRB                                                       
         ZAP   DUB,REMSTRP                                                      
         CVB   R6,DUB                                                           
         ST    R6,REMSTRB                                                       
         ZAP   DUB,REMENDP                                                      
         CVB   R6,DUB                                                           
         ST    R6,REMENDB                                                       
         ZAP   DUB,RUNENDP                                                      
         CVB   R6,DUB                                                           
         ST    R6,RUNENDB                                                       
*                                                                               
         CLI   REGVOIDH+5,0                                                     
         BE    NUMC050                                                          
         LA    R3,SCANVDS                                                       
         BAS   RE,RENUM                                                         
*                                                                               
NUMC050  CLI   REGSKIPH+5,0                                                     
         BE    NUMC060                                                          
         LA    R3,SCANSKP                                                       
         BAS   RE,RENUM                                                         
*                                                                               
NUMC060  DS    0H                                                               
         MVC   REGFRUN,RUNSTR                MOVE NUMBERS TO SCREEN             
         OI    REGFRUNH+6,TRANSMIT           AND TRANSMIT                       
         MVC   REGFREM,REMSTR                                                   
         OI    REGFREMH+6,TRANSMIT                                              
         MVC   REGLREM,REMEND                                                   
         OI    REGLREMH+6,TRANSMIT                                              
         MVC   REGLRUN,RUNEND                                                   
         OI    REGLRUNH+6,TRANSMIT                                              
         CLI   SCLETTER,C' '                     CHECK OVERRIDE CHAR            
         BNH   NUMC200                           IF IT WAS CHANGED PUT          
         MVC   REGFRUN(L'SCLETTER),SCLETTER      IT TO SCREEN                   
         MVC   REGFREM(L'SCLETTER),SCLETTER                                     
         MVC   REGLREM(L'SCLETTER),SCLETTER                                     
         MVC   REGLRUN(L'SCLETTER),SCLETTER                                     
*                                                                               
NUMC200  EDIT  (P6,REMCHKS),(8,REGIGDC),0   CALCULATE NUMBERS                   
         OI    REGIGDCH+6,TRANSMIT          FOR INPUT SUMMARY                   
         ZAP   TEMPTOT,RNVOID                                                   
         AP    TEMPTOT,VOIDCKS                                                  
         EDIT  (P4,TEMPTOT),(8,REGIVDC),0                                       
         OI    REGIVDCH+6,TRANSMIT                                              
         AP    TEMPTOT,REMCHKS                                                  
         EDIT  (P4,TEMPTOT),(8,REGITOT),0                                       
         OI    REGITOTH+6,TRANSMIT                                              
         MVC   FERR#,=AL2(OK)                                                   
         B     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        TAKE VOIDS AND SKIPS AND READJUST ALL CHECK NUMBERS                    
*-----------------------------------------------------------------*             
RENUM    NTR1                                                                   
         USING SCAND,R3                                                         
         LA    R2,10                   LOOP PASSES                              
REN40    CLI   LEN1,0                                                           
         BE    REN70                                                            
         ICM   R4,15,BIN1                                                       
         ICM   R5,15,BIN2                                                       
         SR    R5,R4                                                            
         LA    R5,1(R5)                                                         
         CLC   BIN1,REMSTRB                                                     
         BH    REN50                                                            
         L     R6,REMSTRB                                                       
         AR    R6,R5                                                            
         ST    R6,REMSTRB                                                       
         L     R6,REMENDB                                                       
         AR    R6,R5                                                            
         ST    R6,REMENDB                                                       
         L     R6,RUNENDB                                                       
         AR    R6,R5                                                            
         ST    R6,RUNENDB                                                       
         B     REN70                                                            
*                                                                               
REN50    CLC   BIN1,REMENDB                                                     
         BH    REN60                                                            
         L     R6,REMENDB                                                       
         AR    R6,R5                                                            
         ST    R6,REMENDB                                                       
         L     R6,RUNENDB                                                       
         AR    R6,R5                                                            
         ST    R6,RUNENDB                                                       
         B     REN70                                                            
*                                                                               
REN60    DS    0H                                                               
*        CLC   BIN1,RUNENDB                                                     
*        BH    REN70                                                            
         L     R6,RUNENDB                                                       
         AR    R6,R5                                                            
         ST    R6,RUNENDB                                                       
         B     REN70                                                            
*                                                                               
REN70    LA    R3,SCANLEN(R3)                                                   
         BCT   R2,REN40                                                         
*                                                                               
         L     R6,RUNSTRB                                                       
         CVD   R6,DUB                                                           
         ZAP   RUNSTRP,DUB                                                      
         UNPK  RUNSTR,RUNSTRP                                                   
         OI    RUNSTR+(L'RUNSTR-1),X'F0'                                        
*                                                                               
         L     R6,REMSTRB                                                       
         CVD   R6,DUB                                                           
         ZAP   REMSTRP,DUB                                                      
         UNPK  REMSTR,REMSTRP                                                   
         OI    REMSTR+(L'REMSTR-1),X'F0'                                        
*                                                                               
         L     R6,REMENDB                                                       
         CVD   R6,DUB                                                           
         ZAP   REMENDP,DUB                                                      
         UNPK  REMEND,REMENDP                                                   
         OI    REMEND+(L'REMEND-1),X'F0'                                        
*                                                                               
         L     R6,RUNENDB                                                       
         CVD   R6,DUB                                                           
         ZAP   RUNENDP,DUB                                                      
         UNPK  RUNEND,RUNENDP                                                   
         OI    RUNEND+(L'RUNEND-1),X'F0'                                        
         B     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        FILL IN SCANNER BLOCK SO THAT ALL ENTRIES LOOK                         
*        LIKE A RANGE                                                           
*-----------------------------------------------------------------*             
FILSCAN  NTR1                                                                   
         USING SCAND,R3                                                         
         SR    R2,R2                                                            
         LA    R4,10                                                            
FILSC10  OC    0(SCANLEN,R3),0(R3)                                              
         BZ    FILXIT                                                           
         OC    LEN2,LEN2                                                        
         BNZ   FILSC20                                                          
         MVC   LEN2,LEN1                                                        
         MVC   VALID2,VALID1                                                    
         MVC   BIN2,BIN1                                                        
         MVC   DATA2,DATA1                                                      
FILSC20  LA    R2,1(R2)                                                         
         LA    R3,SCANLEN(R3)                                                   
         BCT   R4,FILSC10                                                       
FILXIT   STC   R2,BYTE                                                          
         B     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        MAKE SURE SCAN LINES HAVE NO OVERLAPPING GROUPS                        
*        R2 = NUMBER OF GROUPS RETURNED FROM SCANNER                            
*        R3 = ADDRESS OF SCAN SPACE TO BE SORTED                                
*        DUB1 = LOWER CHECK BOUNDARY (PACKED)                                   
*        DUB2 = UPPER CHECK BOUNDARY (PACKED)                                   
*-----------------------------------------------------------------*             
SINOVLP  NTR1                                                                   
         GOTO1 VCALLOV,DMCB,0,X'D900A12'                                        
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(0,SCANSRT1),10,32,20,12                               
*                                                                               
         CVB   R6,DUB1                                                          
         USING SCAND,R3                                                         
         LA    R3,SCANSRT1                                                      
         LA    R4,10                                                            
         XC    TEMPTOT,TEMPTOT                                                  
SIN120   OC    BIN1,BIN1                                                        
         BZ    SIN150                                                           
         CLC   BIN1,TEMPTOT                                                     
         BNH   SIN300                                                           
         MVC   FULL,BIN1                                                        
         C     R6,FULL                                                          
         BH    SIN250                                                           
SIN150   MVC   TEMPTOT,BIN2                                                     
         LA    R3,SCANLEN(R3)                                                   
         BCT   R4,SIN120                                                        
*                                                                               
         CVB   R6,DUB2                                                          
         A     R6,VOIDS                                                         
         A     R6,SKIPS                                                         
         C     R6,FULL                                                          
         BL    SIN250                                                           
*                                                                               
SIN200   MVC   FERR#,=AL2(OK)                                                   
         B     EXIT                                                             
*                                                                               
SIN250   MVC   FERR#,=AL2(SPECIAL)                                              
         MVC   MSG,SPACES                                                       
         MVC   MSG(31),=C'OUTSIDE CHECK NUMBER BOUNDARIES'                      
         B     EXIT                                                             
*                                                                               
SIN300   MVC   FERR#,=AL2(SPECIAL)                                              
         MVC   MSG,SPACES                                                       
         MVC   MSG(24),=C'OVERLAPPING CHECK RANGES'                             
         B     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        MAKE SURE SKIPS AND VOIDS HAVE NO CROSSOVER OVERLAPS                   
*-----------------------------------------------------------------*             
DBLOVLP  NTR1                                                                   
         MVC   SCANSRT1(160),SCANVDS                                            
         MVC   SCANSRT1+160(160),SCANVDS+160                                    
         MVC   SCANSRT2(160),SCANSKP                                            
         MVC   SCANSRT2+160(160),SCANSKP+160                                    
         GOTO1 VCALLOV,DMCB,0,X'D900A12'                                        
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(0,SCANSRT1),20,32,20,12                               
*                                                                               
         USING SCAND,R3                                                         
         LA    R3,SCANSRT1                                                      
         LA    R4,20                                                            
         XC    TEMPTOT,TEMPTOT                                                  
DBL120   OC    BIN1,BIN1                                                        
         BZ    DBL150                                                           
         CLC   BIN1,TEMPTOT                                                     
         BNH   DBL300                                                           
DBL150   MVC   TEMPTOT,BIN2                                                     
         LA    R3,SCANLEN(R3)                                                   
         BCT   R4,DBL120                                                        
*                                                                               
DBL200   MVC   FERR#,=AL2(OK)                                                   
         B     EXIT                                                             
*                                                                               
DBL300   MVC   FERR#,=AL2(SPECIAL)                                              
         MVC   MSG,SPACES                                                       
         MVC   MSG(25),=C'OVERLAPPING CHECK RANGES:'                            
         B     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        UPDATE CHECK REGISTER DETAIL ELEMENTS                                  
*-----------------------------------------------------------------*             
UPWORK   NTR1                                                                   
UPW150   MVC   WRKCOMD,=CL8'READ'                                               
         GOTO1 VDATAMGR,DMCB,WRKCOMD,WRKFILE,WRKEY,AIOAREA2,ATIA                
         TM    DMCB+8,X'10'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         USING CRDELD,R2                                                        
         L     R2,AIOAREA2                                                      
         LA    R2,4(R2)                                                         
         CLI   CRDEL,PSHDELQ             REMITTANCE RECORD (50) MEANS           
         BE    EXIT                      I'M DONE WITH SPECIAL ELS              
         CLI   CRDEL,CRDELQ              REMITTANCE RECORD (50) MEANS           
         BNE   UPW150                    I'M DONE WITH SPECIAL ELS              
*                                                                               
         MVC   CRDDATA,SPACES                                                   
         CLI   CRDTYPE,CRDFRST           STARTING CHECK NUMBER                  
         BNE   UPW162                                                           
         MVC   CRDDATA(L'RUNSTR),RUNSTR                                         
         B     UPW180                                                           
UPW162   CLI   CRDTYPE,CRDVOID           VOIDS                                  
         BNE   UPW164                                                           
         MVC   CRDDATA(L'REGVOID),REGVOID                                       
         OC    CRDDATA,SPACES                                                   
         B     UPW180                                                           
UPW164   CLI   CRDTYPE,CRDSKIP           SKIPS                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CRDDATA(L'REGSKIP),REGSKIP                                       
         OC    CRDDATA,SPACES                                                   
         B     UPW180                                                           
*                                                                               
UPW180   MVC   WRKCOMD,=CL8'WRITE'                                              
         GOTO1 VDATAMGR,DMCB,WRKCOMD,WRKFILE,WRKEY,AIOAREA2,ATIA                
         B     UPW150                                                           
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        ROUTINE TO VALIDATE A NUMBER FIELD                                     
*-----------------------------------------------------------------*             
NUMVAL   NTR1                                                                   
         MVC   WORK(6),=6X'F0'                                                  
         LR    R5,R1                                                            
         LA    R5,8(R5)                                                         
         CLI   0(R5),C'0'                                                       
         BNL   *+8                                                              
         LA    R5,1(R5)                                                         
         L     R1,FADR                                                          
         MVC   FERR#,=AL2(INVNUM)                                               
         ZIC   R4,5(R1)                                                         
         BCTR  R4,0                                                             
         CLI   8(R1),C'0'                                                       
         BNL   *+6                                                              
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),0(R5)                                                    
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),=6X'F0'                                                  
         BNE   NUMSX                                                            
         MVC   FERR#,=AL2(OK)                                                   
NUMSX    CLC   FERR#,=AL2(OK)                                                   
         B     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        UPDATE THE LEDGER RECORD IF THEY HAVE OVERRIDDEN                       
*        OPTIONAL NUMBER TYPE                                                   
*-----------------------------------------------------------------*             
LEGLOCK  NTR1                                                                   
         MVC   FERR#,=AL2(OK)                                                   
         GOTO1 VCALLOV,DMCB,0,X'D9000A66',0     GET A(SETLOCK)                  
         L     RF,0(R1)                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVI   KEY+1,C'S'                                                       
         MVC   KEY+2(1),LEDGER                                                  
         GOTO1 (RF),DMCB,(C'L',KEY),ACOMFACS    SETLOCK                         
         CLI   DMCB+4,0                                                         
         BE    EXIT                                                             
         MVC   FERR#,=AL2(SPECIAL)                                              
         MVC   MSG,SPACES                                                       
         MVC   MSG(26),=C'LEDGER IS TEMPORARILY LOCKED'                         
         B     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        UPDATE THE LEDGER RECORD IF THEY HAVE OVERRIDDEN                       
*        OPTIONAL NUMBER TYPE                                                   
*-----------------------------------------------------------------*             
*UPLEDG   NTR1                                                                  
*         MVC   FERR#,=AL2(OK)                                                  
*         MVC   KEY,SPACES          GET COMPANY RECORD FOR START OF             
*         MVC   KEY(1),COMPANY      FINANCIAL YEAR                              
*         MVI   KEY+1,C'S'                                                      
*         MVC   KEY+2(1),LEDGER                                                 
*         GOTO1 AREADL,AIOAREA1           READ RECORD                           
*         BE    UPL05                                                           
*         MVC   FERR#,=AL2(CANTAMND)                                            
*         B     EXIT                                                            
**                                                                              
*UPL05    L     R2,AIOAREA1                                                     
*         AH    R2,DATADISP                                                     
*UPL10    CLI   0(R2),0                  IF NO '54' EL FOUND THAT               
*         BE    UPL30                    MATCHES SIGNON ID, PUT OUT             
*         CLI   0(R2),X'54'              ERROR AND EXIT                         
*         BE    UPL20                                                           
*UPL15    ZIC   R3,1(R2)                                                        
*         AR    R2,R3                                                           
*         B     UPL10                                                           
*                                                                               
*         USING ACOFFD,R2                                                       
*UPL20    CLC   TWAUSRID,ACOFID          TRY NEXT 54 EL FOR MATCH               
*         BNE   UPL15                                                           
*         MVC   ACOFNTYP,SCLETTER                                               
*         GOTO1 AWRITE,AIOAREA1           READ RECORD                           
**                                                                              
*UPL30    DS    0H                                                              
*         B     EXIT                                                            
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        NEED COUNT OF BYTES USED IN CHECK REGISTER DETAIL ELS                  
*        R2 = ADDRESS OF CHECK REG DETAIL ELEMENT                               
*-----------------------------------------------------------------*             
CHARCNT  NTR1                                                                   
         USING CRDELD,R2                                                        
         LA    R5,CRDDATA                                                       
         LA    R3,0                                                             
         LA    R4,80                                                            
CHAR10   CLI   0(R5),C' '                                                       
         BE    CHAR20                                                           
         LA    R5,1(R5)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,CHAR10                                                        
CHAR20   DS    0H                                                               
         STC   R3,BYTE                                                          
         B     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        CLEAR ALL NON-KEY FIELDS UPON KEY CHANGE                               
*        CANNOT USE TWAXC - ONLY CLEARS UNPROTECTED FIELDS                      
*-----------------------------------------------------------------*             
CLRSCRN  NTR1                                                                   
         MVC   REGFRUN,SPACES                CLEAR                              
         OI    REGFRUNH+6,TRANSMIT           TRANSMIT                           
         MVC   REGFREM,SPACES                CLEAR                              
         OI    REGFREMH+6,TRANSMIT           TRANSMIT                           
         MVC   REGLREM,SPACES                ETC...                             
         OI    REGLREMH+6,TRANSMIT                                              
         MVC   REGLRUN,SPACES                                                   
         OI    REGLRUNH+6,TRANSMIT                                              
         MVC   REGVOID,SPACES                                                   
         OI    REGVOIDH+6,TRANSMIT                                              
         MVC   REGSKIP,SPACES                                                   
         OI    REGSKIPH+6,TRANSMIT                                              
         MVC   REGIGDC,SPACES                                                   
         OI    REGIGDCH+6,TRANSMIT                                              
         MVC   REGRGDC,SPACES                                                   
         OI    REGRGDCH+6,TRANSMIT                                              
         MVC   REGIVDC,SPACES                                                   
         OI    REGIVDCH+6,TRANSMIT                                              
         MVC   REGRVDC,SPACES                                                   
         OI    REGRVDCH+6,TRANSMIT                                              
         MVC   REGITOT,SPACES                                                   
         OI    REGITOTH+6,TRANSMIT                                              
         MVC   REGRTOT,SPACES                                                   
         OI    REGRTOTH+6,TRANSMIT                                              
         MVC   REGREV,SPACES                                                    
         OI    REGREVH+6,TRANSMIT                                               
         B     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        FORMAT OUTPUT MESSAGE/EXTRA MESSAGE INTO MSG & EXIT.                   
*-----------------------------------------------------------------*             
         USING GETTXTD,PARMS                                                    
ERROR    MVC   REGMSG,MSG                                                       
         CLC   FERR#,=AL2(SPECIAL)                                              
         BE    ERR900                                                           
         MVC   REGMSG,SPACES                                                    
         XC    PARMS(L'GTBLOCK),PARMS                                           
         MVC   GTMSGNO,FERR#                                                    
*        SR    RF,RF                                                            
*        CLC   XMSG,SPACES                                                      
*        BNH   ERR020                                                           
*        LA    RE,XMSG+L'XMSG-1                                                 
*        LA    RF,L'XMSG                                                        
*RR005   CLI   0(RE),C' '          FIND FIRST CHARACTER                         
*        BH    ERR020                                                           
*        BCTR  RE,0                                                             
*        BCT   RF,ERR005                                                        
                                                                                
*ERR020   GOTO1 VGETTXT,PARMS,,REGMSGH,(GTMERR,DMCB),(L'XMSG,XMSG),             
*               (GT1NOREF,0),0                                                  
ERR020   GOTO1 VGETTXT,PARMS,,REGMSGH,(C'E',DMCB),(L'XMSG,XMSG),       X        
               (X'80',0),0                                                      
                                                                                
ERR900   OI    REGMSGH+6,TRANSMIT                                               
         ICM   R1,15,FADR                                                       
         BNZ   ERR910                                                           
         LA    R1,REGACTH          DEFAULT FIELD                                
                                                                                
ERR910   OI    6(R1),CURSOR                                                     
         B     EXIT                                                             
*                                                                               
OKXIT    SR    RB,RB                                                            
ERRXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*-----------------------------------------------------------------*             
* EXTRACT AND PRE-VALIDATE AN INPUT FIELD.                                      
*                                                                               
* ADDRESS OF FIELD HEADER IS PASSED IN R1. RETURN WITH:-                        
*                                                                               
*              FADR     = A(INPUT FIELD HEADER)                                 
*              FERR#    = MISSING INPUT FIELD IF NO INPUT                       
*              FNDX     = ZERO                                                  
*              FLDH     = INPUT FIELD HEADER                                    
*              FLD      = EXTRACTED & SPACE FILLED INPUT FIELD                  
*                                                                               
* RETURN WITH CC=EQU IF NO INPUT IN FIELD                                       
*-----------------------------------------------------------------*             
FVAL     NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         MVI   FNDX,0                                                           
         MVC   FERR#,=AL2(NOINPUT)                                              
         ST    R1,FADR                                                          
         MVC   FLDH,0(R1)                                                       
         MVC   FLD,SPACES                                                       
*                                                                               
FVAL2    TM    1(R1),PROTECT       IF FLD IS PROTECTED                          
         BNO   FVAL4               CONSTRUCT AN INPUT LENGTH                    
         ZIC   RE,0(R1)                                                         
         LA    RF,0(RE,R1)                                                      
         BCTR  RF,0                                                             
         SH    RE,=H'8'                                                         
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   RE,*-10                                                          
         STC   RE,FLDH+5                                                        
*                                                                               
FVAL4    ZIC   RE,FLDH+5           MOVE FLD TO FLD                              
         SH    RE,=H'1'                                                         
         BM    FVALX                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R1)                                                     
         MVC   FERR#,=AL2(OK)                                                   
FVALX    CLC   FERR#,=AL2(NOINPUT)                                              
         B     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        EXTRACT NAME FROM A RECORD INTO WORK.                                  
*        RECORD IS ADDRESSED BY WORD AT R1.                                     
*-----------------------------------------------------------------*             
GETNAME  NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         L     R1,0(R1)                                                         
         MVC   WORK,SPACES                                                      
         AH    R1,DATADISP                                                      
         SR    RF,RF                                                            
GETNAME2 CLI   0(R1),0                                                          
         BE    GETNAMEX                                                         
         LA    R0,3                                                             
         CLI   0(R1),X'20'                                                      
         BE    GETNAME3                                                         
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     GETNAME2                                                         
GETNAME3 IC    RF,1(R1)                                                         
         SR    RF,R0                                                            
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    RF,*+8                                                           
         B     GETNAMEX                                                         
         MVC   WORK(0),0(R1)                                                    
GETNAMEX B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* ACCOUNT FILE I/O EXECUTIVE.                                                   
*                                                                               
* I/O IS EXECUTED ON KEY INTO I/O AREA ADDRESSED BY R1. COMMAND IS              
* PASSED IN THE HIGH ORDER BYTE OF RF AS FOLLOWS:-                              
*                                                                               
*              BITS 0-3 = COMMAND NUMBER (1-5 SEE IOCMNDS)                      
*                   5ON = PASS BACK DELETED RECORDS                             
*                   6ON = READ KEY WITH LOCK AND SAVE REC IN TIA                
*                   7ON = SAVE KEY IN KEYSAVE BEFORE I/O                        
*                                                                               
* RETURN WITH CC=NEQ ON I/O ERROR WITH FERR# SET TO ERROR MESSAGE NUM.          
*---------------------------------------------------------------------*         
ACCIO    NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         STCM  RF,8,DUB            SAVE COMMAND BYTE                            
         L     R1,0(R1)                                                         
         ST    R1,AIOAREA          SAVE A(I/O AREA)                             
         SRL   RF,28                                                            
         SLL   RF,3                                                             
         LA    RF,IOCMNDS-8(RF)                                                 
         ST    RF,DMCB             SET A(COMMAND)                               
         TM    DUB,X'04'                                                        
         BZ    *+8                                                              
         OI    DMCB,X'08'          SET TO PASS BACK DELETES                     
         TM    DUB,X'02'                                                        
         BZ    *+8                                                              
         OI    DMCB,X'80'          SET TO READ WITH LOCK                        
         TM    DUB,X'01'                                                        
         BZ    *+10                                                             
         MVC   KEYSAVE,KEY         SAVE KEY                                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,,IOFILE,KEY,AIOAREA                                
         MVC   FERR#,=AL2(OK)      SET FIELD ERROR NUMBER                       
         CLI   DMCB+8,0                                                         
         BNE   ACCIO2                                                           
*                                                                               
ACCIO1   TM    DUB,X'02'           IF RECORD FOUND AND LOCKED, SAVE IN          
         BNO   ACCIOX              TIA                                          
         L     RE,AIOAREA                                                       
         LH    RF,ACLENGTH-ACKEYD(RE)                                           
         L     R0,ATIA                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     ACCIOX                                                           
*                                                                               
ACCIO2   MVC   FERR#,=AL2(NOTFOUND)   ERRORS                                    
         TM    DMCB+8,X'10'           TEST N/F                                  
         BO    ACCIOX                                                           
         MVC   FERR#,=AL2(DELETED)                                              
         TM    DMCB+8,X'02'        TEST DELETED                                 
         BO    ACCIOX                                                           
         MVC   FERR#,=AL2(IOERROR) EOF/ERR/DUP/LOCKS                            
*                                                                               
ACCIOX   CLC   FERR#,=AL2(OK)      EXIT WITH CC=EQ IF I/O OK                    
         B     EXIT                                                             
*                                                                               
*                                  LIST OF I/O COMMANDS/FILES                   
IOCMNDS  DS    0CL8                                                             
         DC    C'DMADD   '                                                      
         DC    C'DMRDHI  '                                                      
         DC    C'DMREAD  '                                                      
         DC    C'DMRSEQ  '                                                      
         DC    C'DMWRT   '                                                      
IOFILE   DC    C'ACCOUNT '                                                      
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        WORKER FILE ROUTINES                                                   
*-----------------------------------------------------------------*             
WRKIO    NTR1                                                                   
         L     RA,ABASE2                                                        
         SRL   RF,24                                                            
         SLL   RF,3                                                             
         LA    R2,WRKCMNDS-8(RF)   P1                                           
         LA    R3,WRKFILE          P2                                           
         LA    R4,WRKEY            P3                                           
         L     R5,AIOAREA2         P4                                           
         L     R6,ATIA             P5                                           
         STM   R2,R6,DMCB                                                       
         GOTO1 VDATAMGR,DMCB                                                    
         TM    DMCB+8,X'10'        TEST NOT FOUND                               
         BZ    WRKIOX                                                           
         MVC   FERR#,=AL2(SPECIAL)                                              
WRKIOX   CLC   FERR#,=AL2(OK)      CC=EQ IF OK                                  
         B     EXIT                                                             
*                                                                               
WRKCMNDS DS    0CL8                                                             
         DC    C'IND     '                                                      
*                                                                               
WRKFILE  DC    C'WKFILE'                                                        
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        LITERALS ETC.                                                          
*-----------------------------------------------------------------*             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
RELO     DS    F                                                                
*-----------------------------------------------------------------*             
* TABLE OF A&V-TYPES FOR RELOCATING INTO GLOBAL W/S.                            
*                                                                               
*              BYTE 0-3 = A/V-TYPE ADDRESS                                      
*                   1-N = HIGH ORDER BYTE VALUES DELIMITED BY X'FF'             
*-----------------------------------------------------------------*             
LEDGTAB  DS    0H                                                               
         DC    C'YVWXUSTPQ'                                                     
         DC    X'FF'                                                            
*                                                                               
ROUTTAB  DS    0X                                                               
         DC    AL3(FVAL),X'00FF'                                                
         DC    AL3(GETNAME),X'00FF'                                             
         DC    AL3(ACCIO),X'1025273436404250FF'                                 
         DC    AL3(WRKIO),X'01FF'                                               
         DC    X'FF'                                                            
*                                                                               
TABW     EQU   80                                                               
*                                                                               
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        DSECT TO COVER GLOBAL W/S                                              
*-----------------------------------------------------------------*             
GWS      DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
FULL     DS    F                                                                
FULL1    DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
BYTE     DS    CL1                                                              
BYTE1    DS    CL1                                                              
DIV      DS    PL16                                                             
WORK     DS    CL40                                                             
WORK1    DS    CL40                                                             
FLAG     DS    X                                                                
DMCB     DS    6F                                                               
PARMS    DS    6F                                                               
HEXCMP   DS    CL2                                                              
TEMP     DS    CL400                                                            
*                                                                               
MSG      DS    CL60                OUTPUT ERROR MESSAGE                         
XMSG     DS    CL12                USER SUPPLIED EXTRA MESSAGE                  
SPACES   DS    CL132               SET TO SPACES BY ROOT                        
FADR     DS    A                   A(CURRENT INPUT FIELD HEADER)                
FLDH     DS    CL8                 EXTRACTED INPUT FIELD HEADER                 
FLD      DS    CL80                EXTRACTED & SPACE FILLED INPUT FIELD         
FERR#    DS    XL2                 ERROR MESSAGE NUMBER                         
FNDX     DS    X                   MULTIPLE FIELD INDEX                         
*                                                                               
SCANSRT  DS    0CL640                                                           
SCANSRT1 DS    CL320                                                            
SCANSRT2 DS    CL320                                                            
*                                                                               
COMPANY  DS    CL1                 COMPANY CODE                                 
TERMTYPE DS    CL1                 TERMINAL TYPE (T2260 OR T3270)               
TODAYB   DS    CL3                 TODAY IN BINARY                              
         DS    13C                 SPARE                                        
GWSINITX DS    0H                  INITIALISED UP TO HERE                       
         EJECT                                                                  
*-----------------------------------------------------------------*             
*                                                                               
*-----------------------------------------------------------------*             
AWORK    DS    A                   ROOT RD SAVE AREA                            
ABASE1   DS    A                   A(ROOT PHASE)                                
ABASE2   DS    A                   A(ROOT PHASE+4096)                           
APHASE   DS    A                   A(APPLICATION PHASE)                         
ATWA     DS    A                   A(TWA)                                       
ATIA     DS    A                   A(TIA)                                       
ACOMFACS DS    A                   A(COMMON FACILITIES LIST)                    
*                                  EXTERNAL DIRECTORY                           
VCALLOV  DS    V                                                                
VDATAMGR DS    V                                                                
VDATCON  DS    V                                                                
VGETTXT  DS    V                                                                
VGETPROF DS    V                                                                
VHELLO   DS    V                                                                
VSCANNER DS    V                                                                
VGETFACT DS    V                                                                
VDATVAL  DS    V                                                                
VHEXOUT  DS    V                                                                
VSPOON   DS    V                                                                
VCWKSCAN DS    V                                                                
VXTRAINF DS    V                   EXTRA INFORMATION                            
         DS    16C                                                              
*                                  INTERNAL/EXTERNAL DIRECTORY                  
AROUTINE DS    0A                                                               
AFVAL    DS    A                   A(FLD PRE-VALIDATION ROUTINE)                
AGETNAME DS    A                   A(NAME EXTRACT ROUTINE)                      
AADD     DS    A                   A(ADD)                                       
ARDHI    DS    A                   A(READ HIGH/SAVEKEY/PASSDEL)                 
ARDHIL   DS    A                   A(READ HIGH/SAVEKEY/PASSDEL/LOCK)            
AREAD    DS    A                   A(READ)                                      
AREADL   DS    A                   A(READ/LOCK)                                 
ASEQ     DS    A                   A(READ SEQ)                                  
ASEQL    DS    A                   A(READ SEQ/LOCK)                             
AWRITE   DS    A                   A(WRITE)                                     
AWRKIND  DS    A                   A(WORKER INDEX READ)                         
         DS    12C                 SPARE                                        
*                                                                               
AIOAREA  DS    A                   A(CURRENT I/O AREA)                          
AIOAREA1 DS    A                                                                
AIOAREA2 DS    A                                                                
PHASE    DS    X                   OVERLAY PHASE NUMBER                         
DATADISP DS    H                   DISPLACEMENT TO FIRST ELEMENT                
*                                   FIELDS RETURNED/USED BY GETACC              
KEY      DS    CL42                I/O KEY                                      
KEYSAVE  DS    CL42                I/O KEY SAVE AREA                            
LASTKEY  DS    CL42                I/O KEY SAVE AREA                            
ORIGIN   DS    CL6                                                              
BOOKNM   DS    CL10                                                             
WRKCOMD  DS    CL8                                                              
STACK    DS    2000C                                                            
IOAREAS  DS    4096C               2*1024 BYTE I/O AREAS                        
GWSX     DS    0C                                                               
*                                                                               
*                                  EQUATES FOR FLDHDRS                          
SCAND    DSECT                                                                  
LEN1     DS    CL1                                                              
LEN2     DS    CL1                                                              
VALID1   DS    CL1                                                              
VALID2   DS    CL1                                                              
BIN1     DS    CL4                                                              
BIN2     DS    CL4                                                              
DATA1    DS    CL10                                                             
DATA2    DS    CL10                                                             
SCANLEN  EQU   *-LEN1                                                           
*                                  EQUATES FOR FLDHDRS                          
VALPREV  EQU   X'20'                                                            
NOTVALP  EQU   X'DF'                                                            
PROTECT  EQU   X'20'               (ALSO BUDTSTAT)                              
TRANSMIT EQU   X'80'                                                            
CURSOR   EQU   X'40'                                                            
IOERROR  EQU   0                                                                
NOINPUT  EQU   1                                                                
INVALID  EQU   2                                                                
INVNUM   EQU   3                                                                
INVTYPE  EQU   5                                                                
INVFORMT EQU   6                                                                
LEDGNVAL EQU   9                                                                
INVACTN  EQU   12                                                               
INVDATE  EQU   13                                                               
INVWC    EQU   19                                                               
INVAMNT  EQU   25                                                               
INVBTYPE EQU   27                                                               
DUPED    EQU   35                                                               
TOOSHORT EQU   36                                                               
TOOLONG  EQU   37                                                               
LOCKOUT  EQU   55                                                               
RECXIST  EQU   59                                                               
DUPLICT  EQU   59                                                               
STGTREND EQU   64                                                               
NOTALTYP EQU   69                                                               
INVACTSQ EQU   75                                                               
NOTFOUND EQU   76                                                               
DELETED  EQU   77                                                               
KEYDIFFS EQU   78                                                               
INVOPTN  EQU   79                                                               
TOOSMALL EQU   80                                                               
TOOBIG   EQU   81                                                               
CANTAMND EQU   87                                                               
NOTCLOSE EQU   88                                                               
INCOMPAT EQU   89                                                               
NOCHANGE EQU   90                                                               
LEVNXIST EQU   95                                                               
NTDELETD EQU   97                                                               
NOPARNTH EQU   98                                                               
FINCMPAT EQU   99                                                               
BTYPNXST EQU   100                                                              
BTYPACLE EQU   101                                                              
BTYPCALE EQU   102                                                              
WRONGLEV EQU   103                                                              
INVYEAR  EQU   104                                                              
NOTAVAIL EQU   105                                                              
TOOMANY  EQU   107                                                              
INVNTADD EQU   109                                                              
WRNADV#  EQU   357       CANNOT UPDATE-NOT CONNECTED TO HOME ADV-USE &T         
SYSNOTUP EQU   358       ADV CURRENTLY NOT ALLOWING UPDATES-TRY LATER           
NOTAUTH  EQU   360       NOT AUTHORIZED TO UPDATE-SEE SECURITY ADMIN            
SPECIAL  EQU   254+X'FF00'                                                      
OK       EQU   255+X'FF00'                                                      
*                                                                               
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        DSECT TO COVER TWA                                                     
*-----------------------------------------------------------------*             
       ++INCLUDE FATWA                                                          
         ORG   TWAUSER                                                          
         DS    CL48                                                             
       ++INCLUDE ACREGFFD                                                       
         ORG   TWAD+2100                                                        
PROGPROF DS    CL16                                                             
***                                                                             
TWASV    DS    0H                                                               
*                                                                               
*        SCANNER TOTALS FOR VOIDS AND SKIPS                                     
RNVOID   DS    D                   VOIDS IN BINARY                              
RNSKIP   DS    D                   SKIPS IN BINARY                              
LINEVDS  DS    F                   LINE-UP VOIDS FROM WORKER FILE               
LEADVDS  DS    F                   LINE-UP VOIDS FROM WORKER FILE               
VOIDS    DS    F                   TOTAL VOIDS FROM SCANNER                     
SKIPS    DS    F                   TOTAL SKIPS FROM SCANNER                     
VOIDLNS  DS    C                   LINES FROM SCANNER                           
SKIPLNS  DS    C                   LINES FROM SCANNER                           
*                                                                               
WRKEY    DS    CL32                WORKER KEY                                   
*                                                                               
RUNSTR   DS    CL6                 RUN START CHAR                               
RUNSTRP  DS    PL4                 RUN START PACKED                             
REMSTR   DS    CL6                 REM START CHAR                               
REMSTRP  DS    PL4                 REM START PACKED                             
REMEND   DS    CL6                 REM END CHAR                                 
REMENDP  DS    PL4                 REM END PACKED                               
RUNEND   DS    CL6                 RUN END CHAR                                 
RUNENDP  DS    PL4                 RUN END PACKED                               
STAKSTR  DS    CL6                 RUN START CHAR                               
RUNSTRB  DS    F                                                                
REMSTRB  DS    F                                                                
REMENDB  DS    F                                                                
RUNENDB  DS    F                                                                
*                                                                               
PKWORK   DS    CL6                                                              
CHARWK   DS    CL6                                                              
TEMPTOT  DS    PL4                                                              
*                                                                               
*        TOTALS FROM WORKER FILE                                                
TOTCHKS  DS    PL6                 TOTAL CHECKS                                 
REMCHKS  DS    PL6                 TOTAL REMITTANCES                            
VOIDCKS  DS    PL6                 TOTAL VOIDS                                  
MIDVOIDS DS    PL6                 VOIDS FOR MULTI-PAGE REMITTANCES             
TOTINC   DS    PL10                TOTAL DOLLARS                                
SCANHDH  DS    F                                                                
SCANVDS  DS    CL320                                                            
SCANSKP  DS    CL320                                                            
TWASVEN  DS    0H                                                               
*                                                                               
MONTH    DS    CL2                                                              
DAY      DS    CL2                                                              
YEAR     DS    CL2                                                              
SAVPSR   DS    CL3                                                              
SAVPLR   DS    CL3                                                              
LEDGER   DS    CL1                                                              
LDLETTER DS    CL1                  FIRST CHAR OVERRIDE FROM LEDGER             
SCLETTER DS    CL1                  FIRST CHAR OVERRIDE FROM SCREEN             
DRFTRUN  DS    CL1                                                              
SEQNUM   DS    CL3                                                              
LASTACT  DS    CL5                                                              
*                                                                               
KEYCH    DS    CL1                                                              
FRSTTIM  EQU   X'01'                                                            
FILREAD  EQU   X'02'                                                            
SECPASS  EQU   X'04'                                                            
LASTPAS  EQU   X'08'                                                            
LINEUPS  EQU   X'10'                                                            
RANDRFT  EQU   X'20'                                                            
*                                                                               
MICROSW  DS    CL1                                                              
MICROCH  EQU   X'80'                                                            
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDACCFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDACCFACS                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* FAXTRAINF                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
* DMWRKRD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKRD                                                        
         PRINT ON                                                               
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKRK                                                        
         PRINT ON                                                               
* DDSPOOK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOK                                                        
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019ACREG00   01/22/14'                                      
         END                                                                    
