*          DATA SET TAGEN38    AT LEVEL 023 AS OF 07/30/12                      
*PHASE T70238C,*                                                                
         TITLE 'T70238 - COMMERCIAL LIST'                                       
T70238   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70238                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R6,TWAHOLE                                                       
         USING TWAHOLED,R6                                                      
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
COM10    GOTO1 INITIAL,DMCB,0                                                   
*                                                                               
         BRAS  RE,SETSTART       SET LIST TO START WHERE WE LEFT OFF            
*                                                                               
*                                                                               
COM15    CLI   MODE,VALKEY             FIRST TIME IN                            
         BE    VK                                                               
*                                                                               
COM20    CLI   MODE,LISTRECS                                                    
         BNE   COM30                                                            
         TWAXC SCOSELH,SCOLSTH,PROT=Y   CLEAR SCREEN                            
         LA    R2,LISTAR                                                        
         MVC   LISTAR,SPACES       CLEAR 1ST LINE                               
         B     LR                       AFTER LAST LISTMON                      
*                                                                               
COM30    CLI   MODE,PRINTREP                                                    
         BNE   COMX                                                             
         XC    KEY,KEY             START REPORT FROM BEGINING                   
         ZAP   COUNTER,=P'0'       LINE COUNTER                                 
         LA    R2,COSPECS          SPECS                                        
         ST    R2,SPECS                                                         
         LA    R2,HDHOOK           HEADLINE HOOK                                
         ST    R2,HEADHOOK                                                      
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
COMX     B     XIT                                                              
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY                                                              
*                                                                               
VK       GOTO1 FLDVAL,DMCB,(X'40',SCOAGYH),(X'80',SCOCOPH)                      
         BE    VK230                                                            
*                                                                               
         MVC   SCOHD1+4(7),=C'Comm ID'                                          
         MVC   SCOHD1+17(3),=C'Prd'                                             
         OI    SCOHD1H+6,X'80'                                                  
                                                                                
         MVC   SCOHD2(12),=C'Product Name'                                      
         OI    SCOHD2H+6,X'80'                                                  
                                                                                
         OC    TGCLGACC,TGCLGACC                                                
         BZ    VK10                                                             
         CLI   SCOCLGH+5,0                                                      
         BNE   VK10                                                             
         MVC   SCOCLG,TGCLGACC                                                  
         MVI   SCOCLGH+5,L'TGCLGACC                                             
         OI    SCOCLGH+6,X'80'                                                  
*                                                                               
VK10     LA    R2,SCOAGYH          VALIDATE AGENCY                              
         MVI   BYTE,0              SET ENFORCE LIMIT ACCESS RESTRICT.           
         CLI   5(R2),0             IF NO AGENCY INPUT                           
         BNE   VK30                                                             
         OC    TGCLGACC,TGCLGACC   IF NO CLIENT GROUP ACCESS                    
         BNZ   VK20                                                             
*                                                                               
         USING FAWSSVRD,R1                                                      
         LA    R1,BLOCK                                                         
         MVC   FAWSTOKN(3),=C'STF'                                              
         MVI   FAWSTOKN+3,1                                                     
         MVI   FAWSACTN,FAWSARST   RECALL STAFF2 INFORMATION VIA WWSVR          
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,TGAS2ACC                                                 
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         BNE   VK15                                                             
         DROP  R1                                                               
*                                                                               
         USING TAVAD,R1                                                         
         L     R1,TGAS2ACC                                                      
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS AGENCY LIMITS                   
         BNZ   VK50                INPUT REQUIRED                               
         DROP  R1                                                               
*                                                                               
VK15     CLI   SCOCLGH+5,0         ELSE, OKAY IF CLIENT GROUP INPUT             
         BE    VK50                                                             
         B     VK60                                                             
VK20     CLI   SCOCLGH+5,0         IF NO CLIENT GROUP INPUT                     
         BNE   VK60                                                             
         CLC   TGAGY,SPACES        BUT THERE IS A GLOBAL AGENCY                 
         BNH   VK60                                                             
         MVC   SCOAGY,TGAGY        THEN USE IT                                  
         MVI   5(R2),L'TGAGY                                                    
         OI    6(R2),X'80'                                                      
         B     VK40                FOR CLIENT GROUP DISPLAY CHECK               
*                                  (INPUT IN AGENCY FIELD)                      
VK30     OC    TGCLGACC,TGCLGACC   IF CLIENT GROUP ACCESS DEFINED               
         BZ    VK50                                                             
         CLI   SCOCLGH+5,0         AND NO CLIENT GROUP INPUT                    
         BNE   *+12                                                             
VK40     BAS   RE,CHKINCLG         CHKS IN CLI GRP (MAY SET CGRP FLD)           
         BNE   VK50                                                             
         MVI   BYTE,X'80'          SKIP LIMIT ACCESS RESTRICTION                
         J     VK55                                                             
*                                                                               
VK50     CLI   SCOMUSH+5,0         NO CLIENT GROUP OR MUSIC                     
         JE    *+12                                                             
VK55     CLI   SCOAGYH+5,0         HAS CLIENT GROUP OR MUSIC                    
         JE    VK60                IF NO AGY, DON'T DO RECVAL                   
*                                                                               
         GOTO1 RECVAL,DMCB,(BYTE,TLAYCDQ),(X'20',(R2))  VALIDATE AGENCY         
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
VK60     LA    R2,SCOCLIH          VALIDATE CLIENT                              
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VK70                                                             
         CLI   SCOMUSH+5,0         ALLOW IF MUSIC INPUT                         
         BNE   VK80                                                             
         CLI   SCOCLGH+5,0               OR CLIENT GROUP INPUT                  
         BNE   VK80                                                             
         CLC   TGCLI,SPACES              OR SOMETHING IN GLOBAL                 
         BNH   MISSERR                                                          
VK70     GOTO1 RECVAL,DMCB,TLCLCDQ,(R2)        VALIDATE CLIENT                  
*                                                                               
VK80     XC    TGPRD,TGPRD                                                      
         LA    R2,SCOPRDH          VALIDATE PRODUCT                             
         CLI   5(R2),0                                                          
         BE    VK90                                                             
         GOTO1 RECVAL,DMCB,TLPRCDQ,(R2)  VALIDATE PRODUCT                       
*                                                                               
VK90     LA    R2,SCOMUSH          VALIDATE MUSIC                               
         CLI   5(R2),0             IF INPUT                                     
         BE    VK100                                                            
         CLI   SCOCLGH+5,0         DO NOT ALLOW MUSIC IF CLI GRP INUT           
         BNE   INVERR                                                           
         CLI   SCOCOPH+5,0         OR COM POOL INPUT                            
         BNE   INVERR                                                           
         GOTO1 RECVAL,DMCB,TLMUCDQ,(R2)  VALIDATE MUSIC                         
*                                                                               
VK100    LA    R2,SCOFMTH          FORMAT                                       
         MVI   RDSEQ,C'C'                                                       
         CLI   5(R2),0             IF INPUT                                     
         BE    VK110                                                            
         CLI   SCOMUSH+5,0         DO NOT ALLOW FORMAT IF MUSIC INPUT           
         BNE   INVERR                                                           
         MVI   RDSEQ,C'C'          SET TO CODE SEQUENCE                         
         CLI   8(R2),C'C'                                                       
         BE    VK110                                                            
         MVI   RDSEQ,C'A'          ALPHA - NAME                                 
         CLI   8(R2),C'A'                                                       
         BNE   INVERR                                                           
*                                                                               
VK110    LA    R2,SCOSTRH          VALIDATE START AT?                           
         XC    SVSTART,SVSTART                                                  
         CLI   5(R2),0                                                          
         BE    VK120                                                            
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SVSTART(0),8(R2)                                                 
*                                                                               
VK120    LA    R2,SCOMEDH          VALIDATE MEDIA FILTER                        
         CLI   5(R2),0                                                          
         BE    VK130                                                            
         GOTO1 MEDVAL,DMCB,8(R2)   VALIDATE MEDIA                               
         BNE   INVERR                                                           
*                                                                               
VK130    LA    R2,SCOCLGH          VALIDATE CLIENT GROUP FIELD                  
         CLI   5(R2),0             IF NO INPUT                                  
         BE    VK150                                                            
         OC    SCOCLG,SPACES                                                    
         OC    TGCLGACC,TGCLGACC   AND CLIENT GROUP ACCESS DEFINED              
         BZ    VK140                                                            
         CLC   SCOCLG,TGCLGACC     MUST BE EQUAL                                
         BNE   INVERR                                                           
VK140    GOTO1 RECVAL,DMCB,TLCGCDQ,(R2)                                         
*                                                                               
VK150    LA    R2,SCOTYPEH         VALIDATE COMMERCIAL TYPE FIELD               
         CLI   5(R2),0                                                          
         BE    VK180                                                            
         CLI   5(R2),1             IF INPUT IS 1 CHARACTER LONG                 
         BNE   VK160               * LISTS ALL COMMERCIAL WITH NO               
         CLI   8(R2),C'*'          TYPE SETTING                                 
         BE    VK180                                                            
         LA    RF,8(R2)            COMMERCIAL TYPE BY ITSELF LISTS              
         B     VK170               JUST THAT COMMERCIAL TYPE                    
VK160    CLI   8(R2),C'-'                                                       
         BNE   INVERR                                                           
         LA    RF,9(R2)                                                         
VK170    GOTO1 CTYPVAL,DMCB,0(RF)  COMMERCIAL TYPE PRECEDED BY MINUS            
         BNE   INVERR              LISTS ALL BUT THAT COMMERCIAL TYPE           
*                                                                               
VK180    LA    R2,SCOVERSH         VALIDATE VERSIONS                            
         CLI   5(R2),0                                                          
         BE    VK190                                                            
         CLI   SCOVERS,C'N'                                                     
         BE    VK190                                                            
         CLI   SCOVERS,C'Y'                                                     
         BNE   INVERR                                                           
*                                                                               
VK190    LA    R2,SCOCOPH          VALIDATE COMMERCIAL GROUP FIELD              
         CLI   5(R2),0             IF NO INPUT                                  
         BE    VK200                                                            
         CLI   SCOCLGH+5,0         CLIENT GROUP INFORMATION NOT                 
         BNE   INVERR              ALLOWED                                      
         OC    SCOCOP,SPACES                                                    
         GOTO1 RECVAL,DMCB,TLOGCDQ,(X'04',(R2))                                 
         BE    VK200                                                            
         MVC   FULL(6),TGPRD                                                    
         XC    TGPRD,TGPRD                                                      
         GOTO1 RECVAL,DMCB,TLOGCDQ,(X'04',(R2))                                 
         MVC   TGPRD,FULL                                                       
         BNE   INVERR                                                           
*                                                                               
VK200    BAS   RE,VALOPTS          VALIDATE OPTIONS FIELD                       
                                                                                
         CLI   SCOCLGH+5,0            IF CLIENT GROUP INPUT                     
         BE    VK210                                                            
         CLI   SCOAGYH+5,0            AND NO AGENCY INPUT                       
         BNE   VK210                                                            
         MVC   SCOHD1+17(3),=C'Agy'   SHOW AGY INSTEAD OF PRD                   
                                                                                
VK210    CLI   SCOVERS,C'Y'           IF VERSIONS INPUT                         
         BNE   VK220                  CHANGE HEADINGS                           
         MVC   SCOHD1+4(7),=C'Ver ID '                                          
         MVC   SCOHD2(12),=C'Comm ID     '                                      
*                                                                               
VK220    GOTO1 FLDVAL,DMCB,(X'20',SCOAGYH),(X'80',SCOCOPH)                      
         XC    KEY,KEY             RESET KEY                                    
         GOTOR INIT                INITIALIZE SYSIO                             
*                                                                               
VK230    LA    R3,PFCOMML          REGULAR PFTAB                                
         CLI   SCOVERS,C'Y'                                                     
         BNE   VK240                                                            
         LA    R3,PFVCOMML         VERSION PFTAB                                
         B     VK250                                                            
VK240    CLI   SCOCLGH+5,0         IF CLIENT GROUP INPUT                        
         BE    VK250                                                            
         CLI   SCOAGYH+5,0         AND NO AGENCY INPUT                          
         BNE   VK250                                                            
         LA    R3,PFCLGCOM         USE CLIENT GROUP PFTAB                       
VK250    GOTO1 INITIAL,DMCB,(R3)                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PUTS CLIENT GROUP TO SCREEN FIELD AND SETS               
*              CC TO SKIP AGY LIMIT CHECK IF AGENCY INPUT NOT IN                
*              LIMITED AGENCY LIST BUT IS IN CLIENT GROUP ACCESS                
         SPACE 1                                                                
CHKINCLG NTR1                                                                   
         OC    8(L'SCOAGY,R2),SPACES                                            
*        BAS   RE,LIMITCHK         IF AGENCY INPUT NOT IN LIMITED LIST          
*        BE    NO                                                               
*                                                                               
         XC    KEY,KEY             BUT IT IS IN THEIR CLI GRP ACCESS            
         LA    R4,KEY                                                           
         USING TLCLPD,R4                                                        
         MVI   TLCLPCD,TLCLGCDQ                                                 
         MVC   TLCLGCLG,TGCLGACC                                                
         MVC   TLCLGAGY,8(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLCLGCLI-TLCLPD),KEYSAVE                                     
         BNE   NO                                                               
         MVC   SCOCLG,TGCLGACC     SET CLIENT GROUP TO SCREEN                   
         MVI   SCOCLGH+5,L'TGCLGACC                                             
         OI    SCOCLGH+6,X'80'                                                  
         B     YES                 AND SKIP LIMIT AGY RESTRICT. CHECK           
         SPACE 2                                                                
*&&DO                                                                           
LIMITCHK NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         BE    YES                                                              
*                                                                               
         LHI   R2,1                                                             
*                                                                               
         USING FAWSSVRD,R1                                                      
LIMCHK10 LA    R1,BLOCK                                                         
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R2,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF2 INFORMATION VIA WWSVR          
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,TGAS2ACC                                                 
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0           IF NOT FOUND, STAFF HAS NO ACCESS            
         BNE   NO                                                               
         DROP  R1                                                               
*                                                                               
         AHI   R2,1                                                             
*                                                                               
         USING TAVAD,R1                                                         
         L     R1,TGAS2ACC                                                      
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS NO AGENCY LIMITS,               
         BZ    YES                 STAFF HAS ACCESS TO ALL RECORDS              
*                                                                               
LIMCHK20 CLI   0(R1),0             RECALL NEXT RECORD FROM WSSVR                
         BE    LIMCHK10                                                         
*                                                                               
         CLC   SCOAGY,TAVAAGY      IF AGENCY IS FOUND IN STAFF LIMITS           
         BE    YES                 ACCESS IS GRANTED                            
*                                                                               
LIMCHK30 ZIC   RE,TAVALEN          BUMP TO NEXT VALID AGENCY/CLIENT             
         AR    R1,RE               ELEMENT                                      
         J     LIMCHK20                                                         
         DROP  R4                                                               
*&&                                                                             
         EJECT                                                                  
*              VALIDATE OPTIONS FIELD                                           
         SPACE 1                                                                
VALOPTS  NTR1                                                                   
         XC    OPTS,OPTS           CLEAR OPTION FIELDS                          
*                                                                               
         LA    R2,SCOOPTSH         R2 = A(FIELD)                                
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0               INVALID INPUT                                
         BZ    INVERR                                                           
*                                                                               
VOPT20   MVC   ERRDISP,SCDISP1                                                  
         CLI   SCLEN1,0            LHS IS REQUIRED                              
         BE    INVERR                                                           
         LA    R4,OPTTAB           LOOK UP IN OPTIONS TABLE                     
         USING OPTD,R4                                                          
         ZIC   RF,SCLEN1                                                        
         BCTR  RF,0                                                             
*                                                                               
VOPT30   EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),OPTLHS   MATCH ON LHS                                 
         BE    VOPT35                                                           
         LA    R4,OPTNEXT                                                       
         CLI   0(R4),X'FF'                                                      
         BE    INVERR              END OF TABLE                                 
         B     VOPT30                                                           
*                                                                               
VOPT35   LH    RF,OPTDISP          DISP TO VAL. ROUTINE                         
         AR    RF,RB                                                            
         MVC   ERRDISP,SCDISP2                                                  
         BASR  RE,RF               GO VALIDATE                                  
*                                                                               
VOPT40   LA    R3,SCANNEXT                                                      
         BCT   R0,VOPT20                                                        
*                                                                               
VOPTX    OI    4(R2),X'20'                                                      
         MVI   ERRDISP,0                                                        
         B     XIT                                                              
         EJECT                                                                  
*              OPTION VALIDATION SUBSIDIARY ROUTINES                            
         SPACE 1                                                                
         USING SCAND,R3            R3 = A(SCAN BLOCK ENTRY)                     
         SPACE 1                                                                
VALREL   DS    0H                  RELEASED COMMERCIALS                         
         CLI   SCDATA2,C'Y'        INCLUDE                                      
         BE    VALRELX                                                          
         CLI   SCDATA2,C'N'        EXCLUDE (DEFAULT)                            
         BNE   INVERR                                                           
VALRELX  MVC   OPTREL,SCDATA2                                                   
         BR    RE                                                               
         SPACE 2                                                                
VALLOCK  DS    0H                  LOCKED COMMERCIALS                           
         CLI   SCDATA2,C'Y'        INCLUDE                                      
         BE    VALLOCKX                                                         
         CLI   SCDATA2,C'N'        EXCLUDE (DEFAULT)                            
         BNE   INVERR                                                           
VALLOCKX MVC   OPTLOCK,SCDATA2                                                  
         BR    RE                                                               
         SPACE 2                                                                
VALDATE  CLI   SCOVERS,C'Y'                                                     
         BE    INVERR                                                           
         LR    R5,RE                                                            
         BAS   RE,VALDATER                                                      
         LR    RE,R5               SAVE ADDRESS OF RTRN POINT                   
         BR    RE                                                               
         SPACE 2                                                                
VALALL   DS    0H                                                               
         OC    OPTS,OPTS           NO OTHER OPTIONS CAN BE SELECTED             
         BNZ   INVERR                                                           
         MVI   OPTALL,C'Y'                                                      
         B     VALDATE             GO VALIDATE ACTIVE DATE                      
         EJECT                                                                  
*                                                                               
*        VALIDATE DATE FIELD                                                    
*                                                                               
VALDATER NTR1                                                                   
         TM    SCVAL2,X'80'        TEST VALID NUMERIC                           
         BZ    VALD10                                                           
         CLI   SCLEN2,3            MORE THAN 3 CHARS?                           
         BH    VALD10              YES, ASSUME USE OF DATE                      
         XR    RF,RF                                                            
         ICM   RF,7,SCBIN2+1       YES, MOVE TO RF                              
         LCR   RF,RF               AND COMPLEMENT                               
         GOTO1 ADDAY,DMCB,TGTODAY0,WORK,(RF) GO BACK N'DAYS                     
         B     VALD20                                                           
*                                                                               
VALD10   LA    R4,PERBLK                                                        
         USING PERVALD,R4                                                       
         MVC   BYTE,SCLEN2         SET LENGTH INTO BYTE                         
         OI    BYTE,X'40'          SET VALIDATE AS MM/DD                        
         GOTO1 PERVAL,DMCB,(BYTE,SCDATA2),('PVINSGLS+PVIN1DYL',(R4))            
         TM    4(R1),PVRCINV1      TEST START                                   
         BO    DATINV                                                           
         MVC   WORK(6),PVALESTA                                                 
*                                                                               
VALD20   GOTO1 DATCON,DMCB,(0,WORK),(1,OPTDATE) LAST ACTIVE DATE                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        CONTROL LISTING RECORDS                                                
*                                                                               
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
*                                                                               
LR10     MVI   NLISTS,16           IN ORDER TO GET BACK CONTROL                 
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15              AFTER 1 FULL PAGE                         
*                                                                               
         XC    TIQSTART,TIQSTART                                                
         XC    TIQSKEY,TIQSKEY                                                  
*                                                                               
         CLI   TIERROR,TINOTOLN                                                 
         BNE   LR20                                                             
         LA    R2,CONRECH                                                       
         B     ONLINERR            CANNOT BE RUN ONLINE                         
*                                                                               
LR20     CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK                 
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(18,R1),=C'COMMERCIAL RECORDS'                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
LRX      B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        PROCESS SYSIO RECORDS                                                  
*                                                                               
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      IF PROCESSING RECORDS                        
         BNE   XIT                                                              
                                                                                
         L     R4,TIAREC           R4=A(COMMERCIAL/VERSION RECORD)              
         MVI   TGVER,0                                                          
                                                                                
         USING TLCOD,R4                                                         
         CLI   SCOCOPH+5,0         IF LISTING BY COMMERCIAL GROUP               
         BE    LH10                                                             
         MVC   TIAGY,TLCOAGY       SET AGENCY                                   
         MVC   TICLI,TLCOCLI       CLIENT                                       
         MVC   TIPRD,TLCOPRD       AND PRODUCT CODE                             
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
LH10     MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SVTACOEL,0(R4)      SAVE PRIMARY COMMERCIAL DETAILS              
                                                                                
         CLI   TACOMED,TACOMEDE    SKIP IF MEDIA = EVENT                        
         BE    XIT                                                              
         DROP  R4                                                               
                                                                                
         BRAS  RE,PROAVER          FILTER/PROCESS VERSIONS BY ALPHA             
         BNE   XIT                                                              
         BRAS  RE,PROCVER          FILTER/PROCESS VERSIONS BY CODE              
         BNE   XIT                                                              
                                                                                
         BAS   RE,FILTER           FILTER BASED ON OPTIONS                      
         BNE   XIT                                                              
                                                                                
         MVC   PRNAME,SPACES       CLEAR PRD NAME                               
         CLI   TIPRD,C' '          PRD EXISTS ?                                 
         BH    LH20                                                             
         MVI   PRNAMEH,24                                                       
         MVC   AIO,TIAREC                                                       
         GOTO1 CHAROUT,DMCB,TAFNELQ,PRNAMEH,TAFNTPRD                            
         MVC   AIO,AIO1                                                         
         B     LH30                                                             
                                                                                
LH20     MVC   TGAGY,TIAGY         RE-SET CODES AND GET PRD NAME                
         MVC   TGCLI,TICLI                                                      
         MVC   TGPRD,TIPRD                                                      
         GOTO1 XNAME,DMCB,TLPRCDQ,PRNAME,TIKEY                                  
                                                                                
         USING COMD,R2                                                          
LH30     MVC   COPRD,TIPRD         SHOW PRODUCT CODE                            
         CLI   SCOCLGH+5,0         IF CLIENT GROUP INPUT                        
         BE    LH40                                                             
         CLI   SCOAGYH+5,0         AND NO AGENCY INPUT                          
         BNE   LH40                                                             
         MVC   COAGY,TIAGY         SHOW AGENCY CODE INSTEAD                     
LH40     MVC   COTITLE,TINAME      TITLE                                        
         MVC   COPRNAM,PRNAME      PRODUCT NAME                                 
         MVC   COLEN,TISEC         SECONDS                                      
         MVC   COID,TICID          COMM ID CODE                                 
                                                                                
         USING TACOD,R4                                                         
         LA    R4,SVTACOEL         R4=A(COMMERCIAL DETAILS ELEMENT)             
         MVC   COMEDIA,TACOMED                                                  
         GOTO1 DATCON,DMCB,(1,TACOFCYC),(5,COFFC)                               
         DROP  R4                                                               
                                                                                
         MVC   COVER,SPACES                                                     
         CLI   TGVER,0             SHOW VERSION NUMBER                          
         BE    LH50                                                             
         EDIT  TGVER,COVER,ALIGN=LEFT                                           
                                                                                
LH50     CLI   SCOVERS,C'Y'        IF LISTING VERSIONS                          
         BNE   LH60                                                             
         MVC   COPRNAM,SPACES      DISPLAY PRIMARY COMMERCIAL ID                
         MVC   COPRNAM(L'TGCID),TGCID                                           
                                                                                
LH60     MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS                            
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   LH70                                                             
         GOTO1 CATCHIOS            ENSURE DON'T DO TOO MANY IO'S                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AP    COUNTER,=P'1'                                                    
         B     XIT                                                              
                                                                                
LH70     CLI   LISTNUM,15          IF END OF PAGE                               
         BNE   LH80                BUT THERE ARE MORE RECS                      
         MVC   MYMSGNO1,OKNO           PUT MSG - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SCOSELH                                                       
         B     ERRXIT                                                           
                                                                                
LH80     GOTO1 LISTMON             CALL LISTMON                                 
         B     XIT                      GENCON                                  
         DROP  R2                                                               
                                                                                
*              ROUTINE TO FILTER ON OPTIONS                                     
         SPACE 1                                                                
FILTER   NTR1                                                                   
         CLI   SCOAGYH+5,0         IF AGENCY FILTER DEFINED                     
         BE    *+14                                                             
         CLC   TIFAGY,TIAGY        AGENCY MUST MATCH                            
         BNE   NO                                                               
                                                                                
         CLI   SCOPRDH+5,0         IF PRODUCT FILTER DEFINED                    
         BE    *+14                                                             
         CLC   TIFPRD,TIPRD        PRODUCT MUST MATCH                           
         BNE   NO                                                               
                                                                                
         USING TACOD,R4                                                         
         LA    R4,SVTACOEL         R4=A(COMMERCIAL DETAILS ELEMENT)             
                                                                                
         CLI   SCOMEDH+5,0         IF MEDIA FILTER DEFINED                      
         BE    *+14                                                             
         CLC   SCOMED,TACOMED      MEDIA MUST MATCH                             
         BNE   NO                                                               
                                                                                
         CLI   SCOCLGH+5,0         IF CLIENT GROUP FILTER DEFINED               
         BE    *+14                                                             
         CLC   TIFCLG,TACOCLG      CLIENT GROUP MUST MATCH                      
         BNE   NO                                                               
                                                                                
         CLI   SCOCOPH+5,0         IF COMMERCIAL POOL FILTER DEFINED            
         BE    *+14                                                             
         CLC   TIFPRG,TACOCGRP     COMMERCIAL POOL  MUST MATCH                  
         BNE   NO                                                               
                                                                                
         BRAS  RE,TYPEFILT         FILTER ON COMMERCIAL TYPE                    
         BNE   NO                                                               
                                                                                
         CLI   OPTALL,C'Y'         IF RELEASED, LOCKED AND ACTIVE               
         BNE   FILT10              REQUESTED                                    
         TM    TACOSTAT,TACOSTRL   AND COMMERCIAL IS RELEASED                   
         BO    YES                                                              
         TM    TACOSTAT,TACOSTLO   OR LOCKED                                    
         BO    YES                 OK TO DISPLAY                                
                                                                                
FILT10   OC    OPTDATE,OPTDATE     IF ANY ACTIVITY DATE OPTION                  
         BZ    FILT30                                                           
         OC    TACOPDTE,TACOPDTE   IF THERE IS NO LAST PYMT DATE                
         BZ    FILT20                                                           
         CLC   TACOPDTE,OPTDATE                                                 
         BNL   FILT30              IF LAST PYMT DATE BELOW CUTOFF DATE          
         B     NO                  REJECT RECORD                                
         DROP  R4                                                               
                                                                                
         USING TAACD,R4                                                         
FILT20   MVC   AIO,TIAREC          ELSE, IF NO LAST PYMT DATE                   
         MVI   ELCODE,TAACELQ                                                   
         GOTO1 ACTVOUT,DMCB,(X'20',0)                                           
         MVC   AIO,AIO1                                                         
         ICM   R4,15,DMCB          R4=A(ACTIVITY ELEMENT)                       
         BZ    NO                                                               
         CLC   TAACCDTE,OPTDATE    COMPARE AGAINST LAST ACTIVITY DATE           
         BL    NO                  IGNORE RECORD                                
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         LA    R4,SVTACOEL         R4=A(COMMERCIAL ELEMENT)                     
                                                                                
FILT30   CLI   OPTREL,C'Y'         IF RELEASED REQUESTED                        
         BNE   FILT40                                                           
         TM    TACOSTAT,TACOSTRL   COMMERCIAL MUST BE RELEASED                  
         BO    YES                                                              
                                                                                
FILT40   CLI   OPTLOCK,C'Y'        IF LOCKED REQUESTED                          
         BNE   FILT50                                                           
         TM    TACOSTAT,TACOSTLO   COMMERCIAL MUST BE LOCKED                    
         BO    YES                                                              
         B     NO                                                               
                                                                                
FILT50   CLI   OPTREL,C'Y'         IF RELEASED REQUESTED                        
         BE    NO                  REJECT IF DIDN'T PASS EITHER TEST            
                                                                                
         TM    TACOSTAT,TACOSTRL+TACOSTLO  ELSE IF COMML REL OR LOCKED          
         BZ    YES                         REJECT                               
         B     NO                                                               
         DROP  R4                                                               
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 1                                                                
HDHOOK   NTR1                                                                   
         MVI   BYTE,C'H'           SET PRINTING HEADINGS                        
         GOTO1 PRTSCRN,DMCB,EFHTAG,SCOHD1H,H4-1                                 
         GOTO1 (RF),(R1),SCOHD1H,SCOSELH,H7-5                                   
         MVC   H7-5(5),SPACES      CLEAR SELECT FIELD                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
DATINV   MVI   ERROR,INVDATE       INVALID DATE                                 
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
ONLINERR MVI   ERROR,NOTONLIN                                                   
         B     ERRXIT                                                           
*                                                                               
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     ERRXIT                                                           
                                                                                
ERRXIT   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
*              CONSTANTS, ETC.                                                  
*                                                                               
PFCOMML  DS    0C                  PF KEYS TABLE FOR REGULAR COMML/LIST         
*                                                                               
         DC    AL1(PF10X-*,10,0,(PF10X-PF10)/KEYLNQ,0)                          
         DC    CL3'CA',CL8'CAST    ',CL8'LIST'                                  
PF10     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'COID-1),AL2(COID-COMD)                            
PF10X    EQU   *                                                                
*                                                                               
         DC    AL1(PF11X-*,11,0,(PF11X-PF11)/KEYLNQ,0)                          
         DC    CL3'HI',CL8'HISTORY ',CL8'LIST'                                  
PF11     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'COID-1),AL2(COID-COMD)                            
PF11X    EQU   *                                                                
*                                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'  ',CL8'COMPOOL ',CL8'DISPLAY'                               
PF13     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCLI-1),AL2(TGCLI-TGD)                           
         DC    AL1(KEYTYGLB,L'TGPRD-1),AL2(TGPRD-TGD)                           
         DC    AL1(KEYTYTWA,L'SCOCOP-1),AL2(SCOCOP-T702FFD)                     
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF21X-*,21,PFTINT,(PF21X-PF21)/KEYLNQ,PFTUSE)                
         DC    CL3'  ',CL8'        ',CL8'PAY '                                  
PF21     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'COID-1),AL2(COID-COMD)                            
PF21X    EQU   *                                                                
*                                                                               
         DC    AL1(PF22X-*,22,PFTINT+PFTCPROG)                                  
         DC    AL1((PF22X-PF22)/KEYLNQ,0)                                       
         DC    CL3'S ',CL8'COMM    ',CL8'DISP'                                  
PF22     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'COID-1),AL2(COID-COMD)                            
PF22X    EQU   *                                                                
*                                                                               
         DC    AL1(PF23X-*,23,PFTINT+PFTCPROG)                                  
         DC    AL1((PF23X-PF23)/KEYLNQ,0)                                       
         DC    CL3'C ',CL8'COMM    ',CL8'CHA '                                  
PF23     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'COID-1),AL2(COID-COMD)                            
PF23X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFVCOMML DS    0C                  PF KEYS TABLE FOR VERSION COMML/LIST         
*                                                                               
         DC    AL1(PFV10X-*,10,0,(PFV10X-PFV10)/KEYLNQ,0)                       
         DC    CL3'CA',CL8'CAST    ',CL8'LIST'                                  
PFV10    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'COID-1),AL2(COID-COMD)                            
PFV10X   EQU   *                                                                
*                                                                               
         DC    AL1(PFV11X-*,11,0,(PFV11X-PFV11)/KEYLNQ,0)                       
         DC    CL3'HI',CL8'HISTORY ',CL8'LIST'                                  
PFV11    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'COID-1),AL2(COID-COMD)                            
PFV11X   EQU   *                                                                
*                                                                               
         DC    AL1(PFV13X-*,13,0,(PFV13X-PFV13)/KEYLNQ,0)                       
         DC    CL3'  ',CL8'COMPOOL ',CL8'DISPLAY'                               
PFV13    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCLI-1),AL2(TGCLI-TGD)                           
         DC    AL1(KEYTYGLB,L'TGPRD-1),AL2(TGPRD-TGD)                           
         DC    AL1(KEYTYTWA,L'SCOCOP-1),AL2(SCOCOP-T702FFD)                     
PFV13X   EQU   *                                                                
*                                                                               
         DC    AL1(PFV21X-*,21,PFTINT,(PFV21X-PFV21)/KEYLNQ,PFTUSE)             
         DC    CL3'  ',CL8'        ',CL8'PAY '                                  
PFV21    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'COID-1),AL2(COID-COMD)                            
PFV21X   EQU   *                                                                
*                                                                               
         DC    AL1(PFV22X-*,22,PFTINT+PFTCPROG)                                 
         DC    AL1((PFV22X-PFV22)/KEYLNQ,0)                                     
         DC    CL3'S ',CL8'COMM    ',CL8'DISP'                                  
PFV22    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'COID-1),AL2(COID-COMD)                            
PFV22X   EQU   *                                                                
*                                                                               
         DC    AL1(PFV23X-*,23,PFTINT+PFTCPROG)                                 
         DC    AL1((PFV23X-PFV23)/KEYLNQ,0)                                     
         DC    CL3'C ',CL8'COMM    ',CL8'CHA '                                  
PFV23    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'COID-1),AL2(COID-COMD)                            
PFV23X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFCLGCOM DS    0C                  PF KEYS TABLE FOR CLIENT GROUP               
*                                  COMML/LIST                                   
         DC    AL1(PFG10X-*,10,0,(PFG10X-PFG10)/KEYLNQ,0)                       
         DC    CL3'CA',CL8'CAST    ',CL8'LIST'                                  
PFG10    DC    AL1(KEYTYCUR,L'COAGY-1),AL2(COAGY-COMD)                          
         DC    AL1(KEYTYCUR,L'COID-1),AL2(COID-COMD)                            
PFG10X   EQU   *                                                                
*                                                                               
         DC    AL1(PFG11X-*,11,0,(PFG11X-PFG11)/KEYLNQ,0)                       
         DC    CL3'HI',CL8'HISTORY ',CL8'LIST'                                  
PFG11    DC    AL1(KEYTYCUR,L'COAGY-1),AL2(COAGY-COMD)                          
         DC    AL1(KEYTYCUR,L'COID-1),AL2(COID-COMD)                            
PFG11X   EQU   *                                                                
*                                                                               
         DC    AL1(PFG13X-*,13,0,(PFG13X-PFG13)/KEYLNQ,0)                       
         DC    CL3'  ',CL8'COMPOOL ',CL8'DISPLAY'                               
PFG13    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCLI-1),AL2(TGCLI-TGD)                           
         DC    AL1(KEYTYGLB,L'TGPRD-1),AL2(TGPRD-TGD)                           
         DC    AL1(KEYTYTWA,L'SCOCOP-1),AL2(SCOCOP-T702FFD)                     
PFG13X   EQU   *                                                                
*                                                                               
         DC    AL1(PFG21X-*,21,PFTINT,(PFG21X-PFG21)/KEYLNQ,PFTUSE)             
         DC    CL3'  ',CL8'        ',CL8'PAY '                                  
PFG21    DC    AL1(KEYTYGLB,L'COAGY-1),AL2(COAGY-COMD)                          
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'COID-1),AL2(COID-COMD)                            
PFG21X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
OPTTAB   DS    0H                                                               
         DC    CL10'RELEASED  ',AL2(VALREL-T70238)                              
         DC    CL10'LOCKED    ',AL2(VALLOCK-T70238)                             
         DC    CL10'ACTIVE    ',AL2(VALDATE-T70238)                             
         DC    CL10'ALL       ',AL2(VALALL-T70238)                              
         DC    X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
COSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'COMMERCIAL RECORDS'                                      
         SSPEC H2,32,C'------------------'                                      
         SPACE 1                                                                
         SSPEC H8,1,C'-------'                                                  
         SSPEC H8,14,C'---'                                                     
         SSPEC H8,21,C'-----'                                                   
         SSPEC H8,40,C'------------'                                            
         SSPEC H8,56,C'-------'                                                 
         SSPEC H8,65,C'---'                                                     
         SSPEC H8,69,C'---'                                                     
         SSPEC H8,73,C'---'                                                     
         DC    X'00'                                                            
         EJECT                                                                  
*        SET UP SYSIO                                                           
*                                                                               
INIT     NTR1  BASE=*,LABEL=*                                                   
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
         MVC   TIQSTART,SVSTART                                                 
*                                                                               
         SPACE 1                                                                
         CLI   SCOCLGH+5,0         IF CLIENT GROUP INPUT                        
         BE    INIT5                                                            
         MVI   TIREAD,TLCOGCDQ     READ CLIENT GROUP/CID PASSIVE                
         CLI   RDSEQ,C'C'                                                       
         BE    INIT15                                                           
         MVI   TIREAD,TLCOLCDQ     READ CLIENT GROUP/CID NAME PASSIVE           
         B     INIT15                                                           
         SPACE 1                                                                
INIT5    CLI   SCOCOPH+5,0         IF COMMERCIAL GROUP INPUT                    
         BE    INIT6                                                            
         MVC   TIFPRG,TGCOG                                                     
         MVI   TIREAD,TLCOGIDQ     READ COMM'L GROUP/CID PASSIVE                
         CLI   RDSEQ,C'C'                                                       
         BE    INIT15                                                           
         MVI   TIREAD,TLCOGNDQ     READ COMM'L GROUP/CID NAME PASSIVE           
         B     INIT15                                                           
         SPACE 1                                                                
INIT6    CLI   SCOMUSH+5,0         IF MUSIC INPUT                               
         BE    INIT7                                                            
         MVI   TIREAD,TLCOMCDQ     READ MUSIC PASSIVE                           
         CLI   SCOAGYH+5,0                                                      
         BNE   *+8                                                              
         MVI   TIREAD,TLCOOCDQ                                                  
         B     INIT15                                                           
         SPACE 1                                                                
INIT7    MVI   TIREAD,TLCOCDQ      READ ACTIVE COMMERCIAL                       
         CLI   RDSEQ,C'C'          IF CODE                                      
         BE    *+8                                                              
         MVI   TIREAD,TLCONCDQ     ELSE READ COMMERCIAL NAME PASSIVE            
         SPACE 1                                                                
INIT15   CLI   SCOVERS,C'Y'        IF WANT TO SEE VERSIONS                      
         BNE   INIT16                                                           
         MVI   TIREAD,TLCOICDQ     READ COMMERCIAL VERSION PASSIVE              
         CLI   RDSEQ,C'C'          OR IF READING BY ALPHA                       
         BE    INIT16                                                           
         OI    TIQFLAG3,TIQFIVER   SET STATUS                                   
         MVI   TIREAD,TLCONCDQ     AND READ COMMERCIAL NAME PASSIVE             
         SPACE 1                                                                
INIT16   CLI   SCOAGYH+5,0                                                      
         BE    *+10                                                             
         MVC   TIFAGY,TGAGY        AGENCY                                       
         CLI   SCOCLIH+5,0                                                      
         BE    *+10                                                             
         MVC   TIFCLI,TGCLI        CLIENT                                       
         CLI   SCOMUSH+5,0                                                      
         BE    *+10                                                             
         MVC   TIFMUSIC,TGMUS      MUSIC                                        
         CLI   SCOCLGH+5,0                                                      
         BE    INIT18                                                           
         MVC   TIFCLG,TGCLG        CLIENT GROUP                                 
         OI    TIQFLAG2,TIQFEXCT   SET TO FILTER REQUEST EXACTLY                
         OC    TGCLGACC,TGCLGACC   AND IF CLIENT GROUP ACCESS                   
         BZ    INIT18                                                           
         OI    TIQFLAG2,TIQFNLIM   SET NOT TO DO LIMIT ACCESS CHECKS            
*                                                                               
INIT18   XC    TIFPRD,TIFPRD                                                    
         CLI   SCOPRDH+5,0         PRODUCT                                      
         BE    INITX                                                            
         MVC   TIFPRD,SCOPRD       MOVE PRD INTO FILTER (TGPRD GETS             
         OC    TIFPRD,SPACES            CLEARED FOR XNAME)                      
INITX    XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO START THE LIST WHERE IT LEFT OFF                      
         SPACE 1                                                                
SETSTART NTR1  BASE=*,LABEL=*                                                   
         OC    TIQSKEY,TIQSKEY   IF THIS IS THE 1ST TIME LISTING, EXIT          
         BZ    SSTARTX                                                          
*                                                                               
         LA    R4,TIQSKEY                                                       
         CLI   0(R4),TLCOCDQ     IF LISTING BY COMMERCIAL ID                    
         BNE   SSTART10                                                         
         USING TLCOD,R4                                                         
         MVC   TIQSTART,TLCOCID  START WITH LAST COMMERCIAL ID                  
         B     SSTARTX                                                          
         DROP  R4                                                               
*                                                                               
         USING TLCOPCD,R4                                                       
SSTART10 CLI   0(R4),TLCONCDQ    IF LISTING BY COMMERCIAL NAME                  
         BNE   SSTART20                                                         
         MVC   TIQSTART,TLCONAME START WITH LAST COMMERCIAL NAME                
         B     SSTARTX                                                          
*                                                                               
SSTART20 CLI   0(R4),TLCOICDQ    IF LISTING WITH VERSIONS                       
         BNE   SSTART30                                                         
         MVC   TIQSTART,TLCOICID START WITH LAST COMM'L VERSION                 
         B     SSTARTX                                                          
*                                                                               
SSTART30 CLI   0(R4),TLCOGCDQ    CLIENT GROUP AND COMML ID                      
         BNE   SSTART40                                                         
         MVC   TIQSTART,TLCOGCID START WITH LAST COMM'L ID                      
         B     SSTARTX                                                          
*                                                                               
SSTART40 CLI   0(R4),TLCOLCDQ    CLIENT GROUP AND COMML NAME                    
         BNE   SSTART50                                                         
         MVC   TIQSTART,TLCOLNAM START WITH LAST COMM'L NAME                    
         B     SSTARTX                                                          
*                                                                               
SSTART50 CLI   0(R4),TLCOGIDQ    COMML GROUP AND COMML ID                       
         BNE   SSTART60                                                         
         MVC   TIQSTART,TLCOGIID START WITH LAST COMM'L ID                      
         B     SSTARTX                                                          
*                                                                               
SSTART60 CLI   0(R4),TLCOGNDQ    COMML GROUP AND COMML NAME                     
         BNE   SSTART70                                                         
         MVC   TIQSTART,TLCOGNNM START WITH LAST COMM'L ID                      
         B     SSTARTX                                                          
*                                                                               
SSTART70 CLI   0(R4),TLCOGNDQ    MUSIC COMM'L                                   
         BNE   SSTARTX                                                          
         MVC   TIQSTART,TLCOMCID START WITH LAST COMM'L ID                      
SSTARTX  XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO FILTER COMMERCIALS BY TYPE                            
         SPACE 1                                                                
         USING TACOD,R4                                                         
TYPEFILT NTR1  BASE=*,LABEL=*                                                   
******** TM    TACOSTA2,TACOPCYC   ALWAYS FILTER OUT PER CYCLES                 
******** BO    TFNO                                                             
         SPACE 1                                                                
         CLI   SCOTYPEH+5,0        IF COMMERCIAL TYPE FILTER NOT                
         BE    TFYES               DEFINED, SKIP AHEAD                          
         SPACE 1                                                                
         CLI   SCOTYPE,C'*'        IF WANT ALL COMMERCIALS WITHOUT              
         BNE   TF10                A TYPE SETTING                               
         CLI   TACOTYPE,0          ONLY LIST COMMERCIALS WITHOUT                
         BNE   TFNO                TYPE SETTING                                 
         B     TFYES                                                            
         SPACE 1                                                                
TF10     CLI   SCOTYPE,C'-'        IF FILTERING OUT A PARTICULAR                
         BNE   TF20                COMMERCIAL TYPE                              
         CLC   TACOTYPE,SCOTYPE+1  DO NOT LIST COMMERCIALS WITH                 
         BE    TFNO                THAT TYPE                                    
         B     TFYES                                                            
         SPACE 1                                                                
TF20     CLC   TACOTYPE,SCOTYPE    IF ONLY WANT ONE COMMERCIAL TYPE             
         BNE   TFNO                ONLY RETURN THAT COMMERCIAL TYPE             
         SPACE 1                                                                
TFYES    XR    RC,RC                                                            
TFNO     LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO FILTER/PROCESS ALPHA VERSIONS                     *         
***********************************************************************         
                                                                                
PROAVER  NTR1  BASE=*,LABEL=*                                                   
         CLI   SCOVERS,C'Y'        IF INCLUDING VERSIONS                        
         JNE   YES                                                              
         CLI   SCOFMT,C'A'         AND LISTING BY TITLE ...                     
         JNE   YES                                                              
                                                                                
         L     R4,TIAREC                                                        
         CLI   0(R4),TLCOCDQ       ... IF WE HAVE COMMERCIAL RECORD             
         JNE   PAV10               RECORD                                       
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL            SET VERSION 1 IF COMMERCIAL HAS              
         JNE   YES                 VERSIONS                                     
         MVI   TGVER,1                                                          
         MVC   TGCID,TICID                                                      
         J     YES                                                              
                                                                                
         USING TLVRD,R4                                                         
PAV10    MVC   TGCOM,TLVRCOM       ... ELSE IF WE HAVE VERSION                  
         MVC   TGVER,TLVRVER       RECORD                                       
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TICID,TACOCID       SET VERSION ID                               
         DROP  R4                                                               
                                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'24',0)                                   
         JE    *+6                                                              
         DC    H'00'               READ PRIMARY COMMERCIAL RECORD               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO                                                           
         MVC   TIAGY,TLCOAGY       SET AGENCY                                   
         MVC   TICLI,TLCOCLI       CLIENT                                       
         MVC   TIPRD,TLCOPRD       AND PRODUCT CODE                             
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TGCID,TACOCID                                                    
         MVC   SVTACOEL,0(R4)      SET PRIMARY COMMERCIAL DETAILS               
         DROP  R4                                                               
                                                                                
         MVC   KEY,TIKEY           RESTORE SYSIO READ SEQUENCE                  
         GOTO1 HIGH                                                             
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO FILTER/PROCESS CODE VERSIONS                      *         
***********************************************************************         
                                                                                
PROCVER  NTR1  BASE=*,LABEL=*                                                   
         CLI   SCOVERS,C'Y'        IF INCLUDING VERSIONS                        
         JNE   YES                                                              
         CLI   SCOFMT,C'A'         AND LISTING BY CODE ...                      
         JE    YES                                                              
                                                                                
         L     R4,TIAREC                                                        
                                                                                
         USING TLCOPD,R3                                                        
         LA    R3,TIKEY                                                         
         CLI   TLCOIVER,1          SKIP VERSION 1 KEYS                          
         JE    NO                                                               
         MVC   TGCOM,TLCOICOM      SET INTERNAL COMMERCIAL NUMBER               
         MVC   TGVER,TLCOIVER      VERSION NUMBER                               
         MVC   TGCID,TLCOICID      AND COMMERCIAL ID                            
         DROP  R3                                                               
                                                                                
         CLI   TGVER,0             IF VERSION IS 0 COMMERCIAL MAY               
         JNE   PCV10               STILL HAVE VERSION 1                         
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   YES                                                              
         MVI   TGVER,1             IF SO, SET VERSION NUMBER                    
         J     YES                                                              
                                                                                
PCV10    CLI   TGVER,26            IF VERSION NUMBER IS GREATER THAN            
         JH    PCV20               26, READ PRIMARY COMMERCIAL RECORD           
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'24',0)                                   
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO                                                           
         MVC   TIAGY,TLCOAGY       SET AGENCY                                   
         MVC   TICLI,TLCOCLI       CLIENT                                       
         MVC   TIPRD,TLCOPRD       AND PRODUCT CODE                             
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TGCID,TACOCID                                                    
         MVC   SVTACOEL,0(R4)      SAVE PRIMARY COMMERCIAL DETAILS              
         DROP  R4                                                               
                                                                                
PCV20    GOTO1 RECVAL,DMCB,TLVRCDQ,(X'24',0)                                    
         JE    *+6                                                              
         DC    H'00'               READ VERSION RECORD                          
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TICID,TACOCID       SET VERSION ID AND LENGTH                    
         EDIT  (B1,TACOSEC),(3,TISEC),ALIGN=LEFT                                
         DROP  R4                                                               
                                                                                
         MVC   TINAME,SPACES                                                    
                                                                                
         USING TANAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TANAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   TINAME(0),TANANAME  SET VERSION NAME                             
         DROP  R4                                                               
                                                                                
         MVC   KEY,TIKEY           RESTORE SYSIO READ SEQUENCE                  
         GOTO1 HIGH                                                             
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
*                                                                               
COMD     DSECT                                                                  
COID     DS    CL12                                                             
         DS    CL1                                                              
COPRD    DS    CL6                 PRODUCT CODE OR AGENCY CODE                  
         ORG   COPRD                                                            
COAGY    DS    CL6                                                              
         DS    CL1                                                              
COTITLE  DS    CL18                                                             
         DS    CL1                                                              
COPRNAM  DS    CL15                                                             
         DS    CL1                                                              
COFFC    DS    CL8                                                              
         DS    CL2                                                              
COMEDIA  DS    CL1                                                              
         DS    CL2                                                              
COLEN    DS    CL3                                                              
         DS    CL1                                                              
COVER    DS    CL3                                                              
         SPACE 3                                                                
*              DSECT TO COVER OPTIONS TABLE                                     
*                                                                               
OPTD     DSECT                                                                  
OPTLHS   DS    CL10                                                             
OPTDISP  DS    AL2                                                              
OPTNEXT  EQU   *                                                                
         EJECT                                                                  
*              DSECT TO COVER TWAHOLE STORAGE                                   
TWAHOLED DSECT                                                                  
         DS    XL100                                                            
SVTACOEL DS    XL(TACOLNQ2+50)                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR38D                                                       
         EJECT                                                                  
*                                                                               
COUNTER  DS    PL4                 LINE COUNTER                                 
RDSEQ    DS    CL1                 READ WITH ACTIVE/PASSIVE POINTER             
*                                                                               
OPTS     DS    0CL11               OPTIONS                                      
OPTREL   DS    CL1                 LIST RELEASED COMMERCIALS                    
OPTLOCK  DS    CL1                 LIST LOCK COMMERCIALS                        
OPTALL   DS    CL1                 LIST RELEASED, LOCKED AND ACTIVE             
         DS    CL5                 N/D                                          
OPTDATE  DS    XL3                 LAST ACTIVE DATE                             
*                                                                               
SVCOID   DS    CL12                                                             
*                                                                               
SVDSKADD DS    XL4                 SAVED COMMERCIAL DISK ADDRESS                
PRNAMEH  DS    CL8                 PRODUCT NAME HEADER                          
PRNAME   DS    CL16                PRODUCT NAME                                 
SVSTART  DS    CL24                STARTING FIELD                               
PERBLK   DS    CL56                PERVAL BLOCK                                 
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023TAGEN38   07/30/12'                                      
         END                                                                    
