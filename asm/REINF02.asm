*          DATA SET REINF02    AT LEVEL 042 AS OF 10/07/04                      
*PHASE T80B02A,*                                                                
         TITLE 'T80B02 - REINF02 - INFO READER AND LISTER II'                   
*                                                                               
*********************************************************************           
*                                                                   *           
*        REINF02 --- REP INFO READER/LISTER PART 2                  *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JUN08/89 (MRR) --- HISTORY LOST                                   *           
*                    ADD 'ACE', 'FAX', 'COMBO', 'INTER' OPTIONS     *           
*                     FOR STATION                                   *           
*                                                                   *           
* 07/20/89  PJS  1) PROGRAM TOO LARGE....R9 MADE INTO 2ND BASE      *           
*                   REGISTER FOR FUTURE MODIFICATIONS.              *           
*                                                                   *           
*                2) ADDED NEW RECORDS:                              *           
*                   'DMENU' - DEMO MENU                             *           
*                   'COMMENT' OR 'CMT' - STANDARD COMMENT* *                    
*                                                                   *           
* 07/31/89  PJS  1) FOR 'DMENU' RECS, DISPLAY IN 2 LINE FORMAT      *           
*                   WITH CODE & DESC ON LINE 1 AND DEMOS CODES      *           
*                   ON LINE 2.  IF MORE THAN 1 LINE OF DEMOS EXIST  *           
*                   FLAG LINE WITH '**' (MORE INDICATORE)           *           
*                                                                   *           
* AUG23/89 (MRR) REASSEMBLE WITH CHANGE IN RGENEROL TO PASS REPCODE *           
*                 TO DATAMGR                                        *           
*                                                                   *           
* 08/25/89  PJS  PRD/AOF - PASS REP CODE ON HIGH/SEQ (NEW DATAMGR)  *           
*                                                                   *           
* 09/05/89  PJS  ADDED REP RECORD LISTING                           *           
*                                                                   *           
* 01/24/90  PJS  ADDED POINT PERSON AND CONTRACT TYPE LISTING       *           
*                                                                   *           
* 07/25/90  BU   STATION RECORD:  EI OPTION.  ACTIVE+INACTIVE FILTER*           
*                WITH DEFAULT = ACTIVE.                             *           
*                AGENCY RECORD:   EI OPTION.                        *           
*                PRODUCT RECORD:  'NETWORK' OPTION=ALTERNATE DISPLAY*           
*                                 POINT PERSON FILTER               *           
*                                 NETWORK CONTRACT FILTER           *           
*                                                                   *           
* 06MAR91  (EFJ) --- UPDATE TO DEAL WITH NEW COMMENT KEY            *           
*                                                                   *           
* 23JAN92  (SKU) PRODUCT RECORD: 'SPOT' OPTION, ALTERNATE DISPLAY   *           
*                                                                   *           
* 04MAR92  (SKU) FIX COMMENT BUG, SHOULD LIST OWN COMMENTS          *           
*                                                                   *           
* 15APR92  (SKU) AGY RECORD: RISK OPTION, LIAB OPTION               *           
*                                                                   *           
* 01MAY92  (SKU) FIX BUY TO DISPLAY SPOTPAK INFO IN PROD REC        *           
*                                                                   *           
* 28OCT92  (BU ) STATION/COMBO SCREEN:  DISPLAY UP TO 4 COMBO STNS. *           
*                FILTER ON COMBO STATIONS (-CM).  DROP CHANNEL AND  *           
*                AFFILIATION FROM SCREEN TO MAKE ROOM.              *           
*                                                                   *           
* 11NOV92  (SKU) FAX DISPLAY OPTION FOR OFF,SAL AND AGY RECORDS     *           
*                                                                   *           
* 19MAR93  (SKU) FOR COMBO, DON'T SHOW STATION WITH '-' STATUS      *           
*                                                                   *           
* 22NOV93  (BU ) FOR INTER, 1) DROP CHANNEL/AFFL COLS 2) ADD COMBO  *           
*                PARTICIPANT COLUMN  3) DISPLAY ALL PARTICIPANTS    *           
*                FOR COMBO+, 1)DROP CHANNEL/AFFL COLS 2) BYPASS     *           
*                PARENT STATIONS  3) DISPLAY PARENT  OF COMBO CHILD *           
*                                                                   *           
* 08FEB94  (BU ) ADD DEVELOPMENTAL S/P + CONTRACT TYPE DISPLAYS     *           
*                                                                   *           
* 22FEB95  (BU ) ADD LEAVE DATE TO DEV S/P + POINT PERSON DISPLAYS  *           
*                                                                   *           
* 22FEB95  (BU ) SHOW DATES JOINED/LEFT IF PROF # 1 ON              *           
*                                                                   *           
* 23FEB95  (BU ) STATION OPTIONS: DATE/COMP                         *           
*                                                                   *           
* 23FEB95  (BU ) TEAM/OFFICE FILTERS FOR STATION DISPLAY            *           
*                                                                   *           
* 30MAR95  (BU ) FIX DEMO LIST PROBLEM                              *           
*                                                                   *           
* 13NOV95  (RHV) FILTER REP FILES ON MASTER/PARENT REP              *           
*                                                                   *           
* 13DEC95  (RHV) FIX CONTYP REC CODE DISPLAY LENGTH BUG             *           
*                                                                   *           
* 20FEB96  (RHV) FIX START AT BUG FOR PRD & OWN RECORDS             *           
*                                                                   *           
* 10APR96  (RHV) SUPPORT DISPLAY OF 34 BYTE AGY ADDRESS FIELDS      *           
*                                                                   *           
* 01MAY96  (WSB) ADD STA 'REP' OPTION: DISPLAY FORMER/NEW REPS      *           
*                                                                   *           
* 09AUG96  (SEP) ADD 'TER' DISPLAY TERRITORY CODES AND NAMES        *           
*                                                                   *           
* 03OCT96  (SEP) ALLOW LOW POWER TV STAION ENTRY                    *           
*                                                                   *           
* 11OCT96  (RHV) AGY/AOF RECORD HANDLING MOVED TO NEW REINF03 MODULE*           
*                                                                   *           
* 11FEB97  (DBU) ADD AGENCY/FLIGHT DATES OPTION                     *           
*                                                                   *           
* JAN09/98 (JRD) ADD 'PARENT' 'PARENT+' 'MARKET' & 'OWNER' TO       *           
*                 STATION.                                          *           
*                                                                   *           
* JUN23/98 (SKU) FORMAT FAX NUMBERS                                 *           
*                                                                   *           
* AUG12/98 (AST) SCOPE FILTER FOR NATIONAL/LOCAL OFFICES            *           
*                                                                   *           
* MAR18/99 (SKU) CALL DEMOCON VIA CORE                              *           
*                                                                   *           
* OCT07/04 (BU ) PRODUCT LIST / AGY OPT:  'NOT ON FILE' IF NO AGY   *           
*                                                                   *           
*                                                                   *           
*                ***  END TOMBSTONE  ***                            *           
*********************************************************************           
*                                                                               
T80B02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**8B02**,R9,R7,RR=R5                                           
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T80BFFD,RA                                                       
*                                                                               
         ST    R5,RELO1            SAVE RELO FACTOR                             
         MVC   KEY,SAVEKEY                                                      
         LA    R2,INFOUTH                                                       
         XC    INFOUT,INFOUT                                                    
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,CLRKTAB          CLEAR KEYTABLE                               
*                                                                               
         SR    RF,RF                                                            
         L     R6,4(R1)            GET LINK                                     
         IC    RF,0(R6)                                                         
         B     BRANCH(RF)                                                       
         SPACE 2                                                                
*                                                                               
BRANCH   DC    XL24'00'                                                         
*        THESE RECORDS ARE REFERENCED IN T80B01                                 
***      B     ADV10                                                            
***      B     AGY10                                                            
***      B     DIV10                                                            
***      B     TEM10                                                            
***      B     SAL10                                                            
***      B     REG10                                                            
         B     STAT0020                                                         
         B     OFF10                                                            
         B     PROD0020                                                         
         DC    2H'0'               AOF NOW HANDLED IN 03 REINF03                
***      B     AOF10                                                            
         B     CAT10                                                            
         B     CLS10                                                            
         B     GRP10                                                            
         B     OWN10                                                            
*                                                                               
         B     DMN10               DMENU RECORDS                                
*                                                                               
         B     CMT10               STANDARD COMMENT RECORDS                     
*                                                                               
         B     REP10               REP RECORD                                   
*                                                                               
         DC    2H'0'               PLACE-SAVER FOR SPECIAL FIX LINK.            
*                                                                               
         B     POIN0020            POINT PERSON                                 
*                                                                               
         B     CTY10               CONTRACT TYPE                                
*                                                                               
         B     DSP10               DEVELOPMENTAL S/P                            
*                                                                               
         B     DCT10               DEVELOPMENTAL CONTRACT TYPE                  
*                                                                               
         B     TER10               TERRITORY                                    
         EJECT                                                                  
STAT0020 EQU   *                                                                
         LA    R8,KEYTAB           KEY TABLE POINTER                            
         LA    RE,INFOUTH                                                       
STAT0022 DS    0H                  CLEAR THE SCREEN                             
         OI    6(RE),X'80'                                                      
         MVC   8(79,RE),SPACES                                                  
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BNE   STAT0022                                                         
*                                                                               
         CLI   OPTNBYTE,17         COMBO PARENT OPTION?                         
         BE    STAT2000            YES - NEED TO USE PASSIVE KEYS               
         CLI   OPTNBYTE,18         MULTIPLE COMBO PARENT OPTION?                
         BE    STAT2000            YES - NEED TO USE PASSIVE 8301 KEYS          
*                                                                               
         CLI   OPTNBYTE,19         MARKET LIST OPTION?                          
         BE    STAT2100            YES - NEED TO USE PASSIVE 8302 KEYS          
*                                                                               
         CLI   OPTNBYTE,20         OWNER LIST OPTION?                           
         BE    STAT2200            YES - NEED TO USE PASSIVE 8303 KEYS          
*                                                                               
         CLI   OPTNBYTE,5          COMBO OPTION?                                
         BNE   STAT0040            NO                                           
         MVI   BYTE2,C'C'          YES - SET TO COMBOS ONLY                     
         B     STAT0060                                                         
STAT0040 EQU   *                                                                
         MVI   BYTE2,0                                                          
         CLC   MEDFLTR,ALL         'M=' ON FILTERS LINE...                      
         BE    STAT0060                                                         
         MVI   BYTE2,C' '                                                       
         CLC   MEDFLTR,=C'TV'                                                   
         BE    STAT0060                                                         
*                                                                               
         MVI   BYTE2,C'A'                                                       
         CLC   MEDFLTR,=C'AM'                                                   
         BE    STAT0060                                                         
*                                                                               
         MVI   BYTE2,C'C'          COMBINED STATION                             
         CLC   MEDFLTR,=C'CM'                                                   
         BE    STAT0060                                                         
*                                                                               
         MVI   BYTE2,C'F'                                                       
*                                                                               
STAT0060 CLI   NEXTBYTE,1                                                       
         BE    STAT0080                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),INFSTRT                                                
         OC    KEY+22(5),SPACES                                                 
*                                                                               
STAT0080 BAS   RE,HIGH             FIRST READ                                   
         CLI   KEY,2               SAME REC TYPE                                
         BNE   NOREC                                                            
         CLC   REPALPHA,KEY+20     SAME REP CODE                                
         BNE   NOREC                                                            
         B     STAT0120                                                         
         SPACE 1                                                                
STAT0100 BAS   RE,SEQ                                                           
         CLI   KEY,2                                                            
         BNE   SETDONE                                                          
         CLC   REPALPHA,KEY+20                                                  
         BNE   SETDONE                                                          
*                                                                               
STAT0120 CLC   BYTE2,ALL           ANY MEDIA FILTER?                            
         BE    STAT0140            NO                                           
         CLC   BYTE2,KEY+26        YES - STATION AGREES?                        
         BNE   STAT0100                                                         
*                                                                               
STAT0140 MVC   SAVEKEY,KEY                                                      
         BAS   RE,GETREC                                                        
         OC    AFFLTR,SPACES                                                    
         OC    GRPFLTR(2),SPACES                                                
         OC    TVBFLTR,SPACES                                                   
         OC    OWNFLTR,SPACES                                                   
         OC    ACFLTR,SPACES                                                    
         OC    OFTMFLTR,SPACES                                                  
*                                                                               
         CLC   AFFLTR,SPACES                                                    
         BE    STAT0160                                                         
         CLC   AFFLTR,RSTAAFFL                                                  
         BNE   STAT0100                                                         
*                                                                               
STAT0160 CLC   GRPFLTR(2),SPACES                                                
         BE    STAT0180                                                         
         CLC   GRPFLTR,RSTAGRUP                                                 
         BNE   STAT0100                                                         
         CLC   SGRFLTR,SPACES                                                   
         BE    STAT0180                                                         
         CLC   SGRFLTR,RSTAGRUP+1                                               
         BNE   STAT0100                                                         
*                                                                               
STAT0180 CLC   TVBFLTR,SPACES                                                   
         BE    STAT0200                                                         
         CLC   TVBFLTR,RSTATVB                                                  
         BNE   STAT0100                                                         
*                                                                               
STAT0200 CLC   OWNFLTR,SPACES                                                   
         BE    STAT0220                                                         
         CLC   OWNFLTR,RSTAOWN                                                  
         BNE   STAT0100                                                         
*                                                                               
STAT0220 CLC   RNKFLTR,ALL                                                      
         BE    STAT0240                                                         
         CLC   RNKFLTR,RSTARANK                                                 
         BNE   STAT0100                                                         
*                                                                               
STAT0240 CLC   TRAFLTR,ALL                                                      
         BE    STAT0260                                                         
         CLC   TRAFLTR,RSTATRAF                                                 
         BNE   STAT0100                                                         
*                                                                               
STAT0260 CLC   ACFLTR,SPACES       ACCEPT ACT+INACT STAS?                       
         BNE   STAT0270            YES                                          
         CLC   RSTAEND,ALL         NO END DATE = ACTIVE                         
         BNE   STAT0100            TAKE ACTIVES ONLY                            
*                                                                               
STAT0270 EQU   *                                                                
         CLC   OFTMFLTR,SPACES     ANY TEAM/OFFTEAM FILTER?                     
         BE    STAT0280            NO                                           
         BAS   RE,OFTMFILT         YES - CHECK FILTER                           
         BNZ   STAT0100            NOT ACCEPTED - SKIP IT                       
*                                                                               
STAT0280 CLI   OPT2BYTE,7          EI OPTION REQUESTED?                         
         BNE   STAT0300            NO                                           
         LA    R6,RSTAREC          YES - CHECK X'08' ELEMENT                    
         MVI   ELCODE,8                                                         
         BAS   RE,GETEL                                                         
         BNE   STAT0100                                                         
         CLI   5(R6),C'Y'          EASI STATION?                                
         BNE   STAT0100            NO - SKIP RECORD                             
*                                                                               
STAT0300 SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R5,0(R2,R5)                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         CLI   0(R5),0                                                          
         BE    SETNEXT                                                          
         USING LINE4,R2                                                         
*                                                                               
STAT0320 MVC   L4STA,RSTAKSTA                                                   
         MVC   0(L'L4STA,R8),L4STA  PUT IN KEY TABLE WAIT TO BUMP R8            
*                                    POINTR TIL SURE OF LINE ADVANCE            
         MVC   L4MED,=C'TV'                                                     
         MVC   L'L4STA(2,R8),=C'-T' PUT IN KEY TAB ENTRY                        
         CLI   RSTAKSTA+4,C' '                                                  
         BE    STAT0340                                                         
*                                                                               
         MVC   L4MED,=C'L '        MOVE IN L FOR LOW POWER STATIONS             
         MVC   L'L4STA(2,R8),=C'-L' PUT IN KEY TAB ENTRY                        
         CLI   RSTAKSTA+4,C'L'     IS IT A LOW POWER STAION                     
         BE    STAT0340            THEN TAKE BRANCH                             
*                                                                               
         MVC   L4MED,=C'FM'                                                     
         CLI   RSTAKSTA+4,C'F'                                                  
         MVC   L'L4STA(2,R8),=C'-F' PUT IN KEY TAB ENTRY                        
         BE    STAT0340                                                         
*                                                                               
         MVC   L4MED,=C'CM'        COMBINED STATION                             
         MVC   L'L4STA(2,R8),=C'-C' PUT IN KEY TAB ENTRY                        
         CLI   RSTAKSTA+4,C'C'                                                  
         BE    STAT0340                                                         
*                                                                               
         MVC   L'L4STA(2,R8),=C'-A' PUT IN KEY TAB ENTRY                        
         MVC   L4MED,=C'AM'                                                     
*                                                                               
STAT0340 EQU   *                                                                
         MVC   L4MKT,RSTAMKT                                                    
         MVI   L4STA+4,C'-'                                                     
         CLI   OPTNBYTE,5          COMBO OPTION?                                
         BE    STAT0400            YES - DON'T DISPLAY CHANNEL                  
*                                     USE ALT LINE DEFINITION                   
         CLI   OPTNBYTE,6          INTER OPTION?                                
         BE    STAT0380            YES - DON'T DISPLAY CHANNEL                  
*                                     USE ALT LINE DEFINITION                   
         CLI   OPTNBYTE,7          COMBO+ OPTION?                               
         BE    STAT0400            YES - DON'T DISPLAY CHANNEL                  
*                                     USE ALT LINE DEFINITION                   
         CLI   OPTNBYTE,26         UID OPTION?                                  
         BNE   STAT0350            NO  -                                        
*                                                                               
         MVI   ELCODE,X'2A'        FIND UNIQUE ID ELEMENT                       
         LA    R6,RSTAREC                                                       
         BAS   RE,GETEL                                                         
         BNE   STAT0360                NO ELEMENT                               
         USING RSTAUIEL,R6                                                      
         MVC   L4CNL(6),RSTAUIST   INSERT UNIQUE ID                             
         DROP  R6                                                               
         B     STAT0360                                                         
STAT0350 EQU   *                                                                
         EDIT  RSTACHAN,(4,L4CNL),ALIGN=LEFT                                    
STAT0355 EQU   *                                                                
         CLI   L4MED,C'F'                                                       
         BNE   STAT0360                                                         
         LA    R1,L4CNL+3                                                       
         CLI   0(R1),C' '                                                       
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVC   1(1,R1),0(R1)                                                    
         MVI   0(R1),C'.'                                                       
STAT0360 EQU   *                                                                
         CLI   OPTNBYTE,26         UID OPTION?                                  
         BNE   STAT0370            NO  -                                        
         MVC   L4AFL+1,RSTAAFFL    YES - OFFSET AFFIL ONE POS                   
         B     STAT0375                                                         
*                                                                               
STAT0370 EQU   *                                                                
         MVC   L4AFL,RSTAAFFL                                                   
STAT0375 EQU   *                                                                
         MVC   L4GRP,RSTAGRUP                                                   
         MVI   L4GRP+1,C'/'                                                     
         MVC   L4SG,RSTAGRUP+1                                                  
*                                                                               
         MVI   L4ACT,C'Y'                                                       
         CLI   RSTAEND,0                                                        
         BE    STAT0420                                                         
         MVI   L4ACT,C'N'                                                       
         B     STAT0420                                                         
STAT0380 EQU   *                   FORMAT FOR INTER OPTION DISPLAY              
         MVC   L4MKT+16(4),SPACES  CLEAR LAST 4 CHARS -                         
*                                     THIS FORMAT USES SHORTER FIELD            
         MVC   L4BGRP-LINE4B(1,R2),RSTAGRUP                                     
         MVI   L4BGRP+1-LINE4B(R2),C'/'                                         
         MVC   L4BSG-LINE4B(1,R2),RSTAGRUP+1                                    
*                                                                               
         MVI   L4BACT-LINE4B(R2),C'Y'                                           
         CLI   RSTAEND,0                                                        
         BE    STAT0420                                                         
         MVI   L4BACT-LINE4B(R2),C'N'                                           
         B     STAT0420                                                         
*                                                                               
STAT0400 EQU   *                   FORMAT FOR COMBO OPTION DISPLAY              
         MVC   L4AGRP-LINE4A(1,R2),RSTAGRUP                                     
         MVI   L4AGRP+1-LINE4A(R2),C'/'                                         
         MVC   L4ASG-LINE4A(1,R2),RSTAGRUP+1                                    
*                                                                               
         MVI   L4AACT-LINE4A(R2),C'Y'                                           
         CLI   RSTAEND,0                                                        
         BE    STAT0420                                                         
         MVI   L4AACT-LINE4A(R2),C'N'                                           
*                                                                               
STAT0420 EQU   *                                                                
         CLI   OPTNBYTE,2         CODE OPTION                                   
         BE    STAT0620                                                         
         CLI   OPTNBYTE,3         ACE OPTION                                    
         BE    STAT0640                                                         
         CLI   OPTNBYTE,4         FAX OPTION                                    
         BE    STAT0740                                                         
         CLI   OPTNBYTE,5         COMBO OPTION                                  
         BE    STAT0800                                                         
         CLI   OPTNBYTE,6         INTER OPTION?                                 
         BE    STAT0920           YES                                           
         CLI   OPTNBYTE,7         COMBO+ OPTION?                                
         BE    STAT0920           YES                                           
         CLI   OPTNBYTE,13        DATE   OPTION?                                
         BE    STAT0440           YES                                           
         CLI   OPTNBYTE,14        COMPETITIVE STATIONS OPTION?                  
         BE    STAT0500           YES                                           
         CLI   OPTNBYTE,15        FORMER REP/NEW REP OPTION?                    
         BE    STAT1000           YES                                           
*                                                                               
         LA    RF,SVPGPBIT         CHECK PROFILE FOR DISPLAY CHOICE             
         TM    0(RF),X'80'         1ST PROF BIT ON?                             
         BNO   STAT0500            NO  -                                        
STAT0440 EQU   *                                                                
         OC    RSTASTRT,RSTASTRT   ANY START DATE?                              
         BZ    STAT0460            NO                                           
         GOTO1 VDATCON,DMCB,(3,RSTASTRT),(5,L4CPS+3)                            
*                                  YES - SHOW JOIN DATE                         
STAT0460 EQU   *                                                                
         OC    RSTAEND,RSTAEND     ANY END DATE?                                
         BZ    STAT0480            NO                                           
         GOTO1 VDATCON,DMCB,(3,RSTAEND),(5,L4CPS+13)                            
*                                  YES - SHOW LEAVE DATE                        
STAT0480 EQU   *                                                                
         CLI   OPT2BYTE,8          DOUBLE LINE EMAIL ADDRESS                    
         BNE   STAT0490                                                         
         FOUT  (R2)                                                             
         MVI   ELCODE,X'25'                                                     
         LA    R6,RSTAREC                                                       
         BAS   RE,GETEL                                                         
         BNE   STAT0490                NO ELEMENT                               
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
*                                                                               
         USING RSTAEML,R6                                                       
         LA    R4,L4MKT                                                         
STAT0485 EQU   *                                                                
         ZIC   R5,RSTAEMLN                                                      
         CHI   R5,3                                                             
         BNH   C                       EMPTY ELEMENT                            
         SHI   R5,RSTAADD-RSTAEMC                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),RSTAADD                                                  
B        BAS   RE,NEXTEL                                                        
         BNE   STAT0487                                                         
         AR    R4,R5                                                            
         MVI   1(R4),C','                                                       
         AHI   R4,3                                                             
         B     STAT0485                                                         
C        SHI   R4,2                                                             
         MVI   0(R4),C' '                                                       
         B     B                                                                
*                                                                               
STAT0487 EQU   *                                                                
         FOUT  (R2)                ADVANCE LINE POINTER                         
STAT0490 SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         LA    R8,L'KEYTAB(R8)     ADVANCE KEYTAB POINTER (SAVE KEY)            
         B     STAT0100            GO BACK FOR NEXT STATION                     
STAT0500 EQU   *                                                                
         LA    R1,L4CPS                                                         
         LA    R3,3                                                             
         LA    R6,RSTACODE                                                      
STAT0520 ZIC   R4,1(R6)                                                         
         AR    R6,R4                                                            
         USING RSTAMKEL,R6                                                      
         CLI   RSTAMKCO,2                                                       
         BE    STAT0540                                                         
         BAS   RE,STAT0600                                                      
         B     STAT0100                                                         
STAT0540 MVC   0(4,R1),RSTAMKST                                                 
         CLI   RSTAMKST+4,C' '                                                  
         BNE   STAT0560            RADIO                                        
         MVI   4(R1),C'('                                                       
         MVC   5(3,R1),RSTAMKAF                                                 
         MVI   8(R1),C')'                                                       
         B     STAT0580                                                         
         SPACE 1                                                                
STAT0560 MVI   4(R1),C'-'                                                       
         MVC   5(1,R1),RSTAMKST+4                                               
         SPACE 1                                                                
STAT0580 EQU   *                                                                
         LA    R1,10(R1)                                                        
         BCT   R3,STAT0520                                                      
         BAS   RE,STAT0600                                                      
         ZIC   R4,1(R6)                                                         
         AR    R6,R4                                                            
         CLI   RSTAMKCO,2                                                       
         BNE   STAT0100                                                         
         LA    R1,L4CPS                                                         
         LA    R3,3                                                             
         B     STAT0540                                                         
         DROP  R6                                                               
*                                                                               
*                                                                               
STAT0600 ST    RE,FULL                  SAVE LINK REGISTER                      
         MVC   L'KEYTAB(L'KEYTAB,R8),0(R8) COPY CURRENT ENTRY                   
*                                          TO NEXT SLOT                         
         LA    R8,L'KEYTAB(R8)          ADV KEYTAB POINTER (SAVE KEY)           
*                                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         LA    R4,INFLAST                                                       
         CR    R2,R4               MAY BE AT BOTTOM OF SCREEN                   
         BNL   STAT0100                                                         
         BAS   RE,CLRLINE                                                       
         L     RE,FULL             BACK TO CALLER                               
         BR    RE                                                               
         SPACE 2                                                                
STAT0620 MVC   L4OWN,RSTAOWN                                                    
         MVC   L4TVB,RSTATVB                                                    
         MVC   L4RNK,RSTARANK                                                   
         MVC   L4TRAF,RSTATRAF                                                  
*                                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     STAT0100                                                         
*                                                                               
         SPACE 2                                                                
STAT0640 EQU   *                       ACE OUTPUT                               
*                                                                               
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,5                                                         
         BAS   RE,GETEL                                                         
         BNE   STAT0660                NO ELEMENT                               
         MVC   L4CPS(8),2(R6)          RECEIVING SIGN ON CODE                   
STAT0660 EQU   *                                                                
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,6                                                         
         BAS   RE,GETEL                                                         
         BNE   STAT0680                NO ELEMENT                               
         MVC   L4CPS+9(8),2(R6)        SIGN-ON CODE                             
STAT0680 EQU   *                                                                
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,8                                                         
         BAS   RE,GETEL                                                         
         BNE   STAT0700                NO ELEMENT                               
         MVC   L4CPS+18(8),11(R6)      DESTINATION ID                           
STAT0700 EQU   *                                                                
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
*                                                                               
STAT0720 EQU   *                                                                
         B     STAT0100                                                         
*                                                                               
         SPACE 2                                                                
STAT0740 EQU   *                       FAX OUTPUT                               
*                                                                               
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,8                                                         
         BAS   RE,GETEL                                                         
         BNE   STAT0760                 NO ELEMENT                              
         USING RSTAXXEL,R6                                                      
         MVC   L4CPS(13),RSTAOFAX       FAX NUMBER                              
*                                                                               
         CLI   RSTAOFAX,X'00'      FIRST BYTE EMPTY?                            
         BNE   STAT0760            NO  - JUST PUT OUT WHAT'S THERE              
         CLI   RSTAOFAX+1,X'00'    ANYTHING IN SECOND BYTE?                     
         BE    STAT0760            NO  - JUST PUT OUT WHAT'S THERE              
         ZIC   R3,RSTAOFAX+1       INTERNATIONAL CODE                           
         EDIT  (R3),(3,L4CPS),FILL=0                                            
         UNPK  WORK(16),RSTAOFAX+3(8)                                           
         ZIC   RF,RSTAOFAX+2       LENGTH OF SIGNIFICANT DIGITS                 
         LA    RE,16               MAXIMUM LENGTH OF FIELD                      
         SR    RE,RF               GET SIGNIFICANT OFFSET                       
         LA    RF,WORK             A(UNPACKED NUMBER)                           
         AR    RF,RE               ADD OFFSET                                   
         ZIC   RE,RSTAOFAX+2       GET LENGTH OF FAX# FIELD AGAIN               
         BCTR  RE,0                DECREMENT FOR EX                             
         EX    RE,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   L4CPS+3(0),0(RF)                                                 
*                                                                               
         DROP  R6                                                               
*                                                                               
STAT0760 EQU   *                                                                
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,7                                                         
         BAS   RE,GETEL                                                         
         BNE   STAT0780                NO ELEMENT                               
         MVC   L4CPS+14(15),2(R6)      TWX NUMBER                               
STAT0780 EQU   *                                                                
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
*                                                                               
         B     STAT0100                                                         
         SPACE 2                                                                
STAT0800 EQU   *                          COMBO OUTPUT                          
         LR    R5,R2                      SET A(DISPLAY POSITION)               
         LA    R5,L4ACPS-LINE4A(R5)       DISPLACE TO COMBO DISPLAY             
STAT0820 EQU   *                                                                
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'0A'        COMBO ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   STAT0900            NO ELEMENT                                   
*                                                                               
STAT0840 EQU   *                                                                
         CLI   7(R6),C'-'          STATION NO LONGER PARTICIPATING?             
         BE    STAT0880            SKIP IF TRUE                                 
*                                                                               
         MVC   0(5,R5),2(R6)       COMBO STATION LETTERS                        
         MVC   5(1,R5),4(R5)       MOVE MEDIA DOWN 1 POSITION                   
         MVI   4(R5),C'-'          INSERT DASH                                  
         CLI   1(R6),7             ELEMENT LENGTH = 7 (OLD FORMAT)?             
         BE    STAT0860            YES -                                        
         CLI   7(R6),C'*'          PREFERRED STATION?                           
         BNE   STAT0860            NO                                           
         MVI   6(R5),C'*'          YES - SET INDICATOR                          
*                                                                               
STAT0860 EQU   *                                                                
         LA    R5,9(R5)            BUMP A(SCREEN DISPLAY)                       
*                                                                               
STAT0880 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT, IF ANY                     
         BE    STAT0840            FOUND - DISPLAY IT                           
*                                                                               
STAT0900 EQU   *                                                                
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     STAT0100                                                         
         SPACE 2                                                                
STAT0920 EQU   *                   INTER OR COMBO+ OPTION                       
         CLI   OPTNBYTE,7          COMBO+ OPTION?                               
         BNE   STAT0940            NO                                           
         CLI   RSTAKSTA+4,C'C'     YES - 'PARENT' STATION?                      
         BE    STAT0100            YES - SKIP IT                                
         B     STAT0800            GO BACK AND DISPLAY STATIONS                 
STAT0940 EQU   *                                                                
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,8                                                         
         BAS   RE,GETEL                                                         
         BNE   STAT0960                NO ELEMENT                               
         MVC   L4BCPS-LINE4B(10,R2),34(R6)                                      
*                                  INTERFACE CODE                               
STAT0960 EQU   *                                                                
         LR    R5,R2               NO  - SET A(DISPLAY POSITION)                
         LA    R5,L4BCPS+11-LINE4B(R5)                                          
*                                  DISPLACE TO COMBO DISPLAY PAST               
*                                     'INTERFACE CODE' FIELD                    
         B     STAT0820            GO AND DISPLAY COMBO PARTICIPANTS            
         SPACE 2                                                                
STAT1000 EQU   *                   FORMER REP/NEW REP                           
*                                                                               
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'0C'        FORMER REP/NEW REP                           
         BAS   RE,GETEL                                                         
         BNE   STAT1020            NO ELEMENT                                   
         USING RSTAFNEL,R6                                                      
         MVC   L4CPS+6(L'RSTAFNFO),RSTAFNFO    FORMER REP                       
         MVC   L4CPS+18(L'RSTAFNNE),RSTAFNNE   NEW REP                          
         DROP  R6                                                               
STAT1020 EQU   *                                                                
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
*                                                                               
         B     STAT0100                                                         
         DROP  R2                                                               
*                                                                               
* LIST STATIONS USING COMBO PASSIVE 8301 KEY                                    
*                                                                               
STAT2000 DS    0H                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         ZAP   DUB,=P'0'                                                        
         ZAP   HALF,=P'0'                                                       
         MVC   FULL,AIOAREA                                                     
*                                                                               
         CLI   NEXTBYTE,1                                                       
         BE    STAT2002                                                         
*                                                                               
         XC    KEY,KEY                                                          
K        USING RSTAKEY,KEY                                                      
         MVC   KEY(2),=X'8301'                                                  
         MVC   K.RST3KREP,REPALPHA                                              
         MVC   K.RST3KCST,INFSTRT                                               
         OC    K.RST3KCST,SPACES                                                
*                                                                               
STAT2002 BAS   RE,HIGH             FIRST READ                                   
         CLC   KEY(2),=X'8301'     SAME REC TYPE                                
         BNE   NOREC                                                            
         CLC   K.RST3KREP,REPALPHA                                              
         BNE   NOREC                                                            
         B     STAT2020                                                         
*                                                                               
STAT2010 BAS   RE,SEQ                                                           
         CLC   KEY(2),=X'8301'     SAME REC TYPE                                
         BNE   *+14                                                             
         CLC   K.RST3KREP,REPALPHA                                              
         BE    STAT2020                                                         
*                                                                               
         CP    DUB,=P'0'                                                        
         BE    SETDONE                                                          
*                                                                               
         CLI   OPTNBYTE,18         MULTIPLE COMBO PARENT OPTION?                
         BNE   *+14                NO                                           
         CP    HALF,=P'1'          MORE THAN 1 PARENT?                          
         BNH   SETDONE             NO                                           
*                                                                               
         CVB   R1,DUB                                                           
         LA    R1,1(R1)            LEAVE A LINE AT THE BOTTOM                   
         LR    RE,R2                                                            
STAT2012 DS    0H                  VERIFY ENOUGH LINES FOR ENTRY                
         CLI   0(RE),0                                                          
         BE    SETNEXT                                                          
         ZIC   RF,0(RE)                                                         
         AR    RF,RE                                                            
         BCT   R1,STAT2012                                                      
*                                                                               
         CVB   R1,DUB                                                           
         L     RE,AIOAREA                                                       
STAT2014 DS    0H                  PRINT OUT STATION LIST LINES                 
         MVC   8(79,R2),0(RE)                                                   
         OI    6(R2),X'80'                                                      
         LA    RE,79(RE)                                                        
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R1,STAT2014                                                      
*                                                                               
         B     SETDONE                                                          
*                                                                               
STAT2020 DS    0H                                                               
         CLC   K.RST3KCST(5),SAVEKEY+(RST3KCST-RSTAKEY)                         
         BE    STAT2030             SAME STATION AS LAST KEY                    
*                                                                               
         CP    DUB,=P'0'                                                        
         BE    STAT2028                                                         
*                                                                               
         CLI   OPTNBYTE,18         MULTIPLE COMBO PARENT OPTION?                
         BNE   *+14                NO                                           
         CP    HALF,=P'1'          MORE THAN 1 PARENT?                          
         BNH   STAT2028            NO                                           
*                                                                               
STAT2022 DS    0H                  VERIFY ENOUGH LINES FOR ENTRY                
         CVB   R1,DUB                                                           
         LA    R1,1(R1)            LEAVE A LINE AT THE BOTTOM                   
         LR    RE,R2                                                            
STAT2024 DS    0H                  VERIFY ENOUGH LINES FOR ENTRY                
         CLI   0(RE),0                                                          
         BE    SETNEXT                                                          
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         BCT   R1,STAT2024                                                      
*                                                                               
         CVB   R1,DUB                                                           
         L     RE,AIOAREA                                                       
STAT2026 DS    0H                  PRINT OUT STATION LIST LINES                 
         MVC   8(79,R2),0(RE)                                                   
         OI    6(R2),X'80'                                                      
         LA    RE,79(RE)                                                        
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R1,STAT2026                                                      
*                                                                               
STAT2028 DS    0H                                                               
         MVC   SAVEKEY,KEY         SAVE KEY WITH NEW MARKET                     
         CP    DUB,=P'14'          MAX LINES ON SCREEN                          
         BNL   SETNEXT             NEXT PAGE                                    
*                                                                               
         L     RE,AIOAREA          CLEAR LINE BUFFER                            
         LA    RF,79*15                                                         
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         ZAP   DUB,=P'1'           SET TO SINGLE LINE                           
         ZAP   HALF,=P'0'                                                       
         MVC   FULL,AIOAREA                                                     
*                                                                               
         L     R5,AIOAREA                                                       
         MVC   0(4,R5),K.RST3KCST                                               
         MVI   4(R5),C'-'                                                       
         MVC   5(2,R5),=C'TV'                                                   
*                                                                               
         CLI   K.RST3KCST+4,C'A'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'AM'                                                   
*                                                                               
         CLI   K.RST3KCST+4,C'F'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'FM'                                                   
*                                                                               
         CLI   K.RST3KCST+4,C'C'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'CM'                                                   
*                                                                               
         CLI   K.RST3KCST+4,C'L'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'L '                                                   
*                                                                               
         LA    R5,12(R5)                                                        
*                                                                               
STAT2030 DS    0H                                                               
         L     RE,FULL                                                          
         LR    RF,R5                                                            
         SR    RF,RE                                                            
         CH    RF,=H'70'                                                        
         BNH   STAT2032                                                         
*                                                                               
         CP    DUB,=P'14'          MAX LINES ON SCREEN                          
         BNL   STAT2022            PRINT IT OUT, ITS TOO BIG                    
*                                                                               
         LA    RE,79               DISPLACE TO NEXT LINE                        
         A     RE,FULL                                                          
         ST    RE,FULL             STORE START OF NEW LINE                      
         LA    R5,15(RE)                                                        
         AP    DUB,=P'1'           ADD ONE MORE LINE TO BUFFER                  
*                                                                               
STAT2032 DS    0H                                                               
         AP    HALF,=P'1'          ADD ANOTHER STATION TO COUNT                 
*                                                                               
         MVC   0(4,R5),K.RST3KPST                                               
         MVI   4(R5),C'-'                                                       
         MVC   5(2,R5),=C'TV'                                                   
*                                                                               
         CLI   K.RST3KPST+4,C'A'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'AM'                                                   
*                                                                               
         CLI   K.RST3KPST+4,C'F'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'FM'                                                   
*                                                                               
         CLI   K.RST3KPST+4,C'C'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'CM'                                                   
*                                                                               
         CLI   K.RST3KPST+4,C'L'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'L '                                                   
*                                                                               
         LA    R5,8(R5)                                                         
         B     STAT2010             READ NEXT                                   
         DROP  K                                                                
         EJECT                                                                  
*                                                                               
* LIST STATIONS USING COMBO PASSIVE 8302 KEY                                    
*                                                                               
STAT2100 DS    0H                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         ZAP   DUB,=P'0'                                                        
         ZAP   HALF,=P'0'                                                       
         MVC   FULL,AIOAREA                                                     
*                                                                               
         CLI   NEXTBYTE,1                                                       
         BE    STAT2102                                                         
*                                                                               
         XC    KEY,KEY                                                          
K        USING RSTAKEY,KEY                                                      
         MVC   KEY(2),=X'8302'                                                  
         MVC   K.RST4KREP,REPALPHA                                              
         MVC   K.RST4KMKT,INFSTRT                                               
         OC    K.RST4KMKT,SPACES                                                
*                                                                               
STAT2102 BAS   RE,HIGH             FIRST READ                                   
         CLC   KEY(2),=X'8302'     SAME REC TYPE                                
         BNE   NOREC                                                            
         CLC   K.RST4KREP,REPALPHA                                              
         BNE   NOREC                                                            
         B     STAT2120                                                         
*                                                                               
STAT2110 BAS   RE,SEQ                                                           
         CLC   KEY(2),=X'8302'     SAME REC TYPE                                
         BNE   *+14                                                             
         CLC   K.RST4KREP,REPALPHA                                              
         BE    STAT2120                                                         
*                                                                               
         CP    DUB,=P'0'                                                        
         BE    SETDONE                                                          
*                                                                               
         CVB   R1,DUB                                                           
         LA    R1,1(R1)            LEAVE A LINE AT THE BOTTOM                   
         LR    RE,R2                                                            
STAT2112 DS    0H                  VERIFY ENOUGH LINES FOR ENTRY                
         CLI   0(RE),0                                                          
         BE    SETNEXT                                                          
         ZIC   RF,0(RE)                                                         
         AR    RF,RE                                                            
         BCT   R1,STAT2112                                                      
*                                                                               
         CVB   R1,DUB                                                           
         L     RE,AIOAREA                                                       
STAT2114 DS    0H                  PRINT OUT STATION LIST LINES                 
         MVC   8(79,R2),0(RE)                                                   
         OI    6(R2),X'80'                                                      
         LA    RE,79(RE)                                                        
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R1,STAT2114                                                      
*                                                                               
         B     SETDONE                                                          
*                                                                               
STAT2120 DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,IOAREA1,    +        
               DMWORK,0                                                         
*                                                                               
         OC    ACFLTR,SPACES                                                    
         CLC   ACFLTR,SPACES       ACCEPT ACT+INACT STAS?                       
         BNE   STAT2121            YES                                          
         CLC   IOAREA1+RSTAEND-RSTAREC(3),ALL                                   
         BNE   STAT2110            TAKE ACTIVES ONLY                            
*                                                                               
STAT2121 DS    0H                                                               
         CLC   K.RST4KMKT,SAVEKEY+(RST4KMKT-RSTAKEY)                            
         BE    STAT2130             SAME MARKET AS LAST KEY                     
*                                                                               
         CP    DUB,=P'0'                                                        
         BE    STAT2128                                                         
*                                                                               
STAT2122 DS    0H                                                               
         CVB   R1,DUB                                                           
         LA    R1,1(R1)            LEAVE A LINE AT THE BOTTOM                   
         LR    RE,R2                                                            
STAT2124 DS    0H                  VERIFY ENOUGH LINES FOR ENTRY                
         CLI   0(RE),0                                                          
         BE    SETNEXT                                                          
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         BCT   R1,STAT2124                                                      
*                                                                               
         CVB   R1,DUB                                                           
         L     RE,AIOAREA                                                       
STAT2126 DS    0H                  PRINT OUT STATION LIST LINES                 
         MVC   8(79,R2),0(RE)                                                   
         OI    6(R2),X'80'                                                      
         LA    RE,79(RE)                                                        
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R1,STAT2126                                                      
*                                                                               
STAT2128 DS    0H                                                               
         MVC   SAVEKEY,KEY         SAVE KEY WITH NEW MARKET                     
         CP    DUB,=P'14'          MAX LINES ON SCREEN                          
         BNL   SETNEXT             NEXT PAGE                                    
*                                                                               
         L     RE,AIOAREA          CLEAR LINE BUFFER                            
         LA    RF,79*15                                                         
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         ZAP   DUB,=P'1'           SET TO SINGLE LINE                           
         ZAP   HALF,=P'0'                                                       
         MVC   FULL,AIOAREA                                                     
*                                                                               
         L     R5,AIOAREA                                                       
         MVC   0(L'RST4KMKT,R5),K.RST4KMKT                                      
         LA    R5,25(R5)                                                        
*                                                                               
STAT2130 DS    0H                                                               
         L     RE,FULL                                                          
         LR    RF,R5                                                            
         SR    RF,RE                                                            
         CH    RF,=H'70'                                                        
         BNH   STAT2132                                                         
*                                                                               
         CP    DUB,=P'14'          MAX LINES ON SCREEN                          
         BNL   STAT2122            PRINT IT OUT, ITS TOO BIG                    
*                                                                               
         LA    RE,79               DISPLACE TO NEXT LINE                        
         A     RE,FULL                                                          
         ST    RE,FULL             STORE START OF NEW LINE                      
         LA    R5,28(RE)                                                        
         AP    DUB,=P'1'           ADD ONE MORE LINE TO BUFFER                  
*                                                                               
STAT2132 DS    0H                                                               
         AP    HALF,=P'1'          ADD ANOTHER STATION TO COUNT                 
*                                                                               
         MVC   0(4,R5),K.RST4KSTA                                               
         MVI   4(R5),C'-'                                                       
         MVC   5(2,R5),=C'TV'                                                   
*                                                                               
         CLI   K.RST4KSTA+4,C'A'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'AM'                                                   
*                                                                               
         CLI   K.RST4KSTA+4,C'F'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'FM'                                                   
*                                                                               
         CLI   K.RST4KSTA+4,C'C'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'CM'                                                   
*                                                                               
         CLI   K.RST4KSTA+4,C'L'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'L '                                                   
*                                                                               
         LA    R5,8(R5)                                                         
         B     STAT2110             READ NEXT                                   
         DROP  K                                                                
         EJECT                                                                  
*                                                                               
* LIST STATIONS USING COMBO PASSIVE 8303 KEY                                    
*                                                                               
STAT2200 DS    0H                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         ZAP   DUB,=P'0'                                                        
         ZAP   HALF,=P'0'                                                       
         MVC   FULL,AIOAREA                                                     
*                                                                               
         CLI   NEXTBYTE,1                                                       
         BE    STAT2202                                                         
*                                                                               
         XC    KEY,KEY                                                          
K        USING RSTAKEY,KEY                                                      
         MVC   KEY(2),=X'8303'                                                  
         MVC   K.RST5KREP,REPALPHA                                              
         MVC   K.RST5KOWN,INFSTRT                                               
         OC    K.RST5KOWN,SPACES                                                
*                                                                               
STAT2202 BAS   RE,HIGH             FIRST READ                                   
         CLC   KEY(2),=X'8303'     SAME REC TYPE                                
         BNE   NOREC                                                            
         CLC   K.RST5KREP,REPALPHA                                              
         BNE   NOREC                                                            
         B     STAT2220                                                         
*                                                                               
STAT2210 BAS   RE,SEQ                                                           
         CLC   KEY(2),=X'8303'     SAME REC TYPE                                
         BNE   *+14                                                             
         CLC   K.RST5KREP,REPALPHA                                              
         BE    STAT2220                                                         
*                                                                               
         CP    DUB,=P'0'                                                        
         BE    SETDONE                                                          
*                                                                               
         CVB   R1,DUB                                                           
         LA    R1,1(R1)            LEAVE A LINE AT THE BOTTOM                   
         LR    RE,R2                                                            
STAT2212 DS    0H                  VERIFY ENOUGH LINES FOR ENTRY                
         CLI   0(RE),0                                                          
         BE    SETNEXT                                                          
         ZIC   RF,0(RE)                                                         
         AR    RF,RE                                                            
         BCT   R1,STAT2212                                                      
*                                                                               
         CVB   R1,DUB                                                           
         L     RE,AIOAREA                                                       
STAT2214 DS    0H                  PRINT OUT STATION LIST LINES                 
         MVC   8(79,R2),0(RE)                                                   
         OI    6(R2),X'80'                                                      
         LA    RE,79(RE)                                                        
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R1,STAT2214                                                      
*                                                                               
         B     SETDONE                                                          
*                                                                               
STAT2220 DS    0H                                                               
         CLC   K.RST5KOWN,SAVEKEY+(RST5KOWN-RSTAKEY)                            
         BE    STAT2230             SAME OWNER AS LAST KEY                      
*                                                                               
         CP    DUB,=P'0'                                                        
         BE    STAT2228                                                         
*                                                                               
STAT2222 DS    0H                                                               
         CVB   R1,DUB                                                           
         LA    R1,1(R1)            LEAVE A LINE AT THE BOTTOM                   
         LR    RE,R2                                                            
STAT2224 DS    0H                  VERIFY ENOUGH LINES FOR ENTRY                
         CLI   0(RE),0                                                          
         BE    SETNEXT                                                          
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         BCT   R1,STAT2224                                                      
*                                                                               
         CVB   R1,DUB                                                           
         L     RE,AIOAREA                                                       
STAT2226 DS    0H                  PRINT OUT STATION LIST LINES                 
         MVC   8(79,R2),0(RE)                                                   
         OI    6(R2),X'80'                                                      
         LA    RE,79(RE)                                                        
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R1,STAT2226                                                      
*                                                                               
STAT2228 DS    0H                                                               
         MVC   SAVEKEY,KEY         SAVE KEY WITH NEW OWNER                      
         CP    DUB,=P'14'          MAX LINES ON SCREEN                          
         BNL   SETNEXT             NEXT PAGE                                    
*                                                                               
         OC    K.RST5KOWN,K.RST5KOWN                                            
         BZ    STAT2210            SKIP OWNERLESS                               
*                                                                               
         XC    KEY,KEY             READ OWNER RECORD                            
OK       USING ROWNKEY,KEY                                                      
         MVI   OK.ROWNKTYP,X'2A'                                                
         MVC   OK.ROWNKREP,REPALPHA                                             
         MVC   OK.ROWNKOWN,SAVEKEY+(RST5KOWN-RSTAKEY)                           
         DROP  OK                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'ROWNKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   WORK(20),=CL20'OWNER NOT ON FILE'                                
         B     STAT2229                                                         
*                                                                               
         GOTO1 GETREC                                                           
         L     RE,AIOAREA                                                       
         USING ROWNREC,RE                                                       
         MVC   WORK(L'ROWNNAME),ROWNNAME                                        
         DROP  RE                                                               
*                                                                               
STAT2229 DS    0H                                                               
         MVC   KEY,SAVEKEY         RESTORE SEQUENCE                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+6                 HAD TO BE THERE                              
         DC    H'0'                                                             
*                                                                               
         L     RE,AIOAREA          CLEAR LINE BUFFER                            
         LA    RF,79*15                                                         
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         ZAP   DUB,=P'1'           SET TO SINGLE LINE                           
         ZAP   HALF,=P'0'                                                       
         MVC   FULL,AIOAREA                                                     
*                                                                               
         L     R5,AIOAREA                                                       
         MVC   0(L'RST5KOWN,R5),K.RST5KOWN                                      
         MVC   L'RST5KOWN+2(L'ROWNNAME,R5),WORK                                 
         LA    R5,30(R5)                                                        
*                                                                               
STAT2230 DS    0H                                                               
         L     RE,FULL                                                          
         LR    RF,R5                                                            
         SR    RF,RE                                                            
         CH    RF,=H'70'                                                        
         BNH   STAT2232                                                         
*                                                                               
         CP    DUB,=P'14'          MAX LINES ON SCREEN                          
         BNL   STAT2222            PRINT IT OUT, ITS TOO BIG                    
*                                                                               
         LA    RE,79               DISPLACE TO NEXT LINE                        
         A     RE,FULL                                                          
         ST    RE,FULL             STORE START OF NEW LINE                      
         LA    R5,33(RE)                                                        
         AP    DUB,=P'1'           ADD ONE MORE LINE TO BUFFER                  
*                                                                               
STAT2232 DS    0H                                                               
         AP    HALF,=P'1'          ADD ANOTHER STATION TO COUNT                 
*                                                                               
         MVC   0(4,R5),K.RST4KSTA                                               
         MVI   4(R5),C'-'                                                       
         MVC   5(2,R5),=C'TV'                                                   
*                                                                               
         CLI   K.RST4KSTA+4,C'A'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'AM'                                                   
*                                                                               
         CLI   K.RST4KSTA+4,C'F'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'FM'                                                   
*                                                                               
         CLI   K.RST4KSTA+4,C'C'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'CM'                                                   
*                                                                               
         CLI   K.RST4KSTA+4,C'L'                                                
         BNE   *+10                                                             
         MVC   5(2,R5),=C'L '                                                   
*                                                                               
         LA    R5,8(R5)                                                         
         B     STAT2210             READ NEXT                                   
         DROP  K                                                                
         EJECT                                                                  
*                                                                               
*   OFTMFILT:  CHECK STATION RECORD FOR COMPLIANCE WITH OFFICE/TEAM             
*        OR TEAM FILTER.  IF NOT SELECTED, RETURN CC NOT = ZERO.                
*                                                                               
OFTMFILT NTR1                                                                   
         LA    R3,RSTAELEM         A(DESCRIPT ELEMENT)                          
OTMF0020 EQU   *                                                                
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         CLI   0(R3),0             END OF RECORD?                               
         BE    OTMF0200            YES - NOT FOUND                              
         CLI   0(R3),X'04'         OFFICE/TEAM ELEMENT?                         
         BNE   OTMF0020            NO  - GO BACK FOR NEXT                       
         USING RSTAOTEL,R3         YES - CHECK IT OUT                           
         TM    FLTRBYT3,X'01'      OFFICE/TEAM REQUEST?                         
         BNO   OTMF0080            NO  - CHECK FOR TEAM ONLY                    
         CLC   OFTMFLTR,RSTAOTOF   FOUND? FILTER VS OFF/TEAM IN ELT             
         BNE   OTMF0020            NO  - GO BACK FOR NEXT                       
         B     OTMF0160            YES - RETURN CC = ZERO                       
OTMF0080 EQU   *                                                                
         TM    FLTRBYT3,X'02'      TEAM REQUEST?  (REDUNDANT)                   
         BO    *+6                 YES                                          
         DC    H'0'                SHOULDN'T HAPPEN                             
         CLC   OFTMFLTR(2),RSTAOTTM FOUND? FILTER VS TEAM IN ELT                
         BNE   OTMF0020            NO  - GO BACK FOR NEXT                       
OTMF0160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     OTMF0240                                                         
OTMF0200 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
OTMF0240 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
OFF10    LA    R8,KEYTAB                                                        
         CLI   NEXTBYTE,1                                                       
         BE    OFF12                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,4                                                            
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),INFSTRT                                                
         OC    KEY+25(2),SPACES                                                 
*                                                                               
OFF12    BAS   RE,HIGH                                                          
         CLI   KEY,4                                                            
         BNE   NOREC                                                            
         CLC   REPALPHA,KEY+23                                                  
         BNE   NOREC                                                            
         B     OFF22                                                            
         SPACE 1                                                                
OFF20    BAS   RE,SEQ                                                           
         CLI   KEY,4                                                            
         BNE   SETDONE                                                          
         CLC   REPALPHA,KEY+23                                                  
         BNE   SETDONE                                                          
*                                                                               
OFF22    BAS   RE,GETREC                                                        
         CLC   REGFLTR,ALL                                                      
         BE    OFF24                                                            
         CLC   REGFLTR,ROFFREG                                                  
         BNE   OFF20                                                            
         B     OFF30                                                            
*                                                                               
OFF24    DS    0H                                                               
         CLC   SCPFLTR,ALL                                                      
         BE    OFF30                                                            
         MVC   SAVEKEY,KEY                                                      
         MVI   KEY,ROFF2TYQ        GET OFF2 RECORD                              
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   OFF27               NO SCOPE INFO, SKIP                          
*                                                                               
         L     R6,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   OFF27               NO SCOPE INFO, SKIP                          
*                                                                               
         CLI   SCPFLTR,C'L'        LOCAL FILTER?                                
         BE    OFF25                                                            
         USING ROFF2FXE,R6         * NATIONAL FILTER                            
         TM    ROFF2PRF+1,X'80'    LOCAL OFFICE?                                
         BO    OFF28               LOCAL OFF, SKIP                              
         B     OFF29                                                            
*                                                                               
OFF25    DS    0H                  * LOCAL FILTER                               
         TM    ROFF2PRF+1,X'80'    LOCAL OFFICE?                                
         BZ    OFF28               NO, SKIP                                     
         B     OFF29                                                            
         DROP  R6                                                               
*                                                                               
OFF27    DS    0H                  NO SCOPE INFOR FOR THIS RECORD               
         CLI   SCPFLTR,C'N'        NATIONAL SCOPE FILTER?                       
         BE    OFF29               YES, DISPLAY RECORD                          
*                                                                               
OFF28    MVC   KEY,SAVEKEY         RESTORE SEQ ORDER                            
         BAS   RE,HIGH             CHECK NEXT OFFICE RECORD                     
         B     OFF20                                                            
*                                                                               
OFF29    DS    0H                                                               
         MVC   KEY,SAVEKEY         RESTORE SEQ ORDER                            
         BAS   RE,HIGH             CHECK ORIGINAL RECORD                        
         BAS   RE,GETREC                                                        
*                                                                               
OFF30    MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         USING LINE7,R2                                                         
         MVI   L7SCP,C'N'          ASSUME NATIONAL OFFICE FOR NOW               
         MVC   L7OFF,ROFFKOFF                                                   
*                                                                               
         MVC   0(L'L7OFF,R8),L7OFF  INSERT ENTRY IN TAB                         
         LA    R8,L'KEYTAB(R8)      BUMP KEYTAB POINTER                         
*                                                                               
         MVC   L7RGN,ROFFREG                                                    
         MVC   L7NAM,ROFFNAME                                                   
         CLI   OPTNBYTE,4                                                       
         BE    OFF40                                                            
*                                                                               
         MVC   L7ADDR,ROFFADD1                                                  
         MVC   L7ADD2,ROFFADD2                                                  
         MVC   L7STAT,ROFFSTT                                                   
         MVC   L7ZIP,ROFFZIP                                                    
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         MVI   KEY,ROFF2TYQ                                                     
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   OFF50                                                            
*                                                                               
         BAS   RE,GETREC                                                        
         L     R6,AIOAREA                                                       
         USING ROFF2FXE,R6                                                      
         MVI   ELCODE,ROFF2CDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   OFF50                                                            
*                                                                               
         TM    ROFF2PRF+1,X'80'    LOCAL OFFICE?                                
         BZ    OFF50               NO, SKIP                                     
         MVI   L7SCP,C'L'          LOCAL OFFICE                                 
         DROP  R6                                                               
*                                                                               
         B     OFF50                                                            
*                                                                               
OFF40    DS    0H                                                               
         MVI   L7SCP,C'N'          ASSUME NATIONAL OFFICE                       
         MVC   SAVEKEY,KEY                                                      
         MVI   KEY,ROFF2TYQ                                                     
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   OFF50                                                            
*                                                                               
         BAS   RE,GETREC                                                        
         L     R6,AIOAREA                                                       
         USING ROFF2FXE,R6                                                      
         MVI   ELCODE,ROFF2CDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   OFF50                                                            
*                                                                               
         MVI   L7FAX,C'('          FAX NUMBER                                   
         MVC   L7FAX+1(3),ROFF2FAX                                              
         MVI   L7FAX+4,C')'                                                     
         MVC   L7FAX+5(3),ROFF2FAX+3                                            
         MVI   L7FAX+8,C'-'                                                     
         MVC   L7FAX+9(4),ROFF2FAX+6                                            
         TM    ROFF2PRF+1,X'80'    LOCAL OFFICE?                                
         BZ    OFF50               NO, SKIP                                     
         MVI   L7SCP,C'L'          LOCAL OFFICE                                 
         DROP  R6                                                               
*                                                                               
OFF50    DS    0H                                                               
         MVC   KEY,SAVEKEY         RESTORE SEQ ORDER                            
         BAS   RE,HIGH                                                          
*                                                                               
OFF60    DS    0H                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     OFF20                                                            
         DROP  R2                                                               
         EJECT                                                                  
TER10    CLI   NEXTBYTE,1                                                       
         BE    TER12                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'3D'                                                        
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),INFSTRT                                                
         OC    KEY+25(2),SPACES                                                 
*                                                                               
TER12    BAS   RE,HIGH                                                          
         CLI   KEY,X'3D'                                                        
         BNE   NOREC                                                            
         CLC   REPALPHA,KEY+23                                                  
         BNE   NOREC                                                            
         B     TER22                                                            
         SPACE 1                                                                
TER20    BAS   RE,SEQ                                                           
         CLI   KEY,X'3D'                                                        
         BNE   SETDONE                                                          
         CLC   REPALPHA,KEY+23                                                  
         BNE   SETDONE                                                          
*                                                                               
TER22    BAS   RE,GETREC                                                        
         MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         USING LINE7,R2                                                         
         MVC   L7OFF,RTERKTER                                                   
         MVC   L7NAM,RTERNAME                                                   
*****    CLI   OPTNBYTE,4                                                       
*****    BE    TER40                                                            
*                                                                               
TER40    DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         MVI   KEY,X'3D'                                                        
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TER50                                                            
*                                                                               
TER50    DS    0H                                                               
         MVC   KEY,SAVEKEY         RESTORE SEQ ORDER                            
         BAS   RE,HIGH                                                          
*                                                                               
TER60    DS    0H                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     TER20                                                            
         DROP  R2                                                               
         EJECT                                                                  
PROD0020 CLI   OPTNBYTE,8                                                       
         BNE   PROD0040                                                         
         LA    R4,PROD0320         DISPLAY RTN: POINT PERSON,ETC                
         B     PROD0100                                                         
PROD0040 CLI   OPTNBYTE,9                                                       
         BNE   PROD0060                                                         
         LA    R4,PROD0400         DISPLAY RTN: SPOT                            
         B     PROD0100                                                         
PROD0060 CLI   OPTNBYTE,16         DISPLAY: AG/OFF FLIGHT DATES                 
         BNE   PROD0080                                                         
         LA    R4,PROD0460         DISPLAY RTN: SPOT                            
         B     PROD0100                                                         
PROD0080 LA    R4,PROD0280         DISPLAY RTN: REGULAR PRODUCT                 
*                                                                               
PROD0100 LA    R1,KEYTAB                                                        
         CLI   NEXTBYTE,1                                                       
         BE    PROD0120                                                         
*                                                                               
         OC    INFSTRT,SPACES                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,9                                                            
         MVC   KEY+18(4),ADVFLTR                                                
         MVC   KEY+25(2),REPALPHA                                               
*                                                                               
PROD0120 BAS   RE,HIGH                                                          
         CLI   KEY,9                                                            
         BNE   SETDONE                                                          
         B     PROD0160                                                         
*                                                                               
PROD0140 EQU   *                                                                
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,SEQ                                                           
         CLI   KEY,9                                                            
         BNE   SETDONE                                                          
*                                                                               
PROD0160 CLC   ADVFLTR,ALL                                                      
         BE    PROD0180                                                         
         CLC   ADVFLTR,KEY+18                                                   
         BNE   PROD0140                                                         
*                                                                               
PROD0180 CLC   KEY+22(3),INFSTRT                                                
         BL    PROD0140                                                         
*                                                                               
         CLI   REPBYTE,1                                                        
         BE    PROD0200                                                         
         CLC   REPALPHA,KEY+25                                                  
         BE    PROD0220                                                         
*                                                                               
PROD0200 CLI   REPBYTE,2                                                        
         BE    PROD0140                                                         
         CLC   =C'ZZ',KEY+25                                                    
         BE    PROD0220                                                         
         MVC   KEY+25(2),=C'ZZ'                                                 
         B     PROD0120                                                         
*                                                                               
PROD0220 MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         BAS   RE,GETREC                                                        
*                                                                               
         OC    NETFLTR,SPACES                                                   
         OC    PPFLTR,SPACES                                                    
         CLC   PPFLTR,SPACES       POINT PERSON FILTER?                         
         BE    PROD0240                                                         
         LA    R6,RPRDREC                                                       
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   PROD0140            NO ELEMENT - SKIP RECORD                     
         CLC   PPFLTR,22(R6)       CHECK POINT PERSON                           
         BNE   PROD0140            NOT FOUND - SKIP RECORD                      
PROD0240 CLC   NETFLTR,SPACES      NETWORK CONT FILTER?                         
         BE    PROD0260            NO                                           
         CLC   NETFLTR,RPRDNET#    CHECK NET CONTRACT #                         
         BNE   PROD0140            NOT FOUND - SKIP RECORD                      
*                                                                               
PROD0260 BR    R4                                                               
*                                                                               
         USING LINE8,R2                                                         
PROD0280 MVC   L8PRD1,RPRDKPRD                                                  
         MVC   L8ADV1,RPRDKADV                                                  
         MVC   L8NAM1,RPRDNAME                                                  
         MVC   L8CLS1,RPRDCLSS                                                  
         MVC   L8CTG1,RPRDCATG                                                  
*                                                                               
         BAS   RE,BLDPRDK            INSERT KEYTAB ENTRY                        
         LA    R1,L'KEYTAB(R1)       BUMP KEYTAB POINTER                        
*                                                                               
         LA    R4,PROD0300                                                      
         B     PROD0140                                                         
*                                                                               
PROD0300 MVC   L8PRD2,RPRDKPRD                                                  
         MVC   L8ADV2,RPRDKADV                                                  
         MVC   L8NAM2,RPRDNAME                                                  
         MVC   L8CTG2,RPRDCATG                                                  
         MVC   L8CLS2,RPRDCLSS                                                  
*                                                                               
         BAS   RE,BLDPRDK            INSERT KEYTAB ENTRY                        
         LA    R1,L'KEYTAB(R1)       BUMP KEYTAB POINTER                        
*                                                                               
         LA    R4,PROD0280                                                      
         FOUT  (R2)                                                             
*                                                                               
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     PROD0140                                                         
         DROP  R2                                                               
*                                                                               
         USING LINE8A,R2                                                        
PROD0320 MVC   L8APRD1,RPRDKPRD                                                 
         MVC   L8AADV1,RPRDKADV                                                 
         CLC   RPRDNET#,SPACES                                                  
         BE    PROD0340                                                         
         MVC   L8ANET1,RPRDNET#                                                 
         LA    R6,RPRDREC                                                       
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   PROD0340            SHOULDN'T HAPPEN                             
         MVC   L8APNT1,22(R6)                                                   
         MVC   L8ADESC1,2(R6)                                                   
PROD0340 LA    R4,PROD0360                                                      
         B     PROD0140                                                         
*                                                                               
PROD0360 MVC   L8APRD2,RPRDKPRD                                                 
         MVC   L8AADV2,RPRDKADV                                                 
         CLC   RPRDNET#,SPACES                                                  
         BE    PROD0380                                                         
         MVC   L8ANET2,RPRDNET#                                                 
         LA    R6,RPRDREC                                                       
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   PROD0380            SHOULDN'T HAPPEN                             
         MVC   L8APNT2,22(R6)                                                   
         MVC   L8ADESC2,2(R6)                                                   
PROD0380 LA    R4,PROD0320                                                      
         FOUT  (R2)                                                             
*                                                                               
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     PROD0140                                                         
         DROP  R2                                                               
*                                                                               
         USING LINE8B,R2                                                        
PROD0400 MVC   L8BPRD,RPRDKPRD                                                  
         MVC   L8BNAME,RPRDNAME                                                 
         MVC   L8BADV,RPRDKADV                                                  
         CLC   RPRDNET#,SPACES                                                  
         BE    PROD0440                                                         
         MVC   L8BNET,RPRDNET#                                                  
         LA    R6,RPRDREC                                                       
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   PROD0420            SHOULDN'T HAPPEN                             
         MVC   L8BPNT,22(R6)                                                    
         MVC   L8BDESC,2(R6)                                                    
PROD0420 LA    R6,RPRDREC                                                       
         MVI   ELCODE,3                                                         
         BAS   RE,GETEL                                                         
         BNE   PROD0440            SHOULDN'T HAPPEN                             
         MVC   L8BSPCLT,2(R6)                                                   
         MVC   L8BSPPRD,5(R6)                                                   
         LA    R3,RPRDSPES-RPRDSPOT(R6)                                         
         EDIT  (1,0(R3)),(3,L8BSPES),ZERO=NOBLANK,ALIGN=LEFT                    
*                                                                               
PROD0440 LA    R4,PROD0400                                                      
         FOUT  (R2)                                                             
*                                                                               
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     PROD0140                                                         
         DROP  R2                                                               
*                                                                               
         USING LINE8C,R2                                                        
PROD0460 EQU   *                                                                
         MVC   L8CCODE,RPRDKPRD    PRODUCT CODE                                 
         MVC   L8CADV,RPRDKADV     ADVERTISER                                   
         LA    R6,RPRDREC                                                       
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   PROD0480                                                         
         MVC   L8CNAME,2(R6)       PRODUCT NAME                                 
*                                                                               
PROD0480 EQU   *                                                                
         LA    R6,RPRDREC                                                       
         MVI   ELCODE,4                                                         
         BAS   RE,GETEL                                                         
         BNE   PROD0580                                                         
         MVC   L8CAGY(4),2(R6)     AGENCY CODE                                  
         OC    6(2,R6),6(R6)       ANY OFFICE?                                  
         BZ    PROD0500            NO                                           
         MVI   L8CAGY+4,C'-'                                                    
         MVC   L8CAGY+5(2),6(R6)   OFFICE                                       
PROD0500 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(3,8(R6)),(11,L8CDATE)  START DATE                  
         OC    11(3,R6),11(R6)     END DATE?                                    
         BZ    PROD0520                                                         
         MVI   L8CDATE+8,C'-'                                                   
         GOTO1 VDATCON,DMCB,(3,11(R6)),(11,L8CDATE+9)  END DATE                 
*                                                                               
* DISPLAY AGENCY NAME                                                           
*                                                                               
PROD0520 EQU   *                                                                
         OC    2(4,R6),2(R6)       AGENCY?                                      
         BZ    PROD0580                                                         
         MVC   SAVEKEY,KEY         SAVE KEY TO REASTABLISH THE SEQ              
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'0A'           AGENCY RECORD                                
         MVC   KEY+19(6),SPACES    MOVE SPACES INTO AGY/OFFICE SPOTS            
         MVC   KEY+25(2),REPALPHA  REP CODE                                     
         MVC   KEY+19(4),2(R6)     AGENCY                                       
         OC    6(2,R6),6(R6)       AGENCY OFFICE?                               
         BZ    *+10                NO                                           
         MVC   KEY+23(2),6(R6)     OFFICE                                       
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     HAS AGY BEEN FOUND?                          
         BE    PROD0540            NO - SKIP                                    
         MVC   L8CAGYN(20),=C'AGENCY NOT ON FILE  '                             
         B     PROD0560                                                         
*                                                                               
* AGY HAS BEEN FOUND - DISPLAY AGENCY NAME                                      
*                                                                               
PROD0540 EQU   *                                                                
         L     R8,AIOAREA          SAVE A(CURRENT IO AREA)                      
         GOTO1 GETREC              RETRIEVE CATEGORY RECORD                     
         USING RAGYREC,R8          AGENCY RECORD                                
         MVC   L8CAGYN(20),RAGYNAM1   MOVE AGY NAME TO THE SCREEN               
         DROP  R8                                                               
PROD0560 EQU   *                                                                
         MVC   KEY,SAVEKEY         RESTORE THE KEY                              
         GOTO1 HIGH                                                             
*                                                                               
PROD0580 EQU   *                                                                
         LA    R4,PROD0460                                                      
         FOUT  (R2)                                                             
*                                                                               
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     PROD0140                                                         
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
CAT10    LA    R4,CAT40                                                         
         CLI   NEXTBYTE,1                                                       
         BE    CAT12                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0F'                                                        
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),INFSTRT                                                
         OC    KEY+25(2),SPACES                                                 
*                                                                               
CAT12    BAS   RE,HIGH                                                          
         CLI   KEY,X'0F'                                                        
         BNE   NOREC                                                            
         CLC   REPALPHA,KEY+23                                                  
         BNE   NOREC                                                            
         B     CAT22                                                            
         SPACE 1                                                                
CAT20    BAS   RE,SEQ                                                           
         CLI   KEY,X'0F'                                                        
         BNE   SETDONE                                                          
         CLC   REPALPHA,KEY+23                                                  
         BNE   SETDONE                                                          
*                                                                               
CAT22    BAS   RE,GETREC                                                        
         CLC   CLSFLTR,ALL                                                      
         BE    CAT30                                                            
         CLC   CLSFLTR,RCTGCLSS                                                 
         BNE   CAT20                                                            
*                                                                               
CAT30    MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         USING LINE9,R2                                                         
         BR    R4                                                               
*                                                                               
CAT40    MVC   L9CODE1,RCTGKCTG                                                 
         MVC   L9NAME1,RCTGNAME                                                 
         MVC   L9FLD1,RCTGCLSS                                                  
         LA    R4,CAT42                                                         
         B     CAT20                                                            
*                                                                               
CAT42    MVC   L9CODE2,RCTGKCTG                                                 
         MVC   L9NAME2,RCTGNAME                                                 
         MVC   L9FLD2,RCTGCLSS                                                  
         LA    R4,CAT40                                                         
         FOUT  (R2)                                                             
*                                                                               
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         AR    R2,R5                                                            
         BAS   RE,CLRLINE                                                       
         B     CAT20                                                            
         DROP  R2                                                               
         EJECT                                                                  
CLS10    LA    R4,CLS40                                                         
         CLI   NEXTBYTE,1                                                       
         BE    CLS12                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0D'                                                        
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),INFSTRT                                                
         OC    KEY+25(2),SPACES                                                 
*                                                                               
CLS12    BAS   RE,HIGH                                                          
         CLI   KEY,X'0D'                                                        
         BNE   NOREC                                                            
         CLC   REPALPHA,KEY+23                                                  
         BNE   NOREC                                                            
         B     CLS30                                                            
         SPACE 1                                                                
CLS20    BAS   RE,SEQ                                                           
         CLI   KEY,X'0D'                                                        
         BNE   SETDONE                                                          
         CLC   REPALPHA,KEY+23                                                  
         BNE   SETDONE                                                          
*                                                                               
CLS30    MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         BAS   RE,GETREC                                                        
         USING LINE9,R2                                                         
         BR    R4                                                               
*                                                                               
CLS40    MVC   L9CODE1,RCLSKCLS                                                 
         MVC   L9NAME1,RCLSNAME                                                 
         LA    R4,CLS42                                                         
         B     CLS20                                                            
*                                                                               
CLS42    MVC   L9CODE2,RCLSKCLS                                                 
         MVC   L9NAME2,RCLSNAME                                                 
         LA    R4,CLS40                                                         
*                                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         AR    R2,R5                                                            
         BAS   RE,CLRLINE                                                       
         B     CLS20                                                            
         DROP  R2                                                               
         EJECT                                                                  
GRP10    LA    R4,GRP40                                                         
         CLI   NEXTBYTE,1                                                       
         BE    GRP12                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'07'                                                        
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),INFSTRT                                                
         OC    KEY+25(2),SPACES                                                 
*                                                                               
GRP12    BAS   RE,HIGH                                                          
         CLI   KEY,X'07'                                                        
         BNE   NOREC                                                            
         CLC   REPALPHA,KEY+23                                                  
         BNE   NOREC                                                            
         B     GRP30                                                            
         SPACE 1                                                                
GRP20    BAS   RE,SEQ                                                           
         CLI   KEY,X'07'                                                        
         BNE   SETDONE                                                          
         CLC   REPALPHA,KEY+23                                                  
         BNE   SETDONE                                                          
*                                                                               
GRP30    MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         BAS   RE,GETREC                                                        
         USING LINE11,R2                                                        
         BR    R4                                                               
*                                                                               
GRP40    MVC   L11CODE,RGRPKGRP                                                 
         MVC   L11GRP,RGRPNAME                                                  
         MVC   L11SGRP,RGRPSBNM                                                 
         LA    R4,GRP42                                                         
         B     GRP20                                                            
*                                                                               
GRP42    MVC   L11CODE2,RGRPKGRP                                                
         MVC   L11GRP2,RGRPNAME                                                 
         MVC   L11SGRP2,RGRPSBNM                                                
         LA    R4,GRP40                                                         
*                                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         AR    R2,R5                                                            
         BAS   RE,CLRLINE                                                       
         B     GRP20                                                            
         DROP  R2                                                               
         EJECT                                                                  
OWN10    LA    R4,OWN40            SET OUTPUT AREA OF PRINT LINE                
         CLI   NEXTBYTE,1                                                       
         BE    OWN12                                                            
*                                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'2A'           OWNERSHIP RECORD TYPE                        
         MVC   KEY+22(2),REPALPHA  REP CODE                                     
*****    MVC   KEY+29(1),INFSTRT   CONTROL BYTE                                 
*****    OI    KEY+29,C' '                                                      
         MVC   KEY+24(3),INFSTRT                                                
         OC    KEY+24(3),SPACES                                                 
*                                                                               
OWN12    BAS   RE,HIGH                                                          
         CLI   KEY,X'2A'                                                        
         BNE   NOREC                                                            
         CLC   REPALPHA,KEY+22                                                  
         BNE   NOREC                                                            
         B     OWN30                                                            
         SPACE 1                                                                
OWN20    BAS   RE,SEQ                                                           
         CLI   KEY,X'2A'                                                        
         BNE   SETDONE                                                          
         CLC   REPALPHA,KEY+22                                                  
         BNE   SETDONE                                                          
*                                                                               
OWN30    MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0             FIELD LENGTH=0?                              
         BE    SETNEXT                                                          
         BAS   RE,GETREC                                                        
         USING LINE13,R2           R2 PTS FST DISPLAY LN OF SCREEN              
         BR    R4                                                               
*                                                                               
OWN40    MVC   L13CODE1(3),ROWNKOWN   DISPLAY FIRST COLUMN                      
         MVC   L13OWN1(20),ROWNNAME                                             
         LA    R4,OWN42                                                         
         B     OWN20               READS NEXT OWNERSHIP RECORD                  
*                                                                               
OWN42    MVC   L13CODE2(3),ROWNKOWN   DISPLAY SECOND COLUMN                     
         MVC   L13OWN2(20),ROWNNAME                                             
         LA    R4,OWN44                                                         
         B     OWN20                                                            
*                                                                               
OWN44    MVC   L13CODE3(3),ROWNKOWN   DISPLAY THIRD COLUMN                      
         MVC   L13OWN3(20),ROWNNAME                                             
         LA    R4,OWN40            DISPLAY NEXT LINE -1ST COLUMN                
*                                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     OWN20                                                            
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*- DMENU - LIST DEMO MENU RECORDS                                               
*                                                                               
DMN10    MVC   SAVEKEY,KEY                                                      
*                                                                               
*- 1ST TIME IN OR NEXT?                                                         
         CLI   NEXTBYTE,0                                                       
         BNE   DMN20               NEXT                                         
*                                                                               
*- BUILD STARTING KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'23'           KEY ID, DMENU REC                            
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),INFSTRT   OPTIONAL STARTING POINT                      
         MVC   SAVEKEY,KEY                                                      
*                                                                               
*- READ KEY                                                                     
DMN20    BAS   RE,HIGH                                                          
         B     DMN40                                                            
*                                                                               
*- READ NEXT KEY                                                                
DMN30    BAS   RE,SEQ                                                           
*                                                                               
*- CHECK FOR END OF DATA                                                        
DMN40    CLC   KEY(25),SAVEKEY     = THRU REP CODE?                             
         BNE   SETDONE                                                          
*                                                                               
         TM    KEY+27,X'80'        DELETED?                                     
         BO    DMN30                                                            
*                                                                               
         MVC   SAVEKEY,KEY         SAVE NEW KEY                                 
*                                                                               
         CLI   0(R2),0             END OF SCREEN?                               
         BE    SETNEXT             YES.  GET OUT.                               
*                                                                               
         ZIC   R5,0(R2)            NEED ROOM FOR DEMO CODES ALSO                
         LA    R5,0(R2,R5)                                                      
         CLI   0(R5),0                                                          
         BE    SETNEXT             DON'T HAVE 2ND LINE. EXIT                    
*                                                                               
         BAS   RE,GETREC           READ IN RECORD                               
*                                                                               
*- MOVE RECORD CODE & DESCRIPTION TO OUTPUT                                     
         USING LINE14,R2                                                        
         MVC   L14CODE,RDEMKDEM    DEMO MENU CODE                               
         MVC   L14DESC,RDEMDES     DEMO MENU DESCRIPTION                        
         DROP  R2                                                               
*                                                                               
*- POINT TO NEXT SCREEN LINE                                                    
         FOUT  (R2)                TRANSMIT THIS LINE                           
         ZIC   R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
*                                                                               
*- FORMAT DEMO VALUES ON 2ND SCREEN LINE                                        
         SPACE 1                                                                
         XC    IOAREA1(200),IOAREA1  BUILD DEMO VALUES HERE                     
         LA    R6,RDEMDEM          DEMO LIST                                    
         LA    R4,IOAREA1                                                       
         ZIC   R5,RDEMNUM                                                       
         SPACE 1                                                                
         LA    R8,TRDEMOB                                                       
         USING DEMOD,R8                                                         
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         SPACE 1                                                                
DMN100   CLI   1(R6),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R6),C'I'                                                       
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000AE0',0                                     
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(0,0(R6)),(6,0(R4)),(0,DBLOCK),0                       
*                                                                               
         CLI   1(R6),C'I'          REVERSE FUDGE FOR DEMOCON                    
         BNE   *+8                                                              
         MVI   1(R6),C'T'                                                       
         DROP  R8                                                               
         SPACE 1                                                                
         LR    RE,R4                                                            
         LA    RF,6                                                             
DMN130   CLI   0(RE),C' '          PUT COMMA AT END OF DEMO EXPRESSION          
         BE    DMN140                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,DMN130                                                        
DMN140   MVI   0(RE),C','                                                       
         LA    R4,1(RE)                                                         
         LA    R6,3(R6)                                                         
         CLI   0(R6),X'FF'         END OF LIST                                  
         BE    *+8                                                              
         BCT   R5,DMN100                                                        
*                                                                               
*- MOVE OUT DEMO VALUES.  IF TEXT EXCEEDS SCREEN SPACE, FLAG W/'**'             
         USING LINE14A,R2                                                       
         MVC   L14ADEMS,IOAREA1                                                 
         ZICM  RF,L14ADEMS-1+L'L14ADEMS,(1)   TEST LAST BYTE                    
         BZ    DMN200                                                           
         MVC   L14AMORE,=C'**'     MORE DEMOS THAN SCREEN                       
*                                                                               
*- WORKING FROM BACK OF LINE, CLEAR OUT PARTIAL DEMO CODES & LAST COMMA         
DMN200   EQU   *                                                                
         LA    RE,L14ADEMS-1+L'L14ADEMS      LAST SCREEN FLD BYTE               
         LA    RF,1                                                             
         LA    R0,L'L14ADEMS                                                    
         LA    R6,0                                                             
DMN220   CLI   0(RE),C','          THE TRAILING COMMA?                          
         BNE   DMN240                                                           
         LA    RF,0                                                             
DMN240   EQU   *                                                                
         STC   R6,0(RE)            0 TRAILING GARBAGE                           
         SR    RE,RF               BACK UP 1                                    
         LTR   RF,RF                                                            
         BZ    DMN300              STOP                                         
         BCT   R0,DMN220                                                        
*                                                                               
*- POINT TO NEXT SCREEN LINE                                                    
DMN300   EQU   *                                                                
         DROP  R2                                                               
         FOUT  (R2)                TRANSMIT THIS LINE                           
         ZIC   R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
*                                                                               
*- INSERT BLANK LINE AFTER DEMO VALUES (IF ROOM)                                
         CLI   0(R2),0                                                          
         BE    DMN30               ALREADY AT END OF SCREEN                     
         FOUT  (R2)                                                             
         ZIC   R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         CLI   0(R2),0                                                          
         BE    DMN30               LAST LINE                                    
         BAS   RE,CLRLINE                                                       
         B     DMN30               READ NEXT                                    
         EJECT                                                                  
*                                                                               
*- DISPLAY STANDARD COMMENT RECORDS                                             
*                                                                               
CMT10    MVC   SAVEKEY,KEY                                                      
*                                                                               
*- 1ST TIME IN OR NEXT?                                                         
         CLI   NEXTBYTE,0                                                       
         BNE   CMT20               NEXT                                         
*                                                                               
*- BUILD STARTING KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'2E'           KEY ID                                       
         MVC   KEY+15(2),REPALPHA                                               
         MVC   KEY+17(2),=X'FFFF'                                               
         MVC   KEY+19(8),INFSTRT  OPTIONAL STARTING POINT                       
         OC    KEY+19(8),SPACES                                                 
         MVC   SAVEKEY,KEY                                                      
*                                                                               
*- READ KEY                                                                     
CMT20    BAS   RE,HIGH                                                          
         B     CMT40                                                            
*                                                                               
*- READ NEXT KEY                                                                
CMT30    BAS   RE,SEQ                                                           
*                                                                               
*- CHECK FOR END OF DATA                                                        
CMT40    CLC   KEY(17),SAVEKEY     = THRU REP CODE?                             
         BNE   SETDONE                                                          
*                                                                               
         TM    KEY+27,X'80'        DELETED?                                     
         BO    CMT30                                                            
*                                                                               
         MVC   SAVEKEY,KEY         SAVE NEW KEY                                 
*                                                                               
         CLI   0(R2),0             END OF SCREEN?                               
         BE    SETNEXT             YES.  GET OUT.                               
*                                                                               
         BAS   RE,GETREC           READ IN RECORD                               
*                                                                               
*- MOVE RECORD CODE & 1ST NON-BLANK COMMENT LINE TO SCREEN                      
         USING LINE15,R2                                                        
         MVC   L15CODE,RCMTKCDE    COMMENT CODE                                 
*                                                                               
         LA    R6,RCMTREC                                                       
         MVI   ELCODE,X'02'        COMMENT ELEMENTS                             
         BAS   RE,GETEL                                                         
         BNZ   CMT60               RECORD IS EMPTY                              
*                                                                               
*- FIND 1ST NON-BLANK ELEMENT                                                   
CMT50    CLI   1(R6),3             WE HAVE TEXT IF > 3                          
         BH    CMT70                                                            
*                                                                               
         CLI   2(R6),C' '          BLANK ELEMENT?                               
         BNE   CMT70               NO. WE HAVE TEXT.                            
*                                                                               
         BAS   RE,NEXTEL                                                        
         BZ    CMT50               CHECK FOR BLANKS                             
*                                                                               
*- RECORD IS EMPTY.  SAY SO.                                                    
CMT60    LA    R6,CMTEMPTY         EMPTY LITERAL                                
*                                                                               
*- MOVE OUT COMMENT TEXT.  DO NOT EXCEED FIELD MAX.                             
CMT70    ZIC   RE,1(R6)                                                         
         LA    RF,L'L15TEXT        MAX OUTPUT LEN                               
         CR    RE,RF               TEXT LEN -VS- FIELD SIZE                     
         BNH   CMT80                                                            
         LR    RE,RF               JUST MOVE FOR FLD SIZE                       
*                                                                               
CMT80    BCTR  RE,R0               LESS 1 FOR 'EX'                              
         EX    RE,CMTOUT           TEXT TO SCREEN                               
*                                                                               
*- POINT TO NEXT SCREEN LINE                                                    
         FOUT  (R2)                TRANSMIT THIS LINE                           
         ZIC   R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     CMT30               READ NEXT                                    
*                                                                               
CMTOUT   MVC   L15TEXT(0),2(R6)    COMMENT TEXT TO SCREEN                       
         DROP  R2                                                               
*                                                                               
*- LOOK LIKE A TEXT ELEMENT                                                     
CMTEMPTY DC    X'00',AL1(L'CMTEMPTL)                                            
CMTEMPTL DC    C'** COMMENT RECORD IS EMPTY **'                                 
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*- DISPLAY REP RECORDS                                                          
*                                                                               
REP10    MVC   SAVEKEY,KEY                                                      
*                                                                               
*- 1ST TIME IN OR NEXT?                                                         
         CLI   NEXTBYTE,0                                                       
         BNE   REP20               NEXT                                         
*                                                                               
*- BUILD STARTING KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           KEY ID                                       
         MVC   KEY+25(2),INFSTRT   OPTIONAL STARTING POINT                      
         OC    KEY+25(2),SPACES                                                 
         MVC   SAVEKEY,KEY                                                      
*                                                                               
*- READ KEY                                                                     
REP20    BAS   RE,HIGH                                                          
         B     REP40                                                            
*                                                                               
*- READ NEXT KEY                                                                
REP30    BAS   RE,SEQ                                                           
*                                                                               
*- CHECK FOR END OF DATA                                                        
REP40    CLI   KEY,X'01'           END OF REP KEYS?                             
         BNE   SETDONE                                                          
*                                                                               
         TM    KEY+27,X'80'        DELETED?                                     
         BO    REP30                                                            
*                                                                               
         MVC   SAVEKEY,KEY         SAVE NEW KEY                                 
*                                                                               
         CLI   0(R2),0             END OF SCREEN?                               
         BE    SETNEXT             YES.  GET OUT.                               
*                                                                               
         BAS   RE,GETREC           READ IN RECORD                               
*                                                                               
         CLI   FLTRBYTE,0          NO PARENT/MASTER REP FILTER?                 
         BE    REP45                                                            
         CLI   FLTRBYTE,X'FF'      PARENT FILTER?                               
         BNE   REP43                                                            
         CLC   RREPPAR,MEDFLTR     CHECK AGAINST FILTER                         
         BNE   REP30                                                            
         B     REP45                                                            
REP43    CLI   FLTRBYTE,X'80'      MASTER FILTER?                               
         BNE   REP45                                                            
         CLC   RREPMAST,MEDFLTR    CHECK AGAINST FILTER                         
         BE    REP45                                                            
         CLC   RREPMAST,=X'FFFF'   IS THIS A MASTER?                            
         BNE   REP30                                                            
         CLC   RREPKREP,MEDFLTR    IF MASTER, CHECK AGAINST FILTER              
         BNE   REP30                                                            
*                                                                               
*- BUILD LINE FOR DISPLAY                                                       
         USING LINE16,R2                                                        
REP45    MVC   L16ID,RREPKREP      REP ID                                       
         MVC   L16NAME,RREPNAME    FULL REP NAME                                
         MVC   L16SHRT,RREPSHRT    SHORT NAME                                   
         MVC   L16PRNT,RREPPAR     PARRENT REP                                  
*                                                                               
*- POINT TO NEXT SCREEN LINE                                                    
         FOUT  (R2)                TRANSMIT THIS LINE                           
         ZIC   R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     REP30               READ NEXT                                    
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*- POINT PERSON RECORDS                                                         
*                                                                               
POIN0020 MVC   SAVEKEY,KEY                                                      
*                                                                               
*- 1ST TIME IN OR NEXT?                                                         
         CLI   NEXTBYTE,0                                                       
         BNE   POIN0040            NEXT                                         
*                                                                               
*- BUILD STARTING KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'31'           KEY ID                                       
         MVC   KEY+22(2),REPALPHA                                               
         MVC   KEY+24(3),INFSTRT   OPTIONAL STARTING POINT                      
         MVC   SAVEKEY,KEY                                                      
*                                                                               
*- READ KEY                                                                     
POIN0040 BAS   RE,HIGH                                                          
         B     POIN0080                                                         
*                                                                               
*- READ NEXT KEY                                                                
POIN0060 BAS   RE,SEQ                                                           
*                                                                               
*- CHECK FOR END OF DATA                                                        
POIN0080 CLC   KEY(24),KEYSAVE     = THRU REP?                                  
         BNE   SETDONE                                                          
*                                                                               
         TM    KEY+27,X'80'        DELETED?                                     
         BO    POIN0060                                                         
*                                                                               
         MVC   SAVEKEY,KEY         SAVE NEW KEY                                 
*                                                                               
         CLI   0(R2),0             END OF SCREEN?                               
         BE    SETNEXT             YES.  GET OUT.                               
*                                                                               
         BAS   RE,GETREC           READ IN RECORD                               
*                                                                               
*- BUILD LINE FOR DISPLAY                                                       
         USING LINE17,R2                                                        
         MVC   L17CODE,RPTPKREC    PERSON CODE                                  
         MVC   L17NAME,RPTPNAME    NAME                                         
         MVC   L17FONE,RPTPFONE    PHONE                                        
         MVC   L17REP,RPTPREP      OPTIONAL REP                                 
         MVC   L17OFF,RPTPOFF      OFFICE                                       
         CLI   OPTNBYTE,25         EDI OPTION?                                  
         BE    POIN0120            YES                                          
         MVC   L17SPERS,RPTPSPER   SALESPERSON                                  
         OC    RPTPLDAT,RPTPLDAT   ANY LEAVE DATE?                              
         BZ    POIN0260            NO  - ACTIVE                                 
         OC    ACFLTR,ACFLTR       ACCEPT ALL POINTPERSONS?                     
         BNZ   POIN0100            YES - DON'T CHECK LEAVE DATE                 
         GOTO1 VDATCON,DMCB,(5,WORK),(2,WORK)                                   
*                                  NO  - GET TODAY'S DATE (COMPRESSED)          
         CLC   RPTPLDAT,WORK       LEAVE DATE VS TODAY'S DATE                   
         BNH   POIN0380            GONE - DON'T DISPLAY IT                      
POIN0100 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(2,RPTPLDAT),(5,L17LDATE)                           
*                                  LEAVE DATE                                   
         B     POIN0260                                                         
POIN0120 EQU   *                                                                
         OC    RPTPLDAT,RPTPLDAT   ANY LEAVE DATE?                              
         BZ    POIN0140            NO  - ACTIVE                                 
         OC    ACFLTR,ACFLTR       ACCEPT ALL POINTPERSONS?                     
         BNZ   POIN0140            YES - DON'T CHECK LEAVE DATE                 
         GOTO1 VDATCON,DMCB,(5,WORK),(2,WORK)                                   
*                                  NO  - GET TODAY'S DATE (COMPRESSED)          
         CLC   RPTPLDAT,WORK       LEAVE DATE VS TODAY'S DATE                   
         BNH   POIN0380            GONE - DON'T DISPLAY IT                      
POIN0140 EQU   *                                                                
         MVC   L17EDI,=C'NO '      SET EDI USE  = NO                            
         TM    RPTPFLG,X'20'       BLOCK EDI?                                   
         BO    POIN0160            YES                                          
         MVC   L17EDI,=C'YES'      SET EDI USE  = YES                           
POIN0160 EQU   *                                                                
         MVC   L17FAX,=C'NO '      SET FAX PREF = NO                            
         LA    RF,RPTPELEM                                                      
POIN0180 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    POIN0240            YES                                          
         CLI   0(RF),X'21'         FAX ELEMENT?                                 
         BE    POIN0200            YES                                          
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     POIN0180                                                         
POIN0200 EQU   *                                                                
         CLC   RPTPFXFX-RPTPFXEM(12,RF),SPACES                                  
*                                  ANY VALUE IN FAX NUMBER?                     
         BNH   POIN0220            NO  - DON'T DISPLAY                          
         MVC   L17FAX#,RPTPFXFX-RPTPFXEM(RF)                                    
*                                  YES - MOVE TO DISPLAY                        
POIN0220 EQU   *                                                                
         TM    RPTPFXFG-RPTPFXEM(RF),X'20'                                      
*                                  FAX PREFERENCE SET TO YES?                   
         BNO   POIN0240            NO                                           
         MVC   L17FAX,=C'YES'      SET FAX PREF = YES                           
POIN0240 EQU   *                                                                
         B     POIN0360            SKIP FETCH OF SP NAME                        
POIN0260 EQU   *                                                                
*                                                                               
*- GET SALESPERSON NAME                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           KEY ID                                       
         MVC   KEY+22(2),REPALPHA  LOAD DEFAULT REP                             
         OC    RPTPREP,RPTPREP     ANY OPTIONAL REP?                            
         BZ    POIN0280                                                         
         MVC   KEY+22(2),RPTPREP   OPTIONAL REP                                 
POIN0280 EQU   *                                                                
         MVC   KEY+24(3),RPTPSPER  SALESMAN INITIALS                            
         BAS   RE,HIGH                                                          
*                                                                               
POIN0300 CLC   KEY(27),KEYSAVE                                                  
         BNE   POIN0340                                                         
         TM    KEY+27,X'80'        DELETED?                                     
         BZ    POIN0320                                                         
         BAS   RE,SEQ                                                           
         B     POIN0300                                                         
*                                                                               
POIN0320 BAS   RE,GETREC           READ IN RECORD                               
         MVC   L17SNAME,RSALNAME   SALESPERSON NAME                             
*                                                                               
POIN0340 MVC   KEY,SAVEKEY         REESTABLISH SEQ ORDER                        
         BAS   RE,HIGH                                                          
*                                                                               
*- POINT TO NEXT SCREEN LINE                                                    
POIN0360 FOUT  (R2)                TRANSMIT THIS LINE                           
         ZIC   R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
POIN0380 EQU   *                                                                
         BAS   RE,CLRLINE                                                       
         B     POIN0060            READ NEXT                                    
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*- CONTRACT TYPE RECORDS                                                        
*                                                                               
CTY10    MVC   SAVEKEY,KEY                                                      
*                                                                               
*- 1ST TIME IN OR NEXT?                                                         
         CLI   NEXTBYTE,0                                                       
         BNE   CTY20               NEXT                                         
*                                                                               
*- BUILD STARTING KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'32'           KEY ID                                       
         MVC   KEY+24(2),REPALPHA                                               
         MVC   KEY+26(1),INFSTRT   OPTIONAL STARTING POINT                      
         MVC   SAVEKEY,KEY                                                      
*                                                                               
*- READ KEY                                                                     
CTY20    BAS   RE,HIGH                                                          
         B     CTY40                                                            
*                                                                               
*- READ NEXT KEY                                                                
CTY30    BAS   RE,SEQ                                                           
*                                                                               
*- CHECK FOR END OF DATA                                                        
CTY40    CLC   KEY(26),KEYSAVE     = THRU REP?                                  
         BNE   SETDONE                                                          
*                                                                               
         TM    KEY+27,X'80'        DELETED?                                     
         BO    CTY30                                                            
*                                                                               
         MVC   SAVEKEY,KEY         SAVE NEW KEY                                 
*                                                                               
         CLI   0(R2),0             END OF SCREEN?                               
         BE    SETNEXT             YES.  GET OUT.                               
*                                                                               
         BAS   RE,GETREC           READ IN RECORD                               
*                                                                               
*- BUILD LINE FOR DISPLAY                                                       
         USING LINE18,R2                                                        
         MVC   L18CODE(1),RCTYKCTY PERSON CODE                                  
         MVC   L18DESC,RCTYDESC    NAME                                         
*                                                                               
         LA    R6,RCTYREC                                                       
         MVI   ELCODE,X'EF'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL                                                         
         BNZ   CTY60               NOT THERE?                                   
*                                                                               
         USING RCTYAELM,R6                                                      
         GOTO1 VDATCON,DMCB,(3,RCTYA1ST),(5,L18ADD)  CREATION DATE              
         GOTO1 VDATCON,DMCB,(3,RCTYALST),(5,L18CHA)  LAST ACTIVITY              
         DROP  R6                                                               
*                                                                               
*- POINT TO NEXT SCREEN LINE                                                    
CTY60    FOUT  (R2)                TRANSMIT THIS LINE                           
         ZIC   R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     CTY30               READ NEXT                                    
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*- DEVELOPMENTAL SALESPERSON RECORDS                                            
*                                                                               
DSP10    MVC   SAVEKEY,KEY                                                      
*                                                                               
*- 1ST TIME IN OR NEXT?                                                         
         CLI   NEXTBYTE,0                                                       
         BNE   DSP20               NEXT                                         
*                                                                               
*- BUILD STARTING KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'3A'           KEY ID                                       
         MVC   KEY+22(2),REPALPHA                                               
         MVC   KEY+24(3),INFSTRT   OPTIONAL STARTING POINT                      
         MVC   SAVEKEY,KEY                                                      
*                                                                               
*- READ KEY                                                                     
DSP20    BAS   RE,HIGH                                                          
         B     DSP40                                                            
*                                                                               
*- READ NEXT KEY                                                                
DSP30    BAS   RE,SEQ                                                           
*                                                                               
*- CHECK FOR END OF DATA                                                        
DSP40    CLC   KEY(24),KEYSAVE     = THRU REP?                                  
         BNE   SETDONE                                                          
*                                                                               
         TM    KEY+27,X'80'        DELETED?                                     
         BO    DSP30                                                            
*                                                                               
         MVC   SAVEKEY,KEY         SAVE NEW KEY                                 
*                                                                               
         CLI   0(R2),0             END OF SCREEN?                               
         BE    SETNEXT             YES.  GET OUT.                               
*                                                                               
         BAS   RE,GETREC           READ IN RECORD                               
*                                                                               
*- BUILD LINE FOR DISPLAY                                                       
         USING LINE23,R2                                                        
         MVC   L23CODE,RDSPKSAL    DEV S/P NAME                                 
         MVC   L23NAME,RDSPNAME    NAME                                         
         MVC   L23TELE,RDSPTEL     TELEPHONE NUMBER                             
         MVC   L23FAX,RDSPFAX      FAX NUMBER                                   
         OC    RDSPLEAV,RDSPLEAV   ANY LEAVE DATE?                              
         BZ    DSP60               NO  - ACTIVE                                 
         OC    ACFLTR,ACFLTR       ACCEPT ALL DEV S/PS?                         
         BNZ   DSP50               YES - DON'T CHECK LEAVE DATE                 
         GOTO1 VDATCON,DMCB,(5,WORK),(2,WORK)                                   
*                                  NO  - GET TODAY'S DATE (COMPRESSED)          
         CLC   RDSPLEAV,WORK       LEAVE DATE VS TODAY'S DATE                   
         BNH   DSP80               GONE - DON'T DISPLAY IT                      
DSP50    EQU   *                                                                
         GOTO1 VDATCON,DMCB,(2,RDSPLEAV),(5,L23LDATE)                           
*                                                                               
*- POINT TO NEXT SCREEN LINE                                                    
DSP60    FOUT  (R2)                TRANSMIT THIS LINE                           
         ZIC   R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
DSP80    EQU   *                                                                
         BAS   RE,CLRLINE                                                       
         B     DSP30               READ NEXT                                    
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*- DEVELOPMENTAL CONTRACT TYPE RECORDS                                          
*                                                                               
DCT10    MVC   SAVEKEY,KEY                                                      
*                                                                               
*- 1ST TIME IN OR NEXT?                                                         
         CLI   NEXTBYTE,0                                                       
         BNE   DCT20               NEXT                                         
*                                                                               
*- BUILD STARTING KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'3B'           KEY ID                                       
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),INFSTRT   OPTIONAL STARTING POINT                      
         MVC   SAVEKEY,KEY                                                      
*                                                                               
*- READ KEY                                                                     
DCT20    BAS   RE,HIGH                                                          
         B     DCT40                                                            
*                                                                               
*- READ NEXT KEY                                                                
DCT30    BAS   RE,SEQ                                                           
*                                                                               
*- CHECK FOR END OF DATA                                                        
DCT40    CLC   KEY(25),KEYSAVE     = THRU REP?                                  
         BNE   SETDONE                                                          
*                                                                               
         TM    KEY+27,X'80'        DELETED?                                     
         BO    DCT30                                                            
*                                                                               
         MVC   SAVEKEY,KEY         SAVE NEW KEY                                 
*                                                                               
         CLI   0(R2),0             END OF SCREEN?                               
         BE    SETNEXT             YES.  GET OUT.                               
*                                                                               
         BAS   RE,GETREC           READ IN RECORD                               
*                                                                               
*- BUILD LINE FOR DISPLAY                                                       
         USING LINE22,R2                                                        
         MVC   L22CODE,RDCTKCTY    DEV CON TYPE CODE                            
         MVC   L22DESC,RDCTDESC                 NAME                            
*                                                                               
*- POINT TO NEXT SCREEN LINE                                                    
DCT60    FOUT  (R2)                TRANSMIT THIS LINE                           
         ZIC   R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     DCT30               READ NEXT                                    
         DROP  R2                                                               
         EJECT                                                                  
CLRLINE  SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LTR   R5,R5                                                            
         BZ    *+8                                                              
         SH    R5,=H'9'                                                         
         EX    R5,OCLINE                                                        
         BZR   RE                                                               
         EX    R5,XCLINE                                                        
         FOUT  (R2)                                                             
         BR    RE                                                               
*                                                                               
XCLINE   XC    8(0,R2),8(R2)                                                    
*                                                                               
OCLINE   OC    8(0,R2),8(R2)                                                    
         SPACE 3                                                                
NOREC    LA    R2,INFTITLH                                                      
NOREC2   BAS   RE,CLRLINE                                                       
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         CLI   0(R2),0                                                          
         BNE   NOREC2                                                           
*                                                                               
         LA    R2,INFRCRDH                                                      
         BAS   RE,INVAL                                                         
         MVC   INFMESS(L'MSG3),MSG3                                             
         FOUT  INFMESSH                                                         
         B     EXIT                                                             
         SPACE 2                                                                
FLOAT    OI    0(R8),C' '                                                       
         CLI   0(R8),C' '                                                       
         BNE   FLOAT2                                                           
         BCT   R8,FLOAT                                                         
*                                                                               
FLOAT2   LA    R8,2(R8)                                                         
         BR    RE                                                               
         EJECT                                                                  
INVAL    NI    INFRCRDH+4,X'DF'                                                 
         NI    INFSTRTH+4,X'DF'                                                 
         NI    INFFLTRH+4,X'DF'                                                 
         NI    INFOPTNH+4,X'DF'                                                 
         MVI   NEXTBYTE,0                                                       
         BR    RE                                                               
         SPACE 2                                                                
SETNEXT  MVI   NEXTBYTE,1                                                       
         MVC   INFMESS(L'MSG2),MSG2                                             
         FOUT  INFMESSH                                                         
         LA    R2,INFNEXTH                                                      
         B     EXIT                                                             
         SPACE 2                                                                
SETDONE  FOUT  (R2)                                                             
         OC    INFOUT,INFOUT                                                    
         BZ    NOREC                                                            
         BAS   RE,INVAL                                                         
         MVC   INFMESS(L'MSG1),MSG1                                             
         FOUT  INFMESSH                                                         
*                                  CLEAR REST OF PAGE                           
DONE10   SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         CLI   0(R2),0                                                          
         BE    DONE20                                                           
         BAS   RE,CLRLINE                                                       
         B     DONE10                                                           
*                                                                               
DONE20   EQU   *                                                                
         LA    R2,INFRCRDH                                                      
         B     EXIT                                                             
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
         EJECT                                                                  
*                                                                               
CLRKTAB  NTR1                                                                   
         LHI   R3,NUMENTSK                                                      
         LA    R4,KEYTAB                                                        
CLR10    XC    0(L'KEYTAB,R4),0(R4)                                             
         LA    R4,L'KEYTAB(R4)                                                  
         BCT   R3,CLR10                                                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  BUILD KEY ENTRY FOR PRODUCT RECORD                                           
*                                                                               
BLDPRDK  NTR1                                                                   
         MVC   0(L'RPRDKADV,R1),RPRDKADV  GET ADVERTISER CODE                   
*                                                                               
         LHI   R3,4                                                             
BLDPRD10 CLI   0(R1),X'40'        LOOK FOR A SPACE CHAR                         
         BE    BLDPRD20           IF FOUND,GET PROD CODE                        
         LA    R1,1(R1)           ELSE,BUMP TO NEXT CHAR                        
         BCT   R3,BLDPRD10        AND REPEAT LOOP                               
*                                                                               
BLDPRD20 MVI   0(R1),C'-'                     INSERT HYPHEN                     
         MVC   1(L'RPRDKPRD,R1),RPRDKPRD      INSERT PROD CODE                  
         XIT1                                                                   
*                                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
MSG1     DC    C'ACTION COMPLETED - ENTER NEXT REQUEST'                         
MSG2     DC    C'MORE RECORDS AVAILABLE - HIT ''ENTER'' TO CONTINUE'            
MSG3     DC    C'NO RECORDS ON FILE - ENTER NEXT REQUEST'                       
SPACES   DC    CL80' '                                                          
ALL      DC    5X'00'                                                           
ELCODE   DC    1X'00'                                                           
       ++INCLUDE REINFWRK                                                       
DEMOD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042REINF02   10/07/04'                                      
         END                                                                    
