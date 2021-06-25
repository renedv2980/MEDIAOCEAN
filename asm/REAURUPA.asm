*          DATA SET REAURUPA   AT LEVEL 020 AS OF 07/07/14                      
*PHASE REAURUAA,*                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE BUFFAHI                                                                
*INCLUDE COVAIL                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE REPVALMN                                                               
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REAURUP - REPPAK AVERAGE UNIT RATE UPDATE'                      
********************************************************************            
*   HISTORY OF CHANGES                                             *            
********************************************************************            
*   MAY19/93 (BU ) --- ORIGINAL ENTRY                              *            
*                                                                  *            
*   JUN01/93 (BU ) --- ADD 'DISK RECOVERY FILE' OPTION FOR TESTING *            
*                                                                  *            
*   JUL20/93 (BU ) --- ADD DUMP OF INPUT RECOVERY FILE RECORDS     *            
*                                                                  *            
*   SEP27/93 (BU ) --- FIX XS SPOTS/$$ ADDED TO FILE               *            
*                                                                  *            
*   SEP30/93 (BU ) --- ADJUST BASIS FOR CHANGE TOTALS              *            
*                                                                  *            
*   NOV16/95 (BU ) --- INCREASE ALL IO AREAS TO 2048C              *            
*                                                                  *            
*   JUL23/97 (BU ) --- FIX DIVIDE BUG                              *            
*                                                                  *            
*   MAY03/99 (AST) --- DON'T PUT OUT AGENCY AND OFFICE RAURKTYPES  *            
*                      FOR '2C' RECORDS                            *            
*                                                                  *            
*   NOV29/06 (BU ) --- DMOPEN FOR UPDATE                           *            
*                                                                  *            
*   JUL03/14 (YYUN)--- INSERT MEXIT IN MAIN0130 FOR XBASE FIX      *            
*                  --- BUG FIX FOR ZICM  RF,(RE),2 TO 0(RE)        *            
*                                                                  *            
*                                                                  *            
*                      ***  END TOMBSTONE  ***                     *            
********************************************************************            
*  HELPER DUMPS AVAILABLE: (DUMPID=XXXXXXXXXX IN RUNSTREAM)        *            
*                  RECOVERY FILE RECORDS                           *            
*   INPUT        - DISPLAY ENTIRE RECORD                           *            
*   INPUTDMP     - DUMP BEFORE FIRST RECORD DISPLAY                *            
*   INPUTKEY     - DISPLAY KEYS ONLY                               *            
*                                                                  *            
*   BUFF1        - PUT OUT TYPE(S) 1          BUFFALO OUTPUT RECS  *            
*   BUFF12       - PUT OUT TYPE(S) 1,2        BUFFALO OUTPUT RECS  *            
*   BUFF123      - PUT OUT TYPE(S) 1,2,3      BUFFALO OUTPUT RECS  *            
*   BUFF1234     - PUT OUT TYPE(S) 1,2,3,4    BUFFALO OUTPUT RECS  *            
*   BUFB         - PUT OUT BUFFALO RETURN RECORDS                  *            
*   SORTBACK     - PUT OUT RETURN FROM SORT                        *            
*   SETCNTRT                                                       *            
*   INSETCNT                                                       *            
*   OUTPUT       - PUT OUT RECORDS ADDED/CHANGED ON FILE           *            
*   REGCOM       -                                                 *            
*   BUFFSORT     - PUT OUT TYPE 1 BUFFALO, RETURN FROM SORT .      *            
*   SPECIAL      - SAME AS BUFF1+BUFFSORT+OUTPUT                   *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
REAURUP  CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NBASE 0,REAURUP,VREGSAVE,R9,RA                                         
****     LA    RC,2048(RB)                                                      
****     LA    RC,2048(RC)                                                      
****     LA    RA,2048(RC)                                                      
****     LA    RA,2048(RA)                                                      
****     USING REAURUP+4096,RC,RA                                               
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
         XC    WORK,WORK                                                        
***      ST    RC,WORK             START DUMP AT WORKSPACE                      
         ST    RB,WORK             START DUMP AT WORKSPACE                      
         L     R4,=V(STXITER)                                                   
         ST    R4,WORK+4                                                        
         OI    WORK+4,X'80'                                                     
         GOTO1 =V(STXITER),DMCB,WORK                                            
*                                                                               
         GOTO1 =A(INITIAL),DMCB,(RC)                                            
*                                                                               
*   OPEN CONTROL SYSTEM TO ACCESS CONTROL FILE IDENTIFICATION                   
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',AIO,0                                              
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL FILE ID RECORD                  
         MVC   WORK+15(10),SAVNAME LOAD AGENCY NAME                             
         OC    WORK+15(10),SPACES  SET REMAINDER TO SPACES                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIO                  
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R1,AIO                                                           
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
INPU0140 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   INPU0150            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    INPU0160            YES                                          
INPU0150 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   INPU0140            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
INPU0160 EQU   *                                                                
         MVC   UTL+4(1),3(R1)      OVERRIDE CONTROL FILE UTL                    
*                                     WITH REP UTL CODE                         
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'REP',FLIST,AIO                    
*                                                                               
         MVI   DATADISP+1,34       ACCESS DEFAULT SDD RECORD                    
         XC    KEY,KEY                                                          
         MVI   KEY+RSDDKTYP-RSDDKEY,X'26'                                       
         MVC   KEY+RSDDKREP-RSDDKEY(2),REP                                      
         MVC   KEY+RSDDKSTA-RSDDKEY(4),=4XL1'FF'                                
*                                  INSERT DEFAULT STATION                       
         GOTO1 HIGH                                                             
         CLC   KEY(26),KEYSAVE     DEFAULT FOUND?                               
         BNE   INPU0180            NO                                           
         GOTO1 GETREC              YES - RETRIEVE IT                            
*                                                                               
*   IF TWO DEFAULTS ARE SET UP, TV IS FOLLOWED BY RADIO.                        
*                                                                               
         CLI   KEY+26,C' '         IS IT TV?                                    
         BNE   INPU0170            NO  - PROCESS RADIO                          
         L     R6,AIO              YES - STORE IT AWAY                          
         LA    R2,DEFSDDT                                                       
         BAS   RE,SVSDD            SET UP TV DEFAULT                            
*                                                                               
         GOTO1 SEQ                 READ NEXT RECORD                             
         CLC   KEY(26),KEYSAVE     RADIO DEFAULT FOUND?                         
         BNE   INPU0180            NO  -                                        
         GOTO1 GETREC              YES - RETRIEVEIT                             
INPU0170 LA    R2,DEFSDDR                                                       
         L     R6,AIO                                                           
         BAS   RE,SVSDD            SET UP RADIO DEFAULT                         
*                                                                               
INPU0180 MVI   FRST,C'Y'                                                        
         EJECT                                                                  
*                                                                               
*      READ DAILY RECOVERY TAPE                                                 
*                                                                               
RECO0010 DS    0H                                                               
         BAS   RE,GETRCV           GET RECOVERY FILE RECORD                     
         BNZ   MAIN0010            NON-ZERO = END OF FILE                       
*                                  SET 2X'00' AT EOR                            
         L     RE,AIO              SET A(IO AREA)                               
         LR    R2,RE                                                            
         AH    RE,0(R2)            POINT TO END OF REC                          
         XC    0(2,RE),0(RE)       CLEAR ELCODE AND LEN                         
*                                                                               
         LA    R2,4(R2)                                                         
         USING RECVHDR,R2                                                       
*                                                                               
         CLI   RFILTY,X'81'        DIRECTORY POINTER?                           
         BE    RECO0010            YES - SKIP RECORD                            
*                                                                               
         LA    R6,24(R2)                                                        
         L     R7,AIO2                                                          
         USING SORTRECD,R7                                                      
         XC    0(43,R7),0(R7)      CLEAR KEY AREA                               
         CLC   0(2,R6),=X'0C00'    CONTRACT RECORD?                             
         BNE   RECO0060            NO  - CHECK FOR BUY                          
         CLC   =C'INPUT',DUMPID                                                 
         BNE   RECO0012                                                         
         MVC   P(14),=C'INPUT CONTRACT'                                         
         MVC   P+17(27),0(R6)      LOAD KEY                                     
         GOTO1 =V(PRINTER)                                                      
         CLC   =C'INPUTKEY',DUMPID                                              
         BE    RECO0012            ONLY DUMP KEYS                               
         XC    FULL,FULL           CLEAR INTERMEDIATE AREA                      
         MVC   FULL+2(2),27(R6)    LOAD LENGTH                                  
         L     R3,FULL             SET LENGTH                                   
         CLC   =C'INPUTDMP',DUMPID                                              
         BNE   *+6                                                              
         DC    H'0'                TEST DUMP                                    
         LA    R3,24(R3)           ADD LENGTH OF RECV HDR                       
         GOTO1 =V(PRNTBL),DMCB,(0,(R2)),(R2),C'DUMP',(R3),=C'2D'                
*                                  DUMP WITH CONTROL HEADER                     
*                                                                               
RECO0012 EQU   *                                                                
         USING RCONKEY,R6                                                       
         CLC   RCONKREP,REP        YES - CORRECT REP?                           
         BNE   RECO0010            NO - SKIP IT                                 
*                                                                               
*                                                                               
         OC    MED,MED             ANY MEDIA ENTERED?                           
         BZ    RECO0020            NO                                           
         CLI   MED,C'T'            YES - IS IT TV?                              
         BNE   *+16                NO  -                                        
         CLI   RCONKSTA+4,C' '     YES - IS CONTRACT TV?                        
         BNE   RECO0010            NO  - SKIP IT                                
         B     *+12                YES                                          
*                                                                               
         CLI   RCONKSTA+4,C' '     NOT TV - IS CONTRACT TV?                     
         BE    RECO0010            YES - SKIP IT                                
*                                                                               
RECO0020 EQU   *                                                                
****     OC    STAGEFIL,STAGEFIL   ANY STATION END FILTER?                      
****     BZ    RECO0030            NO                                           
****     CLC   RCONKGRP(7),STAGEFIL                                             
****     BH    RECO0010                                                         
*                                                                               
RECO0030 OC    STAGFIL,STAGFIL     ANY STATION START FILTER?                    
         BZ    *+14                NO                                           
         CLC   RCONKGRP(7),STAGFIL                                              
         BL    RECO0010                                                         
*                                                                               
RECO0040 CLI   RRECTY,COPY         IS IT A COPY RECORD?                         
         BNE   RECO0050            NO                                           
*                                                                               
*   FOR COPY RECORD, SAVE 'OLD' VALUES IN ORIGINAL CONTRACT RECORD              
*       COPY IS ALWAYS FOLLOWED BY A CHANGE RECORD, DETAILING NEW               
*         VALUES FOR THESE FIELDS.                                              
*                                                                               
         MVC   SVOTYPE,RCONTYPE    SAVE CONTRACT TYPE                           
         MVC   SVOOFF,RCONKOFF     SAVE CONTRACT OFFICE                         
         MVC   SVOAGY,RCONKAGY     SAVE CONTRACT AGENCY/AGY OFF                 
         MVC   SVOGSTA,RCONKGRP    SAVE CONTRACT GROUP/STATION                  
         B     RECO0010                                                         
*                                                                               
RECO0050 ZIC   RE,RRECTY           EXTRACT RECOVERY REC TYPE                    
         LA    RE,1(RE)            SET SORT:  ADD=1 COPY=2 CHG=3                
         STC   RE,SRACT                                                         
         CLI   RRECTY,3            ADD ?                                        
         BNE   *+8                                                              
         MVI   SRACT,SRTADDE       SORT ADD FIRST                               
*                                                                               
         LR    R4,R6                                                            
         BAS   RE,SETCONSR                                                      
         B     RECO0010                                                         
*                                                                               
*                                                                               
*                                                                               
RECO0060 CLI   0(R6),X'0B'         IS IT A BUY RECORD?                          
         BNE   RECO0010            NO  - SKIP IT                                
         CLC   =C'INPUT',DUMPID                                                 
         BNE   RECO0062                                                         
         MVC   P(09),=C'INPUT BUY'                                              
         MVC   P+17(27),0(R6)                                                   
         GOTO1 =V(PRINTER)                                                      
         CLC   =C'INPUTKEY',DUMPID                                              
         BE   RECO0062             ONLY DUMP KEYS                               
         MVC   FULL+2(2),27(R6)    LOAD LENGTH                                  
         L     R3,FULL             SET LENGTH                                   
         GOTO1 =V(PRNTBL),DMCB,(0,(R6)),(R6),C'DUMP',(R3),=C'2D'                
*                                                                               
RECO0062 EQU   *                                                                
         USING RBUYKEY,R6                                                       
         CLC   RBUYKREP,REP        CORRECT REP?                                 
         BNE   RECO0010            NO - SKIP IT                                 
         CLC   =XL2'FFFF',RBUYKMLN PLAN RECORD?                                 
         BE    RECO0010            YES - SKIP IT                                
*                                                                               
         LA    R1,CNLSTTAB         CONTRACT IN TABLE?                           
         LA    R0,CNLSTTBX         END OF LIST                                  
RECO0070 OC    0(4,R1),0(R1)       ANY ENTRY?                                   
         BZ    RECO0080            NO  - FINISHED                               
         CLC   0(4,R1),RBUYKCON    CONTRACT VS TABLE                            
         BNE   *+14                NOT FOUND                                    
         MVC   SRSTAG(7),4(R1)     FOUND - LOAD GROUP/STATION                   
         B     RECO0090                                                         
*                                                                               
         LA    R1,11(R1)           BUMP TABLE ENTRY                             
         CR    R0,R1               END OF TABLE?                                
         BH    RECO0070            NO  - GO BACK FOR NEXT                       
*                                                                               
*                                  CONTRACT NOT IN TABLE - MUST BE              
*                                     RETRIEVED FROM FILE                       
RECO0080 EQU   *                                                                
         XC    KEY,KEY             SET CONTRACT KEY FOR READ                    
         MVI   KEY,X'8C'                                                        
         MVC   KEY+(RCONPREP-RCONKEY)(2),REP                                    
         PACK  KEY+23(1),RBUYKCON+3(1)                                          
         PACK  KEY+24(1),RBUYKCON+2(1)                                          
         PACK  KEY+25(1),RBUYKCON+1(1)                                          
         PACK  KEY+26(1),RBUYKCON(1)                                            
         GOTO1 HIGH                RETRIEVE KEY                                 
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BNE   RECO0010            NO  - SKIP IT                                
*                                                                               
*   SHOULDN'T REALLY HAPPEN THAT A RECORD IS SKIPPED.  PREVIOUSLY,              
*        THIS CAUSED A DUMP.                                                    
*                                                                               
         MVC   AIO,AIO3            RESET A(IO AREA)                             
         BAS   RE,GETREC           RETRIEVE THIS CONTRACT                       
         MVC   AIO,AIO1            RESET A(IO AREA) AGAIN                       
         L     R4,AIO3                                                          
         MVC   SRSTAG,RCONKGRP-RCONKEY(R4)                                      
*                                  SET GROUP/STATION                            
*                                                                               
*   DETERMINE IF CONTRACT JUST READ AGREES WITH FILTERS                         
*     ENTERED, IF ANY                                                           
*                                                                               
         OC    MED,MED             FILTER ON MEDIA, IF ENTERED                  
         BZ    RECO0082                                                         
         CLI   MED,C'T'                                                         
         BNE   *+16                                                             
         CLI   SRSTAG+6,C' '                                                    
         BNE   RECO0010            FAILS FILTERING:  SKIP IT                    
         B     *+12                                                             
*                                                                               
         CLI   SRSTAG+6,C' '                                                    
         BE    RECO0010            FAILS FILTERING: SKIP IT                     
*                                                                               
RECO0082 EQU   *                                                                
****     OC    STAGEFIL,STAGEFIL   FILTER ON END STATION, IF ENTERED            
****     BZ    RECO0084                                                         
****     CLC   SRSTAG,STAGEFIL                                                  
****     BH    RECO0010            FAILS FILTERING: SKIP IT                     
*                                                                               
RECO0084 EQU   *                                                                
         OC    STAGFIL,STAGFIL     FILTER ON START STATION, IF ENTERED          
         BZ    *+14                                                             
         CLC   SRSTAG,STAGFIL                                                   
         BL    RECO0010            FAILS FILTERING: SKIP IT                     
*                                                                               
         MVI   SRACT,MYCON         SET 'I READ CONTRACT' FLAG                   
         BAS   RE,SETCONSR         GENERATE A CONTRACT SORT RECORD              
*                                                                               
RECO0090 EQU   *                                                                
         OC    MED,MED             FILTER ON MEDIA, IF ENTERED                  
         BZ    RECO0100                                                         
         CLI   MED,C'T'                                                         
         BNE   *+16                                                             
         CLI   SRSTAG+6,C' '                                                    
         BNE   RECO0010                                                         
         B     *+12                                                             
*                                                                               
         CLI   SRSTAG+6,C' '                                                    
         BE    RECO0010                                                         
*                                                                               
RECO0100 EQU   *                                                                
****     OC    STAGEFIL,STAGEFIL   FILTER ON END STATION, IF ENTERED            
****     BZ    RECO0110                                                         
****     CLC   SRSTAG,STAGEFIL                                                  
****     BH    RECO0010                                                         
*                                                                               
RECO0110 EQU   *                                                                
         OC    STAGFIL,STAGFIL     FILTER ON START STATION, IF ENTERED          
         BZ    *+14                                                             
         CLC   SRSTAG,STAGFIL                                                   
         BL    RECO0010                                                         
*                                                                               
         MVC   SRCON#,RBUYKCON     SET UP BUY SORT RECORD                       
         MVI   SRREC,X'02'         BUY RECORD                                   
         MVC   SRACT,RRECTY        INSERT ACTION TYPE                           
         MVC   SRBTYP,RBUYTYP      INSERT BUY TYPE                              
         MVC   SRBDUR,RBUYDUR      INSERT DURATION                              
         MVC   SRBNW,RBUYNW        INSERT NUMBER PER WEEK                       
         MVC   SRBCOS,RBUYCOS      INSERT COST                                  
         MVC   SRBCNTL,RBUYCNTL    INSERT BUY CONTROL BYTE                      
         MVI   SRBCOMBO,C'N'       SET COMBO FLAG TO 'NO'                       
         TM    RBUYRTS,X'40'       IS BUY A COMBO?                              
         BNO   RECO0112            NO                                           
         MVI   SRBCOMBO,C'Y'       SET COMBO FLAG TO 'YES'                      
RECO0112 EQU   *                                                                
         MVI   SRBPLN,C'N'         SET PLAN TO 'NO'                             
         CLC   RBUYKPLN,=3X'FF'    PLAN BUY?                                    
         BE    *+8                 NO                                           
         MVI   SRBPLN,C'Y'         YES - SET PLAN TO 'YES'                      
         MVC   SRLEN(2),=H'32'     INSERT BUY RECORD LENGTH                     
         L     RE,RECCNT           INCREMENT COUNTER                            
         LA    RE,1(RE)                                                         
         ST    RE,RECCNT                                                        
         STCM  RE,15,SRNUM         KEEP COPY/CHANGE CONSECUTIVE                 
*                                                                               
         LA    R5,SRBDTM           A(BUY DAY/TIME ELEMENTS)                     
         LR    R4,R6               SAVE A(BUY RECORD)                           
         MVI   ELCODE,X'02'        RETRIEVE DAY/TIME ELEMENT                    
RECO0120 BAS   RE,GETEL            GET FIRST                                    
         B     *+8                                                              
RECO0130 BAS   RE,NEXTEL           GET NEXT                                     
         BNE   RECO0140            NOT FOUND - FINISHED DAY/TIME ELTS           
         SR    RE,RE                                                            
         ICM   RE,3,SRLEN          L(SORT RECORD)                               
         ZIC   RF,1(R6)            L(D/T ELT)                                   
         AR    RE,RF               ADD L(D/T ELT) TO L(SORT RECORD)             
         STCM  RE,3,SRLEN          RESET L(SORT RECORD)                         
         BCTR  RF,0                DECREMENT FOR 'EX' STATEMENT                 
         EX    RF,MVCELEM          MOVE D/T ELT TO SORT RECORD                  
         AR    R5,RF               BUMP TO NEXT AVAILABLE POSITION              
         LA    R5,1(R5)               ADD 1 PREVIOUSLY SUBTRACTED               
         B     RECO0130            GO BACK FOR NEXT D/T ELT                     
*                                                                               
RECO0140 CLI   ELCODE,X'07'        WAS CREDIT ELEMENT SOUGHT?                   
         BE    RECO0150            YES - FINISHED                               
         LR    R6,R4               RESET A(BUY RECORD)                          
         CLI   ELCODE,X'02'        WAS D/T ELT SOUGHT?                          
         BNE   *+12                NO                                           
         MVI   ELCODE,X'03'        YES - SET FOR EFF DATE ELTS                  
         B     RECO0120            GO BACK AND ADD EFF DATE ELTS                
         CLI   ELCODE,X'03'        WAS EFF DATE ELT SOUGHT?                     
         BNE   *+12                NO                                           
         MVI   ELCODE,X'06'        YES - SET FOR BUY MISSED ELTS                
         B     RECO0120            GO BACK AND ADD THEM                         
         MVI   ELCODE,X'07'        SET FOR CREDIT ELTS                          
         B     RECO0120            GO BACK AND ADD THEM                         
*                                                                               
RECO0150 AP    INCNTB,=P'1'        INCREMENT COUNTER                            
         CLC   =C'SORTOUT',DUMPID                                               
         BNE   RECO0155                                                         
         MVC   P(13),=C'SORTOUT: RECO'                                          
         GOTO1 =V(PRINTER)                                                      
         ZICM  R3,SRLEN,2          SET LENGTH                                   
         GOTO1 =V(PRNTBL),DMCB,(0,(R7)),(R7),C'DUMP',(R3),=C'2D'                
*                                                                               
RECO0155 EQU   *                                                                
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRLEN                                    
*                                  RELEASE RECORD TO SORT                       
         B     RECO0010            GO BACK AND READ NEXT RECOV RECD             
RECO0160 DS    0H                                                               
*                                                                               
MVCELEM  MVC   0(0,R5),0(R6)                                                    
         DROP  R2,R6,R7                                                         
         EJECT                                                                  
*                                                                               
*   GETRCV:  RETURN A BUY RECORD FROM THE RECOVERY FILE IN                      
*        THE RECORD RECOVERY AREA                                               
*                                                                               
GETRCV   NTR1                                                                   
*                                                                               
*                                                                               
         L     RE,AIO                     CLEAR THE RCVREC AREA                 
         LA    RF,1280             L(IO AREA 1)                                 
         XCEFL                                                                  
*                                                                               
         CLI   RECOVERY,C'T'       RECOVERY SOURCE = TAPE?                      
         BNE   GRCV1               NO  - RETRIEVE DISK RECORD                   
         BAS   RE,GETTAPE          YES - RETRIEVE TAPE FILE RECORD              
         BZ    GRCVGOOD            RECORD FOUND                                 
         B     GRCVBAD             END OF FILE                                  
*                                                                               
GRCV1    EQU   *                                                                
         OC    ARCVREC,ARCVREC     FIRST PASS?                                  
         BNZ   GRCV20              NO                                           
*                                                                               
GRCV10   EQU   *                   YES - BRING IN BLOCK                         
         L     RE,ARCVBLK          A(IO AREA FOR RECOVERY BLOCK)                
         LR    RF,RE               SAVE A(BLOCK)                                
         LA    RF,6(RF)                                                         
         ST    RF,ARCVREC          A(FIRST RECORD IN BLOCK)                     
         L     RF,=A(RCVBLKX-RCVBLK)                                            
*                                  L(BLOCK)                                     
         XCEFL                                                                  
         L     R0,ARCVBLK          SET A(IO AREA)                               
         GET   RECDSKIN,(R0)       RETRIEVE RECOVERY FILE BLOCK                 
         L     RF,RECCOUNT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,RECCOUNT                                                      
         L     R2,ARCVREC          SET A(RECORD IN PROGRESS)                    
         B     GRCV30                                                           
*                                                                               
GRCV20   EQU   *                                                                
         L     R2,ARCVREC          SET A(RECORD IN PROGRESS)                    
         ZICM  R3,0(R2),2          RECORD LENGTH                                
         AR    R2,R3               A(NEXT RECORD)                               
         OC    0(2,R2),0(R2)       ANY DATA?                                    
         BZ    GRCV10              NO  - GET NEXT BLOCK                         
         ST    R2,ARCVREC          YES - SET A(RECORD IN PROGRESS)              
*                                                                               
GRCV30   EQU   *                                                                
         CLI   RFILTY-RECVHDR+2(R2),X'82'    REP FILE?                          
         BNE   GRCV20                                                           
*                                                                               
*   NOTE:  TO GET THE TAPE AND DISK RECORD AREAS IN SYNCH, THE                  
*     FOLLOWING ADJUSTMENT IS MADE FOR DISK RECORDS:                            
*        1.  THE DISK RECORD IS LOADED FROM THE FIRST BYTE, AND                 
*            IT IS OFFSET TWO BYTES INTO THE IO AREA.                           
*        2.  THE LENGTH HALF-WORD, WHICH IS ORIGINALLY IN AIO+2,                
*            IS MOVED UP TO AIO.                                                
*        3.  THE LENGTH IS INCREASED BY TWO BYTES, TO TAKE INTO                 
*            ACCOUNT THE TWO-BYTE SHIFT IN THE WORK AREA.                       
*                                                                               
         ZICM  R1,0(R2),2          L(RECORD IN BLOCK)                           
         LA    RE,0(R2)            A(START OF RECORD IN BLOCK)                  
         L     RF,AIO              AREA TO LOAD RECORD                          
         LA    RF,2(RF)            BUMP DOWN TWO BYTES                          
         MOVE  ((RF),(R1)),(RE)    MOVE RECORD TO WORK AREA                     
         PRINT GEN                                                              
         L     RE,AIO              SET A(IO AREA)                               
         LA    RE,2(RE)            BUMP DOWN TWO BYTES                          
         ZICM  RF,0(RE),2          TAKE ORIGINAL LENGTH                         
         LA    RF,2(RF)            ADD TWO BYTES FOR LENGTH SHIFT               
         L     RE,AIO              RESET A(IO AREA)                             
         STCM  RF,3,0(RE)          INSERT INTO HIGH-ORDER                       
         PRINT NOGEN                                                            
*                                                                               
GRCVGOOD EQU   *                                                                
         SR    R0,R0               SET CC = ZERO:  GOOD RETURN                  
         B     GRCVEXIT                                                         
GRCVBAD  EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO: BAD RETURN                
GRCVEXIT EQU   *                                                                
         XIT1                                                                   
RECDEOD  EQU   *                   END OF FILE FOUND                            
         B     GRCVBAD                                                          
         EJECT                                                                  
*                                                                               
*        GETTAPE --- GET A TAPE RECOVERY RECORD                                 
*                                                                               
GETTAPE  NTR1                                                                   
*                                                                               
         L     R2,AIO              A(IO AREA TO USE)                            
         GET   RECVIN,(R2)                                                      
*                                                                               
GTAPGOOD EQU   *                                                                
         SR    R0,R0               SET CC = ZERO:  GOOD RETURN                  
         B     GTAPEXIT                                                         
GTAPBAD  EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO:  BAD RETURN               
         SPACE 3                                                                
GTAPEXIT EQU   *                                                                
         XIT1                                                                   
RTAPEEOD EQU   *                                                                
         B     GTAPBAD             END OF TAPE                                  
*                                                                               
*   TAPE END-OF-DATA:  SINGLE PASS BRINGS IN ONE OR MULTI TAPES                 
         SPACE                                                                  
         EJECT                                                                  
*******************************************************************             
*                                                                 *             
*            -----     MAIN LINE CODE     -----                   *             
*                                                                 *             
*            -----       END = MAIN0130   -----                   *             
*                                                                 *             
* READ SORT FILE:  1ST ANALYZE CONTRACT CHANGES:                  *             
*   SET CHANGE AREAS TO TRIGGER TOTAL CONTRACT # CHANGE -         *             
*      READ ALL BUYS FOR CONTRACT                                 *             
*      PUT BUFFALO RECORDS WITH NEG/POS SPOTS/DOLLARS             *             
*   IF NO TOTAL CHANGE                                            *             
*      READ BUYS FROM SORT TO CREATE BUFFALO RECORDS WITH NEW     *             
*           AMOUNTS.                                              *             
*      ON STATION BREAK, RETURN BUFFALO RECORDS BACK.             *             
*           RETRIEVE AUR RECORDS AND MAKE CHANGES.                *             
*      CYCLE THROUGH ALL STATIONS.                                *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
MAIN0010 EQU   *                                                                
         CLI   RECOVERY,C'T'       RECOVERY SOURCE = TAPE?                      
         BNE   MAIN0012            NO  - CLOSE RECOVERY DISK                    
         CLOSE RECVIN              YES - CLOSE RECOVERY TAPE                    
         B     MAIN0016                                                         
MAIN0012 EQU   *                                                                
         CLOSE RECDSKIN            CLOSE RECOVERY DISK                          
MAIN0016 EQU   *                                                                
         MVC   P(9),=C'CONTRACTS'  DISPLAY SOME CONTROL INFO                    
         EDIT  (P4,INCNTC),(7,P+11)                                             
         MVC   P+20(4),=C'BUYS'                                                 
         EDIT  (P4,INCNTB),(7,P+26)                                             
         GOTO1 =V(PRINTER)                                                      
         SPACE                                                                  
MAIN0020 GOTO1 =V(SORTER),DMCB,=C'GET'                                          
*                                  RETURN SORT RECORD                           
         ICM   R2,15,4(R1)         EOF REACHED?                                 
         BZ    MAIN0130            YES                                          
*                                                                               
* MOVE RECORD *                                                                 
         L     R7,AIO1             SET A(IO AREA)                               
         USING SORTRECD,R7                                                      
         LH    RE,0(R2)            SET RECORD LENGTH                            
MAIN0030 CH    RE,=H'256'          MOVE RECORD TO STORAGE AREA                  
         BNH   MAIN0040               IN SEGMENTS                               
         MVC   0(256,R7),0(R2)                                                  
         LA    R7,256(R7)                                                       
         LA    R2,256(R2)                                                       
         SH    RE,=H'256'                                                       
         B     MAIN0030                                                         
MAIN0040 BCTR  RE,0                                                             
         EX    RE,MVCREC           MOVE LAST PIECE                              
*                                                                               
         LA    R7,1(RE,R7)         POINT TO END OF REC                          
         XC    0(2,R7),0(R7)       CLEAR NEXT ELEM CODE/LENGTH                  
         L     R7,AIO1                                                          
         CLC   =C'SPECIAL',DUMPID                                               
         BE    MAIN0048                                                         
         CLC   =C'BUFFSORT',DUMPID                                              
         BE    MAIN0048                                                         
         CLC   =C'SORTBACK',DUMPID                                              
         BNE   MAIN0050                                                         
MAIN0048 EQU   *                                                                
         MVC   P(08),=C'SORTBACK'                                               
         GOTO1 =V(PRINTER)                                                      
         LH    R3,0(R7)            SET LENGTH                                   
         GOTO1 =V(PRNTBL),DMCB,(0,(R7)),(R7),C'DUMP',(R3),=C'2D'                
*                                                                               
MAIN0050 EQU   *                                                                
         CLC   REJSTA,SRSTAG       STATION REJECTED?                            
         BE    MAIN0020            YES - GO BACK FOR NEXT                       
         XC    REJSTA,REJSTA       CLEAR REJECTED STATION                       
*                                                                               
         CLI   SRREC,1             CONTRACT RECORD?                             
         BNE   MAIN0100            NO  - DO BUY CHANGES                         
         CLI   FRST,C'Y'           FIRST TIME?                                  
         BE    MAIN0070            YES                                          
*                                                                               
* STA BREAK & NEW CONTRACT #:                                                   
*     ADD/CHANGE AUR RECORDS                                                    
*     PRINT AND CLEAR STATION                                                   
*                                                                               
         CLC   SRSTAG,SVSRKEY+SVGSTAE                                           
*                                  SAME GROUP/STATION?                          
         BE    MAIN0060            YES                                          
         CLI   ABUYSW,C'Y'         DID LAST CONTRACT HAVE BUYS?                 
         BE    *+8                 YES - SKIP CHANGES                           
         BAS   RE,READBUY          NO  - READ ALL BUYS FOR CON#                 
         XC    BUYCHGN,BUYCHGN     CLEAR INDICATOR                              
         GOTO1 =A(CHGAUREC),DMCB,(RC)                                           
*                                  UPDATE AVERAGE UNIT RATE RECS                
         BZ    MAIN0052            NOTHING FOR THIS STATION                     
         GOTO1 =A(PRNTSTA),DMCB,(RC)                                            
         BAS   RE,RSETBUFF         RESET STATION BUFFALO                        
MAIN0052 EQU   *                                                                
         XC    LSTSTA,LSTSTA                                                    
         B     MAIN0070                                                         
*                                                                               
*   NO STATION BREAK BUT NEW CONTRACT #                                         
*                                                                               
MAIN0060 CLC   SRCON#,SVSRKEY+SVCONE                                            
*                                  SAME CONTRACT NUMBER?                        
         BE    MAIN0080            YES                                          
         CLI   ABUYSW,C'Y'         BUY CHANGES?                                 
         BE    *+8                 YES                                          
         BAS   RE,READBUY          TYPE CODE (???) CHANGE                       
*                                     READ ALL BUYS FOR CONTRACT                
         XC    BUYCHGN,BUYCHGN     CLEAR INDICATOR                              
*                                                                               
MAIN0070 MVI   ABUYSW,C'N'         SET BUY SWITCH TO 'NO'                       
         XC    NCDTES,NCDTES                                                    
         MVI   FSTCHG,C'Y'                                                      
         MVI   ADDCNT,C'N'                                                      
MAIN0080 MVC   SVSRKEY,0(R7)                                                    
         BAS   RE,SETCNTRT                                                      
         CLC   =C'SETCNTRT',DUMPID                                              
         BNE   MAIN0090                                                         
         MVC   P(34),FSTCHG                                                     
         GOTO1 =V(HEXOUT),DMCB,NCNTL,P+36,12,=C'T0G'                            
         GOTO1 =V(PRINTER)                                                      
MAIN0090 EQU   *                                                                
         MVI   FRST,C'N'                                                        
         B     MAIN0020                                                         
*                                                                               
*   PROCESS BUYS                                                                
*                                                                               
MAIN0100 CLI   ABUYSW,C'Y'         ANY BUYS?                                    
         BE    *+8                 YES -                                        
         BAS   RE,READBUY          TYPE CODE CHANGE:                            
*                                     READ ALL BUYS FOR CONTRACT                
         ICM   RE,15,BUYCHGN       ANY BUY CHANGES?                             
         BZ    MAIN0110            NO                                           
         CLI   SRACT,3             DON'T PROCESS ADDS                           
         BE    MAIN0120                                                         
         CLC   SRNUM,BUYCHGN                                                    
         BNL   MAIN0120                                                         
         MVC   SVSTA,OGRSTA                                                     
         BAS   RE,GETSDD                                                        
MAIN0110 CLI   SRACT,CHANGE                                                     
         BNE   *+12                                                             
         TM    SRBCNTL,X'80'       RECORD DELETED?                              
         BO    MAIN0120            YES                                          
         MVC   NCNTL,SRBCNTL       NO  -                                        
         MVC   NDUR,SRBDUR         INSERT DURATION                              
         MVC   NTYP,SRBTYP         INSERT TYPE                                  
         MVC   NNW,SRBNW           INSERT NUMBER PER WEEK                       
         MVC   DUB,SRBCOS          WORD ALIGN COST                              
         MVI   PLANSW,C'N'         SET PLAN SWITCH TO 'NO'                      
         CLI   SRBPLN,C'Y'         IS IT A PLAN BUY?                            
         BNE   *+8                 NO                                           
         MVI   PLANSW,C'Y'         YES - SET SWITCH TO 'YES'                    
         LA    R6,SRBDTM           A(1ST D/T ELT)                               
         ST    R6,ADYTMEL          SAVE ADDR                                    
         MVC   COMBOREC,SRBCOMBO   SET COMBO RECORD FLAG                        
         BAS   RE,CALCBUY                                                       
MAIN0120 MVI   ABUYSW,C'Y'         SET 'BUYS' TO 'YES'                          
         B     MAIN0020            GO BACK FOR NEXT SORT REC                    
*                                                                               
MAIN0130 EQU   *                                                                
*        MVC   P(19),=C'EOF ON SORT REACHED'                                    
*                                  **TEST                                       
*        GOTO1 =V(PRINTER)                                                      
*                                  **TEST                                       
         CLI   FRST,C'Y'           ANY UPDATES?                                 
         BNE   MAIN0140            YES                                          
         MVC   P(25),=C'NO A.U.R. RECORDS UPDATED'                              
         GOTO1 =V(PRINTER)                                                      
         B     MEXIT                                                            
         SPACE                                                                  
MAIN0140 CLI   ABUYSW,C'Y'         ANY BUY CHANGES?                             
         BE    *+8                 YES                                          
         BAS   RE,READBUY          TYPE CODE CHANGE:                            
*                                     READ ALL BUYS FOR CONTRACT                
         GOTO1 =A(CHGAUREC),DMCB,(RC)                                           
*                                  UPDATE AVERAGE UNIT RATE RECS                
         BZ    MAIN0150            NOTHING UPDATED                              
         GOTO1 =A(PRNTSTA),DMCB,(RC)                                            
MAIN0150 EQU   *                                                                
         GOTO1 =A(FINALPRT),DMCB,(RC)                                           
*                                                                               
MEXIT    XBASE                                                                  
EXIT     XIT1                                                                   
*                                                                               
MVCREC   MVC   0(0,R7),0(R2)                                                    
         EJECT                                                                  
***********************************************************************         
*  2 TYPES OF CONTRACT RECORDS CHANGE/ADD - 1ST CHG TAKES OLD TYPES   *         
*    & NEW TYPES - ALL OTHER CHANGES JUST NEW TYPES                   *         
*  IF AN ADD NO OLD TYPES (NOT ADDED TO ATHENA FILE UNTIL TODAY)      *         
*  ROUTINE BUILDS MONTH TABLES FROM LAST CON (CON DATES ONLY GROW)    *         
*  READS FOR STATION SDD RECORD / IF STA HAS NO SDD REC SETS REJSTA   *         
***********************************************************************         
         SPACE 2                                                                
SETCNTRT NTR1                                                                   
         OC    NCDTES(6),NCDTES                                                 
         BNZ   *+14                                                             
         MVC   NCDTES(6),SRCDTES   SET UP DATES                                 
         B     SETC0010                                                         
         CLC   NCDTES(3),SRCDTES                                                
         BL    *+10                                                             
         MVC   NCDTES(3),SRCDTES   SET UP LOWEST DATE                           
         CLC   SRCDTES+3(3),NCDTES+3                                            
         BL    *+10                                                             
         MVC   NCDTES+3(3),SRCDTES+3 SET UP HIGHEST DATE                        
SETC0010 EQU   *                                                                
*                                                                               
*   TEST DATES                                                                  
*        MVC   P+1(06),=C'DATES:'                                               
*        MVC   P+10(6),NCDTES                                                   
*        MVC   P+20(6),SRCDTES                                                  
*        GOTO1 =V(PRINTER)                                                      
*   TEST DATES END                                                              
*                                                                               
         CLI   SRACT,MYCON         IF JUST READ FROM FILE NEWEST                
         BNE   SETC0020                                                         
         CLI   FSTCHG,C'Y'                                                      
         BE    SETC0040                                                         
         B     SETC0060            IF CONTRACT NOT CHG DON'T USE TYPES          
SETC0020 CLI   ADDCNT,C'Y'         IF ADDED TODAY NO READ OF BUYS               
         BE    SETC0050                                                         
         CLI   SRACT,SRTADDE                                                    
         BNE   SETC0030                                                         
         MVI   ADDCNT,C'Y'         CONTRACT ADDED TODAY                         
         B     SETC0050                                                         
*                                                                               
* CHANGE NO ADD (ADD ALWAYS 1ST) 1ST ONLY SAVE OLD TYPES                        
SETC0030 CLI   FSTCHG,C'Y'                                                      
         BNE   SETC0050                                                         
         MVI   FSTCHG,C'N'                                                      
SETC0040 MVC   OTYPE,SRCOTYPE      SAVE OLD CONTRACT TYPE                       
         MVC   OOFF,SRCOOFF        SAVE OLD OFFICE                              
         MVC   OAGY,SRCOAGY        SAVE OLD AGENCY                              
*                                                                               
SETC0050 MVC   NTYPE,SRCTYPE       SAVE NEW CONTRACT TYPE                       
         MVC   NOFF,SRCOFF         SAVE NEW OFFICE                              
         MVC   NAGY,SRCAGY         SAVE NEW AGENCY                              
         MVC   NGRSTA,SRSTAG       SAVE NEW GROUP/STATION                       
         CLI   SRACT,4             IF I READ NO CHANGE                          
         BE    SETC0060                                                         
         CLI   SRACT,1             ADD                                          
         BE    SETC0060                                                         
         OC    BUYCHGN,BUYCHGN                                                  
         BNZ   SETC0060                                                         
         CLC   SRCOGSTA,SRSTAG                                                  
         BE    SETC0060                                                         
         MVC   OGRSTA,SRCOGSTA                                                  
         MVC   BUYCHGN,SRCBUYN                                                  
SETC0060 EQU   *                                                                
         CLC   =C'INSETCNT',DUMPID                                              
         BNE   SETC0070                                                         
         GOTO1 =V(HEXOUT),DMCB,OGRSTA,P+22,18,=C'T0G'                           
         GOTO1 =V(PRINTER)                                                      
SETC0070 EQU   *                                                                
         CLC   SVSTA,SVSRKEY+SVGSTAE                                            
         BE    EXIT                                                             
         MVC   SVSTA,SVSRKEY+SVGSTAE                                            
         BAS   RE,GETSDD                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* GET SDD RECORD                                                                
GETSDD   ST    RE,FULL                                                          
         XC    KEY,KEY                                                          
         MVI   KEY+RSDDKTYP-RSDDKEY,X'26'                                       
         MVC   KEY+RSDDKREP-RSDDKEY(2),REP                                      
         MVC   KEY+RSDDKSTA-RSDDKEY(5),SVSTA+2                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     FOUND STATION SDD                            
         BNE   DD100                                                            
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO3                                                          
         LA    R2,STASDDT                                                       
         BAS   RE,SVSDD                                                         
         B     DD200                                                            
         MVC   AIO,AIO1                                                         
*                                                                               
DD100    LA    R2,DEFSDDT                                                       
         CLI   SVSRKEY+SVSTAME,C' '                                             
         BE    *+8                                                              
         LA    R2,DEFSDDR                                                       
         CLI   0(R2),0             DEFAULT SDD                                  
         BNE   DD200                                                            
         MVC   REJSTA,SVSRKEY+SVGSTAE                                           
         MVC   P(7),SVSRKEY+SVGSTAE                                             
         MVC   P+9(18),=C'HAS NO SDD RECORD'                                    
         GOTO1 =V(PRINTER)                                                      
         MVC   ACURSDDT,=X'FFFFFFFF'                                            
*                                  FLAG A(CURRENT SDD RECORD)                   
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
DD200    ST    R2,ACURSDDT                                                      
*                                                                               
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*****************************************************************               
*                                                               *               
*  ROUTINE READS BUYS FOR 1 CONTRACT - STORES 2-8 BUFFALO RECS  *               
*  - 1-4 BUFF RECORDS WITH OLD CONTRACT INFO NEGATED            *               
*  - 1-4 BUFF RECORDS WITH NEW CONTRACT INFO - READS NEXT BUY   *               
*  UNTIL ALL BUYS FOR CONTRACT NUMBER ARE READ                  *               
*                                                               *               
*****************************************************************               
         SPACE                                                                  
READBUY  NTR1                                                                   
         CLC   ACURSDDT,=X'FFFFFFFF'                                            
*                                  ANY ADDRESS?                                 
         BE    RB300               NO  - DON'T PROCESS THIS ORDER               
         BAS   RE,SETCBUCS         SET UP MONTH BUCKETS                         
         ICM   RE,15,BUYCHGN                                                    
         BNZ   RB140                                                            
         MVI   TCDCHGSW,C'N'                                                    
         CLI   ADDCNT,C'Y'         NEW CONTRACT: NO CHANGE                      
         BE    RB300                                                            
         CLC   OTYPE(7),NTYPE      TYPE/OFFICE/AGENCY CHANGE?                   
         BE    RB300                                                            
RB120    EQU   *                                                                
         MVI   TCDCHGSW,C'Y'       CHANGE TO TYPE CODES                         
RB140    EQU   *                                                                
         MVI   RDBUYSW,C'Y'        READING BUYS SWITCH FOR CALCBUY              
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'           SET UP FOR BUYS                              
         MVC   KEY+(RBUYKREP-RBUYKEY)(2),REP                                    
         MVC   KEY+18(4),SVSRKEY+SVCONE CONTRACT NUMBER                         
         GOTO1 HIGH                READ FIRST BUY OF CONTRACT                   
         B     RB220                                                            
*                                                                               
RB200    GOTO1 SEQ                 READ NEXT  BUY OF CONTRACT                   
RB220    CLC   KEY(22),KEYSAVE     SAME KEY?                                    
         BNE   RB300               NO  - FINISHED                               
         CLC   =XL2'FFFF',KEY+25   SKIP PLAN RECORDS                            
         BE    RB200                                                            
         ICM   RE,15,BUYCHGN       SOME INDICATOR TEST???                       
         BZ    *+12                ZERO:  SKIP DELETE TEST                      
         TM    KEY+27,X'80'        SKIP  DELETES                                
         BO    RB200                                                            
         LA    R3,KEY                                                           
*        GOTO1 =V(HEXOUT),DMCB,0(R3),P+10,34,=C'T0G'                            
*        GOTO1 =V(PRINTER)                                                      
         MVC   AIO,AIO2                                                         
         BAS   RE,GETREC           RETRIEVE RECORD INTO IO AREA 2               
         MVC   AIO,AIO1            RESET IO AREA TO 1                           
         L     R6,AIO2             SET COMBO FLAG IF APPROPRIATE                
         LA    R6,34(R6)           POSITION TO DESCRIPTOR ELEMENT               
         MVI   COMBOREC,C'N'       SET COMBO/REG BUY TO 'REG'                   
         TM    RBUYRTS-RBUYELEM(R6),X'40'                                       
         BNO   RB222               OFF = REGULAR BUY                            
         MVI   COMBOREC,C'Y'       ON  = COMBO BUY                              
RB222    EQU   *                                                                
         L     R6,AIO2             SET USING OVER IO AREA                       
         USING RBUYKEY,R6                                                       
         CLI   RBUYCHGI,C'C'       CANCELLED BUY?                               
         BE    RB200               YES - SKIP BUY                               
         CLI   RBUYCHGI+1,C'C'     CANCELLED BUY?                               
         BE    RB200               YES - SKIP BUY                               
         MVC   NCNTL,RBUYCNTL      INSERT NEW CONTROL                           
         MVC   NDUR,RBUYDUR        INSERT NEW LENGTH                            
         MVC   DUB,RBUYCOS         INSERT NEW COST                              
         MVC   NNW,RBUYNW          INSERT NEW NUMBER PER WEEK                   
         MVC   NTYP,RBUYTYP        INSERT NEW CONTRACT TYPE                     
         MVI   PLANSW,C'N'         SET PLAN SWITCH TO 'NO'                      
         CLC   RBUYKPLN,=3X'FF'    PLAN BUY?                                    
         BE    *+8                 NO                                           
         MVI   PLANSW,C'Y'         YES - SET PLAN SWITCH TO 'YES'               
         MVI   ELCODE,X'02'        GET 1ST DAY/TIME ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   RB200               NO 02 ELT:                                   
*                                  THIS IS A BAD RECORD: SKIP IT                
         ST    R6,ADYTMEL          SAVE A(1ST DAY/TIME ELEMENT)                 
*                                                                               
         BAS   RE,CALCBUY          GO CALCULATE THE BUY                         
         B     RB200                                                            
*                                                                               
RB300    MVI   RDBUYSW,C'N'        SET OFF READ BUY SWITCH                      
RBEXT    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*                                                               *               
*  ROUTINE ADDS BUFF RECORDS FOR BUY - MATCH DAY/TIMES TO SDD   *               
*  RECORDS. DOES THIS BY 1ST FINDING A MATCH ON DAY/TIME START  *               
*  (CAN ONLY MATCH TO 1) THAN CHECKS END TIME FOR MATCH         *               
*  YES - READS NEXT ELEMENT, NO ADDS 1 TO SDD END TIME, STORES  *               
*  IN START TIME USE DPT SDD TO MATCH ON DAY/TIME - NO MATCH    *               
*  CREATE  PSUEDO(ZZZ) DPT. AFTER THIS MESS IT READS NEXT ELEM  *               
*  AND STARTS OVER USING MATCHED DPT.                           *               
*  CREATES BUFFALO RECORDS (UP TO 4)                            *               
*                                                               *               
*****************************************************************               
         SPACE                                                                  
CALCBUY  NTR1                                                                   
         XC    BFREC,BFREC         CLEAR BUFF RECORD                            
         MVC   BFSTA,SVSTA         INSERT STATION                               
         MVC   BFSPL,NDUR          SET SPOT LENGTH                              
         CLI   PLANSW,C'Y'         PLAN?                                        
         BNE   CALC0020            NO                                           
         MVC   BFDPT(2),=X'FEFE'   YES - SET BUFF DPT/SUBDPT                    
         MVC   BFSPL,=X'FFFF'      SET BUF SPOT LENGTH                          
         MVI   BFPGT,X'FC'         SET PROG TYPE TO PLAN INDICATOR              
         B     CALC0240                                                         
CALC0020 CLI   NTYP,C' '           ANY PROGRAM TYPE?                            
         BE    CALC0040            NO                                           
         SR    RE,RE               YES - THERE ARE NO DAYPARTS                  
         ICM   RE,1,NTYP           EXTRACT TYPE                                 
         BZ    CALC0040            TYPE IS ZERO -                               
         MVC   BFDPT(2),=X'FEFE'   SET BUFF DPT/SUBDPT                          
         STC   RE,BFPGT            SET PROG TYPE                                
         B     CALC0240                                                         
*                                                                               
CALC0040 MVI   ELCODE,X'02'        GET DAY/TIME ELEM                            
         L     R6,ADYTMEL                                                       
         MVI   WORK+5,C'Y'         SET 1ST TIME SWITCH TO 'YES'                 
         CLI   0(R6),X'02'         DAY/TIME ELEM ADDR                           
         BNE   EXIT                SHOULDN'T HAPPEN - BUT DOES!                 
***      BE    *+6                                                              
***      DC    H'0'                SHOULDN'T HAPPEN                             
         BAS   R5,CHKTIM                                                        
*                                  ROUTINE MAY NOT RETURN HERE....              
         L     R2,ACURSDDT         SET A(CURRENT SDD RECORD)                    
CALC0060 LR    R3,R2               SAVE A(CURRENT SDD RECORD)                   
         MVC   BFDPT(2),0(R2)      SET DAYPART INTO BUFF RECORD                 
CALC0080 ZIC   R5,2(R2)            LOOP CONTROL: # OF TIMES FOR DP              
         LA    R2,3(R2)            SET A(1ST TIME ELT OF DP)                    
CALC0100 ZIC   RE,0(R2)            SET DAY OF D/T STRING                        
         EX    RE,CALC0120         DAY OVERLAP                                  
         B     CALC0140                                                         
CALC0120 TM    3(R6),0             BUY D/T ELT VS SDD REC                       
CALC0140 EQU   *                                                                
         BZ    CALC0160            NO DAYS SET                                  
*                                                                               
*    DAY OVERLAP TEST ENSURES THAT BUY DAYS OVERLAP SDD DAYS.  AN               
*      ADDITIONAL TEST IS REQUIRED TO ENSURE THAT BUY DOESN'T                   
*      EXCEED SDD, AS IN THE CASE WHERE SDD IS M-F, AND BUY IS                  
*      M-SU.  THE OVERLAP TEST PASSES, BUT THE BUY SHOULD NOT                   
*      BE INCLUDED IN M-F.                                                      
*                                                                               
         BAS   RE,CHKDYLAP         DOES BUY CONTAIN MORE DAYS THAN              
*                                     SDD RECORD DAYPART?                       
*                                                                               
         BZ    CALC0160            YES - DOESN'T FIT - SKIP IT                  
         CLC   WORK(2),3(R2)       ELEMENT START TIME > DP END TIME?            
         CLC   WORK(2),3(R2)       ELT START TIME > DP END TIME?                
         BH    CALC0160            YES - SKIP THIS DP ELT                       
         CLC   WORK(2),1(R2)       ELT START TIME >= DP START?                  
         BNL   CALC0180            YES - PROCESS THIS DP ELT                    
*                                  DAY OR TIME NOT MATCHED                      
CALC0160 LA    R2,5(R2)            BUMP TO NEXT SDD D/T STRING                  
         BCT   R5,CALC0100         GO BACK FOR NEXT                             
*                                                                               
         CLI   WORK+5,C'Y'         NO MATCH:  FIRST TIME?                       
         BNE   CALC0220            NO                                           
         CLI   0(R2),0             YES - IS THERE ANOTHER DAYPART?              
         BNE   CALC0060            YES - GO BACK AND CHECK IT                   
         B     CALC0220            NO  - START TIME HAS TO FIT                  
*                                                                               
CALC0180 EQU   *                                                                
*   CONTINUE LOOKING FOR MATCHES:  NO 'FIRST-TIME=NO' SETTING                   
*                                                                               
****>>>> MVI   WORK+5,C'N'         SET 'FIRST TIME' TO NO                       
         OC    WORK+2(2),WORK+2    ANY ELEMENT END TIME?                        
         BZ    CALC0200            NO                                           
         CLC   WORK+2(2),3(R2)     YES - ELT END TIME > DP END TIME?            
         BNH   CALC0200            NO  - THIS IS A MATCH                        
*                                                                               
         SR    RF,RF               BUMP DP END TIME                             
         ICM   RF,3,3(R2)          ADD 1 TO END DATE -> NEW START               
         LA    RF,1(RF)            ADJUST TO NEAREST HOUR(??????)               
         LR    R1,RF                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'          CHECK FOR END OF AN HOUR                     
         CH    RE,=H'60'                                                        
         BNE   *+8                                                              
         AH    R1,=H'40'                                                        
         LR    RF,R1                                                            
         STCM  RF,3,WORK                                                        
         LR    R2,R3                                                            
         B     CALC0080            GO BACK AND CHECK NEW TIME                   
*                                                                               
CALC0200 BAS   RE,NEXTEL           GET BUY'S NEXT D/T ELEMENT                   
         BNE   CALC0240            NO MORE                                      
         BAS   R5,CHKTIM                                                        
         LR    R2,R3                                                            
         B     CALC0080            GO BACK AND CHECK NEW TIME                   
*                                                                               
* CHKTIM SUB ROUTINE MAY RETURN HERE                                            
CALC0220 MVC   BFDPT(2),=X'FFFF'                                                
*                                                                               
CALC0240 L     R6,ADYTMEL          SET BACK TO DAY TIME ELEMENT                 
         LA    R2,MTHTBL                                                        
         MVI   ELCODE,X'03'        GET WEEKS ELEM                               
CALC0260 BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   CALC0460            NO MORE                                      
         MVC   WORK(3),2(R6)       SET START DATE                               
CALC0280 CLC   WORK(3),5(R2)       START DATE VS END DATE                       
         BNH   CALC0300            START <= END                                 
         BAS   RE,SETADDBF         START >  END                                 
*                                                                               
         LA    R2,8(R2)            BUMP TO NEXT DP                              
         OC    0(2,R2),0(R2)       ANY ENTRY?                                   
         BNZ   CALC0280            YES - CHECK IT                               
         MVC   P(24),=C'* DATES OUTSIDE CONTRACT'                               
         L     R6,AIO2                                                          
         GOTO1 =V(HEXOUT),DMCB,0(R6),P+38,20,=C'T0G'                            
         GOTO1 =V(PRINTER)                                                      
         B     CALC0700                                                         
*                                                                               
CALC0300 MVC   BFYM,0(R2)          SET BUFF RECORD DATE                         
         ZIC   RF,NNW              NUMBER PER WEEK TESTING                      
         CLI   1(R6),5                                                          
         BE    CALC0320                                                         
         ZIC   RF,RBUYDTNW-RBUYDTEL(R6)                                         
CALC0320 LR    R1,RF                                                            
         CLI   COMBOREC,C'Y'       COMBO BUY?                                   
         BE    CALC0340            YES - COMBO                                  
         A     RF,BFSPOT1          NO  - ACCUM REGULAR SPOT COUNT               
         ST    RF,BFSPOT1                                                       
         B     CALC0360                                                         
CALC0340 EQU   *                                                                
         A     RF,BFSPOT2          ACCUMULATE COMBO    SPOT COUNT               
         ST    RF,BFSPOT2                                                       
CALC0360 EQU   *                                                                
         ICM   RF,15,DUB                                                        
         BZ    CALC0420                                                         
         CLI   BFPGT,X'FC'         IF PLAN ITS TOTAL COST                       
         BNE   CALC0380                                                         
         LA    R1,1                                                             
CALC0380 LR    RF,R1                                                            
         M     RE,DUB                                                           
         LTR   RF,RF                                                            
         BM    *+12                                                             
         AH    RF,=H'50'                                                        
         B     *+8                                                              
         SH    RF,=H'50'                                                        
         D     RE,=F'100'                                                       
         CLI   COMBOREC,C'Y'       COMBO BUY?                                   
         BE    CALC0400            YES - COMBO                                  
         A     RF,BFDOLS1          NO  - ACCUM REGULAR SPOT $$                  
         ST    RF,BFDOLS1                                                       
         B     CALC0420                                                         
CALC0400 EQU   *                                                                
         A     RF,BFDOLS2          ACCUMULATE COMBO    SPOT $$                  
         ST    RF,BFDOLS2                                                       
*                                                                               
CALC0420 CLI   1(R6),5             NO END DATE - NEXT ELEM                      
         BE    CALC0260                                                         
         LA    R5,7                1 WEEK                                       
         TM    8(R6),X'40'                                                      
         BNO   *+8                                                              
         LA    R5,14               2 WEEKS                                      
         ZIC   RE,WORK+2                                                        
         AR    RE,R5                                                            
         CH    RE,=H'28'                                                        
         BH    *+12                                                             
         STC   RE,WORK+2           LT 28 ADD 7/14 TO DAYS                       
         B     CALC0440                                                         
*                                                                               
         GOTO1 =V(DATCON),DMCB,(3,WORK),(0,WORK+6)                              
         GOTO1 =V(ADDAY),(R1),WORK+6,WORK+12,(R5)                               
         GOTO1 =V(DATCON),(R1),(0,WORK+12),(3,WORK)                             
CALC0440 CLC   WORK(3),5(R6)                                                    
         BH    CALC0260                                                         
         B     CALC0280                                                         
*                                                                               
CALC0460 BAS   RE,SETADDBF                                                      
*                                                                               
* LOOK FOR MAKEGOOD/MISSED ELEMENTS                                             
*                                                                               
CALC0480 MVI   ELCODE,X'06'        GET WEEKS ELEM                               
CALC0500 L     R6,ADYTMEL                                                       
CALC0520 BAS   RE,NEXTEL                                                        
         BNE   CALC0680                                                         
*                                                                               
         ZIC   RF,RBUYMSSP-RBUYMSEL(R6)                                         
         TM    NCNTL,X'80'                                                      
         BO    *+6                                                              
         LCR   RF,RF                                                            
         CLI   COMBOREC,C'Y'       COMBO BUY?                                   
         BE    CALC0540            YES                                          
         ST    RF,BFSPOT1          NO  - ACCUM REGULAR SPOTS                    
         B     CALC0560                                                         
CALC0540 EQU   *                                                                
         ST    RF,BFSPOT2          ACCUMULATE COMBO    SPOTS                    
CALC0560 EQU   *                                                                
         CLI   ELCODE,X'07'        MISSED $$ ALREADY ADDED IN                   
         BE    CALC0620                                                         
         CLI   BFPGT,X'FC'         PLAN?                                        
         BE    CALC0620            YES - USE TOTAL COST                         
         M     RE,DUB                                                           
         LTR   RF,RF                                                            
         BM    *+12                                                             
         AH    RF,=H'50'                                                        
         B     *+8                                                              
         SH    RF,=H'50'                                                        
         D     RE,=F'100'                                                       
         CLI   COMBOREC,C'Y'       COMBO BUY?                                   
         BE    CALC0580            YES                                          
         ST    RF,BFDOLS1          NO  - ACCUM REGULAR $$                       
         B     CALC0600                                                         
CALC0580 EQU   *                                                                
         ST    RF,BFDOLS2          ACCUMULATE COMBO    $$                       
CALC0600 EQU   *                                                                
*                                                                               
CALC0620 LA    R2,MTHTBL                                                        
CALC0640 CLC   2(3,R6),5(R2)                                                    
         BNH   CALC0660            END BEFORE START                             
         LA    R2,8(R2)                                                         
         OC    0(2,R2),0(R2)                                                    
         BNZ   CALC0640                                                         
         DC    H'0'                                                             
CALC0660 MVC   BFYM,0(R2)                                                       
         BAS   RE,SETADDBF                                                      
         B     CALC0520                                                         
*                                                                               
CALC0680 CLI   ELCODE,X'07'                                                     
         BE    CALC0700                                                         
         MVI   ELCODE,X'07'                                                     
         B     CALC0500                                                         
*                                                                               
CALC0700 B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   CHKTIM:  INSPECT TIME STRINGS                                               
*        **** ROUTINE RETURNS TO BC500 IF NONE OR VARIOUS TIMES ****            
*                                                                               
CHKTIM   MVC   WORK(4),4(R6)       START/END TIME                               
*        GOTO1 =V(HEXOUT),DMCB,WORK,P+40,4,=C'T0G'                              
         CLC   =C'NONE',WORK                                                    
         BE    CALC0220                                                         
         CLC   =C'VARY',WORK                                                    
         BE    CALC0220                                                         
         CLC   =C'CC',WORK+2                                                    
         BNE   *+10                                                             
         MVC   WORK+2(2),=H'2600'                                               
         CLC   WORK(2),=H'600'                                                  
         BNL   CHKT0040                                                         
         CLC   WORK(2),=H'500'                                                  
         BL    CHKT0020                                                         
         MVC   WORK(2),=H'600'     5A - 6A                                      
         CLC   WORK+2(2),=H'559'                                                
         BH    CHKT0080                                                         
         CLC   WORK+2(2),=H'501'                                                
         BL    CHKT0040                                                         
         MVC   WORK+2(2),=H'600'   5A = 6A                                      
         B     CHKT0080                                                         
CHKT0020 CLC   WORK(2),=H'201'                                                  
         BL    CHKT0040                                                         
         MVC   WORK(2),=H'200'     2A - 459                                     
CHKT0040 CLC   WORK+2(2),=H'600'                                                
         BNL   CHKT0080                                                         
         CLC   WORK+2(2),=H'201'                                                
         BL    CHKT0080                                                         
         CLC   WORK+2(2),=H'500'                                                
         BH    CHKT0060                                                         
         MVC   WORK+2(2),=H'200'   201A - 459 = 2A                              
         B     CHKT0080                                                         
CHKT0060 MVC   WORK+2(2),=H'600'   5A = 6A                                      
CHKT0080 CLC   WORK(2),WORK+2                                                   
         BNE   *+10                                                             
         XC    WORK+2(2),WORK+2                                                 
         LA    R1,WORK                                                          
         BAS   RE,ADJTIME                                                       
*        GOTO1 =V(HEXOUT),DMCB,WORK,P+20,4,=C'T0G'                              
*        GOTO1 =V(PRINTER)                                                      
         CLC   =H'2600',WORK+2     LEAVE 2A ALONE                               
         BE    CHKT0100                                                         
         SR    RE,RE                                                            
         ICM   RE,3,WORK+2                                                      
         BZ    CHKT0120                                                         
         SH    RE,=H'1'            ADJUST END TIME                              
         STCM  RE,3,WORK+2                                                      
CHKT0100 CLC   WORK(2),WORK+2      START GT END NO MATCH                        
         BH    CALC0220                                                         
CHKT0120 BR    R5                                                               
         EJECT                                                                  
*                                                                               
*   CHKDYLAP:  SDD DAYS OVERLAP BUY DAYS.  HOWEVER, IF BUY EXCEEDS              
*        DAYS SPECIFIED IN SDD, THE MATCH SHOULD BE REJECTED.                   
*        0(R2)  =  SDD DAYS                                                     
*        3(R6)  =  BUY DAYS                                                     
*     BUY DAY MASK AND SDD DAY MASK ARE LOADED INTO REGISTERS, THEN             
*        SHIFTED TO HIGH ORDER.  THE BUY MASK IS CHECKED TO SEE IF              
*        DAY IS SPECIFIED, BY CHECKING REGISTER AS NEGATIVE.  IF SO,            
*        THE CORRESPONDING BIT IN THE SDD MASK MUST BE ON.  IF IT IS            
*        NOT, THERE IS NO AGREEMENT, AND IT IS CONSIDERED NOT TO BE             
*        A MATCH.                                                               
*                                                                               
CHKDYLAP NTR1                                                                   
         ZIC   RE,0(R2)            LOAD SDD DAYS INTO REGISTER                  
         SLL   RE,28               SHIFT TO HIGH ORDER BYTE                     
         ZIC   RF,3(R6)            LOAD BUY DAYS INTO REGISTER                  
         SLL   RF,28               SHIFT TO HIGH ORDER BYTE                     
         LA    R0,8                LOOP CONTROL                                 
*                                                                               
CHKD0010 EQU   *                                                                
         LTR   RF,RF               CHECK BUY DAYS BIT FOR 'ON'                  
         BNM   CHKD0030            REG NOT NEGATIVE:  BIT OFF                   
         LTR   RE,RE               REG NEGATIVE: BIT ON                         
*                                     IS DAY IN SDD MASK?                       
         BNM   CHKD0040            REG NOT NEGATIVE:  BIT OFF                   
*                                     NOT ALLOWED - SKIP IT                     
CHKD0030 EQU   *                                                                
         SLL   RE,1                SHIFT REGISTER TO NEXT BIT                   
         SLL   RF,1                SHIFT REGISTER TO NEXT BIT                   
         BCT   R0,CHKD0010         GO BACK AND CHECK NEXT BIT                   
         LTR   RB,RB               SET CC NOT = ZERO FOR                        
*                                     GOOD RETURN                               
         B     CHKD0060                                                         
CHKD0040 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO FOR                            
*                                     ERROR RETURN                              
CHKD0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*      SET TO ADD TO BUFFALO                                                    
*                                                                               
SETADDBF NTR1                                                                   
         CLI   RDBUYSW,C'Y'        BUYS READ?                                   
         BNE   SETA0040            NO                                           
         MVC   TYP(09),NTYPE       YES - INSERT CON TYPE/OFF/AGENCY             
         BAS   RE,ADTOBUFF         ADD TO NEW RECORDS                           
         ICM   RE,15,BUYCHGN                                                    
         BNZ   SETA0020                                                         
*                                                                               
         MVC   TYP(09),OTYPE                                                    
         L     RE,BFSPOT1          COMPLEMENT REGULAR VALUES FOR BUY            
         LCR   RE,RE                                                            
         ST    RE,BFSPOT1                                                       
         L     RE,BFDOLS1                                                       
         LCR   RE,RE                                                            
         ST    RE,BFDOLS1                                                       
         L     RE,BFSPOT2          COMPLEMENT COMBO   VALUES FOR BUY            
         LCR   RE,RE                                                            
         ST    RE,BFSPOT2                                                       
         L     RE,BFDOLS2                                                       
         LCR   RE,RE                                                            
         ST    RE,BFDOLS2                                                       
         BAS   RE,ADTOBUFF         SUBTRACT FROM OLD RECORDS                    
SETA0020 XC    BFYM,BFYM                                                        
         XC    BFSPOT1(16),BFSPOT1                                              
         B     SETA0140                                                         
*                                                                               
SETA0040 CLI   TCDCHGSW,C'Y'       CHANGE TO TYPE CODES                         
         BNE   SETA0060                                                         
         CLI   SRACT,COPY          IGONORE CHANGE/ADD RECORDS                   
         BE    SETA0080                                                         
         B     SETA0100            ADD TO OLD RECORDS                           
*                                                                               
* NO TYPE CODE CHANGES                                                          
SETA0060 CLI   SRACT,COPY          COPY RECORDS SUBTRACT FROM OLD               
         BE    SETA0080                                                         
         MVC   TYP(09),NTYPE                                                    
         B     SETA0120            ADD TO NEW RECORDS                           
*                                                                               
SETA0080 L     RE,BFDOLS1                                                       
         LCR   RE,RE                                                            
         ST    RE,BFDOLS1                                                       
         L     RE,BFSPOT1                                                       
         LCR   RE,RE                                                            
         ST    RE,BFSPOT1                                                       
         L     RE,BFDOLS2                                                       
         LCR   RE,RE                                                            
         ST    RE,BFDOLS2                                                       
         L     RE,BFSPOT2                                                       
         LCR   RE,RE                                                            
         ST    RE,BFSPOT2                                                       
SETA0100 MVC   TYP(09),OTYPE                                                    
SETA0120 BAS   RE,ADTOBUFF         SUBTRACT FROM OLD RECORDS                    
SETA0140 B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* PUT UP TO 4 BUFFALO RECORDS                                                   
ADTOBUFF NTR1                                                                   
         OC    BFYM,BFYM                                                        
         BZ    AB500                                                            
         MVI   BFTYP,1                                                          
         XC    BFTYPC,BFTYPC                                                    
         BAS   RE,PUTBUFF                                                       
         LA    R4,BFREC                                                         
         CLC   =C'SPECIAL',DUMPID  DUMP BUFF RECORD?                            
         BE    AB005               NO                                           
         CLC   =C'BUFF1',DUMPID    DUMP BUFF RECORD?                            
         BE    AB005               NO                                           
         CLC   =C'BUFFSORT',DUMPID DUMP BUFF RECORD?                            
         BNE   AB010               NO                                           
AB005    EQU   *                                                                
         MVC   P(08),=C'BUFFALO:'                                               
         GOTO1 =V(PRINTER)                                                      
         LA    R3,BFLENGTH                                                      
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R3),=C'2D'                
AB010    EQU   *                                                                
*                                                                               
         OC    TYP,TYP             ANY CONTRACT TYPE?                           
         BZ    AB100               NO                                           
         MVI   BFTYP,2                                                          
         MVC   BFTYPC(1),TYP       INSERT CONTRACT TYPE                         
         BAS   RE,PUTBUFF                                                       
         LA    R4,BFREC                                                         
         CLC   =C'BUFF12',DUMPID   DUMP BUFF RECORD?                            
         BNE   AB100               NO                                           
         MVC   P(08),=C'BUFFALO:'                                               
         GOTO1 =V(PRINTER)                                                      
         LA    R3,BFLENGTH                                                      
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R3),=C'2D'                
*                                                                               
AB100    OC    AGY,AGY             ANY AGENCY?                                  
         BZ    AB200                                                            
*        MVI   BFTYP,3                                                          
*        MVC   BFTYPC(6),AGY       INSERT AGENCY                                
*        OC    AGY+4(2),AGY+4      OFFICE BINARY ZEROS?                         
*        BNZ   AB120               YES                                          
*        MVC   BFTYPC+4(2),SPACES  SPACE FILL AGENCY OFFICE                     
*B120    EQU   *                                                                
*        BAS   RE,PUTBUFF                                                       
*        LA    R4,BFREC                                                         
*        CLC   =C'BUFF123',DUMPID  DUMP BUFF RECORD?                            
*        BNE   AB200               NO                                           
*        MVC   P(08),=C'BUFFALO:'                                               
*        GOTO1 =V(PRINTER)                                                      
*        LA    R3,BFLENGTH                                                      
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R3),=C'2D'                
*                                                                               
AB200    OC    OFF,OFF             ANY OFFICE?                                  
         BZ    AB500                                                            
*        MVI   BFTYP,4                                                          
*        XC    BFTYPC,BFTYPC       CLEAR FIELD                                  
*        MVC   BFTYPC(2),OFF       INSERT OFFICE                                
*        BAS   RE,PUTBUFF                                                       
*        LA    R4,BFREC                                                         
*        CLC   =C'BUFF1234',DUMPID DUMP BUFF RECORD?                            
*        BNE   AB500               NO                                           
*        MVC   P(08),=C'BUFFALO:'                                               
*        GOTO1 =V(PRINTER)                                                      
*        LA    R3,BFLENGTH                                                      
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R3),=C'2D'                
*                                                                               
AB500    CLI   RDBUYSW,C'Y'                                                     
         BE    ABEXT                                                            
         XC    BFYM,BFYM                                                        
         XC    BFSPOT1(16),BFSPOT1                                              
ABEXT    XIT1                                                                   
         EJECT                                                                  
         SPACE 2                                                                
* ROUTINE TO MOVE SDD RECORDS TO GIVEN AREA                                     
* - R2 = ADDRESS OF AREA TO PLACE RECORD                                        
SVSDD    NTR1                                                                   
         LR    RE,R2                                                            
         LA    RF,LSDDTBL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SD100    BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         MVC   0(3,R2),3(R6)       DAYPART - # OF TIMES                         
         LA    R2,3(R2)                                                         
         LR    R3,R6                                                            
         ZIC   R5,5(R6)            # DAY TIMES IN ELEMENT                       
SD200    MVC   0(5,R2),6(R3)                                                    
         LA    R1,1(R2)                                                         
         BAS   RE,ADJTIME                                                       
         CLC   =H'2600',3(R2)      LEAVE 2A ALONE                               
         BE    SD220                                                            
         SR    RE,RE                                                            
         ICM   RE,3,3(R2)                                                       
         BNZ   *+14                                                             
         MVC   3(2,R2),1(R2)       END 0 SAME AS START                          
         B     SD220                                                            
*                                                                               
         SH    RE,=H'1'            ADJUST END TIME                              
         STCM  RE,3,3(R2)                                                       
SD220    LA    R3,5(R3)            NEXT ELEM DAY TIME                           
         LA    R2,5(R2)            NEXT OF TALBE                                
         BCT   R5,SD200                                                         
         B     SD100                                                            
         SPACE                                                                  
* ADJUST TIME IF LESS THAN 600 (001-2A)                                         
ADJTIME  NTR1                                                                   
         LR    R0,2                                                             
AJ100    CLC   =H'600',0(R1)       ADJUST AFTER 1200                            
         BNH   AJ200                                                            
         SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         AH    RE,=H'2400'                                                      
         STCM  RE,3,0(R1)                                                       
AJ200    LA    R1,2(R1)                                                         
         OC    0(2,R1),0(R1)       IF END TIME 0 DON'T CONVERT                  
         BE    EXIT                                                             
         BCT   R0,AJ100                                                         
         B     EXIT                                                             
         EJECT                                                                  
* SET UP CONTRACT BUCKETS                                                       
SETCBUCS NTR1                                                                   
         XC    MTHTBL(200),MTHTBL                                               
         XC    MTHTBL+200(185),MTHTBL+200                                       
         LA    R3,MTHTBL           2(BYTES)-YM/3 BCM-ST YMD/3-EN YMD            
         LA    R5,48                                                            
         GOTO1 =V(DATCON),DMCB,(3,NCDTES),(0,WORK+20)                           
SB100    GOTO1 =V(GETBROAD),(R1),WORK+20,WORK,V(GETDAY),V(ADDAY)                
         CLI   0(R1),X'FF'                                                      
         BE    SB120                                                            
         GOTO1 =V(DATCON),(R1),(0,WORK),(3,2(R3))                               
         GOTO1 =V(DATCON),(R1),(0,WORK+6),(3,5(R3))                             
         LA    R0,7                GET DATE WITH CORRECT MONTH                  
         GOTO1 =V(ADDAY),(R1),WORK,WORK+30,(R0)                                 
         GOTO1 =V(DATCON),(R1),(0,WORK+30),(3,WORK+38)                          
         MVC   0(2,R3),WORK+38     MOVE IN MONTH YEAR                           
         CLC   NCDTES+3(3),5(R3)                                                
         BNH   SBEXT                                                            
         LA    R0,1                                                             
         GOTO1 =V(ADDAY),(R1),WORK+6,WORK+20,(R0)                               
         LA    R3,8(R3)                                                         
         BCT   R5,SB100                                                         
*                                                                               
         MVC   P+1(27),=C'DATES EXCEED TABLE CAPACITY'                          
         MVC   P+32(6),NCDTES                                                   
         GOTO1 =V(PRINTER)                                                      
***      DC    H'0'                                                             
         B     SBEXT                                                            
SB120    EQU   *                                                                
*                                                                               
*   TEST BUCS                                                                   
         MVC   P+1(27),=C'ERROR PROCESSING ORDER:    '                          
         MVC   P+32(6),NCDTES                                                   
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,DISPBUCS                                                      
         B     SBEXT                                                            
*   TEST END                                                                    
SBEXT    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
DISPBUCS NTR1                                                                   
         MVC   P+1(16),=C'DISPBUCS NCDTES:'                                     
         MVC   P+18(6),NCDTES                                                   
         MVC   P+27(8),=C'WORK+20:'                                             
         MVC   P+37(6),WORK+20                                                  
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*                                                                               
* SET UP CONTRACT SORT FIELDS                                                   
         USING RCONKEY,R4                                                       
         USING SORTRECD,R7                                                      
SETCONSR ST    RE,FULL                                                          
         MVI   SRREC,X'01'         CONTRACT RECORD                              
         MVC   SRSTAG,RCONKGRP                                                  
         MVC   SRCOTYPE(16),SVOTYPE                                             
         UNPK  WORK+10(9),RCONKCON(5) CONTRACT NUMBER                           
         PACK  WORK(5),WORK+10(8)                                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),WORK(5)                                               
         MVO   WORK(5),WORK+10(5)                                               
         MVC   SRCON#,WORK                                                      
         PACK  SRCON#(1),WORK+3(1) REVERSE THE COMPLIMENT                       
         PACK  SRCON#+1(1),WORK+2(1)                                            
         PACK  SRCON#+2(1),WORK+1(1)                                            
         PACK  SRCON#+3(1),WORK(1)                                              
         MVC   SRCTYPE,RCONTYPE    SET CONTRACT TYPE                            
         MVC   SRCOFF,RCONKOFF     SET CONTRACT OFFICE                          
         MVC   SRCAGY,RCONKAGY     SET AGENCY                                   
         MVC   SRCDTES,RCONDATE                                                 
         MVC   SRCBUYN,RECCNT                                                   
         CLI   SRACT,MYCON         DID 'I READ THIS CONTRACT?'                  
         BNE   *+10                NO  - CAME FROM RECOVERY TAPE                
         MVC   SRCOTYPE(09),SRCTYPE                                             
*                                  SET OLD CODES TO NEW CODES                   
         MVC   SRLEN(2),=H'56'                                                  
         AP    INCNTC,=P'1'                                                     
         CLC   =C'SORTOUT',DUMPID                                               
         BNE   SESR0050                                                         
         MVC   P(13),=C'SORTOUT: SETC'                                          
         GOTO1 =V(PRINTER)                                                      
         ZICM  R3,SRLEN,2          SET LENGTH                                   
         GOTO1 =V(PRNTBL),DMCB,(0,(R7)),(R7),C'DUMP',(R3),=C'2D'                
*                                                                               
SESR0050 EQU   *                                                                
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRLEN                                    
         XC    SVOTCD,SVOTCD                                                    
         CLI   SRACT,MYCON         STORE CONTRACT # THAT I READ                 
         BNE   TCEXT                                                            
*                                                                               
* STORE GROUP/STATION CONTRACT # IN TABLE                                       
         L     RF,ANEXTCNT         SET A(NEXT ENTRY)                            
         MVC   0(4,RF),SRCON#      SAVE CONTRACT NUMBER                         
         MVC   4(7,RF),SRSTAG      SAVE GROUP/STATION                           
         LA    RF,11(RF)           BUMP A(NEXT ENTRY)                           
         LA    R0,CNLSTTBX         END OF LIST                                  
         CR    R0,RF               END OF TABLE REACHED?                        
         BNL   *+8                 NO  - SAVE NEXT ADDRESS                      
         LA    RF,CNLSTTAB         YES - RESTART TABLE                          
         ST    RF,ANEXTCNT         SAVE NEXT ADDRESS                            
TCEXT    L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R4,R7                                                            
         EJECT                                                                  
*                                                                               
*  BUFFALO ROUTINES                                                             
HIGHBUFF XC    BFKEY,BFKEY                                                      
         LA    R1,=C'HIGH'                                                      
         B     BUFFX                                                            
*                                                                               
SEQBUFF  LA    R1,=C'SEQ'                                                       
         B     BUFFX                                                            
*                                                                               
PUTBUFF  LA    R1,=C'PUT'                                                       
         B     BUFFX                                                            
*                                                                               
RSETBUFF LA    R1,=C'RESET'                                                     
         B     BUFFX                                                            
*                                                                               
BUFFX    NTR1                                                                   
         ST    R1,DMCB                                                          
         GOTO1 =V(BUFFALO),DMCB,,BUFFC,BFREC,1                                  
         TM    DMCB+8,X'80'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*              I/O HANDLING ROUTINES - DIRECTORY                                
         SPACE                                                                  
HIGH     MVC   COMMAND(6),=C'DMRDHI'                                            
         B     DR100                                                            
         SPACE                                                                  
SEQ      MVC   COMMAND(6),=C'DMRSEQ'                                            
         B     DR100                                                            
         SPACE                                                                  
READ     MVC   COMMAND(6),=C'DMREAD'                                            
DR100    MVC   KEYSAVE,KEY                                                      
         B     DIR                                                              
         SPACE                                                                  
WRITE    MVC   COMMAND(6),=C'DMWRT '                                            
         B     DIR                                                              
         SPACE                                                                  
ADD      MVC   COMMAND(6),=C'DMADD '                                            
         SPACE                                                                  
DIR      NTR1                                                                   
         ZIC   R4,DMINBTS                                                       
         GOTO1 =V(DATAMGR),DMCB,((R4),COMMAND),=C'REPDIR',KEY,KEY,0             
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE                                                                  
FILE     NTR1                                                                   
         LA    R2,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         ZIC   R4,DMINBTS                                                       
         GOTO1 =V(DATAMGR),DMCB,((R4),COMMAND),=C'REPFILE',            X        
               (R2),AIO,DMWORK                                                  
         SPACE 2                                                                
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVI   DMINBTS,X'08'                                                    
         TM    8(R1),X'02'         DELETED OK                                   
         BO    EXIT                                                             
         CLI   8(R1),0                                                          
         BZ    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
         SPACE 3                                                                
GETEL    GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
         PRINT GEN                                                              
DUMMYDCB DS    0F                                                               
RECDSKIN DCB   DDNAME=RECDSKIN,                                        X        
               DSORG=PS,                                               X        
               RECFM=U,                                                X        
               BLKSIZE=9500,                                           X        
               MACRF=GM,                                               X        
               EODAD=RECDEOD                                                    
*                                                                               
         DS    0F                                                               
RECVIN   DCB   DDNAME=RECVIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=4048,                                             X        
               MACRF=GM,                                               X        
               EODAD=RTAPEEOD                                                   
*                                                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0F                                                               
VREGSAVE DC    V(REGSAVE)                                                       
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
         DC    C'*DMCB*'                                                        
DMCB     DS    6F                                                               
BUFFC    DS    A                                                                
         DC    C'*KEY'                                                          
KEY      DS    XL34                                                             
KEYSAVE  DS    XL34                                                             
SVSRKEY  DS    XL21                                                             
         DS    0D                                                               
DMWORK   DS    XL96                                                             
COMMAND  DS    CL8                                                              
DMINBTS  DC    X'08'               (PASS BACK DELETED RECORDS)                  
*                                                                               
         DS    0F                                                               
WORK     DS    CL80                                                             
UTL      DC    F'0',X'0A'                                                       
*SB      DC    F'2'                INHIBIT RECOVERY                             
         DS    0D                                                               
SSB      DS    0CL256              NEW SSB                                      
         DC    XL2'00'                                                          
         DC    X'FF'                                                            
         DC    X'02'                                                            
         DC    252X'00'                                                         
ARCVREC  DS    F                   A(CURRENT RECORD FROM RECOV FILE)            
SAVNAME  DS    CL10                                                             
SAVLOCAL DS    CL1                                                              
RECOVERY DS    CL1                 RECOVERY FILE SOURCE                         
COMBOREC DS    CL1                 COMBO RECORD IND:  Y'ES' OR N'O'             
         DC    CL8'*NEWELT*'                                                    
NEW02ELT DS    CL17                BUILD AREA FOR NEW ELEMENT                   
ADDORPUT DS    CL1                                                              
RECCOUNT DS    F                                                                
CODE02   EQU   0                   DISPLACEMENT EQUATES                         
LNGT02   EQU   1                                                                
DATE02   EQU   2                                                                
CTRL02   EQU   4                                                                
REGS02   EQU   5                                                                
REG$02   EQU   7                                                                
COMS02   EQU   11                                                               
COM$02   EQU   13                                                               
FILTAGY  DS    CL4                                                              
FILTADV  DS    CL4                                                              
DUMPID   DS    CL8                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(4,18,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=1004'                                  
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8'UREPFILE'                                                    
         DC    CL8'UREPDIR '                                                    
         DC    CL8'X'                                                           
BUFEND   DS    C                                                                
         DS    0F                                                               
ANEXTCNT DC    A(CNLSTTAB)         ADDRESS OF NEXT STATION IN TABLE             
ACURSDDT DS    A                   ADDRESS OF STATION SDD REC                   
ADYTMEL  DS    A                   ADDRESS OF BUY DAYTIME ELEM                  
TSTASP1  DS    F                   TOTAL REGULAR SPOTS/STATION                  
TSTACS1  DS    F                   TOTAL REGULAR $$/STATION                     
TSTPSP1  DS    F                   TOTAL REGULAR SPOTS/STATION (PUTS)           
TSTPCS1  DS    F                   TOTAL REGULAR $$/STATION (PUTS)              
TTLSP1   DS    F                   TOTAL REGULAR SPOTS                          
TTLCS1   DS    F                   TOTAL REGULAR $$                             
TSTASP2  DS    F                   TOTAL COMBO   SPOTS/STATION                  
TSTACS2  DS    F                   TOTAL COMBO   $$/STATION                     
TSTPSP2  DS    F                   TOTAL COMBO   SPOTS/STATION (PUTS)           
TSTPCS2  DS    F                   TOTAL COMBO   $$/STATION (PUTS)              
TTLSP2   DS    F                   TOTAL COMBO   SPOTS                          
TTLCS2   DS    F                   TOTAL COMBO   $$                             
RECCNT   DS    F                   ORDER OF RECORDS ADDED INTO SORT             
DATADISP DS    H                                                                
TODAY2   DS    H                   TODAY"S COMPRESSED DATE                      
         DS    0F                                                               
COLVALS  DS    24XL16                                                           
AIO      DS    A                                                                
AIO1     DS    A                   IOAREA                                       
AIO2     DS    A                   2ND IOAREA                                   
AIO3     DS    A                   3RD IOAREA                                   
ARCVBLK  DS    A                   A(RECOVERY BLOCK)                            
CNLSTTAB DS    1100C               STA TBL IN RECOVERY  (RR100-RREND)           
CNLSTTBX EQU   *-1                                                              
REP      DS    CL2                 REP CODE                                     
MED      DS    CL1                 MEDIA                                        
STAGFIL  DS    CL7                 STATION GROUP FILTER                         
STAGEFIL DS    CL7                 END STATION GROUP FILTER                     
WRITEFLG DS    CL1                 DON'T WRITE                                  
FRST     DS    CL1                                                              
FSTCHG   DS    CL1                 KEEP 1ST CHANGES OLD PRODUCT                 
ADDCNT   DS    CL1                 CONTRACT ADDED TODAY (NO ATHNEAS)            
ABUYSW   DS    CL1                 HAD A BUY FOR THIS CONTRACT #                
TCDCHGSW DS    CL1                 CONTRACT TYPE CODE CHANGE                    
RDBUYSW  DS    CL1                 ADD TO BUFFALO FROM BUY READS                
CHGSTASW DS    CL1                 A STATION CHANGE                             
PLANSW   DS    CL1                 BUY A PLAN                                   
TYP      DS    CL1                 CONTRACT TYPE                                
OFF      DS    CL2                 CONTRACT OFFICE                              
AGY      DS    CL6                 AGENCY/AGY OFFICE                            
PRD      DS    CL3                 PRODUCT-ALWAYS SPACES                        
NTYPE    DS    CL1                 NEW CONTRACT TYPE                            
NOFF     DS    CL2                 NEW OFFICE                                   
NAGY     DS    CL6                 NEW AGENCY/AGENCY OFFICE                     
OTYPE    DS    CL1                 OLD CONTRACT TYPE                            
OOFF     DS    CL2                 OLD OFFICE                                   
OAGY     DS    CL6                 OLD AGENCY/AGENCY OFFICE                     
SVSTA    DS    CL7                 LAST CONTRACT STATION FOR SDD READ           
LSTSTA   DS    CL7                 LAST CONTRACT STATION FOR ADD RTN            
OGRSTA   DS    CL7                 OLD CONTRACT STATION                         
NGRSTA   DS    CL7                 NEW CONTRACT STATION                         
BUYCHGN  DS    CL4                 STATION CHG - NEXT BUY #                     
REJSTA   DS    CL7                 REJECT THIS STATION                          
NCNTL    DS    XL1                 DEFAULT SPOTS PER WEEK                       
NNW      DS    XL1                 DEFAULT SPOTS PER WEEK                       
NTYP     DS    CL1                 BUY TYPE                                     
NDUR     DS    CL2                 BUY LENGTH                                   
NCDTES   DS    CL6                 NEW CONTRACT DATES                           
SVOTCD   DS    0CL17               3 TYPE CODES                                 
SVOTYPE  DS    CL1                 CONTRACT TYPE                                
SVOOFF   DS    CL2                 OFFICE CODE                                  
SVOAGY   DS    CL6                 AGENCY CODE                                  
SVOGSTA  DS    CL7                 GROUP/STATION                                
ELCODE   DS    XL1                                                              
COUNT    DC    PL3'0'              COUNT OF ATHENA RECS/STATION                 
TACOUNT  DC    PL5'0'              COUNT OF ATHENA RECS/ADDED TOTAL             
TCCOUNT  DC    PL5'0'              COUNT OF ATHENA RECS/CHG TOTAL               
INCNTC   DC    PL4'0'              COUNT OF CONTRACT INPUT RECORDS              
INCNTB   DC    PL4'0'              COUNT OF BUY INPUT RECORDS                   
MTHTBL   DS    CL385               48 BROADCAST MONTH DATES+ENDING 0000         
* MAX NUMBER OF DAY/TIMES (48) * MAX LENGTH SIZE OF ENTRY (8),                  
* + ENDING ZERO (1) = TOTAL SIZE (385)                                          
LSDDTBL  EQU   48*8+1                                                           
DEFSDDT  DS    (LSDDTBL)CL1        DEFAULT TV SDD TABLE                         
         ORG   DEFSDDT                                                          
DEFDPT   DS    0CL2                - DAYPART                                    
DEFCNT   DS    0CL1                - COUNT OF DAY TIMES                         
DEFDAY   DS    0CL1                - DAY                                        
DEFTIME  DS    0CL4                - TIME                                       
         ORG   DEFSDDT+LSDDTBL                                                  
DEFSDDR  DS    (LSDDTBL)CL1        DEFAULT RADIO SDD TABLE                      
         ORG   DEFSDDR                                                          
DEFDPTR  DS    0CL2                - DAYPART                                    
DEFCNTR  DS    0CL1                - COUNT OF DAY TIMES                         
DEFDAYR  DS    0CL1                - DAY                                        
DEFTIMER DS    0CL4                - TIME                                       
         ORG   DEFSDDR+LSDDTBL                                                  
STASDDT  DS    (LSDDTBL)CL1        STATION SDD TABLE                            
         ORG   STASDDT                                                          
STADPT   DS    0CL2                - DAYPART                                    
STACNT   DS    0CL1                - COUNT OF DAY TIMES                         
STADAY   DS    0CL1                - DAY                                        
STATIME  DS    0CL4                - TIME                                       
         ORG   STASDDT+LSDDTBL                                                  
         DS    0F                                                               
BFREC    DS    0CL40               BUFFALO RECORD LAYOUT                        
BFKEY    DS    0CL24                                                            
BFSTA    DS    CL7                 GROUP/STATION                                
BFTYP    DS    CL1                 TYPE (1-4)                                   
BFTYPC   DS    CL7                 CONTYPE/AGENCY/SALES OFFICE                  
BFDPT    DS    CL1                 DAYPART                                      
BFSDPT   DS    CL1                 SUB DAYPART                                  
BFPGT    DS    CL1                 PROGRAM TYPE                                 
BFSPL    DS    CL2                 SPOT LENGTH                                  
BFYM     DS    CL2                 YEAR MONTH                                   
         DS    CL2                 SPARE                                        
BFSPOT1  DS    F                   TOTAL SPOTS    : REGULAR                     
BFDOLS1  DS    F                   TOTAL DOLLARS  : REGULAR                     
BFSPOT2  DS    F                   TOTAL SPOTS    : COMBO                       
BFDOLS2  DS    F                   TOTAL DOLLARS  : COMBO                       
*                                                                               
BFLENGTH EQU   *-BFREC                                                          
*                                                                               
         BUFF  LINES=1,ROWS=1,COLUMNS=4,FLAVOR=BINARY,                 X        
               KEYLIST=(24,A)                                                   
*                                                                               
         DC    C'REC1'                                                          
IO1      DS    6100C                                                            
         DC    C'REC2'                                                          
IO2      DS    6100C                                                            
         DC    C'REC3'                                                          
IO3      DS    6100C                                                            
*                                                                               
*        RECOVERY FILE BLOCK AREA                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'*RCVBLK*'                                                    
RCVBLK   EQU   *                                                                
         DS    9600C                                                            
RCVBLKX  EQU   *                                                                
         DC    CL16'*END OF RCVBLK***'                                          
*                                                                               
         SPACE 2                                                                
SVLEN    EQU   0                                                                
SVGSTAE  EQU   4                                                                
SVSTAE   EQU   6                                                                
SVSTAME  EQU   10                                                               
SVCONE   EQU   11                                                               
SVRECE   EQU   15                                                               
SVNUME   EQU   16                                                               
SVACTE   EQU   20                                                               
*          DATA SET NET133     AT LEVEL 010 AS OF 10/22/86                      
COPY     EQU   1                                                                
CHANGE   EQU   2                                                                
*  ADD      EQU   3                SOME WHERE A DUPLICATE                       
SRTCPYE  EQU   2                                                                
SRTCHGE  EQU   3                                                                
SRTADDE  EQU   1                                                                
MYCON    EQU   4                                                                
         SPACE 2                                                                
*                                                                               
SORTRECD DSECT                                                                  
SRLEN    DS    CL4                 LENGTH                                       
SRSTAG   DS    CL7                 GROUP/STATION                                
SRCON#   DS    CL4                 CONTRACT NUMBER (P'S COMP REVERSED)          
SRREC    DS    CL1                 REC (01-CONTRACT/02-BUY)                     
SRNUM    DS    CL4                 ORDER OF RECS (IGNORE IF CONTRACT)           
SRACT    DS    CL1                 ACTION (ADD/COPY/CHANGE)                     
SRCOMMON DS    0CL1                START OF RECORD INFO                         
* CONTRACT INFO EXTRACTED                                                       
SRCTYPE  DS    CL1                 CONTRACT TYPE                                
SRCOFF   DS    CL2                 OFFICE                                       
SRCAGY   DS    CL6                 AGENCY/AGENCY OFFICE                         
SRCDTES  DS    CL6                 START END DATES OF CONTRACT                  
SRCBUYN  DS    CL4                 MOST RECENT BUY NUMBER                       
SRCOTYPE DS    CL1                 OLD CONTRACT TYPE                            
SRCOOFF  DS    CL2                 OLD OFFICE                                   
SRCOAGY  DS    CL6                 OLD AGENCY/AGY OFF                           
SRCOGSTA DS    CL7                 GROUP/STATION                                
* BUY INFO EXTRACTED                                                            
         ORG   SRCOMMON                                                         
SRBCNTL  DS    CL1                 CONTROL WORK (X'80'DEL/CAN)                  
SRBCOMBO DS    CL1                 COMBO FLAG                                   
SRBTYP   DS    CL1                 TYPE                                         
SRBDUR   DS    CL2                 LENGTH                                       
SRBNW    DS    CL1                 NUMBER SPOT PER WEEK                         
SRBCOS   DS    CL4                 RATE                                         
SRBPLN   DS    CL1                 PLAN = Y                                     
SRBDTM   DS    0CL1                DAY TIME ELEMENTS                            
SRBWKS   DS    0CL1                WEEKS RUN ELEMENT                            
         DSECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
* REGENCON                                                                      
* REGENBUY                                                                      
* REGENAUR                                                                      
* REGENSDD                                                                      
* DDBUFFALOD                                                                    
* DDDPRINT                                                                      
         DSECT                                                                  
         DS    CL4                                                              
       ++INCLUDE REGENCON                                                       
         DSECT                                                                  
       ++INCLUDE REGENBUY                                                       
       ++INCLUDE REGENAUR                                                       
       ++INCLUDE REGENSDD                                                       
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDDPRINT                                                       
       EJECT                                                                    
*                                                                               
*  ROUTINE ADDS A.U.R. RECORDS - BUILDS STATION TOTALS                          
*                                                                               
CHGAUREC CSECT                                                                  
         NMOD1 0,*CHGA*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         SR    R3,R3                                                            
         BAS   RE,HIGHBUF2                                                      
         B     *+8                                                              
*                                                                               
CHGA0020 BAS   RE,SEQBUF2                                                       
         BO    CHGA0300                                                         
*                                                                               
         CLC   =C'BUFB',DUMPID     DUMP BUFF RECORD?                            
         BNE   CHGA0022            NO                                           
         LA    R4,BFREC                                                         
         MVC   P(08),=C'BUFFBAK:'                                               
         GOTO1 =V(PRINTER)                                                      
         LA    R3,BFLENGTH                                                      
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R3),=C'2D'                
CHGA0022 EQU   *                                                                
         OC    BFSPOT1(16),BFSPOT1                                              
         BZ    CHGA0020            NO SPOTS OR DOLLARS DON'T UPDATE             
*                                                                               
         OC    LSTSTA,LSTSTA                                                    
         BZ    CHGA0024            FIRST TIME                                   
         CLC   BFSTA,LSTSTA        SAME STATION?                                
         BZ    CHGA0024                                                         
         GOTO1 =A(PRNTSTA),DMCB,(RC)                                            
CHGA0024 EQU   *                                                                
         MVC   LSTSTA,BFSTA                                                     
*                                                                               
* SEE IF RECORD EXISTS                                                          
         MVI   DMINBTS,X'80'       READ FOR UPDATE                              
         XC    KEY,KEY                                                          
         MVI   KEY,X'2C'                                                        
         MVC   KEY+RAURKREP-RAURKEY(2),REP                                      
         MVC   KEY+RAURKGRP-RAURKEY(7),BFSTA                                    
         MVC   KEY+RAURKTPE-RAURKEY(7),BFTYP                                    
         MVC   KEY+RAURKDPT-RAURKEY(7),BFDPT                                    
         GOTO1 HIGH2                                                            
         MVC   AIO,AIO2                                                         
         L     R6,AIO2                                                          
         CLC   KEY(27),KEYSAVE     AUR RECORD FOUND?                            
         BNE   CHGA0180            NO                                           
*                                                                               
         GOTO1 GETREC2             YES - RETRIEVE IT                            
         USING RAURRECD,R6                                                      
*                                                                               
         L     R4,AIO                                                           
         CLC   =C'SPECIAL',DUMPID  DUMP BUFF RECORD?                            
         BNE   CHGA0028            NO                                           
         ZICM  R3,RAURLEN,2        GET RECORD LENGTH                            
         MVC   P(17),=C'AUR RECORD FOUND:'                                      
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R3),=C'2D'                
*                                                                               
CHGA0028 EQU   *                                                                
         LR    RE,R6               CHECK LAST POSITION                          
         ZICM  RF,RAURLEN,2        GET RECORD LENGTH                            
         AR    RE,RF               A(END OF RECORD)                             
         BCTR  RE,0                BACK OFF 1 POSITION                          
         MVI   0(RE),0             SET LAST CHAR TO ZERO                        
         MVC   RAURLCHG,TODAY2     SET LAST CHANGE DATE TO TODAY                
*                                                                               
         DROP  R6                                                               
*                                                                               
         SR    R5,R5               CLEAR INDICATOR/SAVE SPACE                   
         MVI   ELCODE,X'02'        RETRIEVE SPOT/COST ELEMENT                   
         BAS   RE,GETEL            GET FIRST                                    
         B     *+8                                                              
CHGA0040 BAS   RE,NEXTEL           GET NEXT                                     
         BNE   CHGA0060            NOT FOUND                                    
         CLC   TODAY2,RAURSCAS-RAURSCDT(R6)                                     
*                                  FOUND:  CHECK AS-AT DATE                     
         BNE   CHGA0040            NOT SAME - GO BACK FOR NEXT                  
         LR    R5,R6               SAME     - SAVE A(02 ELEMENT)                
         BAS   RE,UNLOAD02         SAVE 02 ELEMENT                              
         B     CHGA0040            GO BACK FOR END OF RECORD                    
*                                                                               
CHGA0060 EQU   *                                                                
         LTR   R5,R5               ELT W/SAME DATE FOUND?                       
         BZ    CHGA0080            NO  - BUILD NEW ELEMENT                      
         B     CHGA0100            YES - USE EXISTING ELEMENT                   
CHGA0080 EQU   *                                                                
         XC    NEW02ELT,NEW02ELT   CLEAR NEW 02 ELT AREA                        
         MVC   NEW02ELT+CODE02(2),=X'020B'                                      
         MVC   NEW02ELT+DATE02,TODAY2                                           
*                                  INSERT TODAY'S DATE                          
CHGA0100 L     RE,BFSPOT1          REGULAR SPOTS                                
         SR    RF,RF                                                            
         ICM   RF,3,NEW02ELT+REGS02                                             
         AR    RE,RF               ACCUMULATE REGULAR SPOTS                     
         STCM  RE,3,NEW02ELT+REGS02                                             
*****>   STCM  RE,15,BFSPOT1       FOR SPOT TOTALS                              
*                                                                               
         L     RE,BFDOLS1          REGULAR $$                                   
         ICM   RF,15,NEW02ELT+REG$02                                            
         AR    RE,RF               ACCUMULATE REGULAR $$                        
         STCM  RE,15,NEW02ELT+REG$02                                            
*****>   STCM  RE,15,BFDOLS1       FOR DOLLAR TOTALS                            
*                                                                               
         L     RE,BFSPOT2          COMBO   SPOTS                                
         SR    RF,RF                                                            
         ICM   RF,3,NEW02ELT+COMS02                                             
         AR    RE,RF               ACCUMULATE COMBO   SPOTS                     
         STCM  RE,3,NEW02ELT+COMS02                                             
*****>   STCM  RE,15,BFSPOT2       FOR SPOT TOTALS                              
*                                                                               
         L     RE,BFDOLS2          COMBO   $$                                   
         ICM   RF,15,NEW02ELT+COM$02                                            
         AR    RE,RF               ACCUMULATE COMBO   $$                        
         STCM  RE,15,NEW02ELT+COM$02                                            
*****>   STCM  RE,15,BFDOLS2       FOR DOLLAR TOTALS                            
*                                                                               
         XC    NEW02ELT+CTRL02(1),NEW02ELT+CTRL02                               
*                                  CLEAR CONTROL BYTE                           
         OC    NEW02ELT+REGS02(6),NEW02ELT+REGS02                               
*                                  ANY REGULAR SPOTS/$$?                        
         BZ    CHGA0120            NO  -                                        
         OI    NEW02ELT+CTRL02,X'80'                                            
*                                  YES - SET 'REGULAR SPOTS/$$'                 
         OC    NEW02ELT+COMS02(6),NEW02ELT+COMS02                               
*                                  ANY COMBO   SPOTS/$$?                        
         BZ    CHGA0140            NO  -                                        
         OI    NEW02ELT+CTRL02,X'40'                                            
*                                  YES - SET 'COMBO SPOTS/$$'                   
         MVC   NEW02ELT+CODE02(2),=X'0211'                                      
*                                  SET NEW LENGTH INTO CONTROL                  
         B     CHGA0140                                                         
CHGA0120 EQU   *                                                                
         MVI   NEW02ELT+LNGT02,X'0B'                                            
*                                  SET ELEMENT LEN TO 11 BYTES                  
         OC    NEW02ELT+COMS02(6),NEW02ELT+COMS02                               
*                                  ANY COMBO   SPOTS/$$?                        
         BZ    CHGA0140            NO  - ELEMENT WILL BE OUTPUT AS              
*                                     11 CHARS, NO CONTROL SET                  
         OI    NEW02ELT+CTRL02,X'40'                                            
*                                  YES - SET 'COMBO SPOTS/$$'                   
         MVC   NEW02ELT+REGS02(6),NEW02ELT+COMS02                               
*                                  MOVE COMBO DATA TO 1ST BUCKETS               
CHGA0140 EQU   *                                                                
         L     R6,AIO2             A(RECORD IN IO AREA 2)                       
         USING RAURRECD,R6                                                      
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),(R6),NEW02ELT,0                
         CLI   DMCB+12,X'05'       IS RECORD TOO LONG???                        
         BNE   CHGA0160                                                         
         MVC   P(25),=C'*ERROR - RECORD TOO LARGE'                              
         MVC   P+26(7),RAURKGRP                                                 
         GOTO1 =V(HEXOUT),DMCB,BFREC,P+38,20,=C'T0G'                            
         GOTO1 =V(PRINTER)                                                      
         B     CHGA0020                                                         
*                                                                               
CHGA0160 EQU   *                                                                
         AP    TCCOUNT,=P'1'                                                    
         MVI   ADDORPUT,C'P'       SET 'PUT' FLAG                               
         ZICM  R3,RAURLEN,2        L(RECORD FOR DISPLAY)                        
         CLI   WRITEFLG,C'Y'                                                    
         BNE   CHGA0240                                                         
         BAS   RE,DUMPOUTP         SEE IF RECORD DUMP WANTED                    
         BAS   RE,PUTREC2          CHANGE FILE RECORD                           
         B     CHGA0260                                                         
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* UNLOAD02:  FOR 02 ELEMENT FOUND FOR DATE:                                     
*     MOVES IT TO NEW02ELT                                                      
*     EXPANDS NEW02ELT FOR REGULAR/COMBO                                        
*     RESETS CONTROL BYTE FOR REGULAR/COMBO                                     
*     FLAGS ORIGINAL ELEMENT AS X'FF' AND DELETES IT                            
*                                                                               
UNLOAD02 NTR1                                                                   
         XC    NEW02ELT,NEW02ELT   CLEAR NEW 02 AREA                            
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXEC STATEMENT                 
         EX    RF,UNLO0090         MOVE BY LENGTH                               
         TM    NEW02ELT+CTRL02,X'80'                                            
*                                  REGULAR SPOTS?                               
         BO    UNLO0010            YES - ELEMENT OKAY AS LOADED                 
         TM    NEW02ELT+CTRL02,X'40'                                            
*                                  NO  - COMBO   SPOTS?                         
         BNO   UNLO0010            NO  - LEAVE AS IS                            
         MVC   NEW02ELT+COMS02(6),NEW02ELT+REGS02                               
*                                  YES - W/NO REG SPOTS, COMBO SPOTS            
*                                     ARE IN 1ST BUCKETS - MOVE TO              
*                                     SECOND SET OF BUCKETS                     
         MVI   NEW02ELT+LNGT02,X'11'                                            
*                                  RESET LENGTH TO 17 CHARACTERS                
         XC    NEW02ELT+REGS02(6),NEW02ELT+REGS02                               
*                                  CLEAR COMBO VALUES FROM REG SLOT             
UNLO0010 EQU   *                                                                
         XC    NEW02ELT+CTRL02(1),NEW02ELT+CTRL02                               
*                                  RESET CONTROL BYTE                           
         MVI   0(R6),X'FF'         SET OLD ELEMENT FOR DELETE                   
         L     R6,AIO2             SET A(RECORD)                                
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'FF',(R6)),0,0               
         XIT1                                                                   
UNLO0090 MVC   NEW02ELT(0),0(R6)                                                
         EJECT                                                                  
*                                                                               
*    NEW RECORD - BUILD & ADD                                                   
*                                                                               
CHGA0180 EQU   *                                                                
         L     R6,AIO2             SET A(RECORD AREA)                           
         USING RAURRECD,R6                                                      
*                                                                               
         XC    0(256,R6),0(R6)     CLEAR RECORD AREA                            
         MVI   RAURKTYP,X'2C'                                                   
         MVC   RAURKREP,REP        REP                                          
         MVC   RAURKGRP(7),BFSTA                                                
         MVC   RAURCODE(2),=X'010C'                                             
         MVC   RAURCREA,TODAY2                                                  
         MVC   RAURLCHG,TODAY2                                                  
         MVC   RAURKTPE(8),BFTYP                                                
         MVC   RAURKDPT(7),BFDPT                                                
         MVC   RAURLEN,=H'58'      INITIAL LENGTH                               
         LA    R4,RAURCODE         A(01 ELEMENT)                                
         ZIC   RE,1(R4)            BUMP TO A(NEXT ELEMENT)                      
         AR    R4,RE                                                            
*                                                                               
         USING RAURSCDT,R4                                                      
*                                                                               
         XC    0(24,R4),0(R4)      CLEAR ANY RESIDUAL DATA                      
         MVC   0(2,R4),=X'020B'    SET ELT CODE/LENGTH                          
         MVC   RAURSCAS,TODAY2     SET AS-AT DATE                               
         MVI   RAURSCTL,0          RESET CONTROL BYTE                           
         OC    BFSPOT1(8),BFSPOT1  ANY REGULAR VALUE?                           
         BZ    CHGA0200            NO                                           
*                                  YES - LOAD INTO FIRST BUCKETS                
         OI    RAURSCTL,X'80'      SET CONTROL INDICATOR                        
         MVC   RAURSCS1,BFSPOT1+2  LOAD REGULAR SPOTS                           
         MVC   RAURSCC1,BFDOLS1    LOAD REGULAR DOLLARS                         
         OC    BFSPOT2(8),BFSPOT2  ANY COMBO   VALUE?                           
         BZ    CHGA0220            NO                                           
*                                  YES - LOAD INTO SECOND BUCKETS               
         OI    RAURSCTL,X'40'      YES - SET CONTROL INDICATOR                  
         MVC   RAURSCS2,BFSPOT2+2  LOAD COMBO   SPOTS                           
         MVC   RAURSCC2,BFDOLS2    LOAD COMBO   DOLLARS                         
         MVI   1(R4),X'11'         INCREASE LENGTH TO 17                        
*                                     FOR BOTH REG+COMBO COUNTERS               
         MVC   RAURLEN,=H'64'      INCREASE OVERALL REC LENGTH                  
*                                     FOR BOTH REG+COMBO COUNTERS               
         B     CHGA0220                                                         
CHGA0200 EQU   *                                                                
         OC    BFSPOT2(8),BFSPOT2  ANY COMBO   VALUE?                           
         BZ    CHGA0220            NO                                           
*                                  YES - LOAD INTO FIRST BUCKETS                
         OI    RAURSCTL,X'40'      YES - SET CONTROL INDICATOR                  
         MVC   RAURSCS1,BFSPOT2+2  LOAD COMBO   SPOTS                           
         MVC   RAURSCC1,BFDOLS2    LOAD COMBO   DOLLARS                         
CHGA0220 EQU   *                                                                
         ZICM  R3,RAURLEN,2        L(RECORD) FOR DISPLAY                        
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
         AP    TACOUNT,=P'1'                                                    
         MVI   ADDORPUT,C'A'       SET 'ADD' FLAG                               
         CLI   WRITEFLG,C'Y'                                                    
         BNE   CHGA0240                                                         
         BAS   RE,DUMPOUTP         SEE IF RECORD DUMP WANTED                    
         BAS   RE,ADDREC2          ADD DIRECTLY TO FILE                         
         B     CHGA0260                                                         
*                                                                               
CHGA0240 L     R4,AIO                                                           
         CLC   =C'SPECIAL',DUMPID  DUMP BUFF RECORD?                            
         BE    CHGA0242            NO                                           
         CLC   =C'OUTPUT',DUMPID   DUMP BUFF RECORD?                            
         BNE   CHGA0260            NO                                           
CHGA0242 EQU   *                                                                
         MVC   P(16),=C'OUTPUT FROM ADD:'                                       
         CLI   ADDORPUT,C'A'       SET 'WHERE IT CAME FROM'                     
         BE    CHGA0250                                                         
         MVC   P(16),=C'OUTPUT FROM PUT:'                                       
CHGA0250 EQU   *                                                                
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R3),=C'2D'                
*                                                                               
CHGA0260 CLI   BFTYP,1             ONLY ADD 1 BUYLINES SPOTS/DOLLARS            
         BNE   CHGA0280                                                         
         CLI   ADDORPUT,C'A'       ADD?                                         
         BNE   CHGA0270            NO  - ACCUMULATE 'PUT' FIGURES               
*                                  YES - ACCUMULATE 'ADD' FIGURES               
         L     RE,TSTASP1          ACCUMULATE REGULAR FIGURES                   
         A     RE,BFSPOT1                                                       
         ST    RE,TSTASP1                                                       
         L     RE,TSTACS1                                                       
         A     RE,BFDOLS1                                                       
         ST    RE,TSTACS1                                                       
         L     RE,TSTASP2          ACCUMULATE COMBO   FIGURES                   
         A     RE,BFSPOT2                                                       
         ST    RE,TSTASP2                                                       
         L     RE,TSTACS2                                                       
         A     RE,BFDOLS2                                                       
         ST    RE,TSTACS2                                                       
         B     CHGA0280                                                         
CHGA0270 EQU   *                   ACCUMULATE 'PUT' FIGURES                     
         L     RE,TSTPSP1          ACCUMULATE REGULAR FIGURES                   
         A     RE,BFSPOT1                                                       
         ST    RE,TSTPSP1                                                       
         L     RE,TSTPCS1                                                       
         A     RE,BFDOLS1                                                       
         ST    RE,TSTPCS1                                                       
         L     RE,TSTPSP2          ACCUMULATE COMBO   FIGURES                   
         A     RE,BFSPOT2                                                       
         ST    RE,TSTPSP2                                                       
         L     RE,TSTPCS2                                                       
         A     RE,BFDOLS2                                                       
         ST    RE,TSTPCS2                                                       
CHGA0280 AP    COUNT,=P'1'                                                      
         B     CHGA0020                                                         
*                                                                               
CHGA0300 MVC   AIO,AIO1                                                         
         LTR   R3,R3                                                            
CHGA0320 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  DUMPOUTP:  UPON REQUEST, DUMP THE OUTPUT RECORD                              
*                                                                               
DUMPOUTP NTR1                                                                   
         CLC   =C'SPECIAL',DUMPID  DUMP OUTPUT RECORD?                          
         BE    DMOP0020            NO                                           
         CLC   =C'OUTPUT',DUMPID   DUMP OUTPUT RECORD?                          
         BE    DMOP0020            NO                                           
         CLC   =C'REGCOM',DUMPID   DUMP OUTPUT RECORD?                          
         BNE   DMOP0040            NO                                           
         L     R4,AIO2             FIND X'02' ELEMENT                           
         LA    R4,34(R4)           BUMP TO DESCRIP ELEMENT                      
DMOP0010 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    DMOP0040            YES                                          
         CLI   0(R4),2             02 ELEMENT?                                  
         BNE   DMOP0016            NO  -                                        
         TM    RAURSCTL-RAURSCDT(R4),X'C0'                                      
*                                  BOTH REGULAR AND COMBO?                      
         BO    DMOP0020            YES - PRINT IT                               
DMOP0016 EQU   *                                                                
         ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         B     DMOP0010            GO BACK FOR NEXT                             
DMOP0020 EQU   *                                                                
         L     R4,AIO2             A(RECORD BEING DISPLAYED)                    
         USING RAURRECD,R4                                                      
         ZICM  R3,RAURLEN,2        L(RECORD BEING DISPLAYED)                    
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R3),=C'2D'                
         B     DMOP0040                                                         
DMOP0040 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*  BUFFALO ROUTINES                                                             
HIGHBUF2 XC    BFKEY,BFKEY                                                      
         LA    R1,=C'HIGH'                                                      
         B     BUFFX2                                                           
*                                                                               
SEQBUF2  LA    R1,=C'SEQ'                                                       
         B     BUFFX2                                                           
*                                                                               
PUTBUF2  LA    R1,=C'PUT'                                                       
         B     BUFFX2                                                           
*                                                                               
RSETBUF2 LA    R1,=C'RESET'                                                     
         B     BUFFX2                                                           
*                                                                               
BUFFX2   NTR1                                                                   
         ST    R1,DMCB                                                          
         GOTO1 =V(BUFFALO),DMCB,,BUFFC,BFREC,1                                  
         TM    DMCB+8,X'80'                                                     
BUFFXIT2 XIT1                                                                   
         EJECT                                                                  
*              I/O HANDLING ROUTINES - DIRECTORY                                
         SPACE                                                                  
HIGH2    MVC   COMMAND(6),=C'DMRDHI'                                            
         B     DR2100                                                           
         SPACE                                                                  
SEQ2     MVC   COMMAND(6),=C'DMRSEQ'                                            
         B     DR2100                                                           
         SPACE                                                                  
READ2    MVC   COMMAND(6),=C'DMREAD'                                            
DR2100   MVC   KEYSAVE,KEY                                                      
         B     DIR2                                                             
         SPACE                                                                  
WRITE2   MVC   COMMAND(6),=C'DMWRT '                                            
         B     DIR2                                                             
         SPACE                                                                  
ADD2     MVC   COMMAND(6),=C'DMADD '                                            
         SPACE                                                                  
DIR2     NTR1                                                                   
         ZIC   R4,DMINBTS                                                       
         GOTO1 =V(DATAMGR),DMCB,((R4),COMMAND),=C'REPDIR',KEY,KEY,0             
         B     DMCHECK2                                                         
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
GETREC2  MVC   COMMAND,=C'GETREC'                                               
         B     FILE2                                                            
         SPACE 2                                                                
PUTREC2  MVC   COMMAND,=C'PUTREC'                                               
         B     FILE2                                                            
         SPACE 2                                                                
ADDREC2  MVC   COMMAND,=C'ADDREC'                                               
         B     FILE2                                                            
         SPACE                                                                  
FILE2    NTR1                                                                   
         LA    R2,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         ZIC   R4,DMINBTS                                                       
         GOTO1 =V(DATAMGR),DMCB,((R4),COMMAND),=C'REPFILE',            X        
               (R2),AIO,DMWORK                                                  
         SPACE 2                                                                
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK2 MVI   DMINBTS,X'08'                                                    
         TM    8(R1),X'02'         DELETED OK                                   
         BO    DMCHEXIT                                                         
         CLI   8(R1),0                                                          
         BZ    DMCHEXIT                                                         
         DC    H'0'                                                             
DMCHEXIT EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  ROUTINE PRINTS TOTAL PER STATION                                             
*                                                                               
PRNTSTA  CSECT                                                                  
         NMOD1 0,*PRNS*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CP    LINE,=P'48'                                                      
         BL    PS100                                                            
         ZAP   LINE,=P'100'        FORCE PAGE BREAK                             
         MVC   TITLE+11(20),=C'A.U.R. UPDATE REPORT'                            
PS100    MVC   P(7),LSTSTA                                                      
         CLI   P+6,C' '                                                         
         BNE   *+8                                                              
         MVI   P+6,C'T'                                                         
         MVC   P+12(29),=CL29'** STATION RECORDS UPDATED **'                    
         EDIT  (P3,COUNT),(5,P+45)                                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P+12(06),=C'ADDED:'                                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P(19),=CL19'** STATION SPOTS **'                                 
         EDIT  (4,TSTASP1),(6,P+21)                                             
         MVC   P+29(21),=CL21'** STATION DOLLARS **'                            
         EDIT  (4,TSTACS1),(12,P+52)                                            
         MVC   P+66(19),=CL19'** COMBO   SPOTS **'                              
         EDIT  (4,TSTASP2),(6,P+87)                                             
         MVC   P+95(21),=CL21'** COMBO   DOLLARS **'                            
         EDIT  (4,TSTACS2),(12,P+118)                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P+12(08),=C'CHANGED:'                                            
         GOTO1 =V(PRINTER)                                                      
         MVC   P(19),=CL19'** STATION SPOTS **'                                 
         EDIT  (4,TSTPSP1),(6,P+21)                                             
         MVC   P+29(21),=CL21'** STATION DOLLARS **'                            
         EDIT  (4,TSTPCS1),(12,P+52)                                            
         MVC   P+66(19),=CL19'** COMBO   SPOTS **'                              
         EDIT  (4,TSTPSP2),(6,P+87)                                             
         MVC   P+95(21),=CL21'** COMBO   DOLLARS **'                            
         EDIT  (4,TSTPCS2),(12,P+118)                                           
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                  ACCUMULATE 'ADDED/CHANGED' INTO              
*                                     COMMON ACCUMULATORS                       
         L     RE,TTLSP1           ACCUMULATE REGULAR FIGURES                   
         A     RE,TSTASP1                                                       
         ST    RE,TTLSP1                                                        
         L     RE,TTLCS1                                                        
         A     RE,TSTACS1                                                       
         ST    RE,TTLCS1                                                        
         L     RE,TTLSP2           ACCUMULATE COMBO FIGURES                     
         A     RE,TSTASP2                                                       
         ST    RE,TTLSP2                                                        
         L     RE,TTLCS2                                                        
         A     RE,TSTACS2                                                       
         ST    RE,TTLCS2                                                        
         L     RE,TTLSP1           ACCUMULATE REGULAR FIGURES                   
         A     RE,TSTPSP1                                                       
         ST    RE,TTLSP1                                                        
         L     RE,TTLCS1                                                        
         A     RE,TSTPCS1                                                       
         ST    RE,TTLCS1                                                        
         L     RE,TTLSP2           ACCUMULATE COMBO FIGURES                     
         A     RE,TSTPSP2                                                       
         ST    RE,TTLSP2                                                        
         L     RE,TTLCS2                                                        
         A     RE,TSTPCS2                                                       
         ST    RE,TTLCS2                                                        
*                                                                               
         ZAP   COUNT,=P'0'         CLEAR STATION ACCUMS                         
         XC    TSTASP1(16),TSTASP1                                              
         XC    TSTASP2(16),TSTASP2                                              
*                                                                               
PSEXT    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*  ROUTINE PRINTS TOTAL PER STATION                                             
FINALPRT CSECT                                                                  
         NMOD1 0,*FINA*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         ZAP   LINE,=P'100'        FORCE PAGE BREAK                             
         MVC   TITLE+11(20),=C'A.U.R. UPDATE REPORT'                            
         MVC   MID1+50(11),=C'FINAL TOTAL'                                      
         MVC   MID2+50(12),=15CL1'-'                                            
         MVC   P(17),=CL17'** TOTAL SPOTS **'                                   
         EDIT  (4,TTLSP1),(6,P+20)                                              
         MVC   P+30(19),=CL19'** TOTAL DOLLARS **'                              
         EDIT  (4,TTLCS1),(12,P+50)                                             
         MVC   P+65(17),=CL17'** COMBO SPOTS **'                                
         EDIT  (4,TTLSP2),(6,P+85)                                              
         MVC   P+94(19),=CL19'** COMBO DOLLARS **'                              
         EDIT  (4,TTLCS2),(12,P+116)                                            
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P(31),=CL31'** NUMBER OF RECORDS ADDED **'                       
         EDIT  (P5,TACOUNT),(8,P+32)                                            
         GOTO1 =V(PRINTER)                                                      
         MVC   P(31),=CL31'** NUMBER OF RECORDS CHANGED **'                     
         EDIT  (P5,TCCOUNT),(8,P+32)                                            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
FNEXT    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CARD INPUT PROCESSING AND BASIC SETUP                                       
*                                                                               
INITIAL  NMOD1 0,*INIT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     RE,=A(IO1)                                                       
         ST    RE,AIO1                                                          
         ST    RE,AIO                                                           
         L     RE,=A(IO2)                                                       
         ST    RE,AIO2                                                          
         L     RE,=A(IO3)                                                       
         ST    RE,AIO3                                                          
         L     RE,=A(RCVBLK)                                                    
         ST    RE,ARCVBLK                                                       
         GOTO1 =V(DATCON),(R1),(5,WORK),(2,TODAY2)                              
         BAS   R5,GETMONDT         GET MONDAY DATE                              
         MVC   NEW02ELT(04),=C'020B'                                            
*                                  INITIALIZE NEW 02 ELEMENT                    
         MVI   RECOVERY,C'T'       SET RECOVERY SOURCE TO TAPE                  
*                                                                               
INIT0020 GOTO1 =V(CARDS),DMCB,WORK,=C'RE00'                                     
         CLC   =C'/*',WORK                                                      
         BE    INIT0240                                                         
         CLC   =C'WRITE=Y',WORK                                                 
         BNE   *+16                                                             
         MVI   WRITEFLG,C'Y'                                                    
         MVI   FLIST,C'U'                                                       
         B     INIT0020                                                         
*                                                                               
         CLC   =C'DATE=',WORK                                                   
         BNE   INIT0040                                                         
         GOTO1 =V(DATVAL),DMCB,(0,WORK+5),DUB                                   
         OC    DMCB(4),DMCB                                                     
         BZ    INIT0200                                                         
         GOTO1 =V(DATCON),(R1),(0,DUB),(2,TODAY2)                               
         BAS   R5,GETMONDT         GET MONDAY DATE                              
         B     INIT0020                                                         
*                                                                               
INIT0040 CLC   =C'MED=',WORK                                                    
         BNE   *+14                                                             
         MVC   MED,WORK+4                                                       
         B     INIT0020                                                         
*                                                                               
         CLC   =C'REP=',WORK                                                    
         BNE   *+14                                                             
         MVC   REP,WORK+4                                                       
         B     INIT0020                                                         
*                                                                               
         CLC   =C'STE=',WORK                                                    
         BNE   INIT0060                                                         
         MVC   STAGEFIL,WORK+4                                                  
         CLI   STAGEFIL+6,C'T'                                                  
         BNE   INIT0020                                                         
         MVI   STAGEFIL+6,C' '                                                  
         B     INIT0020                                                         
*                                                                               
INIT0060 CLC   =C'STA=',WORK                                                    
         BNE   INIT0080                                                         
         MVC   STAGFIL,WORK+4                                                   
         CLI   STAGFIL+6,C'T'                                                   
         BNE   INIT0020                                                         
         MVI   STAGFIL+6,C' '                                                   
         B     INIT0020                                                         
INIT0080 EQU   *                                                                
         CLC   =C'ID=',WORK                                                     
         BNE   INIT0100                                                         
         MVC   SAVNAME,WORK+3                                                   
         B     INIT0020                                                         
INIT0100 EQU    *                                                               
         CLC   =C'LOCAL=',WORK                                                  
         BNE   INIT0120                                                         
         MVC   SAVLOCAL,WORK+6                                                  
         B     INIT0020                                                         
INIT0120 EQU   *                                                                
         CLC   =C'FILTAGY=',WORK   FILTER ON AGENCY?                            
         BNE   INIT0140            NO                                           
         MVC   FILTAGY,WORK+8      YES - SAVE AGENCY FILTER                     
         B     INIT0020                                                         
INIT0140 EQU   *                                                                
         CLC   =C'FILTADV=',WORK   FILTER ON ADVERTISER?                        
         BNE   INIT0160            NO                                           
         MVC   FILTAGY,WORK+8      YES - SAVE ADVERTISER FILTER                 
         B     INIT0020                                                         
INIT0160 EQU   *                                                                
         CLC   =C'DUMPID=',WORK    DUMP INSTRUCTION?                            
         BNE   INIT0180            NO                                           
         MVC   DUMPID,WORK+7       YES - SAVE ADVERTISER FILTER                 
         MVC   P(12),=C'DUMP OPTION:'                                           
         MVC   P+14(08),DUMPID                                                  
         GOTO1 =V(PRINTER)                                                      
         B     INIT0020                                                         
INIT0180 EQU   *                                                                
         CLC   =C'RECOVER=',WORK   RECOVERY SOURCE?                             
         BNE   INIT0200            NO                                           
         MVC   RECOVERY,WORK+8     YES - SAVE RECOVERY SOURCE                   
         B     INIT0020                                                         
*                                                                               
INIT0200 MVC   P(30),=C'*** UNKNOWN PARAMETER CARD ***'                         
INIT0220 GOTO1 =V(LOGIO),DMCB,1,(30,P)                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),WORK                                                       
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
         EJECT                                                                  
*                                                                               
* GET A MONDAY DATE      -- USES DUB/FULL(6)/HALF --                            
*                                                                               
GETMONDT GOTO1 =V(DATCON),DMCB,(2,TODAY2),DUB                                   
         GOTO1 =V(GETDAY),(R1),DUB,FULL                                         
         ZIC   RE,0(R1)            DAY NUMBER OF AIR DATE                       
         LA    R0,1                USE THE START DAY'S NUMBER                   
         SR    R0,RE               DIFFERENCE BETWEEN THE TWO DAYS              
         BZ    MOEXT               THERE IS NONE                                
         GOTO1 =V(ADDAY),(R1),DUB,FULL,(R0)                                     
         GOTO1 =V(DATCON),(R1),(0,FULL),(2,TODAY2)                              
MOEXT    BR    R5                                                               
         EJECT                                                                  
*                                                                               
INIT0240 OC    REP,REP                                                          
         BNZ   INIT0260                                                         
         MVC   P(30),=CL30'** REP CARD MISSING (REP=) **'                       
         B     INIT0220                                                         
*                                                                               
INIT0260 L     R1,=A(BUFFALOC)                                                  
         ST    R1,BUFFC                                                         
         GOTO1 =V(COVAIL),DMCB,C'SETB',3000,600000,BUFFC                        
         ICM   R1,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFC,DMCB+12                                                    
         GOTO1 =V(BUFFALO),DMCB,=C'SET',BUFFC                                   
*                                                                               
         CLI   RECOVERY,C'T'       RECOVERY SOURCE = TAPE?                      
         BNE   INIT0280            NO  - OPEN DISK INPUT                        
         OPEN  (RECVIN,(INPUT))    YES - OPEN TAPE INPUT                        
         LTR   RF,RF                                                            
         BZ    INIT0300                                                         
         DC    H'0'                                                             
INIT0280 EQU   *                                                                
         OPEN  (RECDSKIN,(INPUT))  OPEN DISK INPUT                              
         LTR   RF,RF                                                            
         BZ    INIT0300                                                         
         DC    H'0'                                                             
INIT0300 EQU   *                                                                
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020REAURUPA  07/07/14'                                      
         END                                                                    
