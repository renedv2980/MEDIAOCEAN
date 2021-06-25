*          DATA SET REAURCRA   AT LEVEL 106 AS OF 05/01/02                      
*PHASE REAURCRA,*                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE BUFFAHI                                                                
*INCLUDE CARDS                                                                  
*INCLUDE COVAIL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE REPVALMN                                                               
*INCLUDE DMDMGRL                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE LOCKSPC                                                                
         TITLE 'REAURCR - REPPAK AVERAGE UNIT RATE FILE CREATE'                 
**********************************************************************          
*  HISTORY OF CHANGES                                                *          
**********************************************************************          
*  04MAY/93 (BU ) --- ORIGINAL ENTRY                                 *          
*                                                                    *          
*  16AUG/93 (BU ) --- COMMENT THE CHKTIM ROUTINE.                    *          
*                                                                    *          
*  17FEB96  (BU ) --- FOR SELTEL ORDERS, WHERE BUY ELEMENT DATES     *          
*                     ARE OUTSIDE THE CONTRACT FLIGHT DATES, SKIP    *          
*                     THE ELEMENTS.                                  *          
*                                                                    *          
*  19MAY99  (AST) --- CHANGE PACING TO RELY ON CREATE DATE OF BUYS   *          
*                       INSTEAD OF AS AT DATES, DON'T CREATE AGENCY  *          
*                       OR OFFICE AUR RECORDS ANYMORE                *          
*                                                                    *          
*  02SEP99  (BU ) --- TURN OFF 'FIRST-TIME=NO' FLAG FOR DAYPART      *          
*                     COMPARISON                                     *          
*                                                                    *          
*  04OCT99  (BU ) --- SKIP CONTRACT WITH BAD FLIGHT DATES            *          
*                                                                    *          
*  14JUN00  (BU ) --- TEST VERSION                                   *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
REAURCR  CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NBASE 0,REAURCR,VREGSAVE,R9                                            
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING REAURCR+4096,RC,RA                                               
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         XC    WORK,WORK                                                        
         ST    RB,WORK                                                          
         L     R4,=V(STXITER)                                                   
         ST    R4,WORK+4                                                        
         OI    WORK+4,X'80'                                                     
         GOTO1 =V(STXITER),DMCB,WORK                                            
         GOTO1 =V(DATCON),(R1),(5,WORK),(2,TODAY2)                              
         GOTO1 =V(DATCON),(R1),(5,WORK),(3,AGO3YRS)                             
         ZIC   RF,AGO3YRS          TAKE YEAR                                    
         S     RF,=F'3'            BACK UP THREE YEARS                          
         STC   RF,AGO3YRS                                                       
         BAS   R5,GETMONDT                                                      
*                                                                               
INPU0010 GOTO1 =V(CARDS),DMCB,WORK,=C'RE00'                                     
         CLC   =C'/*',WORK         END OF CARD INPUT?                           
         BE    INPU0080            YES                                          
*                                                                               
*   PROCESS CARD INPUT (OPTION SETTINGS)                                        
*                                                                               
         CLC   =C'WRITE=YES',WORK                                               
         BNE   *+16                                                             
         MVI   WRITEFLG,C'Y'                                                    
         MVI   FLIST,C'U'                                                       
         B     INPU0010                                                         
*                                                                               
         CLC   =C'MED=',WORK                                                    
         BNE   *+14                                                             
         MVC   MED,WORK+4                                                       
         B     INPU0010                                                         
*                                                                               
         CLC   =C'REP=',WORK                                                    
         BNE   *+14                                                             
         MVC   REP,WORK+4                                                       
         B     INPU0010                                                         
*                                                                               
         CLC   =C'PURGE=ALL',WORK                                               
         BNE   *+12                                                             
         MVI   PRGALL,C'Y'                                                      
         B     INPU0010                                                         
*                                                                               
*        CLC   =C'RECREATE=YES',WORK                                            
*        BNE   *+12                                                             
*        MVI   RECRTE,C'Y'                                                      
*        B     INPU0010                                                         
*                                                                               
*        CLC   =C'UPDATE=YES',WORK                                              
*        BNE   *+12                                                             
*        MVI   WUPDTE,C'Y'                                                      
*        B     INPU0010                                                         
*                                                                               
         CLC   =C'DELETE=YES',WORK                                              
         BNE   *+12                                                             
         MVI   DELFLG,C'Y'                                                      
         B     INPU0010                                                         
*                                                                               
         CLC   =C'FILTAGY=',WORK   FILTER ON AN AGENCY?                         
         BNE   INPU0012            NO                                           
         MVC   FILTAGY,WORK+8      YES - SAVE AGENCY FILTER                     
         B     INPU0010                                                         
*                                                                               
INPU0012 EQU   *                                                                
         CLC   =C'FILTADV=',WORK   FILTER ON AN ADVERTISER?                     
         BNE   INPU0014            NO                                           
         MVC   FILTADV,WORK+8      YES - SAVE ADVERTISER FILTER                 
         B     INPU0010                                                         
*                                                                               
INPU0014 EQU   *                                                                
         CLC   =C'DUMPID=',WORK    DUMP INSTRUCTION?                            
         BNE   INPU0016            NO                                           
         MVC   DUMPID,WORK+7       YES - SAVE DUMP ID                           
         B     INPU0010                                                         
*                                                                               
INPU0016 EQU   *                                                                
         CLC   =C'DATE=',WORK                                                   
         BNE   INPU0020                                                         
         GOTO1 =V(DATVAL),DMCB,(0,WORK+5),DUB                                   
         OC    DMCB(4),DMCB                                                     
         BZ    INPU0060                                                         
         GOTO1 =V(DATCON),(R1),(0,DUB),(2,TODAY2)                               
         BAS   R5,GETMONDT         GET MONDAY DATE                              
         B     INPU0010                                                         
*                                                                               
INPU0020 CLC   =C'STE=',WORK       LAST STATION TO PROCESS?                     
         BNE   INPU0030            NO                                           
         MVC   STAGEFIL,WORK+4                                                  
         CLI   STAGEFIL+6,C'T'                                                  
         BNE   INPU0010                                                         
         MVI   STAGEFIL+6,C' '                                                  
         B     INPU0010                                                         
*                                                                               
INPU0030 CLC   =C'STA=',WORK       FIRST STATION TO PROCESS?                    
         BNE   INPU0040            NO                                           
         MVC   STAGFIL,WORK+4                                                   
         CLI   STAGFIL+6,C'T'                                                   
         BNE   INPU0010                                                         
         MVI   STAGFIL+6,C' '                                                   
         B     INPU0010                                                         
*                                                                               
INPU0040 EQU   *                                                                
         CLC   =C'ID=',WORK        ID FOR SYSTEM # ASSIGNMENT?                  
         BNE   INPU0050            NO                                           
         MVC   SAVNAME,WORK+3                                                   
         B     INPU0010                                                         
*                                                                               
INPU0050 EQU   *                                                                
         CLC   =C'LOCAL=',WORK     TESTING:  DUMP INDICATOR                     
         BNE   INPU0055                                                         
         MVC   SAVLOCAL,WORK+6                                                  
         B     INPU0010                                                         
*                                                                               
INPU0055 EQU   *                                                                
         CLC   =C'1STDATE=',WORK                                                
         BNE   INPU0058                                                         
         GOTO1 =V(DATVAL),DMCB,(0,WORK+8),DUB                                   
         OC    DMCB(4),DMCB                                                     
         BZ    INPU0060                                                         
         GOTO1 =V(DATCON),(R1),(0,DUB),(3,EARLYDAT)                             
         B     INPU0010                                                         
INPU0058 EQU   *                                                                
         CLC   =C'LASTDATE=',WORK                                               
         BNE   INPU0059                                                         
*        BNE   INPU0060                                                         
         GOTO1 =V(DATVAL),DMCB,(0,WORK+9),DUB                                   
         OC    DMCB(4),DMCB                                                     
         BZ    INPU0060                                                         
         GOTO1 =V(DATCON),(R1),(0,DUB),(3,LASTDATE)                             
         B     INPU0010                                                         
*                                                                               
INPU0059 CLC   =C'XP=',WORK                                                     
         BE    INPU0080                                                         
INPU0060 MVC   P(30),=C'*** UNKNOWN PARAMETER CARD ***'                         
INPU0070 GOTO1 =V(LOGIO),DMCB,1,(30,P)                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),WORK                                                       
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
*                                                                               
INPU0080 OC    REP,REP                                                          
         BNZ   INPU0090                                                         
         MVC   P(30),=CL30'** REP CARD MISSING (REP=) **'                       
         B     INPU0070                                                         
*                                                                               
INPU0090 L     R1,=A(BUFFALOC)                                                  
         ST    R1,BUFFC                                                         
         GOTO1 =V(COVAIL),DMCB,C'SETB',3000,600000,BUFFC                        
         ICM   R1,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFC,DMCB+12                                                    
         GOTO1 =V(BUFFALO),DMCB,=C'SET',BUFFC                                   
*                                                                               
*   OPEN CONTROL SYSTEM TO ACCESS CONTROL FILE IDENTIFICATION                   
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',AREC,0                                             
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL FILE ID RECORD                  
         MVC   WORK+15(10),SAVNAME LOAD AGENCY NAME                             
         OC    WORK+15(10),SPACES  SET REMAINDER TO SPACES                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',WORK,AREC                 
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R1,AREC                                                          
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
INPU0100 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   INPU0110            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    INPU0120            YES                                          
INPU0110 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   INPU0100            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
INPU0120 EQU   *                                                                
         MVC   UTL+4(1),3(R1)      OVERRIDE CONTROL FILE UTL                    
*                                     WITH REP UTL CODE                         
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'REP',FLIST,AREC                   
*                                                                               
         CLI   SAVLOCAL,C'D'       LOCAL REQUEST = DUMP?                        
         BNE   INPU0130            NO                                           
         DC    H'0'                YES - BLOW IT UP                             
INPU0130 EQU   *                                                                
         CLI   DELFLG,C'Y'         DELETE THIS WEEK ELEMENTS ONLY               
         BE    INPU0140                                                         
         CLI   WUPDTE,C'Y'         WEEKLY RE-CREATE                             
         BE    INPU0140                                                         
         CLI   PRGALL,C'Y'         PURGE                                        
         BE    INPU0140                                                         
         OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INPU0140 EQU   *                                                                
         MVI   DATADISP+1,34       ACCESS DEFAULT SDD RECORD                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSDDKEY,R4                                                       
         MVI   RSDDKTYP,X'26'                                                   
         MVC   RSDDKREP(2),REP     REP                                          
         MVC   RSDDKSTA(4),=4XL1'FF'                                            
*                                  INSERT DEFAULT STATION                       
         GOTO1 HIGH                                                             
         CLC   KEY(26),KEYSAVE     DEFAULT FOUND?                               
         BNE   INPU0160            NO                                           
         GOTO1 GETREC              YES - RETRIEVE IT                            
*                                                                               
*   IF TWO DEFAULTS ARE SET UP, TV IS FOLLOWED BY RADIO.                        
*                                                                               
         CLI   RSDDKSTA+4,C' '     IS IT TV?                                    
         BNE   INPU0150            NO  - PROCESS RADIO                          
         L     R6,AREC             YES - STORE IT AWAY                          
         LA    R2,DEFSDDT                                                       
         BAS   RE,SVSDD            SET UP TV DEFAULT                            
         DROP  R4                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT RECORD                             
         CLC   KEY(26),KEYSAVE     RADIO DEFAULT FOUND?                         
         BNE   INPU0160            NO  -                                        
         GOTO1 GETREC              YES - STORE IT AWAY                          
INPU0150 LA    R2,DEFSDDR                                                       
         L     R6,AREC                                                          
         BAS   RE,SVSDD            SET UP RADIO DEFAULT                         
*                                                                               
INPU0160 MVI   FRST,C'Y'                                                        
*                                                                               
         CLI   PRGALL,C'Y'                                                      
         BE    INPU0170                                                         
         CLI   DELFLG,C'Y'         DELETE THIS WEEK ELEMENTS ONLY               
         BE    INPU0170                                                         
         CLI   RECRTE,C'Y'         RECREATE DELETE ALL ELEMENTS                 
         BE    INPU0170                                                         
         CLI   WUPDTE,C'Y'         WEEKLY RE-CREATE                             
         BNE   MAIN0010                                                         
INPU0170 XC    KEY,KEY                                                          
         MVI   KEY,X'2C'                                                        
         MVC   KEY+4(2),REP        REP                                          
         OC    STAGFIL,STAGFIL     A STATION                                    
         BZ    *+10                                                             
         MVC   KEY+6(7),STAGFIL                                                 
INPU0180 GOTO1 HIGH                                                             
         B     INPU0200                                                         
*                                                                               
INPU0190 GOTO1 SEQ                                                              
INPU0200 CLC   KEY(6),KEYSAVE      DELETE THRU REP                              
         BNE   INPU0270                                                         
         OC    STAGFIL,STAGFIL     A STATION                                    
         BZ    *+14                                                             
         CLC   KEY+6(7),STAGFIL                                                 
         BNE   INPU0270                                                         
*                                                                               
         OC    MED,MED                                                          
         BZ    INPU0220                                                         
         CLI   MED,C'T'                                                         
         BNE   INPU0210                                                         
         CLI   KEY+12,C' '                                                      
         BE    INPU0220                                                         
         ZIC   RE,KEY+11           NOT EQUAL TO MEDIA SKIP STATION              
         LA    RE,1(RE)                                                         
         STC   RE,KEY+11                                                        
         MVI   KEY+12,C' '                                                      
         XC    KEY+13(14),KEY+13                                                
         B     INPU0180                                                         
*                                                                               
INPU0210 CLI   KEY+12,C' '         BLANK = TV ALL OTHERS RADIO                  
         BNE   INPU0220                                                         
         ZIC   RE,KEY+12           NOT EQUAL TO MEDIA SKIP MEDIA                
         LA    RE,1(RE)                                                         
         STC   RE,KEY+12                                                        
         XC    KEY+13(14),KEY+13                                                
         B     INPU0180                                                         
*                                                                               
* PURGE DELETE KEY                                                              
INPU0220 CLI   PRGALL,C'Y'                                                      
         BNE   INPU0230                                                         
         OI    KEY+27,X'80'                                                     
         AP    DELCNT,=P'1'                                                     
         CLI   WRITEFLG,C'Y'                                                    
         BNE   *+12                                                             
         BAS   RE,WRITE                                                         
         B     INPU0190                                                         
         GOTO1 =V(HEXOUT),DMCB,KEY,P,30,=C'T0G'                                 
         GOTO1 =V(PRINTER)                                                      
         B     INPU0190                                                         
*                                                                               
INPU0230 GOTO1 GETREC                                                           
         L     R2,AREC                                                          
         CLI   RECRTE,C'Y'         RECREATE DELETE ALL ELEMENTS                 
         BNE   INPU0240                                                         
*                                                                               
* DELETE ELEMENTS AND WRITE BACK RECORD                                         
         USING RAURRECD,R2                                                      
*                                     ELEM 2 IS VARIABLE LENGTH:                
*                                        NEED TO DETERMINE L(1ST ELT)           
         LR    R6,R2                                                            
         MVI   ELCODE,X'02'        RETRIEVE SPOT/COST ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    INPU0236            FOUND                                        
         DC    H'0'                SHOULDN'T HAPPEN                             
INPU0236 EQU   *                                                                
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,=H'47'           KEY(27) + CNTRL(7) + ELEM1(12) + 1           
         AR    RE,RF               ADD VALUES                                   
         LA    R6,0(RF,R6)                                                      
         MVI   0(R6),0             ZERO NEW END OF RECORD                       
         CLI   WRITEFLG,C'Y'                                                    
         BNE   INPU0260                                                         
         BAS   RE,PUTREC                                                        
         B     INPU0190                                                         
*                                                                               
*     RE-UPDATE 1 WEEK ELEMENTS                                                 
*                                                                               
INPU0240 LR    R6,R2                                                            
         SR    R3,R3               R3 WILL = LENGTH TO MOVE                     
         ICM   R3,3,RAURLEN                                                     
         SH    R3,=H'46'           - KEY(27) CONTROL(7) ELEM 1(12)              
*                                                                               
*     DELETE ELEMENT AND WRITE BACK RECORD                                      
*                                                                               
         MVI   ELCODE,X'02'        RETRIEVE SPOT/COST ELEMENT                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
INPU0250 BAS   RE,NEXTEL                                                        
         BNE   INPU0190            NOT FOUND                                    
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         SR    R3,RF               SUBTRACT L(ELT TO BE DROPPED)                
         CLC   TODAY2,2(R6)        CREATE DATE = TODAY?                         
         BNE   INPU0250            NO  - GO BACK FOR NEXT                       
         SR    RE,RE               YES -                                        
         ICM   RE,3,RAURLEN        RETRIEVE RECORD LENGTH                       
         SR    RE,RF               SUBTRACT ELEMENT LENGTH                      
         STCM  RE,3,RAURLEN        RESET RECORD LENGTH                          
         LTR   R3,R3                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    RE,0(RF,R6)         MOVE RECORD DOWN                             
         LR    R2,R6                                                            
         LR    RF,R3                                                            
         MVCL  R2,RE               0 ALREADY AT END OF RECORD                   
         AP    DCOUNT,=P'1'                                                     
         CLI   WRITEFLG,C'Y'       GENERATE OUTPUT?                             
         BNE   INPU0260            NO  - DISPLAY AFFECTED RECS                  
         BAS   RE,PUTREC           YES - DON'T DISPLAY                          
         B     INPU0190                                                         
*                                                                               
INPU0260 L     R4,AREC                                                          
         LR    R6,R4                                                            
         MVI   ELCODE,X'02'        RETRIEVE SPOT/COST ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    INPU0262            FOUND                                        
         DC    H'0'                SHOULDN'T HAPPEN                             
INPU0262 EQU   *                                                                
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    R5,=H'47'           KEY(27) + CNTRL(7) + ELEM1(12) + 1           
         AR    R5,RF               ADD VALUES                                   
         ZIC   RF,DISPCTR          DISPLAY COUNTER                              
         CH    RF,=H'100'          IS IT 100?                                   
         BE    INPU0264            YES - PRINT IT OUT                           
         LA    RF,1(RF)            NO  - INCREMENT                              
         STC   RF,DISPCTR                                                       
         B     INPU0190                                                         
INPU0264 EQU   *                                                                
         XC    DISPCTR,DISPCTR     RESET COUNTER                                
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R5),=C'2D'                
         B     INPU0190                                                         
         DROP  R2                                                               
*                                                                               
INPU0270 CLI   PRGALL,C'Y'                                                      
         BNE   INPU0280                                                         
         MVC   P(30),=30C'*'                                                    
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P(26),=CL24'* # OF DELETED RECORDS *'                            
         EDIT  (P5,DELCNT),(8,P+30)                                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P(30),=30C'*'                                                    
         GOTO1 =V(PRINTER)                                                      
         B     MAIN0140                                                         
*                                                                               
INPU0280 CLI   RECRTE,C'Y'                                                      
         BE    MAIN0010                                                         
         MVC   P(26),=CL26'* # OF ELEMENTS DELETED *'                           
         EDIT  (P5,DCOUNT),(8,P+30)                                             
         GOTO1 =V(PRINTER)                                                      
         CLI   DELFLG,C'Y'                                                      
         BE    MAIN0140                                                         
         EJECT                                                                  
*****************************************************************               
*                                                               *               
*            -----     MAIN LINE CODE     -----                 *               
*                                                               *               
*            -----       END = MAIN0120   -----                 *               
*                                                               *               
* READ CONTRACTS BY STATION - FOR EACH CONTRACT READ BUYS -     *               
* PUT BUFFALO RECS (DPT/PRGT/SPL/YM/CTYP/AGY/OFF/SPOT/DOLRS) -  *               
* READ UNTIL A CONTRACT STATION BREAK OCCURS -                  *               
* READ BUFFALO RECS & ADD A.U.R. RECORDS - PRINT STATION TOTALS *               
* RESET BUFFALO - START NEXT STATION                            *               
*                                                               *               
*****************************************************************               
         SPACE 2                                                                
MAIN0010 EQU   *                                                                
         XC    KEY,KEY              SET UP CONTRACT KEY                         
         MVC   KEY(2),=X'0C00'           FOR                                    
         MVC   KEY+2(2),REP                REP                                  
         MVC   KEY+4(7),STAGFIL              STATION                            
MAIN0020 GOTO1 HIGH                GET FIRST                                    
         B     MAIN0040                                                         
*                                                                               
MAIN0030 EQU   *                                                                
         GOTO1 SEQ                 GET NEXT                                     
MAIN0040 EQU   *                                                                
         CLC   KEY(4),KEYSAVE      SAME REC TYPE AND REP?                       
         BNE   MAIN0120            NO  - FINISHED                               
*                                                                               
*   SPECIAL TEST                                                                
***      CLC   =X'02910218',KEY+23 SELECT ONLY ONE CONTRACT                     
***      BNE   MAIN0030            SKIP ALL OTHERS                              
*   SPECIAL TEST END                                                            
*                                                                               
         TM    KEY+27,X'80'        SKIP DELETES                                 
         BO    MAIN0030                                                         
*                                                                               
         OC    FILTAGY,FILTAGY     ANY AGENCY FILTER?                           
         BZ    MAIN0042            NO                                           
         CLC   FILTAGY,KEY+13      SAME AGENCY?                                 
         BNE   MAIN0030            NO  - SKIP IT                                
MAIN0042 EQU   *                                                                
         OC    FILTADV,FILTADV     ANY ADVERTISER FILTER?                       
         BZ    MAIN0044            NO                                           
         CLC   FILTADV,KEY+19      SAME ADVERTISER?                             
         BNE   MAIN0030            NO  - SKIP IT                                
MAIN0044 EQU   *                                                                
*        *                                                                      
*   TEST                                                                        
*        OC    REJSTA,REJSTA                                                    
*        BZ    TEST0001                                                         
*        MVC   P+1(12),=C'REJSTA/KEY+4'                                         
*        MVC   P+15(07),REJSTA                                                  
*        MVC   P+25(27),KEY                                                     
*        GOTO1 =V(PRINTER)                                                      
TEST0001 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         CLC   REJSTA,KEY+4        GRP/SUBGRP,STA/MED = REJECTED?               
         BNE   MAIN0050            NO                                           
         XC    REJSTA,REJSTA       YES - CLEAR REJECTED                         
         ZIC   RE,KEY+10           BUMP UP MEDIA TO SKIP STATION                
         LA    RE,1(RE)                                                         
         STC   RE,KEY+10                                                        
         XC    KEY+11(16),KEY+11   CLEAR REST OF KEY                            
*        *                                                                      
*   TEST                                                                        
*        MVC   P+1(12),=C'SKIP RESTART'                                         
*        MVC   P+15(27),KEY                                                     
*        GOTO1 =V(PRINTER)                                                      
*   TEST END                                                                    
*                                                                               
         B     MAIN0020            GO BACK AND DO 'HIGH'                        
*                                                                               
MAIN0050 OC    MED,MED             ANY MEDIA ENTERED?                           
         BZ    MAIN0070            NO                                           
         CLI   MED,C'T'            YES - TV?                                    
         BNE   MAIN0060            NO  - LOOK AT RADIO                          
         CLI   KEY+10,C' '         YES - CHECK CONTRACT FOR TV                  
         BE    MAIN0070            TV CONTRACT - CONTINUE PROCESS               
         ZIC   RE,KEY+9            BUMP UP MEDIA TO SKIP STATION                
         LA    RE,1(RE)                                                         
         STC   RE,KEY+9                                                         
         MVI   KEY+10,C' '                                                      
         XC    KEY+11(16),KEY+11   CLEAR REST OF KEY                            
         B     MAIN0020            GO BACK AND DO 'HIGH'                        
*                                                                               
*                                  CHECK CONTRACT FOR RADIO                     
MAIN0060 CLI   KEY+10,C' '         BLANK = TV                                   
         BNE   MAIN0070            NOT TV                                       
         ZIC   RE,KEY+10           TV  - BUMP UP MEDIA TO SKIP STATION          
         LA    RE,1(RE)                                                         
         STC   RE,KEY+10                                                        
         XC    KEY+11(16),KEY+11   CLEAR REST OF KEY                            
         B     MAIN0020            GO BACK AND DO 'HIGH'                        
*                                                                               
MAIN0070 OC    STAGEFIL,STAGEFIL   ANY STATION END ENTERED?                     
         BZ    MAIN0080            NO                                           
         CLC   KEY+4(7),STAGEFIL   YES - IS RUN PAST END?                       
         BH    MAIN0120            YES - PROCESS NO FURTHER                     
         B     MAIN0090            NO  - PROCESS FURTHER                        
*                                                                               
MAIN0080 OC    STAGFIL,STAGFIL     ANY STATION START ENTERED?                   
         BZ    *+14                NO  - DON'T TEST START                       
         CLC   KEY+4(7),STAGFIL    YES - CHECK CONTRACT STATION                 
         BNE   MAIN0120            NOT SAME - FINISHED                          
*                                                                               
MAIN0090 LA    RE,REC2             SET IO AREA TO SECOND AREA                   
         ST    RE,AREC                                                          
         GOTO1 GETREC              RETRIEVE CONTRACT RECORD                     
         LA    RE,REC              RESET IO AREA TO FIRST AREA                  
         ST    RE,AREC                                                          
         L     RF,CONCTR           INCREMENT COUNTERS                           
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
         L     RF,CONCTR2                                                       
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR2                                                       
         CLC   CONCTR2,=F'500'     DISPLAY EVERY N RECORDS                      
*                                                                               
         B     MAIN0095            UNCONDITIONAL BRANCH                         
*                                                                               
         BNE   MAIN0095            COUNTER BRANCH                               
***      B     MAIN0120            PROCESS FIRST N RECORDS, END                 
*                                                                               
         MVC   P+1(08),=C'STATION:'                                             
         MVC   P+18(20),=C'CONTRACTS PROCESSED:'                                
         MVC   P+10(6),REC2+4                                                   
         EDIT  CONCTR,(8,P+40)                                                  
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,WORK                                                          
         GOTO1 =V(HEXOUT),DMCB,WORK,P+55,4,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
         XC    CONCTR2,CONCTR2                                                  
MAIN0095 EQU   *                                                                
         CLI   FRST,C'Y'           FIRST TIME?                                  
         BE    MAIN0100            YES                                          
         CLC   KEY+4(7),SVKEY+SVGSTAE                                           
*                                  NO  - SAME GS/STA/MED?                       
         BE    MAIN0100            YES                                          
         MVC   HLDKEY,KEY          NO  - SAVE KEY TEMPORARILY                   
         GOTO1 =A(ADDAUREC),DMCB,(RC)  ADD NEW DATA                             
         BAS   RE,PRNTSTA          PRINT RESULT                                 
         BAS   RE,SETNXSTA         SET NEXT STATION                             
         MVC   KEY,HLDKEY          RESTORE KEY                                  
*                                                                               
MAIN0100 MVC   SVKEY,KEY           SAVE KEY FOR BREAK TESTING                   
         BAS   RE,SETCNTRT                                                      
*                                  BUILD BC MON TABLE, SET SDD RECORDS          
         BE    MAIN0110            OKAY TO PROCEED                              
         CLI   BYTE,X'FF'          BAD DATES IN CONTRACT?                       
         BE    MAIN0030            YES - SKIP CONTRACT                          
         XC    KEY,KEY             NO SDD - SKIP THIS STATION                   
         MVC   KEY(11),SVKEY       RESET KEY                                    
         ZIC   RE,KEY+10           BUMP THE MEDIA FOR NEXT STATION              
         LA    RE,1(RE)                                                         
         STC   RE,KEY+10                                                        
         XC    KEY+11(16),KEY+11   ZERO REST OF KEY                             
         B     MAIN0020            GO BACK FOR NEXT                             
*                                                                               
MAIN0110 EQU   *                                                                
         BAS   RE,READBUYS                                                      
         MVC   KEY,SVKEY           RE-READ CONTRACT                             
         GOTO1 HIGH                                                             
         CLC   KEY,KEYSAVE                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FRST,C'N'                                                        
         B     MAIN0030                                                         
*                                                                               
MAIN0120 CLI   FRST,C'Y'                                                        
         BNE   MAIN0130                                                         
         MVC   P(25),=C'NO A.U.R. RECORDS CREATED'                              
         GOTO1 =V(PRINTER)                                                      
         B     MAIN0140                                                         
         SPACE                                                                  
MAIN0130 EQU   *                                                                
         PRINT GEN                                                              
         GOTO1 =A(ADDAUREC),DMCB,(RC)                                           
         PRINT NOGEN                                                            
         BAS   RE,PRNTSTA                                                       
         BAS   RE,FINALPRT                                                      
MAIN0140 CLOSE FILEOUT                                                          
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
*  ROUTINE BUILDS MONTH TABLES / EXTRACTS CTYP/AGY/OFF FROM CONTRACT  *         
*  READS FOR STATION SDD RECORD / READS EXISTING AUR RECORDS          *         
*  DELETES ELEMENTS IF UPDATE OPTION - ADDS BUFF REC WITH DKA         *         
***********************************************************************         
SETCNTRT NTR1                                                                   
         LA    R4,REC2                                                          
         USING RCONKEY,R4                                                       
*                                                                               
         CLC   RCONDATE+3(3),AGO3YRS                                            
*                                  FLIGHT ENDS BEFORE CUTOFF DATE?              
         BL    SETC0011            YES - DON'T PROCESS                          
         MVC   SAVCREAT,RCONHDRD   SAVE DATE CONTRACT CREATED                   
         XC    MTHTBL,MTHTBL                                                    
         MVC   CTYP,RCONTYPE       EXTRACT CONTRACT TYPE                        
         MVC   AGY,RCONKAGY        EXTRACT AGENCY/AGENCY OFFICE                 
         MVC   OFF,RCONKOFF        EXTRACT SALES OFFICE                         
         LA    R3,MTHTBL                                                        
*                                                                               
*   FORMAT OF TABLE: (20 OCCURENCES OF FOLLOWING)                               
*     BYTES  1 - 2  =  CALENDAR MONTH (IE:  9211)                               
*     BYTES  3 - 5  =  BROADCAST MONTH START                                    
*     BYTES  5 - 8  =  BROADCAST MONTH END                                      
*                                                                               
         LA    R5,20                                                            
         MVI   BYTE,0              CLEAR FLAG                                   
         GOTO1 =V(DATCON),DMCB,(3,RCONDATE),(0,WORK+20)                         
*                                  CONTRACT FLIGHT START DATE                   
SETC0010 GOTO1 =V(GETBROAD),(R1),WORK+20,WORK,V(GETDAY),V(ADDAY)                
*                                  BCM DATES OF MONTH                           
         CLI   0(R1),X'FF'         ERROR RETURN?                                
         BNE   SETC0012            NO                                           
SETC0011 EQU   *                                                                
         MVI   BYTE,X'FF'          YES - SET SKIP FLAG                          
         B     SETC0040            EXIT                                         
SETC0012 EQU   *                                                                
         GOTO1 =V(DATCON),(R1),(0,WORK),(3,2(R3))                               
*                                  INSERT BCM START INTO TABLE                  
         GOTO1 =V(DATCON),(R1),(0,WORK+6),(3,5(R3))                             
*                                  INSERT BCM END   INTO TABLE                  
         LA    R0,7                GET DATE WITH CORRECT MONTH                  
         GOTO1 =V(ADDAY),(R1),WORK,WORK+30,(R0)                                 
         GOTO1 =V(DATCON),(R1),(0,WORK+30),(3,WORK+38)                          
         MVC   0(2,R3),WORK+38     MOVE IN MONTH YEAR                           
*                                  INSERT CALENDAR MONTH INTO TABLE             
         CLC   RCONDATE+3(3),5(R3) FLIGHT END REACHED?                          
         BNH   SETC0020            YES                                          
         LA    R0,1                NO  - BUMP TO NEXT MONTH                     
         GOTO1 =V(ADDAY),(R1),WORK+6,WORK+20,(R0)                               
         LA    R3,8(R3)            BUMP TO NEXT TABLE POS                       
         BCT   R5,SETC0010         GO BACK FOR NEXT                             
***      DC    H'0'                SOMETHING IS AMISS                           
*                                                                               
*   IF TABLE IS EXCEEDED, PROCESS AS IF END OF TABLE REACHED.                   
*                                                                               
*                                                                               
SETC0020 CLC   SVSTAG,RCONKGRP     SAME GRP/STATION/MEDIA?                      
         BE    SETC0070            YES -                                        
         XC    KEY,KEY             NO  - RETRIEVE SDD RECORD(S)                 
         LA    R6,KEY                                                           
         USING RSDDKEY,R6                                                       
         MVI   RSDDKTYP,X'26'                                                   
         MVC   RSDDKREP,REP        REP                                          
         MVC   RSDDKSTA,RCONKSTA   DEFAULT STATION                              
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     FOUND STATION SDD                            
         BNE   SETC0030                                                         
         TM    KEY+27,X'80'        SKIP DELETED RECORDS                         
         BO    SETC0030                                                         
         GOTO1 GETREC              RETRIEVE RECORD                              
         L     R6,AREC                                                          
         LA    R2,STASDDT          STATION STORAGE ADDRESS                      
         BAS   RE,SVSDD            MOVE IT TO STORAGE                           
*                                                                               
*        GOTO1 =V(HEXOUT),DMCB,STASDDT,P,60,=C'T0G'                             
*        GOTO1 =V(PRINTER)                                                      
*        GOTO1 =V(HEXOUT),DMCB,STASDDT+60,P,60,=C'T0G'                          
*        GOTO1 =V(PRINTER)                                                      
*        GOTO1 =V(HEXOUT),DMCB,STASDDT+120,P,60,=C'T0G'                         
*        GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     SETC0060                                                         
*                                                                               
*                                  NO STATION SDD RECORD                        
SETC0030 LA    R2,DEFSDDT          DEFAULT TV SPACE                             
         CLI   SVKEY+SVSTAME,C' '  TV?                                          
         BE    *+8                 YES                                          
         LA    R2,DEFSDDR          NO  - DEFAULT RADIO SPACE                    
         CLI   0(R2),0             ANY DEFAULT SDD?                             
         BNE   SETC0060            YES - CONTINUE                               
         MVC   P(7),SVKEY+SVGSTAE  NO  - SKIP STATION                           
         MVC   REJSTA,SVKEY+SVGSTAE                                             
*                                  SET 'REJECT STATION' FLAG                    
         MVC   P+9(18),=C'HAS NO SDD RECORD'                                    
*                                  SHOW SKIP ON LISTING                         
         GOTO1 =V(PRINTER)                                                      
SETC0040 LTR   RB,RB                                                            
SETC0050 XIT1                                                                   
*                                                                               
SETC0060 ST    R2,ACURSDDT         SAVE A(CURRENT SDD STORAGE)                  
         MVC   SVSTAG,SVKEY+SVGSTAE                                             
*                                  SAVE CURRENT GRP/STATION/MEDIA               
SETC0070 CR    RE,RE               SET CC = EQUAL                               
         B     SETC0050                                                         
         DROP  R4,R6                                                            
         EJECT                                                                  
*****************************************************************               
*                                                               *               
*  ROUTINE READS BUYS FOR 1 CONTRACT - MATCH DAY/TIMES TO SDD   *               
*  RECORDS. DOES THIS BY 1ST FINDING A MATCH ON DAY/START TIME  *               
*  (CAN ONLY MATCH TO 1) THAN CHECKS END TIME FOR MATCH         *               
*  YES - READS NEXT ELEMENT, NO ADDS 1 TO SDD END TIME, STORES  *               
*  IN START TIME USE DPT SDD TO MATCH ON DAY/TIME - NO MATCH    *               
*  CREATE  PSEUDO(ZZZ) DPT. AFTER THIS MESS IT READS NEXT ELEM  *               
*  AND STARTS OVER USING MATCHED DPT.                           *               
*  CREATES BUFFALO RECORDS (UP TO 4) - READS NEXT BUY           *               
*                      (NOW ONLY  2)                                            
*  END OF BUYS FOR CONTRACT DONE                                *               
*                                                               *               
*****************************************************************               
READBUYS NTR1                                                                   
         XC    KEY,KEY             SET UP FOR BUYS                              
         MVI   KEY,X'0B'                                                        
         MVC   KEY+(RBUYKREP-RBUYKEY)(2),REP                                    
         UNPK  WORK+10(9),SVKEY+SVCON#(5)  CONTRACT NUMBER                      
         PACK  WORK(5),WORK+10(8)                                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),WORK(5)                                               
         MVO   WORK(5),WORK+10(5)                                               
         MVC   KEY+18(4),WORK                                                   
         PACK  KEY+18(1),WORK+3(1) REVERSE THE COMPLIMENT                       
         PACK  KEY+19(1),WORK+2(1)                                              
         PACK  KEY+20(1),WORK+1(1)                                              
         PACK  KEY+21(1),WORK(1)                                                
         DS    0H                                                               
         GOTO1 HIGH                GET FIRST BUY OF CONTRACT                    
         B     REBU0020                                                         
*                                                                               
REBU0010 GOTO1 SEQ                 GET NEXT  BUY OF CONTRACT                    
REBU0020 CLC   KEY(22),KEYSAVE     SAME KEY?                                    
         BNE   REBU0390            NO  - FINISHED                               
         TM    KEY+27,X'80'        SKIP  DELETES                                
         BO    REBU0010                                                         
         CLC   =XL2'FFFF',KEY+25   SKIP PLAN RECORDS                            
         BE    REBU0010                                                         
*                                                                               
         BAS   RE,GETREC           RETRIEVE RECORD                              
         XC    BFREC,BFREC         SET UP BUFFALO RECORD                        
         L     R6,AREC             SET COMBO FLAG IF APPROPRIATE                
         LA    R6,34(R6)           POSITION TO DESCRIPTOR ELEMENT               
         MVI   COMBOREC,C'N'       SET COMBO/REG BUY TO 'REG'                   
         TM    RBUYRTS-RBUYELEM(R6),X'40'                                       
         BNO   REBU0028            OFF = REGULAR BUY                            
         MVI   COMBOREC,C'Y'       ON  = COMBO BUY                              
REBU0028 EQU   *                                                                
         L     R6,AREC             SET USING OVER IO AREA                       
         USING RBUYKEY,R6                                                       
         TM    RBUYCNTL,X'80'      DELETED RECORD?                              
         BO    REBU0010            YES - SKIP IT                                
         CLI   RBUYCHGI,C'C'       CANCELLED RECORD?                            
         BE    REBU0010            YES - SKIP IT                                
         CLI   RBUYCHGI+1,C'C'     CANCELLED RECORD?                            
         BE    REBU0010            YES - SKIP IT                                
**********************************************************************          
         CLC   =C'BUYSIN',DUMPID   DUMP BUY RECS?                               
         BNE   REBU0030            NO                                           
*                                                                               
*   SPECIAL CONTRACT TEST                                                       
         CLC   RBUYKCON,=X'41506779'                                            
         BNE   REBU0030                                                         
         SR    R3,R3                                                 *          
         ICM   R3,3,RBUYLEN                                          *          
         GOTO1 =V(PRNTBL),DMCB,(0,(R6)),(R6),C'DUMP',(R3),=C'2D'     *          
REBU0030 EQU   *                                                                
**********************************************************************          
         MVC   BFSPL,RBUYDUR       SET SPOT LENGTH                              
*** NEW                                                                         
         OC    RBUYCREA,RBUYCREA   ANY CREATION DATE IN BUY?                    
         BNZ   REBU003A            YES                                          
         MVC   RBUYCREA,SAVCREAT   NO  - USE CONTRACT'S CREATE DATE             
REBU003A EQU   *                                                                
         GOTO1 =V(DATCON),DMCB,(3,RBUYCREA),DUB                                 
         GOTO1 =V(GETDAY),(R1),DUB,FULL                                         
         ZIC   RE,0(R1)            DAY NUMBER OF AIR DATE                       
         LA    R0,1                USE THE START DAY'S NUMBER                   
         SR    R0,RE               DIFFERENCE BETWEEN THE TWO DAYS              
         BZ    REBU0031            THERE IS NONE                                
         GOTO1 =V(ADDAY),(R1),DUB,FULL,(R0)                                     
         GOTO1 =V(DATCON),(R1),(0,FULL),(2,BFBYCRDT)                            
         B     REBU0035                                                         
REBU0031 EQU   *                                                                
         GOTO1 =V(DATCON),(R1),(0,DUB),(2,BFBYCRDT)                             
REBU0035 EQU   *                                                                
*** END NEW                                                                     
         MVC   DUB(4),RBUYCOS      SET COST                                     
         MVC   BYTE,RBUYNW         SET NUMBER/WEEK                              
         CLC   RBUYKPLN,=3X'FF'    DEFAULT PLAN (PKG) CODE?                     
         BE    REBU0040            YES                                          
         MVC   BFDPT(2),=X'FEFE'   NO  - SET BUFF DPT/SUBDPT                    
         MVC   BFSPL,=X'FFFF'      SET BUFF SPOT LENGTH                         
         MVI   BFPGT,X'FC'         SET PROG TYPE TO PLAN INDICATOR              
         B     REBU0160                                                         
*                                                                               
REBU0040 EQU   *                                                                
         CLI   RBUYTYP,C' '        ANY PROGRAM TYPE?                            
         BE    REBU0050            NO                                           
         SR    RE,RE               YES - THERE ARE NO DAYPARTS                  
         ICM   RE,1,RBUYTYP        EXTRACT TYPE                                 
         BZ    REBU0050            TYPE IS ZERO -                               
         MVC   BFDPT(2),=X'FEFE'   SET BUFF DPT/SUBDPT                          
         STC   RE,BFPGT            SET PROG TYPE                                
         B     REBU0160                                                         
         DROP  R6                                                               
*                                                                               
REBU0050 L     R6,AREC                                                          
         MVI   ELCODE,X'02'        GET DAY/TIME ELEM                            
         MVI   WORK+5,C'Y'         SET 1ST TIME SWITCH TO 'YES'                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
REBU0060 DC    H'0'                                                             
         BAS   R5,CHKTIM                                                        
*                                  ROUTINE MAY NOT RETURN HERE....              
         L     R2,ACURSDDT         SET A(CURRENT SDD RECORD)                    
*                                                                               
*   TEST PRINT                                                                  
**       MVC   P+1(08),=C'SDD RECD'                                             
**       MVC   P+9(124),0(R2)      LOAD RECORD                                  
**       GOTO1 =V(PRINTER)                                                      
*   TEST PRINT END                                                              
*                                                                               
REBU0070 EQU   *                                                                
         LR    R3,R2               SAVE A(CURRENT SDD RECORD)                   
         MVC   BFDPT(2),0(R2)      SET DAYPART INTO BUFF RECORD                 
         MVC   BFDP2(2),0(R2)      SAVE DAYPART FOR BUFF RECORD                 
*                                                                               
*   TEST PRINT                                                                  
**       MVC   P+1(08),=C'REBU0070'                                             
**       L     RF,AREC                                                          
**       MVC   P+9(27),0(RF)       LOAD KEY                                     
**       MVI   P+36,C'/'                                                        
**       MVC   P+37(2),BFDPT                                                    
**       GOTO1 =V(PRINTER)                                                      
*   TEST PRINT END                                                              
*                                                                               
REBU0080 EQU   *                                                                
         ZIC   R5,2(R2)            LOOP CONTROL: # TIME ELTS FOR DP             
         LA    R2,3(R2)            SET A(1ST TIME ELT OF DP)                    
REBU0090 EQU   *                                                                
*                                                                               
*   TEST PRINT                                                                  
**       MVC   P+1(08),=C'SDD ELT '                                             
**       MVC   P+9(32),0(R2)      LOAD ELT                                      
**       GOTO1 =V(PRINTER)                                                      
*   TEST PRINT END                                                              
*                                                                               
         ZIC   RE,0(R2)            SET DAY OF D/T STRING                        
         EX    RE,REBU0100         DAY OVERLAP                                  
         B     REBU0110                                                         
REBU0100 TM    3(R6),0             BUY D/T ELT VS SDD REC                       
REBU0110 BZ    REBU0120            NO DAYS SET                                  
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
*   TEST PRINT                                                                  
*        MVC   P+1(08),=C'TIMETEST'                                             
*        MVC   P+9(02),WORK       LOAD ELT                                      
*        MVI   P+11,C'/'                                                        
*        MVC   P+12(4),1(R2)                                                    
*        MVC   P+20(32),0(R2)                                                   
*        GOTO1 =V(PRINTER)                                                      
*   TEST PRINT END                                                              
*                                                                               
*                                                                               
         BZ    REBU0120            YES - DOESN'T FIT - SKIP IT                  
         CLC   WORK(2),3(R2)       ELEMENT START TIME > DP END TIME?            
         BH    REBU0120            YES - SKIP THIS DP ELT                       
         CLC   WORK(2),1(R2)       ELEMENT START TIME >= DP START?              
         BNL   REBU0130            YES - PROCESS THIS DP ELT                    
*                                  DAY OR TIME NOT MATCHED                      
*                                                                               
*   TEST PRINT                                                                  
**       MVC   P+1(08),=C'WORKDROP'                                             
**       MVC   P+9(02),WORK       LOAD ELT                                      
**       MVI   P+11,C'/'                                                        
**       MVC   P+12(4),1(R2)                                                    
**       MVC   P+20(32),0(R2)                                                   
**       GOTO1 =V(PRINTER)                                                      
*   TEST PRINT END                                                              
*                                                                               
***>>>NEW                                                                       
         LA    R2,5(R2)            BUMP TO NEXT SDD D/T STRING                  
         CLI   0(R2),0             END OF SDD'S?                                
         BE    REBU0135            YES, DON'T COMPARE ANYMORE                   
         BCT   R5,REBU0090                                                      
***      LA    R2,3(R2)            NO - NOW BUMP TO DAY IN STRING               
         B     REBU0070                                                         
***>>>NEW                                                                       
REBU0120 EQU   *                                                                
         LA    R2,5(R2)            BUMP TO NEXT SDD D/T STRING                  
         BCT   R5,REBU0090         GO BACK FOR NEXT                             
*                                                                               
         CLI   WORK+5,C'Y'         NO MATCH:  FIRST TIME?                       
         BNE   REBU0150            NO                                           
         CLI   0(R2),0             YES - IS THERE ANOTHER DAYPART?              
         BNE   REBU0070            YES - GO BACK AND CHECK IT                   
         B     REBU0150            NO  - START TIME HAS TO FIT                  
*                                                                               
REBU0130 EQU   *                                                                
*                                  TURN OFF 'FIRST TIME = NO'                   
***>>>>> MVI   WORK+5,C'N'         SET 'FIRST TIME' TO NO                       
*DCH0************************                                                   
***      DC    H'0'                                                             
*DCH0************************                                                   
         OC    WORK+2(2),WORK+2    ANY ELEMENT END TIME?                        
         BZ    REBU0140            NO                                           
         CLC   WORK+2(2),3(R2)     YES - ELT END TIME > DP END TIME?            
         BNH   REBU0139            NO  - THIS IS A MATCH                        
* NEW                                                                           
         LA    R2,5(R2)            BUMP TO NEXT SDD D/T STRING                  
         CLI   0(R2),0             END OF SDD'S?                                
         BE    REBU0135            YES, DON'T COMPARE ANYMORE                   
         LA    R2,3(R2)            NO - NOW BUMP TO DAY IN STRING               
REBU0135 EQU   *                                                                
         B     REBU0140                                                         
***      B     REBU0090                                                         
* END NEW                                                                       
*                                                                               
         SR    RF,RF               BUMP DP END TIME                             
         ICM   RF,3,3(R2)          ADD 1 TO END TIME -> NEW START               
         LA    RF,1(RF)            ADJUST TO NEAREST HOUR(?????)                
         LR    R1,RF                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'          CHECK FOR END OF AN HOUR                     
         CH    RE,=H'60'                                                        
         BNE   *+8                                                              
         AH    R1,=H'40'                                                        
         LR    RF,R1                                                            
         STCM  RF,3,WORK                                                        
         LR    R2,R3                                                            
         B     REBU0080            GO BACK AND CHECK NEW TIME                   
*                                                                               
REBU0139 EQU   *                                                                
****     LR    R1,R2               GET ADDRESS OF DAY IN STRING                 
****     SH    R1,=H'3'            BUMP BACK TO DPT CODE                        
         MVC   BFDPT(2),BFDP2      SET DPT INTO BUFF RECORD                     
*                                                                               
*   TEST PRINT                                                                  
**       MVC   P+1(08),=C'REBU0139'                                             
**       L     RF,AREC                                                          
**       MVC   P+9(27),0(RF)       LOAD KEY                                     
**       MVI   P+36,C'/'                                                        
**       MVC   P+37(2),BFDPT                                                    
**       GOTO1 =V(PRINTER)                                                      
*   TEST PRINT END                                                              
*                                                                               
REBU0140 EQU   *                                                                
         BAS   RE,NEXTEL           GET BUY'S NEXT D/T ELEMENT                   
         BNE   REBU0160            NO MORE                                      
*                                                                               
         XC    P,P                                                              
***      MVC   P(5),=C'****!'                                                   
***      GOTO1 =V(PRINTER)                                                      
*                                                                               
         BAS   R5,CHKTIM                                                        
         LR    R2,R3                                                            
         B     REBU0080            GO BACK AND CHECK NEW TIME                   
*                                                                               
* CHKTIM SUB ROUTINE MAY RETURN HERE                                            
REBU0150 MVC   BFDPT(2),=X'FFFF'                                                
         CLC   =C'PSEUDO',DUMPID   DUMP PSEUDO BASIS?                           
         BNE   REBU0160            NO                                           
         GOTO1 =A(PSEUDO),DMCB,(RC),(R6)                                        
*                                                                               
REBU0160 L     R6,AREC                                                          
         LA    R2,MTHTBL                                                        
         MVI   ELCODE,X'03'        GET BUY EFFECTIVE DATE ELEMENT               
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         B     *+8                                                              
REBU0170 BAS   RE,NEXTEL           GET NEXT  ELEMENT                            
         BNE   REBU0270            NO MORE                                      
         MVC   WORK(3),2(R6)       SET START DATE                               
REBU0180 CLC   WORK(3),5(R2)       START DATE VS END DATE                       
         BNH   REBU0200            START <= END                                 
         BAS   RE,ADTOBUFF         START >  END                                 
REBU0190 LA    R2,8(R2)            BUMP TO NEXT DP                              
         OC    0(2,R2),0(R2)       ANY ENTRY?                                   
         BNZ   REBU0180            YES - CHECK IT                               
         B     REBU0170            NO  - SHOULDN'T HAPPEN                       
*                                     SKIP THIS ELEMENT                         
*                                                                               
REBU0200 MVC   BFYM,0(R2)          SET BUFF REC DATE                            
         ZIC   RF,BYTE                                                          
         CLI   1(R6),5                                                          
         BE    REBU0210                                                         
         ZIC   RF,RBUYDTNW-RBUYDTEL(R6)                                         
REBU0210 EQU   *                                                                
         LR    R1,RF                                                            
         CLI   COMBOREC,C'Y'       COMBO BUY?                                   
         BE    REBU0220            YES - COMBO                                  
         A     RF,BFSPOT1          NO  - ACCUM REGULAR SPOT COUNT               
         ST    RF,BFSPOT1                                                       
         B     REBU0230                                                         
REBU0220 EQU   *                                                                
         A     RF,BFSPOT2          ACCUMULATE COMBO   SPOT COUNT                
         ST    RF,BFSPOT2                                                       
REBU0230 EQU   *                                                                
         ICM   RF,15,DUB                                                        
         BZ    REBU0250                                                         
         CLI   BFPGT,X'FC'         IF PLAN ITS TOTAL COST                       
         BNE   *+8                                                              
         LA    R1,1                                                             
         LR    RF,R1                                                            
         M     RE,DUB                                                           
         LTR   RF,RF                                                            
         BM    *+12                                                             
         AH    RF,=H'50'                                                        
         B     *+8                                                              
         SH    RF,=H'50'                                                        
         D     RE,=F'100'                                                       
         CLI   COMBOREC,C'Y'       COMBO BUY?                                   
         BE    REBU0240            YES - COMBO                                  
         A     RF,BFDOLS1          NO  - ACCUM REGULAR SPOT $$                  
         ST    RF,BFDOLS1                                                       
         B     REBU0250                                                         
REBU0240 EQU   *                                                                
         A     RF,BFDOLS2          ACCUMULATE COMBO   SPOT $$                   
         ST    RF,BFDOLS2                                                       
*                                                                               
REBU0250 CLI   1(R6),5             NO END DATE - NEXT ELEM                      
         BE    REBU0170                                                         
         LA    R5,7                1 WEEK                                       
         TM    8(R6),X'40'                                                      
         BNO   *+8                                                              
         LA    R5,14               2 WEEKS                                      
         ZIC   RE,WORK+2                                                        
         AR    RE,R5                                                            
         CH    RE,=H'28'                                                        
         BH    *+12                                                             
         STC   RE,WORK+2           LT 28 ADD 7/14 TO DAYS                       
         B     REBU0260                                                         
*                                                                               
         GOTO1 =V(DATCON),DMCB,(3,WORK),(0,WORK+6)                              
         GOTO1 =V(ADDAY),(R1),WORK+6,WORK+12,(R5)                               
         GOTO1 =V(DATCON),(R1),(0,WORK+12),(3,WORK)                             
REBU0260 CLC   WORK(3),5(R6)                                                    
         BH    REBU0170                                                         
         B     REBU0180                                                         
*                                                                               
REBU0270 BAS   RE,ADTOBUFF                                                      
*                                                                               
* LOOK FOR MAKEGOOD/CREDIT ELEMENTS                                             
*                                                                               
REBU0280 MVI   ELCODE,X'06'        GET WEEKS ELEM                               
REBU0290 L     R6,AREC                                                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
REBU0300 BAS   RE,NEXTEL                                                        
         BNE   REBU0380                                                         
*                                                                               
         ZIC   RF,RBUYMSSP-RBUYMSEL(R6)                                         
         LCR   RF,RF                                                            
         CLI   COMBOREC,C'Y'       COMBO BUY?                                   
         BE    REBU0310            YES - COMBO                                  
         ST    RF,BFSPOT1          NO  - ACCUMULATE REGULAR SPOTS               
         B     REBU0320                                                         
REBU0310 EQU   *                                                                
         ST    RF,BFSPOT2          ACCUMULATE COMBO SPOTS                       
REBU0320 EQU   *                                                                
         CLI   ELCODE,X'07'        MISSED $$ ALREADY CALCULATED IN              
         BE    REBU0350                                                         
         CLI   BFPGT,X'FC'         PLAN?                                        
         BE    REBU0350            YES - USE TOTAL COST                         
         M     RE,DUB                                                           
         LTR   RF,RF                                                            
         BM    *+12                                                             
         AH    RF,=H'50'                                                        
         B     *+8                                                              
         SH    RF,=H'50'                                                        
         D     RE,=F'100'                                                       
         CLI   COMBOREC,C'Y'       COMBO BUY?                                   
         BE    REBU0330            YES - COMBO                                  
         ST    RF,BFDOLS1          NO  - ACCUMULATE REGULAR $$                  
         B     REBU0340                                                         
REBU0330 EQU   *                                                                
         ST    RF,BFDOLS2          ACCUMULATE COMBO   $$                        
REBU0340 EQU   *                                                                
*                                                                               
REBU0350 LA    R2,MTHTBL                                                        
REBU0360 CLC   2(3,R6),5(R2)                                                    
         BNH   REBU0370            END BEFORE START                             
         LA    R2,8(R2)                                                         
         OC    0(2,R2),0(R2)                                                    
         BNZ   REBU0360                                                         
         DC    H'0'                                                             
REBU0370 MVC   BFYM,0(R2)                                                       
         BAS   RE,ADTOBUFF                                                      
         B     REBU0300                                                         
*                                                                               
REBU0380 CLI   ELCODE,X'07'                                                     
         BE    REBU0010                                                         
         MVI   ELCODE,X'07'                                                     
         B     REBU0290                                                         
*                                                                               
REBU0390 B     SETC0050                                                         
         EJECT                                                                  
*                                                                               
* CHKTIM:  INSPECT TIME STRINGS                                                 
*    ROUTINE RETURNS TO RB500 IF NONE OR VARIOUS TIMES                          
*                                                                               
CHKTIM   MVC   WORK(4),4(R6)       START/END TIME                               
         CLC   =C'NONE',WORK       NO START/END TIMES?                          
         BE    REBU0150            YES - RETURN                                 
         CLC   =C'VARY',WORK       'VARIOUS'?                                   
         BE    REBU0150            YES - RETURN                                 
         CLC   =C'CC',WORK+2       CONCLUSION?                                  
         BNE   CHKT0000            NO                                           
         MVC   WORK+2(2),=H'2600'  YES - SET TO '2AM'                           
CHKT0000 EQU   *                                                                
*                                                                               
*   START TIME TESTING                                                          
*                                                                               
         CLC   WORK(2),=H'600'     6AM?                                         
         BNL   CHKT0020            NOT EARLIER                                  
         CLC   WORK(2),=H'500'     EARLIER - 5AM?                               
         BL    CHKT0010            EARLIER THAN 5AM                             
         MVC   WORK(2),=H'600'     5A - 6A: SET TO 6AM                          
*                                                                               
*   END   TIME TESTING                                                          
*                                                                               
         CLC   WORK+2(2),=H'559'                                                
         BH    CHKT0040            6AM OR LATER                                 
         CLC   WORK+2(2),=H'501'   5AM OR EARLIER?                              
         BL    CHKT0020            YES                                          
         MVC   WORK+2(2),=H'600'   5A - 6A: SET TO 6AM                          
         B     CHKT0040                                                         
CHKT0010 EQU   *                                                                
         CLC   WORK(2),=H'201'     START 2AM CHECK                              
         BL    CHKT0020            2AM OR EARLIER                               
         MVC   WORK(2),=H'200'     2A - 459: SET TO 2AM                         
CHKT0020 EQU   *                                                                
         CLC   WORK+2(2),=H'600'   END  2AM CHECK                               
         BNL   CHKT0040            6AM OR LATER                                 
         CLC   WORK+2(2),=H'201'                                                
         BL    CHKT0040            2AM OR EARLIER                               
         CLC   WORK+2(2),=H'459'                                                
         BH    CHKT0030            5AM OR LATER                                 
         MVC   WORK+2(2),=H'200'   201A - 459: SET TO 2AM                       
         B     CHKT0040                                                         
CHKT0030 MVC   WORK+2(2),=H'600'   5A - 6A END: SET TO 6AM                      
CHKT0040 CLC   WORK(2),WORK+2      START TIME = END TIME?                       
         BNE   *+10                NO                                           
         XC    WORK+2(2),WORK+2    YES - CLEAR END TIME                         
         LA    R1,WORK                                                          
         BAS   RE,ADJTIME                                                       
         CLC   =H'2600',WORK+2     LEAVE 2A ALONE                               
         BE    CHKT0050                                                         
         SR    RE,RE                                                            
         ICM   RE,3,WORK+2                                                      
         BZ    CHKT0060                                                         
         SH    RE,=H'1'            ADJUST END TIME                              
         STCM  RE,3,WORK+2                                                      
CHKT0050 CLC   WORK(2),WORK+2      START GT END NO MATCH                        
         BH    REBU0150                                                         
CHKT0060 BR    R5                                                               
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
* PUT UP TO 4 BUFFALO RECORDS (NOW ONLY TWO BUFFALOS)                           
ADTOBUFF NTR1                                                                   
         OC    BFYM,BFYM                                                        
         BZ    ADTO0030                                                         
         MVI   BFTYP,1                                                          
         XC    BFTYPC,BFTYPC                                                    
         BAS   RE,PUTBUFF                                                       
         LA    R4,BFREC                                                         
         CLC   =C'BUFF',DUMPID     DUMP BUFF RECORD?                            
         BNE   ADTO0002            NO                                           
         LA    R3,BFLENGTH                                                      
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R3),=C'2D'                
*                                                                               
ADTO0002 EQU   *                                                                
         OC    CTYP,CTYP           ANY CONTRACT TYPE?                           
         BZ    ADTO0010            NO                                           
         MVI   BFTYP,2             YES - SET TYPE TO '2'                        
         XC    BFTYPC,BFTYPC       SPACE OUT TYPE FIELD                         
         MVC   BFTYPC(1),CTYP      INSERT CONTRACT TYPE                         
         BAS   RE,PUTBUFF                                                       
*                                                                               
ADTO0010 OC    AGY,AGY             ANY AGENCY CODE?                             
         BZ    ADTO0020            NO                                           
*** NEW FOR AUR                                                                 
*        MVI   BFTYP,3             YES - SET TYPE TO '3'                        
*        XC    BFTYPC,BFTYPC       SPACE OUT TYPE FIELD                         
*        MVC   BFTYPC(6),AGY       INSERT AGENCY/AGENCY OFFICE                  
*        BAS   RE,PUTBUFF                                                       
*                                                                               
ADTO0020 OC    OFF(2),OFF          ANY OFFICE CODE?                             
         BZ    ADTO0030            NO                                           
*** NEW FOR AUR                                                                 
*        MVI   BFTYP,4             YES - SET TYPE TO '4'                        
*        XC    BFTYPC,BFTYPC       SPACE OUT TYPE FIELD                         
*        MVC   BFTYPC(2),OFF       INSERT SALESMAN OFFICE                       
*        BAS   RE,PUTBUFF                                                       
*                                                                               
ADTO0030 XC    BFYM,BFYM                                                        
         XC    BFSPOT1(16),BFSPOT1                                              
*                                  REINITIALIZE COUNTERS                        
ADTO0040 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*  ROUTINE PRINTS TOTAL PER STATION                                             
PRNTSTA  NTR1                                                                   
         CP    LINE,=P'48'                                                      
         BL    PRNS0010                                                         
         ZAP   LINE,=P'100'        FORCE PAGE BREAK                             
         MVC   TITLE+10(22),=C'A.U.R. CREATION REPORT'                          
PRNS0010 MVC   P(7),SVKEY+SVGSTAE                                               
         CLI   P+6,C' '                                                         
         BNE   *+8                                                              
         MVI   P+6,C'T'                                                         
         MVC   P+12(29),=CL29'** STATION RECORDS CREATED **'                    
         EDIT  (P3,COUNT),(5,P+45)                                              
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
         GOTO1 =V(PRINTER)                                                      
         L     RE,TTLSP1           ACCUMULATE TOTAL REG SPOTS                   
         A     RE,TSTASP1                                                       
         ST    RE,TTLSP1                                                        
         L     RE,TTLSP2           ACCUMULATE TOTAL COMBO SPOTS                 
         A     RE,TSTASP2                                                       
         ST    RE,TTLSP2                                                        
         L     RE,TTLCS1           ACCUMULATE TOTAL REG COST                    
         A     RE,TSTACS1                                                       
         ST    RE,TTLCS1                                                        
         L     RE,TTLCS2           ACCUMULATE TOTAL COMBO COST                  
         A     RE,TSTACS2                                                       
         ST    RE,TTLCS2                                                        
*                                                                               
PRNS0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*  ROUTINE PRINTS TOTAL PER STATION                                             
FINALPRT NTR1                                                                   
         ZAP   LINE,=P'100'        FORCE PAGE BREAK                             
         MVC   TITLE+10(22),=C'A.U.R. CREATION REPORT'                          
         MVC   MID1+50(11),=C'FINAL TOTAL'                                      
         MVC   MID2+50(12),=15CL1'-'                                            
         MVC   P(30),=CL30'** NUMBER OF RECORDS ADDED **'                       
         EDIT  (P5,TCOUNT),(8,P+32)                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=CL17'** TOTAL SPOTS **'                                   
         EDIT  (4,TTLSP1),(6,P+20)                                              
         MVC   P+28(19),=CL19'** TOTAL DOLLARS **'                              
         EDIT  (4,TTLCS1),(12,P+50)                                             
         MVC   P+65(17),=CL17'** COMBO SPOTS **'                                
         EDIT  (4,TTLSP2),(6,P+85)                                              
         MVC   P+94(19),=CL19'** COMBO DOLLARS **'                              
         EDIT  (4,TTLCS2),(12,P+116)                                            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
FNEXT    EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
* ROUTINE TO MOVE SDD RECORDS TO GIVEN AREA                                     
* - R2 = ADDRESS OF AREA TO PLACE RECORD                                        
SVSDD    NTR1                                                                   
         LR    RE,R2               A(STORAGE SPACE FOR SDD RECORD)              
         LA    RF,LSDDTBL          L(TABLE ENTRY)                               
         SR    R1,R1               ZERO PAD CHAR + SEND LENGTH                  
         MVCL  RE,R0               ZERO OUT TABLE ENTRY                         
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SVSD0010 BAS   RE,NEXTEL                                                        
         BNE   FNEXT                                                            
         MVC   0(3,R2),3(R6)       LOAD DAYPT + # OF TIMES                      
         LA    R2,3(R2)            BUMP TABLE TO STORE TIMES                    
         LR    R3,R6                                                            
         ZIC   R5,5(R6)            LOOP CONTROL: # DAY/TIMES IN ELT             
SVSD0020 MVC   0(5,R2),6(R3)       LOAD DAY + MILITARY TIME                     
         LA    R1,1(R2)            TIME IN TABLE ENTRY                          
         BAS   RE,ADJTIME          ADJUST IT IF NEEDED                          
         CLC   =H'2600',3(R2)      LEAVE 2AM ALONE                              
         BE    SVSD0030                                                         
         SR    RE,RE                                                            
         ICM   RE,3,3(R2)                                                       
         BNZ   *+14                                                             
         MVC   3(2,R2),1(R2)       END 0 SAME AS START                          
         B     SVSD0030                                                         
*                                                                               
         SH    RE,=H'1'            ADJUST END TIME                              
         STCM  RE,3,3(R2)                                                       
SVSD0030 LA    R3,5(R3)            BUMP TO NEXT DAY/TIME IN ELT                 
         LA    R2,5(R2)            BUMP TO NEXT TABLE ENTRY                     
         BCT   R5,SVSD0020         GO BACK FOR NEXT D/T IN ELT                  
         B     SVSD0010            GO BACK FOR NEXT ELEMENT                     
         SPACE                                                                  
* ADJUST TIME IF LESS THAN 600 (001-2A)                                         
ADJTIME  NTR1                                                                   
         LR    R0,2                                                             
ADJT0010 CLC   =H'600',0(R1)       ADJUST AFTER 1200                            
         BNH   ADJT0020                                                         
         SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         AH    RE,=H'2400'                                                      
         STCM  RE,3,0(R1)                                                       
ADJT0020 LA    R1,2(R1)                                                         
         OC    0(2,R1),0(R1)       IF END TIME 0 DON'T CONVERT                  
         BE    FNEXT                                                            
         BCT   R0,ADJT0010                                                      
         B     SETC0050                                                         
         EJECT                                                                  
*  ROUTINE PRINTS TOTAL PER STATION                                             
SETNXSTA NTR1                                                                   
         BAS   RE,RSETBUFF                                                      
*                                                                               
         ZAP   COUNT,=P'0'                                                      
         XC    TSTASP1(16),TSTASP1                                              
*                                  ZERO OUT GRAND TOTALS                        
         XC    MTHTBL,MTHTBL                                                    
*                                                                               
NXEXT    B     FNEXT                                                            
         SPACE                                                                  
* GET A MONDAY DATE      -- USES DUB/FULL(6)/HALF --                            
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
         XIT1                                                                   
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
               (R2),AREC,DMWORK                                                 
         SPACE 2                                                                
*                  DATA MANAGER ERRORS AND SETC0050                             
         SPACE 3                                                                
DMCHECK  MVI   DMINBTS,X'08'                                                    
         CLI   8(R1),X'02'                                                      
         BE    SETC0050                                                         
         CLI   8(R1),0                                                          
         BZ    SETC0050                                                         
         DC    H'0'                                                             
         EJECT                                                                  
         SPACE 3                                                                
GETEL    GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BUFNO=2,BLKSIZE=32760                                 
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
AREC     DC    A(REC)                                                           
         DC    C'*KEY'                                                          
KEY      DS    XL34                                                             
KEYSAVE  DS    XL34                                                             
SVKEY    DS    XL34                                                             
HLDKEY   DS    XL34                                                             
DMWORK   DS    XL96                                                             
COMMAND  DS    CL8                                                              
DMINBTS  DC    X'00'                                                            
*                                                                               
         DS    0F                                                               
WORK     DS    CL80                                                             
UTL      DC    F'0',X'0A'                                                       
*SB      DC    F'2'                INHIBIT RECOVERY                             
SSB      DS    0CL256              NEW SSB                                      
         DC    XL2'00'                                                          
         DC    X'FF'                                                            
         DS    XL253                                                            
*                                                                               
SAVNAME  DS    CL10                                                             
SAVLOCAL DS    CL1                                                              
*                                                                               
REPFILE  DC    CL8'REPFILE '                                                    
FLIST    DS    0H                                                               
         DC    CL8' REPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'X'                                                           
BUFEND   DS    C                                                                
         DS    0F                                                               
COLVALS  DS    24XL16                                                           
         DC    C'*REC'                                                          
         DS    0F                                                               
RECLN    DS    F                                                                
REC      DS    1024C               IOAREA                                       
REP      DS    CL2                 REP CODE                                     
MED      DS    CL1                 MEDIA                                        
STAGFIL  DS    CL7                 STATION GROUP FILTER                         
STAGEFIL DS    CL7                 END STATION GROUP FILTER                     
REJSTA   DS    CL7                 REJECTED STATION GROUP                       
WRITEFLG DS    CL1                 DON'T WRITE                                  
WUPDTE   DS    CL1                 JUST UPDATE RECORDS                          
RECRTE   DS    CL1                 RE-CREATE A.U.R.'S                           
DELFLG   DS    CL1                 DELETE ELEMENTTS ONLY                        
PRGALL   DS    CL1                 DELETE ELEMENTTS ONLY                        
FRST     DS    CL1                                                              
COMBOREC DS    CL1                 COMBO/REGULAR FLAG: COMBO = Y                
BFDP2    DS    CL2                 SAVE AREA FOR DAYPART CODE                   
EARLYDAT DC    XL3'5C0101'         EARLIEST DATE FOR DATA                       
LASTDATE DC    XL3'0'              LAST     DATE FOR DATA                       
ADDPUT   DS    CL1                                                              
FILTAGY  DS    CL4                 FILTER FOR AGENCY                            
FILTADV  DS    CL4                 FILTER FOR ADVERTISER                        
DUMPID   DS    CL8                                                              
CTYP     DS    CL1                 CONTRACT TYPE                                
AGY      DS    CL6                 AGENCY/AGENCY OFFICE                         
OFF      DS    CL2                 SALESMAN OFFICE                              
SVSTAG   DS    CL7                 LAST CONTRACT GROUP/STATION                  
ELCODE   DS    XL1                                                              
DISPCTR  DC    XL1'64'             DISPLAY COUNTER - INITIAL = 100              
COUNT    DC    PL3'0'              COUNT OF A.U.R. RECS/STATION                 
TCOUNT   DC    PL5'0'              COUNT OF A.U.R. RECS/TOTAL                   
DCOUNT   DC    PL5'0'              COUNT OF A.U.R. RECS DELETED ELEMS           
DELCNT   DC    PL5'0'              COUNT OF A.U.R. RECS DELETED                 
ACCSPOT1 DS    F                   ACCUMULATORS FOR NEW RECORDS                 
ACCDOLS1 DS    F                   "                                            
ACCSPOT2 DS    F                   "                                            
ACCDOLS2 DS    F                   "                                            
CONCTR   DS    F                                                                
CONCTR2  DS    F                                                                
DATADISP DS    H                                                                
TODAY2   DS    H                   TODAY"S COMPRESSED DATE                      
AGO3YRS  DS    CL3                 THREE YEARS AGO TODAY                        
SAVCREAT DS    CL3                 CREATION DATE                                
ACURSDDT DS    A                   ADDRESS OF STATION SDD REC                   
TSTASP1  DS    F                   TOTAL SPOTS/STATION - REGULAR                
TSTACS1  DS    F                   TOTAL DOLLARS/STATION - REGULAR              
TSTASP2  DS    F                   TOTAL SPOTS/STATION - COMBO                  
TSTACS2  DS    F                   TOTAL DOLLARS/STATION - COMBO                
TTLSP1   DS    F                   TOTAL SPOTS - REGULAR                        
TTLCS1   DS    F                   TOTAL DOLLARS - REGULAR                      
TTLSP2   DS    F                   TOTAL SPOTS - COMBO                          
TTLCS2   DS    F                   TOTAL DOLLARS - COMBO                        
MTHTBL   DS    CL162               20 BROADCASTS MONTH DATES                    
*                                     + 2 CHARS EXTRA                           
* MAX NUMBER 0F DAY/TIMES (48) * MAX LENGTH SIZE OF ENTRY (8),                  
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
*                                                                               
BFREC    DS    0CL32               BUFFALO RECORD LAYOUT                        
BFKEY    DS    0CL16                                                            
BFTYP    DS    CL1                 TYPE (1-4)                                   
BFTYPC   DS    CL6                 CONTYPE/AGY-AGYOFF/SALES OFFICE              
BFDPT    DS    CL1                 DAYPART                                      
BFSDPT   DS    CL1                 SUB DAYPART                                  
BFPGT    DS    CL1                 PROGRAM TYPE                                 
BFSPL    DS    CL2                 SPOT LENGTH                                  
BFYM     DS    CL2                 YEAR MONTH                                   
BFBYCRDT DS    CL2                 **WARNING:  THIS IS FOR ALIGNMENT            
*                                    TO ENSURE RECORD LENGTH AGREEMENT          
*                                    AND HAS BEEN ADDED INTO THE                
*                                    LENGTH OF 'BFREC'                          
BFSPOT1  DS    F                   TOTAL SPOTS:   REGULAR                       
BFDOLS1  DS    F                   TOTAL DOLLARS: REGULAR                       
BFSPOT2  DS    F                   TOTAL SPOTS:   COMBO                         
BFDOLS2  DS    F                   TOTAL DOLLARS: COMBO                         
*                                                                               
BFLENGTH EQU   *-BFREC                                                          
*                                                                               
SBFKEY   DS    CL16                SAVED BFKEY                                  
REC2     DS    6000C               2ND IOAREA                                   
*                                                                               
         BUFF  LINES=1,ROWS=1,COLUMNS=4,FLAVOR=BINARY,                 X        
               KEYLIST=(16,A)                                                   
*                                                                               
PRGEND   EQU   *                                                                
         SPACE 2                                                                
SVIDE    EQU   0                                                                
SVREPE   EQU   2                                                                
SVGSTAE  EQU   4                                                                
SVSTAE   EQU   6                                                                
SVSTAME  EQU   10                                                               
SVCON#   EQU   23                                                               
         SPACE 2                                                                
* REGENBUY                                                                      
* REGENATNA                                                                     
* REGENSDD                                                                      
* DDBUFFALOD                                                                    
* DDDPRINT                                                                      
* REGENCON                                                                      
*        PRINT OFF                                                              
       ++INCLUDE REGENBUY                                                       
       ++INCLUDE REGENAUR                                                       
       ++INCLUDE REGENSDD                                                       
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE REGENCON                                                       
         ORG     RCONREC+6000                                                   
*        PRINT ON                                                               
         EJECT                                                                  
*                                                                               
*  ADDAUREC: ADDS AVERAGE UNIT RATE RECORDS - BUILDS STATION TOTALS             
*                                                                               
ADDAUREC CSECT                                                                  
         NMOD1 0,*ADAR*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R6,REC                                                           
         ST    R6,AREC                                                          
         USING RAURRECD,R6                                                      
         LR    RE,R6               ZERO OUT IO BUFFER                           
         LA    RF,1000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVI   RAURKTYP,X'2C'      BUILD SKELETON RECORD                        
         MVC   RAURKREP,REP        REP                                          
         MVC   RAURKGRP(7),SVKEY+SVGSTAE                                        
*                                  INSERT GRP-SUBGRP/STA/MEDIA                  
         MVC   RAURCODE(2),=X'010C'                                             
*                                  SET DESCRIPTOR ELEMENT                       
         MVC   RAURCREA,TODAY2     SET CREATE DATE                              
         MVC   RAURLCHG,TODAY2     SET LAST CHANGE DATE                         
*                                                                               
         XC    SBFKEY,SBFKEY                                                    
         BAS   RE,HIGHBUFF         RETRIEVE HIGH BUFFALO RECORD                 
         BO    ADDA0190            FINISHED                                     
*                                                                               
ADDA0010 DS    0H                                                               
***      CLI   RECRTE,C'Y'         RECREATE DELETE ALL ELEMENTS                 
***      BE    ADDA0040                                                         
***      CLI   WUPDTE,C'Y'         WEEKLY RE-CREATE                             
***      BE    ADDA0040                                                         
*                                                                               
*   NEW RECORD - BUILD & ADD                                                    
*                                                                               
         MVC   RAURKTPE(7),BFTYP   SET RECORD TYPE                              
         MVC   RAURKDPT(7),BFDPT   SET RECORD DAYPART                           
****     MVC   RAURLEN,=H'58'      SET RECORD LENGTH (ORIG)                     
         MVC   RAURLEN,=H'46'      SET RECORD LENGTH                            
         XC    REC+46(255),REC+46                                               
*                                                                               
ADDA0015 EQU   *                   NEXT '02' ELEM                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING RAURSCDT,R4                                                      
*                                                                               
         MVC   0(2,R4),=X'020B'    SET ELT CODE/LENGTH                          
         MVC   RAURSCAS,BFBYCRDT   SET CREATE DATE                              
*        MVC   RAURSCAS,TODAY2     SET AS-AT DATE                               
         MVI   RAURSCTL,0          RESET CONTROL INDICATOR                      
         OC    BFSPOT1(8),BFSPOT1  ANY REGULAR VALUE?                           
         BZ    ADDA0020            NO                                           
*                                  YES - LOAD INTO FIRST BUCKETS                
         OI    RAURSCTL,X'80'      SET CONTROL INDICATOR                        
         MVC   RAURSCS1,BFSPOT1+2  LOAD REGULAR SPOTS                           
         MVC   RAURSCC1,BFDOLS1    LOAD REGULAR DOLLARS                         
         OC    BFSPOT2(8),BFSPOT2  ANY COMBO   VALUE?                           
         BZ    ADDA0030            NO                                           
*                                  YES - LOAD INTO SECOND BUCKETS               
         OI    RAURSCTL,X'40'      YES - SET CONTROL INDICATOR                  
         MVC   RAURSCS2,BFSPOT2+2  LOAD COMBO   SPOTS                           
         MVC   RAURSCC2,BFDOLS2    LOAD COMBO   DOLLARS                         
         MVI   1(R4),X'11'         INCREASE LENGTH TO 17                        
*                                     FOR BOTH REG+COMBO COUNTERS               
         B     ADDA0030                                                         
ADDA0020 EQU   *                                                                
         OC    BFSPOT2(8),BFSPOT2  ANY COMBO   VALUE?                           
         BZ    ADDA0030            NO                                           
*                                  YES - LOAD INTO FIRST BUCKETS                
         OI    RAURSCTL,X'40'      YES - SET CONTROL INDICATOR                  
         MVC   RAURSCS1,BFSPOT2+2  LOAD COMBO   SPOTS                           
         MVC   RAURSCC1,BFDOLS2    LOAD COMBO   DOLLARS                         
ADDA0030 EQU   *                                                                
*** NEW                                                                         
         GOTO1 =V(HELLO),DMCB,(C'P',REPFILE),REC,WORK                           
*DCH0************************                                                   
****     DC    H'0'                                                             
*DCH0************************                                                   
*                                                                               
         CLI   BFTYP,1             ONLY ADD 1 BUYLINES SPOTS/DOLLARS            
         BNE   ADDA0031                                                         
         L     RE,TSTASP1          ACCUMULATE REGULAR SPOTS                     
         A     RE,BFSPOT1                                                       
         ST    RE,TSTASP1                                                       
         L     RE,TSTASP2          ACCUMULATE COMBO   SPOTS                     
         A     RE,BFSPOT2                                                       
         ST    RE,TSTASP2                                                       
         L     RE,TSTACS1          ACCUMULATE REGULAR DOLLARS                   
         A     RE,BFDOLS1                                                       
         ST    RE,TSTACS1                                                       
         L     RE,TSTACS2          ACCUMULATE COMBO   DOLLARS                   
         A     RE,BFDOLS2                                                       
         ST    RE,TSTACS2                                                       
ADDA0031 AP    COUNT,=P'1'                                                      
         AP    TCOUNT,=P'1'                                                     
*                                                                               
*** TEST                                                                        
*        XC    P,P                                                              
*        MVC   P(32),BFREC                                                      
*        GOTO1 =V(PRINTER)                                                      
*** END TEST                                                                    
         MVC   SBFKEY,BFKEY        SAVE OLD BFKEY                               
         BAS   RE,SEQBUFF          RETRIEVE NEXT BUFFALO RECORD                 
         BO    ADDA0187            FINISHED                                     
         CLC   BFKEY(14),SBFKEY    COMPARE UP TO YEAR/MONTH                     
         BE    ADDA0015                                                         
*                                     LENGTH RECORD                             
         ZICM  R3,RAURLEN,2        INSERT LENGTH OF RECORD                      
         LR    RE,R3               SET RECORD LENGTH FOR VARIABLE               
*                                     LENGTH RECORD                             
         LA    RE,4(RE)            ADD VARIABLE CONTROL LENGTH                  
         SLL   RE,16                                                            
         ST    RE,RECLN                                                         
         OC    EARLYDAT,EARLYDAT   ANY EARLY DATE ENTERED?                      
         BZ    ADDA0032            NO                                           
         CLC   RAURKYM,EARLYDAT    YES - DATA < EARLIEST DATE?                  
         BL    ADDA0180            YES - DON'T OUTPUT IT                        
*                                     AND DON'T COUNT IT, EITHER..              
ADDA0032 EQU   *                                                                
         OC    LASTDATE,LASTDATE   ANY LAST  DATE ENTERED?                      
         BZ    ADDA0034            NO                                           
         CLC   RAURKYM,LASTDATE    YES - DATA > LAST DATE?                      
         BH    ADDA0180            YES - DON'T OUTPUT IT                        
*                                     AND DON'T COUNT IT, EITHER..              
ADDA0034 EQU   *                                                                
         MVI   ADDPUT,C'A'         SET 'WHERE ADDED FROM'                       
         BAS   RE,DUMPOUTP         CHECK FOR OUTPUT REC DISPLAY                 
         CLI   WRITEFLG,C'Y'                                                    
         BNE   ADDA0180            DON'T WRITE TO TAPE, GET NEXT RECORD         
         LA    R0,RECLN                                                         
         PUT   FILEOUT,(R0)                                                     
         B     ADDA0180            DONE WITH RECORD, GET NEXT REC               
*                                                                               
         DROP  R4                                                               
*                                                                               
**SEE IF RECORD EXISTS                                                          
*ADDA0040 MVI   DMINBTS,X'80'       READ FOR UPDATE                             
**       XC    KEY,KEY                                                          
**       MVI   KEY,X'2C'                                                        
**       MVC   KEY+RAURKREP-RAURKEY(2),REP                                      
**       MVC   KEY+RAURKGRP-RAURKEY(7),SVSTAG                                   
**       MVC   KEY+RAURKTPE-RAURKEY(7),BFTYP                                    
**       MVC   KEY+RAURKDPT-RAURKEY(7),BFDPT                                    
**       GOTO1 HIGH                                                             
**       CLC   KEY(27),KEYSAVE     AUR RECORD FOUND?                            
**       BNE   ADDA0120            NO                                           
**                                                                              
**       GOTO1 GETREC              YES - RETRIEVE IT                            
**                                                                              
ADDA0050 EQU   *                                                                
**       XC    ACCSPOT1(16),ACCSPOT1                                            
**                                 CLEAR ACCUMULATORS                           
**       MVI   ELCODE,X'02'                                                     
**       BAS   RE,GETEL                                                         
**       B     *+8                                                              
*ADDA0060 BAS   RE,NEXTEL                                                       
**       BNE   ADDA0080                                                         
**       SR    RE,RE                                                            
**       TM    RAURSCTL-RAURSCDT(R6),X'80'                                      
**                                 ANY REGULAR VALUES?                          
**       BNO   ADDA0070            NO                                           
**                                 YES - ACCUMULATE REGULAR FIGURES             
**                                    FROM 1ST BCKTS INTO 1ST BCKTS             
**       ICM   RE,3,RAURSCS1-RAURSCDT(R6)                                       
**       A     RE,ACCSPOT1                                                      
**       ST    RE,ACCSPOT1                                                      
**       ICM   RE,15,RAURSCC1-RAURSCDT(R6)                                      
**       A     RE,ACCDOLS1                                                      
**       ST    RE,ACCDOLS1                                                      
**       TM    RAURSCTL-RAURSCDT(R6),X'40'                                      
**                                 ANY COMBO   VALUES?                          
**       BNO   ADDA0060            NO                                           
**                                 YES - ACCUMULATE COMBO   FIGURES             
**                                    FROM 2ND BCKTS INTO 2ND BCKTS             
**       ICM   RE,3,RAURSCS2-RAURSCDT(R6)                                       
**       A     RE,ACCSPOT2                                                      
**       ST    RE,ACCSPOT2                                                      
**       ICM   RE,15,RAURSCC2-RAURSCDT(R6)                                      
**       A     RE,ACCDOLS2                                                      
**       ST    RE,ACCDOLS2                                                      
**       B     ADDA0060                                                         
ADDA0070 EQU   *                                                                
**       TM    RAURSCTL-RAURSCDT(R6),X'40'                                      
**                                 ANY COMBO   VALUES?                          
**       BNO   ADDA0060            NO                                           
**                                 YES - ACCUMULATE COMBO   FIGURES             
**                                    FROM 1ST BCKTS INTO 2ND BCKTS             
**       ICM   RE,3,RAURSCS1-RAURSCDT(R6)                                       
**       A     RE,ACCSPOT2                                                      
**       ST    RE,ACCSPOT2                                                      
**       ICM   RE,15,RAURSCC1-RAURSCDT(R6)                                      
**       A     RE,ACCDOLS2                                                      
**       ST    RE,ACCDOLS2                                                      
**       B     ADDA0060                                                         
**                                                                              
*ADDA0080 CLC   BFSPOT1(16),ACCSPOT1                                            
**       BE    ADDA0180            NO CHANGE TO RECORD                          
**       XC    0(18,R6),0(R6)      CLEAR 1 BYTE BEYOND ELEMENTS                 
**       MVC   0(2,R6),=X'020B'    BUILD ELEMENT (SHORT VERSION)                
**       MVC   2(2,R6),BFBYCRDT    SET CREATE DATE                              
**??     MVC   2(2,R6),TODAY2                                                   
**       L     RE,BFSPOT1          REGULAR SPOTS:  BUFF REC                     
**       S     RE,ACCSPOT1         REGULAR SPOTS:  ACCUMULATED                  
**       STCM  RE,3,5(R6)          INSERT REGULAR SPOTS                         
**       STCM  RE,15,BFSPOT1       FOR REGULAR SPOT TOTALS                      
**       L     RE,BFDOLS1                                                       
**       S     RE,ACCDOLS1                                                      
**       STCM  RE,15,7(R6)         INSERT REGULAR DOLLARS                       
**       STCM  RE,15,BFDOLS1       FOR REGULAR DOLLAR TOTALS                    
**       L     RE,BFSPOT2          COMBO   SPOTS:  BUFF REC                     
**       S     RE,ACCSPOT2         COMBO   SPOTS:  ACCUMULATED                  
**       STCM  RE,3,11(R6)         INSERT COMBO   SPOTS                         
**       STCM  RE,15,BFSPOT2       FOR COMBO   SPOT TOTALS                      
**       L     RE,BFDOLS2                                                       
**       S     RE,ACCDOLS2                                                      
**       STCM  RE,15,13(R6)        INSERT COMBO   DOLLARS                       
**       STCM  RE,15,BFDOLS2       FOR COMBO   DOLLAR TOTALS                    
**       OC    5(6,R6),5(R6)       ANY REGULAR SPOTS/DOLLARS?                   
**       BZ    ADDA0090            NO                                           
**       OI    4(R6),X'80'         YES - SET INDICATOR                          
ADDA0090 EQU   *                                                                
**       OC    11(6,R6),11(R6)     ANY COMBO   SPOTS/DOLLARS?                   
**       BZ    ADDA0110            NO                                           
**       OI    4(R6),X'40'         YES - SET INDICATOR                          
**       TM    4(R6),X'80'         ANY REGULAR SPOTS/DOLLARS?                   
**       BO    ADDA0100            YES                                          
**       MVC   5(6,R6),11(R6)      NO  - MOVE COMBO TO 1ST BUCKETS              
**       XC    11(6,R6),11(R6)     CLEAR FOLLOWING BYTES                        
**       B     ADDA0110                                                         
ADDA0100 EQU   *                                                                
**       MVI   1(R6),X'11'         SET ELEMENT LENGTH TO 17                     
ADDA0110 EQU   *                                                                
**       GOTO1 CHKMAX,DMCB,(R6)    CHECK MAXIMUM RECORD LENGTH                  
**       BNZ   ADDA0010            DOESN'T FIT - ERROR MESSAGE                  
**       SR    R3,R3               GET RECORD LENGTH                            
**       ICM   R3,3,RAURLEN                                                     
**       ZIC   RE,1(R6)            GET ELEMENT LENGTH                           
**       AR    R3,RE               ADD TO EXISTING RECORD LENGTH                
**       STCM  R3,3,RAURLEN        INSERT NEW ELEMENT LENGTH                    
**       MVI   ADDPUT,C'A'         SET 'WHERE ADDED FROM'                       
**       BAS   RE,DUMPOUTP         CHECK FOR OUTPUT REC DISPLAY                 
**       CLI   WRITEFLG,C'Y'                                                    
**       BNE   ADDA0150                                                         
**       BAS   RE,PUTREC                                                        
**       B     ADDA0160                                                         
**                                                                              
**ADD A NEW RECORD                                                              
*ADDA0120 MVC   RAURKTPE(7),BFTYP   INSERT RECORD TYPE                          
**       MVC   RAURKDPT(7),BFDPT   INSERT DAYPART                               
**       MVC   RAURLEN,=H'58'      INSERT INITIAL LENGTH                        
**       LA    R4,RAURCODE         A(DESCRIPTIVE ELEMENT)                       
**       ZIC   RE,1(R4)            GET LENGTH                                   
**       AR    R4,RE               BUMP TO NEXT ELEMENT                         
**                                                                              
**       USING RAURSCDT,R4                                                      
**                                                                              
**       XC    0(24,R4),0(R4)      CLEAR ANY RESIDUAL DATA                      
**       MVC   0(2,R4),=X'020B'    SET ELT CODE AND INIT LENGTH                 
**       MVC   RAURSCAS,BFBYCRDT   SET CREATE DATE                              
**       MVC   RAURSCAS,TODAY2     SET ACTIVITY DATE                            
**       OC    BFSPOT1(8),BFSPOT1  ANY REGULAR VALUE?                           
**       BZ    ADDA0130            NO                                           
**                                 YES - LOAD INTO FIRST BUCKETS                
**       OI    RAURSCTL,X'80'      SET CONTROL INDICATOR                        
**       MVC   RAURSCS1,BFSPOT1+2  LOAD REGULAR SPOTS                           
**       MVC   RAURSCC1,BFDOLS1    LOAD REGULAR DOLLARS                         
**       OC    BFSPOT2(8),BFSPOT2  ANY COMBO   VALUE?                           
**       BZ    ADDA0140            NO                                           
**                                 YES - LOAD INTO SECOND BUCKETS               
**       OI    RAURSCTL,X'40'      YES - SET CONTROL INDICATOR                  
**       MVC   RAURSCS2,BFSPOT2+2  LOAD COMBO   SPOTS                           
**       MVC   RAURSCC2,BFDOLS2    LOAD COMBO   DOLLARS                         
**       MVI   1(R4),X'11'         INCREASE LENGTH TO 17                        
**                                    FOR BOTH REG+COMBO COUNTERS               
**       MVC   RAURLEN,=H'64'      INCREASE OVERALL REC LENGTH                  
**                                    FOR BOTH REG+COMBO COUNTERS               
**       B     ADDA0140                                                         
ADDA0130 EQU   *                                                                
**       OC    BFSPOT2(8),BFSPOT2  ANY COMBO   VALUE?                           
**       BZ    ADDA0140            NO                                           
**                                 YES - LOAD INTO FIRST BUCKETS                
**       OI    RAURSCTL,X'40'      YES - SET CONTROL INDICATOR                  
**       MVC   RAURSCS1,BFSPOT2+2  LOAD COMBO   SPOTS                           
**       MVC   RAURSCC1,BFDOLS2    LOAD COMBO   DOLLARS                         
ADDA0140 EQU   *                                                                
**       ZICM  R3,RAURLEN,2        INSERT RECORD LENGTH                         
**       DROP  R4                                                               
**       CLI   WRITEFLG,C'Y'                                                    
**       BNE   ADDA0150                                                         
**       BAS   RE,ADDREC                                                        
**       B     ADDA0160                                                         
**                                                                              
*ADDA0150 L     R4,AREC                                                         
**                                                                              
*ADDA0160 CLI   BFTYP,1             ONLY ADD 1 BUYLINES SPOTS/DOLLARS           
**       BNE   ADDA0170                                                         
**       L     RE,TSTASP1          ACCUMULATE REGULAR SPOTS                     
**       A     RE,BFSPOT1                                                       
**       ST    RE,TSTASP1                                                       
**       L     RE,TSTASP2          ACCUMULATE COMBO   SPOTS                     
**       A     RE,BFSPOT2                                                       
**       ST    RE,TSTASP2                                                       
**       L     RE,TSTACS1          ACCUMULATE REGULAR DOLLARS                   
**       A     RE,BFDOLS1                                                       
**       ST    RE,TSTACS1                                                       
**       L     RE,TSTACS2          ACCUMULATE COMBO   DOLLARS                   
**       A     RE,BFDOLS2                                                       
**       ST    RE,TSTACS2                                                       
*ADDA0170 AP    COUNT,=P'1'                                                     
**       AP    TCOUNT,=P'1'                                                     
ADDA0180 L     R6,AREC             RESET R6 TO START OF RECORD                  
*** TEST                                                                        
**       XC    P,P                                                              
**       MVC   P(100),REC                                                       
**       GOTO1 =V(PRINTER)                                                      
*** END TEST                                                                    
         B     ADDA0010                                                         
*                                                                               
ADDA0187 EQU   *                                                                
         ZICM  R3,RAURLEN,2        INSERT LENGTH OF RECORD                      
         LR    RE,R3               SET RECORD LENGTH FOR VARIABLE               
*                                     LENGTH RECORD                             
         LA    RE,4(RE)            ADD VARIABLE CONTROL LENGTH                  
         SLL   RE,16                                                            
         ST    RE,RECLN                                                         
         OC    EARLYDAT,EARLYDAT   ANY EARLY DATE ENTERED?                      
         BZ    ADDA0188            NO                                           
         CLC   RAURKYM,EARLYDAT    YES - DATA < EARLIEST DATE?                  
         BL    ADDA0190            YES - DON'T OUTPUT IT                        
*                                     AND DON'T COUNT IT, EITHER..              
ADDA0188 EQU   *                                                                
         OC    LASTDATE,LASTDATE   ANY LAST  DATE ENTERED?                      
         BZ    ADDA0189            NO                                           
         CLC   RAURKYM,LASTDATE    YES - DATA > LAST DATE?                      
         BH    ADDA0190            YES - DON'T OUTPUT IT                        
*                                     AND DON'T COUNT IT, EITHER..              
ADDA0189 EQU   *                                                                
         MVI   ADDPUT,C'A'         SET 'WHERE ADDED FROM'                       
         BAS   RE,DUMPOUTP         CHECK FOR OUTPUT REC DISPLAY                 
         CLI   WRITEFLG,C'Y'                                                    
         BNE   ADDA0190                                                         
         LA    R0,RECLN                                                         
         PUT   FILEOUT,(R0)                                                     
ADDA0190 EQU   *                                                                
         XMOD1                                                                  
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*  CHKMAX:  CHECKS TO ENSURE NEW ELEMENT WILL FIT IN RECORD                     
*     R6  =  A(NEW ELEMENT TO BE ADDED)                                         
*                                                                               
CHKMAX   NTR1                                                                   
         L     R6,0(R1)            RESET A(NEW ELEMENT)                         
         USING RAURRECD,R6                                                      
         SR    R3,R3               GET RECORD LENGTH                            
         ICM   R3,3,RAURLEN                                                     
         ZIC   RE,1(R6)            GET ELEMENT LENGTH                           
         AR    R3,RE               ADD TO EXISTING RECORD LENGTH                
         CH    R3,=H'1000'         CHECK AGAINST MAX SIZE                       
         BNH   CMAX0010            IT FITS                                      
         MVC   P(25),=C'*ERROR - RECORD TOO LARGE'                              
         MVC   P+26(7),RAURKGRP                                                 
         GOTO1 =V(HEXOUT),DMCB,BFREC,P+38,20,=C'T0G'                            
         GOTO1 =V(PRINTER)                                                      
         LTR   RC,RC               RETURN CC NOT ZERO                           
         B     CMAX0020                                                         
CMAX0010 EQU   *                                                                
         SR    RE,RE               RETURN CC = ZERO                             
         LTR   RE,RE                                                            
CMAX0020 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*  DUMPOUTP:  UPON REQUEST, DUMP THE OUTPUT RECORD                              
*                                                                               
DUMPOUTP NTR1                                                                   
         CLC   =C'OUTPUT',DUMPID   DUMP OUTPUT RECORD?                          
         BE    DMOP0020            NO                                           
         CLC   =C'REGCOM',DUMPID   DUMP OUTPUT RECORD?                          
         BNE   DMOP0040            NO                                           
         L     R4,AREC             FIND X'02' ELEMENT                           
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
         CLI   ADDPUT,C'A'         RECORD BEING 'ADDED'?                        
         BNE   DMOP0030            NO                                           
         MVC   P+1(19),=C'RECORD BEING ADDED:'                                  
         GOTO1 =V(PRINTER)                                                      
         B     DMOP0035                                                         
DMOP0030 EQU   *                                                                
         MVC   P+1(23),=C'RECORD BEING REWRITTEN:'                              
         GOTO1 =V(PRINTER)                                                      
DMOP0035 EQU   *                                                                
         L     R4,AREC             A(RECORD BEING DISPLAYED)                    
         USING RAURRECD,R4                                                      
         ZICM  R3,RAURLEN,2        L(RECORD BEING DISPLAYED)                    
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R3),=C'2D'                
         B     DMOP0040                                                         
DMOP0040 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* PRINT OUT BUY RECORD AND CC                                                   
*                                                                               
PRINT1   NMOD1 0,*PRT1*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    P,P                                                              
         L     R5,AREC                                                          
         MVC   P(100),0(R5)                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
*&&DO                                                                           
         XC    P,P                                                              
         IPM   R5            PUTS CC IN BITS 02 && 03 OF R?                     
         SRL   R5,32-4        PUTS CC IN BITS 30 && 31 OF R?                    
         CVD   R5,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  P(2),DUB                                                         
         SPM   R5                                                               
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
P1X      XMOD1                                                                  
*                                                                               
* PRINT OUT BUY RECORD AND SDD ENTRY AND CC                                     
*                                                                               
PRINT2   NMOD1 0,*PRT2*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   P,SPACES                                                         
         MVC   P+1(04),=C'WORK'                                                 
         MVC   P+12(32),WORK                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P+1(08),=C'INDBUCKT'                                             
         MVC   P+12(8),0(R2)                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R5,BFREC                                                         
         MVC   P+1(08),=C'BUFLOREC'                                             
         MVC   P+12(32),0(R5)                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P,SPACES                                                         
         L     R5,ACURSDDT                                                      
         MVC   P+1(08),=C'RECTABLE'                                             
         MVC   P+12(200),0(R5)                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
*&&DO                                                                           
         XC    P,P                                                              
         IPM   R5            PUTS CC IN BITS 02 && 03 OF R?                     
         SRL   R5,32-4        PUTS CC IN BITS 30 && 31 OF R?                    
         CVD   R5,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  P(2),DUB                                                         
         SPM   R5                                                               
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
P2X      XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         CSECT                                                                  
PSEUDO   NMOD1 0,*PSEU*                                                         
         L     RC,0(R1)            LOAD A(WORKSPACE)                            
         L     R6,4(R1)            LOAD A(RECORD)                               
         ZAP   WORK(5),=P'99999999'                                             
         L     R6,AREC                                                          
         PACK  WORK+10(1),21(1,R6)   INVERT SEQUENCE OF DIGITS                  
         PACK  WORK+11(1),20(1,R6)   INVERT SEQUENCE OF DIGITS                  
         PACK  WORK+12(1),19(1,R6)   INVERT SEQUENCE OF DIGITS                  
         PACK  WORK+13(1),18(1,R6)   INVERT SEQUENCE OF DIGITS                  
         MVI   WORK+14,X'0C'       MOVE IN SIGN AND                             
         SRP   WORK+10(5),64-1,0   SHIFT ONE DEC'L PLACE                        
         SP    WORK(5),WORK+10(5)                                               
         SRP   WORK(5),1,0                                                      
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),WORK(4)                                                 
         EDIT  (P5,DUB+3),(8,P+1),FILL=0,ZERO=NOBLANK                           
         LA    R6,25(R6)                                                        
         GOTO1 =V(HEXOUT),DMCB,(R6),P+12,2,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'106REAURCRA  05/01/02'                                      
         END                                                                    
