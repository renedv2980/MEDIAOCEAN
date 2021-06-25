*          DATA SET RESFM15B   AT LEVEL 095 AS OF 05/01/02                      
*PHASE T81815B,*                                                                
         TITLE 'T81815 - RESFM15 - A.U.R. REPORT:  COMBO/MARKET'                
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESFM15 (T81815) --- A.U.R. REPORT: COMBO/MARKET         *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* JUN17/93 (BU ) --- ORIGINAL ENTRY                               *             
*                                                                 *             
* JUL15/93 (BU ) --- 'TOTALS ONLY' ON MKT REPORTS ONLY            *             
*                                                                 *             
* JUL16/93 (BU ) --- EXPAND AGENCY/AGENCY-OFFICE FILTER           *             
*                                                                 *             
* OCT28/94 (BU ) --- FIX MARKET NAME DISPLAY                      *             
*                                                                 *             
* SEP30/98 (JRD) --- ADD MASTER REP REPORT VALIDATION             *             
*                                                                 *             
*                    **  END TOMBSTONE  **                        *             
*******************************************************************             
*                                                                               
T81815   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1815**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R2,RELO                                                          
         SPACE                                                                  
         LA    RF,SAVEREGS                                                      
         STM   R2,RC,0(RF)         SAVE REGS 2 -> C                             
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE FIELDS                                                  
         SPACE 2                                                                
VKEY     DS    0H                                                               
         XC    REPSTBL,REPSTBL     CLEAR REPCODE LIST                           
*                                                                               
         XC    KEY,KEY             READ REP RECORD AND BUILD REPSTBL            
K        USING RREPKEY,KEY                                                      
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,AGENCY                                                
         DROP  K                                                                
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RREPREC,R6                                                       
         CLC   RREPMAST,=X'FFFF'   MASTER REP?                                  
         BE    *+14                YES                                          
         MVC   REPSTBL(2),AGENCY                                                
         B     RDREP100                                                         
*                                                                               
         MVI   ELCODE,X'02'        READ SUBSIDIARY ELEMENTS                     
         USING RREPSUB,R6                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS IS SCREWY                               
         ZIC   RF,RREPSCNT                                                      
         LA    R6,RREPSCOD                                                      
         DROP  R6                                                               
*                                                                               
         LA    R3,REPSTBL                                                       
RDREP010 DS    0H                                                               
         LA    R0,REPSTBL+L'REPSTBL                                             
         CR    R3,R0               CHECK END OF TABLE                           
         BL    *+14                                                             
         MVC   CONHEAD(L'REPSERR),REPSERR                                       
         B     MYEND                                                            
*                                                                               
         MVC   0(2,R3),0(R6)                                                    
         LA    R6,2(R6)                                                         
         LA    R3,2(R3)                                                         
         BCT   RF,RDREP010                                                      
*                                                                               
RDREP100 DS    0H                                                               
         EJECT                                                                  
         XC    MYWORK(256),MYWORK                                               
         XC    MYWORK+256(256),MYWORK+256                                       
         LA    R5,SVFOOT                                                        
         ST    R5,SVFOOTF          SAVE ADDRESS OF FOOT LINES                   
         SPACE 1                                                                
         CLI   TWAOFFC,C'*'        DDS TERMINAL                                 
         BE    VK10                                                             
         CLI   TWAACCS,C'$'        TEST FOR STATION LIMITED ACCESS              
         BNE   VK10                                                             
         OI    PRNTOPT,X'10'       INDICATES LIMITED ACCESS                     
         SPACE 1                                                                
VK10     DS    0H                                                               
*****    LA    R2,AUCGRPH                                                       
*****    CLI   5(R2),0                                                          
*****    BNE   VK20                                                             
         LA    R2,AUCSTAH                                                       
         CLI   5(R2),0                                                          
         BNE   VK50                                                             
         SPACE 1                                                                
VKGRPER  MVC   CONHEAD+10(L'GRPORSTA),GRPORSTA                                  
         B     MYEND               GROUP OR STATION IS REQUIRED                 
         SPACE 1                                                                
*                              ********************************                 
*  NOT SUPPORTED FOR MASTER    *        VALIDATE              *                 
*   LEVEL VALIDATION           *          GROUP               *                 
*                              ********************************                 
*K20     DS    0H                                                               
*****    MVI   ERROR,SECLOCK                                                    
*****    TM    PRNTOPT,X'10'       LIMITED TO SINGLE STATION                    
*****    BO    ERREND                                                           
*****    GOTO1 VALIGRP                                                          
*****    MVC   GROUP,WORK                                                       
*****    MVC   GROUPN(21),SPACES                                                
*****    MVC   GROUPN(10),WORK+10  GROUP NAME                                   
*****    MVC   SGROUPN(10),WORK+20 SUB GROUP NAME                               
*****    LA    R1,GROUPN+9                                                      
*****    LA    RE,9                                                             
*K30     DS    0H                                                               
*****    CLI   0(R1),C' '                                                       
*****    BNE   VK40                                                             
*****    BCTR  R1,0                                                             
*****    BCT   RE,VK30                                                          
*K40     DS    0H                                                               
*****    MVC   2(10,R1),SGROUPN    SUB GROUP NAME                               
*****    OC    GROUPN(21),SPACES                                                
*****    LA    R2,AUCSTAH                                                       
*****    CLI   5(R2),0                                                          
*****    BNE   VK50                NEED STATION OR 'MKT=CODE'                   
*****    LA    R2,AUCSTAH          POINT AT STATN FIELD FOR ERROR               
*****    MVC   CONHEAD+10(L'STAORMKT),STAORMKT                                  
*****    B     MYEND               GROUP OR STATION IS REQUIRED                 
         SPACE 1                                                                
*                              ********************************                 
* REQUIRES MASTER LEVEL        *        VALIDATE              *                 
* STATION CONTROL TO WORK      *         STATION              *                 
*                              ********************************                 
VK50     EQU   *                                                                
         CLC   =C'MKT=',AUCSTA     REQUEST FOR STATIONS IN MARKET?              
         BNE   VK52                NO                                           
         MVI   COMBOFLG,C'N'       TURN OFF COMBO FLAG.                         
         BAS   RE,MKTSTATS                                                      
         BNZ   MYEND               ERROR RETURNED                               
         CLI   SECVIOL,C'Y'        SECURITY VIOLATION RETURNED?                 
         BE    ERREND              YES                                          
         B     VK62                FINISHED WITH STATION LOAD                   
VK52     EQU   *                                                                
         GOTO1 VALISTA                                                          
         MVC   STATN,WORK                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              READ STATION RECORD TO GET GROUP             
         L     R6,AIO                                                           
         MVC   GROUP,RSTAGRUP-RSTAREC(R6)                                       
         MVC   MARKET(20),RSTAMKT-RSTAREC(R6)                                   
         SPACE 1                                                                
         TM    PRNTOPT,X'10'       LIMITED ACCESS                               
         BZ    VK60                                                             
         MVI   ERROR,SECLOCK                                                    
         MVI   ELCODE,X'06'        GET ELEMENT FOR VALID SIGN-ON IDS            
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VK55     BAS   RE,NEXTEL                                                        
         BNE   ERREND                                                           
         CLC   TWAORIG,10(R6)      SIGN-ON ID                                   
         BNE   VK55                                                             
VK60     EQU   *                                                                
         XC    STATLIST,STATLIST   CLEAR STATION LIST                           
         MVI   COMBOFLG,C'N'       SET 'NOT COMBO REQUEST'                      
         L     R6,AIO                                                           
         MVC   STATLIST(2),RSTAGRUP-RSTAREC(R6)                                 
*                                  INSERT GROUP INTO TABLE ENTRY 1              
         MVC   STATLIST+2(5),RSTAKSTA-RSTAREC(R6)                               
*                                  INSERT STATION INTO TABLE ENTRY 1            
         MVC   STATLIST+7(7),STATLIST                                           
*                                  DUPLICATE ENTRY IN TABLE ENTRY 2             
         CLI   RSTAKSTA+4-RSTAREC(R6),C'C'                                      
         BNE   VK62                                                             
         MVI   COMBOFLG,C'Y'       SET 'COMBO REQUEST'                          
         BAS   RE,LOADCMBO                                                      
         B     VK62                                                             
VK62     EQU   *                                                                
         MVC   STATN,STATLIST+2    LOAD 1ST ENTRY FROM TABLE                    
*                                     INTO 'STATION FROM REQUEST'               
         B     VK63                                                             
         EJECT                                                                  
*                                                                               
*   FOR COMBO REQUESTS, LOAD STATIONS PARTICIPATING IN COMBO                    
*                                                                               
LOADCMBO NTR1                                                                   
         LA    RE,1                SET STATION COUNTER TO 1                     
         MVI   STADELIM,0          SET DELIMITER                                
         L     R6,AIO              A(IO AREA)                                   
         LA    R6,34(R6)           A(01 ELEMENT)                                
         LA    R3,STATLIST+7                                                    
LCMB0010 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    LCMB0090            YES                                          
         CLI   0(R6),X'0A'         COMBO STATION ELEMENT?                       
         BNE   LCMB0030            NO                                           
         MVC   2(5,R3),2(R6)       YES - LOAD COMBO STATION                     
         MVC   0(2,R3),STATLIST    FORCE IN GROUP/SUBGROUP                      
         LA    R3,7(R3)            BUMP TO NEXT ELEMENT                         
         LA    RE,1(RE)            BUMP STATION COUNTER                         
LCMB0030 EQU   *                                                                
         ZIC   RF,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,RF                                                            
         B     LCMB0010            GO BACK FOR NEXT                             
LCMB0090 EQU   *                                                                
         LA    RE,1(RE)            ADD 1 FOR TOTALS                             
         ST    RE,STATCTR          SAVE STATION COUNTER                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                              ********************************                 
*                              *        VALIDATE              *                 
*                              *         PERIOD   (MMMYY)     *                 
*                              ********************************                 
VK63     LA    R2,AUCPERH                                                       
         GOTO1 ANY                                                              
         MVI   ERROR,INVDATE                                                    
         GOTO1 SCANNER,DMCB,(R2),(1,WORK),C',=,-'                               
         CLI   4(R1),0                                                          
         BE    ERREND                                                           
         GOTO1 DATVAL,(R1),(0,WORK+12),DUB     IF IT'S OK AS MMMDDYY            
         OC    DMCB(4),DMCB                    THEN IT'S NOT MMMYY              
         BNZ   VK65                             (FUDGE FOR DATCON)              
         GOTO1 (RF),(R1),(2,WORK+12),DUB                                        
         OC    DMCB(4),DMCB                                                     
         BNZ   VK70                                                             
VK65     MVC   CONHEAD+10(L'DATEFMT),DATEFMT                                    
         B     MYEND                                                            
         SPACE 1                                                                
VK70     GOTO1 DATCON,(R1),(0,DUB),(3,WORK+40)                                  
         MVC   SMONTH,WORK+40                                                   
         MVC   EMONTH,WORK+40      IF NO END MONTH USE START                    
         CLI   WORK+1,0                                                         
         BE    VK120                                                            
*                                                                               
         GOTO1 DATVAL,(R1),(0,WORK+22),DUB      IF IT'S OK AS MMMDDYY           
         OC    DMCB(4),DMCB                     THEN IT'S NOT MMMYY             
         BNZ   VK65                                (FUDGE FOR DATCON)           
         GOTO1 (RF),(R1),(2,WORK+22),DUB                                        
         OC    DMCB(4),DMCB                                                     
         BZ    VK65                                                             
         GOTO1 DATCON,(R1),(0,DUB),(3,WORK+50)                                  
         MVC   EMONTH,WORK+50                                                   
*                                                                               
         CLC   SMONTH,EMONTH       START BEFORE END                             
         BH    ERREND                                                           
         ZIC   RE,WORK+40          MORE THAN 1 YEAR                             
         AH    RE,=H'1'                                                         
         STC   RE,WORK+40                                                       
         CLC   WORK+40(2),EMONTH                                                
         BNH   ERREND                                                           
VK120    MVC   PSMONTH(4),SMONTH   SET UP PRIOR YEAR                            
         ZIC   RE,PSMONTH                                                       
         SH    RE,=H'1'                                                         
         STC   RE,PSMONTH                                                       
         ZIC   RE,PEMONTH                                                       
         SH    RE,=H'1'                                                         
         STC   RE,PEMONTH                                                       
*                              *********************************                
*                              *  VALIDATE REPORT TYPE         *                
*                              * L=LENGTH, P=PERIOD, D=DAYPART *                
*                              *********************************                
         LA    R2,AUCTYPH                                                       
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'L'                                                       
         BNE   *+12                                                             
         MVI   REPTYP,C'L'                                                      
         B     VK150                                                            
         CLI   8(R2),C'P'                                                       
         BNE   ERREND                                                           
         MVI   REPTYP,C'P'                                                      
         B     VK150                                                            
*        CLI   8(R2),C'D'                                                       
*        BNE   ERREND                                                           
*        MVI   REPTYP,C'D'                                                      
         SPACE 1                                                                
*                              *********************************                
*                              *  VALIDATE DAYPART - OPTIONAL  *                
*                              *   OR UP TO 4 DAYPARTS ALLOWED *                
*                              *********************************                
VK150    LA    R2,AUCDAYH                                                       
         CLI   5(R2),0                                                          
         BE    VK360                                                            
         SPACE 1                                                                
         MVI   BYTE,2              MAX LENGTH OF DAYPART                        
         BAS   RE,SEPARATE   WORK SET WITH DPTS, BYTE & R5=# OF DPTS            
         CLI   BYTE,4                                                           
         BNH   VK160                                                            
         MVC   CONHEAD(L'MANYDPT),MANYDPT                                       
         B     MYEND                                                            
         SPACE 1                                                                
VK160    STC   R5,DPTCNT           SAVE COUNT                                   
         LA    R3,WORK                                                          
*                                                                               
VK220    CLI   0(R3),C'*'                                                       
         BE    VK300                                                            
*                                                                               
         LA    R4,REPSTBL          POINT TO REP LIST                            
VK222    DS    0H                                                               
         OC    0(2,R4),0(R4)       END OF REP TABLE?                            
         BZ    VKDPTER             YES - DAYPART DOESN'T EXIST                  
         LA    R0,REPSTBL+L'REPSTBL                                             
         CR    R4,R0                                                            
         BNL   VKDPTER                                                          
*                                                                               
         XC    KEY,KEY             DAYPART RECORD                               
         MVI   KEY,X'24'                                                        
         MVC   KEY+RDPTKREP-RDPTKEY(2),0(R4)                                    
         MVC   KEY+RDPTKDPT-RDPTKEY(1),0(R3)                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BE    *+12                                                             
         CLI   1(R3),0             IF PRIMARY DPT USE ALL 2NDARY DPTS           
         BE    VK320                                                            
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'02'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    VK280                                                            
         B     VK262                                                            
*                                                                               
VK260    BAS   RE,NEXTEL           SUB DPT FOUND?                               
         BE    VK280                                                            
*                                                                               
VK262    LA    R4,2(R4)             NO -TRY NEXT REP                            
         B     VK222                                                            
*                                                                               
VK280    CLC   1(1,R3),2(R6)                                                    
         BNE   VK260                                                            
         B     VK320                                                            
*                                                                               
VKDPTER  DS    0H                                                               
         MVC   CONHEAD+13(21),=C'IS AN INVALID DAYPART'                         
         MVC   CONHEAD+10(2),0(R3)                                              
         B     MYEND                                                            
*                                                                               
VK300    DS    0H                                                               
         LA    R6,REPSTBL          POINT TO REP LIST                            
VK302    DS    0H                                                               
         OC    0(2,R6),0(R6)       END OF REP TABLE?                            
         BZ    VKPRGER             YES - DAYPART DOESN'T EXIST                  
         LA    R0,REPSTBL+L'REPSTBL                                             
         CR    R6,R0                                                            
         BNL   VKPRGER                                                          
*                                                                               
         MVI   0(R3),X'FE'         INDICATE PROGRAM TYPE FOR SORT               
         XC    KEY,KEY                                                          
         MVI   KEY,X'25'                                                        
         MVC   KEY+RPGTKREP-RPGTKEY(2),0(R6)                                    
         MVC   KEY+RPGTKPGT-RPGTKEY(1),1(R3)                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+12                                                             
         LA    R6,2(R6)             NO -TRY NEXT REP                            
         B     VK302                                                            
*                                                                               
VKPRGER  DS    0H                                                               
         MVC   CONHEAD+12(26),=C'IS AN INVALID PROGRAM TYPE'                    
         MVC   CONHEAD+10(1),1(R3)                                              
         B     MYEND                                                            
*                                                                               
VK320    LA    R3,2(R3)                                                         
         BCT   R5,VK220                                                         
*                                                                               
         MVC   DPT,WORK                                                         
         SPACE 2                                                                
* SORT ON DAYPART CODES                                                         
*                                                                               
         ZIC   R5,DPTCNT                                                        
         CH    R5,=H'1'                                                         
         BNH   VK350                                                            
         GOTO1 XSORT,DMCB,DPT,(R5),2,2,0                                        
         SPACE 1                                                                
VK350    L     R5,SVFOOTF                                                       
         MVC   0(4,R5),=C'DPT='                                                 
         LA    R5,4(R5)                                                         
         BAS   RE,FOOT                                                          
*                                                                               
*                              *********************************                
*                              *  VALIDATE LENGTH  -           *                
*                              *   IF REPTYP=L - OPTIONAL      *                
*                              *    BLANK MEANS EACH LENGTH    *                
*                              *    OR UP TO 4 LENGTHS ALLOWED *                
*                              *   IF REPTYP=P                 *                
*                              *    BLANK MEANS 1 LINE TOTAL   *                
*                              *    OR 1 LENGTH ALLOWED        *                
*                              *********************************                
VK360    LA    R2,AUCLENH                                                       
         CLI   REPTYP,C'P'                                                      
         BNE   VK370                                                            
         CLI   5(R2),0                                                          
         BNE   VK380                                                            
         OI    PRNTOPT,X'20'       LENGTH = TOT                                 
         B     VK490                                                            
         SPACE 1                                                                
VK370    CLI   5(R2),0                                                          
         BE    VK490                                                            
         SPACE 1                                                                
VK380    MVI   BYTE,4                                                           
         BAS   RE,SEPARATE      WORK SET WITH LEN - BYTE & R5 # LENS            
         CLI   REPTYP,C'P'         PERIOD FORMAT CAN ONLY HAVE 1 LEN            
         BNE   VK390                                                            
         CLI   BYTE,1                                                           
         BE    VK410                                                            
         MVC   CONHEAD+10(L'MANYLENP),MANYLENP                                  
         B     MYEND                                                            
         SPACE 1                                                                
VK390    CLI   BYTE,4                                                           
         BNH   VK410                                                            
         MVC   CONHEAD+10(L'MNYLEN),MNYLEN                                      
         B     MYEND                                                            
         SPACE 1                                                                
VK410    STC   R5,LENCNT           SAVE COUNT                                   
         LA    R3,WORK                                                          
         LA    R4,LEN                                                           
VK420    LA    R6,4                MAX LENGTH                                   
         LR    RE,R3               CHECK FOR M,1-9                              
         B     *+12                                                             
*                                                                               
VK440    CLI   0(RE),0                                                          
         BE    VK460                                                            
         CLI   0(RE),C'M'          MINUTES?                                     
         BE    VK460                                                            
         CLI   0(RE),X'F0'                                                      
         BL    VKLENER                                                          
         CLI   0(RE),X'F9'                                                      
         BH    VKLENER                                                          
         LA    RE,1(RE)                                                         
         BCT   R6,VK440                                                         
         B     VK460                                                            
         SPACE 1                                                                
VKLENER  MVC   CONHEAD+10(14),=C'INVALID LENGTH'                                
         B     MYEND                                                            
         SPACE 1                                                                
* PACK MINUTES                                                                  
VK460    LA    R1,4                                                             
         SR    R1,R6                                                            
         BNP   VKLENER                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VK470                                                            
         PACK  DUB,0(0,R3)                                                      
VK470    CVB   R0,DUB                                                           
         STH   R0,0(R4)                                                         
         CLI   0(RE),C'M'          MINUTES?                                     
         BNE   *+8                                                              
         OI    0(R4),X'80'         TURN ON MINUTES INDICATOR                    
         LA    R3,4(R3)                                                         
         LA    R4,2(R4)                                                         
         BCT   R5,VK420                                                         
         SPACE                                                                  
* SORT ON LENGTH CODES                                                          
         SPACE                                                                  
         ZIC   R5,LENCNT                                                        
         CH    R5,=H'1'                                                         
         BNH   VK480                                                            
         GOTO1 XSORT,DMCB,LEN,(R5),2,2,0                                        
         SPACE 1                                                                
VK480    L     R5,SVFOOTF                                                       
         MVC   0(4,R5),=C'LEN='                                                 
         LA    R5,4(R5)                                                         
         BAS   RE,FOOT                                                          
*                                                                               
*                 **********************************************                
*                 *   VALIDATE OWNERSHIP CODE - OPTIONAL       *                
*                 *      NOT VALID WHEN MKT= NOT USED          *                
*                 **********************************************                
VK490    LA    R2,AUCOWNH                                                       
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BE    VK560                                                            
         CLC   AUCSTA(4),=C'MKT='                                               
         BE    VK500                                                            
VKMKTONL MVC   CONHEAD+10(L'MKTONLY),MKTONLY                                    
         B     MYEND                                                            
         SPACE 1                                                                
VK500    CLI   5(R2),3             CODE MUST BE 3 CHARACTERS                    
         BNE   ERREND                                                           
*                                                                               
         MVI   ERROR,NOTFOUND                                                   
         XC    KEY,KEY             DAYPART RECORD                               
         MVI   KEY,X'2A'                                                        
         MVC   KEY+22(2),AGENCY                                                 
         MVC   KEY+24(3),8(R2)                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
*                                                                               
VK520    MVC   OWNER,8(R2)         OWNER CODE                                   
         MVC   OWNERN,2(R6)        OWNER NAME                                   
         SPACE 1                                                                
         L     R5,SVFOOTF                                                       
         MVC   0(4,R5),=C'OWN='                                                 
         LA    R5,4(R5)                                                         
         BAS   RE,FOOT                                                          
         SPACE 2                                                                
*                 **********************************************                
*                 *   VALIDATE TVB CODE - OPTIONAL             *                
*                 *      NOT VALID WHEN MKT= NOT USED          *                
*                 **********************************************                
VK560    LA    R2,AUCTVBH                                                       
         CLI   5(R2),0                                                          
         BE    VK590                                                            
         CLC   AUCSTA(4),=C'MKT='                                               
         BNE   VKMKTONL                                                         
         L     R1,=A(TVBLST)                                                    
         A     R1,RELO                                                          
VK570    CLC   0(2,R1),8(R2)                                                    
         BE    VK580                                                            
         CLI   0(R1),X'FF'                                                      
         BE    ERREND                                                           
         LA    R1,L'TVBLST(R1)                                                  
         B     VK570                                                            
         SPACE 1                                                                
VK580    MVC   TVB,0(R1)           TVB CODE                                     
         MVC   TVBN,2(R1)          TVB NAME                                     
         SPACE 1                                                                
         L     R5,SVFOOTF                                                       
         MVC   0(4,R5),=C'TVB='                                                 
         LA    R5,4(R5)                                                         
         BAS   RE,FOOT                                                          
         SPACE 1                                                                
*                 **********************************************                
*                 *   VALIDATE RANK - OPTIONAL                 *                
*                 *      NOT VALID WHEN MKT= NOT USED          *                
*                 **********************************************                
VK590    LA    R2,AUCRNKH                                                       
         CLI   5(R2),0                                                          
         BE    VK610                                                            
         CLC   AUCSTA(4),=C'MKT='                                               
         BNE   VKMKTONL                                                         
         MVI   ERROR,NOTNUM                                                     
         TM    4(R2),X'08'         VALID NUMERIC                                
         BZ    ERREND                                                           
         CLI   8(R2),C'7'          MUST BE FROM 1 TO 7                          
         BNH   VK600                                                            
         MVC   CONHEAD+10(L'RANKE2),RANKE2                                      
         B     MYEND                                                            
         SPACE 1                                                                
VK600    MVC   RANK,8(R2)                                                       
         SPACE 1                                                                
         L     R5,SVFOOTF                                                       
         MVC   0(4,R5),=C'RNK='                                                 
         LA    R5,4(R5)                                                         
         BAS   RE,FOOT                                                          
         SPACE 1                                                                
*                                  *****************************                
*                                  *         VALIDATE          *                
*                                  *        AS AT DATE         *                
*                                  *****************************                
VK610    LA    R2,AUCASATH                                                      
         CLI   5(R2),0                                                          
         BE    VK620                                                            
         MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(2,ASAT)      COMPRESSED                    
         SPACE 1                                                                
         L     R5,SVFOOTF                                                       
         MVC   0(4,R5),=C'AAD='                                                 
         LA    R5,4(R5)                                                         
         BAS   RE,FOOT                                                          
         SPACE 1                                                                
*                       ****************************************                
*                       *            VALIDATE FORMAT           *                
*                       *  M=MONTH, Q=QUARTERS, A=WHOLE MONTH  *                
*                       ****************************************                
VK620    LA    R2,AUCFMTH                                                       
         MVI   ERROR,INVALID                                                    
         MVI   MONFMT,C'A'                                                      
         CLI   5(R2),0                                                          
         BE    VK650                                                            
         CLI   8(R2),C'A'                                                       
         BE    VK640                                                            
         CLI   REPTYP,C'L'                                                      
         BNE   VK630                                                            
         MVC   CONHEAD+10(L'TYPER),TYPER                                        
         B     MYEND               FORMAT L MUST BE WHOLE MONTH                 
         SPACE 1                                                                
VK630    CLI   8(R2),C'M'                                                       
         BNE   *+12                                                             
         MVI   MONFMT,C'M'                                                      
         B     VK640                                                            
         CLI   8(R2),C'Q'                                                       
         BNE   ERREND                                                           
         MVI   MONFMT,C'Q'                                                      
         BAS   RE,CHKQTR           PERIOD MUST BE VALID QUARTERS                
         SPACE 1                                                                
VK640    L     R5,SVFOOTF                                                       
         MVC   0(3,R5),=C'FO='                                                  
         LA    R5,3(R5)                                                         
         MVC   0(1,R5),8(R2)                                                    
         MVI   1(R5),C','                                                       
         LA    R5,2(R5)                                                         
         ST    R5,SVFOOTF                                                       
         SPACE 1                                                                
*                       ****************************************                
*                       *            VALIDATE FILTERS          *                
*                       *  T=CON TYP, O=OFF, A=AGY, DEFAULT=ALL*                
*                       ****************************************                
VK650    LA    R2,AUCFLTH                                                       
         MVI   FILTER,1                                                         
         CLI   5(R2),0             ANY DATA IN FIELD?                           
         BE    VK900               NO                                           
         MVI   ERROR,INVALID                                                    
         L     R4,AIO2                                                          
         GOTO1 SCANNER,DMCB,(R2),(1,(R4)),0                                     
         CLI   4(R1),0             ANY DATA FOUND?                              
         BE    ERREND              NO                                           
         SPACE 1                                                                
         CLI   0(R4),1             FIRST HALF PRESENT?                          
         BNE   ERREND              NO  - CAN'T PROCESS                          
         CLI   1(R4),0             SECOND HALF PRESENT?                         
         BE    ERREND              NO  - CAN'T PROCESS                          
         CLI   12(R4),C'T'         CONTRACT TYPE?                               
         BNE   VK690                                                            
VK680    MVC   TYPCODE(1),22(R4)   SAVE CONTRACT TYPE                           
         MVI   FILTER,2            SET TYPE OF FILTER                           
         B     VK800                                                            
         SPACE 2                                                                
*                                  OFFICE                                       
VK690    CLI   12(R4),C'O'         OFFICE FILTER?                               
         BNE   VK710               NO                                           
         XC    KEY,KEY             YES - VALIDATE EXISTENCE                     
         MVI   KEY,X'04'                                                        
         MVC   KEY+23(2),AGENCY    INSERT REP CODE                              
         MVC   KEY+25(2),22(R4)    INSERT OFFICE CODE                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    VK700               YES                                          
         MVC   CONHEAD+10(16),=C'INVALID OFFICE  '                              
         B     MYEND                                                            
         SPACE 1                                                                
VK700    MVI   FILTER,4            SET TYPE CODE                                
         MVC   TYPCODE(2),22(R4) SAVE OFFICE CODE                               
         B     VK800                                                            
*                                                                               
*                                  AGENCY                                       
VK710    CLI   12(R4),C'A'         AGENCY?                                      
         BE    VK720               YES                                          
         MVI   ERROR,INVALID       NO  - NOT RECOGNIZED                         
         B     ERREND                                                           
         SPACE 1                                                                
VK720    XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(6),SPACES    SPACE-FILL AGENCY/OFFICE                     
         GOTO1 SETAGY,DMCB,(R4)    DEVELOP AGENCY/AGENCY-OFFICE                 
         BNZ   VK730                                                            
         MVC   KEY+25(2),AGENCY    INSERT REP CODE                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    VK780                                                            
VK730    EQU   *                                                                
         MVC   CONHEAD+10(18),=C'INVALID AGENCY    '                            
         B     MYEND                                                            
         SPACE 1                                                                
VK780    MVI   FILTER,3                                                         
         MVC   TYPCODE,SPACES      SPACE-FILL AREA                              
         MVC   TYPCODE(6),KEY+19   FILL IN AGENCY                               
         CLC   KEY+23(2),SPACES    CORPORATE AGENCY REQUESTED?                  
         BNE   VK800               NO  - SPECIFIC OFFICE                        
VK790    EQU   *                   CHECK IF OFFICES EXIST                       
         GOTO1 SEQ                 READ NEXT RECORD                             
         CLC   KEY(23),KEYSAVE     SAME THROUGH AGENCY?                         
         BNE   VK800               NO  - ACCEPT INPUT                           
         CLC   KEY+25(2),KEYSAVE+25                                             
*                                  SAME REP?                                    
         BNE   VK790               NO  - GO BACK FOR NEXT RECORD                
         MVC   CONHEAD+10(20),=C'AGENCY OFFICE NEEDED'                          
         B     MYEND                                                            
         SPACE 1                                                                
VK800    L     R5,SVFOOTF                                                       
         MVC   0(2,R5),=C'F='                                                   
         LA    R5,2(R5)                                                         
         BAS   RE,FOOT                                                          
*                                                                               
*                       ****************************************                
*                       *  VALIDATE SHOW SUB DAYPART OPTION    *                
*                       *  Y=SHOW SUB DAYPART*, N=SUPPRESS     *                
*                       ****************************************                
VK900    LA    R2,AUCSDPTH                                                      
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BE    VK950                                                            
         CLI   8(R2),C'Y'                                                       
         BE    VK930                                                            
         CLI   8(R2),C'N'                                                       
         BNE   ERREND                                                           
         OI    PRNTOPT,X'80'       SUPPRESS SUB DAYPART                         
         SPACE 1                                                                
VK930    L     R5,SVFOOTF                                                       
         MVC   0(4,R5),=C'OP1='                                                 
         LA    R5,4(R5)                                                         
         MVC   0(1,R5),8(R2)                                                    
         MVI   1(R5),C','                                                       
         LA    R5,2(R5)                                                         
         ST    R5,SVFOOTF                                                       
         SPACE 1                                                                
*                  *********************************************                
*                  *    *  VALIDATE SHOW ALL DAYPART OPTION    *                
*                  *Y=SHOW ALL DAYPARTS*,N=STATION TOTALS ONLY *                
*                  *********************************************                
VK950    LA    R2,AUCSTOTH                                                      
         CLI   5(R2),0                                                          
         BE    VK970                                                            
         CLI   8(R2),C'Y'                                                       
         BE    VK960                                                            
         CLI   8(R2),C'N'                                                       
         BNE   ERREND                                                           
         OI    PRNTOPT,X'40'       STATION TOTALS ONLY                          
         SPACE 1                                                                
VK960    L     R5,SVFOOTF                                                       
         MVC   0(4,R5),=C'OP2='                                                 
         LA    R5,4(R5)                                                         
         MVC   0(1,R5),8(R2)                                                    
         MVI   1(R5),C','                                                       
         LA    R5,2(R5)                                                         
         ST    R5,SVFOOTF                                                       
         SPACE 1                                                                
*                  *********************************************                
*                  *    *  VALIDATE TOTALS ONLY PAGE           *                
*                  *     Y = TOTAL PAGE ONLY, N= DEFAULT       *                
*                  *********************************************                
VK970    EQU   *                                                                
         MVI   TOTPAGE,C'N'        SET TO 'ALL PAGES'                           
         LA    R2,AUCSTTLH                                                      
         CLI   5(R2),0                                                          
         BE    VK1070                                                           
         CLI   8(R2),C'Y'          TOTAL PAGE ONLY?                             
         BNE   VK975               NO                                           
         CLI   COMBOFLG,C'Y'       COMBO REPORT?                                
         BE    ERREND              YES - NOT ALLOWED                            
         B     VK980                                                            
VK975    EQU   *                                                                
         CLI   8(R2),C'N'          ALL PAGES?                                   
         BNE   ERREND              NO  - UNRECOGNIZED                           
         SPACE 1                                                                
VK980    EQU   *                                                                
         MVC   TOTPAGE,8(R2)       SAVE OPTIONAL VALUE                          
         L     R5,SVFOOTF          PUT $$$ TYPE INTO FOOTER                     
         MVC   0(8,R5),=C'TOTPAGE='                                             
         LA    R5,8(R5)                                                         
         MVC   0(1,R5),8(R2)                                                    
         MVI   1(R5),C','                                                       
         LA    R5,2(R5)                                                         
         ST    R5,SVFOOTF                                                       
*                  *********************************************                
*                  *    *  VALIDATE $$ RETRIEVAL OPTION        *                
*                  *C=COMBO $$, B=COMBO+ REGULAR, DEFAULT = REG*                
*                  *********************************************                
VK1070   EQU   *                                                                
         MVI   OPT$$$,C'R'         SET TO 'REGULAR $$ ONLY'                     
         CLI   COMBOFLG,C'Y'       COMBO REQUEST?                               
         BNE   VK1075              NO                                           
         MVI   OPT$$$,C'C'         YES - SET DEFAULT TO 'COMBO $$'              
VK1075   EQU   *                                                                
         LA    R2,AUCSDOLH                                                      
         CLI   5(R2),0                                                          
         BE    VKEXT                                                            
         CLI   8(R2),C'C'          COMBO $$?                                    
         BE    VK1080              YES                                          
         CLI   8(R2),C'R'          REGULAR $$?                                  
         BE    VK1080              YES                                          
         CLI   8(R2),C'B'          BOTH COMBO+REGULAR $$?                       
         BNE   ERREND              NO  - UNRECOGNIZED                           
         SPACE 1                                                                
VK1080   EQU   *                                                                
         MVC   OPT$$$,8(R2)        SAVE OPTIONAL VALUE                          
         CLI   COMBOFLG,C'Y'       COMBO STATION REQUESTED?                     
         BNE   VK1090              NO                                           
         CLI   OPT$$$,C'R'         YES - REGULAR $$ REQUESTED?                  
         BNE   VK1090              NO                                           
         MVC   CONHEAD+10(L'COMBOREG),COMBOREG                                  
         B     MYEND                                                            
         SPACE 1                                                                
VK1090   EQU   *                                                                
         L     R5,SVFOOTF          PUT $$$ TYPE INTO FOOTER                     
         MVC   0(4,R5),=C'$$$='                                                 
         LA    R5,4(R5)                                                         
         MVC   0(1,R5),8(R2)                                                    
         MVI   1(R5),C','                                                       
         LA    R5,2(R5)                                                         
         ST    R5,SVFOOTF                                                       
VKEXT    DS    0H                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  SETAGY:  DEVELOP THE AGENCY KEY IF JUST AGENCY CODE, OR AGENCY/OFF           
*                                                                               
SETAGY   NTR1                                                                   
         L     R4,0(R1)            RESET R4 FOR ROUTINE                         
         LA    R2,22(R4)           A(AGENCY/AGENCY-OFFICE CODE)                 
         ZIC   RE,1(R4)            L(INPUT)                                     
         AR    R2,RE               FIND LAST POSITION OF FIELD                  
         BCTR  R2,0                BACK UP 1                                    
SETA0010 EQU   *                                                                
         CLI   0(R2),C'-'          SEPARATOR FOUND?                             
         BE    SETA0020            YES                                          
         BCTR  R2,0                NO  - BACK UP 1 CHARACTER                    
         BCT   RE,SETA0010         GO BACK FOR NEXT CHARACTER                   
         CLI   1(R4),4             NO '-' FOUND - CHECK LENGTH                  
         BH    SETA0090            TOO LONG - RETURN ERROR                      
         MVI   AGOFFFLG,C'C'       SET FLAG TO 'CODE ONLY'                      
         B     SETA0030                                                         
SETA0020 EQU   *                                                                
         BCTR  RE,0                CHECK LENGTH OF PRE-'-'                      
         CH    RE,=H'4'            MAX REMAINING = 4                            
         BH    SETA0090            TOO LONG - RETURN ERROR                      
         MVI   AGOFFFLG,C'O'       SET FLAG TO 'CODE+AGY OFFICE'                
SETA0030 EQU   *                                                                
         LA    R2,22(R4)           A(AGENCY/AGENCY-OFFICE CODE)                 
         ZIC   RE,1(R4)            L(INPUT)                                     
         LA    R1,KEY+19           A(KEY FIELD)                                 
SETA0040 EQU   *                                                                
         MVC   0(1,R1),0(R2)       MOVE AGENCY CODE TO KEY                      
         LA    R1,1(R1)            BUMP A(KEY)                                  
         LA    R2,1(R2)            BUMP A(SENDING)                              
         CLI   0(R2),C'-'          SEPARATOR?                                   
         BE    SETA0050            YES                                          
         BCT   RE,SETA0040         NO  - GO BACK FOR NEXT CHARACTER             
         B     SETA0070            FINISHED, NO SEPARATOR FOUND                 
SETA0050 EQU   *                                                                
         BCTR  RE,0                SKIP SEPARATOR                               
         LA    R2,1(R2)                                                         
         LA    R1,KEY+23           POINT TO KEY AGY/OFF FIELD                   
SETA0060 EQU   *                                                                
         MVC   0(1,R1),0(R2)       MOVE IN AGENCY OFFICE                        
         LA    R1,1(R1)            BUMP A(KEY)                                  
         LA    R2,1(R2)            BUMP A(SENDING)                              
         BCT   RE,SETA0060         GO BACK FOR NEXT CHARACTER                   
SETA0070 EQU   *                                                                
         SR    R0,R0               RETURN CC = ZERO:  OKAY                      
SETA0080 EQU   *                                                                
         XIT1                                                                   
SETA0090 EQU   *                                                                
         LTR   RB,RB               SET CC = NOT ZERO:  ERROR                    
         B     SETA0080                                                         
         EJECT                                                                  
SVCTBL   DS    0CL3                                                             
         DC    C'ARB'                                                           
         DC    C'NSI'                                                           
         DC    C'SRC'                                                           
         DC    C'BIR'                                                           
         DC    C'TRC'                                                           
         DC    C'MTD'                                                           
         DC    C'RAM'                                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
FOOT     ST    RE,FULL             MOVE SCREEN INPUT TO FOOT LINE               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCFUT                                                        
         LA    R5,1(RE,R5)                                                      
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         ST    R5,SVFOOTF                                                       
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
MVCFUT   MVC   0(0,R5),8(R2)       MOVE SCREEN INPUT TO FOOT LINE               
         EJECT                                                                  
*  ROUTINE REMOVES COMMAS FROM UP TO 4 ENTRIES AND PUTS THEM                    
* INTO A STRING IN 'WORK'.                                                      
*    ON ENTRY, BYTE HAS MAX LENGTH OF EACH ENTRY.                               
*       ON EXIT, BYTE HAS TRUE NUMBER OF ENTRIES FOR INDIVIDUAL                 
*                MAXIMUM ERROR.                                                 
*       ON EXIT, R5 HAS THE NUMBER OF ENTRIES (1-4)                             
*                                                                               
         SPACE 2                                                                
SEPARATE ST    RE,FULL                                                          
         MVI   ERROR,INVALID                                                    
         XC    WORK,WORK                                                        
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,WORK                                                          
         LA    R4,ELEMENT          INPUT FROM SCREEN LINE                       
         SPACE 1                                                                
         GOTO1 SCANNER,DMCB,(R2),(4,(R4)),0                                     
         CLI   4(R1),0                                                          
         BE    ERREND                                                           
         LA    R6,4                                                             
         SR    R5,R5               COUNTER                                      
         ZIC   R0,BYTE                                                          
SEP40    SR    RE,RE                                                            
         ICM   RE,1,0(R4)          LENGTH OF ENTRY                              
         BZ    SEP80                                                            
         CR    RE,R0                                                            
         BH    ERREND                                                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),12(R4)                                                   
         BCTR  R5,0                COUNT IT                                     
         AR    R3,R0               INCREMENT OUTPUT TABLE                       
         SPACE 1                                                                
         LA    R4,32(R4)           NEXT ENTRY                                   
         BCT   R6,SEP40                                                         
         SPACE 1                                                                
SEP80    LPR   R5,R5                                                            
         MVC   BYTE,4(R1)      TRUE NUMBER OF ENTRIES (FOR MAX ERROR)           
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*  ROUTINE TO CHECK THAT PERIOD IS VALID QUARTER DATES                          
         SPACE                                                                  
CHKQTR   CLI   SMONTH+1,1          MUST START WITH JANUARY                      
         BE    CQ20                                                             
         CLI   SMONTH+1,4          OR APRIL                                     
         BE    CQ20                                                             
         CLI   SMONTH+1,7          OR JULY                                      
         BE    CQ20                                                             
         CLI   SMONTH+1,X'0A'      OR OCTOBER                                   
         BNE   CQERR                                                            
         SPACE                                                                  
CQ20     CLI   EMONTH+1,3          AND END WITH MARCH                           
         BE    CQ40                                                             
         CLI   EMONTH+1,6          OR JULY                                      
         BE    CQ40                                                             
         CLI   EMONTH+1,9          OR SEPTEMBER                                 
         BE    CQ40                                                             
         CLI   EMONTH+1,X'0C'      OR DECEMBER                                  
         BE    CQ40                                                             
CQERR    LA    R2,AUCPERH          POINT CURSOR TO PERIOD                       
         MVC   CONHEAD+10(L'NOTQTR),NOTQTR                                      
         B     MYEND                                                            
         SPACE 1                                                                
CQ40     BR    RE                                                               
         SPACE 2                                                                
*  ROUTINE TO SLOT MONTHS INTO THE CORRECT QUARTERS                             
*     R1 WILL HAVE QUARTER NUMBER (1-4) ON EXIT                                 
         SPACE 2                                                                
GETQTR   LA    R1,4                                                             
*                                                                               
         CLI   KEY+AYME+1,9                                                     
         BH    GQ20                                                             
         BCTR  R1,0                                                             
         CLI   KEY+AYME+1,6                                                     
         BH    GQ20                                                             
         BCTR  R1,0                                                             
         CLI   KEY+AYME+1,3                                                     
         BH    GQ20                                                             
         BCTR  R1,0                                                             
*                                                                               
GQ20     BR    RE                                                               
         EJECT                                                                  
*                                                                               
*   MKTSTATS:  UTILIZES THE MARKET CODE ENTERED BY USER TO SELECT               
*        STATIONS FROM THE FILE FOR INCLUSION ON REPORT                         
*                                                                               
MKTSTATS NTR1                                                                   
         MVC   OWNER,AUCOWN        LOAD UNVALIDATED OWNER CODE                  
         MVC   TVB,AUCTVB          LOAD UNVALIDATED TVB CODE                    
         MVC   RANK,AUCRNK         LOAD UNVALIDATED RANK                        
*                                                                               
*   STATIONS MUST BE FILTERED AS THEY ARE TABLED.  FILTER IS AGAINST            
*     UNVALIDATED VALUE.  IF INVALID LATER, RUN WILL NOT GO THROUGH             
*     ANYWAY, AND TABLE WILL BE REBUILT.                                        
*                                                                               
         XC    STATLIST,STATLIST                                                
         MVI   STADELIM,0          SET DELIMITER                                
         XC    STATCTR,STATCTR                                                  
         MVI   SECVIOL,C'N'                                                     
         LA    R4,STATLIST                                                      
         XC    KEY,KEY             VALIDATE MARKET CODE                         
         MVI   KEY,X'2B'           INSERT REC TYPE                              
         MVC   KEY+21(2),AGENCY    INSERT REP CODE                              
         LA    RF,AUCSTAH          GET LENGTH OF INPUT                          
         ZIC   RF,5(RF)                                                         
         LA    RE,4                SUBTRACT L(MKT=)                             
         SR    RF,RE                                                            
         EX    RF,MKTMOVE                                                       
         B     MKTS0020                                                         
MKTMOVE  MVC   KEY+23(0),AUCSTA+4  INSERT CODE BY LENGTH                        
MKTS0020 EQU   *                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BE    MKTS0040            YES                                          
         MVC   CONHEAD+10(L'MKTBAD),MKTBAD                                      
         B     MKTS0220            EXIT WITH ERROR                              
MKTS0040 EQU   *                                                                
         MVC   SAVMKTCD,KEY+23     SAVE THE MARKET CODE                         
         XC    KEY,KEY             CYCLE THROUGH STATIONS                       
         MVI   KEY,X'02'           INSERT REC TYPE                              
         MVC   KEY+20(2),AGENCY    INSERT REP CODE                              
         GOTO1 HIGH                GET FIRST RECORD                             
         B     MKTS0080                                                         
MKTS0060 EQU   *                                                                
         GOTO1 SEQ                                                              
MKTS0080 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME TYPE/REP?                               
         BNE   MKTS0200            NO  - FINISHED                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'08'        GET EXTENDED DESCRIP ELT                     
         BAS   RE,GETEL                                                         
         BNE   MKTS0060            NOT FOUND - SKIP STATION                     
         CLC   SAVMKTCD,RSTAMKTC-RSTAXXEL(R6)                                   
         BNE   MKTS0060            NOT RIGHT MARKET - SKIP IT                   
         L     R6,AIO              MARKET FOUND -                               
         USING RSTAREC,R6                                                       
MKTS0100 EQU   *                                                                
         OC    OWNER,OWNER         ANY OWNER?                                   
         BZ    MKTS0120            NO                                           
         CLC   OWNER,RSTAOWN       SAME OWNER?                                  
         BNE   MKTS0060            NO  - SKIP IT                                
MKTS0120 EQU   *                                                                
         OC    TVB,TVB             ANY TVB?                                     
         BZ    MKTS0140            NO                                           
         CLC   TVB,RSTATVB         SAME TVB?                                    
         BNE   MKTS0060            NO  - SKIP IT                                
MKTS0140 EQU   *                                                                
         OC    RANK,RANK           ANY RANK?                                    
         BZ    MKTS0160            NO                                           
         CLC   RANK,RSTARANK       SAME RANK?                                   
         BNE   MKTS0060            NO  - SKIP IT                                
MKTS0160 EQU   *                                                                
         OC    STATLIST(7),STATLIST  ANY STATIONS ENTERED?                      
         BNZ   MKTS0180            YES - NOT FIRST TIME                         
         MVC   STATN,KEY+22        NO  - SAVE FIRST STATION FOUND               
         L     R6,AIO              GET DESCRIPTIVE INFORMATION                  
         MVC   GROUP,RSTAGRUP-RSTAREC(R6)                                       
         MVC   MARKET(20),RSTAMKT-RSTAREC(R6)                                   
         TM    PRNTOPT,X'10'       LIMITED ACCESS                               
         BZ    MKTS0180                                                         
         MVI   ERROR,SECLOCK                                                    
         MVI   ELCODE,X'06'        GET ELEMENT FOR VALID SIGN-ON IDS            
         BAS   RE,GETEL                                                         
         B     *+8                                                              
MKTS0170 BAS   RE,NEXTEL                                                        
         BNE   MKTS0240            RETURN TO ERREND....                         
         CLC   TWAORIG,10(R6)      SIGN-ON ID                                   
         BNE   MKTS0170                                                         
MKTS0180 EQU   *                                                                
         L     R6,AIO              RESET A(IO AREA)                             
         MVC   0(2,R4),RSTAGRUP-RSTAREC(R6)                                     
*                                  INSERT GROUP INTO TABLE                      
         MVC   2(5,R4),RSTAKSTA-RSTAREC(R6)                                     
*                                  INSERT STATION INTO TABLE                    
         L     RF,STATCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,STATCTR          INCREMENT STATION COUNTER                    
         LA    R4,7(R4)            BUMP TO NEXT TABLE POSITION                  
         B     MKTS0060                                                         
MKTS0200 EQU   *                                                                
         L     RF,STATCTR          ADD 1 STATION FOR TOTALS                     
         LA    RF,1(RF)                                                         
         ST    RF,STATCTR                                                       
         SR    R0,R0               SET CC = ZERO GOOD RETURN                    
         B     MKTS0260            GO BACK                                      
MKTS0220 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO BAD RETURN                   
         B     MKTS0260            GO BACK                                      
MKTS0240 EQU   *                                                                
         MVI   SECVIOL,C'Y'        SECURITY VIOLATION RETURN                    
         SR    R0,R0               CC OVERRIDDEN ON BAD RETURN                  
MKTS0260 EQU   *                                                                
         XIT1                                                                   
         SPACE 3                                                                
MKTBAD   DC    C'MARKET CODE ENTERED NOT RECOGNIZED'                            
REPSERR  DC    C'TOO MANY SUBSIDIARY REPS, CONTACT DDS'                         
         EJECT                                                                  
SAVEREGS DS    11F                                                              
         SPACE 2                                                                
DASH     DC    10C'-'                                                           
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
MYEND    MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*  MY OWN ERROR MESSAGES                                                        
         SPACE 2                                                                
GRPORSTA DC    C'EITHER GROUP OR STATION REQUIRED'                              
STAORMKT DC    C'STATION OR MKT=CODE REQUIRED'                                  
MKTONLY  DC    C'NOT MKT= - CAN NOT FILTER ON OWNER,TVB,RANK'                   
RANKE2   DC    C'RANK MUST BE FROM 1-7'                                         
TYPER    DC    C'REPORT TYPE L MUST BE WHOLE MONTH'                             
INVFMT   DC    C'INVALID FORMAT AT THIS TIME'                                   
MANYDPT  DC    C'MAXIMUM 4 DAYPARTS ALLOWED'                                    
MNYLEN   DC    C'MAXIMUM 4 LENGTHS ALLOWED'                                     
MANYLENP DC    C'FOR REPORT TYPE P, ONLY 1 LENGTH ALLOWED'                      
DATEFMT  DC    C'FORMAT IS MMM/YY'                                              
NOTQTR   DC    C'DATES MUST INCLUDE COMPLETE QUARTERS'                          
COMBOREG DC    C'REGULAR $$ AND -CM STATION INVALID'                            
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,C'RATE ANALYSIS BY'                                         
         PSPEC H2,1,23C'-'                                                      
         PSPEC H1,76,RUN                                                        
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,76,REPORT                                                     
         PSPEC H2,92,REQUESTOR                                                  
         PSPEC H5,1,C'STATION'                                                  
         PSPEC H5,76,C'PERIOD'                                                  
         PSPEC H8,1,C'DAYPART'                                                  
         PSPEC H9,1,C'--------------'                                           
         PSPEC H7,33,C'- - - - UNITS - - - -'                                   
         PSPEC H8,33,C'PRIOR   CURRENT INDEX'                                   
         PSPEC H9,33,C'---------------------'                                   
         PSPEC H7,57,C'- - - - RATE - - - - '                                   
         PSPEC H8,57,C'PRIOR   CURRENT INDEX'                                   
         PSPEC H9,57,C'---------------------'                                   
         PSPEC H7,81,C'- - - - -  BILLING  - - - -'                             
         PSPEC H8,81,C'PRIOR      CURRENT    INDEX'                             
         PSPEC H9,81,C'---------------------------'                             
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*   WORK AREA                                                                   
         SPACE 2                                                                
         DS    0D                                                               
         EJECT                                                                  
PRNTD    DSECT                                                                  
PDPT     DS    CL2                 DAYPART                                      
         DS    CL2                                                              
PDPTN    DS    CL11                DAYPART NAME                                 
         DS    CL2                                                              
PLEN     DS    CL4                 LENGTH                                       
         DS    CL2                                                              
PYR      DS    CL6                 MONTH/YEAR                                   
         DS    CL3                                                              
PURB     DS    0CL1                UNITS/RATE/BILLING                           
         SPACE 2                                                                
PUNDLD   DSECT                                                                  
PUPRI    DS    CL7                 PRIOR UNITS                                  
         DS    CL1                                                              
PUCUR    DS    CL7                 CURRENT UNITS                                
         DS    CL1                                                              
PUIND    DS    CL4                 INDEX UNITS                                  
         DS    CL4                                                              
PRPRI    DS    CL7                 PRIOR RATE                                   
         DS    CL1                                                              
PRCUR    DS    CL7                 CURRENT RATE                                 
         DS    CL1                                                              
PRIND    DS    CL4                 INDEX RATE                                   
         DS    CL4                                                              
PBPRI    DS    CL10                PRIOR BILLING                                
         DS    CL1                                                              
PBCUR    DS    CL10                CURRENT BILLING                              
         DS    CL1                                                              
PBIND    DS    CL4                 INDEX BILLING                                
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         SPACE 2                                                                
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
* RESFMFCD                                                                      
         SPACE 2                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMECD                                                       
         EJECT                                                                  
* REGENSTA                                                                      
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
* REGENATNA                                                                     
       ++INCLUDE REGENAUR                                                       
         EJECT                                                                  
* REGENSDD                                                                      
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
* REGENDPT                                                                      
       ++INCLUDE REGENDPT                                                       
         EJECT                                                                  
* REGENPGT                                                                      
       ++INCLUDE REGENPGT                                                       
         EJECT                                                                  
* RESFMWORKD                                                                    
       ++INCLUDE RESFMWORKD                                                     
         SPACE 2                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*                  ---  WORK AREA ---                                           
         DS    0F                                                               
MYWORK   DS    0CL768                                                           
UNDL     DS    0CL8                                                             
DLACCM   DS    F                   DOLLAR ACCUM                                 
UNACCM   DS    F                   UNIT ACCUM                                   
PRIORSW  DS    CL1                 Y = RECORD IS PRIOR MONTH                    
GROUP    DS    CL2                 GROUP                                        
STATN    DS    CL5                 STATION                                      
FILTER   DS    CL1                 ATHENA RECORD TYPE  (FILTER)                 
*                                  1=ALL (BLANK)                                
*                                  2=SERVICE (S)                                
*                                  3=CATEGORY (C)                               
*                                  4=ADVERTISER (A)                             
*                                                                               
TYPCODE  DS    CL9                 ADV/SERVICE/CATEGORY CODE                    
PSMONTH  DS    CL2                 PRIOR START YEAR/MONTH (BINARY)              
PEMONTH  DS    CL2                 PRIOR END YEAR/MONTH (BINARY)                
SMONTH   DS    CL2                 START MONTH                                  
EMONTH   DS    CL2                 END MONTH                                    
REPTYP   DS    CL1                 REPORT TYPE                                  
*                                   L=LENGTH                                    
*                                   P=PERIOD                                    
*                                   D=DAYPART                                   
*                                                                               
OPT$$$   DS    CL1                 DOLLAR/UNIT TYPE                             
*                                  R  =  REGULAR DOLLARS/UNITS                  
*                                  C  =  COMBO   DOLLARS/UNITS                  
*                                  B  =  BOTH COMBO + REGULAR                   
AGOFFFLG DS    CL1                 AGENCY/AGENCY-OFFICE FLAG                    
*                                  X'0'  -  NO ENTRY                            
*                                  C'C'  -  AGENCY CODE ONLY                    
*                                  C'O'  -  AGENCY-OFFICE CODE                  
TOTPAGE  DS    CL1                 Y  =  TOTAL PAGE ONLY                        
DPT      DS    CL8                 DAYPARTS                                     
         DS    CL1                 ENDING 0                                     
DPTCNT   DS    CL1                 DAYPART COUNTER                              
LEN      DS    CL8                 LENGTHS                                      
         DS    CL2                 ENDING 0                                     
LENCNT   DS    CL1                 LENGTH COUNTER                               
PRNTOPT  DS    CL1                 X'80'  SUPPRESS 2NDARY DAYPART               
*                                  X'40'  STATION TOTALS ONLY                   
*                                  X'20'  LENGTH = TOT                          
*                                  X'10'  LIMITED ACCESS                        
*                                                                               
OWNER    DS    CL3                 OWNER                                        
OWNERN   DS    CL20                OWNER NAME                                   
TVB      DS    CL2                 TVB REGION                                   
TVBN     DS    CL18                TVB REGION NAME                              
RANK     DS    CL1                 RANK                                         
MARKET   DS    CL20                MARKET NAME                                  
SVMKT    DS    CL20                MARKET NAME (SAVED FOR PRINTING)             
GROUPN   DS    CL10                GROUP NAME                                   
         DS    CL1               FOR SPACE BTWN GROUP & SUBGROUP NAME           
SGROUPN  DS    CL10                SUB GROUP NAME                               
MONFMT   DS    CL1                 FORMAT                                       
*                                     M=MONTH                                   
*                                     Q=QUARTER                                 
*                                     A=WHOLE MONTH                             
         SPACE 1                                                                
ASAT     DS    CL2                 COMPRESSED AS AT DATE                        
CURPRG   DS    CL11                CURRENT PROGRAM NAME                         
CURDPTC  DS    CL1                 CURRENT DAYPART CODE                         
CURDPT   DS    CL5                 CURRENT DAYPART NAME                         
CURSDPT  DS    CL5                 CURRENT SECONDARY DAYPART NAME               
FOOTSW   DS    CL1                                                              
MULTSDPT DS    XL1                 X'80'  1 OR MORE SUB DAYPARTS                
*                                  X'40'  2 OR MORE SUB DAYPARTS                
         DS    0F                                                               
TOTACCM  DS    0CL16               TOTAL ACCUMULATOR                            
TOTPUN   DS    F                   PRIOR UNITS                                  
TOTUN    DS    F                   UNITS                                        
TOTPDL   DS    F                   PRIOR DOLLARS                                
TOTDL    DS    F                   DOLLARS                                      
         DS    0F                                                               
SVATNKY  DS    CL34                SAVE KEY                                     
SVKEY    DS    CL34                SAVE KEY                                     
SAVDPT   DS    CL1                 LAST DAYPART FILTER PRINTED                  
SAVSDPT  DS    CL2                 LAST SUB DAYPART/PROG TYPE PRINTED           
SAVLEN   DS    CL2                 LAST LENGTH PRINTED                          
SVADPTF  DS    A                   A(LAST DAYPART FILTER READ)                  
SVALENF  DS    A                   A(LAST LENGTH FILTER READ)                   
SVFOOTF  DS    A                   A(NEXT POSITION IN FOOTLINE)                 
SAVERE   DS    F                                                                
SAVEE    DS    F                                                                
TOTPRNT  DS    CL1                 Y=SOMETHING TO PRINT FOR LEVEL               
SVFOOT   DS    CL132                                                            
         SPACE 2                                                                
         DS    0F                                                               
BUFREC   DS    0CL56                                                            
BUFKEY   DS    0CL16                                                            
BUFTYP   DS    CL1                                                              
BUFSTA   DS    CL5                                                              
BUFDPT   DS    CL1                                                              
BUFSDPT  DS    CL2                                                              
BUFLEN   DS    CL2                                                              
BUFYR    DS    CL1                                                              
BUFMON   DS    CL1                                                              
         DS    CL3                 NOT DEFINED                                  
BUFDPTN  DS    CL15                                                             
BUFSTA2  DS    CL5                                                              
BUFTOTAL DS    CL4                                                              
         DS    0F                                                               
BUFACCM  DS    0CL16                                                            
BUFPUN   DS    F                                                                
BUFUN    DS    F                                                                
BUFPDL   DS    F                                                                
BUFDL    DS    F                                                                
*                                                                               
BUFREC2  DS    CL(*-BUFREC)                                                     
*                                                                               
BUFREC3  DS    CL(*-BUFREC)                                                     
*                                                                               
STATLIST DS    24CL7               24 GROUP/STATION ENTRIES MAX                 
STADELIM DS    XL1                 DELIMITER                                    
COMBOFLG DS    CL1                                                              
SECVIOL  DS    CL1                 SECURITY VIOLATION FLAG                      
SAVMKTCD DS    CL4                 MARKET CODE FROM X'2B' RECORD                
ASTATN   DS    A                   A(STATION IN PROGRESS)                       
STATCTR  DS    F                   STATION COUNTER                              
STATPROG DS    F                   STATION IN PROGRESS                          
ABFTABLE DS    A                   A(BUFFER PRINT AREA)                         
*                                                                               
REPSTBL  DS    CL(30*2)                                                         
*                                                                               
AREPE    EQU   RAURKREP-RAURKEY    REP                                          
AGRPE    EQU   RAURKGRP-RAURKEY    GROUP                                        
ASTAE    EQU   RAURKSTA-RAURKEY    STATION                                      
ATPEE    EQU   RAURKTPE-RAURKEY    RECORD TYPE (FILTER)                         
ATCPE    EQU   RAURKTCD-RAURKEY    TYPE CODE                                    
AAGYE    EQU   RAURKAGY-RAURKEY    AGENCY                                       
ACTYPE   EQU   RAURKCTP-RAURKEY    CONTRACT TYPE                                
AOFFE    EQU   RAURKOFF-RAURKEY    OFFICE                                       
ADPTE    EQU   RAURKDPT-RAURKEY    DAYPART                                      
ASDTE    EQU   RAURKSDT-RAURKEY    SUB DAYPART                                  
APRGE    EQU   RAURKPRG-RAURKEY    PROGRAM CODE                                 
ASLNE    EQU   RAURKSLN-RAURKEY    SPOT LENGTH                                  
AYME     EQU   RAURKYM-RAURKEY     YEAR/MONTH                                   
*                                                                               
         EJECT                                                                  
T81815   CSECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*TVB TABLE                                                                      
       ++INCLUDE RETVBTAB                                                       
REPDSECT DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'095RESFM15B  05/01/02'                                      
         END                                                                    
