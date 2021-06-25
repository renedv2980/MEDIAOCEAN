*          DATA SET RESFM11    AT LEVEL 096 AS OF 05/01/02                      
*          DATA SET RESFM11    AT LEVEL 057 AS OF 07/16/93                      
*PHASE T81811A,*                                                                
         TITLE 'T81811 - RESFM11 - A.U.R. REPORT'                               
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESFM11 (T81811) --- A.U.R. REPORT                       *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* JUN09/93 (BU ) --- ORIGINAL ENTRY                               *             
*                                                                 *             
* JUL15/93 (BU ) --- EXPAND AGENCY FILTER TO AGENCY/OFFICE        *             
*                                                                 *             
* SEP16/99 (BU ) --- DISPLAY TABLE OVERFLOW MESSAGE RATHER THAN   *             
*                    ABORT                                        *             
*                                                                 *             
*                                                                 *             
*                    **  END TOMBSTONE  **                        *             
*******************************************************************             
*                                                                               
T81811   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1811**,R7,RR=R2                                              
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
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE FIELDS                                                  
         SPACE 2                                                                
VKEY     DS    0H                                                               
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
*                              ********************************                 
*                              *        VALIDATE              *                 
*                              *          GROUP               *                 
*                              ********************************                 
VK10     LA    R2,AURGRPH                                                       
         CLI   5(R2),0                                                          
         BNE   VK20                                                             
         LA    R2,AURSTAH                                                       
         CLI   5(R2),0                                                          
         BNE   VK50                                                             
         SPACE 1                                                                
VKGRPER  MVC   CONHEAD+10(L'GRPORSTA),GRPORSTA                                  
         B     MYEND               GROUP OR STATION IS REQUIRED                 
         SPACE 1                                                                
VK20     MVI   ERROR,SECLOCK                                                    
         TM    PRNTOPT,X'10'       LIMITED TO SINGLE STATION                    
         BO    ERREND                                                           
         GOTO1 VALIGRP                                                          
         MVC   GROUP,WORK                                                       
         MVC   GROUPN(21),SPACES                                                
         MVC   GROUPN(10),WORK+10  GROUP NAME                                   
         MVC   SGROUPN(10),WORK+20 SUB GROUP NAME                               
         LA    R1,GROUPN+9                                                      
         LA    RE,9                                                             
VK30     CLI   0(R1),C' '                                                       
         BNE   VK40                                                             
         BCTR  R1,0                                                             
         BCT   RE,VK30                                                          
VK40     MVC   2(10,R1),SGROUPN    SUB GROUP NAME                               
         OC    GROUPN(21),SPACES                                                
         LA    R2,AURSTAH                                                       
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         LA    R2,AURGRPH          POINT AT GROUP FIELD FOR ERROR               
         B     VKGRPER                                                          
         SPACE 1                                                                
*                              ********************************                 
*                              *        VALIDATE              *                 
*                              *         STATION              *                 
*                              ********************************                 
VK50     GOTO1 VALISTA                                                          
         CLI   WORK+4,C'C'         COMBO STATION REQUESTED?                     
         BNE   VK52                NO                                           
         MVC   CONHEAD+13(36),=C'**  MUST REQUEST AURCOMBO REPORT  **'          
         B     MYEND                                                            
VK52     EQU   *                                                                
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
         EJECT                                                                  
*                                                                               
*                              ********************************                 
*                              *        VALIDATE              *                 
*                              *         PERIOD   (MMMYY)     *                 
*                              ********************************                 
VK60     LA    R2,AURPERH                                                       
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
         LA    R2,AURTYPH                                                       
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
VK150    LA    R2,AURDAYH                                                       
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
         XC    KEY,KEY             DAYPART RECORD                               
         MVI   KEY,X'24'                                                        
         MVC   KEY+RDPTKREP-RDPTKEY(2),AGENCY                                   
         MVC   KEY+RDPTKDPT-RDPTKEY(1),0(R3)                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VKDPTER                                                          
         CLI   1(R3),0             IF PRIMARY DPT USE ALL 2NDARY DPTS           
         BE    VK320                                                            
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    VK280                                                            
         B     VKDPTER                                                          
*                                                                               
VK260    BAS   RE,NEXTEL                                                        
         BE    VK280                                                            
*                                                                               
VKDPTER  MVC   CONHEAD+13(21),=C'IS AN INVALID DAYPART'                         
         MVC   CONHEAD+10(2),0(R3)                                              
         B     MYEND                                                            
*                                                                               
VK280    CLC   1(1,R3),2(R6)                                                    
         BNE   VK260                                                            
         B     VK320                                                            
*                                                                               
VK300    MVI   0(R3),X'FE'         INDICATE PROGRAM TYPE FOR SORT               
         XC    KEY,KEY                                                          
         MVI   KEY,X'25'                                                        
         MVC   KEY+RPGTKREP-RPGTKEY(2),AGENCY                                   
         MVC   KEY+RPGTKPGT-RPGTKEY(1),1(R3)                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    VK320                                                            
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
VK360    LA    R2,AURLENH                                                       
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
*                 *      NOT VALID WHEN SINGLE STATION USED    *                
*                 **********************************************                
VK490    LA    R2,AUROWNH                                                       
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BE    VK560                                                            
         OC    STATN,STATN                                                      
         BZ    VK500                                                            
VK1STAER MVC   CONHEAD+10(L'ONESTAER),ONESTAER                                  
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
*                 *      NOT VALID WHEN SINGLE STATION USED    *                
*                 **********************************************                
VK560    LA    R2,AURTVBH                                                       
         CLI   5(R2),0                                                          
         BE    VK590                                                            
         OC    STATN,STATN                                                      
         BNZ   VK1STAER                                                         
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
*                 *      NOT VALID WHEN SINGLE STATION USED    *                
*                 **********************************************                
VK590    LA    R2,AURRNKH                                                       
         CLI   5(R2),0                                                          
         BE    VK610                                                            
         OC    STATN,STATN                                                      
         BNZ   VK1STAER                                                         
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
VK610    LA    R2,AURASATH                                                      
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
VK620    LA    R2,AURFMTH                                                       
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
VK650    LA    R2,AURFLTH                                                       
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
         GOTO1 =A(SETAGY),DMCB,(RC),(R4),RR=Y                                   
*                                  DEVELOP AGENCY OR AGENCY/OFFICE              
         BNZ   VK730               ERROR                                        
         MVC   KEY+25(2),AGENCY    INSERT REP CODE                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    VK780               YES                                          
VK730    EQU   *                                                                
         MVC   CONHEAD+10(18),=C'INVALID AGENCY    '                            
         B     MYEND                                                            
         SPACE 1                                                                
VK780    MVI   FILTER,3                                                         
         MVC   TYPCODE,SPACES      SPACE FILL AREA                              
         MVC   TYPCODE(6),KEY+19   FILL IN AGENCY/AGY OFF                       
         CLC   KEY+23(2),SPACES    CORPORATE AGENCY REQUESTED?                  
         BNE   VK800               NO  - SPECIFIC OFFICE                        
VK790    EQU   *                   CHECK IF OFFICES EXIST                       
         GOTO1 SEQ READ NEXT RECORD                                             
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
VK900    LA    R2,AURSDPTH                                                      
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
VK950    LA    R2,AURSTOTH                                                      
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
*                  *    *  VALIDATE $$ RETRIEVAL OPTION        *                
*                  *C=COMBO $$, B=COMBO+ REGULAR, DEFAULT = REG*                
*                  *********************************************                
VK970    EQU   *                                                                
         MVI   OPT$$$,0            SET TO 'REGULAR $$ ONLY'                     
         LA    R2,AURSDOLH                                                      
         CLI   5(R2),0                                                          
         BE    VKEXT                                                            
         CLI   8(R2),C'C'          COMBO $$?                                    
         BE    VK980               YES                                          
         CLI   8(R2),C'B'          BOTH COMBO+REGULAR $$?                       
         BNE   ERREND              NO  - UNRECOGNIZED                           
         SPACE 1                                                                
VK980    EQU   *                                                                
         MVC   OPT$$$,8(R2)        SAVE OPTIONAL VALUE                          
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
*              PRINT REPORT                                                     
         SPACE 3                                                                
PREP     EQU   *                                                                
         SPACE                                                                  
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,FHOOK                                                         
         ST    R1,FOOTHOOK                                                      
         MVI   FOOTLNS,1                                                        
         MVC   FOOTSW,FOOTLNS                                                   
*                                                                               
         LA    R2,BUFF             CLEAR OUT PRINT BUFFER                       
         LA    R3,48                                                            
*                                                                               
PR10     EQU   *                                                                
*                                                                               
*   132*48 EXCEEDS THE SIZE OF THE BUFFER, WHICH IS 6144.                       
*   128*48 = 6144.  ADJUSTMENT MADE.  BUFFER IS INADEQUATE                      
*        IN ANY CASE, AS OVERFLOW CAUSES ABORT./                                
*                                                                               
****     MVC   0(132,R2),SPACES                                                 
****     LA    R2,132(R2)                                                       
         MVC   0(128,R2),SPACES                                                 
         LA    R2,128(R2)                                                       
         BCT   R3,PR10                                                          
         SPACE 1                                                                
         GOTO1 BUFFALO,DMCB,=C'SET',(0,ADBUFC)                                  
         SPACE                                                                  
         XC    SVKEY,SVKEY                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'2C'                                                        
         MVC   KEY+AREPE(2),AGENCY     INSERT REP CODE                          
         MVC   KEY+AGRPE(2),GROUP  INSERT GROUP                                 
         OC    STATN,STATN         ANY STATION?                                 
         BNZ   PR70                YES                                          
*                                                                               
* THIS ROUTINE USED WHEN NO STATION IS FILTERED ON                              
*                                                                               
PR20     EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLI   GROUP+1,C' '        IS SUBGROUP REQUESTED?                       
         BNE   PR25                YES                                          
         CLC   KEY(7),KEYSAVE      NO  - ID/REP/GROUP 1 POS ONLY                
         BNE   PR500                                                            
         B     PR27                                                             
PR25     EQU   *                                                                
         CLC   KEY(8),KEYSAVE      YES - ID/REP/GROUP 2 POS                     
         BNE   PR500                                                            
*                                                                               
PR27     EQU   *                                                                
         CLC   KEY+ASTAE(5),KEYSAVE+ASTAE                                       
         BE    PR30                                                             
*                                                                               
*  READ STATION TO GET MARKET NAME AND IF NECESSARY,                            
*                    OWNER, TVB, AND RANK FILTER CHECK                          
*                                                                               
         BAS   RE,READSTA                                                       
         LTR   R3,R3               DOES STATION MEET FILTER CHECK               
         BZ    PR60                NO                                           
*                                                                               
PR30     CLC   KEY+ATPEE(07),FILTER    CORRECT FILTER AND CODE                  
         BE    PR40                    YES                                      
         BH    PR60                    MOVE TO NEXT STATION                     
         MVC   KEY+ATPEE(07),FILTER    MAYBE WE CAN FIND IT                     
         XC    KEY+ADPTE(7),KEY+ADPTE     A BIT FURTHER                         
         B     PR20                                                             
*                                                                               
PR40     CLI   DPTCNT,0                                                         
         BE    PR50                                                             
         XC    KEY+ADPTE(7),KEY+ADPTE                                           
         LA    R3,DPT              YES, RESET TO START OF FILTERS               
         ST    R3,SVADPTF                                                       
*                                                                               
PR50     CLI   LENCNT,0                                                         
         BE    PR90                                                             
         XC    KEY+ASLNE(4),KEY+ASLNE                                           
         LA    R3,LEN                                                           
         ST    R3,SVALENF                                                       
         B     PR90                                                             
*                                                                               
         SPACE 1                                                                
PR60     ZIC   RF,KEY+ASTAE+4      MOVE ON TO NEXT STATION                      
         LA    RF,1(RF)                                                         
         STC   RF,KEY+ASTAE+4                                                   
         XC    KEY+ATPEE(17),KEY+ATPEE                                          
         B     PR20                                                             
*                                                                               
PR70     MVC   KEY+ASTAE(15),STATN     STA,REC TYPE,CONTYP/OFF/AGY              
*                                                                               
PR90     CLI   DPTCNT,0            DAYPART FILTER                               
         BE    PR130                                                            
         LA    R3,DPT                                                           
         ST    R3,SVADPTF                                                       
PR100    CLI   0(R3),X'FE'         PROGRAM TYPE?                                
         BE    *+14                                                             
         MVC   KEY+ADPTE(2),0(R3)   1ST DAYPART - R3 ALSO SET BELOW             
         B     PR110                                                            
*                                                                               
         MVC   KEY+ADPTE(2),=X'FEFE'                                            
         MVC   KEY+APRGE(1),1(R3)                                               
*                                                                               
PR110    CLI   LENCNT,0            LENGTH FILTER                                
         BE    PR130                                                            
         LA    R3,LEN                                                           
         ST    R3,SVALENF                                                       
PR120    MVC   KEY+ASLNE(2),0(R3)      1ST LENGTH                               
         MVC   KEY+AYME(2),PSMONTH                                              
*                                                                               
PR130    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     PR150                                                            
*                                                                               
PR140    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
PR150    EQU   *                                                                
         CLI   GROUP+1,C' '        IS SUBGROUP REQUESTED?                       
         BNE   PR155               YES                                          
         CLC   KEY(7),KEYSAVE      NO  - ID/REP/GROUP 1 POS ONLY                
         BNE   PR500                                                            
         B     PR157                                                            
PR155    CLC   KEY(8),KEYSAVE     ID/REP/GROUP                                  
         BNE   PR500                                                            
PR157    EQU   *                                                                
*                                                                               
         OC    STATN,STATN                                                      
         BNZ   PR160                                                            
         CLI   AGOFFFLG,X'0'       ANY AGENCY OFFICE CODE FLAG?                 
         BE    PR157A              NO  -  TEST 12 CHARS....                     
         CLI   AGOFFFLG,C'O'       ANY AGENCY OFFICE CODE?                      
         BNE   PR157B              NO  -  TEST 10 CHARS....                     
PR157A   EQU   *                                                                
         CLC   KEY+ASTAE(12),KEYSAVE+ASTAE   SAME STATION/FILTER/CODE           
         BNE   PR60                                                             
         B     PR170                                                            
PR157B   EQU   *                                                                
         CLC   KEY+ASTAE(10),KEYSAVE+ASTAE   SAME STATION/FILTER/CODE           
         BNE   PR60                                                             
         B     PR170                                                            
*                                                                               
PR160    EQU   *                                                                
         CLI   AGOFFFLG,X'0'       ANY AGENCY OFFICE CODE FLAG?                 
         BE    PR160A              NO  -  TEST 12 CHARS....                     
         CLI   AGOFFFLG,C'O'       ANY AGENCY OFFICE CODE?                      
         BNE   PR160B              NO  -  TEST 10 CHARS....                     
PR160A   EQU   *                                                                
         CLC   KEY+ASTAE(12),STATN     CORRECT STATION/FILTER/CODE              
         BNE   PR500                                                            
         B     PR170                                                            
PR160B   EQU   *                                                                
         CLC   KEY+ASTAE(10),STATN     CORRECT STATION/FILTER/CODE              
         BNE   PR500                                                            
         SPACE 1                                                                
* FILTER RECORDS                                                                
PR170    CLI   DPTCNT,0            DAYPART FILTER                               
         BE    PR240                                                            
         SPACE 1                                                                
         L     R3,SVADPTF                                                       
         CLI   0(R3),X'FE'         PROGRAM TYPE                                 
         BE    PR210                                                            
         CLC   KEY+ADPTE(2),0(R3)                                               
         BE    PR240                                                            
         CLI   1(R3),0             IF BLANK PASS ALL 2NDARY DPT                 
         BNE   PR180                                                            
         CLC   KEY+ADPTE(1),0(R3)                                               
         BE    PR240                                                            
         SPACE 1                                                                
PR180    LA    R3,2(R3)            NEXT ENTRY - CHECK IF IT = TO KEY            
         CLI   0(R3),0                                                          
         BNE   PR190                                                            
         OC    STATN,STATN         FINISHED DAYPART FILTERS                     
         BZ    PR60                READ NEXT STATION                            
         B     PR500               DONE                                         
*                                                                               
PR190    CLI   0(R3),X'FE'         PROGRAM TYPE                                 
         BE    PR230                                                            
         ST    R3,SVADPTF                                                       
         CLI   1(R3),0             IF BLANK PASS ALL 2NDARY DPT                 
         BNE   PR200                                                            
         CLC   KEY+ADPTE(1),0(R3)                                               
         BH    PR180               KEY GT NEXT FILTER                           
         BE    PR240                                                            
PR200    CLC   KEY+ADPTE(2),0(R3)                                               
         BH    PR180               KEY GT NEXT FILTER                           
         BE    PR240                                                            
         XC    KEY+APRGE(5),KEY+APRGE     CLEAR TO END OF KEY                   
         B     PR100                                                            
*                                                                               
PR210    CLC   KEY+APRGE(1),1(R3)  PROGRAM TYPE                                 
         BE    PR240                                                            
         SPACE 1                                                                
PR220    LA    R3,2(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   PR230               FINISH DAYPART FILTERS                       
         OC    STATN,STATN         FINISHED DAYPART FILTERS                     
         BZ    PR60                READ NEXT STATION                            
         B     PR500               DONE                                         
*                                                                               
PR230    ST    R3,SVADPTF                                                       
         CLC   KEY+APRGE(1),1(R3)                                               
         BH    PR220               KEY GT NEXT FILTER                           
         BE    PR240                                                            
         XC    KEY+ASLNE(4),KEY+ASLNE       CLEAR TO END OF KEY                 
         B     PR100                                                            
*                                                                               
PR240    CLI   LENCNT,0            LENGTH FILTER                                
         BE    PR270                                                            
         CLC   KEY+ADPTE(3),SVKEY+ADPTE                                         
         BE    *+12                                                             
         LA    R3,LEN              NEW DAYPART-RESTART LENGTH TABLE             
         ST    R3,SVALENF                                                       
         L     R3,SVALENF                                                       
         CLC   KEY+ASLNE(2),0(R3)                                               
         BE    PR270                                                            
         BL    PR120                                                            
PR250    LA    R3,2(R3)                                                         
         OC    0(2,R3),0(R3)                                                    
         BNZ   PR260               FINISH LENGTH FILTERS                        
         ZIC   RF,KEY+APRGE                                                     
         LA    RF,1(RF)                                                         
         STC   RF,KEY+APRGE        BUMP DAYPART                                 
         XC    KEY+ASLNE(4),KEY+ASLNE       CLEAR TO END OF KEY                 
         B     PR130                                                            
*                                                                               
PR260    ST    R3,SVALENF                                                       
         CLC   KEY+ASLNE(2),0(R3)                                               
         BH    PR250               KEY GT FILTER NEXT FILTER                    
         BL    PR120                                                            
*                                                                               
PR270    CLC   KEY+AYME(2),PSMONTH                                              
         BNL   *+14                                                             
         MVC   KEY+AYME(2),PSMONTH                                              
         B     PR130                                                            
*                                                                               
         MVI   PRIORSW,C'Y'                                                     
         CLC   KEY+AYME(2),PEMONTH                                              
         BNH   PR300                                                            
*                                                                               
         MVI   PRIORSW,C'N'                                                     
         CLC   KEY+AYME(2),SMONTH                                               
         BNL   *+14                                                             
         MVC   KEY+AYME(2),SMONTH                                               
         B     PR130                                                            
*                                                                               
         CLC   KEY+AYME(2),EMONTH                                               
         BNH   PR300                                                            
         MVI   KEY+AYME,X'FF'      BUMP YEAR - CAN'T BUMP LENGTH                
         B     PR130               IF PLAN, IT WOULD ALREADY BE X'FF'           
         EJECT                                                                  
*                                                                               
* GET RECORD - BUILD AND PUT TO BUFFALO.  AT LEVEL BREAKS, READ,PRINT,          
*               AND CLEAR.                                                      
         SPACE 2                                                                
PR300    CLI   REPTYP,C'L'         LENGTH FORMAT                                
         BE    PR310                                                            
         CLI   REPTYP,C'P'         PERIOD FORMAT                                
         BE    PR310                                                            
         MVC   CONHEAD(L'INVFMT),INVFMT                                         
         B     MYEND                                                            
         SPACE 1                                                                
PR310    OC    SVKEY,SVKEY                                                      
         BNZ   PR315               FIRST TIME                                   
         MVC   SVMKT,MARKET                                                     
         B     PR380                                                            
         SPACE 1                                                                
PR315    CLC   SVKEY(13),KEY        STATION CHANGED                             
         BE    PR320                                                            
         BAS   RE,SUM1                                                          
         BAS   RE,SUM2                                                          
         BAS   RE,SUM3                                                          
         MVI   FORCEHED,C'Y'       NEW PAGE PER STATION                         
         MVI   CURDPTC,C' '                                                     
         MVC   CURDPT,SPACES                                                    
         MVC   CURPRG,SPACES                                                    
         MVC   SVMKT,MARKET                                                     
         B     PR380                                                            
*                                                                               
PR320    EQU   *                                                                
         CLI   AGOFFFLG,X'0'       ANY AGENCY CODE ENTERED?                     
         BE    PR325               NO  - CHECK ALL CHARACTERS                   
         CLI   AGOFFFLG,C'O'       ANY AGENCY OFFICE CODE?                      
         BE    PR325               YES - CHECK TYPE/AGY/AGY OFFICE              
         CLC   SVKEY+13(5),KEY+13  NO  - CHECK TYPE/AGENCY                      
         BNE   PR330                                                            
         CLC   SVKEY+20(1),KEY+20  CHECK DYPART                                 
         BNE   PR330                                                            
         B     PR340                                                            
PR325    EQU   *                                                                
         CLC   SVKEY(21),KEY       DPT CHANGED                                  
         BE    PR340                                                            
PR330    EQU   *                                                                
         BAS   RE,SUM1                                                          
         BAS   RE,SUM2                                                          
         B     PR380                                                            
*                                                                               
PR340    EQU   *                                                                
         CLC   SVKEY+21(1),KEY+21  SUB DPT CHANGED                              
****     BE    PR350                                                            
         B     PR380                                                            
*                                                                               
*   TEST PR340                                                                  
*        MVC   P+1(05),=C'PR340'                                                
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*   TEST END                                                                    
*                                                                               
         BAS   RE,SUM1                                                          
         B     PR380                                                            
*                                                                               
PR350    CLC   SVKEY+22(1),KEY+22  PROGRAM TYPE CHANGED                         
         BE    PR400                                                            
         BAS   RE,SUM2                                                          
*                                                                               
PR380    BAS   RE,READDPT          GET NEW DPT/SUB DPT/PROG TYPE NAME           
         B     PR400                                                            
*                                                                               
*                                                                               
*         LEVEL 3 IS FOR STATION/LENGTH TOTALS                                  
*                                                                               
*                                                                               
PR400    XC    BUFREC,BUFREC                                                    
         MVI   BUFTYP,C'3'         NOW BUILD NEW BUF RECORDS                    
         MVC   BUFSTA,KEY+ASTAE                                                 
         MVC   BUFLEN,KEY+ASLNE    SPOT LENGTH                                  
         CLC   KEY+ADPTE(3),=X'FEFEFC' PLAN                                     
         BNE   *+10                                                             
         MVC   BUFLEN,=X'FEFE'     INDICATE PLAN LENGTH                         
*                                                                               
         CLI   REPTYP,C'P'         PERIOD FORMAT                                
         BNE   PR410                                                            
         TM    PRNTOPT,X'20'       LENGTH=TOT                                   
         BZ    *+10                                                             
         MVC   BUFLEN(2),=X'FFFF'  COMPRESS TO 1 LENGTH                         
         CLI   MONFMT,C'A'         ALL MONTH                                    
         BE    *+10                                                             
         MVC   BUFYR(2),KEY+AYME   YEAR/MONTH                                   
*                                                                               
         CLI   MONFMT,C'Q'         QUARTER FORMAT                               
         BNE   PR410                                                            
         BAS   RE,GETQTR                                                        
         OI    BUFYR,X'F0'         INDICATE QUARTERS                            
         STC   R1,BUFMON                                                        
*                                                                               
PR410    BAS   RE,GETUNDL                                                       
         CLI   PRIORSW,C'Y'                                                     
         BE    PR420                                                            
         MVC   BUFUN,UNACCM                                                     
         MVC   BUFDL,DLACCM                                                     
         B     PR430                                                            
*                                                                               
PR420    MVC   BUFPUN,UNACCM                                                    
         MVC   BUFPDL,DLACCM                                                    
         CLI   MONFMT,C'A'         ALLMONTH                                     
         BE    PR430                                                            
         ZIC   RE,BUFYR            ADJUST PRIOR YEAR TO MATCH CURRENT           
         AH    RE,=H'1'                                                         
         STC   RE,BUFYR                                                         
PR430    GOTO1 BUFFALO,DMCB,=C'PUT',(0,ADBUFC),BUFREC                           
         SPACE 2                                                                
*                                                                               
*         LEVEL 2 IS FOR STATION/PRIMARY DAYPART/LENGTH TOTALS                  
*                                                                               
         MVI   BUFTYP,C'2'                                                      
*                                                                               
         TM    PRNTOPT,X'40'       IF STATION TOTALS ONLY                       
         BO    PR480               ONLY DO LEVEL 3                              
*                                                                               
         CLI   KEY+ADPTE,X'FF'      PSEUDO DAYPART                              
         BNE   PR440                                                            
         MVC   BUFDPT(2),=X'FFFF'                                               
         MVC   BUFDPTN(6),=C'PSEUDO'                                            
         B     PR460                                                            
*                                                                               
PR440    CLI   KEY+ADPTE,X'FE'      PROGRAM TYPE OR PLAN                        
         BNE   PR450                                                            
         MVC   BUFDPT(2),=X'FEFE'                                               
         MVC   BUFDPT+2(1),KEY+APRGE                                            
         CLI   KEY+APRGE,X'FC'     PLAN                                         
         BNE   PR445                                                            
         MVC   BUFDPTN(4),=C'PLAN'                                              
         B     PR460                                                            
PR445    MVC   BUFDPTN,CURPRG      PROGRAM TYPE NAME                            
         B     PR460                                                            
*                                                                               
PR450    MVC   BUFDPT(1),KEY+ADPTE   PRIMARY DAYPART                            
         MVC   BUFDPTN(5),CURDPT                                                
*                                                                               
PR460    EQU   *                                                                
*                                                                               
*   TEST PRINT BUFREC                                                           
*        MVC   P+1(06),=C'BUFREC'                                               
*        MVC   P+10(72),BUFREC                                                  
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*   TEST END                                                                    
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',(0,ADBUFC),BUFREC                           
         SPACE 2                                                                
*                                                                               
*  LEVEL 1 IS FOR STATION/SUB DAYPART/LENGTH TOTALS                             
*                                                                               
         MVI   BUFTYP,C'1'                                                      
*                                                                               
         CLI   KEY+ADPTE,X'FE'     IF PSEUDO OR PROG. TYPE OR PLAN              
         BNL   PR480                                                            
         TM    PRNTOPT,X'80'      OR SUPRESS SUB DAYPART                        
         BO    PR480                                                            
         CLC   CURSDPT,=C'XXXXX'   OR NO SUB DAYPART                            
         BE    PR480               THEN SKIP LEVEL 1                            
*                                                                               
         LA    R0,5                SEPARATE NAMES WITH A /                      
         LA    RE,BUFDPTN+4                                                     
PR470    CLI   0(RE),X'40'                                                      
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R0,PR470                                                         
         MVI   1(RE),C'/'                                                       
         MVC   2(5,RE),CURSDPT                                                  
*                                                                               
         MVC   BUFSDPT(1),KEY+ASDTE    SUB DAYPART                              
         GOTO1 BUFFALO,DMCB,=C'PUT',(0,ADBUFC),BUFREC                           
*                                                                               
*   TEST BUFREC PRINT                                                           
*        MVC   P+1(03),=C'PUT'                                                  
*        MVC   P+10(44),BUFREC                                                  
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*   TEST BUFREC PRINT END                                                       
*                                                                               
*                                                                               
PR480    MVC   SVKEY,KEY                                                        
         B     PR140                                                            
*                                                                               
PR500    BAS   RE,SUM1                                                          
         BAS   RE,SUM2                                                          
         BAS   RE,SUM3                                                          
         SPACE 2                                                                
         MVI   FORCEFUT,C'Y'       FORCE FOOT LINES TO PRINT                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*  ROUTINES TO INDICATE CORRECT LEVEL TO PRINT                                  
         SPACE 2                                                                
SUM1     ST    RE,SAVEE                                                         
         MVI   BUFTYP,C'1'                                                      
         BAS   RE,PRNTSUM                                                       
         L     RE,SAVEE                                                         
         BR    RE                                                               
*                                                                               
SUM2     ST    RE,SAVEE                                                         
         MVI   BUFTYP,C'2'                                                      
         BAS   RE,PRNTSUM                                                       
         L     RE,SAVEE                                                         
         BR    RE                                                               
*                                                                               
SUM3     ST    RE,SAVEE                                                         
         MVI   BUFTYP,C'3'                                                      
         BAS   RE,PRNTSUM                                                       
         L     RE,SAVEE                                                         
         BR    RE                                                               
         EJECT                                                                  
*  ROUTINE TO READ FROM BUFFALO AND PRINT OUT DATA                              
         SPACE 2                                                                
PRNTSUM  NTR1                                                                   
*                                                                               
*   TEST PRNTSUM                                                                
*        MVC   P+1(07),=C'PRNTSUM'                                              
*        MVC   P+20(44),BUFREC                                                  
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*   TEST PRNTSUM END                                                            
*                                                                               
         XC    TOTACCM,TOTACCM     CLEAR OUT ACCUMULATORS                       
         XC    BUFREC+1(L'BUFREC-1),BUFREC+1                                    
         XC    SAVLEN,SAVLEN                                                    
*                                                                               
         LA    R2,BUFF                                                          
         USING PRNTD,R2                                                         
         SR    R3,R3               LINE COUNTER                                 
*                                                                               
         TM    PRNTOPT,X'40'       STATION TOTALS ONLY                          
         BZ    *+8                                                              
         MVI   SAVDPT,X'FF'        FOR 1ST TIME PRINT                           
*                                                                               
         CLI   BUFTYP,C'2'                                                      
         BNE   PS50                                                             
         TM    MULTSDPT,X'C0'      1 OR MORE SUB DAYPARTS                       
         BZ    PS50                NO LEVEL 1                                   
         TM    MULTSDPT,X'40'      2 OR MORE SUB DAYPARTS                       
         BZ    PS470               NO, SO DON'T PRINT MAIN DAYPART              
*                                                                               
PS50     EQU   *                                                                
*        MVC   P+1(05),=C'PS50='  **TEST                                        
*        L     R3,ADBUFC                                                        
*        MVC   P+15(117),0(R3)      **TEST                                      
*        GOTO1 SPOOL,PARAS,(R8)    **TEST                                       
         GOTO1 BUFFALO,DMCB,=C'HIGH',(BUFTYP,ADBUFC),BUFREC,1                   
         TM    DMCB+8,X'80'                                                     
         BO    RXEXT               NO RECORDS FOUND                             
*                                                                               
         CLI   BUFTYP,C'1'                                                      
         BNE   PS150                                                            
         TM    MULTSDPT,X'80'      1 SUB DAYPART                                
         BZ    *+8                                                              
         OI    MULTSDPT,X'40'      2 OR MORE SUB DAYPARTS                       
         OI    MULTSDPT,X'80'                                                   
         B     PS150                                                            
*                                                                               
PS100    GOTO1 BUFFALO,DMCB,=C'SEQ',(BUFTYP,ADBUFC),BUFREC,1                    
         TM    DMCB+8,X'80'                                                     
         BO    PS450               NO MORE RECORDS FOUND                        
*        MVC   P+1(12),=C'PS100RETURN='  ** TEST                                
*        MVC   P+15(L'BUFREC),BUFREC     ** TEST                                
*        GOTO1 SPOOL,PARAS,(R8)    **TEST                                       
         MVI   TOTPRNT,C'Y'        NEED TO PRINT TOTALS FOR THIS LEVEL          
*                                                                               
PS150    EQU   *                                                                
*                                                                               
*   TEST BUFREC PRINT                                                           
*        MVC   P+1(07),=C'BUFREC2'                                              
*        MVC   P+10(44),BUFREC                                                  
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*   TEST BUFREC PRINT END                                                       
*                                                                               
         SR    RE,RE               KEEP ACCUMS FOR 1 LINE TOTAL                 
         ICM   RE,15,TOTUN                                                      
         A     RE,BUFUN                                                         
         STCM  RE,15,TOTUN                                                      
*                                                                               
         ICM   RE,15,TOTPUN                                                     
         A     RE,BUFPUN                                                        
         STCM  RE,15,TOTPUN                                                     
*                                                                               
         ICM   RE,15,TOTDL                                                      
         A     RE,BUFDL                                                         
         STCM  RE,15,TOTDL                                                      
*                                                                               
         ICM   RE,15,TOTPDL                                                     
         A     RE,BUFPDL                                                        
         STCM  RE,15,TOTPDL                                                     
*                                                                               
         CLI   BUFTYP,C'2'         LEVEL 1 AND LEVEL 2                          
         BH    PS250                                                            
*                                                                               
         CLC   SAVDPT(3),BUFDPT       ONLY PRINT DPT NAME ONCE                  
         BE    PS300                                                            
         MVC   SAVDPT(3),BUFDPT                                                 
         MVC   PDPTN,BUFDPTN                                                    
         XC    SAVLEN,SAVLEN                                                    
         CLI   BUFDPT,X'FF'        IF PSEUDO OR                                 
         BE    PS300                                                            
         CLC   BUFDPT(3),=X'FEFEFC'    PLAN, DON'T PRINT DAYPART CODE           
         BE    PS300                                                            
         CLI   BUFDPT,X'FE'        IF PROG TYPE, DON'T PRINT *                  
         BNE   PS200                                                            
         MVC   PDPT(1),BUFSDPT+1                                                
         B     PS300                                                            
PS200    MVC   PDPT(2),BUFDPT      DAYPART CODE                                 
         B     PS300                                                            
         SPACE 1                                                                
PS250    CLI   BUFTYP,C'3'         LEVEL 3                                      
         BNE   PS100                                                            
         CLI   SAVDPT,0                                                         
         BE    *+10                                                             
         MVC   PDPT(13),=C'STATION TOTAL'                                       
         XC    SAVDPT(3),SAVDPT                                                 
         SPACE 1                                                                
PS300    CLC   SAVLEN(2),BUFLEN    ONLY PRINT LENGTH ONCE (PERIOD FMT)          
         BE    PS340                                                            
         MVC   SAVLEN(2),BUFLEN                                                 
         BAS   RE,LENEDIT          EDIT LENGTH                                  
         SPACE 1                                                                
PS340    CLI   MONFMT,C'A'         ALLMONTH                                     
         BE    PS400                                                            
         CLI   MONFMT,C'Q'         QUARTERS                                     
         BNE   PS370                                                            
         MVC   PYR(3),=C'QTR'                                                   
         MVC   PYR+3(1),BUFMON                                                  
         OI    PYR+3,X'F0'                                                      
         B     PS400                                                            
PS370    MVC   WORK(2),BUFYR       MONTHS                                       
         MVI   WORK+2,0                                                         
         GOTO1 DATCON,DMCB,(3,WORK),(6,PYR)                                     
*                                                                               
PS400    LA    R4,PURB             POINT TO OUTPUT ADDRESS                      
         LA    R6,BUFACCM          POINT TO A(UNITS/DOLLARS)                    
         BAS   RE,PRNTUNDL                                                      
         LA    R3,1(R3)            LINE COUNTER                                 
*                                                                               
***      MVC   P+01(06),=C'TABLE:'                                              
***      MVC   P+07(100),0(R2)                                                  
***      GOTO1 SPOOL,PARAS,(R8)                                                 
**>132   LA    R2,132(R2)          NEXT LINE                                    
         LA    R2,112(R2)          NEXT LINE                                    
         LA    RF,BUFF                                                          
         AH    RF,=Y(SYSX-BUFF)                                                 
         CR    R2,RF                                                            
         BL    PS100               STILL ROOM:  ADD MORE                        
         MVC   P+01(32),=C'TABLE FULL:  ALL DATA CANNOT BE '                    
         MVC   P+33(32),=C'DISPLAYED.  MODIFY REQUEST.  LIN'                    
         MVC   P+65(32),=C'E COUNT =                       '                    
         EDIT  (R3),(4,P+75)                                                    
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
PS450    EQU   *                                                                
** TEST                                                                         
*        MVC   P+1(05),=C'PS450'                                                
*        MVC   P+10(04),=C'TOTP'                                                
*        MVC   P+15(1),TOTPRNT                                                  
*        MVC   P+20(44),BUFREC                                                  
*        GOTO1 SPOOL,DMCB,(R8)                                                  
** END TEST                                                                     
         CLI   TOTPRNT,C'Y'        ANYTHING TO PRINT FOR THIS LEVEL             
         BNE   PS460                                                            
         CLI   REPTYP,C'L'                                                      
         BNE   *+10                                                             
         MVC   PLEN(4),=C'TOT*'                                                 
         CLI   REPTYP,C'P'                                                      
         BNE   *+10                                                             
         MVC   PYR(4),=C'TOT*'                                                  
         MVC   PURB(76),STARS                                                   
         LA    R4,PURB             POINT TO OUTPUT ADDRESS                      
         LA    R6,TOTACCM          POINT TO A(UNITS/DOLLARS)                    
         BAS   RE,PRNTUNDL                                                      
         LA    R3,1(R3)            LINE COUNTER                                 
**>132   LA    R2,132(R2)          NEXT LINE                                    
*PS460   MVC   0(132,R2),SPACES    SPACING LINE                                 
         LA    R2,112(R2)          NEXT LINE                                    
PS460    MVC   0(112,R2),SPACES    SPACING LINE                                 
         LA    R3,1(R3)                                                         
*                                                                               
         LA    R2,BUFF             NOW PRINT THE CLUMP                          
         STC   R3,ALLOWLIN                                                      
*PS465   MVC   P,0(R2)                                                          
PS465    MVC   P(112),0(R2)                                                     
         MVC   FOOTSW,FOOTLNS                                                   
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVI   ALLOWLIN,0                                                       
**>132   MVC   0(132,R2),SPACES    CLEAR LINE FOR NEXT TIME                     
         MVC   0(112,R2),SPACES    CLEAR LINE FOR NEXT TIME                     
**>132   LA    R2,132(R2)                                                       
         LA    R2,112(R2)                                                       
         BCT   R3,PS465                                                         
*                                                                               
PS470    CLI   BUFTYP,C'2'                                                      
         BNE   *+8                                                              
         MVI   MULTSDPT,0                                                       
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFTYP,ADBUFC),(X'80',1)                 
         MVI   TOTPRNT,C'N'                                                     
*                                                                               
RXEXT    B     XIT                                                              
*                                                                               
         SPACE 3                                                                
LENEDIT  ST    RE,SAVERE                                                        
         CLC   BUFLEN,=X'FFFF'     DON'T PRINT ANYTHING FOR ALL LENGTHS         
         BE    LE60                                                             
         CLC   BUFLEN,=X'FEFE'     PRINT 'PLAN' IF PLAN                         
         BNE   *+14                                                             
         MVC   PLEN,=C'PLAN'                                                    
         B     LE60                                                             
         SPACE 1                                                                
         TM    BUFLEN,X'80'                                                     
         BZ    LE40                                                             
         MVC   HALF,BUFLEN                                                      
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(2,PLEN)                                                
         MVI   PLEN+2,C'M'                                                      
         B     LE60                                                             
*                                                                               
LE40     EDIT  (2,BUFLEN),(3,PLEN)                                              
LE60     L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         DROP  R2                                                               
*                                                                               
*                                                                               
*                   UPRI    UCUR    UIND    RPRI    RCUR    RIND   BPRI         
STARS    DC    CL76'       *       *     *          *       *    *     X        
                      *          *     *'                                       
*                      BCUR       BIND                                          
         SPACE 2                                                                
         EJECT                                                                  
* ROUTINE TO OUTPUT ONE PRINT LINE OF UNITS AND DOLLARS                         
* PARMS R4 = OUTPUT ADDRESS        R6 = A(/UNITS/DOLLARS)                       
         SPACE 2                                                                
PRNTUNDL NTR1                                                                   
         USING PUNDLD,R4                                                        
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,15,0(R6)          ** UNITS **                                 
         BZ    PU20                                                             
         EDIT  (R5),(7,PUPRI),MINUS=YES                                         
*                                                                               
PU20     ICM   R5,15,4(R6)                                                      
         BZ    PU40                                                             
         EDIT  (R5),(7,PUCUR),MINUS=YES                                         
*                                                                               
         SR    R3,R3               GET INDEX %                                  
         SR    RE,RE                                                            
         ICM   RE,15,0(R6)                                                      
         BZ    PU40                                                             
         ICM   R3,15,4(R6)                                                      
         M     R2,=F'100'                                                       
         SR    R2,R2                                                            
         XC    FULL,FULL                                                        
         STH   RE,FULL+2                                                        
         D     R2,FULL             CURRENT DIVIDED BY PRIOR                     
         SLL   R2,1                ROUND                                        
         C     R2,FULL                                                          
         BL    *+8                                                              
         LA    R3,1(R3)                                                         
         CH    R3,=H'9999'                                                      
         BNH   *+14                                                             
         MVC   PUIND+1(3),=C'MAX'                                               
         B     PU40                                                             
         EDIT  (R3),(4,PUIND)                                                   
         CLC   PUIND,SPACES                                                     
         BNE   PU40                                                             
         MVC   PUIND+1(3),=C'MIN'                                               
*                                                                               
PU40     SR    R2,R2               ** RATE **                                   
         SR    R5,R5                                                            
         SR    RE,RE                                                            
         ICM   RE,15,0(R6)          PRIOR UNITS                                 
         BZ    PU60                                                             
         ICM   R3,15,8(R6)         PRIOR DOLLARS                                
         BZ    PU60                                                             
         BNM   *+6                 RATE COULD BE MINUS                          
         LCR   R3,R3               BUT WE NEED TO WORK WITH POSITIVE            
         XC    FULL,FULL                                                        
         STH   RE,FULL+2                                                        
         D     R2,FULL                                                          
         SLL   R2,1                                                             
         C     R2,FULL                                                          
         BL    *+8                                                              
         LA    R3,1(R3)                                                         
         LR    R5,R3               SAVE PRIOR RATE (AS POSITIVE)                
         ICM   RF,15,8(R6)                                                      
         BNM   *+6                                                              
         LCR   R3,R3               RESTORE NEGATIVE                             
         EDIT  (R3),(7,PRPRI),MINUS=YES                                         
*                                                                               
PU60     SR    R2,R2               CURRENT                                      
         SR    RE,RE                                                            
         ICM   RE,15,4(R6)                                                      
         BZ    PU80                                                             
         ICM   R3,15,12(R6)                                                     
         BZ    PU80                                                             
         BNM   *+6                                                              
         LCR   R3,R3                                                            
         XC    FULL,FULL                                                        
         STH   RE,FULL+2                                                        
         D     R2,FULL                                                          
         SLL   R2,1                                                             
         C     R2,FULL                                                          
         BL    *+8                                                              
         LA    R3,1(R3)                                                         
         ST    R3,FULL             SAVE CURRENT RATE                            
         ICM   RF,15,12(R6)                                                     
         BNM   *+6                                                              
         LCR   R3,R3                                                            
         EDIT  (R3),(7,PRCUR),MINUS=YES                                         
         L     R3,FULL                                                          
*                                                                               
         LTR   R5,R5               R5=PRIOR RATE    (R3=CURRENT RATE)           
         BZ    PU80                                                             
         ST    R5,FULL                                                          
         M     R2,=F'100'                                                       
         SR    R2,R2                                                            
         D     R2,FULL             CURRENT DIVIDED BY PRIOR                     
         SLL   R2,1                                                             
         C     R2,FULL                                                          
         BL    *+8                                                              
         LA    R3,1(R3)                                                         
         CH    R3,=H'9999'                                                      
         BNH   *+14                                                             
         MVC   PRIND+1(3),=C'MAX'                                               
         B     PU80                                                             
         EDIT  (R3),(4,PRIND)                                                   
         CLC   PRIND,SPACES                                                     
         BNE   PU80                                                             
         MVC   PRIND+1(3),=C'MIN'                                               
*                                                                               
PU80     ICM   R3,15,8(R6)         ** BILLING **                                
         BZ    PU100                                                            
         EDIT  (R3),(10,PBPRI),MINUS=YES                                        
*                                                                               
PU100    ICM   R3,15,12(R6)                                                     
         BZ    XIT                                                              
         EDIT  (R3),(10,PBCUR),MINUS=YES                                        
*                                                                               
         ICM   RE,15,8(R6)                                                      
         BZ    XIT                                                              
         ST    RE,FULL                                                          
         M     R2,=F'100'                                                       
         SR    R2,R2                                                            
         D     R2,FULL                                                          
         SLL   R2,1                                                             
         C     R2,FULL                                                          
         BL    *+8                                                              
         LA    R3,1(R3)                                                         
         CH    R3,=H'9999'                                                      
         BNH   *+14                                                             
         MVC   PBIND+1(3),=C'MAX'                                               
         B     XIT                                                              
         EDIT  (R3),(4,PBIND)                                                   
         CLC   PBIND,SPACES                                                     
         BNE   XIT                                                              
         MVC   PBIND+1(3),=C'MIN'                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* READ STATION TO SEE IF IT MEETS FILTER (OWNERSHIP, TVB, RANK)                 
* REQUIREMENTS.  ON EXIT, TAKE RECORD IF R3 IS POSITIVE,                        
*                         REJECT RECORD IF R3 IS ZERO                           
         SPACE 2                                                                
READSTA  NTR1                                                                   
         SR    R3,R3                                                            
         MVC   SVATNKY,KEY                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(5),SVATNKY+ASTAE                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    RS10                                                             
         B     NO                  STATION NOT FOUND:  TREAT AS NO              
***>>>   DC    H'0'                                                             
RS10     EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         L     R6,AIO                                                           
         USING RSTAKEY,R6                                                       
         MVC   MARKET(20),RSTAMKT  MARKET NAME                                  
         OC    OWNER,OWNER                                                      
         BZ    RS20                                                             
         CLC   OWNER,RSTAOWN                                                    
         BNE   NO                                                               
         SPACE 1                                                                
RS20     OC    TVB,TVB                                                          
         BZ    RS40                                                             
         CLC   TVB,RSTATVB                                                      
         BNE   NO                                                               
         SPACE 1                                                                
RS40     OC    RANK,RANK                                                        
         BZ    YES                                                              
         CLC   RANK,RSTARANK                                                    
         BNE   NO                                                               
         DROP  R6                                                               
         SPACE 1                                                                
YES      LA    R3,1                                                             
         SPACE 1                                                                
NO       MVC   KEY(34),SVATNKY     REREAD ATHENA KEY                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    STEXT                                                            
         DC    H'0'                                                             
STEXT    XIT1  REGS=(R3)                                                        
         EJECT                                                                  
* READ & CHECK IF SCREEN POSITION HAS ENOUGH ROOM TO OUTPUT ALL 2NDARY          
* -- NOTE -- IT EXPECTS RECORD TO REMAIN IN AIO2                                
READDPT  NTR1                                                                   
         LR    R5,RE               RE NZ NOT 1ST TIME FOR DPT                   
         MVC   SVATNKY,KEY         SAVE ATHENA KEY                              
         CLC   KEY+ADPTE(2),=X'FFFF'                                            
         BE    DNEXT               PSUEDO                                       
         CLC   KEY+ADPTE(3),=X'FEFEFC'                                          
         BE    DNEXT               PLAN                                         
         CLC   KEY+ADPTE(2),=X'FEFE'                                            
         BE    RD60                PROGRAM TYPE                                 
         XC    CURSDPT,CURSDPT                                                  
         CLC   KEY+ADPTE(1),CURDPTC                                             
         BE    RD20                GET NEXT 2NDARY DPT                          
         SR    R5,R5                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'24'                                                        
         MVC   KEY+24(2),AGENCY     REP                                         
         MVC   KEY+26(1),SVATNKY+ADPTE DAYPART                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
RD20     L     R6,AIO2                                                          
         MVC   CURDPT,RDPTNAME-RDPTKEY(R6)                                      
         MVC   CURDPTC,RDPTKDPT-RDPTKEY(R6)                                     
         XC    CURSDPT,CURSDPT                                                  
         TM    PRNTOPT,X'80'      SUPPRESS SUB DAYPART                          
         BO    RD140                                                            
         MVI   ELCODE,X'02'                                                     
*                                                                               
         BAS   RE,GETEL                                                         
         B     *+8                                                              
RD40     BAS   RE,NEXTEL                                                        
         BNE   RD120                                                            
*                                                                               
*                                                                               
*   TEST PRINT BUFREC                                                           
*        MVC   P+1(07),=C'CURSDPT'                                              
*        MVC   P+10(08),0(R6)                                                   
*        MVC   P+20(34),SVATNKY                                                 
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*   TEST END                                                                    
*                                                                               
         CLC   2(1,R6),SVATNKY+ASDTE                                            
         BNE   RD40                                                             
         MVC   CURSDPT,3(R6)                                                    
         B     RD120                                                            
*                                                                               
RD60     XC    KEY,KEY             READ PROGRAM RECORD                          
         MVI   KEY,X'25'                                                        
         MVC   KEY+24(2),AGENCY                                                 
         MVC   KEY+26(1),SVATNKY+APRGE                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    RD80                                                             
         MVC   CURPRG,=CL11'UNKNOWN'                                            
         B     RD100                                                            
RD80     MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         L     R6,AIO2                                                          
         MVC   CURPRG,RPGTNAME-RPGTKEY(R6)                                      
RD100    SR    R5,R5                                                            
*                                                                               
RD120    OC    CURSDPT,CURSDPT                                                  
         BNZ   *+10                                                             
         MVC   CURSDPT,=C'XXXXX'                                                
RD140    MVC   KEY(34),SVATNKY     REREAD ATHENA KEY                            
         LTR   R5,R5               DON'T RE-READ IF DPT REC NOT READ            
         BNZ   DNEXT                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    DNEXT                                                            
         DC    H'0'                                                             
DNEXT    B     XIT                                                              
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
CQERR    LA    R2,AURPERH          POINT CURSOR TO PERIOD                       
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
*  ROUTINE TO GET UNITS AND DOLLARS FROM ATHENA RECORD                          
         SPACE 2                                                                
GETUNDL  ST    RE,FULL                                                          
         MVI   RDUPDATE,C'N'       REPORTING FUNCTION CAN READ RECORDS          
         GOTO1 GETREC              UNLOCKED                                     
         XC    UNDL,UNDL           UNIT/$ ACCUM                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
GETU0020 BAS   RE,NEXTEL                                                        
         BNE   GETU0140                                                         
*                                                                               
         OC    ASAT,ASAT                                                        
         BZ    *+14                                                             
         CLC   ASAT,2(R6)                                                       
         BL    GETU0020                                                         
*                                                                               
         CLI   OPT$$$,0            REGULAR DOLLARS REQUESTED?                   
         BNE   GETU0040            NO  - REGULAR NOT NEEDED                     
         TM    4(R6),X'80'         REGULAR DOLLARS IN ELEMENT?                  
         BNO   GETU0020            NO  - GO BACK FOR NEXT                       
         SR    RE,RE               YES                                          
         ICM   RE,15,UNACCM        ADD TO UNIT ACCUMULATOR                      
         AH    RE,5(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,UNACCM                                                     
         ICM   RE,15,DLACCM                                                     
         A     RE,7(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,DLACCM                                                     
         B     GETU0120                                                         
GETU0040 EQU   *                                                                
         CLI   OPT$$$,C'B'         BOTH $$$ TYPES REQUESTED?                    
         BNE   GETU0080            NO                                           
         TM    4(R6),X'80'         REGULAR DOLLARS IN ELEMENT?                  
         BNO   GETU0060            NO  - CHECK COMBO $$$                        
         SR    RE,RE               YES                                          
         ICM   RE,15,UNACCM        ADD TO UNIT ACCUMULATOR                      
         AH    RE,5(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,UNACCM                                                     
         ICM   RE,15,DLACCM                                                     
         A     RE,7(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,DLACCM                                                     
         TM    4(R6),X'40'         COMBO   DOLLARS IN ELEMENT?                  
         BNO   GETU0120            NO  - GO BACK FOR NEXT                       
         SR    RE,RE               YES                                          
         ICM   RE,15,UNACCM        ADD TO UNIT ACCUMULATOR                      
         AH    RE,11(R6)           USE SECOND BUCKETS                           
         STCM  RE,15,UNACCM                                                     
         ICM   RE,15,DLACCM                                                     
         A     RE,13(R6)           USE SECOND BUCKETS                           
         STCM  RE,15,DLACCM                                                     
         B     GETU0120                                                         
GETU0060 EQU   *                                                                
         TM    4(R6),X'40'         COMBO   DOLLARS IN ELEMENT?                  
         BNO   GETU0020            NO  - GO BACK FOR NEXT                       
*                                                                               
*   ABOVE SHOULD NEVER HAPPEN.  THIS WOULD INDICATE THAT THERE WAS              
*     A DOLLAR BUCKET WITH NEITHER REGULAR OR COMBO FIELDS                      
*                                                                               
         SR    RE,RE               YES                                          
         ICM   RE,15,UNACCM        ADD TO UNIT ACCUMULATOR                      
         AH    RE,5(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,UNACCM                                                     
         ICM   RE,15,DLACCM                                                     
         A     RE,7(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,DLACCM                                                     
         B     GETU0120                                                         
GETU0080 EQU   *                                                                
         CLI   OPT$$$,C'C'         COMBO $$ REQUESTED?                          
         BE    *+6                 YES                                          
         DC    H'0'                NO TYPE??????                                
         TM    4(R6),X'40'         COMBO   DOLLARS IN ELEMENT?                  
         BNO   GETU0020            NO  - GO BACK FOR NEXT                       
         TM    4(R6),X'80'         REGULAR DOLLARS IN ELEMENT?                  
         BNO   GETU0100            NO  - USE FIRST SET OF BUCKETS               
         SR    RE,RE               YES - USE SECOND SET OF BUCKETS              
         ICM   RE,15,UNACCM                                                     
         AH    RE,11(R6)                                                        
         STCM  RE,15,UNACCM                                                     
         ICM   RE,15,DLACCM                                                     
         A     RE,13(R6)                                                        
         STCM  RE,15,DLACCM                                                     
         B     GETU0120                                                         
GETU0100 EQU   *                                                                
         SR    RE,RE               USE FIRST  SET OF BUCKETS                    
         ICM   RE,15,UNACCM                                                     
         AH    RE,5(R6)                                                         
         STCM  RE,15,UNACCM                                                     
         ICM   RE,15,DLACCM                                                     
         A     RE,7(R6)                                                         
         STCM  RE,15,DLACCM                                                     
GETU0120 EQU   *                                                                
*                                                                               
         B     GETU0020                                                         
*                                                                               
GETU0140 L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*  FOOT HOOK ROUTINE FOR REQUEST DETAILS                                        
FHOOK    NTR1                                                                   
         MVC   P,SPACES                                                         
         CLI   FOOTSW,0            HAVE I ALREADY PRINTED EVERYTHING            
         BE    XIT                 YES                                          
         L     R5,SVFOOTF                                                       
         LA    R1,SVFOOT                                                        
         CR    R5,R1               IS THERE ANYTHING TO PRINT                   
         BE    FH40                NO                                           
         BCTR  R5,0                YES, BACK UP AND BLANK OUT COMMA             
         CLI   0(R5),C','                                                       
         BE    FH20                                                             
         CLI   0(R5),C' '          OR MAY ALREADY BE BLANKED OUT                
         BE    FH20                                                             
         DC    H'0'                SOMETHING IS WRONG                           
         SPACE 1                                                                
FH20     MVI   0(R5),C' '                                                       
         MVC   P(8),=C'DETAILS-'                                                
         MVC   P+8(101),SVFOOT                                                  
FH40     MVI   FOOTSW,0            INDICATE NO MORE TO PRINT                    
         B     XIT                                                              
         EJECT                                                                  
*  HOOK ROUTINE FOR HEADLINE DETAIL                                             
         SPACE 1                                                                
HOOK     NTR1                                                                   
         MVC   H8+15(6),=C'LENGTH'                                              
         MVC   H9+15(6),DASH                                                    
         CLI   REPTYP,C'L'                                                      
         BNE   HK3                                                              
         MVC   H1+17(6),=C'LENGTH'                                              
HK3      CLI   REPTYP,C'P'                                                      
         BNE   HK5                                                              
         MVC   H1+17(6),=C'PERIOD'                                              
         CLI   MONFMT,C'A'                                                      
         BE    *+16                                                             
         MVC   H8+24(4),=C'DATE'                                                
         MVC   H9+23(6),DASH                                                    
         TM    PRNTOPT,X'20'       NO LENGTH                                    
         BZ    HK5                                                              
         MVC   H8+15(6),SPACES                                                  
         MVC   H9+15(6),SPACES                                                  
*                                                                               
HK5      MVC   H5+17(20),SVMKT                                                  
         OC    STATN,STATN                                                      
         BNZ   HK10                                                             
         MVC   H4(5),=C'GROUP'                                                  
         MVC   H4+9(21),GROUPN                                                  
         MVC   H5+9(4),BUFSTA                                                   
         CLI   BUFSTA+4,C' '                                                    
         BE    HK20                                                             
         MVI   H5+13,C'-'                                                       
         MVC   H5+14(1),BUFSTA+4                                                
         B     HK20                                                             
HK10     MVC   H5+9(7),AURSTA                                                   
         SPACE 1                                                                
HK20     LA    R2,H5+84                                                         
         GOTO1 DATCON,DMCB,(3,SMONTH),(6,0(R2))   START MONTH                   
         CLC   SMONTH,EMONTH                                                    
         BE    HKXIT                                                            
         LA    R2,6(R2)                                                         
         MVI   0(R2),C'-'                                                       
         GOTO1 DATCON,DMCB,(3,EMONTH),(6,1(R2))     END MONTH                   
HKXIT    B     XIT                                                              
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
ADBUFC   DC    A(BUFFALOC)                                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*  MY OWN ERROR MESSAGES                                                        
         SPACE 2                                                                
GRPORSTA DC    C'EITHER GROUP OR STATION REQUIRED'                              
ONESTAER DC    C'ONE STATION - CAN NOT FILTER ON OWNER,TVB,RANK'                
RANKE2   DC    C'RANK MUST BE FROM 1-7'                                         
TYPER    DC    C'REPORT TYPE L MUST BE WHOLE MONTH'                             
INVFMT   DC    C'INVALID FORMAT AT THIS TIME'                                   
MANYDPT  DC    C'MAXIMUM 4 DAYPARTS ALLOWED'                                    
MNYLEN   DC    C'MAXIMUM 4 LENGTHS ALLOWED'                                     
MANYLENP DC    C'FOR REPORT TYPE P, ONLY 1 LENGTH ALLOWED'                      
DATEFMT  DC    C'FORMAT IS MMM/YY'                                              
NOTQTR   DC    C'DATES MUST INCLUDE COMPLETE QUARTERS'                          
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
         PSPEC H7,81,C'- - - - -  BOOKED   - - - -'                             
         PSPEC H8,81,C'PRIOR      CURRENT    INDEX'                             
         PSPEC H9,81,C'---------------------------'                             
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*   WORK AREA                                                                   
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL8'**BUFF**'                                                    
         BUFF  LINES=200,ROWS=1,COLUMNS=4,FLAVOR=BINARY,KEYLIST=(16,A),X        
               COMMENT=12                                                       
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
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         SPACE 2                                                                
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
* RESFMF7D                                                                      
         SPACE 2                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE RESFME7D                                                       
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
         PRINT ON                                                               
         SPACE 2                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*                  ---  WORK AREA ---                                           
         DS    0F                                                               
MYWORK   DS    0CL512                                                           
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
*                                  0  =  REGULAR DOLLARS/UNITS                  
*                                  C  =  COMBO   DOLLARS/UNITS                  
*                                  B  =  BOTH COMBO + REGULAR                   
AGOFFFLG DS    CL1 AGENCY/AGENCY OFFICE FLAG                                    
*                                  X'0'  -  NO AGENCY FILTER AT ALL             
*                                  C'C'  -  AGENCY CODE ONLY                    
*                                  C'O'  -  AGENCY/OFFICE                       
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
BUFREC   DS    0CL44                                                            
BUFKEY   DS    0CL16                                                            
BUFTYP   DS    CL1                                                              
BUFSTA   DS    CL5                                                              
BUFDPT   DS    CL1                                                              
BUFSDPT  DS    CL2                                                              
BUFLEN   DS    CL2                                                              
BUFYR    DS    CL1                                                              
BUFMON   DS    CL1                                                              
         DS    CL3                 NOT DEFINED                                  
BUFDPTN  DS    CL11                                                             
         DS    CL1                 NOT DEFINED                                  
         DS    0F                                                               
BUFACCM  DS    0CL16                                                            
BUFPUN   DS    F                                                                
BUFUN    DS    F                                                                
BUFPDL   DS    F                                                                
BUFDL    DS    F                                                                
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
T81807   CSECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*TVB TABLE                                                                      
       ++INCLUDE RETVBTAB                                                       
         SPACE 3                                                                
*                                                                               
*  SETAGY:  DEVELOP THE AGENCY KEY IF JUST AGENCY CODE, OR AGENCY/OFF           
*                                                                               
SETAGY   NMOD1 0,*SAGY*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,4(R1)            RESET R4 FOR ROUTINE                         
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
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096RESFM11   05/01/02'                                      
         END                                                                    
