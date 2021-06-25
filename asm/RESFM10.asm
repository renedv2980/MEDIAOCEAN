*          DATA SET RESFM10    AT LEVEL 098 AS OF 05/01/02                      
*PHASE T81810A,*                                                                
         TITLE 'T81810 - RESFM10 --- ADETAIL AND PDETAIL UPDATE'                
*                                                                               
*********************************************************************           
*                                                                   *           
*        RESFM10 --- ADETAIL/PDETAIL UPDATE                         *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* APR17/89 (MRR) --- RE-LINK TO REMOVE 14TH DATA LINE DUE TO 3K     *           
*                     SPLIT TWA AREA                                *           
*                                                                   *           
* APR22/89 (MRR) --- RE-LINK DUE TO SCREEN CHANGES                  *           
*                                                                   *           
* JUN13/89 (MRR) --- PROGRAM DUMPS ON AN EMPTY TABLE SITUATION...   *           
*                     TRAP AND GET OUT W/O DUMP                     *           
*                                                                   *           
* JUL5/89  (MRR) --- CHECK FOR A SAVED KEY WHEN RE-BUILDING THE     *           
*                     TABLE, RETURN EMPTY LIST IF NO KEY FOUND      *           
*                                                                   *           
* 08/29/89  PJS  --- FIX RATE TRASHING ON LONG TITLE OVERRIDE       *           
*                                                                   *           
*                --- DISPLAY 'HIGH' IF RATE .GT. 999,999            *           
*                                                                   *           
* 09/15/89  PJS  --- DON'T RE-BUILD ITEM TABLE AFTER RETURNING      *           
*                    FROM SELECTS.  ITEM NUMBER GETS OUT OF SYNC.   *           
*                                                                   *           
* OCT12/89 (MRR) --- MAKE SATELLITE INDICATOR ON UPDATE SCREEN WORK *           
*                                                                   *           
* OCT18/89 (MRR) --- ADDING RATES TO A DETAIL LINE HURTS THE        *           
*                     SATELLITE MARKER AND/OR INVENTORY NUMBER      *           
*                                                                   *           
* 02NOV89  (EFJ) --- ALLOW SOME TEXT OR BOOK ELEMS TO BE ADDED IN   *           
*                    INVENTORY FIELD INSTEAD OF USING DETAIL        *           
*                    SCREEN.  IE: TX=... OR BK=...                  *           
*                                                                   *           
* 07MAY90  (EFJ) --- REMOVE 'SOURCE' FIELD                          *           
*                                                                   *           
* 10MAY90  (EFJ) --- AVAIL LIST: ONE RATE FIELD, MULTIPLE RATES     *           
*                     ALLOWED, RATE WILL PROCEED INVENTORY NUMBER.  *           
*                    PROP LIST: RATE, #, AND CD FIELDS MOVED TO     *           
*                     PROCEED INVENTORY NUMBER                      *           
*                                                                   *           
* 22MAY90  (EFJ) --- UNHOOK CHECK TO SEE IF MORE RATES THAN         *           
*                    LENGTHS, AS PER PFOL                           *           
*                                                                   *           
* 24MAY90  (EFJ) --- FIX BUG CREATED BY USING KEYSAVE IN CHAPRAV    *           
*                                                                   *           
* 25MAY90  (EFJ) --- UNDO PJS FIX OF 09/15/90 - CAUSES DISPLAY      *           
*                    PROBLEM ON PD/UP CODE CHANGES                  *           
*                                                                   *           
* 31MAY90  (EFJ) --- FIX BUG CAUSING FIRST LINE TO BE 5 ON LINE-BY- *           
*                    LINE ADD.  (CHANGE MVC TO MVI).                *           
*                                                                   *           
* 06JUN90  (EFJ) --- FIX UNNOTICED BUG IN PUTTEXT                   *           
*                                                                   *           
* 14JUN90  (EFJ) --- FIX BUG WHEN CHANGING INV # - CAUSED DRTE TO   *           
*                    HAVE SPACES - CK FIRST...                      *           
*                                                                   *           
* 27JUL90  (EFJ) --- FORCE REVALIDATION OF KEY IF MYKEY IS BS       *           
*                                                                   *           
* 26SEP90  (EFJ) --- FIX BUG CAUSING RATE TO DISAPPEAR ON CHANGE    *           
*                     OF PD/UP LINE                                 *           
*                                                                   *           
* 10DEC90  (EFJ) --- FIX BUG WHEN ADDING BOOKS IN INV FLD.          *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                    BEFORE CHANGING SCREEN, LOOK AT 'HARD' CODE    *           
*                    BLOCK IN PUTTEXT... (EFJ)                      *           
*                                                                   *           
*********************************************************************           
*                                                                               
T81810   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1810**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         SPACE                                                                  
         LA    R3,13               ** HARD NUMBER OF LINES ON SCREEN **         
         ST    R3,NUMWIND                                                       
         OI    CONSERVH+6,X'81'    ALWAYS SAY SCREEN MODIFIED                   
         SPACE                                                                  
         CLI   MODE,VALKEY         DO ALL THROUGH VALKEY                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   MYSCRNUM,TWASCR     FIRST TIME THROUGH THIS SCREEN?              
         BE    *+14                NO                                           
         MVC   MYSCRNUM,TWASCR     YES -- STORE SCREEN NUMBER                   
         NI    LDECONH+4,X'DF'     FORCE VALIDATION OF KEY                      
         SPACE 1                                                                
         CLI   RETURNED,2          HAVE WE JUST RETURNED FROM DET CHA?          
         BNE   MAIN                NO -- CALLPROG DIDN'T HELP US                
         SPACE 1                                                                
         BAS   RE,TESTSEL          YES -- CHECK FOR SELECTS, THEN EXIT          
         BAS   RE,BLDITAB          REBUILD TABLE(* IS THIS NECESSARY *)         
         BAS   RE,DWIND            DISPLAY WINDOW                               
*                                                                               
         B     EXIT1               TAKE ERROR EXIT                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* WE CAME HERE HEAD-ON (NOT FROM ANOTHER SCREEN)                                
*                                                                               
MAIN     MVI   KEYCHANG,C'N'       INITIALIZE KEY/REC CHANGED FLAGS             
         MVI   RECCHANG,C'N'                                                    
         MVI   SCRCHANG,C'N'                                                    
         MVI   RECNEW,C'N'                                                      
         BAS   RE,VKEY             VALIDATE KEY                                 
         SPACE 1                                                                
         CLI   KEYCHANG,C'Y'       IF KEY HAS CHANGED                           
         BNE   *+8                                                              
         BAS   RE,BLDITAB          BUILD ITEM KEY TABLE (*** ??? ***)           
         SPACE 1                                                                
         BAS   RE,VSTARTAT         VALIDATE STARTAT LINE NUMBER FIELD           
         CLI   KEYCHANG,C'Y'       IF KEY HAS CHANGED                           
         BNE   M30                                                              
         BAS   RE,SETITEM          SET ITEM NUMBER FROM STARTAT ITEM            
         BAS   RE,CHGITEM          CHANGE ITEM NUMBER FROM PFKEYS               
         BAS   RE,DWIND            DISPLAY WINDOW                               
         B     M40                                                              
         SPACE 1                                                                
M30      BAS   RE,VWIND            VALIDATE WINDOW                              
         BAS   RE,CHGITEM          CHANGE ITEM NUMBER FROM PFKEYS               
         BAS   RE,TESTSEL          DO A SELECT FROM SELTAB, THEN EXIT           
         BAS   RE,DWIND            NO SELECTS REMAIN -- DISPLAY WINDOW          
         SPACE 1                                                                
M40      CLI   SCRCHANG,C'N'       IF NO SCREEN FIELDS HAVE CHANGED             
         BNE   EXIT2               THEN DISPLAY . . .                           
         LA    R2,LDESELH                                                       
EXIT1    MVC   RERROR,=H'2'        . . .PLEASE ENTER FIELDS AS REQUIRED         
         MVI   RMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         B     ERREND                                                           
         SPACE 1                                                                
EXIT2    LA    R2,LDECONH                                                       
         MVC   RERROR,=H'17'       ACTION COMPLETED -- ENTER NEXT               
         MVI   RMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         B     ERREND                                                           
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VKEY     NTR1                                                                   
         CLI   MYKEY,0             NULL?                                        
         BNE   VK01                                                             
         NI    LDECONH+4,X'FF'-X'20' FORCE RE-VALIDATION AND...                 
         NI    LDEHDRH+4,X'FF'-X'20' ...SAVE OFF HDR KEY                        
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE CONTRACT  (REQUIRED)                                   *          
*====================================================================*          
         SPACE 1                                                                
VK01     LA    R2,LDECONH                                                       
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 VALICON,DMCB,(R2)                                                
         OI    4(R2),X'20'                                                      
         LA    R2,LDESTNH          POINT TO CALL LETTERS FIELD                  
         GOTO1 DISPCON,DMCB,(R2)                                                
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE TYPE (REQUIRED)                                        *          
*       A=AVAIL,  P=PROPOSAL                                         *          
*====================================================================*          
         SPACE 1                                                                
VK10     MVC   ETYPE,CONREC        WILL BE EITHER 'A' OR 'P'                    
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE SOURCE  (REQUIRED)                                     *          
*        I=INVENTORY,  S=SID                                         *          
*====================================================================*          
         SPACE 1                                                                
*         LA    R2,LDESRCH                                                      
*         TM    4(R2),X'20'                                                     
*         BO    VK20                                                            
*         MVI   KEYCHANG,C'Y'                                                   
*         GOTO1 VALISRC                                                         
*         OI    4(R2),X'20'                                                     
         MVI   ESOURCE,C'I'        ALWAYS 'I' NOW                               
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE HEADER NUMBER  (REQUIRED)                              *          
*====================================================================*          
         SPACE 1                                                                
VK20     LA    R2,LDEHDRH                                                       
         TM    4(R2),X'20'                                                      
         BO    VK30                                                             
         MVI   KEYCHANG,C'Y'                                                    
         MVC   RERROR,=AL2(MISSING)                                             
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         STC   R0,XHDRNUM                                                       
         MVC   EHDRNUM,8(R2)                                                    
         OI    4(R2),X'20'                                                      
         SPACE 1                                                                
VK30     CLI   KEYCHANG,C'Y'       IF KEY CHANGED, BUILD KEY                    
         BNE   VKX                                                              
         SPACE 1                                                                
         XC    KEY,KEY             GET HEADER, THEN BUILD DETAIL KEY            
         LA    R6,KEY                                                           
         MVC   RERROR,=AL2(NOTFOUND)                                            
         MVC   AIO,AIO3            READ HEADER IN IO3                           
         SPACE 1                                                                
         CLI   ETYPE,C'A'          AVAILS?                                      
         BNE   VK40                NO, PROPOSALS                                
         SPACE 1                                                                
         USING RAVLREC,R6                                                       
         MVI   RAVLKTYP,X'14'      BUILD AVAIL HEADER KEY                       
         MVC   RAVLKREP,AGENCY                                                  
         MVC   RAVLKCON,CCONNUM                                                 
         MVC   RAVLKAVN,XHDRNUM                                                 
         MVC   RAVLKSRC,ESOURCE                                                 
         DROP  R6                                                               
         B     VK50                                                             
         SPACE 1                                                                
         USING RPRPREC,R6                                                       
VK40     MVI   RPRPKTYP,X'16'      BUILD PROPOSAL HEADER KEY                    
         MVC   RPRPKREP,AGENCY                                                  
         MVC   RPRPKCON,CCONNUM                                                 
         MVC   RPRPKPRP,XHDRNUM                                                 
         MVC   RPRPKSRC,ESOURCE                                                 
         DROP  R6                                                               
         SPACE 1                                                                
VK50     GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+12                                                             
         LA    R2,LDEHDRH          HEADER RECORD NOT FOUND                      
         B     ERREND                                                           
         SPACE 1                                                                
         MVC   MYKEY(27),KEY                                                    
         GOTO1 GETREC                                                           
         BAS   RE,HDRINFO                                                       
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
**********************************************************************          
*    VALIDATE LINE NUMBER (STARTAT)                                  *          
**********************************************************************          
**********************************************************************          
         SPACE 1                                                                
VSTARTAT NTR1                                                                   
         LA    R2,LDELINH                                                       
         TM    4(R2),X'20'                                                      
         BO    XIT                                                              
         MVI   KEYCHANG,C'Y'                                                    
         MVI   STARTAT,0                                                        
         CLI   5(R2),0                                                          
         BE    VS50                                                             
         SPACE 1                                                                
         MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         STC   R0,STARTAT                                                       
VS50     OI    4(R2),X'20'                                                      
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY WINDOW ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
DWIND    NTR1                                                                   
         BAS   RE,CLEARREC         CLEAR ALL FIELDS                             
         SPACE                                                                  
         LA    R2,LDELNUMH         POINT TO 1ST DISPLAY LINE-LINE #             
         ST    R2,SAVER2                                                        
         L     R3,ITEM             POINT R3 TO FIRST ITEM TO DISPLAY            
         MH    R3,=H'5'            LENGTH OF TABLE ENTRY                        
         LA    R3,ITEMTAB(R3)                                                   
         SPACE 1                                                                
DW20     DS    0H                                                               
         ST    R3,SAVER3                                                        
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BZ    DW25                NO                                           
         ZIC   RE,0(R2)                                                         
         AR    RE,R2               BUMP TO NEXT FIELD                           
         SH    RE,=H'8'            BACK UP TO FIELD ID NUMBER                   
         CLI   0(RE),99            ARE WE INTO THE PF KEY DIRECTORY?            
         BE    DW100               YES -- WE'RE DONE                            
DW25     EQU   *                                                                
         CLI   0(R3),X'FF'         END OF ITEM LIST?                            
         BE    DW100               YES                                          
         CLI   0(R3),0             EMPTIED TABLE                                
         BE    DW100               YES                                          
         SPACE 1                                                                
         LA    R4,LISTAR                                                        
         XC    LISTAR,LISTAR                                                    
         USING LINED,R4                                                         
         ZIC   R5,0(R3)            LINE NUMBER                                  
         EDIT  (R5),(3,LLNUM),ALIGN=LEFT                                        
         SPACE 1                                                                
         CLC   1(4,R3),=X'FFFFFFFF'   ITEM DELETED                              
         BNE   DW30B                                                            
DW30A    EQU   *                                                                
         MVC   LINV(13),=C'** DELETED **'                                       
         B     DWOVX                                                            
DW30B    EQU   *                                                                
         CLC   1(4,R3),=X'00000000'                                             
         BE    DW30A                                                            
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),1(R3)                                                  
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   ETYPE,C'A'          AVAILS?                                      
         BE    DWN60               YES - NO CODE, NO #                          
         SPACE 1                                                                
         USING RPRPDEL,R6                                                       
         CLI   RPRPDNC,C'T'                                                     
         BNE   DWN40                                                            
         DROP  R6                                                               
* NEED TO GET TOTAL NUMBER FROM PLAN HEADER,NOT DETAIL RECORD                   
         L     R6,AIO              CURRENT DETAIL RECORD                        
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(26),0(R6)       GET PACKAGE HEADER                           
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'13'        PACKAGE ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RPRPPEL,R6                                                       
         ZIC   R5,RPRPPSPT         GET TOTAL NUMBER OF SPOTS                    
         DROP  R6                                                               
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RPRPDEL,R6                                                       
         B     DWN50                                                            
         SPACE 1                                                                
DWN40    ZIC   R5,RPRPDNUM                                                      
DWN50    LTR   R5,R5                                                            
         BZ    DWN60                                                            
         EDIT  (R5),(3,LNUM),ALIGN=LEFT                                         
         LR    RF,R0               R0=NUM OF SIGNIF. CHARS                      
         LA    RE,LNUM                                                          
         LA    RF,0(RF,RE)                                                      
         MVC   0(1,RF),RPRPDNC                                                  
         DROP  R6                                                               
         SPACE 1                                                                
DWN60    DS    0H                                                               
         XC    CBLOCK+100(25),CBLOCK+100                                        
         LA    R2,CBLOCK+100                                                    
         CLI   ETYPE,C'P'                                                       
         BNE   DWN70                                                            
         SPACE 1                                                                
         USING RPRPDEL,R6                                                       
         MVC   CBLOCK(3),RPRPDINV                                               
         MVC   CBLOCK+3(3),RPRPDATE                                             
         MVC   CBLOCK+6(1),RPRPDSAT                                             
         B     DWN80                                                            
         DROP  R6                                                               
         SPACE 1                                                                
         USING RAVLDEL,R6                                                       
DWN70    MVC   CBLOCK(3),RAVLDINV                                               
         MVC   CBLOCK+3(3),RAVLDATE                                             
         MVC   CBLOCK+6(1),RAVLDSAT                                             
         DROP  R6                                                               
         SPACE 1                                                                
DWN80    DS    0H                                                               
         GOTO1 DISPINV                                                          
         MVC   LINV,CBLOCK+108                                                  
         SPACE 1                                                                
         CLI   LINV,C'P'                                                        
         BNE   *+14                                                             
         MVC   LTIM(8),=C'**PURE**'                                             
         B     DWDTTX                                                           
         SPACE 1                                                                
         CLC   CBLOCK(3),=C'MAN'                                                
         BE    DWDTTX                                                           
         SPACE 1                                                                
         GOTO1 DISIDTT             GET INVENTORY DAY/TIME/TITLE                 
         OC    CBLOCK+10(L'LDAY),CBLOCK+10                                      
         BNZ   DWN85               INVENTORY RECORD WAS FOUND                   
         MVC   LDAY,HYPHENS                                                     
         MVC   LTIM,HYPHENS                                                     
         MVC   LTTL,HYPHENS                                                     
         B     DWDTTX                                                           
         SPACE 1                                                                
DWN85    MVC   LDAY,CBLOCK+10                                                   
         MVC   LTIM,CBLOCK+20                                                   
         MVC   LTTL,CBLOCK+40                                                   
         SPACE 1                                                                
DWDTTX   CLI   ETYPE,C'P'          PROPOSAL?                                    
         BNE   DWDTTX2             NO                                           
         USING RPRPDEL,R6                                                       
         OC    RPRPDRTE,RPRPDRTE   OUTPUT RATE, IF ANY                          
         BZ    DWRTX                                                            
         ICM   R5,15,RPRPDRTE                                                   
         EDIT  (R5),(6,LRAT),ALIGN=LEFT                                         
*                                                                               
         CLC   RPRPDRTE(4),=F'1000000'     EXCEED DISPLAY MAX?                  
         BL    *+10                                                             
         MVC   LRAT(6),=CL6' HIGH '                                             
*                                                                               
         DROP  R6                                                               
         B     DWRTX                                                            
         SPACE 1                                                                
*                                                                               
*- EDIT AVAIL RATES FOR DISPLAY.                                                
         SPACE 1                                                                
         USING RAVLDEL,R6                                                       
DWDTTX2  OC    RAVLDRTE,RAVLDRTE   ANY RATES?                                   
         BZ    DWRTXX              NO                                           
         SPACE 1                                                                
         LA    R2,LRAT                                                          
         LA    R5,RAVLDRTE                                                      
         LA    R3,6                MAX NUMBER OF RATES                          
         LA    R1,13(R2)                                                        
         SPACE 1                                                                
DWRTX2   DS    0H                                                               
         ICM   RE,15,0(R5)                                                      
         EDIT  (RE),(6,RATE),ALIGN=LEFT                                         
*         EDIT  (RE),(6,0(R2)),ALIGN=LEFT                                       
         AR    R2,R0               LENGTH OF OUTPUT                             
         CR    R2,R1               WILL THIS FIT?                               
         BL    *+10                YES                                          
         SR    R2,R0               NO, AND ERASE LAST COMMA                     
         B     DWRTX5                                                           
         SPACE 1                                                                
         SR    R2,R0               LEFT POSN OF FIELD                           
         LTR   RE,R0               LENGTH OF OUTPUT                             
         BZ    DWRTX4              SKIP EXECUTE                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),RATE                                                     
DWRTX4   DS    0H                                                               
         AR    R2,R0               NEXT PLACE IN FIELD                          
         CR    R2,R1               ANY MORE ROOM LEFT?                          
         BNL   DWRTXX              NOPE                                         
         LA    R5,4(R5)            NEXT RATE                                    
         MVI   0(R2),C','          ADD COMMA                                    
         LA    R2,1(R2)                                                         
         BCT   R3,DWRTX2                                                        
         SPACE 1                                                                
DWRTX5   DS    0H                                                               
         BCTR  R2,0                                                             
         CLI   0(R2),C','          REMOVE FINAL COMMA                           
         BNE   DWRTXX              *+12                                         
         MVI   0(R2),C' '                                                       
         B     *-14                                                             
         SPACE 1                                                                
DWRTX    L     R6,AIO                                                           
         USING RPRPKEY,R6                                                       
         CLC   RPRPKPLN,=X'FFFF'                                                
         BE    DWRTXX                                                           
         MVC   LCD,RPRPKPLN                                                     
         DROP  R6                                                               
         SPACE 1                                                                
DWRTXX   L     R6,AIO                                                           
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
DWOV1    BAS   RE,NEXTEL                                                        
         BNE   DWOVX                                                            
         SPACE 1                                                                
         USING RAVLOEL,R6                                                       
         CLI   RAVLOTYP,1                                                       
         BNE   DWOV20                                                           
         LA    RF,LDAY                                                          
         MVI   LDAYO,C'*'                                                       
         MVC   LDAY,SPACES                                                      
         B     DWOV40                                                           
         SPACE 1                                                                
DWOV20   CLI   RAVLOTYP,2                                                       
         BNE   DWOV30                                                           
         LA    RF,LTIM                                                          
         MVI   LTIMO,C'*'                                                       
         MVC   LTIM,SPACES                                                      
         B     DWOV40                                                           
         SPACE 1                                                                
DWOV30   CLI   RAVLOTYP,3                                                       
         BNE   DWOV1                                                            
         LA    RF,LTTL                                                          
         MVI   LTTLO,C'*'                                                       
         MVC   LTTL(27),SPACES                                                  
         SPACE 1                                                                
DWOV40   ZIC   RE,RAVLOLEN                                                      
         SH    RE,=H'4'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),RAVLODTA                                                 
         B     DWOV1                                                            
         DROP  R6                                                               
*   NOW FILL IN THE SCREEN LINE                                                 
         SPACE 1                                                                
DWOVX    SR    RE,RE                                                            
         L     R2,SAVER2                                                        
         MVC   8(L'LLNUM,R2),LLNUM                                              
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(L'LRAT,R2),LRAT                                                
         OI    4(R2),X'20'                                                      
         CLI   ETYPE,C'A'                                                       
         BNE   DW50                                                             
         MVC   8(13,R2),LRAT       *** HARD ***                                 
*         MVC   8(L'LRAT2,R2),LRAT2                                             
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     DW60                                                             
DW50     DS    0H                                                               
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(L'LNUM,R2),LNUM                                                
         OI    4(R2),X'20'                                                      
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(L'LCD,R2),LCD                                                  
         OI    4(R2),X'20'                                                      
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
DW60     DS    0H                                                               
         MVC   8(L'LINV,R2),LINV                                                
         OI    4(R2),X'20'         FIELD VALIDATED PREVIOUSLY                   
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(L'LDAYO,R2),LDAYO                                              
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(L'LDAY,R2),LDAY                                                
         OI    4(R2),X'20'                                                      
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(L'LTIMO,R2),LTIMO                                              
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(L'LTIM,R2),LTIM                                                
         OI    4(R2),X'20'                                                      
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(L'LTTLO,R2),LTTLO                                              
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(L'LTTL,R2),LTTL                                                
         OI    4(R2),X'20'                                                      
         DROP  R4                                                               
         SPACE 1                                                                
DW90     IC    RE,0(R2)                                                         
         AR    R2,RE               SELECT FIELD                                 
         IC    RE,0(R2)                                                         
         AR    R2,RE               LINE NUMBER FIELD                            
         ST    R2,SAVER2                                                        
         L     R3,SAVER3                                                        
         LA    R3,5(R3)                                                         
         SPACE 1                                                                
         B     DW20                                                             
         SPACE 1                                                                
DW100    B     XIT                                                              
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE WINDOW ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VWIND    NTR1                                                                   
         SPACE 1                                                                
         LA    R2,LDESELH                                                       
         ST    R2,SAVER2           POINT TO 1ST LINE-SELECT FIELD               
         LA    R4,SELTAB           POINT TO SELECT TABLE                        
         XC    SELTAB,SELTAB                                                    
         L     R3,ITEM                                                          
         MH    R3,=H'5'                                                         
         LA    R3,ITEMTAB(R3)      POINT TO FIRST ITEM IN WINDOW                
         SPACE 1                                                                
VW10     TM    1(R2),X'02'         EXTENDED HEADER?                             
         BZ    VW12                NO                                           
         ZIC   RE,0(R2)                                                         
         AR    RE,R2               BUMP TO NEXT FIELD                           
         SH    RE,=H'8'            BACK UP TO FIELD ID NUMBER                   
         CLI   0(RE),99            ARE WE INTO THE PF KEY DIRECTORY?            
         BE    VW300               YES -- WE'RE DONE                            
VW12     EQU   *                                                                
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    VW12A               YES                                          
         CLI   0(R3),00            EMPTIED TABLE                                
         BNE   VW15                NO                                           
         SPACE 1                                                                
VW12A    EQU   *                                                                
         LR    RF,R2               RF NOW AT SEL                                
         SR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         AR    RF,RE               RF NOW AT LINE                               
         IC    RE,0(RF)                                                         
         AR    RF,RE               RF NOW AT RATE(S)                            
         IC    RE,0(RF)                                                         
         AR    RF,RE               RF NOW AT INV (AVAILS) OR # (PROPS)          
         CLI   ETYPE,C'A'                                                       
         BE    VW12B                                                            
         IC    RE,0(RF)                                                         
         AR    RF,RE               RF NOW AT CODE                               
         IC    RE,0(RF)                                                         
         AR    RF,RE               RF NOW AT INV (PROPOSALS)                    
VW12B    DS    0H                                                               
         CLI   5(RF),0             ANY NEW LINES ADDED                          
         BE    VW300                                                            
         MVI   RECNEW,C'Y'         INDICATE NEW RECORD                          
         L     RE,AIO1                                                          
         LA    RF,1000                                                          
         XCEF                                                                   
         B     VW17                                                             
         SPACE 1                                                                
VW15     EQU   *                                                                
         CLC   1(4,R3),=X'FFFFFFFF'    IF RECORD WAS DELETED                    
         BE    VW270                   JUST SKIP IT                             
         CLC   1(4,R3),=X'00000000'                                             
         BE    VW270                                                            
         SPACE 1                                                                
         L     R2,SAVER2           POINT TO SELECT FIELD                        
         MVC   RERROR,=AL2(INVALID)                                             
         CLI   5(R2),1                                                          
         BH    ERREND                                                           
         CLI   5(R2),0             NO SELECTION                                 
         BE    VW16                                                             
         MVC   SELCODE,8(R2)       SAVE SELECT CODE                             
         MVI   SCRCHANG,C'Y'       INDICATE SCREEN HAS CHANGED                  
         CLI   8(R2),C'S'          SELECT FOR CHANGE                            
         BNE   VW15D                                                            
         MVC   0(1,R4),0(R3)       SAVE LINE NUMBER IF WE HAVE ONE              
         CLI   RECNEW,C'Y'         BUT IF NEW RECORD,SAVE 'S'                   
         BNE   VW16                                                             
         MVI   0(R4),C'S'          & REPLACE WITH LINE NUMBER LATER             
         B     VW16                                                             
         SPACE 1                                                                
VW15D    CLI   8(R2),C'D'          DELETE                                       
         BNE   ERREND                                                           
         BAS   RE,DELPRAV                                                       
         MVI   RECCHANG,C'Y'                                                    
         MVC   1(4,R3),=X'FFFFFFFF'  MARK ITEM DELETED IN ITEMTAB               
         B     VW260               NEXT TABLE ENTRY                             
         SPACE 1                                                                
VW16     MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),1(R3)                                                  
         GOTO1 GETREC              GET OLD DETAIL RECORD                        
         SPACE 1                                                                
VW17     MVC   PRPAVDEL,SPACES                                                  
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE RATES - OPTIONAL                                       *          
*====================================================================*          
         SPACE 1                                                                
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE               MOVE TO LINE NUMBER FIELD                    
* 02NOV89      *** START ***                                                    
         XC    DETLINE,DETLINE                                                  
         IC    RE,7(R2)            GET INP LEN                                  
         LTR   RE,RE                                                            
         BZ    VW18                                                             
         BCTR  RE,0                DEC                                          
         EX    RE,*+8              EXEC PACK                                    
         B     *+10                SKIP EX'D INS                                
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         STC   RE,DETLINE                                                       
VW18     DS    0H                                                               
* 02NOV89      ***  END  ***                                                    
VWRAT    DS    0H                                                               
         CLI   ETYPE,C'A'          AVAIL?                                       
         BNE   VWRATP              NO                                           
         SPACE 1                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               POINT TO RATE FIELD                          
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VWINV                                                            
         MVI   RECCHANG,C'Y'                                                    
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(7,BLOCK),0                                    
*         MVC   RERROR,=AL2(MANYRATE)                                           
*         CLC   CNUMLENS,DMCB+4     ARE THERE MORE RATES THAN LENGTHS?          
*         BL    ERREND              YES                                         
         MVC   RERROR,=AL2(INVALID)                                             
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         SPACE 1                                                                
         XC    DRTE,DRTE                                                        
         LA    RF,BLOCK            A(SCANNER BLOCK)                             
         LA    R1,DRTE             BUILD RATE FIELD HERE                        
         SPACE 1                                                                
VR90     CLI   1(RF),0             NO SECOND HALF ALLOWED                       
         BNE   ERREND                                                           
         CLI   0(RF),0             ANY RATE GIVEN FOR THIS LENGTH?              
         BE    *+18                NO                                           
         TM    2(RF),X'80'         YES -- IS IT NUMERIC?                        
         BNO   ERREND              NO                                           
*                                                                               
*- CHECK FOR MAX RATE VALUE (999,999)                                           
         CLC   =F'999999',4(RF)                                                 
         BL    ERREND                                                           
*                                                                               
         MVC   0(4,R1),4(RF)       YES -- PUT RATE IN FIELD                     
         SPACE 1                                                                
         LA    R1,4(R1)                                                         
         LA    RF,32(RF)           BUMP TO NEXT ENTRY                           
         BCT   R0,VR90                                                          
         B     VWINV                                                            
         SPACE 1                                                                
VWRATP   DS    0H                                                               
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               POINT TO RATE FIELD                          
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VWRAT2X                                                          
         MVI   RECCHANG,C'Y'                                                    
         XC    DRTE,DRTE                                                        
         CLI   5(R2),0                                                          
         BNE   *+8                                                              
         B     VWRAT2X                                                          
         SPACE 1                                                                
         MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         STCM  R0,15,DRTE                                                       
         SPACE 1                                                                
*====================================================================*          
*    IF EITHER NUMBER OR CODE FIELD CHANGES, THEN BOTH MUST          *          
*    BE REVALIDATED                                                  *          
*====================================================================*          
         SPACE 1                                                                
VWRAT2X  ZIC   RE,0(R2)                                                         
         AR    R2,RE               POINT TO NUMBER FIELD                        
         SR    RE,RE                                                            
         LR    RF,R2                                                            
         TM    4(RF),X'20'         NUMBER FIELD                                 
         BZ    VWREVAL                                                          
         IC    RE,0(RF)                                                         
         AR    RF,RE                                                            
         TM    4(RF),X'20'         CODE FIELD                                   
         BZ    VWREVAL                                                          
         LR    R2,RF                                                            
         B     VWINV                                                            
         SPACE 1                                                                
VWREVAL  LR    RF,R2               POINT BACK TO NUMBER FIELD                   
         NI    4(RF),X'DF'         TURN OFF PREVIOUSLY VALIDATED                
         IC    RE,0(RF)                                                         
         AR    RF,RE                                                            
         NI    4(RF),X'DF'         CODE FIELD                                   
         MVI   RECCHANG,C'Y'       AND INDICATE RECORD IS CHANGED               
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE # FIELD - REQUIRED FOR PROPOSALS                       *          
*                       NOT ALLOWED FOR AVAILS                       *          
*   VALID INPUT IS #W, FOR NUMBER OF TIMES PER WEEK                  *          
*                  #X, FOR NUMBER OF TIMES PER PROGRAM               *          
*                  #T, FOR NUMBER OF TIMES IN TOTAL FOR THE PACKAGE  *          
*                      (IF #T, THEN CODE FIELD IS REQUIRED)          *          
*====================================================================*          
         SPACE 1                                                                
VWNUM    MVC   RERROR,=AL2(MISSING)                                             
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVC   RERROR,=AL2(INVALID)                                             
         CLI   5(R2),1                                                          
         BE    ERREND                                                           
         SPACE 1                                                                
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         LA    RF,8(RE,R2)                                                      
         MVC   DNC,0(RF)        THE LAST CHAR IN FIELD MUST BE W,X OR T         
         SPACE 1                                                                
         XC    CBLOCK,CBLOCK                                                    
         MVC   CBLOCK(8),0(R2)     SET UP PHONEY FIELD TO GET RID OF IT         
         STC   RE,CBLOCK+5         ADJUST LENGTH                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CBLOCK+8(0),8(R2)                                                
         LA    R5,CBLOCK                                                        
         SPACE 1                                                                
         LA    RF,8(R5)                                                         
         LA    R1,1(RE)            GET LENGTH OF FIELD IN R1                    
VWNUM40  CLI   0(RF),C'0'          AND VALIDATE FOR NUMERIC                     
         BL    VWTVAL                                                           
         CLI   0(RF),C'9'                                                       
         BH    VWTVAL                                                           
         LA    RF,1(RF)                                                         
         BCT   R1,VWNUM40                                                       
         SPACE 1                                                                
         MVC   RERROR,=AL2(INVALID)                                             
         ZAP   DUB,=P'0'                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R5)                                                      
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         SPACE 1                                                                
         CLI   DNC,C'W'            NUMBER PER WEEK                              
         BE    VWNUM60                                                          
         CLI   DNC,C'X'            NUMBER PER PROGRAM                           
         BE    VWNUM60                                                          
         MVI   DNUM,0                                                           
         CLI   DNC,C'T'            TOTAL NUMBER FOR PACKAGE                     
         BNE   VWTVAL                                                           
         STC   R0,PSPT                                                          
         B     VWNUMX                                                           
VWNUM60  STC   R0,DNUM             SAVE NUMBER                                  
         B     VWNUMX                                                           
VWTVAL   MVC   RERROR,=AL2(BADNUM)                                              
         B     ERREND                                                           
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE CODE FIELD - REQUIRED IF # FIELD WAS #T,               *          
*                    ELSE OPTIONAL                                   *          
*          INPUT TO THIS FIELD SETS UP THIS LINE AS A PACKAGE        *          
*====================================================================*          
         SPACE 1                                                                
VWNUMX   L     R6,AIO1                                                          
         USING RPRPREC,R6                                                       
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               POINT TO CODE FIELD                          
         MVC   CODE,8(R2)                                                       
         CLI   DNC,C'T'                                                         
         BE    VWCOD10                                                          
         CLI   5(R2),0                                                          
         BNE   VWCOD20                                                          
         MVC   RPRPKPLN,=X'FFFF'                                                
         B     VWINV                                                            
VWCOD10  MVC   RERROR,=AL2(MISSING)                                             
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
VWCOD20  MVC   RPRPKPLN+1(1),DNC                                                
         MVC   RPRPKPLN(1),8(R2)                                                
         DROP  R6                                                               
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE INVENTORY NUMBER (REQUIRED FOR SOURCE I)               *          
*                              (NOT ALLOWED FOR SOURCE S)            *          
*====================================================================*          
         SPACE 1                                                                
VWINV    DS    0H                                                               
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               MOVE TO INVENTORY NUMBER FIELD               
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VWINVX                                                           
         MVI   RECCHANG,C'Y'                                                    
         SPACE 1                                                                
         CLI   ESOURCE,C'I'                                                     
         BE    VW20                                                             
         DC    H'0'                MUST BE 'I'                                  
*         MVC   RERROR,=AL2(INVALID)                                            
*         CLI   5(R2),0                                                         
*         BE    VWINVX                                                          
*         BNE   ERREND                                                          
VW20     DS    0H                                                               
* 02NOV89      *** START ***                                                    
         OC    DETLINE,DETLINE                                                  
         BZ    VW22                                                             
         CLC   8(3,R2),=C'TX='     TEXT ENTRY?                                  
         BE    VW21                                                             
         CLC   8(3,R2),=C'BK='     BOOK ENTRY?                                  
         BNE   VW22                                                             
VW21     GOTO1 =A(PUTTEXT),DMCB,(RC),RR=RELO                                    
         B     VWINVX                                                           
* 02NOV89      ***  END  ***                                                    
VW22     DS    0H                                                               
         GOTO1 ANY                                                              
         XC    CBLOCK,CBLOCK                                                    
         GOTO1 VALIINV                                                          
         MVC   DINV,CBLOCK                                                      
         MVC   DATE,CBLOCK+3                                                    
         MVC   DSAT,CBLOCK+6                                                    
         CLI   DSAT,X'F0'                                                       
         BH    VW30                                                             
         MVI   DSAT,0                                                           
VW30     EQU   *                                                                
* OR DO SAME FOR AVAILS                                                         
         SPACE 1                                                                
         CLC   DINV(3),=C'MAN'     IF 'MAN'                                     
         BNE   VWINVX                                                           
         L     R6,AIO                                                           
         MVC   ELCODE,5                                                         
         BAS   RE,GETEL            NO UPGRADES ALLOWED                          
         BNE   *+14                                                             
         MVC   RERROR,=AL2(MANINV2)                                             
         B     ERREND                                                           
         SPACE 1                                                                
         OC    CBOOKS,CBOOKS       AND MUST HAVE BOOKS IN HEADER                
         BNZ   *+14                                                             
         MVC   RERROR,=AL2(MANINV3)                                             
         B     ERREND                                                           
         SPACE 1                                                                
         LR    RF,R2         & MAKE SURE TO REVALIDATE DAY/TIME/TITLE           
         SR    RE,RE                                                            
         SPACE 1                                                                
         IC    RE,0(RF)                                                         
         AR    RF,RE               DAY OVERRIDE FIELD                           
         IC    RE,0(RF)                                                         
         AR    RF,RE               DAY FIELD                                    
         NI    4(RF),X'DF'         TURN OFF PREVIOUSLY VALIDATED                
         SPACE 1                                                                
         IC    RE,0(RF)                                                         
         AR    RF,RE               TIME OVERRIDE FIELD                          
         IC    RE,0(RF)                                                         
         AR    RF,RE               TIME FIELD                                   
         NI    4(RF),X'DF'         TURN OFF PREVIOUSLY VALIDATED                
         SPACE 1                                                                
         IC    RE,0(RF)                                                         
         AR    RF,RE               TITLE OVERRIDE FIELD                         
         IC    RE,0(RF)                                                         
         AR    RF,RE               TITLE FIELD                                  
         NI    4(RF),X'DF'         TURN OFF PREVIOUSLY VALIDATED                
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE DAY                                                    *          
*====================================================================*          
         SPACE 1                                                                
VWINVX   ZIC   RE,0(R2)                                                         
         AR    R2,RE               DAY OVERRIDE FIELD                           
         IC    RE,0(R2)                                                         
         AR    R2,RE               POINT TO DAY FIELD                           
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VWDAYX                                                           
         SPACE 1                                                                
         CLC   8(L'LDAY,R2),HYPHENS                                             
         BE    VWDAYX              THIS IS NOT AN OVERRIDE                      
         SPACE 1                                                                
         MVI   RECCHANG,C'Y'                                                    
         CLI   RECNEW,C'Y'                                                      
         BNE   VWDAY20                                                          
         CLC   DINV(3),=C'MAN'     FIELD IS REQUIRED FOR MANUAL INV             
         BE    VWDAY20                                                          
         CLI   5(R2),0             ELSE OPTIONAL FOR NEW RECORD                 
         BE    VWDAYX                                                           
         SPACE 1                                                                
VWDAY20  MVC   RERROR,=AL2(MISSING)                                             
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVC   RERROR,=AL2(INVALID)                                             
         ZIC   R5,5(R2)            LENGTH OF EXPRESSION                         
         GOTO1 DAYVAL,DMCB,((R5),8(R2)),WORK,WORK+10                            
         CLI   WORK,0                                                           
         BE    ERREND                                                           
         MVI   BYTE,X'01'                                                       
         BAS   RE,BLDOVRD                                                       
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE TIME                                                   *          
*====================================================================*          
         SPACE 1                                                                
VWDAYX   ZIC   RE,0(R2)                                                         
         AR    R2,RE               TIME OVERRIDE FIELD                          
         IC    RE,0(R2)                                                         
         AR    R2,RE               POINT TO TIME FIELD                          
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VWTIMX                                                           
         SPACE 1                                                                
         CLC   8(L'LTIM,R2),HYPHENS                                             
         BE    VWTIMX              THIS IS NOT AN OVERRIDE                      
         SPACE 1                                                                
         MVI   RECCHANG,C'Y'                                                    
         CLI   RECNEW,C'Y'                                                      
         BNE   VWTIM20                                                          
         CLC   DINV(3),=C'MAN'     FIELD IS REQUIRED FOR MANUAL INV             
         BE    VWTIM20                                                          
         CLI   5(R2),0             ELSE OPTIONAL FOR NEW RECORD                 
         BE    VWTIMX                                                           
         SPACE 1                                                                
VWTIM20  MVC   RERROR,=AL2(MISSING)                                             
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVC   RERROR,=AL2(INVALID)                                             
         ZIC   R5,5(R2)                                                         
         LA    RE,6(R5,R2)                                                      
         CLC   0(2,RE),=C',B'                                                   
         BNE   *+8                                                              
         SH    R5,=H'2'                                                         
         GOTO1 TIMVAL,DMCB,((R5),8(R2)),WORK                                    
         CLI   DMCB,X'FF'                                                       
         BE    ERREND                                                           
         MVI   BYTE,X'02'                                                       
         BAS   RE,BLDOVRD                                                       
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE TITLE                                                  *          
*====================================================================*          
         SPACE 1                                                                
VWTIMX   ZIC   RE,0(R2)                                                         
         AR    R2,RE               TITLE OVERRIDE FIELD                         
         IC    RE,0(R2)                                                         
         AR    R2,RE               POINT TO TITLE FIELD                         
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VWTITX                                                           
         SPACE 1                                                                
         CLC   8(L'LTTL,R2),HYPHENS                                             
         BE    VWTITX              THIS IS NOT AN OVERRIDE                      
         SPACE 1                                                                
         MVI   RECCHANG,C'Y'                                                    
         CLI   RECNEW,C'Y'                                                      
         BNE   VWTTL20                                                          
         CLC   DINV(3),=C'MAN'     FIELD IS REQUIRED FOR MANUAL INV             
         BE    VWTTL20                                                          
         CLI   5(R2),0             ELSE OPTIONAL FOR NEW RECORD                 
         BE    VWTITX                                                           
         SPACE 1                                                                
VWTTL20  MVC   RERROR,=AL2(MISSING)                                             
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVI   BYTE,X'03'                                                       
         BAS   RE,BLDOVRD                                                       
         SPACE 1                                                                
*====================================================================*          
*    NOW INDICATE ALL FIELDS HAVE BEEN VALIDATED                     *          
*====================================================================*          
         SPACE 1                                                                
VWTITX   DS    0H                                                               
         SR    RE,RE                                                            
         L     R2,SAVER2           POINT R2 TO START OF THIS LINE               
         L     RF,SAVER2                                                        
         LA    R0,12               ** HARD ** NO. OF FLDS ON PRP LINE           
         CLI   ETYPE,C'P'                                                       
         BE    *+8                                                              
         LA    R0,10               ** HARD ** NO. OF FLDS ON AVAIL LINE         
         SR    R1,R1                                                            
         SPACE 1                                                                
         IC    R1,0(RF)            BUMP TO NEXT SELECT                          
         AR    RF,R1                                                            
         BCT   R0,*-6                                                           
         SPACE 1                                                                
VW80     CR    R2,RF                                                            
         BNL   VW90                                                             
         OI    4(R2),X'20'         INDICATE VALIDATED                           
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     VW80                                                             
         EJECT                                                                  
*====================================================================*          
*    NOW ADD OR CHANGE ELEMENTS                                      *          
*====================================================================*          
         SPACE 1                                                                
VW90     CLC   PRPAVDEL,SPACES     DO WE NEED NEW 01 ELEMENT?                   
         BE    VW150               NO                                           
*                                                                               
         CLI   DSAT,C' '           REPLACE BLANK WITH NULL                      
         BNE   VW95                                                             
         MVI   DSAT,0                                                           
VW95     EQU   *                                                                
         LA    R5,ELEM                                                          
         CLI   ETYPE,C'P'                                                       
         BE    VW110                                                            
         SPACE 1                                                                
         CLI   RECNEW,C'Y'         NEW RECORD                                   
         BNE   VW100                                                            
         SPACE 1                                                                
         BAS   RE,GETLNUM          GET NEXT LINE NUMBER                         
         XC    ELEM,ELEM                                                        
         USING RAVLDEL,R5                                                       
         MVI   RAVLDCOD,X'01'                                                   
         MVI   RAVLDLEN,RAVLDLLQ                                                
         B     VW105                                                            
         SPACE 1                                                                
VW100    L     R6,AIO              CHANGED RECORD                               
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    ELEM,ELEM                                                        
         ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R6)       COPY OLD ELEMENT                             
         MVI   0(R6),X'FF'         AND MARK OLD FOR DELETION                    
         SPACE 1                                                                
VW105    CLC   DINV(3),SPACES                                                   
         BE    VW105A                                                           
         CLC   DINV(3),=X'000000'                                               
         BE    VW105A                                                           
         MVC   RAVLDINV,DINV                                                    
         MVC   RAVLDATE,DATE                                                    
         MVC   RAVLDSAT,DSAT                                                    
VW105A   EQU   *                                                                
*         LA    R1,DRTE                                                         
*         CLC   0(4,R1),SPACES      RATES                                       
*         BE    VW106A                                                          
         CLC   DRTE,SPACES                                                      
         BE    VW106B                                                           
         MVC   RAVLDRTE,DRTE                                                    
*VW106A   EQU   *                                                               
*         CLC   DRTE+4(4),SPACES    RATE 2                                      
*         BE    VW106B                                                          
*         MVC   RAVLDRTE+4(4),DRTE+4                                            
VW106B   EQU   *                                                                
         DROP  R5                                                               
         B     VW140                                                            
         SPACE 1                                                                
VW110    CLI   RECNEW,C'Y'         NEW RECORD                                   
         BNE   VW120                                                            
         SPACE 1                                                                
         BAS   RE,GETLNUM          GET NEXT LINE NUMBER                         
         XC    ELEM,ELEM                                                        
         USING RPRPDEL,R5                                                       
         MVI   RPRPDCOD,X'01'                                                   
         MVI   RPRPDLEN,RPRPDELQ                                                
         B     VW130                                                            
         SPACE 1                                                                
VW120    L     R6,AIO              CHANGED RECORD                               
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    ELEM,ELEM                                                        
         ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R6)       COPY OLD ELEMENT                             
         MVI   0(R6),X'FF'         AND MARK OLD FOR DELETION                    
         SPACE 1                                                                
VW130    CLC   DINV(3),SPACES                                                   
         BE    VW130A                                                           
         CLC   DINV(3),=X'000000'                                               
         BE    VW130A                                                           
         MVC   RPRPDINV,DINV                                                    
         MVC   RPRPDATE,DATE                                                    
         MVC   RPRPDSAT,DSAT                                                    
VW130A   EQU   *                                                                
         CLC   DRTE(4),SPACES                                                   
         BE    VW131A                                                           
         MVC   RPRPDRTE,DRTE                                                    
VW131A   EQU   *                                                                
         CLC   DNUM(2),SPACES                                                   
         BE    VW131B                                                           
         MVC   RPRPDNUM,DNUM                                                    
         MVC   RPRPDNC,DNC                                                      
VW131B   EQU   *                                                                
         DROP  R5                                                               
         SPACE 1                                                                
VW140    GOTO1 ADDELEM             ADD X'01' ELEMENT                            
         SPACE 3                                                                
VW150    MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             GET RID OF ANY DELETED ELEMENTS              
         CLI   RECCHANG,C'Y'                                                    
         BNE   VW260                                                            
         EJECT                                                                  
*====================================================================*          
* FOR PROPOSALS AND AVAILS, DO RECORD ADD/PUTS HERE (BECAUSE OF      *          
* COMPLICATIONS WITH PACKAGES - SKIP GENCON)                         *          
*====================================================================*          
         SPACE 1                                                                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         MVC   DETKEY(27),0(R6)                                                 
         SPACE 1                                                                
         CLI   RECNEW,C'Y'                                                      
         BNE   VW180                                                            
         GOTO1 ADDREC                                                           
*  UPDATE ITEMTAB SO NEW LINE WILL SHOW ON REDISPLAY                            
*  ALSO UPDATE NUMBER OF ITEMS                                                  
*  (DON'T WANT TO REBUILD TABLE BECAUSE WE WANT NEW LINES TO                    
*   STAY IN POSTION AT THE END, AND NOT BE RESHUFFLED)                          
         SPACE 1                                                                
         MVC   0(1,R3),DETKEY+26   LINE NUMBER                                  
         MVC   1(4,R3),KEY         DISK ADDRESS                                 
         MVC   5(5,R3),=X'FFFFFFFFFF'   MARK NEW END OF TABLE                   
         L     RE,NUMITEMS                                                      
         LA    RE,1(RE)                                                         
         ST    RE,NUMITEMS                                                      
         B     VW250                                                            
         SPACE 1                                                                
VW180    CLI   RECCHANG,C'Y'                                                    
         BNE   VW260                                                            
         BAS   RE,CHAPRAV                                                       
         SPACE 2                                                                
VW250    CLI   ETYPE,C'A'          AVAIL?                                       
         BE    VW260                                                            
         CLI   DNC,C'T'            IF THIS HAS BEEN CHANGED TO A T PKG          
         BNE   VW260                                                            
         BAS   RE,PKG              ADD/CHANGE PACKAGE HEADER                    
         SPACE                                                                  
VW260    CLI   RECNEW,C'Y'                                                      
         BNE   VW270                                                            
         CLI   0(R4),C'S'          IF RECORD WAS SELECTED                       
         BNE   VW270                                                            
         MVC   0(1,R4),XDETNUM     REPLACE 'S' WITH LINE NUMBER                 
         SPACE 1                                                                
VW270    LA    R3,5(R3)            NEXT ITEM TABLE ENTRY                        
         LA    R4,1(R4)            NEXT SELECT TABLE ENTRY                      
         L     R2,SAVER2                                                        
         LA    R0,12               ** HARD ** NO. OF FLDS ON PRP LINE           
         CLI   ETYPE,C'P'                                                       
         BE    *+8                                                              
         LA    R0,10               ** HARD ** NO. OF FLDS ON AVAIL LINE         
         SR    RF,RF                                                            
         SPACE 1                                                                
         IC    RF,0(R2)            BUMP TO NEXT SELECT                          
         AR    R2,RF                                                            
         BCT   R0,*-6                                                           
         ST    R2,SAVER2           AND SAVE                                     
         SPACE 1                                                                
         CLI   RECCHANG,C'Y'       IF RECORD HAS CHANGED                        
         BE    VW290                                                            
         CLI   RECNEW,C'Y'         OR HAS BEEN ADDED                            
         BNE   *+8                                                              
VW290    MVI   SCRCHANG,C'Y'       INDICATE THAT SCREEN HAS CHANGED             
         SPACE 1                                                                
         MVI   RECCHANG,C'N'                                                    
         MVI   RECNEW,C'N'                                                      
         B     VW10                                                             
         SPACE 1                                                                
VW300    MVI   0(R4),X'FF'         MARK END OF SELECT TABLE                     
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*  THIS ROUTINE SAVES BOOK INFO FROM THE HEADER RECORD               *          
*--------------------------------------------------------------------*          
HDRINFO  NTR1                                                                   
         L     R6,AIO3             POINT TO HEADER RECORD                       
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         USING RAVLELEM,R6                                                      
         SPACE 1                                                                
         XC    CBOOKS,CBOOKS                                                    
         MVC   CBOOKS(L'RAVLBKS),RAVLBKS                                        
         CLI   ETYPE,C'A'          AVAILS?                                      
         BNE   XIT                 NO                                           
         SPACE 1                                                                
         SR    R5,R5               COUNT NUMBER OF LENGTHS                      
         LA    R3,RAVLRFRM         A(LENGTHS)                                   
HI30     CH    R5,=H'6'            ARE WE AT MAX OF SIX?                        
         BE    HI40                YES                                          
         CLI   1(R3),0             END OF LIST?                                 
         BE    HI40                YES                                          
         LA    R3,2(R3)            BUMP TO NEXT LENGTH IN LIST                  
         LA    R5,1(R5)            INCREMENT LENGTH COUNTER                     
         B     HI30                                                             
HI40     DS    0H                                                               
*         STC   R5,CNUMLENS         SAVE NUMBER OF LENGTHS IN HEADER            
         DROP  R6                                                               
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*   THIS ROUTINE ADDS A PACKAGE HEADER RECORD AND AN X'13' ELEMENT   *          
*     WHEN CODE IS T.  THE PACKAGE HEADER IS TO HOLD THE TOTAL NUMBER*          
*     OF SPOTS FOR THE PACKAGE. (THAT'S WHY X AND W DON'T HAVE       *          
*     HEADERS.)                                                      *          
*--------------------------------------------------------------------*          
PKG      NTR1                                                                   
         MVC   AIO,AIO2                                                         
         SPACE 1                                                                
         XC    KEY,KEY                                                          
*********L     R6,AIO1             DETAIL RECORD JUST ADDED                     
*********MVC   KEY(26),0(R6)                                                    
         MVC   KEY(26),DETKEY                                                   
         OI    DMINBTS,X'08'       READ DELETES AS WELL                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PKGNEW                                                           
         SPACE 1                                                                
         TM    KEY+27,X'80'        IS IS DELETED?                               
         BZ    PKG10               NO                                           
         NI    KEY+27,X'7F'        YES                                          
         GOTO1 WRITE               UNDELETE OLD                                 
         SPACE 1                                                                
PKG10    GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'13'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RPRPPEL,R6                                                       
         ZIC   RE,RPRPPSPT                                                      
         ZIC   RF,PSPT                                                          
         CR    RE,RF                                                            
         BE    PKGX                RECORD IS ALREADY CORRECT                    
         MVC   RPRPPSPT,PSPT                                                    
         DROP  R6                                                               
         GOTO1 PUTREC              RECORD NEEDS TO BE CHANGED                   
         B     PKGX                                                             
         SPACE 1                                                                
PKGNEW   L     R6,AIO              ADD A NEW RECORD                             
         LR    RE,R6                                                            
         LA    RF,1000                                                          
         XCEF                                                                   
         SPACE 1                                                                
         USING RPRPREC,R6                                                       
         MVC   RPRPKEY,KEYSAVE                                                  
         MVC   RPRPLEN,=H'46'      LEN OF KEY(34) + X'13' ELEM (12)             
         LA    R5,34(R6)                                                        
         USING RPRPPEL,R5                                                       
         MVC   RPRPPCOD(2),=X'130C'                                             
         MVC   RPRPPSPT,PSPT                                                    
         SPACE 1                                                                
         GOTO1 ADDREC                                                           
         DROP  R5,R6                                                            
         SPACE 1                                                                
PKGX     MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE GETS THE NEXT LINE NUMBER AVAILABLE AND BUILDS THE    *          
* NEW DETAIL KEY.                                                    *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
GETLNUM  NTR1                                                                   
         SPACE 1                                                                
         LA    R5,22               ASSUME PROPOSALS (23 - 1 FOR EX)             
         CLI   ETYPE,C'P'                                                       
         BE    *+8                                                              
         LA    R5,25               NO, IT'S AVAILS (26 - 1 FOR EX)              
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         MVI   XDETNUM,0                                                        
         XC    KEY,KEY                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   KEY(0),MYKEY                                                     
         GOTO1 HIGH                REREAD HEADER                                
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
GL45     OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 SEQ                                                              
         NI    DMINBTS,X'F7'                                                    
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE      SAME HEADER?                                 
         BNE   GL60                                                             
         CLC   XDETNUM,KEY+26                                                   
         BH    GL45                                                             
         MVC   XDETNUM,KEY+26      SAVE HIGHEST NUMBER                          
         B     GL45                                                             
         SPACE 1                                                                
GL60     XC    KEY,KEY                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   KEY(0),KEYSAVE                                                   
         ZIC   R1,XDETNUM                                                       
         LA    R1,1(R1)                                                         
         STC   R1,XDETNUM                                                       
         MVC   KEY+26(1),XDETNUM                                                
         SPACE 1                                                                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         SPACE 1                                                                
         CLI   ETYPE,C'A'          AVAIL?                                       
         BE    GLX                                                              
         MVC   KEY+23(2),=X'FFFF'  START OUT AS NON-PACKAGE                     
*********CLI   DNC,C'T'                                                         
         CLI   CODE,0              ANY CODE GIVEN?                              
         BE    *+16                NO                                           
         MVC   KEY+23(1),CODE      SET KEY FOR PACKAGE                          
         MVC   KEY+24(1),DNC                                                    
         SPACE 1                                                                
GLX      MVC   0(27,R6),KEY                                                     
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE ADDS A X'06' OVERRIDE ELEMENT ON A DETAIL RECORD      *          
* BUT FIRST IT DELETES AN OLD X'06' OF THE SAME TYPE IF NECESSARY.   *          
* THE TYPE OF OVERRIDE IS IN BYTE                                    *          
*   1=DAY, 2=TIME, 3=PROGRAM TITLE, (4=DEMOS, 7=BOOK)                *          
* R2 POINTS TO THE (VALIDATED) FIELD                                 *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
BLDOVRD  NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,6                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
BOV10    BAS   RE,NEXTEL                                                        
         BNE   BOV20                                                            
         USING RAVLOEL,R6                                                       
         CLC   RAVLOTYP,BYTE                                                    
         BNE   BOV10                                                            
         MVI   0(R6),X'FF'         MARK ELEMENT TO BE DELETED                   
         SPACE 1                                                                
BOV20    XC    ELEM,ELEM           AND REBUILD                                  
         LA    R6,ELEM                                                          
         MVI   RAVLOCOD,6                                                       
         MVC   RAVLOTYP,BYTE                                                    
         SPACE 1                                                                
         ZIC   RE,5(R2)            LENGTH OF INPUT                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RAVLODTA(0),8(R2)                                                
         LA    RE,4(RE)                                                         
         STC   RE,RAVLOLEN                                                      
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE DELETES A DETAIL RECORD.                              *          
* ON ENTRY, WE HAVE DISK ADDRESS OF DETAIL RECORD AT 1(R3) (ITEMTAB) *          
* IF RECORD WAS PART OF A 'T' PACKAGE, AND IT'S THE LAST DETAIL      *          
*    RECORD FOR THAT PACKAGE, IT DELETES THE PACKAGE HEADER ALSO.    *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
DELPRAV  NTR1                                                                   
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),1(R3)                                                  
         GOTO1 GETREC              GET OLD DETAIL RECORD                        
         SPACE 1                                                                
         L     R6,AIO1                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(27),0(R6)                                                    
         GOTO1 READ                                                             
         OI    KEY+27,X'80'        TURN ON DELETED BIT                          
         GOTO1 WRITE               AND WRITE IT BACK TO DIRECTORY               
         SPACE 1                                                                
         CLI   ETYPE,C'A'          AVAIL?                                       
         BE    PDELX               YES -- EXIT                                  
         SPACE 1                                                                
         CLI   KEY+24,C'T'         IS IT A T PACKAGE                            
         BNE   PDELX                                                            
         MVI   KEY+26,1                                                         
         GOTO1 HIGH                READ HIGH FOR OTHER DETAIL RECORDS           
         CLC   KEY(26),KEYSAVE                                                  
         BE    PDELX               THERE ARE OTHERS, SO LEAVE HEADER            
         MVC   KEY,KEYSAVE         RESTORE LAST KEY                             
         MVI   KEY+26,0            GO FOR PACKAGE HEADER                        
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   PDELX                                                            
         OI    KEY+27,X'80'        TURN ON DELETED BIT                          
         GOTO1 WRITE                                                            
PDELX    B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE CHANGES A DETAIL RECORD.                              *          
* -IF THE CHANGED RECORD DOESN'T EXIST (CHANGE IN PLAN), IT'S ADDED  *          
* -IF IT WAS DELETED, IT'S RESTORED                                  *          
* -IF RECORD WAS A PACKAGE, AND IS NOW DIFFERENT, THE OLD PKG HDR    *          
*  IS DELETED, IF THERE ARE NO MORE DETAIL LINES LEFT.               *          
*--------------------------------------------------------------------*          
         SPACE 2                                                                
CHAPRAV  NTR1                                                                   
         SPACE 1                                                                
         LA    R5,22               ASSUME PROPOSALS (23 - 1 FOR EX)             
         CLI   ETYPE,C'P'                                                       
         BE    *+8                                                              
         LA    R5,25               NO, IT'S AVAILS (26 - 1 FOR EX)              
         SPACE 1                                                                
         MVC   AIO,AIO2            OLD RECORD                                   
         XC    KEY,KEY                                                          
         MVC   KEY,DETKEY          LOOK FOR NEW DETAIL KEY                      
         OI    DMINBTS,X'08'       READ DELETES AS WELL                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(27),KEY                                                  
         BNE   PCHA6               NO KEY SO ADD RECORD                         
         SPACE 1                                                                
         MVI   RATE,0              USE RATE AS FLAG                             
         MVC   KEYSAVE,KEY         SAVE STATUS BIT                              
         TM    KEY+27,X'80'        IS IT DELETED                                
         BZ    PCHA5               NO                                           
         NI    KEY+27,X'7F'        YES                                          
         MVI   RATE,1              USE RATE AS UNDELETE FLAG                    
         GOTO1 WRITE               UNDELETE OLD                                 
         MVC   1(4,R3),KEY+28      SAVE RESTORED D/A IN ITEMTAB                 
         SPACE 1                                                                
PCHA5    GOTO1 GETREC              GET OLD RECORD (FOR DMGR SEQUENCE)           
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC              PUT NEW RECORD                               
*         TM    KEYSAVE+27,X'80'    DID WE DO A RESTORE?                        
*         BZ    PCHAX               NO                                          
         CLI   RATE,0                                                           
         BE    PCHAX                                                            
         B     PCHA7                                                            
         SPACE 1                                                                
PCHA6    MVC   AIO,AIO1                                                         
         GOTO1 ADDREC                                                           
         MVC   1(4,R3),KEY         SAVE NEW D/A IN ITEMTAB                      
         SPACE 1                                                                
PCHA7    XC    KEY,KEY             LOOK FOR THE ACTIVE POINTER                  
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   KEY(0),DETKEY                                                    
         GOTO1 HIGH                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                SHOULD FIND PROPOSAL HEADER                  
         SPACE 1                                                                
PCHA8    GOTO1 SEQ                                                              
         SPACE 1                                                                
         CLC   KEY(27),DETKEY      THIS IS THE ONE I JUST ADDED                 
         BE    PCHA8                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                THERE IS NO OLD POINTER                      
         SPACE 1                                                                
         CLC   KEY+26(1),DETKEY+26 DOES LINE NUMBER MATCH?                      
         BNE   PCHA8               NO -- KEEP LOOKING                           
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE               DETAIL OLD KEY                               
         SPACE 1                                                                
         CLI   ETYPE,C'A'          AVAIL?                                       
         BE    PCHAX               YES -- EXIT                                  
         CLI   KEY+24,C'T'         OLD WAS NOT A PACKAGE WITH HEADER            
         BNE   PCHAX                                                            
         MVI   KEY+26,1                                                         
         GOTO1 HIGH                ANY MORE DETAILS                             
         CLC   KEYSAVE(26),KEY                                                  
         BE    PCHAX               YES, SO LEAVE PACKAGE HEADER                 
         SPACE 1                                                                
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+26,0            GET PACKAGE HEADER RECORD                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   PCHAX                                                            
         OI    KEY+27,X'80'        AND DELETE IT                                
         GOTO1 WRITE                                                            
         SPACE 1                                                                
PCHAX    B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE BUILD A TABLE OF DETAIL NUMBERS AND DISK ADDRESSESS   *          
* AND SAVES THE TOTAL NUMBER OF ITEMS IN THE ITEM TABLE.             *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
BLDITAB  NTR1                                                                   
         LA    RE,ITEMTAB               CLEAR ITEM KEY LIST                     
         LA    RF,1280             255 ITEMS @ 5 BYTES, + EOT                   
         XCEF                                                                   
*                                                                               
         LA    R4,ITEMTAB          POINT R4 TO ITEM KEY LIST                    
*                                                                               
         SR    R3,R3               CLEAR NUMBER OF ITEMS                        
*                                                                               
         CLI   MYKEY,0             KEY SAVED?                                   
         BE    BL100               NO, GET OUT                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),MYKEY       HEADER KEY                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    BL50                                                             
         DC    H'0'                                                             
*                                                                               
BL10     CLI   ETYPE,C'P'                                                       
         BE    *+18                                                             
         CLC   KEY(26),MYKEY       SAME AVAIL?                                  
         BNE   BL100               NO -- WE'RE DONE                             
         B     BL20                                                             
         SPACE 1                                                                
         CLC   KEY(23),MYKEY       SAME PROPOSAL?                               
         BNE   BL100               NO -- WE'RE DONE                             
         CLI   KEY+26,0            SKIP PACKAGE HEADER RECORDS                  
         BE    BL50                                                             
*                                  BUILD TABLE ENTRY -                          
BL20     MVC   0(1,R4),KEY+26         DETAIL LINE NUMBER                        
         MVC   1(4,R4),KEY+28         DISK ADDRESS                              
*                                                                               
         LA    R3,1(R3)            INCREMENT NUMBER OF ITEMS                    
*                                                                               
         LA    R4,5(R4)            BUMP TO NEXT ITEM KEY                        
*                                                                               
BL50     MVI   RDUPDATE,C'N'       READ NEXT DETAIL RECORD                      
         GOTO1 SEQ                                                              
         B     BL10                                                             
*                                                                               
BL100    ST    R3,NUMITEMS         SAVE TOTAL NUMBER OF ITEMS                   
         MVC   0(5,R4),=X'FFFFFFFFFF' MARK END OF TABLE                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* CALL 'DETAIL CHANGE' FOR FIRST LINE NUMBER FOUND IN SELECT TABLE   *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
TESTSEL  NTR1                                                                   
         LA    R4,SELTAB                                                        
         SPACE 1                                                                
TS10     CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    TSX                                                              
         CLI   0(R4),0             IF ANY LINE NUMBER                           
         BE    TS20                                                             
         EDIT  (1,(R4)),(3,EDETNUM),ALIGN=LEFT                                  
         MVI   0(R4),0             CLEAR LINE NUMBER                            
         CLI   ETYPE,C'P'          TEST PROPOSALS                               
         BNE   TS15                NO                                           
         GOTO1 CALLPROG,BLOCK,(C'Y',0),=C'PDETAIL',=C'CHANGE',0,       X        
               (8,ECONNUM),(3,EHDRNUM),(3,EDETNUM),0                            
*              (8,ECONNUM),(1,ESOURCE),(3,EHDRNUM),(3,EDETNUM),0                
TS15     GOTO1 CALLPROG,BLOCK,(C'Y',0),=C'ADETAIL',=C'CHANGE',0,       X        
               (8,ECONNUM),(3,EHDRNUM),(3,EDETNUM),0                            
*              (8,ECONNUM),(1,ESOURCE),(3,EHDRNUM),(3,EDETNUM),0                
         SPACE 1                                                                
TS20     LA    R4,1(R4)            ELSE BUMP TO NEXT SELTAB ENTRY               
         B     TS10                                                             
         SPACE 1                                                                
TSX      B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE SETS ITEM NUMBER FOR TOP OF DISPLAY FROM STARTAT      *          
* LINE NUMBER.  IF THE STARTAT IS NOT FOUND, THEN DISPLAY STARTS     *          
* AT THE TOP.                                                        *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
SETITEM  NTR1                                                                   
         LA    R4,ITEMTAB          POINT R4 TO ITEM KEY LIST                    
         SR    R3,R3               CLEAR ITEM NUMBER                            
*                                                                               
         CLC   NUMITEMS,=F'2'      IF NUMBER OF ITEMS < 2 THEN DONE             
         BL    SI100                                                            
*                                                                               
SI10     CLC   0(1,R4),STARTAT     ELSE IF ITEM = STARTAT THEN DONE             
         BE    SI100                                                            
         LA    R4,5(R4)            BUMP TO NEXT ITEM                            
         CLI   0(R4),X'FF'         IF END OF ITEM LIST THEN                     
         BNE   *+10                                                             
         SR    R3,R3               TOP OF DISPLAY IS TOP OF TABLE               
         B     SI100                                                            
         LA    R3,1(R3)            INCREMENT ITEM NUMBER                        
         B     SI10                NEXT ITEM                                    
*                                                                               
SI100    ST    R3,ITEM             SAVE ITEM NUMBER                             
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE CHANGES THE ITEM NUMBER FOR TOP OF DISPLAY FROM       *          
* THE PF KEY                                                         *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
CHGITEM  NTR1                                                                   
         L     R3,ITEM             R3 = ITEM                                    
         L     R4,NUMITEMS         R4 = #ITEMS - 1                              
         BCTR  R4,0                                                             
*                                                                               
         CLI   PFKEY,PFTOP         IF PFKEY = TOP                               
         BNE   *+14                                                             
         BAS   RE,BLDITAB          THEN BUILD NEW ITEM TABLE                    
         SR    R3,R3               AND ITEM = 0                                 
         B     CI100                                                            
*                                                                               
         CLI   PFKEY,PFBOTTOM      ELSE IF PFKEY = END                          
         BNE   *+10                                                             
         LR    R3,R4               THEN ITEM = #ITEMS - 1                       
         B     CI100                                                            
*                                                                               
         CLI   PFKEY,PFPGUP        ELSE IF PFKEY = PGUP                         
         BNE   *+12                                                             
         S     R3,NUMWIND          THEN ITEM = ITEM - NUMWIND                   
         B     CI100                                                            
*                                                                               
         CLI   PFKEY,PFPGDN        ELSE IF PFKEY = PGDN                         
         BNE   CI100                                                            
         A     R3,NUMWIND          THEN ITEM = ITEM + NUMWIND                   
*                                                                               
CI100    CR    R3,R4               IF ITEM > #ITEMS - 1                         
         BNH   *+6                                                              
         LR    R3,R4               THEN ITEM = #ITEMS - 1                       
*                                                                               
         LTR   R3,R3               IF ITEM < 0                                  
         BNM   *+6                                                              
         SR    R3,R3               THEN ITEM = 0                                
*                                                                               
         ST    R3,ITEM             SAVE NEW ITEM NUMBER                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE CLEARS ALL THE FIELDS ON THE SCREEN (EXCEPT PF KEYS)  *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
CLEARREC NTR1                                                                   
         LA    R2,LDESELH                                                       
CR10     TM    1(R2),X'02'         EXTENDED HEADER?                             
         BZ    CR20                NO                                           
         ZIC   R5,0(R2)                                                         
         AR    R5,R2               BUMP TO NEXT FIELD                           
         SH    R5,=H'8'            BACK UP TO FIELD ID NUMBER                   
         CLI   0(R5),99            ARE WE INTO THE PF KEY DIRECTORY?            
         BE    XIT                 YES -- WE'RE DONE                            
         SPACE 1                                                                
CR20     BAS   RE,CLEAR                                                         
         NI    4(R2),X'DF'         TURN OFF PREVIOUSLY VALIDATED                
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               GET TO NEXT FIELD                            
         B     CR10                                                             
         SPACE 3                                                                
*--------------------------------------------------------------------*          
* THIS ROUTINE CLEARS OUT A FIELD                                    *          
* ON ENTRY, R2 POINTS TO FIELD HEADER                                *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
CLEAR    ZIC   R1,0(R2)            GET LENGTH OF FIELD                          
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'         EXTENDED HEADER                              
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         SPACE 1                                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         BR    RE                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
ERREND   GOTO1 MYERROR             DOES A GETTXT CALL                           
         SPACE 2                                                                
HYPHENS  DC    (L'LTTL)C'-'                                                     
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* 02NOV89      *** START ***                                                    
*                                                                               
PUTTEXT  NMOD1 0,**PTTX**                                                       
         L     RC,0(R1)                                                         
         SPACE 1                                                                
         XC    SCBLOCK,SCBLOCK                                                  
         LA    R6,KEY                                                           
         CLI   ETYPE,C'P'          CK FOR PROPS...                              
         BNE   PT20                                                             
*                                                                               
* *** HARD *** - START                                                          
         USING RPRPREC,R6                                                       
         MVC   RPRPKPLN,=X'FFFF'   DEFAULT                                      
         LR    R1,R2                                                            
         SH    R1,=H'20'           R1 PTS AT #-HEADER                           
         CLI   19(R1),C' '         IS THERE A CODE?                             
         BE    PT10                NO, USE DEFAULTS                             
         CLI   19(R1),X'00'                                                     
         BE    PT10                                                             
         TM    15(R1),X'20'        NOT CHANGED?                                 
         BZ    PTERR               CAN'T CHANGE WHEN USING TX=                  
         MVC   RPRPKPLN(1),19(R1)  SAVE CODE IN KEY...                          
*                                                                               
         TM    4(R1),X'20'         # MUST BE VALID...                           
         BZ    PTERR                                                            
         CLI   9(R1),C'0'          IS C'TXW' CHAR 2 OR 3?                       
         BNL   *+14                                                             
         MVC   RPRPKPLN+1(1),9(R1)                                              
         B     *+10                                                             
         MVC   RPRPKPLN+1(1),10(R1)                                             
* *** HARD *** - FINISH                                                         
*                                                                               
PT10     MVI   RPRPKTYP,X'16'      BUILD PROP HEADER KEY                        
         MVC   RPRPKREP,AGENCY                                                  
         MVC   RPRPKCON,CCONNUM                                                 
         MVC   RPRPKPRP,XHDRNUM                                                 
         MVC   RPRPKSRC,ESOURCE                                                 
         MVC   RPRPKDET,DETLINE                                                 
         B     PT30                                                             
         DROP  R6                                                               
*                                                                               
PT20     DS    0H                                                               
         USING RAVLREC,R6                                                       
         MVI   RAVLKTYP,X'14'      BUILD AVAIL HEADER KEY                       
         MVC   RAVLKREP,AGENCY                                                  
         MVC   RAVLKCON,CCONNUM                                                 
         MVC   RAVLKAVN,XHDRNUM                                                 
         MVC   RAVLKSRC,ESOURCE                                                 
         MVC   RAVLKDET,DETLINE                                                 
         DROP  R6                                                               
*                                                                               
PT30     GOTO1 READ                READ HEADER REC                              
         CLC   KEY,KEYSAVE         BETTER BE...                                 
         BE    *+6                                                              
         DC    H'0'                REC NOT FOUND                                
         GOTO1 GETREC                                                           
* SCAN INPUT TO SAVE NEW DATA                                                   
PT40     DS    0H                                                               
         CLI   8(R2),C'B'          'BK='?                                       
         BE    PT200                                                            
         MVC   INPUT,SPACES                                                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   INPUT(0),8(R2)                                                   
*                                                                               
         LA    R4,INPUT                                                         
         LA    R4,3(R4)            SKIP 'TX='                                   
         SH    R1,=H'3'                                                         
         EX    R1,*+12                                                          
         BNE   PT50                                                             
         B     *+10                                                             
         CLC   INPUT+3(0),=C'DELETE    '                                        
*                                                                               
         MVI   ELCODE,X'08'                                                     
         GOTO1 REMELEM             ADINGDONG                                    
         B     PT130                                                            
         EJECT                                                                  
* ADDNUMS                                                                       
PT50     L     R6,AIO              GET X'08' (TEXT) ELEMENT                     
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL            IF COMMENT ALREADY EXISTS, DELETE            
         BNE   PT60                                                             
         MVI   ELCODE,X'08'                                                     
         GOTO1 REMELEM             ADINGDONG                                    
* SCAN INPUT DATA                                                               
PT60     DS    0H                                                               
         CLC   INPUT+3(10),=C'ALL       '                                       
         BNE   PT70                                                             
         MVC   DUMMY(5),=C'T=ALL'                                               
         MVI   DUMMYHL,5                                                        
         B     PT100                                                            
*                                                                               
PT70     DS    0H                                                               
         GOTO1 SCANNER,DMCB,(C'C',(R4)),SCBLOCK                                 
         CLI   DMCB+4,0                                                         
         BE    PTERR                                                            
         ZIC   R5,DMCB+4           NUMBER OF INPUT NUMS                         
         LA    R4,DUMMY                                                         
         LA    R3,SCBLOCK                                                       
         XC    DUMMYHL,DUMMYHL                                                  
         B     PT90                NO COMMA FOR FIRST...                        
* START LOOP                                                                    
PT80     DS    0H                                                               
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)            LENGTH OF COMMA                              
         IC    R6,DUMMYHL          UPDATE 'INPUT LENGTH'                        
         LA    R6,1(R6)            LENGTH OF COMMA                              
         STC   R6,DUMMYHL          STORE LENGTH                                 
PT90     DS    0H                                                               
         MVC   0(2,R4),=C'T='                                                   
         ZIC   R1,0(R3)            GET L'FIELD                                  
         BCTR  R1,0                DEC R1 FOR EX                                
         EX    R1,*+8              MOVE TEXT NUMBER TO 'DUMMY FIELD'            
         B     *+10                                                             
         MVC   2(0,R4),12(R3)                                                   
         LA    R1,1(R1)            RESTORE L'FIELD (WAS 1 LESS)                 
         LA    R1,2(R1)            ADD 2 FOR 'T='                               
         ZIC   R6,DUMMYHL          UPDATE 'INPUT LENGTH'                        
         AR    R6,R1               LENGTH OF TEXT NUMBER                        
         STC   R6,DUMMYHL          STORE LENGTH                                 
         AR    R4,R1               SET R4 TO END OF FIELD                       
         LA    R3,32(R3)           NEXT LINE IN BLOCK                           
         BCT   R5,PT80                                                          
* END LOOP                                                                      
PT100    DS    0H                                                               
*                                                                               
* IN KEEPING WITH HOW THE DETAIL SCREEN HANDLES TEXT ELEMENTS,                  
* THE CALL TO RETEXT WILL ALWAYS GO WITH X'16' (AS IN RESFM0F).                 
* I HAD TO DO THIS BECAUSE THIS IS THE ONLY WAY THAT 'TX=ALL'                   
* WILL WORK FOR AVAILS.                                                         
*                                                                               
         GOTO1 VRETEXT,DMCB,DUMMYH,AIO,(X'16',0),SCANNER,VRECUP                 
         CLC   DMCB(4),=F'0'       ANY ERRORS?                                  
         BE    PT130                                                            
         B     PTERR                                                            
         EJECT                                                                  
* RESTORE INVENTORY FIELD                                                       
PT130    L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         XC    CBLOCK+100(25),CBLOCK+100                                        
         LR    R4,R2                                                            
         LA    R2,CBLOCK+100                                                    
         CLI   ETYPE,C'P'          CK FOR PROPS...                              
         BNE   PT140                                                            
*                                                                               
         USING RPRPDEL,R6          THIS CODE PROPS ONLY                         
         MVC   CBLOCK(3),RPRPDINV                                               
         MVC   CBLOCK+3(3),RPRPDATE                                             
         MVC   CBLOCK+6(1),RPRPDSAT                                             
         B     PT150                                                            
         DROP  R6                                                               
*                                                                               
PT140    DS    0H                  THIS CODE AVAILS ONLY                        
         USING RAVLDEL,R6                                                       
         MVC   CBLOCK(3),RAVLDINV                                               
         MVC   CBLOCK+3(3),RAVLDATE                                             
         MVC   CBLOCK+6(1),RAVLDSAT                                             
         DROP  R6                                                               
*                                                                               
PT150    DS    0H                                                               
         GOTO1 DISPINV                                                          
         LR    R2,R4                                                            
         MVC   8(13,R2),CBLOCK+108                                              
         MVI   4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         B     PTX                                                              
         EJECT                                                                  
*                                                                               
PT200    DS    0H                                                               
         XC    FILTER,FILTER       CLEAR FILTER FLAG                            
         XCEF  BLOCK,480                                                        
         ZIC   R1,5(R2)                                                         
         SH    R1,=H'4'            (BK=) + 1 FOR EX                             
         CLI   11(R2),C'-'         FILTER?                                      
         BE    *+12                YES                                          
         CLI   11(R2),C'+'                                                      
         BNE   PT205               NO                                           
         MVC   FILTER,11(R2)       SET FLAG                                     
         BCTR  R1,0                DON'T MOVE FLAG                              
         EX    R1,*+8                                                           
         B     PT207                                                            
         MVC   DUMMY(0),12(R2)     SKIP FLAG                                    
PT205    DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUMMY(0),11(R2)                                                  
PT207    DS    0H                                                               
         LA    R1,1(R1)                                                         
         STC   R1,DUMMYHL                                                       
         GOTO1 BOOKVAL,DMCB,(CSOURCE,DUMMYH),(10,BLOCK),(C'L',SCANNER),X        
               CBKLABEL                                                         
         CLI   4(R1),0                                                          
         BNE   PT210                                                            
         MVC   RERROR,=AL2(INVBOK)                                              
         B     ERREND                                                           
*                                                                               
PT210    DS    0H                                                               
         CLI   ETYPE,C'P'          CK FOR PROPS...                              
         BNE   PT250                                                            
         GOTO1 VUNBOOK,DMCB,(1,BLOCK),DUMMYH,0,0                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   PT240                                                            
         B     PT230                                                            
         USING RAVLOEL,R6                                                       
PT220    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PT240                                                            
PT230    DS    0H                                                               
         CLI   RAVLOTYP,X'07'                                                   
         BNE   PT220               OOPS...                                      
         MVC   RAVLODTA(8),DUMMY   BOOK (FROM BK=)                              
         B     PT130                                                            
PT240    DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   RAVLOCOD,X'06'                                                   
         MVI   RAVLOLEN,11                                                      
         MVI   RAVLOTYP,X'07'                                                   
         MVC   RAVLODTA(8),DUMMY   BOOK (FROM BK=)                              
         GOTO1 ADDELEM                                                          
         B     PT130                                                            
PT250    DS    0H                                                               
         ZIC   R0,4(R1)                                                         
         LA    RF,BLOCK                                                         
*                                                                               
         XI    0(RF),X'80'         FLIP CPP/CPM BITS                            
         LA    RF,3(RF)                                                         
         BCT   R0,*-8                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RAVLDEL,R6                                                       
         MVC   RAVLDBKS,BLOCK                                                   
         MVC   RAVLDBKF(1),FILTER                                               
         DROP  R6                                                               
         B     PT130                                                            
*                                                                               
PTX      XIT1                                                                   
*                                                                               
PTERR    MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 MYERROR             DOES A GETTXT CALL                           
         SPACE 3                                                                
         LTORG                                                                  
INPUT    DS    CL84                HAS TX= DATA                                 
DUMMYH   DC    X'4400000080'       LEN,ATTR,ADDR,INPT INDS                      
DUMMYHL  DS    X                   INPT LEN                                     
DUMMYHC  DC    X'0000'             OUTPT INDS,OUTPT LEN                         
DUMMY    DS    CL60                                                             
         DC    XL8'00'             USED TO FOOL RETEXT                          
SCBLOCK  DS    CL200               SCAN BLOCK                                   
FILTER   DS    X                   FILTER FLAG                                  
*                                                                               
* 02NOV89      ***  END  ***                                                    
*                                                                               
         EJECT                                                                  
LINED    DSECT                                                                  
LLNUM    DS    CL3                 LINE NUMBER                                  
LRAT     DS    CL6                 RATE                                         
LNUM     DS    CL3                 NUMBER (PROPOSALS ONLY)                      
LCD      DS    CL1                 CODE (PROPOSALS ONLY)                        
         DS    CL3                 SPARE (PROPOSALS ONLY)                       
LINV     DS    CL13                INVENTORY NUMBER                             
LDAYO    DS    CL1                 DAY OVERRIDE INDICATOR                       
LDAY     DS    CL7                 DAY                                          
LTIMO    DS    CL1                 TIME OVERRIDE INDICATOR                      
LTIM     DS    CL11                TIME                                         
LTTLO    DS    CL1                 TITLE OVERRIDE INDICATOR                     
LTTL     DS    CL16                TITLE                                        
         DS    CL11                REST OF TITLE DOESN'T FIT                    
         DS    CL1                 ALLOW FOR FULL TITLE                         
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMC2D                                                       
         EJECT                                                                  
* THIS DATA WILL BE SAVED WITH THE SCREEN AND RESTORED WITH THE SCREEN          
         SPACE 1                                                                
MYKEY    DS    XL48                HEADER KEY                                   
         SPACE 1                                                                
SELTAB   DS    XL20                SELECT TABLE                                 
         SPACE 1                                                                
ITEM     DS    F                   ITEM NUMBER FOR TOP OF DISPLAY               
NUMITEMS DS    F                   TOTAL NUMBER OF ITEMS                        
         SPACE 1                                                                
PFTOP    EQU   7                   PFKEY FOR TOP                                
PFBOTTOM EQU   8                             BOTTOM                             
PFPGUP   EQU   10                            PAGE UP                            
PFPGDN   EQU   11                            PAGE DOWN                          
         EJECT                                                                  
         PRINT OFF                                                              
RAVLD    DSECT                                                                  
       ++INCLUDE REGENAVLN                                                      
         EJECT                                                                  
RPRPD    DSECT                                                                  
       ++INCLUDE REGENPRPN                                                      
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         SPACE 3                                                                
         PRINT ON                                                               
         ORG   SYSSPARE                                                         
*                                                                               
*        WORK AREA                                                              
*                                                                               
SAVER2   DS    A                   SAVE REGISTER 2                              
SAVER3   DS    A                   SAVE REGISTER 3                              
NUMWIND  DS    F                   NUMBER OF LINES IN DISPLAY WINDOW            
RATE     DS    6C                  TEMP OUTPUT AREA FOR AVAIL RATES             
DETLINE  DS    X                   DETAIL LINE NUMBER                           
*                                                                               
PSPT     DS    CL1                 TOTAL NUMBER OF SPOTS IN A PACKAGE           
CODE     DS    CL1                 INPUT TO CODE FIELD                          
         SPACE 1                                                                
PRPAVDEL DS    0CL33                                                            
DINV     DS    CL3                 INVENTORY NUMBER                             
DATE     DS    CL3                 SELECTED DATE                                
DRTE     DS    CL24                RATES                                        
DSAT     DS    CL1                 SATELLITE IF NON-0                           
DNUM     DS    CL1                 NUMBER OF SPOTS (0 IF DNC=T)                 
DNC      DS    CL1                 CODE FOR TYPE OF NUMBER IN # FIELD           
*                                     W=PER WEEK                                
*                                     X=PER PROGRAM                             
*                                     T=TOTAL FOR PACKAGE                       
         SPACE 1                                                                
DETKEY   DS    CL48                DETAIL KEY                                   
KEYCHANG DS    C                   Y IF ANY KEY FIELD HAS CHANGED               
RECCHANG DS    C                   Y IF ANY RECORD FIELD HAS CHANGED            
RECNEW   DS    C                   Y IF NEW RECORD IS BEING ADDED               
SCRCHANG DS    C                   Y IF ANY SCREEN FIELD HAS CHANGED            
SELCODE  DS    C                   SELECT CODE                                  
*                                    0 IF BLANK                                 
*                                    LINE NUMBER IF 'S'                         
STARTAT  DS    X                   STARTAT LINE NUMBER FOR DISPLAY              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'098RESFM10   05/01/02'                                      
         END                                                                    
