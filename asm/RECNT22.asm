*          DATA SET RECNT22    AT LEVEL 203 AS OF 02/02/01                      
*PHASE T80222B,+0                                                               
*INCLUDE OUTDAY                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE SQUASHER                                                               
*INCLUDE REGENPBY                                                               
         TITLE 'T80222 - REPPAK STEREO INTERFACE'                               
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT22 (T80222) --- STEREO INTERFACE                    *             
*                                                                 *             
* --------------------------------------------------------------- *             
*                                                                 *             
* 10JAN96 WSB ORIGINATION                                         *             
*                                                                 *             
*******************************************************************             
*                                                                               
T80222   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80222,RR=R5                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R9,RA                                                            
         AH    R9,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R9                                                       
*                                                                               
         L     R1,=V(OUTDAY)       RELOCATE EXTERNAL MODULES                    
         AR    R1,R5                                                            
         ST    R1,OUTDAY                                                        
         L     R1,=V(UNTIME)                                                    
         AR    R1,R5                                                            
         ST    R1,UNTIME                                                        
         L     R1,=V(SQUASHER)                                                  
         AR    R1,R5                                                            
         ST    R1,SQUASHER                                                      
         L     R1,=V(REGENPBY)                                                  
         AR    R1,R5                                                            
         ST    R1,REGENPBY                                                      
*                                                                               
*                                                                               
         TWAXC STESTRTH,PROT=Y     CLEAR WHOLE SCREEN                           
         MVC   STELAST+1(2),=X'0101'   TRANSMIT WHOLE SCREEN                    
         OI    CONSVCH+1,X'01'     SET SERVICE REQ FIELD TO MODIFIED            
*                                                                               
         LA    R2,STESTRT          R2=LOCATION ON SCREEN                        
         LA    R3,78               R3=NUMBER LEFT ON LINE                       
         ST    R2,SCRLOC                                                        
         ST    R3,REMLINE                                                       
         NI    DISPFLGS,X'FF'-X'40'    NOTHING DISPLAYED ON SCREEN YET          
*                                                                               
*                                                                               
         OC    TWASTRSV(30),TWASTRSV   HAS PROG BEEN ENTERED BEFORE?            
         BZ    MAIN80                  NO                                       
*                                                                               
         OI    DISPFLGS,X'80'          PROG ENTERED BEFORE, TURN BIT ON         
         BAS   RE,SHOWREC                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RBUYKEY,R6          BUILD THE BUY KEY                            
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,TWACNUM    GET CONTRACT NUM                             
         MVC   RBUYKPLN(5),TWASTRBY   BUY LINE INFO FROM LAST TIME              
         DROP  R6                                                               
         GOTO1 VHIGH               READ SAME RECORD BACK TO RESTORE SEQ         
         B     MAINNEXT            GO TO READ NEXT RECORD                       
*                                                                               
MAIN80   NI    DISPFLGS,X'FF'-X'80'  PROG NOT ENTERED YET, TURN BIT OFF         
         MVC   0(6,R2),=C'drfrPr'  FORMATTING INSTR. (LOWER CASE!!!!)           
         LA    R4,6                                                             
         AR    R2,R4               INCREMENT LOCATION BY 6                      
         SR    R3,R4               DECREMENT NUM LEFT BY 6                      
         ST    R2,SCRLOC                                                        
         ST    R3,REMLINE                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RBUYKEY,R6          BUILD THE BUY KEY                            
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,TWACNUM    GET CONTRACT NUM                             
         DROP  R6                                                               
         GOTO1 VHIGH                                                            
*                                                                               
MAIN100  CLC   KEY(22),KEYSAVE                                                  
         BE    MAIN110             ANOTHER RECORD--GO ON                        
*                                                                               
         LA    R7,TWASTRSV         NO MORE RECORDS                              
         MVI   0(R7),C'd'          END OF DATA DELIMITER                        
         LA    R7,1(R7)                                                         
         BAS   RE,SHOWREC          PUT TO SCREEN                                
         NI    TWASTREO,X'FF'-X'40'   NOT SAME CON-TURN OFF STEREO DSM          
         B     MAINX                                                            
*                                                                               
MAIN110  GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
         CLC   RBUYKPLN,=3X'FF'      DOES THIS HAVE A PLAN?                     
         BE    *+14                  NO, GO ON                                  
         CLC   RBUYKMLN(2),=X'FFFF'  YES, IS THIS THE PLAN RECORD?              
         BE    MAINNEXT              YES, GET NEXT RECORD                       
*                                                                               
         MVC   WORK(4),OUTDAY      SET UP WORK FOR REGENPBY                     
         MVC   WORK+4(4),UNTIME                                                 
         MVC   WORK+8(4),DATCON                                                 
         XC    WORK+12(4),WORK+12  DON'T EXPLODE ALT WEEKS                      
*                                                                               
         GOTO1 REGENPBY,DMCB,RBUYREC,(80,ASPULAR),WORK,RCONREC                  
         ZIC   R4,DMCB+4           NUMBER OF LINES ON RETURN                    
         LTR   R4,R4                                                            
         BZ    MAINNEXT            IF 0, READ NEXT RECORD                       
         BCTR  R4,0                NUM OF LINES AFTER FIRST LINE                
         STC   R4,LINEBUY                                                       
*                                                                               
         L     R5,ASPULAR          AREA WHERE GETTING INFO FROM                 
         USING PRTLN,R5                                                         
*                                                                               
         LA    R7,TWASTRSV         BUILD AREA FOR SCREEN STUFF                  
*                                                                               
*                                                                               
         LA    R6,RBUYKEY                                                       
         USING RBUYKEY,R6                                                       
         EDIT  RBUYKLIN,(3,(R7)),ALIGN=LEFT   BUY LINE NUM                      
         AR    R7,R0               ADD LENGTH OF NUMBER                         
         MVI   0(R7),C'f'          FIELD DELIMITER                              
         LA    R7,1(R7)                                                         
*                                                                               
         MVC   0(L'PCHG,R7),PCHG   MODIFICATION CODES                           
         LA    R7,L'PCHG(R7)                                                    
         BAS   RE,GOBACK                                                        
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   MAINNEXT                                                         
         USING RBUYELEM,R6                                                      
         EDIT  RBUYFLT,(3,(R7)),ALIGN=LEFT     FLIGHT NUM                       
         LA    R7,3(R7)                                                         
         BAS   RE,GOBACK                                                        
*                                                                               
         ZIC   R4,LINEBUY          NUM OF LINES AFTER CURRENT LINE              
MAIN200  MVC   0(L'PDAY,R7),PDAY   GET A DAY LINE                               
         LA    R7,L'PDAY(R7)                                                    
         BAS   RE,GOBACK                                                        
         BCTR  R7,0                GO BACK OVER THE DELIMITER                   
         LTR   R4,R4               ANY MORE LINES TO CHECK?                     
         BZ    MAIN250             NO                                           
         LA    R5,L'PRTLN(R5)      GO TO NEXT LINE                              
         CLC   0(13,R5),=XL13'00'  NULLS? (POSSIBLE COMMENT)                    
         BNE   *+12                NO, NOT COMMENT LINE                         
         CLI   13(R5),0            IS THIS A COMMENT? (NON NULL CHAR)           
         BNE   MAIN250             YES                                          
         CLI   PDAY,C' '           IS THERE ANOTHER DAY LINE?                   
         BNH   MAIN250             NO                                           
         MVI   0(R7),C'*'          YES, INSERT *                                
         LA    R7,1(R7)                                                         
         BCTR  R4,0                                                             
         B     MAIN200             FIND NEXT LINE                               
*                                                                               
MAIN250  L     R5,ASPULAR          RESTORE TO BEGINNING                         
         LA    R7,1(R7)            GO PAST THE DELIMITER                        
*                                                                               
         ZIC   R4,LINEBUY                                                       
MAIN300  MVC   0(L'PTIM,R7),PTIM   GET A TIME LINE                              
         LA    R7,L'PTIM(R7)                                                    
         BAS   RE,GOBACK                                                        
         BCTR  R7,0                GO BACK OVER THE DELIMITER                   
         LTR   R4,R4                                                            
         BZ    MAIN350                                                          
         LA    R5,L'PRTLN(R5)                                                   
         CLC   0(13,R5),=XL13'00'                                               
         BNE   *+12                                                             
         CLI   13(R5),0                                                         
         BNE   MAIN350                                                          
         CLI   PTIM,C' '                                                        
         BNH   MAIN350                                                          
         MVI   0(R7),C'*'                                                       
         LA    R7,1(R7)                                                         
         BCTR  R4,0                                                             
         B     MAIN300                                                          
*                                                                               
MAIN350  L     R5,ASPULAR                                                       
         LA    R7,1(R7)            GO PAST THE DELIMITER                        
*                                                                               
         GOTO1 SQUASHER,DMCB,PLEN,L'PLEN     LENGTH                             
         MVC   0(L'PLEN,R7),PLEN                                                
         LA    R7,L'PLEN(R7)                                                    
         BAS   RE,GOBACK                                                        
*                                                                               
         ZIC   R4,LINEBUY                                                       
MAIN400  MVC   0(L'PDAT,R7),PDAT   GET A DATE LINE                              
         LA    R7,L'PDAT(R7)                                                    
         BAS   RE,GOBACK                                                        
         BCTR  R7,0                GO BACK OVER THE DELIMITER                   
MAIN420  LTR   R4,R4                                                            
         BZ    MAIN450                                                          
         LA    R5,L'PRTLN(R5)                                                   
         CLC   0(13,R5),=XL13'00'                                               
         BNE   *+12                                                             
         CLI   13(R5),0                                                         
         BNE   MAIN450                                                          
         CLC   PDAT(3),=C'ALT'     DOES LINE SAY 'ALTERNATE WEEKS'?             
         BNE   *+10                NO, GO ON                                    
         BCTR  R4,0                YES, GO TO NEXT LINE                         
         B     MAIN420                                                          
*                                                                               
         CLI   PDAT,C' '                                                        
         BNH   MAIN450                                                          
         MVI   0(R7),C'*'                                                       
         LA    R7,1(R7)                                                         
         BCTR  R4,0                                                             
         B     MAIN400                                                          
*                                                                               
MAIN450  L     R5,ASPULAR                                                       
         LA    R7,1(R7)            GO PAST THE DELIMITER                        
*                                                                               
         MVC   0(L'RBUYTYP,R7),RBUYTYP     TYPE                                 
         LA    R7,L'RBUYTYP(R7)                                                 
         BAS   RE,GOBACK                                                        
*                                                                               
         MVC   0(L'RBUYDPT,R7),RBUYDPT     DAYPART                              
         LA    R7,L'RBUYDPT(R7)                                                 
         BAS   RE,GOBACK                                                        
*                                                                               
         MVC   0(L'RBUYCLS,R7),RBUYCLS     CLASS                                
         LA    R7,L'RBUYCLS(R7)                                                 
         BAS   RE,GOBACK                                                        
*                                                                               
         MVC   0(L'RBUYSEC,R7),RBUYSEC     SECTION                              
         LA    R7,L'RBUYSEC(R7)                                                 
         BAS   RE,GOBACK                                                        
         DROP  R6                                                               
*                                                                               
         LA    R6,RBUYKEY                                                       
         USING RBUYKEY,R6                                                       
         MVC   0(L'RBUYKPLN,R7),RBUYKPLN   PLAN                                 
         CLC   0(L'RBUYKPLN,R7),=3X'FF'    NO PLAN?                             
         BNE   *+10                                                             
         MVC   0(L'RBUYKPLN,R7),MYSPACES   YES, MOVE IN SPACES                  
         LA    R7,L'RBUYKPLN(R7)                                                
         BAS   RE,GOBACK                                                        
         DROP  R6                                                               
*                                                                               
         GOTO1 SQUASHER,DMCB,PNPW,L'PNPW   SPOTS PER WEEK                       
         MVC   0(L'PNPW,R7),PNPW                                                
         LA    R7,L'PNPW(R7)                                                    
         BAS   RE,GOBACK                                                        
*                                                                               
         LA    R6,RCONREC          RATE/SPOT                                    
         MVI   ELCODE,X'17'        COMBO ELEMENT                                
         BAS   RE,GETEL            IS IT A COMBO?                               
         BNE   MAIN500             NO                                           
*                                                                               
         MVC   MYKEYSAV,KEY        YES--SAVE BUY KEY TO GET BACK LATER          
         BAS   RE,GOBACK           EMPTY FIELD FOR SINGLE BUY                   
*                                                                               
         ST    R6,RECDSTOR         SAVE CURRENT POSITION IN COMBO ELEM          
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'01'        GET BUY ELEM                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYELEM,R6                                                      
*                                                                               
         TM    RBUYCOMB,X'80'      IS THIS AN N/A BUY?                          
         BZ    MAIN460             NO                                           
         MVC   0(2,R7),=C'NA'      YES                                          
         LA    R7,2(R7)                                                         
         BAS   RE,GOBACK                                                        
         B     MAIN462                                                          
*                                                                               
MAIN460  EDIT  RBUYCOS,(11,(R7)),2,ALIGN=LEFT   GET THE RATE                    
         LA    R7,11(R7)                                                        
         BAS   RE,GOBACK                                                        
         DROP  R6                                                               
*                                                                               
MAIN462  L     R6,RECDSTOR         RESTORE THE COMBO ELEMENT                    
         ZIC   R4,1(R6)            LENGTH OF COMBO ELEMENT                      
         LA    R1,2                                                             
         AR    R6,R1               MOVE PAST ELEMENT HEADER                     
         SR    R4,R1               DECREASE BY HEADER LENGTH                    
         LTR   R4,R4               IS ANYTHING IN THERE?                        
         BNZ   *+6                                                              
         DC    H'0'                NO, DIE                                      
         LA    R3,4                4 COMBO RATE FIELDS                          
*                                                                               
MAIN470  CLC   RCONKCON,5(R6)      IS IT THE CURRENT CONTRACT?                  
         BE    MAIN490             YES, SKIP IT--ALREADY DISPLAYED              
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),5(4,R6)                                               
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+10(5)  9'S COMPLEMENT OF CONTRACT NUM             
*                                                                               
         ST    R6,RECDSTOR         SAVE CURRENT POSITION IN COMBO ELEM          
         LA    R6,KEY                                                           
         USING RBUYKEY,R6          BUILD NEW BUY KEY TO FIND                    
*                                                                               
         MVO   WORK(5),WORK+5(5)                                                
*                                                                               
         PACK  RBUYKCON+0(1),WORK+3(1)                                          
         PACK  RBUYKCON+1(1),WORK+2(1)                                          
         PACK  RBUYKCON+2(1),WORK+1(1)                                          
         PACK  RBUYKCON+3(1),WORK+0(1)  REVERSE THE 9'S COMPLEMENT              
*                                                                               
         GOTO1 VHIGH               GET THE BUY REC                              
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                NOT THERE                                    
*                                                                               
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'01'        GET BUY ELEM                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYELEM,R6                                                      
*                                                                               
         TM    RBUYCOMB,X'80'      IS THIS AN N/A BUY?                          
         BZ    MAIN485             NO                                           
         MVC   0(2,R7),=C'NA'      YES                                          
         LA    R7,2(R7)                                                         
         BAS   RE,GOBACK                                                        
         B     MAIN487                                                          
*                                                                               
MAIN485  EDIT  RBUYCOS,(11,(R7)),2,ALIGN=LEFT   GET THE RATE                    
         LA    R7,11(R7)                                                        
         BAS   RE,GOBACK                                                        
         DROP  R6                                                               
MAIN487  L     R6,RECDSTOR         RESTORE THE COMBO ELEMENT                    
*                                                                               
MAIN490  BCTR  R3,0                DECREMENT NUM OF FIELDS                      
         LTR   R3,R3                                                            
         BZ    MAIN495             ALL FOUR HAVE BEEN DONE                      
         LA    R1,9                GO TO NEXT CONTRACT IN COMBO                 
         AR    R6,R1                                                            
         SR    R4,R1                                                            
         LTR   R4,R4               ARE THERE ANY MORE?                          
         BNZ   MAIN470             YES, GO ON                                   
*                                                                               
         BAS   RE,GOBACK           PUT IN ANY BLANK FIELDS                      
         BCT   R3,*-4                                                           
*                                                                               
MAIN495  MVC   KEY,MYKEYSAV        RESTORE THE CURRENT BUY                      
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         B     MAIN520                                                          
*                                                                               
MAIN500  LA    R6,RBUYREC          SINGLE RATE/SPOT                             
         MVI   ELCODE,X'01'        GET BUY ELEM                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYELEM,R6                                                      
         EDIT  RBUYCOS,(11,(R7)),2,ALIGN=LEFT   GET THE RATE                    
         LA    R7,11(R7)                                                        
         BAS   RE,GOBACK                                                        
         DROP  R6                                                               
         MVC   0(8,R7),=C' f f f f'                                             
         LA    R7,8(R7)                                                         
*                                                                               
MAIN520  LA    R6,RBUYKEY                                                       
         MVI   ELCODE,X'04'        GET REGULAR COMMENTS                         
         BAS   RE,GETCOMS                                                       
*                                                                               
         MVI   ELCODE,X'84'        GET ORD COMMENTS                             
         BAS   RE,GETCOMS                                                       
*                                                                               
         BCTR  R7,0                MOVE BACK OVER DELIMITER                     
         MVI   0(R7),C'r'          RECORD DELIMITER                             
         LA    R7,1(R7)                                                         
*                                                                               
         BAS   RE,SHOWREC          PUT THE BUY INFO TO THE SCREEN               
         BNE   MAINX                                                            
*                                                                               
MAINNEXT GOTO1 VSEQ                                                             
         B     MAIN100                                                          
********                                                                        
*                                                                               
MAINX    DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
YES      SR    RC,RC               SET CC EQUAL                                 
NO       LTR   RC,RC               SET CC NOT EQUAL                             
EXT      XIT1                                                                   
*                                                                               
R7EXIT   XIT1  REGS=(R7)           EXIT BUT DON'T RESTORE R7                    
*                                                                               
         EJECT                                                                  
*                                                                               
**** DISPLAY ONE RECORD TO SCREEN                                               
*                                                                               
SHOWREC  NTR1                                                                   
         L     R2,SCRLOC                                                        
         L     R3,REMLINE                                                       
*                                                                               
         TM    DISPFLGS,X'80'      HAS PROG ALREADY BEEN ENTERED?               
         BZ    SHOW80              NO                                           
         TM    DISPFLGS,X'40'      YES, IS ANYTHING ON SCREEN YET?              
         BO    SHOW80              YES                                          
*                                                                               
* PROG HAS BEEN ENTERED AND NOTHING ON SCREEN YET--DISPLAY REST OF              
* BUFFER FROM LAST TIME                                                         
         OI    DISPFLGS,X'40'      SOMETHING WILL BE ON SCREEN                  
         LA    R7,TWASTRSV                                                      
         ICM   R4,15,TWASTRDP                                                   
         AR    R7,R4                                                            
         ICM   R8,15,TWASTRNL                                                   
         B     SHOW100                                                          
*                                                                               
* PROG HAS NOT BEEN ENTERED OR PROG HAS BEEN ENTERED AND SOMETHING              
* ON SCREEN ALREADY                                                             
SHOW80   LR    R8,R7               R8=NUMBER OF CHARS LEFT IN TWASTRSV          
         LA    R7,TWASTRSV                                                      
         SR    R8,R7                                                            
*                                                                               
*                                                                               
SHOW100  CR    R8,R3               MORE CHARS THAN FIT ON A LINE?               
         BH    SHOW150             YES                                          
*                                                                               
         LTR   R8,R8               SECURITY CHECK                               
         BNZ   *+6                                                              
         DC    H'0'                THIS CASE SHOULD NEVER HAPPEN                
         BCTR  R8,0                END OF TWASTRSV--CAN FIT ON LINE             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R7)                                                    
         LA    R8,1(R8)            COMPENSATE FOR DECREMENTATION                
         AR    R2,R8                                                            
         SR    R3,R8                                                            
         ST    R2,SCRLOC                                                        
         ST    R3,REMLINE                                                       
         B     YES                 GO TO READ ANOTHER RECORD                    
*                                                                               
SHOW150  LTR   R3,R3               WAS IT AT END OF LINE LAST TIME?             
         BZ    SHOW180             YES, COMPENSATE FOR IT                       
         BCTR  R3,0                NO, TOO MANY CHARS TO FIT ON LINE            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R7)                                                    
         LA    R3,1(R3)            COMPENSATE FOR DECREMENTATION                
         AR    R7,R3               CONTINUE ON IN TWASTRSV                      
         SR    R8,R3               DECREASE NUM LEFT IN TWASTRSV                
         AR    R2,R3               GO TO END OF LINE                            
*                                                                               
SHOW180  LA    R4,STELLIN+78                                                    
         CR    R2,R4                                                            
         BNL   SHOW200             ** HIT ENTER FOR NEXT SCREEN **              
         LA    R2,8(R2)            GO TO NEXT LINE                              
         LA    R3,78                                                            
         B     SHOW100                                                          
*                                                                               
SHOW200  LA    R4,TWASTRSV                                                      
         SR    R7,R4               FIND DISPLACEMENT FROM BEGINNING             
         STCM  R7,15,TWASTRDP      STORE VALUES FOR NEXT TIME                   
         STCM  R8,15,TWASTRNL                                                   
         LA    R6,RBUYKEY                                                       
         USING RBUYKEY,R6                                                       
         MVC   TWASTRBY(5),RBUYKPLN    SAVE INFO ABOUT BUY RECORD               
         DROP  R6                                                               
         B     NO                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
***** GO BACK--SEARCH BACKWARD AND PUT IN A FIELD DELIMITER                     
*                                                                               
GOBACK   NTR1                                                                   
GB10     BCTR  R7,0                GO BACK ONE CHAR                             
         CLI   0(R7),C' '          IS IT <= A SPACE?                            
         BNH   GB10                YES, GO BACK FURTHER                         
*                                                                               
         CLI   0(R7),C'f'          IS IT A FIELD DELIMITER?                     
         BE    *+12                YES, PUT IN A SPACE FOR NO DATA              
         CLI   0(R7),C'r'          IS IT A RECORD DELIMITER?                    
         BNE   GB20                NO, END OF DATA                              
*                                                                               
         MVI   1(R7),C' '          YES, PUT IN A SPACE FOR NO DATA              
         LA    R7,1(R7)            FALL THROUGH                                 
*                                                                               
GB20     MVI   1(R7),C'f'          PUT IN FIELD DELIMITER                       
         LA    R7,2(R7)                                                         
*                                                                               
         B     R7EXIT                                                           
         EJECT                                                                  
*                                                                               
*** GET COMMENTS                                                                
*                                                                               
*   COMING IN TO THIS, ELCODE SHOULD REFLECT THE APPROPRIATE COMMENT            
*   TYPE TO BE FOUND.  THERE IS SPACE FOR 2 COMMENTS, AND BOTH SPACES           
*   HAVE TO BE FILLED WITH AT LEAST 'NO DATA'                                   
*                                                                               
GETCOMS  NTR1                                                                   
         BAS   RE,GETEL            IS THERE AN ELEMENT?                         
         BE    *+12                THERE IS ONE, GO ON                          
*                                                                               
         BAS   RE,GOBACK           NO ELEMENT, PUT IN FIRST BLANK SPACE         
         B     GC100                                                            
*                                                                               
         BAS   RE,ONECOM           GET FIRST COMMENT FROM ELEMENT               
         BAS   RE,GOBACK           PUT IN DELIMITER                             
*                                                                               
         BAS   RE,NEXTEL           IS THERE ANOTHER?                            
         BNE   *+8                 NO, PUT IN BLANK SPACE                       
         BAS   RE,ONECOM           YES, GET COMMENT                             
*                                                                               
GC100    BAS   RE,GOBACK                                                        
         B     R7EXIT              PASS BACK R7                                 
*                                                                               
*                                                                               
ONECOM   NTR1                                                                   
         ZIC   R1,1(R6)            ELEMENT LENGTH                               
         CLI   ELCODE,X'84'        IS IT AN ORD COMMENT?                        
         BNE   *+12                NO                                           
         LA    R4,3                YES, HEADER IS 3 BYTES LONG                  
         B     *+8                                                              
*                                                                               
         LA    R4,2                REGULAR COMMENT, HEADER IS 2 BYTES           
*                                                                               
         SR    R1,R4               SUBTRACT HEADER LENGTH                       
         LTR   R1,R1               IS IT 0?                                     
         BZ    ONECOMX             YES, GET OUT                                 
         AR    R6,R4               GO TO BEGINNING OF COMMENT                   
         BCTR  R1,0                                                             
         EX    R1,*+8              GET COMMENT                                  
         B     *+10                                                             
         MVC   0(0,R7),0(R6)                                                    
         LA    R1,1(R1)                                                         
         AR    R7,R1                                                            
ONECOMX  B     R7EXIT              PASS BACK R7                                 
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONHACTH                                                         
       ++INCLUDE RECNTD3D                                                       
         EJECT                                                                  
       ++INCLUDE REGENPBYD                                                      
         EJECT                                                                  
GENOLD   DSECT                                                                  
         ORG   WORK3               MY WORKING STORAGE DSECT                     
RECDSTOR DS    F                   STORE VALUE OF R6 (POINTER TO REC'D)         
SCRLOC   DS    F                   SCREEN LOCATION                              
REMLINE  DS    F                   AMOUNT REMAINING ON CURRENT LINE             
LINEBUY  DS    XL1                 LINES REMAINING IN CURRENT BUY               
DISPFLGS DS    XL1                 X'80' = PROGRAM PREVIOUSLY ENTERED           
*                                  X'40' = LINES ALREADY ON SCREEN              
MYKEYSAV DS    CL32                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'203RECNT22   02/02/01'                                      
         END                                                                    
