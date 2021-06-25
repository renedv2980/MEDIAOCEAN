*          DATA SET DDLINUP    AT LEVEL 032 AS OF 05/01/02                      
*PHASE T00A75B,+0                                                               
         TITLE 'LINUP - WINDOW LINE MONITOR'                                    
LINUP    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LUWRKL,*LINUP*,RR=R2                                             
*                                                                               
         USING LUWRKD,RC                                                        
         ST    R2,RELO                                                          
         MVC   USERRD,4(RD)        SAVE USER RD                                 
*                                                                               
         L     RA,0(R1)            A(LINUPD)                                    
         LA    RA,0(RA)                                                         
         USING LUBLKD,RA                                                        
         ST    R1,SVAPARM                                                       
*                                                                               
         CLI   LUNEW,C'Y'          IF NEW KEY                                   
         BNE   LU040                                                            
         L     R4,LUSVTAB          CLEAR SAVE AREA                              
         LH    RF,LUSVLEN                                                       
         BCTR  RF,R0                                                            
         ZIC   R0,LUNLINS                                                       
         EX    RF,CLRSAV                                                        
         AH    R4,LUSVLEN                                                       
         BCT   R0,*-8                                                           
*                                                                               
LU040    DS    0H                                                               
         MVI   LUWSTAT,0           CLEAR WINDOW STATUS                          
         EJECT                                                                  
*                                                                               
*              DETERMINE IF THIS WINDOW HAS CURSOR                              
*                                                                               
LDWIN    DS    0H                                                               
*                                                                               
*              NEED TO SCAN ALL FIELDS IN WINDOW TO SEE                         
*                IF ONE HAS CURSOR IN IT                                        
*                                                                               
         L     R8,LUATIOB          POINT TO TIOB WHICH HAS CURSOR INFO          
         USING TIOBD,R8            ESTABLISH AREA                               
         L     R4,LUADSPS          POINT TO START OF LINE DISPLACEMENTS         
         ZIC   R1,LUNLINS          GET NUMBER OF LINES                          
LDCRLOOP DS    0H                                                               
         LH    R2,0(R4)            POINT TO 1ST FLD ON A LINE IN WINDOW         
         A     R2,LUATWA                                                        
         USING FLDHDRD,R2          ESTABLISH FIELD                              
         ZIC   R3,LUNFLDS          GET NUMBER OF FIELDS ON LINE                 
LDCRLUP1 DS    0H                                                               
         LR    RF,R2               LOOKING FOR A FIELD WITH CURSOR              
         S     RF,LUATWA                                                        
         CLM   RF,3,TIOBCURD                                                    
         BE    LDWIN04                                                          
         BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         BCT   R3,LDCRLUP1         GO TEST IT                                   
         LA    R4,2(R4)            POINT TO NEXT LINE ON SCREEN                 
         BCT   R1,LDCRLOOP         GO CHECK NEXT LINE                           
*                                  CURSOR NOT IN WINDOW                         
         B     LDWINX              YES, DONT QUIT                               
*                                                                               
LDWIN04  DS    0H                                                               
         OI    LUWSTAT,LUWCURSQ    TELL CALLER CURSOR WAS IN WINDOW             
LDWINX   DS    0H                                                               
         EJECT                                                                  
         CLI   LUAPMODE,LUAPVALQ   TEST VALIDATING                              
         BE    LUVAL                                                            
         CLI   LUAPMODE,LUAPDSPQ   TEST DISPLAYING                              
         BE    LUDSP                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* VALIDATION ROUTINE                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
LUVAL    DS    0H                                                               
         L     R3,LUADSPS          A(LINE DISPLACEMENT LIST)                    
         L     R4,LUSVTAB          A(SAVE TABLE)                                
         ZIC   R5,LUNLINS          NUMBER OF LINES                              
*                                                                               
LV04     DS    0H                                                               
         SR    R2,R2                                                            
         ICM   R2,3,0(R3)                                                       
         BZ    LUX                 MUST BE EOS                                  
         A     R2,LUATWA                                                        
         USING FLDHDRD,R2                                                       
*                                                                               
         CLI   LUNEW,C'Y'          IF 'NEW KEY'                                 
         BNE   LV06                                                             
         NI    FLDIIND,255-FINPVAL   SET OFF PREV VALIDATED BIT                 
         CLI   FLDILEN,0             AND IF FIELD HAS DATA                      
         BE    *+8                                                              
         OI    FLDIIND,FINPTHIS      SET ON INPUT THIS TIME                     
*                                                                               
LV06     DS    0H                                                               
         MVI   LUFSTAT,0           SET STATUS OF FIRST FIELD                    
         CLI   FLDILEN,0           ANY INPUT?                                   
         BE    *+8                                                              
         OI    LUFSTAT,LUSDATQ     HAS DATA                                     
         TM    FLDIIND,FINPTHIS    ANY INPUT THIS TIME?                         
         BZ    *+8                                                              
         OI    LUFSTAT,LUSINPQ                                                  
         TM    FLDATB,FATBPROT     IF PROTECTED, SKIP VALIDATED TEST            
         BNZ   *+16                                                             
         TM    FLDIIND,FINPVAL     HAS IT BEEN VALIDATED?                       
         BNZ   *+8                 YES, OK                                      
         OI    LUFSTAT,LUSNPVQ     NO, SET NOT VALIDATED                        
*                                                                               
         LR    R1,R2               SAVE R2                                      
         ZIC   R6,LUNFLDS          TEST STATUS OF LINE                          
         MVI   LULSTAT,0           CLEAR LINE STATUS                            
         MVI   LUDSTAT,0           CLEAR DATA FIELD STATUS                      
*                                                                               
LV08     DS    0H                                                               
         CLI   LUNEW,C'Y'          IF 'NEW KEY'                                 
         BNE   LV08B                                                            
         NI    FLDIIND,255-FINPVAL   SET OFF PREV VALIDATED BIT                 
         CLI   FLDILEN,0             AND IF FIELD HAS DATA                      
         BE    *+8                                                              
         OI    FLDIIND,FINPTHIS      SET ON INPUT THIS TIME                     
*                                                                               
LV08B    DS    0H                                                               
         CLI   FLDILEN,0           ANY INPUT?                                   
         BE    LV08D                                                            
         OI    LULSTAT,LUSDATQ     YES, SET HAVE DATA                           
         CR    R1,R2               TEST ON FIRST FIELD                          
         BE    LV08D                                                            
         OI    LUDSTAT,LUSDATQ     NO, SET DATA FIELD STATUS ALSO               
*                                                                               
LV08D    DS    0H                                                               
         TM    FLDIIND,FINPTHIS    ANY INPUT THIS TIME?                         
         BZ    LV08F                                                            
         OI    LULSTAT,LUSINPQ     YES                                          
         CR    R1,R2               TEST ON FIRST FIELD                          
         BE    LV08F                                                            
         OI    LUDSTAT,LUSINPQ     NO, SET DATA FIELD STATUS ALSO               
*                                                                               
LV08F    DS    0H                                                               
         TM    FLDATB,FATBPROT     IF PROTECTED, SKIP VALIDATED TEST            
         BNZ   LV08H                                                            
         TM    FLDIIND,FINPVAL     HAS IT BEEN VALIDATED?                       
         BNZ   LV08H               YES                                          
         OI    LULSTAT,LUSNPVQ     NO, SET NOT VALIDATED                        
         CR    R1,R2               TEST ON FIRST FIELD                          
         BE    LV08H                                                            
         OI    LUDSTAT,LUSNPVQ     NO, SET DATA FIELD STATUS ALSO               
*                                                                               
LV08H    DS    0H                                                               
         BAS   RE,BUMP             NEXT FIELD                                   
         BCT   R6,LV08                                                          
*                                                                               
LV20     DS    0H                                                               
         ST    R1,LUACLIN          SET A(CURRENT LINE)                          
         ST    R4,LUACTAB          SET A(CURRENT TABLE ENTRY)                   
         MVI   LUMODE,LUVALQ                                                    
         BAS   RE,HOOK                                                          
         OC    LUWSTAT,LULSTAT     ADD LINE STATUS TO WINDOW STATUS             
         TM    LUSTAT,LUVERRQ      TEST VALIDATION ERROR                        
         BZ    LV40                                                             
         MVI   LUERR,1             SET ERROR                                    
         OI    LUWSTAT,LUWVERRQ    INDICATE VALIDATION ERROR IN WINDOW          
         TM    LUCNTL,LUERRXTQ     TEST TO EXIT ON ERROR                        
         BZ    LV40                NO                                           
         B     LUX                 YES, QUIT                                    
*                                                                               
LV40     DS    0H                                                               
         TM    LUSTAT,LUCLEARQ     HAS USER ASKED TO CLEAR SCREEN?              
         BZ    LV50                NO                                           
*                                                                               
         S     R3,LUADSPS                                                       
         SRL   R3,1                                                             
         ZIC   R5,LUNLINS                                                       
         SR    R5,R3               NUMBER TO CLEAR                              
         LA    R3,1(R3)            NUMBERS START AT 1 NOT 0                     
         GOTO1 CLRMULT,PARMS,(R3),(R5) CLEAR REST OF SCREEN                     
         B     LUX                                                              
*                                                                               
LV50     DS    0H                                                               
         AH    R4,LUSVLEN          NEXT TABLE ENTRY                             
         LA    R3,2(R3)            NEXT LINE                                    
         BCT   R5,LV04                                                          
*                                                                               
         B     LUX                                                              
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* DISPLAY ROUTINE                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
LUDSP    DS    0H                                                               
*                                                                               
*              DETERMINE IF THIS WINDOW SHOULD BE SCROLLED                      
*              IF CNTROL BIT SET THEN CURSOR MUST BE IN THIS WINDOW             
*                                                                               
         CLI   LUNEW,C'Y'          ALWAYS DISPLAY NEW RECORDS                   
         BE    LUDSP10                                                          
*                                                                               
         TM    LUCNTL,LUCURSQ      DISPLAY IF NOT CURSOR DEPENDENT              
         BNO   LUDSP10                                                          
*                                                                               
         TM    LUWSTAT,LUWCURSQ    DISPLAY IF CURSOR IN WINDOW                  
         BO    LUDSP10                                                          
*                                                                               
         TM    LUCNTL,LUREDSPQ+LUAUTOQ RE-DISPLAY OR AUTO SCROLL                
         BNZ   LUDSP10             YES, DONT QUIT                               
         B     LUX                 ELSE EXIT                                    
*                                                                               
LUDSP10  DS    0H                                                               
         EJECT                                                                  
*                                                                               
*              DETERMINE SCROLLING FACTOR                                       
*                                                                               
LDSCR    DS    0H                                                               
*                                                                               
         ZIC   RF,LUNLINS          DEFAULT SCROLL FACTOR IS FULL PAGE           
*                                                                               
         CLI   LUNEW,C'Y'          FIRST TIME?                                  
         BE    LDSCRX                                                           
*                                                                               
         TM    LUCNTL,LUCURSQ      SCROLL IF CURSOR SELECT AND                  
         BNO   LDSCR10                                                          
         TM    LUWSTAT,LUWCURSQ    CURSOR IN WINDOW                             
         BO    LDSCR20                                                          
*                                                                               
LDSCR10  DS    0H                                                               
         TM    LUCNTL,LUREDSPQ     RE-DISPLAY DEFAULTS TO A PAGE                
         BO    LDSCRX                                                           
*                                                                               
LDSCR20  DS    0H                                                               
         IC    RF,LUSCROLL         GET SCROLLING FIELD                          
         TM    LUSCROLL,LUPAGEQ+LUHALFQ TEST FOR FULL OR HALF PAGE              
         BZ    *+8                 ELSE NUMBER OF SCROLL LINES                  
         IC    RF,LUNLINS          GET LINES ON THE PAGE                        
         TM    LUSCROLL,LUHALFQ    DIVIDE BY 2 FOR HALF PAGE                    
         BNO   *+8                                                              
         SRL   RF,1                                                             
         LTR   RF,RF               RF NOW HAS NUMBER OF SCROLL LINES            
         BNZ   *+8                 0 LINES MEANS A PAGE                         
         IC    RF,LUNLINS          GET LINES ON A PAGE                          
*                                                                               
LDSCRX   DS    0H                                                               
         STC   RF,SVSCROLL         SAVE SCROLL AMOUNT                           
         CLC   SVSCROLL,LUNLINS    CAN BE AT MOST ONE SCREEN                    
         BNH   *+10                                                             
         MVC   SVSCROLL,LUNLINS    DEFAULT TO ONE PAGE                          
         EJECT                                                                  
*                                                                               
*              DETERMINE SCROLLING DIRECTION                                    
*              PRIORITIES - FIRST TIME                                          
*                           CURSOR SELECT AND CURSOR IN WINDOW                  
*                           RE-DISPLAY OF SCREEN                                
*                           DIRECTIONAL PF KEYS                                 
*                           SCROLL FIELD ENTRIES                                
*                           AUTO SCROLL DEFAULT                                 
*                                                                               
*              RE-DISPLAY HONORED BEFORE AUTOSCROLL                             
*                 EXCEPT WHEN WINDOW IS CURSOR SENSITIVE                        
*                             CURSOR IS IN WINDOW AND                           
*                             NO SCROLL PFKEYS OR SCROLL FIELD ENTRIES          
*                 THEN AUTOSCROLL HONORED BEFORE RE-DISPLAY                     
*                                                                               
LDDIR    DS    0H                                                               
         MVI   LUDIR,C'+'          SET FOR DOWN SCROLL                          
         CLI   LUNEW,C'Y'          FIRST TIME?                                  
         BE    LDDIRX                                                           
*                                                                               
         TM    LUCNTL,LUCURSQ      CHECK SCROLL KEYS & FIELDS                   
         BNO   LDDIR20             IF CURSOR SENSITIVE AND                      
         TM    LUWSTAT,LUWCURSQ    CURSOR IN WINDOW                             
         BO    LDDIR20                                                          
*                                                                               
LDDIR10  DS    0H                                                               
         MVI   LUDIR,C'='          SET FOR RE-DISPLAY                           
         TM    LUCNTL,LUREDSPQ     TEST TO REDISPLAY FROM CURRENT START         
         BO    LDDIRX                                                           
         B     LUX                 EXIT OTHERWISE                               
*                                                                               
LDDIR20  DS    0H                                                               
         MVI   LUDIR,C'+'          SET FOR DOWN SCROLL                          
         CLI   LUPFKEY,LUPFDNQ     DOWN PFKEY?                                  
         BE    LDDIRX                                                           
*                                                                               
         MVI   LUDIR,C'-'          SET FOR UP SCROLL                            
         CLI   LUPFKEY,LUPFUPQ     UP PFKEY?                                    
         BE    LDDIRX                                                           
*                                                                               
         MVI   LUDIR,C'+'          SET FOR DOWN SCROLL                          
         TM    LUPOS,LUFRSTQ+LUNEXTQ FIRST OR NEXT SCREEN                       
         BNZ   LDDIRX                                                           
*                                                                               
         MVI   LUDIR,C'-'          SET FOR UP SCROLL                            
         TM    LUPOS,LULASTQ+LUPREVQ FIRST OR NEXT SCREEN                       
         BNZ   LDDIRX                                                           
*                                                                               
         MVI   LUDIR,C'+'          SET FOR DOWN SCROLL                          
         TM    LUCNTL,LUAUTOQ      TEST FOR AUTO SCROLLING                      
         BO    LDDIRX                                                           
*                                                                               
         MVI   LUDIR,C'='          SET FOR RE-DISPLAY                           
         MVC   SVSCROLL,LUNLINS    SET FOR FULL PAGE                            
         TM    LUCNTL,LUREDSPQ     RE-CHECK FOR RE-DISPLAY                      
         BNO   LUX                                                              
*                                                                               
LDDIRX   DS    0H                                                               
         CLI   LUDIR,C'-'          IF BACKWARD SCROLLING                        
         BNE   LDDIREND                                                         
         TM    LUCNTL,LUBACKQ      THEN MUST BE ALLOWED                         
         BNO   LUX                                                              
LDDIREND DS    0H                                                               
         EJECT                                                                  
*                                                                               
*              DETERMINE STARTING POSITION OF DISPLAY                           
*              CHOICES ARE FIRST DISPLAYABLE FIELD                              
*                          LAST  DISPLAYABLE FIELD                              
*                          FIRST FIELD ON CURRENT SCREEN                        
*                          +/-   SCROLL AMOUNT                                  
*                                                                               
LDPOS    DS    0H                                                               
         ZIC   R5,LUNLINS          DEFAULT TO FULL SCREEN DISPLAY               
         LA    RF,1                DEFAULT IS DISPLAY FROM TOP DOWN             
         CLI   LUDIR,C'-'          UPWARD DISPLAY STARTS AT                     
         BNE   *+8                                                              
         IC    RF,LUNLINS          BOTTOM OF SCREEN                             
         STC   RF,SVSCRLIN         SET STARTING SCREEN LINE                     
*                                                                               
         MVC   SVTBLINC,LUSVLEN    SET TABLE INCREMENTS FOR TABLE               
         MVC   SVSCRINC,=H'2'        AND SCREEN                                 
*                                                                               
         LA    R6,PRTAB            DEFAULT PREVIOUS TABLE ENTRY TO              
*                                   A WORK AREA                                 
         XC    PRTAB,PRTAB         CLEAR AREA TO FORCE 1ST DISPLAYABLE          
*                                                                               
         CLI   LUNEW,C'Y'          FIRST TIME?                                  
         BE    LDPOSX                                                           
*                                                                               
         CLI   LUDIR,C'='          IF RE-DISPLAYING                             
         BNE   LDPOS02                                                          
         L     R4,LUSVTAB          MOVE CURRENT FIRST TO PREVIOUS               
         LH    RF,LUSVLEN                                                       
         BCTR  RF,R0                                                            
         EX    RF,MOVSAV                                                        
         B     LDPOSX                                                           
*                                                                               
LDPOS02  DS    0H                                                               
         CLI   LUDIR,C'-'          TEST FOR UPWARD SCROLL                       
         BNE   LDPOS10                                                          
*                                                                               
         LH    RF,SVTBLINC         REVERSE SIGN OF TABLE AND                    
         LNR   RF,RF                 SCREEN INCREMENTS                          
         STH   RF,SVTBLINC                                                      
         LH    RF,SVSCRINC                                                      
         LNR   RF,RF                 SCREEN INCREMENTS                          
         STH   RF,SVSCRINC                                                      
*                                                                               
         TM    LUPOS,LULASTQ       LAST FIELD ON SCREEN IS SPECIAL              
         BO    LDPOS05               CASE                                       
         EJECT                                                                  
         ZIC   RE,SVSCROLL         SAVE TABLE ENTRY                             
         ZIC   RF,LUNLINS          EQUALS BOTTOM MINUS SCROLL FACTOR            
         SR    RF,RE                                                            
         LH    RE,LUSVLEN          GET LENGTH OF TABLE ENTRY                    
         MR    RE,RE               GET DISPLACEMENT INTO TABLE                  
         L     R4,LUSVTAB          POINT TO SAVE TABLE                          
         LA    R4,0(RF,R4)         POINT TO ENTRY IN SAVE TABLE                 
         BCTR  RE,0                DECREMENT FOR EXECUTE                        
         ZIC   R3,SVSCRLIN         GET STARTING DISPLAY LINE                    
LDPOS03  DS    0H                  BACK UP TILL NON-NULL ENTRY FOUND            
         EX    RE,CHKSAV           OKAY IF NON-NULL SAVE AREA FOUND             
         BNZ   LDPOS04                                                          
         BCTR  R3,0                BACK UP STARTING DISPLAY LINE                
         BCTR  R5,0                DECREMENT NUMBER OF LINES TO DISPLAY         
         AH    R4,SVTBLINC         BACK UP TO PREVIOUS ENTRY                    
         C     R4,LUSVTAB          KEEP GOING UNLESS PASSED                     
         BNL   LDPOS03             START OF TABLE                               
         LA    R5,1(R5)            RESTORE NUMBER OF LINES TO DISPLAY           
         B     LDPOSX              GIVE UP - WHOLE SCREEN CLEAR                 
*                                                                               
LDPOS04  DS    0H                                                               
         CLM   R5,1,LUNLINS        DEFAULT TO END OF LIST IF                    
         BNE   LDPOS05A              AT LEAST ONE NULL SAVEAREA                 
         STC   R3,SVSCRLIN         RE-SET STARTING DISPLAY LINE                 
         LR    R6,R4               RE-SET SAVE AREA POINTER                     
         B     LDPOSX                                                           
*                                                                               
LDPOS05A DS    0H                                                               
         LA    R3,1(R3)            RESET STARTING LINE                          
         STC   R3,SVSCRLIN         RE-SET STARTING DISPLAY LINE                 
         LA    R5,1(R5)            RESET NUMBER TO DISPLAY                      
LDPOS05  DS    0H                                                               
         MVI   PRTAB,X'FF'         SET WORK AREA TO HIGH-VALUES                 
         MVC   PRTAB+1(L'PRTAB-1),PRTAB                                         
         B     LDPOSX                                                           
*                                                                               
LDPOS10  DS    0H                                                               
         TM    LUPOS,LUFRSTQ       FIRST FIELD ON SCREEN DONE                   
         BO    LDPOSX                                                           
*                                  GET ADDRESS OF PRIOR                         
         ZIC   RF,SVSCROLL           SAVE TABLE ENTRY                           
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         LH    RE,LUSVLEN          GET LENGTH OF TABLE ENTRY                    
         MR    RE,RE               GET DISPLACEMENT INTO TABLE                  
         L     R6,LUSVTAB          POINT TO SAVE TABLE                          
         LA    R6,0(RF,R6)         POINT TO ENTRY IN SAVE TABLE                 
         B     LDPOSX                                                           
*                                                                               
LDPOSX   DS    0H                                                               
         ST    R6,LUAPTAB          SET STARTING PRIOR TABLE ENTRY               
*                                                                               
LDDSP    DS    0H                    DISPLAYING                                 
         ZIC   R3,SVSCRLIN           GET FIRST DISPLAY LINE                     
         GOTO1 DSPMULT,PARMS,(R3),(R5)  DO DISPLAY                              
         B     LUX                                                              
*                                                                               
         DROP  R8                                                               
         SPACE 2                                                                
***********************************************************************         
*     PROGRAM EXITS                                                   *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
LUX      DS    0H                                                               
         MVI   LUNEW,C'N'          RESET 'NEW KEY' SWITCH                       
         CLI   LUERR,0                                                          
         XIT1                                                                   
         SPACE 2                                                                
CLRSAV   XC    0(0,R4),0(R4)                                                    
CHKSAV   OC    0(0,R4),0(R4)                                                    
MOVSAV   MVC   0(0,R6),0(R4)                                                    
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO DISPLAY MULTIPLE SCREEN LINES                            *         
*                                                                     *         
* ENTRY - PARM1 = NUMBER OF FIRST LINE TO DISPLAY                     *         
*         PARM2 = NUMBER OF LINES TO DISPLAY                          *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
DSPMULT  NTR1                                                                   
*                                                                               
         L     R3,0(R1)            GET NUMBER OF FIRST SCREEN LINE              
         L     R5,4(R1)            NUMBER OF LINES                              
DMREST   DS    0H                  BRANCH BACK HERE TO FINISH SCREEN            
         BCTR  R3,0                DECREMENT FOR INDEXING                       
         LR    R4,R3               SAVE                                         
         SLL   R3,1                CALCULATE SCREEN TABLE INDEX                 
         L     RF,LUADSPS          POINT TO SCREEN DISPLACEMENTS                
         LA    R3,0(R3,RF)         POINT TO DISP OF STARTING LINE               
         MH    R4,LUSVLEN          CALCULATE SAVE TABLE INDEX                   
         L     RF,LUSVTAB          POINT TO SAVE TABLE                          
         LA    R4,0(R4,RF)         POINT TO SAVE TABLE ENTRY                    
         LA    R6,PRTAB            POINT TO WORKAREA FOR RE-DISPLAY             
         MVI   LUMODE,LUDSPQ                                                    
*                                                                               
DMLOOP   DS    0H                                                               
         TM    LUSTAT,LUEOLQ       DONE IF LAST FIELD WAS LAST TO BE            
         BO    DMLPDN              DISPLAYED                                    
         LH    R1,0(R3)                                                         
         A     R1,LUATWA           POINT TO FIELD HDR OF SCREEN LINE            
*                                  DISPLAY A LINE                               
         ST    R1,LUACLIN          A(CURRENT LINE)                              
         ST    R4,LUACTAB          A(CURRENT TABLE ENTRY)                       
         LH    RF,LUSVLEN          CLEAR THE ENTRY                              
         BCTR  RF,R0                                                            
         EX    RF,CLRSAV           CLEAR CURRENT TABLE ELEMENT                  
         BAS   RE,HOOK                                                          
*                                                                               
         CLI   LUDIR,C'='          REDISPLAY ONLY ONE TIME                      
         BNE   *+8                                                              
         MVI   LUDIR,C'+'                                                       
*                                                                               
         MVC   LUAPTAB,LUACTAB     CURRENT BECOMES PREVIOUS                     
         LH    RF,LUSVLEN          TEST ANY DATA                                
         BCTR  RF,R0                                                            
         EX    RF,CHKSAV                                                        
         BZ    DMLPDN              NO, DONE                                     
         AH    R4,SVTBLINC         POINT TO NEXT SAVE TABLE ENTRY               
         AH    R3,SVSCRINC         POINT TO NEXT SCREEN FIELD                   
         BCT   R5,DMLOOP                                                        
         EJECT                                                                  
*                                                                               
*                                  END OF SCREEN                                
*                                                                               
         TM    LUSTAT,LUEOLQ       DONE IF USER HAS NO MORE                     
         BO    DMX                                                              
         MVI   LUMODE,LUMOREQ      TELL USER WINDOW IS FULL                     
         BAS   RE,HOOK                                                          
         B     DMX                                                              
*                                                                               
DMLPDN   DS    0H                  CLEAR REST OF WINDOW                         
         S     R3,LUADSPS                                                       
         BM    DMX                 SCREEN FULL SHOULD NOT HAPPEN HERE           
         SRL   R3,1                                                             
         LA    R3,1(R3)            R3 HAS NUMBER OF 1ST TO BE CLEARED           
*                                                                               
         CLI   LUDIR,C'-'          MOVE LINES IF BACKWARDS DISPLAY              
         BNE   DMMOVENO                                                         
         LA    R3,1(R3)            R3 HAS NUMBER OF 1ST TO BE MOVED             
         ZIC   R4,LUNLINS          NUMBER TO MOVE = MAX - # DISPLAYED           
         SR    R4,R5                                                            
         BNP   DMMOVEN1            NONE TO MOVE IF MAX DISPLAYED                
*                                                                               
         GOTO1 MOVMULT,PARMS,(R3),1,(R4)                                        
*                                                                               
DMMOVEN1 DS    0H                                                               
         MVI   LUDIR,C'+'          CHANGE TO DOWN DIRECTION                     
         MVC   SVTBLINC,LUSVLEN    RESET TO POSITIVE INCREMENTS                 
         MVC   SVSCRINC,=H'2'                                                   
         LR    R3,R4               FIRST TO USE = 1 AFTER LAST MOVED            
         AH    R3,=H'1'                                                         
         LTR   R6,R4               LAST DISPLAYED = NUMBER MOVED                
         BNZ   DMMOVEN2            DEFAULT TO WORK AREA IF WHOLE SCREEN         
         LA    R6,PRTAB                                                         
         XC    PRTAB,PRTAB         INDICATE NO PRIOR ENTRY                      
         B     DMMOVEN3                                                         
DMMOVEN2 DS    0H                                                               
         BCTR  R6,0                DECREMENT FOR EXECUTE                        
         MH    R6,LUSVLEN          POINT TO SAVE TABLE ENTRY                    
         A     R6,LUSVTAB           FOR LAST LINE DISPLAYED                     
DMMOVEN3 DS    0H                                                               
         ST    R6,LUAPTAB          SET PRIOR SAVE TABLE ENTRY                   
         NI    LUSTAT,X'FF'-LUEOLQ TURN OFF END OF SCREEN IND                   
*                                  R5 CONTAINS NUMBER OF LINES LEFT             
*                                    ON SCREEN                                  
         B     DMREST                                                           
*                                                                               
DMMOVENO DS    0H                                                               
         GOTO1 CLRMULT,PARMS,(R3),(R5) CLEAR REST OF SCREEN                     
DMX      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO MOVE SCREEN LINES                                        *         
*                                                                     *         
* ENTRY - PARM1 = NUMBER OF FIRST LINE TO MOVE                        *         
*         PARM2 = NUMBER OF FIRST LINE TO MOVE TO                     *         
*         PARM3 = NUMBER OF LINES TO MOVE                             *         
*                                                                     *         
*   NOTE- CODE DEPENDS UPON EACH LINE BEING CONFIGURED IDENTICALLY    *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
MOVMULT  NTR1                                                                   
         L     R3,0(R1)            FIRST LINE NUMBER                            
         CLM   R3,1,LUNLINS        MUST BE WITHIN DISPLAY                       
         BH    MMX                                                              
         BCTR  R3,0                                                             
         LR    R4,R3                                                            
         SLL   R3,1                X 2                                          
         A     R3,LUADSPS          D(FIRST ONE TO DISPLAY)                      
         MH    R4,LUSVLEN          GET TABLE ENTRY                              
         A     R4,LUSVTAB                                                       
         L     R5,4(R1)            FIRST LINE TO MOVE TO                        
         BCTR  R5,R0                                                            
         LR    R6,R5                                                            
         SLL   R5,1                X 2                                          
         A     R5,LUADSPS          D(FIRST ONE TO DISPLAY)                      
         MH    R6,LUSVLEN          GET TABLE ENTRY                              
         A     R6,LUSVTAB                                                       
         L     R0,8(R1)            NUMBER OF LINES                              
*                                                                               
MM8      DS    0H                                                               
         LH    R1,0(R3)            R1 = FROM LINE                               
         A     R1,LUATWA                                                        
         LH    R2,0(R5)            R2 = TO LINE                                 
         A     R2,LUATWA                                                        
         ZIC   R7,LUNFLDS          NUMBER OF FIELDS/LINE                        
*                                                                               
MM10     DS    0H                                                               
         MVC   HALF,FLDADR-FLDHDRD(R2)   SAVE SCREEN ADDRESS                    
         ZIC   RF,0(R1)                  LENGTH OF FIELD PLUS HEADER(S)         
         BCTR  RF,R0                                                            
         TM    LUCNTL,LUNOPROQ     SKIP PROTECTED FIELDS                        
         BNO   *+8                                                              
         TM    FLDATB-FLDHDRD(R1),FATBPROT  TEST IF PROTECTED                   
         BO    *+8                   SKIP IF IT IS                              
         EX    RF,MOVFLD                                                        
         MVC   FLDADR-FLDHDRD(2,R2),HALF   RESTORE SCREEN ADDRESS               
         OI    FLDOIND-FLDHDRD(R2),X'80'   TRANSMIT                             
*                                                                               
         LA    R1,1(R1,RF)         BUMP FROM FIELD                              
         LA    R2,1(R2,RF)         AND TO FIELD                                 
         BCT   R7,MM10                                                          
*                                                                               
         LH    RF,LUSVLEN                                                       
         BCTR  RF,R0                                                            
         EX    RF,MOVSAV                                                        
*                                                                               
         AH    R4,LUSVLEN          NEXT ENTRY                                   
         LA    R3,2(R3)            NEXT FIELD                                   
         AH    R6,LUSVLEN          NEXT TO ENTRY                                
         LA    R5,2(R5)            NEXT TO FIELD                                
         BCT   R0,MM8                                                           
*                                                                               
MMX      DS    0H                                                               
         XIT1                                                                   
         SPACE 1                                                                
MOVFLD   MVC   0(0,R2),0(R1)                                                    
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* PASS CONTROL TO USER HOOK ROUTINE                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         L     RF,LUHOOK                                                        
         L     RE,USERRD           USER RD                                      
         LM    R1,RC,24(RE)        USERS R1-RC                                  
         BASR  RE,RF                                                            
*                                                                               
         L     RD,4(RD)                                                         
         LM    RE,RC,12(RD)        RESTORE LINUP'S REGS                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO CLEAR MULTIPLE SCREEN LINES AND TABLE ENTRIES            *         
*                                                                     *         
* ENTRY - 0(R1) = NUMBER OF FIRST LINE TO CLEAR                       *         
*         4(R1) = NUMBER OF LINES TO CLEAR                            *         
* NOTE- ONLY WORKS 'DOWNWARD'                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
CLRMULT  NTR1                                                                   
         L     R3,0(R1)            FIRST LINE NUMBER                            
         BCTR  R3,R0                                                            
         ICM   R5,15,4(R1)         NUMBER OF LINES                              
         BZ    CLRMX               EXIT IF SCREEN FULL                          
         LR    R4,R3                                                            
         SLL   R3,1                X 2                                          
         A     R3,LUADSPS          D(FIRST ONE TO CLEAR)                        
         MH    R4,LUSVLEN          GET TABLE ENTRY                              
         A     R4,LUSVTAB                                                       
*                                                                               
CLRMLOOP DS    0H                                                               
         LH    R1,0(R3)                                                         
         LTR   R1,R1                                                            
         BZ    CLRMX               DONE IF AT EOS                               
         A     R1,LUATWA                                                        
         BAS   RE,CLRLIN           CLEAR LINE                                   
         LH    R1,LUSVLEN          CLEAR TABLE ENTRY                            
         BCTR  R1,R0                                                            
         EX    R1,CLRSAV                                                        
         AH    R3,=H'2'            NEXT FIELD                                   
         AH    R4,LUSVLEN          NEXT ENTRY                                   
         BCT   R5,CLRMLOOP                                                      
*                                                                               
CLRMX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO CLEAR A SCREEN LINE                                      *         
*                                                                     *         
* ENTRY - R1 = A(FIRST FIELD OF LINE)                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
CLRLIN   NTR1                                                                   
         LR    R2,R1               A(FIRST FIELD)                               
         ZIC   R3,LUNFLDS          NUMBER OF FIELDS                             
*                                                                               
CLRL2    DS    0H                                                               
         TM    LUCNTL,LUNOPROQ     SKIP PROTECTED FIELDS                        
         BNO   *+8                                                              
         TM    FLDATB-FLDHDRD(R2),FATBPROT  TEST IF PROTECTED                   
         BO    CLRL4                 SKIP IF IT IS                              
         MVI   FLDIIND-FLDHDRD(R2),FINPVAL SET FIELD AS VALID                   
         MVI   FLDILEN-FLDHDRD(R2),0   CLEAR INPUT LENGTH                       
         NI    FLDOIND-FLDHDRD(R2),FOUTTRN TURN OFF ALL BUT TRANSMIT            
         MVI   FLDOLEN-FLDHDRD(R2),0   CLEAR INPUT LENGTH                       
         ZIC   RF,FLDLEN-FLDHDRD(R2)                                            
         SH    RF,=Y(FLDHDRL+1)                                                 
         TM    FLDATB-FLDHDRD(R2),FATBXHDR TEST EXTENDED                        
         BZ    *+8                                                              
         SH    RF,=Y(FLDHDRL)                                                   
         EX    RF,CLRLOC           OR WITH SPACES                               
         EX    RF,CLRLCLC          COMPARE TO SPACES                            
         BE    CLRL4                                                            
         EX    RF,CLRLMVC          SET TO SPACES                                
         MVI   FLDOIND-FLDHDRD(R2),FOUTTRN   TRANSMIT                           
*                                                                               
CLRL4    DS    0H                                                               
         BAS   RE,BUMP                                                          
         BCT   R3,CLRL2                                                         
*                                                                               
CLRLX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
CLRLOC   OC    8(0,R2),SPACES                                                   
CLRLCLC  CLC   8(0,R2),SPACES                                                   
CLRLMVC  MVC   8(0,R2),SPACES                                                   
         EJECT                                                                  
**********************************************************************          
*                                                                    *          
**********************************************************************          
         SPACE 1                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
         SPACE 3                                                                
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
*                                                                               
SPACES   DC    CL80' '                                                          
PATCH    DC    XL64'00'                                                         
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
LUWRKD   DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
RELO     DS    F                                                                
USERRD   DS    F                                                                
SVAPARM  DS    F                                                                
         DS    0F                                                               
PARMS    DS    0XL24                                                            
PARM1    DS    F                                                                
PARM2    DS    F                                                                
PARM3    DS    F                                                                
         DS    3F                                                               
*                                                                               
HALF     DS    H                                                                
SVTBLINC DS    H                   SAVE TABLE INCREMENT                         
SVSCRINC DS    H                   SCREEN DISP INCREMENT                        
SVSCRLIN DS    X                   STARTING SCREEN LINE NUMBER                  
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
REDSPSW  DS    C                                                                
SVSCROLL DS    X                                                                
SVDIR    DS    C                                                                
SVPFK    DS    XL1                                                              
WORK     DS    XL80                                                             
PRTAB    DS    XL256               SAVE AREA FOR PREVIOUS TABLE ENTRY           
LUWRKL   EQU   *-LUWRKD                                                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDLINUPD                                                       
*                                                                               
*  INCLUDE DDFLDHDR/FATIOB                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
*                                                                               
FLDHDRL  EQU   FLDDATA-FLDHDRD   FIELD HEADER LENGTH                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032DDLINUP   05/01/02'                                      
         END                                                                    
