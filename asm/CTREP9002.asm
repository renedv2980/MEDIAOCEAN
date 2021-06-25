*          DATA SET CTREP9002  AT LEVEL 062 AS OF 09/15/14                      
*PHASE CT9002A                                                                  
         SPACE 2                                                                
*******************************************************************             
*       --REQUEST CARD FIELDS--                                   *             
*                                                                 *             
* QPROG    1       C'90'                                          *             
* QID      21      OPTIONAL START(6) OR START(6)/END(6) ACTIVITY  *             
*                  DATE FILTERS                                   *             
* QFILTER1 34      FILE/MEDIA/SOURCE FILTER (3)                   *             
* QSTART   37      START BOOK FILTER (YYMM)                       *             
* QOPT1    60      Y=PRINT OPERAND PRECISIONS                     *             
* QOPT2    61      Y=PRINT ALL BOOKS FOR A DEMO                   *             
*                  DEFAULT=SHOW HIGHEST BOOK FOR A DEMO--ACTIVITY *             
*                  DATE PRINTS INSTEAD OF BOOK.                   *             
* QOPT3    62      Y=ONLY PRINT FORMULAS USING SPECIAL OPERATORS  *             
* QOPT4    63      Y=PRINT DEMO NUMBERS INSTEAD OF EXPRESSIONS    *             
* QOPT5    64      DEMO MODIFIER FILTER                           *             
*                   OR "|" FOR ALL PERSONAL LANGUAGE FORMULAS     *             
*******************************************************************             
CT9002   TITLE '-   DEMO FORMULA LISTING'                                       
CT9002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TEMPX-TEMPD,**CT9002,R9                                          
         USING TEMPD,RC                                                         
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         SPACE 2                                                                
* RUNFRST PROCESSING - INITIALIZE TRT SCAN TABLES                               
*                                                                               
FORM     DS    0H                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   FORM2                                                            
*                                                                               
         XC    STARTAB,STARTAB     CLEAR SCAN TABLES FOR INITIALIZATION         
         XC    TERMTAB,TERMTAB                                                  
         XC    OPTABLE,OPTABLE                                                  
         XC    SPECTAB,SPECTAB                                                  
*                                  OPERAND START CHARACTERS                     
         MVI   STARTAB+X'C1',LETTER                                             
         MVC   STARTAB+X'C2'(8),STARTAB+X'C1'                                   
         MVI   STARTAB+X'D1',LETTER                                             
         MVC   STARTAB+X'D2'(8),STARTAB+X'D1'                                   
         MVI   STARTAB+X'E2',LETTER                                             
         MVC   STARTAB+X'E3'(7),STARTAB+X'E2'                                   
         MVI   STARTAB+240,NUMERIC                                              
         MVC   STARTAB+241(9),STARTAB+240                                       
*                                                                               
*                                  OPERAND TERMINATORS                          
         MVI   TERMTAB+SPACE,SPACE                                              
         MVI   TERMTAB+RPAREN,RPAREN                                            
         MVI   TERMTAB+COMMA,PCISION                                            
         MVI   TERMTAB+PERIOD,PCISION                                           
         MVI   TERMTAB+QUOTE,QUOTE                                              
         MVI   TERMTAB+DBLQUOTE,DBLQUOTE                                        
         MVI   TERMTAB+EXCLAM,EXCLAM                                            
         MVI   TERMTAB+ADD,OPERATOR                                             
         MVI   TERMTAB+SUBTRACT,OPERATOR                                        
         MVI   TERMTAB+MULTIPLY,OPERATOR                                        
         MVI   TERMTAB+DIVIDE,OPERATOR                                          
*                                                                               
*                                  OPERATORS                                    
         MVI   OPTABLE+ADD,OPERATOR                                             
         MVI   OPTABLE+SUBTRACT,OPERATOR                                        
         MVI   OPTABLE+MULTIPLY,OPERATOR                                        
         MVI   OPTABLE+DIVIDE,OPERATOR                                          
*                                                                               
*                                  SPECIAL OPERATORS                            
         MVI   SPECTAB+QUOTE,QUOTE                                              
         MVI   SPECTAB+DBLQUOTE,DBLQUOTE                                        
         MVI   SPECTAB+EXCLAM,EXCLAM                                            
*                                                                               
         GOTO1 DEMOCON,DMCB,(0,0),('DEMOCON_14',APLDTABS),0                     
*                                                                               
         B     FORMOUT                                                          
         EJECT                                                                  
* REQFRST PROCESSING - READ DEMO FORMULA RECORDS AND                            
* WRITE SELECTED RECORDS TO SORT FILE                                           
*                                                                               
FORM2    DS    0H                                                               
         CLI   MODE,REQFRST                                                     
         BNE   FORM20                                                           
*                                                                               
         LHI   RF,TEMPX-TEMPD                                                   
         LR    RE,RC                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0               ZERO OUT WORKING STORAGE                     
*                                                                               
         BAS   RE,SETACT           SET ACTIVITY DATE FILTERS                    
*                                                                               
         CLI   SORTINIT,YES        PREVENT DOUBLE SORTER INIT                   
         BE    FORM3                                                            
         GOTO1 SORTER,DMCB,SORTFLD,RECTYPE                                      
         MVI   SORTINIT,YES                                                     
*                                                                               
FORM3    DS    0H                  READ FORMULAS-WRITE TO SORT                  
         LA    R4,KEY                                                           
         USING CTGREC,R4                                                        
         XC    KEY,KEY             BUILD KEY                                    
         MVI   CTGKTYP,CTGKTEQU                                                 
         LA    R5,14                                                            
         CLI   QFILTER1,SPACE                                                   
         BE    *+14                                                             
         MVC   CTGKFMS,QFILTER1                                                 
         LA    R5,17                                                            
*                                                                               
         XC    HALF,HALF                                                        
         CLC   QSTART,SPACES                                                    
         BE    FORM4                                                            
         MVC   DUB(4),QSTART       SET UP START BOOK FILTER                     
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,DUB,(3,THREE)                                        
         MVC   HALF,THREE          BINARY YM                                    
         XC    HALF,=X'FFFF'       COMPLEMENT IT                                
*                                                                               
FORM4    DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     FORM6                                                            
*                                                                               
FORM5    DS    0H                                                               
         BAS   RE,SEQ                                                           
*                                                                               
FORM6    DS    0H                                                               
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   FORMOUT             ALL DONE WITH READ                           
         OC    HALF,HALF           START DATE FILTERING                         
         BZ    FORM8               NO                                           
         CLC   CTGKSTRT,HALF       YES-KEY AGAINST FILTER                       
         BNL   FORM8                                                            
         MVC   CTGKSTRT,HALF       GO BACK TO READ HIGH                         
         XC    CTGKDEMO,CTGKDEMO                                                
         B     FORM4                                                            
*                                                                               
FORM8    DS    0H                                                               
         CLI   QOPT5,C' '          DEMO MODIFIER FILTER PRESENT?                
         BE    FORM8G              NO                                           
         CLI   QOPT5,C'|'          ALL PERSONAL LANGUAGE FORMULAS ONLY?         
         BNE   FORM8E              NO                                           
         LLC   RE,CTGKDEMO         YES: EXTRACT PERSONAL LANGUAGE               
         A     RE,APLPLDTB                                                      
         CLI   0(RE),C' '          IS THIS A PERSONAL LANGUAGE FORMULA?         
         BNH   FORM5               NO: SKIP THIS RECORD                         
         B     FORM8G              YES: PROCESS IT                              
*                                                                               
FORM8E   DS    0H                                                               
         LLC   RE,CTGKDEMO         YES: EXTRACT EBCDIC MODIFIER                 
         A     RE,APLMODTB                                                      
         CLC   QOPT5,0(RE)         MATCH ON DEMO MODIFIER FILTER?               
         BNE   FORM5               NO: SKIP THIS RECORD                         
*                                                                               
FORM8G   DS    0H                                                               
*                                  PROCESS RECORD                               
         L     R4,ADRECORD                                                      
         LA    R3,IO               BUILD A SORT RECORD                          
         USING SORTD,R3                                                         
         LR    RE,R3               AT IO                                        
         LA    RF,L'IO                                                          
         XR    R1,R1                                                            
         MVCL  RE,R0               ZERO SORT RECORD AREA                        
*                                                                               
         MVC   SORTFMS,CTGKFMS                                                  
         MVC   SORTAGY,CTGKAGY                                                  
         MVC   SORTLCOD,CTGKCODE                                                
         MVC   SORTDEMO,CTGKDEMO                                                
         MVC   SORTSBK,CTGKSTRT                                                 
         LA    RE,SORTDATA-SORTD                                                
         STH   RE,SORTRLEN                                                      
         LA    R7,SORTDATA                                                      
*                                                                               
         L     R6,ADACTIV          EXTRACT ACTIVITY ELEMENT AND                 
         USING CTACTD,R6                                                        
         CLC   CTACTDT,ACTFRST     APPLY ACTIVITY DATE FILTER                   
         BL    FORM5               REJECT RECORD                                
         CLC   CTACTDT,ACTLAST                                                  
         BH    FORM5                                                            
         BAS   RE,MOVEREC          MOVE ELEM TO SORT RECORD                     
*                                                                               
         MVI   ELCODE,CTDSCELQ     COMMENT ELEMENT                              
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         BAS   RE,MOVEREC                                                       
*                                                                               
         MVI   ELCODE,CTPRECDQ     PRECISION ELEMENT                            
         LR    R6,R4               A(RECORD)                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,MOVEREC                                                       
         MVC   BYTE,2(R6)          SAVE PRECISION BYTE                          
*                                                                               
         MVI   ELCODE,CTDPFCDQ     POLISH FORMULA ELEMENT                       
         LR    R6,R4               A(RECORD)                                    
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         BAS   RE,MOVEREC                                                       
*                                                                               
         MVI   ELCODE,CTDINCDQ     INPUT FORMULA ELEMENT                        
         LR    R6,R4               REFRESH RECORD ADDRESS                       
         BAS   RE,GETEL                                                         
         BE    FORM9                                                            
*                                                                               
* BUILD A DUMMY FORMULA ELEMENT IF RECORD HAS NONE                              
*                                                                               
         ST    R7,AINPEL           SAVE A(ELEMENT START)                        
         MVI   0(R7),CTDINCDQ      ELEMENT CODE                                 
*                                                                               
*                                  PUT MODIFIER (AND PLD) IN ELEMENT            
         GOTO1 DEMOCON,DMCB,(0,CTGKDEMO),('DEMOCON_15',3(R7)),0                 
         LLC   RF,DMCB             NUMBER OF RETURNED CHARACTERS                
         LA    R2,3(RF,R7)         R2 = A(NEXT OUTPUT POSITION)                 
         LHI   R1,3                DEVELOP EL LENGTH IN R1                      
         AR    R1,RF               BUMP ELEMENT LENGTH ACCORDINGLY              
         EDIT  (B1,CTGKDEMO+1),(3,(R2)),ALIGN=LEFT,ZERO=NOBLANK                 
         AR    R1,R0               ADD LENGTH OF NUMBER                         
*                                                                               
         LA    RE,0(R1,R7)         POINT RE AT END OF DATA                      
         MVI   0(RE),PERIOD        AND OUTPUT A PRECISION CODE                  
         TM    BYTE,X'80'          DECIMAL PRECISION                            
         BO    *+8                 YES                                          
         MVI   0(RE),COMMA         NO-MOVE IN A COMMA                           
         MVC   1(1,RE),BYTE        CONVERT LOWER 4 BITS FOR PLACES              
         OI    1(RE),X'F0'         INTO NUMERIC FORMAT FOR SCANNING             
         LA    R1,2(R1)            UPDATE ELEMENT LENGTH                        
         LR    R0,R1               SAVE EL LEN IN R0                            
         AH    R1,SORTRLEN                                                      
         STH   R1,SORTRLEN                                                      
         B     FORM12                                                           
*                                                                               
FORM9    DS    0H                                                               
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8              ELEMENT TO SORT RECORD                       
         B     *+10                                                             
         MVC   0(0,R7),0(R6)                                                    
         LA    R1,1(R1)                                                         
         LR    R0,R1               SAVE FIRST ELEMENT LENGTH                    
         ST    R7,AINPEL           SAVE A(ELEMENT) ON SORT RECORD               
         AR    R7,R1                                                            
         AH    R1,SORTRLEN                                                      
         STH   R1,SORTRLEN                                                      
*                                                                               
* CONCATENATE ANY SUBSEQUENT INPUT FORMULA ELEMENTS TO                          
* THE FIRST ONE TO MAKE ONE ELEMENT ON SORT RECORD                              
*                                                                               
FORM10   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   FORM12                                                           
         ZIC   R1,1(R6)                                                         
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),3(R6)       ELEMENT DATA                                 
         LA    R1,1(R1)            RESTORE DATA LENGTH                          
         AR    R0,R1               UPDATE RUNNING ELEMENT LENGTH                
         AR    R7,R1               UPDATE SORT RECORD POINTER                   
         AH    R1,SORTRLEN                                                      
         STH   R1,SORTRLEN                                                      
         B     FORM10              GO BACK TO LOOK FOR MORE                     
*                                                                               
FORM12   DS    0H                                                               
         L     RE,AINPEL           PUT EL LENGTH AT START OF                    
         STC   R0,1(RE)            CONCATENATED INPUT PREC EL                   
         CLI   QOPT3,YES           TEST TO APPLY SPEC OP FILTER                 
         BNE   *+12                                                             
         BAS   RE,SPECOP                                                        
         BE    FORM5               SKIP THIS FORMULA RECORD                     
         GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
         MVI   SORTINIT,NO                                                      
         B     FORM5               RETURN TO READ SEQ                           
         SPACE 2                                                                
* SUB-ROUTINE TO MOVE AN ELEMENT TO SORT RECORD                                 
*                                                                               
MOVEREC  ZIC   R1,1(R6)                                                         
         BCTR  R1,0                MOVE IT TO SORT RECORD                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R6)                                                    
         LA    R1,1(R1)            RESTORE EL LENGTH                            
         AR    R7,R1               UPDATE DATA POINTER                          
         AH    R1,SORTRLEN         UPDATE SORT RECORD LEN                       
         STH   R1,SORTRLEN                                                      
         BR    RE                                                               
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
* REQLAST PROCESSING - READ SORTED RECORDS AND PRINT REPORT                     
*                                                                               
FORM20   DS    0H                                                               
         CLI   MODE,REQLAST                                                     
         BNE   FORMOUT                                                          
*                                                                               
         CLI   SORTINIT,YES                                                     
         BE    FORMOUT             NOTHING IN SORT                              
*                                                                               
         MVC   CONFIL,SPACES                                                    
         XC    LASTDEM,LASTDEM                                                  
         MVI   FORCEHED,YES                                                     
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,0                                                       
         CLI   QOPT2,YES                                                        
         BNE   *+8                                                              
         MVI   RCSUBPRG,1                                                       
         CLC   QSTART,SPACES                                                    
         BE    FORM22                                                           
         MVC   DUB(4),QSTART       CONVERT START BOOK TO OUTPUT                 
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,DUB,(6,STBOOK)                                       
*                                                                               
FORM22   DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R3,15,DMCB+4                                                     
         BZ    FORMOUT             FINITO                                       
         CLI   QOPT2,YES           OPTION TO PRINT ALL BOOKS                    
         BE    *+14                YES                                          
         CLC   LASTDEM,SORTDEMO    TAKE THE HIGHEST BOOK FOR                    
         BE    FORM22              THE SAME DEMO                                
         ST    R3,ADRECORD         SAVE A(SORTED RECORD)                        
*                                                                               
         BAS   RE,GETELS           GET ELEMENT ADDRESSES                        
*                                                                               
*                                  CONTROL BREAKS IN FILE/MEDIA/SOURCE          
         CLC   CONFIL,SPACES                                                    
         BE    *+14                FIRST TIME THROUGH                           
         CLC   CONFIL,SORTFILE                                                  
         BE    FORM23A             NO BREAK                                     
*                                                                               
         MVC   CONFIL,SORTFILE                                                  
         XC    LASTDEM,LASTDEM     RESET LAST DEMO CONTROL                      
         MVI   FORCEHED,YES                                                     
*                                                                               
FORM23A  CLC   SORTMOD,LASTDEM     TEST FOR CHANGE IN MODIFIER                  
         BE    *+8                                                              
         BAS   RE,PRTCOM           YES-PRINT THE DEMO COMMENT                   
         MVC   LASTDEM,SORTDEMO    UPDATE LAST DEMO                             
*                                                                               
         CLI   QOPT2,YES           BOOK NEEDED INSTEAD OF ACTIVITY              
         BE    FORM24              YES                                          
*                                                                               
         L     R6,AACTEL           PUT OUT ACTIVITY DATE                        
         USING CTACTD,R6                                                        
         LA    R2,P+88                                                          
         GOTO1 DATCON,DMCB,(3,CTACTDT),(5,(R2))                                 
         B     FORM25                                                           
         DROP  R6                                                               
*                                                                               
FORM24   DS    0H                                                               
         MVC   THREE(2),SORTSBK    PUT OUT BOOK                                 
         XC    THREE(2),=X'FFFF'   REVERSE THE COMPLEMENT                       
         CLC   CONFIL,=C'TWA'      TEST FOR BBM WEELY                           
         BE    *+10                                                             
         CLC   CONFIL,=C'EIN'      TEST FOR ESTIMATED IMPRESSION                
         BE    *+10                                                             
         CLC   CONFIL,=C'EVN'      TEST FOR ESTIMATED VPH                       
         BE    *+10                                                             
         CLC   CONFIL,=C'CNN'      TEST FOR CABLE                               
         BE    *+10                                                             
         CLC   CONFIL,=C'PNN'      TEST FOR NETWORK                             
         BNE   *+12                                                             
         BAS   RE,NETBOOK                                                       
         B     FORM25                                                           
         MVI   THREE+2,X'01'                                                    
         GOTO1 DATCON,DMCB,(3,THREE),(6,P+88)                                   
*                                                                               
FORM25   DS    0H                                                               
*                                  PRINT DECODED MODIFIER (AND PLD)             
         GOTO1 DEMOCON,DMCB,(0,SORTMOD),('DEMOCON_15',P+1),0                    
         LLC   RF,DMCB             NUMBER OF RETURNED CHARACTERS                
         LA    R7,P+1(RF)          R7 = A(NEXT OUTPUT POSITION)                 
         EDIT  SORTDEM#,(3,(R7)),ALIGN=LEFT,ZERO=NOBLANK  DEMO NUMBER           
         CLI   SORTDEM#,0          IS THIS A MACRO FORMULA?                     
         BE    FORM25E             YES                                          
         AR    R7,R0               NO: UPDATE POINTER                           
         LA    R7,1(R7)                                                         
         MVI   0(R7),LPAREN        ENCLOSE CATEGORY IN PARENS                   
         LA    R7,1(R7)                                                         
*                                                                               
FORM25E  XC    THREE,THREE                                                      
         MVC   THREE+1(2),SORTDEMO                                              
*                                                                               
         LLC   RE,THREE+1          EXTRACT THE EBCDIC MODIFIER                  
         A     RE,APLMODTB                                                      
         MVC   THREE+1(1),0(RE)    DON'T PASS ENCODED MOD. TO DEMOCON           
*                                                                               
         LA    R5,DEMAREA                                                       
         USING DEMOD,R5                                                         
         XC    DBLOCK,DBLOCK       SET UP DBLOCK                                
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '      SET DEFAULT VALUE                            
         LA    RE,FILTAB                                                        
         LA    R1,FILES            EXTRACT 3-BYTE FILE CODE FROM                
         CLC   SORTFILE,0(RE)      TABLE TO PUT IN DBFILE                       
         BE    *+16                                                             
         LA    RE,L'FILTAB(RE)                                                  
         BCT   R1,*-14                                                          
         B     FORM26              NOT IN TABLE                                 
*                                                                               
         MVC   DBFILE,1(RE)        FROM TABLE                                   
*                                                                               
FORM26   DS    0H                                                               
         MVI   DBSELMED,C'T'       SET DEFAULT VALUE                            
         LA    RE,MEDTAB                                                        
         LA    R1,MEDIAS           EXTRACT MEDIA CODE FROM TABLE                
         CLC   SORTMED,0(RE)                                                    
         BE    *+16                MEDIA CODE CHECKS AGAINST TABLE              
         LA    RE,L'MEDTAB(RE)                                                  
         BCT   R1,*-14                                                          
         B     FORM27              NOT IN TABLE                                 
*                                                                               
         MVC   DBSELMED,SORTMED                                                 
*                                                                               
         CLI   DBSELMED,C'I'       FOR EIN                                      
         BNE   *+14                                                             
         MVC   DBFILE(3),=C'NTI'                                                
         MVI   DBSELMED,C'N'                                                    
*                                                                               
         CLI   DBSELMED,C'N'                                                    
         BNE   FORM27                                                           
         CLI   DBFILE,C'M'                                                      
         BNE   FORM27                                                           
         MVC   DBFILE(3),=C'NTI'                                                
*                                                                               
FORM27   DS    0H                                                               
         CLI   SORTDEM#,0          BYPASS NAME IF 0                             
         BE    FORM30                                                           
         MVI   DBDEMTYP,0                                                       
         MVC   FOUR(3),THREE                                                    
*                                                                               
         CLI   SORTLCOD,X'FF'                                                   
         BE    FORM29                                                           
         CLI   SORTLCOD,X'00'                                                   
         BE    FORM29                                                           
         XC    FOUR,FOUR                                                        
         MVC   FOUR+1(1),THREE+1                                                
         MVC   FOUR+2(1),SORTLCOD                                               
         MVC   FOUR+3(1),THREE+2                                                
         MVI   DBDEMTYP,C'4'                                                    
*                                                                               
FORM29   DS    0H                                                               
         GOTO1 DEMOCON,DMCB,FOUR,(6,(R7)),DBLOCK                                
         MVI   DBDEMTYP,0                                                       
         CLI   0(R7),SPACE                                                      
         BE    *+12                                                             
         LA    R7,1(R7)                                                         
         B     *-12                                                             
         MVI   0(R7),RPAREN                                                     
         B     FORM30                                                           
         DROP  R5                                                               
*                                                                               
FORM30   DS    0H                                                               
         L     R1,APRECEL                                                       
         USING CTPRECSD,R1                                                      
         CLI   CTPRECFL,CTPRECFL_EQUATED_FORMULA                                
         BE    FORM34                                                           
         BAS   RE,DECODE           PREPARE INPUT FORMULA FOR PRINTING           
         DROP  R1                                                               
         L     R5,WORD             OUTPUT LINES RETURNED BY DECODE              
         L     R2,FILEC            LINES ARE IN FILEC                           
         MVI   BYTE,1                                                           
         MVI   DEMCNT,0                                                         
*                                                                               
FORM31   DS    0H                                                               
         MVC   P+24(60),24(R2)     FORMULA TO PRINT LINE                        
         CLI   QOPT1,SPACE         PRECISION OPTION                             
         BE    FORM35              NO                                           
         CLI   P+1,SPACE           FIRST LINE OF FORMULA                        
         BE    FORM32              NO                                           
*                                                                               
         MVI   PSECOND+1,LPAREN                                                 
         LA    RE,PSECOND+2                                                     
         L     R1,APRECEL                                                       
         USING CTPRECSD,R1                                                      
         TM    CTPRECCD,X'80'      DECIMAL POINT IN OUTPUT PRECISION            
         BZ    *+12                NO                                           
         MVI   0(RE),PERIOD        YES                                          
         LA    RE,1(RE)                                                         
*                                                                               
         LLC   R0,CTPRECCD         HIGHEST 4 BITS ARE                           
         SLL   R0,28               NUMBER OF PLACES                             
         SRL   R0,28                                                            
         LTR   R0,R0                                                            
         BZ    *+20                                                             
         MVI   0(RE),C'0'          INSERT ONE ZERO FOR EACH PLACE               
         LA    RE,1(RE)                                                         
         BCT   R0,*-8                                                           
         B     *+12                                                             
*                                                                               
         MVI   0(RE),C'1'          ZERO PLACES MEAN UNITS                       
         LA    RE,1(RE)                                                         
*                                                                               
         MVI   0(RE),RPAREN                                                     
         LA    RE,2(RE)                                                         
         TM    CTPRECCD,CTPRECCD_FORMULA_PRECISION_ADJUSTMENT                   
         BZ    *+12                                                             
         MVI   0(RE),C'F'                                                       
         LA    RE,2(RE)            BUMP OUTPUT POINTER                          
         TM    CTPRECFL,CTPRECFL_NO_DIRECT_TRANSFER                             
         BO    *+14                NO                                           
         MVC   0(6,RE),=C'DIRECT'                                               
         LA    RE,7(RE)            BUMP OUTPUT POINTER                          
*                                                                               
         TM    CTPRECFL,CTPRECFL_INDEXED_DEMO                                   
         BZ    *+10                                                             
         MVC   0(5,RE),=C'INDEX'                                                
         DROP  R1                                                               
*                                                                               
FORM32   DS    0H                                                               
         BAS   RE,PREC             PUT PRECISIONS UNDERNEATH OPERANDS           
         CLC   PSECOND,SPACES      ANY PRECISIONS FOR LINE                      
         BE    FORM35              NO                                           
         MVI   BYTE,2              YES-BLOCK 2 LINES TOGETHER                   
         B     FORM35                                                           
*                                                                               
FORM34   DS    0H                  EQUATED FORMULA LOGIC                        
         USING CTPRECSD,R1                                                      
         MVC   DUB(1),CTPRECCD     EQUATED MODIFIER                             
         DROP  R1                                                               
         LA    R5,1                ONE LINE OF OUTPUT                           
         MVC   P+24(3),=C'USE'                                                  
*                                  PRINT DECODED MODIFIER (AND PLD)             
         GOTO1 DEMOCON,DMCB,(0,DUB),('DEMOCON_15',P+28),0                       
         LLC   RF,DMCB             NUMBER OF RETURNED CHARACTERS                
         LA    R6,P+28(RF)         R6 = A(NEXT OUTPUT POSITION)                 
         EDIT  SORTDEM#,(3,(R6)),ALIGN=LEFT   DEMO NUMBER                       
*                                                                               
FORM35   DS    0H                                                               
         BAS   RE,PRT              PRINT                                        
         CLI   QOPT1,SPACE                                                      
         BE    *+8                                                              
         BAS   RE,SKIPLIN                                                       
         LA    R2,132(R2)          BUMP POINTER TO NEXT FORMULA LINE            
         BCT   R5,FORM31                                                        
         CLI   QOPT1,SPACE         WHEN PRINTING PRECISIONS,                    
         BNE   *+8                 DO NOT NEED LINE AFTER FORMULA               
         BAS   RE,SKIPLIN          SKIP LINE AFTER FORMULA                      
         MVI   DEMCNT,0                                                         
         OC    APOLEL,APOLEL       TEST FOR POLISH FORMULA ELEMENT              
         BZ    *+8                                                              
         BAS   RE,POLISH           YES-PRINT THE POLISH STRING                  
         B     FORM40                                                           
*                                                                               
FORM40   DS    0H                                                               
         B     FORM22              GET NEXT SORT RECORD                         
*                                                                               
FORMOUT  XMOD1 1                                                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* SUB-ROUTINE TO SET ACTIVITY DATE FILTERS                                      
*                                                                               
SETACT   ST    RE,FULL                                                          
         XC    ACTFRST,ACTFRST     SET FIRST DATE TO ZERO                       
         MVC   ACTLAST,XFF         SET LAST ACTIV FILTER TO MAX                 
         CLC   QID(6),SPACES       TEST FOR ACTIVITY DATE FILTER                
         BE    SETACTX                                                          
         GOTO1 DATCON,DMCB,QID,(3,ACTFRST)                                      
         CLC   QID+6(6),SPACES     TEST FOR END DATE                            
         BE    SETACTX             NO                                           
         GOTO1 (RF),(R1),QID+6,(3,ACTLAST)                                      
*                                                                               
SETACTX  L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SCAN AN INPUT FORMULA ELEMENT FOR A SPECIAL OPERATOR           
*                                                                               
* AT ENTRY, AINPEL=A(FORMULA ELEMENT) AND SPECTAB INITIALIZED                   
* ON EXIT,  CC=EQ IF NO SPECIAL OPERATORS FOUND                                 
*                                                                               
SPECOP   NTR1                                                                   
         L     RE,AINPEL                                                        
         USING CTDINCD,RE                                                       
         LLC   R5,CTDINLEN         GET ELEMENT LENGTH                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     XIT                                                              
         TRT   CTDINCS#(0),SPECTAB                                              
         DROP  RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SET ELEMENT ADDRESSES ON A SORT RECORD                         
*                                                                               
GETELS   ST    RE,FULL                                                          
         XC    AELEMS(AELEMSL),AELEMS                                           
         LA    R6,SORTDATA         R6=A(ELEMENTS)-BXLE INDEX                    
         LH    RF,SORTRLEN                                                      
         LA    RF,SORTD(RF)                                                     
         BCTR  RF,0                RF=LAST BYTE IN RECORD-BXLE LIMIT            
         SR    RE,RE                                                            
*                                                                               
GETELS2  IC    RE,1(R6)            GET ELEM LEN-BXLE INCREMENT                  
         CLI   0(R6),CTACTELQ                                                   
         BNE   *+12                                                             
         ST    R6,AACTEL           ACTIVITY ELEM                                
         B     GETELS4                                                          
*                                                                               
         CLI   0(R6),CTDSCELQ                                                   
         BNE   *+12                                                             
         ST    R6,ACOMEL           COMMENTS ELEM                                
         B     GETELS4                                                          
*                                                                               
         CLI   0(R6),CTPRECDQ                                                   
         BNE   *+12                                                             
         ST    R6,APRECEL          PRECISION ELEM                               
         B     GETELS4                                                          
*                                                                               
         CLI   0(R6),CTDPFCDQ                                                   
         BNE   *+12                                                             
         ST    R6,APOLEL           POLISH FORMULA ELEM                          
         B     GETELS4                                                          
*                                                                               
         CLI   0(R6),CTDINCDQ                                                   
         BNE   *+8                                                              
         ST    R6,AINPEL           INPUT FORMULA ELEMENT                        
*                                                                               
GETELS4  BXLE  R6,RE,GETELS2                                                    
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO PRINT THE COMMENT FOR A DEMO                                   
*                                                                               
PRTCOM   ST    RE,FULL                                                          
         ICM   RF,15,ACOMEL                                                     
         BZR   RE                  NO COMMENT ELEMENT                           
         USING CTDSCD,RF                                                        
         LLC   R1,CTDSCLEN                                                      
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+24(0),CTDSC       MOVE COMMENT TO PRINT LINE                   
         DROP  RF                                                               
         MVI   BYTE,1              PRINT ONE LINE                               
         BAS   RE,PRT                                                           
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO PUT OUT THE NETWORK BOOK IN PRINTABLE FORMAT                   
*                                                                               
NETBOOK  DS    0H                                                               
         ZIC   R0,THREE            YEAR                                         
         ZIC   R1,THREE+1          WEEK                                         
         CVD   R1,DUB              TAKE THE WEEK FIRST                          
         UNPK  P+88(2),DUB+6(2)                                                 
         OI    P+89,X'F0'                                                       
         MVI   P+90,SLASH                                                       
         CVD   R0,DUB              NOW DO THE YEAR                              
         UNPK  P+91(2),DUB+6(2)                                                 
         OI    P+92,X'F0'                                                       
         BR    RE                                                               
         EJECT                                                                  
* DECODE INPUT FORMULA STRING                                                   
*                                                                               
* CONVERT DEMO CODES TO LOGICAL CATEGORIES                                      
* REMOVE PRECISION EXPRESSIONS FROM OUTPUT                                      
* SURROUND OPERATORS WITH BLANKS                                                
*                                                                               
* R1 - ARGUMENT BYTE ADDRESS       R2 - FUNCTION BYTE VALUE                     
* R3 - INPUT POINTER               R4 - OUTPUT POINTER                          
* R5 - BYTES REMAINING TO BE SCANNED                                            
*                                                                               
DECODE   NTR1                                                                   
*                                                                               
         L     RE,AINPEL           INPUT FORMULA ELEMENT                        
         USING CTDINCD,RE                                                       
         LLC   R1,CTDINLEN                                                      
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STRING(0),CTDINIF   MOVE FORMULA TO SCAN AREA                    
         DROP  RE                                                               
         LA    R1,1(R1)            LENGTH OF FORMULA STRING                     
         LA    RE,STRING(R1)       POINT PAST STRING END AND PUT A              
         MVI   0(RE),SPACE         BLANK THERE TO END SCAN.                     
         ST    RE,ASTREND          SAVE A(STRING END)                           
         MVI   TARGET,OPSTART      LOOK FOR OPERAND START FIRST                 
         LA    R3,STRING           INPUT POINTER                                
         LA    R4,IO               OUTPUT POINTER                               
         XC    HALF,HALF           ZERO COUNT OF DEMOS/PRECISIONS               
         B     SCAN                                                             
         EJECT                                                                  
SCAN     DS    0H                                                               
         L     R5,ASTREND                                                       
         SR    R5,R3               BYTES LEFT TO SCAN                           
         BZ    SCAN2               SCAN COMPLETED                               
         TM    TARGET,OPSTART                                                   
         BO    SCANST                                                           
         TM    TARGET,OPTERM                                                    
         BO    SCANTR                                                           
         TM    TARGET,OPERATOR                                                  
         BO    SCANOP                                                           
         DC    H'0'                SOMETHING WENT HAYWIRE                       
         SPACE 2                                                                
SCAN2    DS    0H                                                               
         LA    R6,IO                                                            
         LR    R5,R4               END OF OUTPUT                                
         SR    R5,R6               LENGTH OF OUTPUT                             
         GOTO1 SQUASHER,DMCB,(R6),(R5)                                          
         L     R5,DMCB+4           OUTPUT LENGTH                                
         SPACE 1                                                                
         L     R3,FILEC                                                         
         LA    R2,60                                                            
         GOTO1 CHOPPER,DMCB,(0,(R6)),((R2),24(R3)),(C'P',5),C'LEN=',   +        
               (R5)                                                             
         MVC   WORD,DMCB+8                                                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* SCAN FOR OPERAND START - EITHER A LETTER OR NUMBER                            
*                                                                               
SCANST   DS    0H                                                               
         L     R1,ASTREND          IN CASE NOTHING IS FOUND                     
         EX    R5,*+8                                                           
         B     *+10                                                             
         TRT   0(0,R3),STARTAB                                                  
         BZ    SCANST2             NO MORE OPERANDS                             
         STC   R2,BYTE             SAVE FUNCTION BYTE                           
         ST    R1,WORD             SAVE A(FIST OPERAND BYTE)                    
         LR    R6,R1                                                            
         SR    R6,R3               BYTES BEFORE OPERAND START                   
         BZ    SCANST2             NONE                                         
         BCTR  R6,0                                                             
         EX    R6,*+8              MOVE BYTES FROM SCAN START                   
         B     *+10                UP TO OPERAND START TO OUTPUT                
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(R4,R6)         UPDATE OPUTPUT POINTER                       
*                                                                               
SCANST2  DS    0H                                                               
         MVI   TARGET,OPTERM       LOOK FOR OPERAND TERMINATOR NEXT             
         LR    R3,R1               RESET SCAN SEARCH START                      
*                                                                               
SCANSTX  DS    0H                                                               
         B     SCAN                                                             
         SPACE 2                                                                
*                                                                               
* SCAN FOR AN OPERAND TERMINATOR                                                
*                                                                               
SCANTR   DS    0H                                                               
         L     R1,ASTREND                                                       
         EX    R5,*+8                                                           
         B     *+10                                                             
         TRT   0(0,R3),TERMTAB                                                  
*                                                                               
         ST    R1,FULL             SAVE A(TERMINATOR)                           
         STC   R2,TEST             FUNCTION VALUE OF TERMINATOR                 
         LR    R6,R1                                                            
         SR    R6,R3               LENGTH OF OPERAND                            
         BCTR  R6,0                                                             
         CLI   BYTE,NUMERIC                                                     
         BE    SCANTR3             OPERAND IS A CONSTANT                        
         CLI   BYTE,QUOTE          TEST FOR PRECEDING QUOTE                     
         BE    SCANTR4             SKIP OVER PRECEDING OPERAND CODE             
         CLI   BYTE,DBLQUOTE       TEST FOR PRECEDING DBLQUOTE                  
         BE    SCANTR4             SKIP OVER PRECEDING OPERAND CODE             
         CLI   BYTE,EXCLAM         TEST FOR PRECEDING EXCLAM                    
         BE    SCANTR4             SKIP OVER PRECEDING OPERAND CODE             
*                                                                               
SCANTR2  DS    0H                  OPERAND IS A DEMO CODE                       
         XC    THREE,THREE                                                      
         MVC   THREE+1(1),0(R3)    MODIFIER                                     
         BCTR  R6,0                ADJUST LEN FOR MODIFIER                      
         EX    R6,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R3)         PACK DEMO NUMBER AND                         
         CVB   RE,DUB              CONVERT IT TO BINARY                         
         STC   RE,THREE+2                                                       
         LTR   RE,RE               IF OPERAND IS A ZERO DEMO,                   
         BNZ   *+18                REPLACE IT WITH RESULT'S DEMO                
         CLI   LASTDEM+1,0         PROVIDED THAT IT IS NON-ZERO.                
         BE    SCANTR3                                                          
         MVC   THREE+2(1),LASTDEM+1                                             
*                                                                               
         GOTO1 DEMOCON,DMCB,THREE,(6,WORK),DEMAREA                              
         MVC   0(6,R4),WORK        DEMO CATEGORY TO OUTPUT                      
         CLI   QOPT4,C'Y'                                                       
         BNE   SCANOP4X                                                         
         XC    WORK,WORK                                                        
         MVC   WORK(1),THREE+1                                                  
         EDIT  (B1,THREE+2),(3,WORK+1)                                          
         MVC   0(6,R4),WORK        DEMO CATEGORY TO OUTPUT                      
SCANOP4X LA    R4,6(R4)            BUMP OUTPUT POINTER                          
         MVC   WORK+6(2),=C',0'    SET DEFAULT PRECISION                        
         B     SCANTR4                                                          
*                                                                               
SCANTR3  DS    0H                                                               
         CLI   BYTE,LETTER         FOR A ZERO DEMO, ADD BACK 1                  
         BNE   *+8                 TO INCLUDE MODIFIER IN MOVE                  
         LA    R6,1(R6)            BELOW                                        
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       MOVE CONSTANT TO OUTPUT                      
         LA    R4,1(R6,R4)         UPDATE OUTPUT POINTER                        
         CLI   BYTE,LETTER         SET UP AN ENTRY IN PRECISION                 
         BNE   SCANTR4             TABLE FOR ZERO DEMO                          
         MVC   WORK(L'PRECTAB),SPACES                                           
         MVC   WORK(2),0(R3)       ZERO DEMO                                    
         MVC   WORK+6(2),=C',0'    DEFAULT PRECISION                            
         B     SCANTR4                                                          
*                                                                               
SCANTR4  DS    0H                                                               
         L     R3,FULL             POINT R3 AT TERMINATOR                       
         CLI   TEST,SPACE                                                       
         BE    SCANTR6             NOTHING MORE FOR BLANK                       
         CLI   TEST,RPAREN         FOR A PAREN                                  
         BNE   *+12                LOOK FOR OPERATOR NEXT.                      
         MVI   TARGET,OPERATOR                                                  
         B     SCANTR6                                                          
*                                                                               
         CLI   TEST,QUOTE          TEST FOR SPECIAL OPERATORS                   
         BE    *+8                                                              
         CLI   TEST,DBLQUOTE                                                    
         BE    *+8                                                              
         CLI   TEST,EXCLAM                                                      
         BNE   *+22                                                             
         MVC   0(1,R4),TEST        MOVE SPECIAL OP TO OUTPUT                    
         LA    R4,1(R4)            BUMP OUTPUT POINTER                          
         LA    R3,1(R3)            BEGIN SCAN AFTER SPECIAL OP                  
         B     SCANTR6                                                          
*                                                                               
         CLI   TEST,PCISION                                                     
         BNE   SCANTR5                                                          
         MVC   WORK+6(2),0(R3)     SET UP FULL DEMO/PRECISION ENTRY             
         LA    R3,2(R3)            BUMP INPUT POINTER PAST PRECISION            
         MVI   TARGET,OPERATOR     EXPRESSION SO IT WILL NOT APPEAR             
         B     SCANTR6             IN OUTPUT.                                   
*                                                                               
SCANTR5  DS    0H                  OPERATOR LOGIC                               
         MVI   0(R4),SPACE         BLANK BEFORE                                 
         MVC   1(1,R4),0(R3)       OPERATOR                                     
         MVI   2(R4),SPACE         AND AFTER IT.                                
         LA    R4,3(R4)            BUMP OUTPUT POINTER                          
         LA    R3,1(R3)            RESUME SCAN AFTER OPERATOR                   
         MVI   TARGET,OPSTART                                                   
*                                                                               
SCANTR6  DS    0H                                                               
         CLI   BYTE,NUMERIC                                                     
         BE    SCANTR8                                                          
*                                                                               
         LH    R0,HALF             COUNT OF ENTRIES                             
         LR    R2,R0               SAVE IT                                      
         LA    R1,L'PRECTAB        INDEX INTO TABLE OF DEMOS/PRECISIONS         
         MR    R0,R0               AND ADD NEW ENTRY AT END                     
         LA    RE,PRECTAB(R1)                                                   
         MVC   0(8,RE),WORK                                                     
         LA    R2,1(R2)            UPDATE COUNT                                 
         CHI   R2,60                                                            
         BNH   *+6                                                              
         DC    H'0'                TABLE LIMIT EXCEEDED                         
         STH   R2,HALF                                                          
*                                                                               
SCANTR8  DS    0H                                                               
         CLI   TEST,QUOTE          TEST FOR QUOTE TERMINATOR                    
         BE    SCANTR10                                                         
         CLI   TEST,DBLQUOTE       TEST FOR DBLQUOTE TERMINATOR                 
         BE    SCANTR10                                                         
         CLI   TEST,EXCLAM         TEST FOR EXCLAM TERMINATOR                   
         BE    SCANTR10                                                         
         MVI   BYTE,0                                                           
         B     SCAN                                                             
*                                                                               
SCANTR10 DS    0H                  EXIT LOGIC FOR SPECIAL OPERATORS             
         MVI   TARGET,OPTERM       LOOK FOR ANOTHER TERMINATOR                  
         MVC   BYTE,TEST           NEXT-TREAT SP/OP LIKE OPERAND                
         B     SCAN                                                             
         SPACE 2                                                                
*                                                                               
* SCAN FOR AN OPERATOR - MOVE BYTES FROM SCAN START UP TO OPERATOR              
* OR END OF STRING TO OUTPUT FIRST                                              
*                                                                               
SCANOP   DS    0H                                                               
         L     R1,ASTREND                                                       
         EX    R5,*+8                                                           
         B     *+10                                                             
         TRT   0(0,R3),OPTABLE                                                  
*                                                                               
         LR    R6,R1                                                            
         SR    R6,R3               BYTES BEFORE OP OR END                       
         BZ    SCANOP2             NONE                                         
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       MOVE THEM TO OUTPUT                          
         LA    R4,1(R6,R4)                                                      
*                                                                               
SCANOP2  DS    0H                                                               
         C     R1,ASTREND                                                       
         BE    SCANOP4             NO OPERATORS FOUND                           
         MVI   0(R4),SPACE                                                      
         MVC   1(1,R4),0(R1)       ARGUMENT ADDRESS IS OPERATOR                 
         MVI   2(R4),SPACE                                                      
         LA    R4,3(R4)                                                         
*                                                                               
SCANOP4  DS    0H                                                               
         LR    R3,R1               UPDATE SCAN START                            
         C     R3,ASTREND                                                       
         BE    *+8                                                              
         LA    R3,1(R3)            RESTART SCAN AFTER OPERATOR                  
*                                                                               
SCANOPX  DS    0H                                                               
         MVI   TARGET,OPSTART      LOOK FOR OPERAND START NEXT                  
         B     SCAN                                                             
         EJECT                                                                  
* INSERT PRECISION EXPRESSIONS UNDER THEIR RESPECTIVE OPERANDS                  
* ON PRINT LINE                                                                 
*                                                                               
PREC     NTR1                                                                   
         XC    IO(256),IO          SCAN TABLE HAS NON-ZERO FUNCTION             
         MVC   IO(256),STARTAB     BYTES FOR LETTERS                            
         XC    IO+240(10),IO+240                                                
         LA    R3,P+24             START OF SCAN                                
         LA    R6,P+84             END OF SCAN                                  
         ZIC   R7,DEMCNT           INDEX INTO DEMO/PRECISION TABLE              
         CH    R7,HALF                                                          
         BE    PRECX               ALL OPERANDS ALREADY DONE                    
*                                                                               
PREC2    DS    0H                  SCAN FOR BEGINNING OF OPERAND                
         LR    R5,R6                                                            
         SR    R5,R3               BYTES LEFT TO SCAN                           
         BZ    PRECX               ALL DONE                                     
         LR    R1,R6               SET ARGUMENT ADDRESS TO END                  
         EX    R5,*+8                                                           
         B     *+10                                                             
         TRT   0(0,R3),IO          SCAN FOR A LETTER                            
         BZ    PRECX               NONE FOUND-ALL DONE                          
*                                                                               
         ST    R1,FULL             SAVE ADDRESS OF OPERAND START                
         LR    R3,R1               UPDATE SCAN START                            
*                                                                               
         LR    R5,R6               NOW FIND FIRST BYTE AFTER OPERAND            
         SR    R5,R3                                                            
         LR    R1,R6               END OF SCAN FIELD                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         TRT   0(0,R3),TERMTAB                                                  
*                                                                               
         LR    R0,R1               SAVE ARGUMENT BYTE ADDRESS                   
         SR    R1,R3               LENGTH OF OPERAND                            
         LR    R3,R0               UPDATE SCAN START                            
*                                                                               
PREC4    DS    0H                  FIND PRECISION ENTRY IN TABLE                
         LR    R0,R7               INDEX                                        
         LA    R1,L'PRECTAB        ENTRY LENGTH                                 
         MR    R0,R0                                                            
         LA    RE,PRECTAB(R1)      POINT TO ENTRY                               
         LA    R7,1(R7)            INCREMENT INDEX                              
*                                                                               
PREC5    DS    0H                  PRINT PRECISION                              
         L     R4,FULL             OPERAND START                                
         LA    R4,132(R4)          NEXT LINE-UNDERNEATH                         
         MVI   0(R4),LPAREN                                                     
         LA    R4,1(R4)            UPDATE OUTPUT POINTER                        
         CLI   6(RE),PERIOD        DECIMAL POINT PRECISION                      
         BNE   *+12                NO                                           
         MVI   0(R4),PERIOD        YES                                          
         LA    R4,1(R4)                                                         
*                                                                               
         PACK  DOUBLE,7(1,RE)                                                   
         CVB   R0,DOUBLE                                                        
         LTR   R0,R0                                                            
         BNZ   PREC6                                                            
         CLI   6(RE),PERIOD                                                     
         BNE   *+6                                                              
         BCTR  R4,0                BACK UP TO WRITE OVER PERIOD                 
         MVI   0(R4),C'1'          ZERO PLACES MEANS UNITS                      
         LA    R4,1(R4)                                                         
         B     PREC7                                                            
*                                                                               
PREC6    DS    0H                                                               
         MVI   0(R4),C'0'          ONE ZERO FOR EACH PLACE                      
         LA    R4,1(R4)                                                         
         BCT   R0,*-8                                                           
*                                                                               
PREC7    DS    0H                                                               
         MVI   0(R4),RPAREN                                                     
*                                                                               
PRECN    DS    0H                                                               
         CH    R7,HALF             ALL OPERANDS ACCOUNTED FOR                   
         BL    PREC2               NO-KEEP GOING                                
*                                                                               
PRECX    DS    0H                                                               
         STC   R7,DEMCNT           REPLACE COUNT BEFORE EXIT                    
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO PRINT THE POLISH STRING                                        
*                                                                               
POLISH   NTR1                                                                   
         L     R6,APOLEL                                                        
         USING CTDPFCD,R6                                                       
         LLC   R1,CTDPFLEN         ELEMENT LENGTH                               
         DROP  R6                                                               
         LR    R0,R1                                                            
         LR    RE,R6                                                            
         AR    RE,R0                                                            
         BCTR  RE,0                RE=A(LAST BYTE IN POLISH ELEMENT)            
         SHI   R0,2                R0=L'ELEMENT DATA                            
*                                                                               
POLISH2  CLI   0(RE),X'01'         LOOK BACKWARDS FOR FIRST X'01'               
         BE    POLISH4                                                          
         BCTR  RE,0                                                             
         BCT   R0,POLISH2                                                       
         DC    H'0'                                                             
*                                                                               
POLISH4  LR    RF,RE                                                            
         SR    RF,R6               RF=DISP TO FIRST OPERAND                     
         SR    R1,RF               R1=LENGTH OF POLISH STRING                   
         LA    R4,IO               R4=A(OUTPUT AREA)                            
         MVC   IO(L'P),SPACES                                                   
         MVC   IO+L'P(L'P),SPACES                                               
*                                                                               
POLISH6  CLI   0(RE),X'40'         TEST FOR OPERAND OR OPERATOR                 
         BH    POLISH7             OPERATOR                                     
         ZIC   R0,0(RE)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB         PRINT THE OPERAND NUMBER                     
         LA    R4,3(R4)                                                         
         B     POLISH8                                                          
*                                                                               
POLISH7  MVC   0(1,R4),0(RE)       MOVE OUT OPERATION                           
         LA    R4,2(R4)                                                         
*                                                                               
POLISH8  LA    RE,1(RE)            NEXT INPUT BYTE                              
         BCT   R1,POLISH6                                                       
*                                                                               
POLISH10 LA    RF,IO                                                            
         SR    R4,RF               COMPUTE L'OUTPUT STRING                      
         GOTO1 CHOPPER,DMCB,((R4),IO),(60,P+24),(C'P',4),0                      
         MVC   BYTE,DMCB+11        N'LINES TO PRINT                             
         BAS   RE,PRT                                                           
         BAS   RE,SKIPLIN          SKIP A LINE AFTER IT                         
*                                                                               
POLISHX  B     XIT                                                              
         EJECT                                                                  
* CONTROL LINE PRINTING                                                         
*                                                                               
PRT      NTR1                                                                   
         ZIC   R0,LINE                                                          
         ZIC   R1,BYTE                                                          
         AR    R0,R1                                                            
         CLM   R0,1,MAXLINES                                                    
         BNH   *+8                                                              
         MVI   FORCEHED,YES                                                     
*                                                                               
         CLI   FORCEHED,YES                                                     
         BNE   PRTLINE                                                          
*                                                                               
         MVC   HEAD3+8(1),CONFILE  FILE                                         
         MVI   HEAD3+9,DASH                                                     
         LA    RE,FILTAB           SEE IF FILE CAN BE EXPANDED                  
         LA    R1,FILES                                                         
         CLC   CONFILE,0(RE)                                                    
         BE    *+16                                                             
         LA    RE,L'FILTAB(RE)                                                  
         BCT   R1,*-14                                                          
         B     PRT2                                                             
*                                                                               
         MVC   HEAD3+10(20),4(RE)  FILE NAME EXPANSION                          
*                                                                               
PRT2     DS    0H                                                               
         MVC   HEAD4+8(1),CONMED   MEDIA                                        
         MVI   HEAD4+9,DASH                                                     
         LA    RE,MEDTAB                                                        
         LA    R1,MEDIAS                                                        
         CLC   CONMED,0(RE)                                                     
         BE    *+16                                                             
         LA    RE,L'MEDTAB(RE)                                                  
         BCT   R1,*-14                                                          
         B     *+10                                                             
*                                                                               
         MVC   HEAD4+10(10),1(RE)  MEDIA NAME                                   
*                                                                               
         MVC   HEAD5+8(1),CONSRC   SOURCE                                       
         MVI   HEAD5+9,DASH                                                     
         LA    RE,SRCTAB                                                        
         LA    R1,SOURCES                                                       
         CLC   CONSRC,0(RE)                                                     
         BE    *+16                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         BCT   R1,*-14                                                          
         B     *+10                                                             
*                                                                               
         MVC   HEAD5+10(3),1(RE)   SOURCE EXPANSION                             
*                                                                               
         CLC   QSTART,SPACES                                                    
         BE    PRTLINE                                                          
         MVC   HEAD4+88(10),=C'START BOOK'                                      
         MVC   HEAD4+99(6),STBOOK                                               
*                                                                               
PRTLINE  DS    0H                                                               
         MVI   SPACING,1                                                        
         GOTO1 REPORT                                                           
         B     XIT                                                              
         SPACE 2                                                                
SKIPLIN  DS    0H                  ROUTINE TO CONTROL SKIPPING A LINE           
         STM   RE,RF,DOUBLE                                                     
         CLI   FORCEHED,YES                                                     
         BE    SKIPLINX                                                         
         ZIC   RE,LINE                                                          
         ZIC   RF,SPACING                                                       
         AR    RE,RF                                                            
         CLM   RE,1,MAXLINES                                                    
         BH    SKIPLINX                                                         
         GOTO1 REPORT                                                           
*                                                                               
SKIPLINX DS    0H                                                               
         LM    RE,RF,DOUBLE                                                     
         BR    RE                                                               
         EJECT                                                                  
* I/O ROUTINES                                                                  
*                                                                               
HIGH     LA    RF,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     LINKDIR                                                          
         SPACE 2                                                                
SEQ      LA    RF,DMRSEQ                                                        
         MVC   KEYSAVE,KEY                                                      
         B     LINKDIR                                                          
         SPACE 2                                                                
LINKDIR  NTR                                                                    
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         L     R2,FILEC                                                         
         GOTO1 DATAMGR,DMCB,,CTFILE,KEY,(R2),(0,DMWORK)                         
         MVC   KEY,0(R2)                                                        
         BAS   RE,POSTEL                                                        
         MVC   LASTLEN,25(R2)                                                   
         B     XIT                                                              
         SPACE 2                                                                
POSTEL   NTR1                                                                   
         ST    R2,ADRECORD                                                      
         AH    R2,DATADISP                                                      
         ST    R2,ADDATA                                                        
         SR    R3,R3                                                            
         XC    ADACTIV(8),ADACTIV                                               
         SPACE 1                                                                
POST2    CLI   0(R2),0                                                          
         BE    POSTEX                                                           
         CLI   0(R2),CTACTELQ                                                   
         BNE   *+8                                                              
         ST    R2,ADACTIV                                                       
         CLI   0(R2),CTDSCELQ                                                   
         BNE   *+8                                                              
         ST    R2,ADDESC                                                        
         IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     POST2                                                            
         SPACE 1                                                                
POSTEX   B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
FILTAB   DS    0CL24                                                            
         DC    C'C',C'PAV',CL20'CABLE'                                          
         DC    C'E',C'EVN',CL20'ESTIMATED'                                      
         DC    C'T',C'TP ',CL20'TIME PERIOD'                                    
         DC    C'P',C'PAV',CL20'PROG AVERAGE'                                   
         DC    C'M',C'MPA',CL20'MKT PRG ANAL'                                   
         DC    C'N',C'NAD',CL20'NAD'                                            
         DC    C'I',C'INV',CL20'INVENTORY'                                      
         DC    C'A',C'AE ',CL20'AUDIENCE ESTIMATOR'                             
         DC    C'O',C'OPT',CL20'OPTIMUM'                                        
         DC    C'R',C'RUA',CL20'RADIO COUNTY COVERAG'                           
FILES    EQU   (*-FILTAB)/L'FILTAB                                              
         SPACE 2                                                                
MEDTAB   DS    0CL11                                                            
         DC    C'C',CL10'CANADA'                                                
         DC    C'H',CL10'NHTI'                                                  
         DC    C'I',CL10'IMP'                                                   
         DC    C'N',CL10'NETWORK'                                               
         DC    C'P',CL10'MPA'                                                   
         DC    C'T',CL10'TELEVISION'                                            
         DC    C'R',CL10'RADIO'                                                 
         DC    C'W',CL10'WEEKLY'                                                
         DC    C'U',CL10'UPGRADES'                                              
         DC    C'V',CL10'VPH'                                                   
         DC    C'D',CL10'DPT'                                                   
         DC    C'O',CL10'OVERNIGHT'                                             
         DC    C'A',CL10'A (???)'                                               
MEDIAS   EQU   (*-MEDTAB)/L'MEDTAB                                              
         SPACE 2                                                                
SRCTAB   DS    0CL4                                                             
         DC    C'A',C'ARB'                                                      
         DC    C'N',C'NSI'                                                      
         DC    C'B',C'BBM'                                                      
         DC    C'M',C'BBR'                                                      
         DC    C'C',C'CSI'                                                      
         DC    C'S',C'SRC'                                                      
         DC    C'R',C'RDR'         RADAR                                        
         DC    C'A',C'ARB'                                                      
         DC    C'F',C'FUS'                                                      
         DC    C'I',C'IDX'         OPTIMUM PROGRAM INDEX                        
         DC    C'Q',C'TVQ'                                                      
         DC    C'G',C'G  '         IAG FACTORS                                  
         DC    C'T',C'TRI'         TRITON                                       
SOURCES  EQU   (*-SRCTAB)/L'SRCTAB                                              
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* PERMANENT STORAGE                                                             
*                                                                               
         DS    0D                                                               
PATCH    DC    XL30'00'                                                         
XFF      DC    16X'FF'                                                          
SORTFLD  DC    CL80'SORT FIELDS=(5,10,A),FORMAT=BI'                             
RECTYPE  DC    CL80'RECORD TYPE=V,LENGTH=1000'                                  
FOUR     DS    XL4                                                              
*                                                                               
* PERSONAL LANGUAGE DEMO MODIFIER TRANSLATE TABLES                              
         DS    0A                                                               
APLDTABS DS    0XL(6*4)            6 ADDRESSES                                  
APLMODTB DS    V                   A(MODIFIER TABLE)                            
APLPLDTB DS    V                   A(PERSONAL LANG. ATTRIBUTE TABLE)            
APLENCTB DS    V                   A(MODIFIER BYTE ENCODING TABLE)              
APLPRNTB DS    V                   A(PERSONAL LANG. PRINTABLE DESCRIPS)         
         DS    V                   SPARE                                        
         DS    V                   SPARE                                        
*                                                                               
STARTAB  DS    XL256                                                            
TERMTAB  DS    XL256                                                            
OPTABLE  DS    XL256                                                            
SPECTAB  DS    XL256                                                            
         SPACE                                                                  
SORTINIT DC    C'N'                                                             
         SPACE 2                                                                
* DSECT TO COVER SORT RECORD                                                    
*                                                                               
SORTD    DSECT                                                                  
SORTRLEN DS    H                                                                
         DS    H                                                                
SORTKEY  DS    0CL10               HARD-CODED IN SORTFLD (SORT FIELDS)          
SORTFMS  DS    0CL3                                                             
SORTFILE DS    C                                                                
SORTMED  DS    C                                                                
SORTSRC  DS    C                                                                
SORTAGY  DS    XL2                                                              
SORTLCOD DS    X                                                                
SORTDEMO DS    0XL2                                                             
SORTMOD  DS    X                                                                
SORTDEM# DS    X                                                                
SORTSBK  DS    XL2                                                              
SORTDATA DS    0C                                                               
         SPACE 1                                                                
* DSECT TO COVER WORKING STORAGE                                                
*                                                                               
TEMPD    DSECT                                                                  
AELEMS   DS    0A                                                               
AACTEL   DS    A                   A(ACTIVITY ELEMENT)                          
ACOMEL   DS    A                   A(COMMENT ELEMENT)                           
APRECEL  DS    A                   A(PRECISION ELEMENT)                         
APOLEL   DS    A                   A(POLISH FORMULA ELEMENT) OR A(0)            
AINPEL   DS    A                   A(INPUT FORMULA ELEMENT)                     
AELEMSL  EQU   *-AELEMS                                                         
ASTREND  DS    A                                                                
*                                                                               
ELCODE   DS    X                                                                
CONFIL   DS    0CL3                                                             
CONFILE  DS    C                                                                
CONMED   DS    C                                                                
CONSRC   DS    C                                                                
TEST     DS    C                                                                
TARGET   DS    B                                                                
DEMCNT   DS    X                                                                
*                                                                               
ACTFRST  DS    XL3                 FIRST ACTIVITY DATE                          
ACTLAST  DS    XL3                 LAST ACTIVITY DATE                           
*                                                                               
LASTDEM  DS    XL2                                                              
STBOOK   DS    CL6                                                              
*                                                                               
         DS    0D                                                               
DEMAREA  DS    CL256                                                            
STRING   DS    CL256                                                            
PRECTAB  DS    60CL8                                                            
*                                                                               
IO       DS    CL1000                                                           
TEMPX    EQU   *                                                                
         SPACE 2                                                                
* EQUATED VALUES                                                                
*                                                                               
LETTER   EQU   X'10'                                                            
NUMERIC  EQU   X'11'                                                            
OPSTART  EQU   X'01'                                                            
OPTERM   EQU   X'02'                                                            
OPERATOR EQU   X'04'                                                            
*                                                                               
SPACE    EQU   C' '                                                             
RPAREN   EQU   C')'                                                             
PCISION  EQU   C','                                                             
LPAREN   EQU   C'('                                                             
PERIOD   EQU   C'.'                                                             
COMMA    EQU   C','                                                             
QUOTE    EQU   C''''                                                            
DBLQUOTE EQU   C'"'                                                             
EXCLAM   EQU   C'!'                                                             
ADD      EQU   C'+'                                                             
SUBTRACT EQU   C'-'                                                             
MULTIPLY EQU   C'*'                                                             
DIVIDE   EQU   C'/'                                                             
SLASH    EQU   DIVIDE                                                           
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
DASH     EQU   C'-'                                                             
         SPACE 2                                                                
* CONTROL WORKING STORAGE AND FILE DSECTS                                       
*                                                                               
         PRINT OFF                                                              
DEMOD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE DEDEMEQUS2                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062CTREP9002 09/15/14'                                      
         END                                                                    
