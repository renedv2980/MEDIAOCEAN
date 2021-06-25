*          DATA SET CTREP9002M AT LEVEL 060 AS OF 05/01/02                      
*PHASE CT9002A,+0                                                               
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
*******************************************************************             
CT9002   TITLE '-   DEMO FORMULA LISTING'                                       
CT9002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TEMPX-TEMPD,**CT9002                                             
         USING TEMPD,RC                                                         
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING CT9002+4096,R9                                                   
         SPACE 2                                                                
**********************************************************************          
* RUNFRST PROCESSING - INITIALIZE TRT SCAN TABLES                               
**********************************************************************          
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
         B     FORMOUT                                                          
         EJECT                                                                  
**********************************************************************          
* REQFRST PROCESSING - READ DEMO FORMULA RECORDS AND                            
* WRITE SELECTED RECORDS TO SORT FILE                                           
**********************************************************************          
*                                                                               
FORM2    DS    0H                                                               
         CLI   MODE,REQFRST                                                     
         BNE   FORM20                                                           
*                                                                               
         LH    RF,=AL2(TEMPX-TEMPD)                                             
         LR    RE,RC                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0               ZERO OUT WORKING STORAGE                     
*                                                                               
         BAS   RE,SETACT           SET ACTIVITY DATE FILTERS                    
         LA    R2,SORTC                                                         
         CLI   SORTINIT,YES        PREVENT DOUBLE SORTER INIT                   
         BE    FORM3                                                            
         GOTO1 SORTER,DMCB,SORTFLD,RECTYPE,(40,(R2))                            
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
         MVC   CTGKFILE(3),QFILTER1                                             
         LA    R5,17               LENGTH OF KEY TO COMPARE AFTER READ          
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
         EX    R5,KEYCOMP                                                       
         BNE   FORMOUT             ALL DONE WITH READ                           
         OC    HALF,HALF           START DATE FILTERING                         
         BZ    FORM8               NO                                           
         CLC   CTGKSTRT,HALF       YES-KEY AGAINST FILTER                       
         BNL   FORM8                                                            
         MVC   CTGKSTRT,HALF       GO BACK TO READ HIGH                         
         XC    CTGKDEMO,CTGKDEMO                                                
         B     FORM4                                                            
*                                                                               
FORM8    DS    0H                  PROCESS RECORD                               
         L     R4,ADRECORD                                                      
         LA    R3,IO               BUILD A SORT RECORD                          
         USING SORTD,R3                                                         
         LR    RE,R3               AT IO                                        
         LA    RF,L'IO                                                          
         XR    R1,R1                                                            
         MVCL  RE,R0               ZERO SORT RECORD AREA                        
*                                                                               
         MVC   SORTFILE(3),CTGKFILE                                             
         MVC   SORTAGY,CTGKAGY                                                  
         MVC   SORTMODF,CTGKDEMO   DEMO MODIFIER                                
         MVC   SORTLCOD,CTGKCODE   LOOK UP CODE (GROUP CODE)                    
         MVC   SORTDEM,CTGKDEMO+1  DEMO NUMBER                                  
*---                                                                            
         CLI   CTGKCODE,X'FF'      OLD DEMO NUMBER?                             
         BNE   FORM8B                                                           
         LA    R1,EQU2TO3          CONVERT OLD # TO NEW #                       
FORM8A   OC    0(3,R1),0(R1)       END OF TABLE                                 
         BZ    FORM8B                                                           
         CLC   SORTDEM,0(R1)                                                    
         BE    *+12                                                             
         LA    R1,3(R1)                                                         
         B     FORM8A                                                           
         MVC   SORTDEM,2(R1)       MOVE IN NEW DEMO #                           
         MVC   SORTLCOD,1(R1)      MOVE IN NEW MODIFIER                         
*---                                                                            
FORM8B   MVC   SORTSBK,CTGKSTRT                                                 
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
*                                                                               
         BAS   RE,MOVEREC          MOVE ELEM TO SORT RECORD                     
*                                                                               
         MVI   ELCODE,X'02'        COMMENT ELEMENT                              
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         BAS   RE,MOVEREC                                                       
*                                                                               
         MVI   ELCODE,X'03'        PRECISION ELEMENT                            
         LR    R6,R4               A(RECORD)                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,MOVEREC                                                       
         MVC   BYTE,2(R6)          SAVE PRECISION BYTE                          
*                                                                               
         MVI   ELCODE,X'04'        POLISH FORMULA ELEMENT                       
         LR    R6,R4               A(RECORD)                                    
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         BAS   RE,MOVEREC                                                       
*                                                                               
         MVI   ELCODE,X'05'        INPUT FORMULA ELEMENT                        
         LR    R6,R4               REFRESH RECORD ADDRESS                       
         BAS   RE,GETEL                                                         
         BE    FORM9                                                            
*                                                                               
* BUILD A DUMMY FORMULA ELEMENT IF RECORD HAS NONE                              
*                                                                               
         ST    R7,AINPEL           SAVE A(ELEMENT START)                        
         LA    R1,4                DEVELOP EL LENGTH IN R1                      
         MVI   0(R7),X'05'         ELEMENT CODE                                 
         MVC   3(1,R7),CTGKDEMO    MODIFIER                                     
         ZIC   RF,CTGKDEMO+1                                                    
         EDIT  (RF),(3,4(R7)),ALIGN=LEFT,ZERO=NOBLANK                           
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
         EX    R1,MOVEEL           ELEMENT TO SORT RECORD                       
         LA    R1,1(R1)                                                         
         LR    R0,R1               SAVE FIRST ELEMENT LENGTH                    
         ST    R7,AINPEL           SAVE A(ELEMENT) ON SORT RECORD               
         AR    R7,R1                                                            
         AH    R1,SORTRLEN                                                      
         STH   R1,SORTRLEN                                                      
*                                                                               
**********************************************************************          
* CONCATENATE ANY SUBSEQUENT INPUT FORMULA ELEMENTS TO                          
* THE FIRST ONE TO MAKE ONE ELEMENT ON SORT RECORD                              
**********************************************************************          
*                                                                               
FORM10   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   FORM12                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
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
KEYCOMP  CLC   KEY(0),KEYSAVE                                                   
MOVEEL   MVC   0(0,R7),0(R6)                                                    
         SPACE 2                                                                
**********************************************************************          
* SUB-ROUTINE TO MOVE AN ELEMENT TO SORT RECORD                                 
**********************************************************************          
*                                                                               
MOVEREC  ZIC   R1,1(R6)                                                         
         BCTR  R1,0                MOVE IT TO SORT RECORD                       
         EX    R1,MOVEEL                                                        
         LA    R1,1(R1)            RESTORE EL LENGTH                            
         AR    R7,R1               UPDATE DATA POINTER                          
         AH    R1,SORTRLEN         UPDATE SORT RECORD LEN                       
         STH   R1,SORTRLEN                                                      
         BR    RE                                                               
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* REQLAST PROCESSING - READ SORTED RECORDS AND PRINT REPORT                     
**********************************************************************          
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
         BE    FORM22A             YES                                          
         CLC   SORTMODF,LASTDEM    TAKE THE HIGHEST BOOK FOR                    
         BNE   FORM23              THE SAME DEMO                                
         CLC   SORTDEM,LASTDEM+1                                                
         BE    FORM22                                                           
FORM22A  ST    R3,ADRECORD         SAVE A(SORTED RECORD)                        
         BAS   RE,GETELS           GET ELEMENT ADDRESSES                        
*                                                                               
FORM23   DS    0H                  CONTROL BREAKS IN FILE/MEDIA/SOURCE          
         CLC   CONFIL,SPACES                                                    
         BE    *+14                FIRST TIME THROUGH                           
         CLC   CONFIL,SORTFILE                                                  
         BE    FORM23A             NO BREAK                                     
*                                                                               
         MVC   CONFIL,SORTFILE                                                  
         XC    LASTDEM,LASTDEM     RESET LAST DEMO CONTROL                      
         MVI   FORCEHED,YES                                                     
*                                                                               
FORM23A  CLC   SORTMODF,LASTDEM    TEST FOR CHANGE IN MODIFIER                  
         BE    *+8                                                              
         BAS   RE,PRTCOM           YES-PRINT THE DEMO COMMENT                   
         MVC   LASTDEM(1),SORTMODF    UPDATE LAST DEMO                          
         MVC   LASTDEM+1(1),SORTDEM   UPDATE LAST DEMO                          
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
         CLI   SORTDEM,0           CHECK FOR ZERO DEMO                          
         BNE   *+18                NO                                           
         MVC   P+1(1),SORTMODF     MODIFIER                                     
         MVI   P+2,C'0'                                                         
         B     FORM25A                                                          
*                                  PRINT OUT:                                   
         MVC   P+1(1),SORTMODF       MODIFIER                                   
         LA    R7,P+2                GROUP                                      
         ZIC   RF,SORTLCOD                                                      
         EDIT  (RF),(3,(R7)),ALIGN=LEFT                                         
         LA    R7,P+6                NEW DEMO NUMBER                            
         ZIC   RF,SORTDEM                                                       
         EDIT  (RF),(3,(R7)),ALIGN=LEFT                                         
*                                                                               
         AR    R7,R0               UPDATE POINTER                               
         LA    R7,1(R7)                                                         
         MVI   0(R7),LPAREN        ENCLOSE CATEGORY IN PARENS                   
         LA    R7,1(R7)                                                         
*                                                                               
FORM25A  XC    THREE,THREE                                                      
         MVC   THREE+1(1),SORTMODF                                              
         MVC   THREE+2(1),SORTDEM                                               
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
FORM27   DS    0H                                                               
         CLI   SORTDEM,0        BYPASS NAME IF 0                                
         BE    FORM30                                                           
         MVI   DBDEMTYP,0                                                       
         MVC   FOUR(3),THREE                                                    
         CLI   SORTLCOD,X'FF'                                                   
         BE    FORM29                                                           
         XC    FOUR,FOUR                                                        
         MVC   FOUR+1(1),THREE+1                                                
         MVC   FOUR+2(1),SORTLCOD                                               
         MVC   FOUR+3(1),THREE+2                                                
         MVI   DBDEMTYP,C'4'                                                    
FORM29   GOTO1 DEMOCON,DMCB,FOUR,(6,(R7)),DBLOCK                                
         MVI   DBDEMTYP,0                                                       
         CLI   0(R7),SPACE                                                      
         BE    *+12                                                             
         LA    R7,1(R7)                                                         
         B     *-12                                                             
         MVI   0(R7),RPAREN                                                     
*                                                                               
         LA    R1,EQU2TO3          IF DEMO# CONVERTED FROM OLD #?               
FORM29A  OC    0(3,R1),0(R1)       THEN PRINT OUT OLD# TOO.                     
         BZ    FORM30              DEMO # NOT IN TABLE--NO OLD#                 
         CLC   SORTDEM,2(R1)       SAME NEW#?                                   
         BNE   *+14                                                             
         CLC   SORTLCOD,1(R1)                                                   
         BE    *+12                                                             
         LA    R1,3(R1)            TEST NEXT ENTRY IN TABLE                     
         B     FORM29A                                                          
         LA    R7,2(R7)            LEAVE A SPACE AFTER DEMO NAME                
         ZIC   RF,0(R1)            OLD DEMO#                                    
         EDIT  (RF),(3,(R7)),ALIGN=LEFT                                         
         LA    R7,4(R7)                                                         
         B     FORM30                                                           
         DROP  R5                                                               
*                                                                               
FORM30   DS    0H                                                               
         L     R1,APRECEL                                                       
         CLI   3(R1),X'20'         TEST FOR EQUATED FORMULA                     
         BE    FORM34                                                           
         BAS   RE,DECODE           PREPARE INPUT FORMULA FOR PRINTING           
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
         TM    2(R1),X'80'         DECIMAL POINT IN OUTPUT PRECISION            
         BZ    *+12                NO                                           
         MVI   0(RE),PERIOD        YES                                          
         LA    RE,1(RE)                                                         
*                                                                               
         ZIC   R0,2(R1)            HIGHEST 4 BITS ARE                           
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
         TM    2(R1),X'20'         TEST FOR FORMULA PRECISION                   
         BZ    *+12                                                             
         MVI   0(RE),C'F'                                                       
         LA    RE,2(RE)            BUMP OUTPUT POINTER                          
         TM    3(R1),X'80'         TEST IF DIRECT TRANSFER                      
         BO    *+14                NO                                           
         MVC   0(6,RE),=C'DIRECT'                                               
         LA    RE,7(RE)            BUMP OUTPUT POINTER                          
*                                                                               
         TM    3(R1),X'40'         TEST FOR INDEX APPLICATION                   
         BZ    *+10                                                             
         MVC   0(5,RE),=C'INDEX'                                                
*                                                                               
FORM32   DS    0H                                                               
         BAS   RE,PREC             PUT PRECISIONS UNDERNEATH OPERANDS           
         CLC   PSECOND,SPACES      ANY PRECISIONS FOR LINE                      
         BE    FORM35              NO                                           
         MVI   BYTE,2              YES-BLOCK 2 LINES TOGETHER                   
         B     FORM35                                                           
*                                                                               
FORM34   DS    0H                  EQUATED FORMULA LOGIC                        
         LA    R5,1                ONE LINE OF OUTPUT                           
         MVC   P+24(3),=C'USE'                                                  
         MVC   P+28(1),2(R1)       EQUATED MODIFIER                             
         ZIC   R2,SORTDEM          DEMO NUMBER                                  
         EDIT  (R2),(3,P+29),ALIGN=LEFT                                         
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
         ZIC   R5,1(RE)            GET ELEMENT LENGTH                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     XIT                                                              
         TRT   2(0,RE),SPECTAB                                                  
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
         CLI   0(R6),X'01'                                                      
         BNE   *+12                                                             
         ST    R6,AACTEL           ACTIVITY ELEM                                
         B     GETELS4                                                          
*                                                                               
         CLI   0(R6),X'02'                                                      
         BNE   *+12                                                             
         ST    R6,ACOMEL           COMMENTS ELEM                                
         B     GETELS4                                                          
*                                                                               
         CLI   0(R6),X'03'                                                      
         BNE   *+12                                                             
         ST    R6,APRECEL          PRECISION ELEM                               
         B     GETELS4                                                          
*                                                                               
         CLI   0(R6),X'04'                                                      
         BNE   *+12                                                             
         ST    R6,APOLEL           POLISH FORMULA ELEM                          
         B     GETELS4                                                          
*                                                                               
         CLI   0(R6),X'05'                                                      
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
         ZIC   R1,1(RF)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+24(0),2(RF)       MOVE COMMENT TO PRINT LINE                   
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
         L     RE,AINPEL           INPUT FORMULA ELEMENT                        
         ZIC   R1,1(RE)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STRING(0),3(RE)     MOVE FORMULA TO SCAN AREA                    
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
        GOTO1 CHOPPER,DMCB,(0,(R6)),((R2),24(R3)),(C'P',5),C'LEN=',(R5)         
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
*        GOTO1 DEMOCON,DMCB,THREE,(0,WORK),DEMAREA                              
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
         CH    R2,=H'60'                                                        
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
         ZIC   R1,1(R6)            ELEMENT LENGTH                               
         LR    R0,R1                                                            
         LR    RE,R6                                                            
         AR    RE,R0                                                            
         BCTR  RE,0                RE=A(LAST BYTE IN POLISH ELEMENT)            
         SH    R0,=H'2'            R0=L'ELEMENT DATA                            
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
         MVC   HEAD3+7(1),CONFILE  FILE                                         
         MVI   HEAD3+8,DASH                                                     
         LA    RE,FILTAB           SEE IF FILE CAN BE EXPANDED                  
         LA    R1,FILES                                                         
         CLC   CONFILE,0(RE)                                                    
         BE    *+16                                                             
         LA    RE,L'FILTAB(RE)                                                  
         BCT   R1,*-14                                                          
         B     PRT2                                                             
*                                                                               
         MVC   HEAD3+9(12),4(RE)   FILE NAME                                    
*                                                                               
PRT2     DS    0H                                                               
         MVC   HEAD4+7(1),CONMED   MEDIA                                        
         MVI   HEAD4+8,DASH                                                     
         LA    RE,MEDTAB                                                        
         LA    R1,MEDIAS                                                        
         CLC   CONMED,0(RE)                                                     
         BE    *+16                                                             
         LA    RE,L'MEDTAB(RE)                                                  
         BCT   R1,*-14                                                          
         B     *+10                                                             
*                                                                               
         MVC   HEAD4+9(10),1(RE)   MEDIA NAME                                   
*                                                                               
         MVC   HEAD3+96(1),CONSRC  SOURCE                                       
         MVI   HEAD3+97,DASH                                                    
         LA    RE,SRCTAB                                                        
         LA    R1,SOURCES                                                       
         CLC   CONSRC,0(RE)                                                     
         BE    *+16                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         BCT   R1,*-14                                                          
         B     *+10                                                             
*                                                                               
         MVC   HEAD3+98(3),1(RE)   SOURCE EXPANSION                             
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
         CLI   0(R2),X'01'                                                      
         BNE   *+8                                                              
         ST    R2,ADACTIV                                                       
         CLI   0(R2),X'02'                                                      
         BNE   *+8                                                              
         ST    R2,ADDESC                                                        
         IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     POST2                                                            
         SPACE 1                                                                
POSTEX   B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
FILTAB   DS    0CL16                                                            
         DC    C'C',C'PAV',CL12'CABLE'                                          
         DC    C'E',C'EVN',CL12'ESTIMATED'                                      
         DC    C'T',C'TP ',CL12'TIME PERIOD'                                    
         DC    C'P',C'PAV',CL12'PROG AVERAGE'                                   
         DC    C'M',C'MPA',CL12'MKT PRG ANAL'                                   
         DC    C'N',C'NAD',CL12'NAD'                                            
         DC    C'I',C'INV',CL12'INVENTORY'                                      
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
MEDIAS   EQU   (*-MEDTAB)/L'MEDTAB                                              
         SPACE 2                                                                
SRCTAB   DS    0CL4                                                             
         DC    C'A',C'ARB'                                                      
         DC    C'N',C'NSI'                                                      
         DC    C'B',C'BBM'                                                      
         DC    C'M',C'BBR'                                                      
         DC    C'C',C'CSI'                                                      
         DC    C'S',C'SRC'                                                      
SOURCES  EQU   (*-SRCTAB)/L'SRCTAB                                              
         EJECT                                                                  
*                                                                               
EQU2TO3  DS    0C                                                               
       ++INCLUDE DEEQU2TO3                                                      
         EJECT                                                                  
*                                                                               
* LITERAL POOL                                                                  
         LTORG                                                                  
         EJECT                                                                  
* PERMANENT STORAGE                                                             
*                                                                               
         DS    0D                                                               
PATCH    DC    XL30'00'                                                         
XFF      DC    16X'FF'                                                          
SORTFLD  DC    CL80'SORT FIELDS=(5,10,A),FORMAT=BI,WORK=1'                      
RECTYPE  DC    CL80'RECORD TYPE=V,LENGTH=1000'                                  
         SPACE 1                                                                
FOUR     DS    XL4                                                              
STARTAB  DS    XL256                                                            
TERMTAB  DS    XL256                                                            
OPTABLE  DS    XL256                                                            
SPECTAB  DS    XL256                                                            
         SPACE                                                                  
SORTINIT DC    C'N'                                                             
         DS    0D                                                               
SORTC    DC    41000X'00'                                                       
         SPACE 2                                                                
* DSECT TO COVER SORT RECORD                                                    
*                                                                               
SORTD    DSECT                                                                  
SORTRLEN DS    H                                                                
         DS    H                                                                
SORTKEY  DS    0CL10                                                            
SORTFILE DS    C                                                                
SORTMED  DS    C                                                                
SORTSRC  DS    C                                                                
SORTAGY  DS    XL2                                                              
SORTMODF DS    X                   DEMO MODIFIER                                
SORTLCOD DS    X                   LOOK UP OR GROUP CODE                        
SORTDEM  DS    X                   DEMO NUMBER                                  
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
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060CTREP9002M05/01/02'                                      
         END                                                                    
