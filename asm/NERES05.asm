*          DATA SET NERES05    AT LEVEL 065 AS OF 05/01/02                      
*PHASE T32105A,*                                                                
T32105   TITLE '-   EDIT FOR RESEARCH WRITER'                                   
T32105   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RES***,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         SPACE 1                                                                
         CLI   MODE,VALREC         VALIDATE RECORD KEY                          
         BNE   XIT                                                              
         SPACE 1                                                                
         MVI   BOXOPT,C'Y'         PRESET FIELDS                                
         MVI   REPAVE,C'N'                                                      
         MVI   QRTOPT,C'N'                                                      
         MVC   DEMOS(24),DEFDEM                                                 
         MVI   NUMDEMS,8                                                        
         MVI   SPACOPT,1                                                        
         MVI   ROWOPT,C'Y'                                                      
         XC    UNIVOPT,UNIVOPT                                                  
         MVI   DPOPT,C'N'                                                       
         XC    TOPOPT,TOPOPT                                                    
         XC    MINOPT,MINOPT                                                    
         XC    MAXOPT,MAXOPT                                                    
         MVI   SURVOPT,0                                                        
         MVI   OVEROPT,0                                                        
         MVI   FFTNOPT,0                                                        
         MVI   SOLOOPT,0                                                        
         MVI   EFFOPT,0                                                         
         MVI   SCHEMOPT,0                                                       
         MVI   HUT52OPT,0                                                       
         MVI   TPOPT,0                                                          
         MVI   SEPOPT,0                                                         
         MVI   NOVOPT,0                                                         
         XC    HUTOVER,HUTOVER                                                  
         MVI   TYPEOPT,0                                                        
         MVI   TRACEOPT,C'N'                                                    
         MVI   EVERYOPT,C'N'                                                    
         MVI   NAVE,0                                                           
         MVI   DAYOPT,C'P'         P=POCKETPIECE                                
         MVI   DOWNOPT,0           DOWNLOAD                                     
         EJECT                                                                  
*              VALIDATE REQUEST FIELDS (KEY)                                    
         SPACE 3                                                                
VKEY     LA    R2,RESSRCEH         VALIDATE SOURCE                              
         MVI   OPTION,X'FF'        EVERYTHING SUPPORTED                         
         GOTO1 VVALSRC                                                          
         MVI   TPOPT,C'N'                                                       
         CLC   8(5,R2),=C'NTI,T'                                                
         BNE   *+12                                                             
         MVI   TPOPT,C'Y'                                                       
         MVI   FFTNOPT,C'Y'        FORCE SHORT QHR ITEMS OUT                    
         SPACE 1                                                                
         LA    R2,RESPSTRH         POSSIBLE START/END                           
         GOTO1 VVALSEND                                                         
         SPACE 1                                                                
         LA    R2,RESBOOKH         VALIDATE BOOK                                
         MVI   MAX,8                                                            
         GOTO1 VVALBOOK                                                         
         SPACE 1                                                                
         LA    R2,RESDEMOH         VALIDATE DEMOS                               
         MVI   MAX,20              DUMMY SO I CAN DO ERROR MSG                  
         MVI   NFLDS,1                                                          
         GOTO1 VVALDEM                                                          
         MVC   NUMDEMS(1),ACTUAL                                                
         LA    R3,DEMOS                                                         
         CLI   ACTUAL,8            REALLY ALLOW 8 DEMOS                         
         BNH   VKEY20              (IF COST FIELD USED, ONLY ALLOW 5)           
         MVC   CONHEAD(L'MANYDEM),MANYDEM                                       
         B     MYEND                                                            
         SPACE 1                                                                
VKEY20   LA    R2,RESDAYH          VALIDATE DAY/TIME FIELDS                     
         GOTO1 VVALDYTM,PARAS,6    6 DAY/DETAIL FIELDS                          
         SPACE 1                                                                
         LA    R2,RESNETH          VALIDATE NETWORK(S)                          
         LA    R3,9                (MAX 9)                                      
         LA    R5,5                (5 BYTES EACH)                               
         CLI   RESSRCE,C'P'        EXCEPT FOR PROGRAMS                          
         BNE   *+12                                                             
         LA    R3,8                WHICH HAVE MAX 8                             
         LA    R5,7                (7 BYTES EACH)                               
         LA    R4,NETSAVE                                                       
         XC    NETSAVE,NETSAVE                                                  
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
VKEY30   GOTO1 VVALNET                                                          
         MVC   0(7,R4),ACTNET                                                   
         AR    R4,R5                                                            
VKEY35   BAS   RE,BUMP                                                          
         BCT   R3,*+8                                                           
         B     VKEY40                                                           
         CLI   5(R2),0             ANOTHER NETWORK?                             
         BNE   VKEY30                                                           
         B     VKEY35                                                           
         SPACE 1                                                                
VKEY40   LA    R2,RESDETH          DETAILS                                      
         BAS   RE,VALIDET                                                       
         LA    R2,RESAVEH          AVERAGES                                     
         BAS   RE,VALIAVE                                                       
         LA    R2,RESOPTH          OPTIONS                                      
         BAS   RE,EDITOPT                                                       
         LA    R2,RESFILTH         FILTERS                                      
         BAS   RE,EDITFILT                                                      
         LA    R2,RESTITLH         OWN TITLE                                    
         GOTO1 VVALTITL                                                         
         LA    R2,RESSRCEH                                                      
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              EDIT OPTIONS                                                     
         SPACE 3                                                                
EDITOPT  NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    YES                                                              
         GOTO1 SCANNER,DMCB,(20,(R2)),(10,BLOCK),0                              
         ZIC   R0,4(R1)                                                         
         LA    R3,BLOCK                                                         
         MVI   FIELDERR,1                                                       
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE 1                                                                
EDIT46   CLC   12(2,R3),=C'S '     SPACING                                      
         BNE   EDIT48                                                           
         CLI   11(R3),0                                                         
         BE    BADOPT                                                           
         MVC   SPACOPT,11(R3)                                                   
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT48   CLC   12(4,R3),=C'UNIV'   UNIVERSE OPTION                              
         BNE   EDIT49                                                           
         CLI   RESSRCE,C'P'        OPTION FOR PROGRAM AVG. ONLY                 
         BNE   BADOPT                                                           
         OC    8(4,R3),8(R3)                                                    
         BZ    BADOPT                                                           
         MVC   UNIVOPT,8(R3)                                                    
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT49   CLC   12(4,R3),=C'HOMES'  HOMES OPTION                                 
         BNE   EDIT50                                                           
         CLI   RESSRCE,C'P'        OPTION FOR PROGRAM AVG. ONLY                 
         BNE   BADOPT                                                           
         OC    8(4,R3),8(R3)                                                    
         BZ    BADOPT                                                           
         MVC   HOMEOPT,8(R3)                                                    
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT50   CLC   12(3,R3),=C'BOX'    BOX OPTION                                   
         BNE   EDIT52                                                           
         MVC   BOXOPT,22(R3)                                                    
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT52   CLC   12(3,R3),=C'ROW'    ROW OPTION                                   
         BNE   EDIT54                                                           
         CLI   RESSRCE,C'P'        OPTION NOT FOR PROGRAM AVG.                  
         BNE   EDIT52A                                                          
         CLI   22(R3),C'N'                                                      
         BE    BADOPT                                                           
EDIT52A  MVC   ROWOPT,22(R3)                                                    
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT54   CLC   12(7,R3),=C'QUARTER'  OPTION TO SHOW QUARTER HOUR                
         BNE   EDIT56                                                           
         MVI   QRTOPT,C'Y'                                                      
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT56   DS    0H                                                               
         SPACE 1                                                                
EDIT58   CLC   12(7,R3),=C'DAYPART'  DAYPART GROUPING                           
         BNE   EDIT60                                                           
         MVI   DPOPT,C'Y'                                                       
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT60   CLC   12(3,R3),=C'TOP'    SHOW ONLY TOP NNN                            
         BNE   EDIT61                                                           
         CLI   NAVE,0                                                           
         BNE   BADOPT              MULTI-AVERAGES NOT ALLOWED                   
         OC    8(4,R3),8(R3)                                                    
         BZ    BADOPT                                                           
         MVC   TOPOPT,8(R3)                                                     
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT61   CLC   12(3,R3),=C'MAX='   DONT SHOW ABOVE MAX                          
         BNE   EDIT62                                                           
         CLI   NAVE,0                                                           
         BNE   BADOPT              MULTI-AVERAGES NOT ALLOWED                   
         BAS   RE,VALCASH                                                       
         BNE   BADOPT                                                           
         MVC   MAXOPT,WORK                                                      
         LA    R1,MAXCOL                                                        
         BAS   RE,ANYCOL                                                        
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT62   CLC   12(3,R3),=C'MIN='   DONT SHOW BELOW MIN                          
         BNE   EDIT64                                                           
         CLI   NAVE,0                                                           
         BNE   BADOPT              MULTI-AVERAGES NOT ALLOWED                   
         BAS   RE,VALCASH                                                       
         BNE   BADOPT                                                           
         MVC   MINOPT,WORK                                                      
         LA    R1,MINCOL                                                        
         BAS   RE,ANYCOL                                                        
         B     EDITEND                                                          
         SPACE 1                                                                
         SPACE 1                                                                
EDIT64   CLC   12(5,R3),=C'SURVEY' SURVEY OPTION                                
         BNE   EDIT68                                                           
         MVC   SURVOPT,22(R3)                                                   
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT68   CLC   12(4,R3),=C'OVER'   DEMO OVERRIDE OPTION                         
         BNE   EDIT70                                                           
         MVC   OVEROPT,22(R3)                                                   
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT70   CLC   12(2,R3),=C'15'     15 SECOND OPTIONS                            
         BNE   EDIT72                                                           
         CLI   RESSRCE,C'P'        OPTION NOT FOR PROGRAM AVG.                  
         BNE   EDIT70A                                                          
         CLI   22(R3),C'Y'                                                      
         BE    BADOPT                                                           
         CLI   22(R3),C'O'                                                      
         BE    BADOPT                                                           
EDIT70A  MVC   FFTNOPT,22(R3)                                                   
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT72   CLC   12(4,R3),=C'SOLO'   SOLO OPTION                                  
         BNE   EDIT74                                                           
         CLI   RESSRCE,C'P'        OPTION NOT FOR PROGRAM AVG.                  
         BNE   EDIT72A                                                          
         CLI   22(R3),C'N'                                                      
         BE    BADOPT                                                           
EDIT72A  CLI   NAVE,0                                                           
         BNE   BADOPT              MULTI-AVERAGES NOT ALLOWED                   
         MVC   SOLOOPT,22(R3)                                                   
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT74   CLC   12(6,R3),=C'SCHEME'                                              
         BNE   EDIT78                                                           
         CLI   RESSRCE,C'P'        OPTION FOR PROGRAM AVG. ONLY                 
         BNE   BADOPT                                                           
         MVC   SCHEMOPT,22(R3)                                                  
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT78   CLC   12(2,R3),=C'52'                                                  
         BNE   EDIT80                                                           
         CLI   RESSRCE,C'P'        OPTION FOR PROGRAM AVG. ONLY                 
         BE    EDIT78A                                                          
         CLI   22(R3),C'Y'                                                      
         BE    BADOPT                                                           
EDIT78A  MVC   HUT52OPT,22(R3)                                                  
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT80   CLC   12(3,R3),=C'SEP'                                                 
         BNE   EDIT82                                                           
         MVI   SEPOPT,C'N'                                                      
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT82   CLC   12(3,R3),=C'NOV'                                                 
         BNE   EDIT84                                                           
         MVI   NOVOPT,C'N'                                                      
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT84   CLC   12(3,R3),=C'HUT'    HUT=123.45 PERCENT ADJUST                    
         BNE   EDIT86                                                           
         ZIC   R1,1(R3)            L'SECOND HALF                                
         ST    R1,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,22(R3)                                              
         CLI   DMCB,X'FF'                                                       
         BE    BADOPT                                                           
         OC    DMCB+4(2),DMCB+4                                                 
         BNZ   BADOPT                                                           
         MVC   HUTOVER,DMCB+6                                                   
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT86   CLC   12(4,R3),=C'TYPE'   HUT TYPE (D, A, I OR C)                      
         BNE   EDIT88                                                           
         CLI   RESSRCE,C'P'        OPTION FOR PROGRAM AVG. ONLY                 
         BNE   BADOPT                                                           
         MVC   TYPEOPT,22(R3)                                                   
         CLI   22(R3),C'D'         DIARY                                        
         BE    EDITEND                                                          
         CLI   22(R3),C'A'         ASCRIBED                                     
         BE    EDITEND                                                          
         CLI   22(R3),C'I'         INTEGRATED                                   
         BE    EDITEND                                                          
         CLI   22(R3),C'C'         CONFORMED                                    
         BE    EDITEND                                                          
         CLI   22(R3),C'O'         NAD/OPTIONAL                                 
         BE    EDITEND                                                          
         CLI   22(R3),C'Z'         Z-BOOK (TEST)                                
         BE    EDITEND                                                          
         B     BADOPT                                                           
         SPACE 1                                                                
EDIT88   CLC   12(3,R3),=C'DAY'                                                 
         BNE   EDIT94                                                           
         MVC   DAYOPT,22(R3)                                                    
         CLI   22(R3),C'I'         I - INDIVIDUAL DAYS ONLY                     
         BE    EDITEND                                                          
         B     BADOPT                                                           
         SPACE 1                                                                
EDIT94   CLC   12(4,R3),=C'DOWN'                                                
         BNE   EDIT96                                                           
         CLI   TRACEOPT,C'Y'       (NO DOWNLOADING AND TRACING)                 
         BE    BADOPT                                                           
         MVI   DOWNOPT,C'D'        DOWNLOAD                                     
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT96   CLC   12(5,R3),=C'TRACE'                                               
         BNE   EDIT98                                                           
         CLI   DOWNOPT,C'Y'        (NO DOWNLOADING AND TRACING)                 
         BE    BADOPT                                                           
         MVI   TRACEOPT,C'Y'       TRACE                                        
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT98   CLC   12(5,R3),=C'EVERY'                                               
         BNE   EDIT100                                                          
         MVI   EVERYOPT,C'Y'                                                    
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT100  CLC   12(3,R3),=C'DPT'    DAYPART OPTION                               
         BNE   EDIT102                                                          
         CLI   RESSRCE,C'P'                                                     
         BNE   BADOPT                                                           
         MVC   DPTOPT,22(R3)                                                    
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT102  DS    0H                                                               
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
EDITEND  LA    R3,42(R3)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,EDIT46                                                        
         B     YES                                                              
         EJECT                                                                  
*              EDIT FILTERS                                                     
         SPACE 3                                                                
EDITFILT NTR1                                                                   
         XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0                                                          
         BE    YES                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R3,BLOCK                                                         
         MVI   FIELDERR,1                                                       
         LA    R4,FILTERS                                                       
         LTR   R0,R0                                                            
         BZ    BADFILT                                                          
         SPACE 1                                                                
FILT2    CLI   1(R3),0             GENERAL FILTERS - NO KEYWORD                 
         BNE   FILT4                                                            
         ZIC   R1,0(R3)                                                         
         CH    R1,=H'6'                                                         
         BL    *+8                                                              
         LA    R1,6                                                             
         LTR   R1,R1                                                            
         BZ    FILT40                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),12(R3)      SAVE THIS FILTER                             
         LA    R4,6(R4)            AND ADDRESS THE NEXT AREA                    
         B     FILT40                                                           
         SPACE 1                                                                
FILT4    CLC   12(3,R3),=C'TOP'    SHOW ONLY TOP NN                             
         BNE   FILT12                                                           
         OC    8(4,R3),8(R3)                                                    
         BZ    BADFILT                                                          
         MVC   TOPOPT,8(R3)                                                     
         B     FILT40                                                           
         SPACE 1                                                                
FILT12   CLC   12(3,R3),=C'MAX'    DONT SHOW ABOVE MAX                          
         BNE   FILT14                                                           
         BAS   RE,VALCASH                                                       
         BNE   BADFILT                                                          
         MVC   MAXOPT,WORK                                                      
         LA    R1,MAXCOL                                                        
         BAS   RE,ANYCOL                                                        
         B     FILT40                                                           
         SPACE 1                                                                
FILT14   CLC   12(3,R3),=C'MIN'    DONT SHOW BELOW MIN                          
         BNE   FILT16                                                           
         BAS   RE,VALCASH                                                       
         BNE   BADFILT                                                          
         MVC   MINOPT,WORK                                                      
         LA    R1,MINCOL                                                        
         BAS   RE,ANYCOL                                                        
         B     FILT40                                                           
         SPACE 1                                                                
FILT16   CLC   12(2,R3),=C'15'     15 SECOND OPTIONS                            
         BNE   FILT18                                                           
         MVC   FFTNOPT,22(R3)                                                   
         B     EDITEND                                                          
         SPACE 1                                                                
FILT18   CLC   12(4,R3),=C'SOLO'   SOLO OPTION                                  
         BNE   FILT20                                                           
         MVC   SOLOOPT,22(R3)                                                   
         B     EDITEND                                                          
         SPACE 1                                                                
FILT20   CLC   12(3,R3),=C'EFF'    EFFECTIVE DATE FILTER                        
         BNE   FILT22                                                           
         CLI   11(R3),0            SECOND OPERAND S/B 1-3                       
         BE    BADFILT                                                          
         CLI   11(R3),3                                                         
         BH    BADFILT                                                          
         MVC   EFFOPT,11(R3)                                                    
         B     EDITEND                                                          
         SPACE 1                                                                
FILT22   DS    0H                                                               
BADFILT  MVC   CONHEAD(L'FILTERR),FILTERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
FILT40   LA    R3,32(R3)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,FILT2                                                         
*                                                                               
         LA    R0,10                                                            
         LA    R4,FILTERS                                                       
FILT50   OC    0(6,R4),0(R4)                                                    
         BZ    YES                                                              
         CLI   FILTERS,C'-'                                                     
         BE    FILT55                                                           
         CLI   0(R4),C'-'                                                       
         BE    BADFILT                                                          
         B     FILT60                                                           
FILT55   CLI   0(R4),C'-'                                                       
         BNE   BADFILT                                                          
FILT60   LA    R4,6(R4)                                                         
         BCT   R0,FILT50                                                        
         B     YES                                                              
         EJECT                                                                  
*              EDIT DETAILS                                                     
         SPACE 3                                                                
VALIDET  NTR1                                                                   
         GOTO1 ANY                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R4,4(R1)                                                         
         LA    R3,BLOCK                                                         
         MVI   FIELDERR,1                                                       
         LTR   R4,R4                                                            
         BZ    NO                                                               
         STC   R4,NDETS                                                         
         LA    R5,DETS                                                          
         SR    R6,R6                                                            
         SPACE 1                                                                
VDET2    BAS   RE,CHEKLIST                                                      
         BNE   BADDET                                                           
         MVC   0(1,R5),12(R3)      SAVE FIRST LETTER                            
         MVC   1(3,R5),WORK        AND NUMBER/WIDTH/AVE CONTROL                 
         ZIC   RE,WORK+1                                                        
         LA    R6,1(RE,R6)         ADD WIDTH+1                                  
         LA    R3,32(R3)                                                        
         AI    FIELDERR,1                                                       
         LA    R5,4(R5)                                                         
         BCT   R4,VDET2                                                         
         STC   R6,WDETS                                                         
         B     XIT                                                              
         SPACE 1                                                                
BADDET   MVC   CONHEAD(L'DETERR),DETERR                                         
         B     MYCURSOR                                                         
         EJECT                                                                  
*              EDIT AVERAGES                                                    
         SPACE 2                                                                
VALIAVE  NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    YES                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R4,4(R1)                                                         
         MVI   FIELDERR,1                                                       
         LA    R3,BLOCK                                                         
         LTR   R4,R4                                                            
         BZ    BADAVE                                                           
         LR    R1,R4                                                            
         BCTR  R1,0                                                             
         STC   R1,NAVE             SAVE NUMBER OF AVERAGES(-1)                  
         SPACE 1                                                                
VAVE2    CLI   12(R3),C'R'         R AVERAGE MEANS REPORT                       
         BNE   VAVE4                                                            
         MVC   REPAVE,22(R3)                                                    
         B     VAVE10                                                           
         SPACE 1                                                                
VAVE4    BAS   RE,CHEKLIST                                                      
         BNE   BADAVE                                                           
         LA    R1,DETS             OTHERWISE AVERAGE REFERS TO                  
         ZIC   R0,NDETS            PREVIOUSLY SPECIFIED DETAIL                  
         BCT   R0,VAVE6            BUT NOT MOST DETAILED LEVEL                  
         B     BADAVE                                                           
         SPACE 1                                                                
VAVE6    CLC   1(1,R1),WORK                                                     
         BE    VAVE8                                                            
         CLI   0(R1),C'R'          CANT HAVE AVERAGES OR TOTALS                 
         BE    BADAVE              BELOW RANK                                   
         LA    R1,4(R1)                                                         
         BCT   R0,VAVE6                                                         
         B     BADAVE                                                           
         SPACE 1                                                                
VAVE8    MVC   3(1,R1),22(R3)                                                   
         CLI   3(R1),C' '                                                       
         BH    *+8                                                              
         MVI   3(R1),C'A'          DEFAULT IS AVERAGE                           
         CLI   3(R1),C'A'          ALLOW A T OR B                               
         BE    VAVE10                                                           
         CLI   3(R1),C'T'                                                       
         BE    VAVE10                                                           
         CLI   3(R1),C'B'                                                       
         BE    VAVE10                                                           
         B     BADAVE                                                           
         SPACE 1                                                                
VAVE10   LA    R3,32(R3)                                                        
         AI    FIELDERR,1                                                       
         BCT   R4,VAVE2                                                         
         B     XIT                                                              
         SPACE 1                                                                
BADAVE   MVC   CONHEAD(L'AVEERR),AVEERR                                         
         B     MYCURSOR                                                         
         EJECT                                                                  
*              ROUTINE TO CHECK AGAINST LIST                                    
         SPACE 3                                                                
CHEKLIST NTR1                                                                   
         ZIC   R2,0(R3)                                                         
         LTR   R2,R2                                                            
         BZ    NO                                                               
         BCTR  R2,0                                                             
         LA    R1,DETLIST                                                       
         SPACE 1                                                                
CL2      EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),0(R1)                                                   
         BE    CL4                                                              
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BNE   CL2                                                              
         B     NO                                                               
         SPACE 1                                                                
CL4      CLC   RESSRCE(3),=C'NAD'  NAD EXCEPTIONS                               
         BNE   CL4A                                                             
         CLI   8(R1),10            CODE                                         
         BE    NO                                                               
         CLI   8(R1),14            DATE                                         
         BE    NO                                                               
*                                                                               
CL4A     CLC   RESSRCE(3),=C'NTI'  NIT EXCEPTIONS                               
         BNE   CL4B                                                             
         CLI   8(R1),10            CODE                                         
         BE    NO                                                               
*                                                                               
CL4B     CLI   RESSRCE,C'P'        P EXCEPTIONS                                 
         BNE   CL5                                                              
         CLI   8(R1),2             BOOK                                         
         BE    NO                                                               
         CLI   8(R1),18            NUMBER                                       
         BE    NO                                                               
         CLI   8(R1),19            FAD                                          
         BE    NO                                                               
*                                                                               
CL5      MVC   WORK(4),8(R1)                                                    
         B     YES                                                              
         SPACE 1                                                                
DETLIST  DS    0D                                                               
         DC    C'NETWORK ',AL1(01,07,00,00)                                     
         DC    C'BOOK    ',AL1(02,06,00,00)                                     
         DC    C'WEEKS   ',AL1(03,05,00,00)                                     
         DC    C'MONTH   ',AL1(04,05,00,00)                                     
         DC    C'QUARTER ',AL1(05,07,00,00)                                     
         DC    C'YEAR    ',AL1(06,04,00,00)                                     
         DC    C'PROGRAM ',AL1(07,16,00,00)                                     
         DC    C'DAY     ',AL1(08,05,00,00)                                     
         DC    C'TIME    ',AL1(09,11,00,00)                                     
         DC    C'CODE    ',AL1(10,06,00,00)                                     
         DC    C'RANK    ',AL1(11,05,00,00)                                     
         DC    C'NTI     ',AL1(12,06,00,00)                                     
         DC    C'FILTER  ',AL1(13,06,00,00)                                     
         DC    C'DATE    ',AL1(14,05,00,00)                                     
         DC    C'LENGTH  ',AL1(15,06,00,00)                                     
         DC    C'DAYPART ',AL1(16,07,00,00)                                     
         DC    C'DP      ',AL1(16,07,00,00)                                     
         DC    C'EFFECT  ',AL1(17,06,00,00)                                     
         DC    C'NUMBER  ',AL1(18,05,00,00)                                     
         DC    C'FAD     ',AL1(19,05,00,00)                                     
         DC    C'QY      ',AL1(20,07,00,00)                                     
         DC    C'EPISODE ',AL1(21,16,00,00)                                     
         DC    C'LPROGRA ',AL1(22,25,00,00)                                     
         DC    C'LEPISOD ',AL1(23,25,00,00)                                     
         DC    X'FF'                                                            
         SPACE 1                                                                
NO       LA    R1,1                                                             
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
*              INPUTS              R3=A(SCANNER BLOCK LINE)                     
*                                  R4=A(OUTPUT COLUMN NUMBER)                   
         SPACE 1                                                                
ANYCOL   MVI   0(R1),1             DEFAULT COLUMN IS 1                          
         CLI   0(R3),4                                                          
         BNER  RE                                                               
         MVC   0(1,R1),15(R3)      MOVE IN SPECIFIED COLUMN                     
         NI    0(R1),X'0F'                                                      
         CLI   0(R1),1                                                          
         BL    BADOPT                                                           
         CLI   0(R1),8                                                          
         BH    BADOPT                                                           
         BR    RE                                                               
         SPACE 1                                                                
VALCASH  NTR1                                                                   
         ZIC   R2,1(R3)                                                         
         GOTO1 CASHVAL,DMCB,22(R3),(R2)                                         
         CLI   DMCB,X'FF'                                                       
         BE    NO                                                               
         MVC   WORK(4),DMCB+4                                                   
         B     YES                                                              
         SPACE 1                                                                
MYCURSOR MVI   ERROR,X'FE'         USING MY OWN ERROR MESSAGE                   
         GOTO1 VCURSERR            AND POSITIONING CURSOR                       
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MESSAGE                   
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 1                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*              ERROR MESSAGES AND TABLES                                        
         SPACE 3                                                                
MANYDEM  DC    C'** ERROR ** TOO MANY DEMOS - LIMIT IS 8'                       
BOOKERR  DC    C'** ERROR ** INVALID BOOK EXPRESSION'                           
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
FILTERR  DC    C'** ERROR ** INVALID FILTER'                                    
DETERR   DC    C'** ERROR ** INVALID DETAIL EXPRESSION'                         
AVEERR   DC    C'** ERROR ** INVALID AVERAGE'                                   
SOONERR  DC    C'** ERROR ** SOON LIMITED TO 16 WEEKS'                          
         SPACE 1                                                                
DEFDEM   DC    X'00',C'R',AL1(01)                                               
         DC    X'00',C'T',AL1(01)                                               
         DC    X'00',C'T',AL1(41)                                               
         DC    X'00',C'T',AL1(47)                                               
         DC    X'00',C'T',AL1(45)                                               
         DC    X'00',C'T',AL1(91)                                               
         DC    X'00',C'T',AL1(97)                                               
         DC    X'00',C'T',AL1(95)                                               
         EJECT                                                                  
*              LITERAL POOL FOR RESEARCH EDIT                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              NERESALL                                                         
         PRINT OFF                                                              
       ++INCLUDE NERESALL1                                                      
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NERESF5D                                                       
         EJECT                                                                  
*              LOCAL STORAGE                                                    
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   OVWORK                                                           
REPAVE   DS    CL1                                                              
DETS     DS    CL40                                                             
NDETS    DS    CL1                                                              
WDETS    DS    CL1                                                              
UNIVOPT  DS    F                                                                
SPACOPT  DS    CL1                                                              
ROWOPT   DS    CL1                                                              
HOMEOPT  DS    F                                                                
SAVBMKT  DS    CL7                                                              
QRTOPT   DS    CL1                                                              
DPOPT    DS    CL1                                                              
AVOPT    DS    CL1                                                              
TOPOPT   DS    F                                                                
MINOPT   DS    F                                                                
MAXOPT   DS    F                                                                
SURVOPT  DS    CL1                                                              
MINCOL   DS    CL1                                                              
MAXCOL   DS    CL1                                                              
OVEROPT  DS    CL1                                                              
FFTNOPT  DS    CL1                                                              
SOLOOPT  DS    CL1                                                              
EFFOPT   DS    CL1                                                              
SCHEMOPT DS    CL1                                                              
HUT52OPT DS    CL1                                                              
TPOPT    DS    CL1                                                              
SEPOPT   DS    CL1                                                              
NOVOPT   DS    CL1                                                              
HUTOVER  DS    H                                                                
TYPEOPT  DS    CL1                                                              
TRACEOPT DS    CL1                                                              
EVERYOPT DS    CL1                                                              
NAVE     DS    CL1                                                              
DOWNOPT  DS    CL1                                                              
DPTOPT   DS    CL1                                                              
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065NERES05   05/01/02'                                      
         END                                                                    
