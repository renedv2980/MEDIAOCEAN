*          DATA SET NERES04    AT LEVEL 043 AS OF 05/01/02                      
*PHASE T32104,*                                                                 
T32104   TITLE '-   TREND REPORT'                                               
T32104   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TREN**,RA,RR=R2                                              
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
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
         SPACE 3                                                                
VKEY     MVI   PRINTOPT,0                                                       
         LA    R2,TRESRCEH         VALIDATE SOURCE                              
         MVI   OPTION,X'7F'        (PROGRAMS NOT SUPPORTED)                     
         GOTO1 VVALSRC                                                          
         LA    R2,TREBOOKH         VALIDATE BOOK                                
         MVI   MAX,10                                                           
         GOTO1 VVALBOOK                                                         
         GOTO1 VEXPBOOK                                                         
         SPACE 1                                                                
VKEY5    LA    R2,TREDEMOH         VALIDATE DEMOS                               
         MVI   MAX,20              DUMMY SO I CAN DO ERROR MSG                  
         MVI   NFLDS,1                                                          
         GOTO1 VVALDEM                                                          
         CLI   ACTUAL,4            REALLY ALLOW 4 DEMOS                         
         BNH   VKEY4                                                            
         MVC   CONHEAD(L'MANYDEM),MANYDEM                                       
         B     MYEND                                                            
         SPACE 1                                                                
VKEY4    LA    R2,TRENETH          VALIDATE NETWORK                             
         LA    R3,3                MAX 3                                        
         LA    R4,NETSAVE                                                       
         XC    NETSAVE,NETSAVE                                                  
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
VKEY6    GOTO1 VVALNET                                                          
         MVC   0(7,R4),ACTNET                                                   
         LA    R4,7(R4)                                                         
VKEY7    BAS   RE,BUMP                                                          
         BCT   R3,*+8                                                           
         B     VKEY8                                                            
         CLI   5(R2),0             ANOTHER NETWORK?                             
         BNE   VKEY6                                                            
         B     VKEY7                                                            
         SPACE 1                                                                
VKEY8    LA    R2,TREDAYH          DAY                                          
         MVI   DAYNUM,X'FF'                                                     
****     CLC   8(3,R2),=C'ALL'                                                  
****     BE    VKEY10                                                           
         GOTO1 VVALDAY                                                          
         MVC   DAYNUM,ACTUAL                                                    
         SPACE 1                                                                
VKEY10   LA    R2,TRESTRTH         START TIME                                   
         GOTO1 VVALTIM                                                          
         MVC   STARTNUM,ACTUAL                                                  
         LA    R2,TREENDH          END TIME                                     
         GOTO1 VVALTIM                                                          
         MVC   ENDNUM,ACTUAL                                                    
         SPACE 1                                                                
         LA    R2,TREOPTH          OPTIONS                                      
         BAS   RE,VALOPT                                                        
         LA    R2,TRETITLH         OWN TITLE                                    
         GOTO1 VVALTITL                                                         
         LA    R2,TREFILTH         FILTERS                                      
         GOTO1 VVALFILT                                                         
         SPACE 1                                                                
         LA    R2,TRENDXH          INDEX BOOK                                   
         CLI   5(R2),0             OPTIONAL INPUT FIELD                         
         BE    VKEYX                                                            
         MVI   MAX,2               DUMMY SO I CAN DO ERROR MSG                  
         MVC   SAVBOOKS,BOOKS      SAVE BOOKS                                   
         ZIC   R0,NUMBOOKS         AND NUMBER OF BOOKS                          
         GOTO1 VVALBOOK                                                         
         MVC   BOOK+30(3),BOOK     SAVE INDEX BOOK HERE                         
         MVC   BOOKS(30),SAVBOOKS  RESTORE REAL BOOKS                           
         STC   R0,NUMBOOKS                                                      
         LA    R1,TREBOOKH         RESTORE MAIN BOOK ADDRESS                    
         ST    R1,ARESBOOK                                                      
         OI    PRINTOPT,X'40'      INDICATE INDEX BOOK                          
         CLI   ACTUAL,1            REALLY ONLY 1 BOOK ALLOWED                   
         BE    VKEYX                                                            
         MVC   CONHEAD(L'MANYBKS),MANYBKS                                       
         B     MYEND                                                            
         SPACE 1                                                                
VKEYX    LA    R2,TRESRCEH                                                      
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VALOPT   NTR1                                                                   
         MVI   SPACOPT,1           PRESET VALUES FOR OPTIONS                    
         MVI   DAYOPT,C'P'         P=POCKETPIECE                                
         SPACE 1                                                                
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(8,BLOCK)                                      
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    BADOPT                                                           
         LA    R3,BLOCK                                                         
         SPACE 1                                                                
OPT2     CLC   12(2,R3),=C'S  '                                                 
         BNE   OPT4                                                             
         MVI   SPACOPT,2                                                        
         CLI   22(R3),C'2'                                                      
         BE    OPTEND                                                           
         MVI   SPACOPT,3                                                        
         CLI   22(R3),C'3'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
         SPACE 1                                                                
OPT4     CLC   12(3,R3),=C'DAY'                                                 
         BNE   OPT9                                                             
         CLI   22(R3),C'I'         I=INDIVIDUAL DAYS                            
         BNE   BADOPT                                                           
         MVC   DAYOPT,22(R3)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT9     B     BADOPT                                                           
         SPACE 1                                                                
OPTEND   LA    R3,32(R3)                                                        
         BCT   R4,OPT2                                                          
         B     XIT                                                              
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     MYEND                                                            
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
*              INPUT               R4=A(MAIN DBLOCK)                            
         SPACE 1                                                                
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVC   RESTITLE,=CL40'TREND REPORT'                                     
         EJECT                                                                  
*              CONTROL OF REPORT                                                
         SPACE 3                                                                
REP2     BAS   RE,CLEARPAD                                                      
         LA    R2,NETSAVE                                                       
         SR    R3,R3                                                            
         ZIC   R4,NUMNETS                                                       
         SPACE 1                                                                
REP4     BAS   RE,CONIO                                                         
         LA    R2,7(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,REP4                                                          
         CLC   P,SPACES            DID WE FIND ANYTHING                         
         BE    REP6                                                             
         BAS   RE,PRINTPAD                                                      
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         SPACE 1                                                                
REP6     CLC   STARTNUM,ENDNUM                                                  
         BNL   XIT                                                              
         ZIC   R1,STARTNUM                                                      
         LA    R1,2(R1)                                                         
         STC   R1,STARTNUM                                                      
         B     REP2                                                             
         EJECT                                                                  
*              CONTROL OF IO                                                    
         SPACE 3                                                                
CONIO    NTR1                                                                   
         LA    R6,DBLOCKA                                                       
         USING DBLOCK,R6                                                        
         MVC   DBSELSTA,0(R2)                                                   
         GOTO1 VADJSEL             ADJUST FOR PEOPLE METERS                     
*                                  NOTE R3=NETWORK NUMBER, USED BY POST         
         LA    R2,PAD                                                           
         LA    R4,BOOKS                                                         
         LA    R5,11                                                            
         SPACE 1                                                                
CON2     OC    0(3,R4),0(R4)                                                    
         BZ    CON6                                                             
         MVC   DBSELPUR(1),STARTNUM                                             
         IC    R1,DAYNUM                                                        
         SLL   R1,4                                                             
         STC   R1,DBSELPUR+1                                                    
         CLI   DBFUNCT,DBGETNTI                                                 
         BNE   CON4                                                             
         ZIC   R1,DAYNUM           NTI NEEDS LOGICAL DAY/TIME                   
         LA    R1,DAYLIST(R1)                                                   
         MVC   DBSELDAY,0(R1)                                                   
         ZIC   R1,STARTNUM                                                      
         SR    R0,R0                                                            
         D     R0,=F'4'                                                         
         LA    R1,6(R1)                                                         
         MH    R1,=H'100'                                                       
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,30(R1)                                                        
         CH    R1,=H'2400'                                                      
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STH   R1,DUB                                                           
         MVC   DBSELTIM,DUB                                                     
         MVC   DBSELTIM+2(2),DBSELTIM                                           
         SPACE 1                                                                
CON4     MVC   DBSELBK,1(R4)                                                    
         MVC   DBDAYOPT,DAYOPT     P (POCKETPIECE) OR I (INDIVIDUAL)            
         GOTO1 DEMAND,DMCB,DBLOCKA,POST                                         
         SPACE 1                                                                
CON6     LA    R2,300(R2)                                                       
         LA    R4,3(R4)                                                         
         BCT   R5,CON2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXTRACT VALUES AND POST                               
         SPACE 3                                                                
POST     NTR1                                                                   
         XC    DUB,DUB                                                          
         GOTO1 DEFINE,DMCB,=C'TYPE',DBLOCKA,DUB                                 
         GOTO1 VCHEFILT,DMCB,DUB                                                
         BNE   XIT                                                              
         ZIC   R1,STARTNUM                                                      
         SRL   R1,1                                                             
         MH    R1,=H'5'                                                         
         LA    R1,TIMETAB(R1)                                                   
         MVC   P+1(5),0(R1)                                                     
         USING PADRECD,R2                                                       
         MVC   PADBOOK,1(R4)                                                    
         LR    R5,R3                                                            
         SLL   R3,4                                                             
         LA    R3,PADPROGS(R3)                                                  
         GOTO1 DEFINE,DMCB,=C'PROG',DBLOCKA,WORK                                
         MVC   0(16,R3),WORK                                                    
         MH    R5,=H'32'                                                        
         LA    R5,PADDEMOS(R5)                                                  
         LA    R3,DEMOS                                                         
         ZIC   R4,NUMDEMS          (R5 HAS A(DEMO CHUNK) )                      
         SPACE 1                                                                
POST2    GOTO1 DEMOUT,DMCB,(C'D',(R3)),DBLOCKA,(R5)                             
         LA    R3,3(R3)                                                         
         LA    R5,4(R5)                                                         
         BCT   R4,POST2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT FROM PAD                                        
         SPACE 3                                                                
PRINTPAD NTR1                                                                   
         LA    R2,PAD                                                           
         ZIC   R3,NUMBOOKS                                                      
         ZIC   R4,NUMNETS                                                       
         SPACE 1                                                                
PP2      BAS   RE,DUPES            TAKE OUT DUPLICATE NAMES                     
         BAS   RE,MANY             SEE IF BLOCK WILL FIT ON PAGE                
         SPACE 1                                                                
PP4      BAS   RE,PROGS            DIG OUT PROGRAMS TO P                        
         CLC   P,SPACES                                                         
         BE    PP6                                                              
         OC    P,SPACES                                                         
         GOTO1 SPOOL,PARAS,(R8)                                                 
         SPACE 1                                                                
PP6      CLI   0(R2),0                                                          
         BE    PP8                                                              
         GOTO1 DATCON,PARAS,(3,(R2)),(8,WORK)                                   
         MVC   P+7(3),WORK                                                      
         MVC   P+10(2),WORK+6                                                   
         CLC   TRESRCE(3),=C'NTI'  SHOW NETWORK AS WW/YY                        
         BNE   PP7                                                              
         EDIT  (1,1(R2)),(2,P+7)                                                
         MVI   P+9,C'/'                                                         
         SPACE 1                                                                
PP7      MVI   NUMTYPE,C'D'        DEMOS                                        
         BAS   RE,NUMBERS                                                       
         CLC   P,SPACES                                                         
         BE    PP8                                                              
         OC    P,SPACES                                                         
         GOTO1 SPOOL,PARAS,(R8)                                                 
         OC    BOOK+30(3),BOOK+30                                               
         BZ    PP8                                                              
         BAS   RE,INDEXPAD         INDEX                                        
         MVI   NUMTYPE,C'I'                                                     
         BAS   RE,NUMBERS                                                       
         CLC   P,SPACES                                                         
         BE    PP8                                                              
         OC    P,SPACES                                                         
         GOTO1 SPOOL,PARAS,(R8)                                                 
         SPACE 1                                                                
PP8      LA    R2,300(R2)                                                       
         BCT   R3,PP4                                                           
         BAS   RE,CLEARPAD                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO TAKE OUT DUPLICATE NAMES                             
         SPACE 3                                                                
DUPES    NTR1                                                                   
         LA    R2,PAD+2                                                         
         LA    R5,300(R2)                                                       
         BCT   R3,DUPES2                                                        
         B     XIT                                                              
         SPACE 1                                                                
DUPES2   BAS   RE,DUPES4                                                        
         LA    R2,16(R2)                                                        
         LA    R5,16(R5)                                                        
         BCT   R4,DUPES2                                                        
         B     XIT                                                              
         SPACE 1                                                                
DUPES4   NTR1                                                                   
         SPACE 1                                                                
DUPES6   CLC   0(16,R2),0(R5)                                                   
         BE    DUPES8                                                           
         LR    R2,R5                                                            
         B     DUPES10                                                          
         SPACE 1                                                                
DUPES8   MVC   0(16,R5),SPACES                                                  
         SPACE 1                                                                
DUPES10  LA    R5,300(R5)                                                       
         BCT   R3,DUPES6                                                        
         B     XIT                                                              
         EJECT                                                                  
*              CALCULATION OF HOW MANY LINES WILL PRINT                         
         SPACE 3                                                                
MANY     NTR1                                                                   
         USING PADRECD,R2                                                       
         SR    R5,R5               COUNT LINES IN R5                            
         SPACE 1                                                                
MANY2    CLC   PADPROGS(96),SPACES ANY PROGS TO PRINT                           
         BE    *+8                                                              
         LA    R5,1(R5)                                                         
         OC    PADDEMOS(192),PADDEMOS                                           
         BZ    MANY4                                                            
         LA    R5,1(R5)                                                         
         CLI   NUMDEMS,5           MORE THAN 4 DEMOS REQUIRES 2 LINES           
         BL    *+8                                                              
         LA    R5,1(R5)                                                         
         LA    R6,PAD                                                           
         LA    R6,3098(R6)                                                      
         OC    0(192,R6),0(R6)     ANY INDEX                                    
         BZ    MANY4                                                            
         LA    R5,1(R5)                                                         
         CLI   NUMDEMS,5                                                        
         BL    *+8                                                              
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
MANY4    LA    R2,300(R2)                                                       
         BCT   R3,MANY2                                                         
         STC   R5,ALLOWLIN                                                      
         B     XIT                                                              
         EJECT                                                                  
*              PAD HANDLING ROUTINES                                            
         SPACE 3                                                                
INDEXPAD NTR1                                                                   
         LA    R2,98(R2)           R2=A(LINE TO BE INDEXED)                     
         LA    R3,PAD                                                           
         LA    R3,3098(R3)         R3=A(INDEX DEMOS)                            
         LA    R4,48                                                            
         SPACE 1                                                                
IP2      L     R1,0(R2)                                                         
         XC    0(4,R2),0(R2)                                                    
         M     R0,=F'200'                                                       
         OC    0(4,R3),0(R3)                                                    
         BZ    IP4                                                              
         D     R0,0(R3)                                                         
         AH    R1,=H'1'                                                         
         SRL   R1,1                                                             
         ST    R1,0(R2)                                                         
         SPACE 1                                                                
IP4      LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,IP2                                                           
         B     XIT                                                              
         SPACE 1                                                                
PROGS    NTR1                      DIG OUT PROGRAM NAMES                        
         LA    R3,P+20                                                          
         LA    R2,2(R2)                                                         
         ZIC   R4,NUMNETS                                                       
         SPACE 1                                                                
PROGS2   MVC   0(16,R3),0(R2)                                                   
         GOTO1 CENTER,PARAS,(R3),16                                             
         LA    R2,16(R2)                                                        
         LA    R3,32(R3)                                                        
         BCT   R4,PROGS2                                                        
         B     XIT                                                              
         SPACE 1                                                                
CLEARPAD NTR1                                                                   
         XCEF  PAD,3600                                                         
         LA    R2,PAD                                                           
         USING PADRECD,R2                                                       
         LA    R3,12                                                            
         SPACE 1                                                                
CP2      MVC   PADPROGS(96),SPACES                                              
         LA    R2,300(R2)                                                       
         BCT   R3,CP2                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT THE NUMBERS                                    
         SPACE 3                                                                
NUMBERS  NTR1                                                                   
         USING PADRECD,R2                                                       
         LA    R2,PADDEMOS                                                      
         LA    R3,P+13                                                          
         ZIC   R4,NUMNETS          UP TO 3 NETWORKS                             
         SPACE 1                                                                
NUM2     BAS   RE,NUM4                                                          
         LA    R2,32(R2)                                                        
         LA    R3,32(R3)                                                        
         BCT   R4,NUM2                                                          
         B     XIT                                                              
         SPACE 1                                                                
NUM4     NTR1                      UP TO 4 DEMOS FOR NETWORK                    
         ZIC   R4,NUMDEMS                                                       
         LA    R5,DEMOS                                                         
         SPACE 1                                                                
NUM6     CLI   NUMTYPE,C'I'        (NO DECS FOR INDEX)                          
         BE    NUM7                                                             
         CLI   1(R5),C'R'          CHECK FOR 1 DEC PLACE                        
         BE    NUM8                                                             
         CLI   1(R5),C'P'                                                       
         BE    NUM8                                                             
         CLI   1(R5),C'S'                                                       
         BE    NUM8                                                             
         CLI   1(R5),C'O'          TPT PUT                                      
         BE    NUM8                                                             
         CLI   1(R5),C'Q'          TPT SHARE                                    
         BE    NUM8                                                             
         CLI   1(R5),C'V'                                                       
         BE    NUM8                                                             
         SPACE 1                                                                
NUM7     EDIT  (4,0(R2)),(7,0(R3))                                              
         B     NUM10                                                            
         SPACE 1                                                                
NUM8     EDIT  (4,0(R2)),(7,0(R3)),1,ZERO=BLANK                                 
         SPACE 1                                                                
NUM10    LA    R2,4(R2)                                                         
         LA    R3,8(R3)                                                         
         LA    R5,3(R5)                                                         
         BCT   R4,NUM6                                                          
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H5+76(5),=C'DAY -'                                               
         MVC   H5+82(3),TREDAY                                                  
         TM    PRINTOPT,X'40'      INDEX BOOK                                   
         BNO   HOOK2                                                            
         MVC   H6(5),=C'INDEX '                                                 
         MVC   H6+10(8),TRENDX                                                  
         SPACE 1                                                                
HOOK2    MVC   RESTITA,SPACES                                                   
         MVC   RESTITB,SPACES                                                   
         MVC   RESTITC,SPACES                                                   
         MVC   RESCOLS,SPACES                                                   
         MVC   RESTITA(11),=C' TIME  BOOK'                                      
         MVC   RESCOLS(7),=C'L     C'                                           
         LA    R2,NETSAVE                                                       
         LA    R3,RESTITA+13                                                    
         LA    R5,RESCOLS+12                                                    
         ZIC   R4,NUMNETS                                                       
         SPACE 1                                                                
HOOK4    MVC   14(4,R3),0(R2)      NETWORK ONTO FIRST LINE                      
         CLI   4(R2),C'A'          SHOW ASCRIBED AS ABC                         
         BNE   *+8                                                              
         MVI   17(R3),C' '                                                      
         CLI   4(R2),C'D'          SHOW DIARY AS ABCD                           
         BNE   *+8                                                              
         MVI   17(R3),C'D'                                                      
         CLI   4(R2),C'C'          SHOW CONFORMED AS ABCC                       
         BNE   *+8                                                              
         MVI   17(R3),C'C'                                                      
         CLI   4(R2),C'I'          SHOW INTEGRATED AS ABCI                      
         BNE   *+8                                                              
         MVI   17(R3),C'I'                                                      
         MVI   0(R5),C'C'                                                       
         BAS   RE,HOOKDEMS                                                      
         LA    R2,7(R2)                                                         
         LA    R3,32(R3)                                                        
         LA    R5,32(R5)                                                        
         BCT   R4,HOOK4                                                         
         MVI   0(R5),C'R'                                                       
         GOTO1 VRESHEAD                                                         
         B     XIT                                                              
         SPACE 1                                                                
HOOKDEMS NTR1                      DEMOS ONTO LINES 2 & 3                       
         LA    R2,DEMOS                                                         
         LA    R3,132(R3)                                                       
         ZIC   R4,NUMDEMS                                                       
         SPACE 1                                                                
HOOK6    GOTO1 DEMOCON,DMCB,(0,(R2)),(7,WORK),(0,DBLOCKA)                       
         CLC   WORK+7(5),=C'*****'                                              
         BNE   *+10                                                             
         MVC   WORK+7(5),=C'(000)'                                              
         MVC   0(7,R3),WORK                                                     
         GOTO1 CENTER,DMCB,(R3),7                                               
         MVC   133(5,R3),WORK+7                                                 
         LA    R2,3(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R4,HOOK6                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS AND TABLES                                              
         SPACE 3                                                                
MYEND    MVI   ERROR,X'FE'         USING MYH OWN ERROR MESSAGE                  
         GOTO1 VERRXIT                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
TIMETAB  DS    0H                                                               
         DC    C'6A   630A 7A   730A 8A   830A '                                
         DC    C'9A   930A 10A  1030A11A  1130A'                                
         DC    C'12N  1230P1P   130P 2P   230P '                                
         DC    C'3P   330P 4P   430P 5P   530P '                                
         DC    C'6P   630P 7P   730P 8P   830P '                                
         DC    C'9P   930P 10P  1030P11P  1130P'                                
         DC    C'12M  1230A1A   130A 2A   230A '                                
         SPACE 1                                                                
DAYLIST  DC    X'7C402010080402017F8003FF'                                      
         SPACE 1                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
*              LITERAL POOL AND CONSTANTS                                       
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 1                                                                
*                                  MY OWN ERROR MESSAGES                        
         SPACE 1                                                                
MANYBKS  DC    C'* ERROR * TOO MANY BOOKS - LIMIT IS 10'                        
MANYDEM  DC    C'* ERROR * TOO MANY DEMOS - LIMIT IS 4'                         
OPTERR   DC    C'* ERROR * INVALID OPTION'                                      
NOTPROG  DC    C'* ERROR * PROGRAMS NOT SUPPORTED'                              
         SPACE 3                                                                
*                                  REPORT HEADLINE SPECS                        
         SPACE 1                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,1,C'MEDIA     NETWORK T.V'                                    
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,NETREP                                                     
         SSPEC H5,104,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
*              ARRANGEMENT OF PAD                                               
         SPACE 3                                                                
PADRECD  DSECT                                                                  
PADREC   DS    0CL300                                                           
PADBOOK  DS    CL2                                                              
PADPROGS DS    6CL16                                                            
PADDEMOS DS    6CL32                                                            
         DS    CL10                                                             
         SPACE 1                                                                
*                                  LINES 1-10 BOOK 1-10                         
*                                        11   INDEX BOOK                        
*                                        12   SPARE                             
         SPACE 1                                                                
*              NERESALL HERE                                                    
         PRINT OFF                                                              
       ++INCLUDE NERESALL1                                                      
         PRINT ON                                                               
         EJECT                                                                  
*              DSECT TO COVER SCREEN                                            
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NERESF4D                                                       
         EJECT                                                                  
*              LOCAL WORKING STORAGE                                            
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
PAD      EQU   BUFF                                                             
SPACOPT  DS    XL1                                                              
PRINTOPT DS    XL1                                                              
STARTNUM DS    CL1                                                              
ENDNUM   DS    CL1                                                              
NWKS     DS    CL1                                                              
SAVBOOKS DS    CL30                                                             
NUMTYPE  DS    CL1                                                              
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
         SPACE 1                                                                
*              DDCOMFACTS & FAFACTS                                             
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043NERES04   05/01/02'                                      
         END                                                                    
