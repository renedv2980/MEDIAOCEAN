*          DATA SET NERES06    AT LEVEL 031 AS OF 05/01/02                      
*PHASE T32106A,*                                                                
         TITLE 'T32106 - NETWORK PROGRAM TREND'                                 
T32106   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NPT***,RA,RR=R2                                              
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
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 3                                                                
VKEY     LA    R2,TRESRCEH         VALIDATE SOURCE                              
         MVI   OPTION,X'20'        ONLY NTI AND NAD SUPPORTED                   
         GOTO1 VVALSRC                                                          
         SPACE 1                                                                
         LA    R2,TREBOOKH         VALIDATE BOOK                                
         MVC   BOOKS(6),=X'000000FFFFFF'                                        
         MVI   MAX,10              DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 VVALBOOK                                                         
         CLI   ACTUAL,2            REALLY ONLY 2 BOOK ALLOWED                   
         BNH   VKEY5                                                            
         MVC   CONHEAD(L'MANYBKS),MANYBKS    TOO MANY BOOKS                     
         B     MYEND                                                            
         SPACE 1                                                                
VKEY5    LA    R2,TREDEMOH         VALIDATE DEMOS                               
         MVI   NUMDEMS,6           PRESET TO 6 DEMO                             
         MVC   DEMOS(19),DEFLIST                                                
         CLI   5(R2),0                                                          
         BE    VKEY7                                                            
         MVI   MAX,20              DUMMY SO I CAN DO ERROR MSG                  
         MVI   NFLDS,1                                                          
         GOTO1 VVALDEM                                                          
         CLI   ACTUAL,8            REALLY ALLOW 8 DEMOS                         
         BNH   VKEY7                                                            
         MVC   CONHEAD(L'MANYDEM),MANYDEM                                       
         B     MYEND                                                            
         SPACE 1                                                                
VKEY7    LA    R2,TRENETH          VALIDATE NETWORK                             
         GOTO1 ANY                 MUST EXIST                                   
         GOTO1 VVALNET                                                          
         MVC   NETSAVE,ACTNET                                                   
         LA    R2,TREPROGH         VALIDATE PROGRAM                             
         GOTO1 ANY                                                              
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   WORK(6),=6X'F0'                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)                                                    
         CLC   WORK(6),=6X'F0'                                                  
         BE    VKEY10                                                           
         MVC   CONHEAD(L'INVNUM),INVNUM                                         
         B     MYEND                                                            
         SPACE 1                                                                
VKEY10   ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STH   R1,DUB                                                           
         MVC   DBSELPRG,DUB                                                     
         MVC   DBSELBK,BOOKS+1                                                  
         MVC   DBSELSTA,NETSAVE                                                 
         GOTO1 VADJSEL                                                          
         GOTO1 VDISPNET                                                         
         MVC   THISNET,WORK                                                     
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,0                                                        
         BE    VKEY12                                                           
         MVC   CONHEAD(L'NOPROG),NOPROG                                         
         B     MYEND                                                            
         SPACE 1                                                                
VKEY12   GOTO1 DEFINE,DMCB,=C'PROG',DBLOCK,WORK                                 
         MVI   TREPNAM,C' '                                                     
         MVC   TREPNAM+1(23),TREPNAM                                            
         MVC   TREPNAM(16),WORK                                                 
         OI    TREPNAMH+6,X'80'                                                 
         LA    R2,TRETITLH         SET USER'S OWN NAME                          
         GOTO1 VVALTITL                                                         
         SPACE 1                                                                
VKEYX    B     XIT                                                              
         EJECT                                                                  
*              PRINT REPORT - INITIALIZE                                        
         SPACE 3                                                                
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVC   RESTITLE,=CL40'NETWORK PROGRAM TREND'                            
         XC    INDEX,INDEX                                                      
         SPACE 1                                                                
         ZIC   R1,NUMDEMS          FIGURE OUT DISPLACEMENT                      
         MH    R1,=H'8'                                                         
         LA    R0,88                                                            
         SR    R0,R1                                                            
         SRL   R0,1                                                             
         ST    R0,DISP                                                          
         SPACE 3                                                                
*              CONTROL OF REPORT                                                
         SPACE 3                                                                
REP2     GOTO1 DEMAND,DMCB,DBLOCK,SPLAT                                         
         CLC   DBSELBK(2),BOOKS+4                                               
         BNL   XIT                                                              
         AI    DBSELBK+1,1         BUMP TO NEXT BOOK                            
         CLI   DBSELBK+1,49                                                     
         BL    REP2                                                             
         AI    DBSELBK,1           WEEK 1 OF NEW YEAR                           
         MVI   DBSELBK+1,1                                                      
         B     REP2                                                             
         EJECT                                                                  
*              ROUTINES TO PRINT THE DATA                                       
         SPACE 3                                                                
SPLAT    NTR1                                                                   
         SPACE 1                                                                
         LA    R6,P                R6 DISPLACES INTO PRINT LINE                 
         A     R6,DISP                                                          
         ZIC   R1,DBSELBK+1        SHOW BOOK                                    
         EDIT  (R1),(2,1(R6)),FILL=0                                            
         MVI   3(R6),C'/'                                                       
         EDIT  (1,DBSELBK),(2,4(R6))                                            
         SPACE 1                                                                
         GOTO1 DEFINE,DMCB,=C'DAY',DBLOCK,WORK                                  
         MVC   7(3,R6),WORK+2                                                   
         SPACE 1                                                                
         GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,WORK                                 
         ZIC   R1,WORK                                                          
         SRL   R1,1                CONVERT TO 1/2 HOUR                          
         MH    R1,=H'5'                                                         
         LA    R1,TIMETAB(R1)                                                   
         MVC   11(5,R6),0(R1)                                                   
         SPACE 1                                                                
         LA    R3,DEMOS                                                         
         ZIC   R4,NUMDEMS                                                       
         LA    R6,22(R6)                                                        
         MVI   FIRSTDEM,C'Y'                                                    
         XC    VALS,VALS                                                        
         SPACE 2                                                                
POST3    GOTO1 DEMOUT,DMCB,(C'D',(R3)),DBLOCK,VALS                              
         CLI   1(R3),C'R'                                                       
         BE    POST4                                                            
         CLI   1(R3),C'C'                                                       
         BE    POST4                                                            
         CLI   1(R3),C'P'                                                       
         BE    POST4                                                            
         CLI   1(R3),C'O'                                                       
         BE    POST4                                                            
         CLI   1(R3),C'Q'                                                       
         BE    POST4                                                            
         CLI   1(R3),C'V'                                                       
         BE    POST4                                                            
         CLI   1(R3),C'X'                                                       
         BE    POST4                                                            
         CLI   1(R3),C'S'                                                       
         BNE   POST6                                                            
         SPACE 2                                                                
POST4    EDIT  (4,VALS),(7,(R6)),1                                              
         B     POST8                                                            
         SPACE 2                                                                
POST6    EDIT  (4,VALS),(7,(R6))                                                
         SPACE 2                                                                
POST8    OC    INDEX,INDEX                                                      
         BNZ   POST10                                                           
         MVC   INDEX,VALS                                                       
         SPACE 2                                                                
POST10   CLI   FIRSTDEM,C'Y'                                                    
         BNE   POST12                                                           
         MVC   VALS+4(4),VALS                                                   
         MVI   FIRSTDEM,C'N'                                                    
         SPACE 1                                                                
POST12   LA    R3,3(R3)                                                         
         LA    R6,8(R6)                                                         
         BCT   R4,POST3                                                         
         SPACE 1                                                                
         L     R1,VALS+4           SHOW INDEX                                   
         OC    INDEX,INDEX                                                      
         BZ    POST14                                                           
         M     R0,=F'200'                                                       
         D     R0,INDEX                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LA    R6,P+17                                                          
         A     R6,DISP                                                          
         EDIT  (R1),(4,(R6))                                                    
         SPACE 1                                                                
POST14   GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         BAS   RE,HOOKHEAD                                                      
         BAS   RE,HOOKBOX                                                       
         MVC   H4+44(8),=C'NTI CODE'                                            
         MVC   H4+53(5),TREPROG                                                 
         MVC   H5+44(16),TREPNAM                                                
         GOTO1 VRESHEAD                                                         
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              HOOK - DEAL WITH HEADINGS                                        
         SPACE 3                                                                
HOOKHEAD NTR1                                                                   
         MVC   RESTITA,SPACES                                                   
         MVC   RESTITB,SPACES                                                   
         MVC   RESTITC,SPACES                                                   
         LA    R3,RESTITA                                                       
         A     R3,DISP                                                          
         MVC   0(22,R3),=C' BOOK  DAY TIME  INDEX   '                           
         SPACE 1                                                                
         LA    R3,23(R3)           NOW POINT TO WHERE                           
         LA    R5,132(R3)          1ST DEMO SHOULD PRINT                        
         LA    R2,DEMOS                                                         
         ZIC   R6,NUMDEMS                                                       
         SPACE 1                                                                
HH2      CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         GOTO1 DEMOCON,PARAS,(0,(R2)),(7,WORK),(0,DBLOCK)                       
         CLI   1(R2),C'I'          RESET FOR NEXT TIME                          
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
         MVC   0(7,R3),WORK        FORMAT                                       
         GOTO1 CENTER,DMCB,(R3),7                                               
         MVC   1(5,R5),WORK+7                                                   
         CLI   1(R5),C'*'                                                       
         BNE   *+10                                                             
         MVC   1(5,R5),=C'(000)'                                                
         CLI   1(R2),C'R'                                                       
         BNE   *+10                                                             
         MVC   1(5,R5),=C'(RTG)'                                                
         CLI   1(R2),C'S'                                                       
         BNE   *+10                                                             
         MVC   1(5,R5),=C'(SHR)'                                                
         CLI   1(R2),C'V'                                                       
         BNE   *+10                                                             
         MVC   1(5,R5),=C'(VPH)'                                                
         LA    R2,3(R2)                                                         
         LA    R3,8(R3)                                                         
         LA    R5,8(R5)                                                         
         BCT   R6,HH2                                                           
         B     XIT                                                              
         EJECT                                                                  
*              HOOK - BOXES                                                     
         SPACE 3                                                                
HOOKBOX  NTR1                                                                   
         MVC   RESCOLS,SPACES                                                   
         LA    R2,RESCOLS                                                       
         A     R2,DISP                                                          
         MVI   0(R2),C'L'                                                       
         MVI   06(R2),C'C'                                                      
         MVI   10(R2),C'C'                                                      
         MVI   16(R2),C'C'                                                      
         SPACE 1                                                                
         LA    R2,22(R2)                                                        
         ZIC   R3,NUMDEMS          NOW SET UP FOR DEMOS                         
         SPACE 1                                                                
HB2      MVI   0(R2),C'C'                                                       
         LA    R2,8(R2)                                                         
         BCT   R3,HB2                                                           
         MVI   0(R2),C'R'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
PATCH    DS    0H                  PATCH AREA                                   
         DC    XL32'00'                                                         
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
         SPACE 2                                                                
DAYTAB   DC    C'M-FMONTUEWEDTHUFRISATSUNM-SVAR'                                
         SPACE 2                                                                
*              CONSTANTS                                                        
         SPACE 1                                                                
MANYBKS  DC    C'** ERROR ** TOO MANY BOOKS - LIMIT IS 2'                       
MANYDEM  DC    C'** ERROR ** TOO MANY DEMOS - LIMIT IS 6'                       
INVNUM   DC    C'** ERROR ** NOT NUMERIC'                                       
NOPROG   DC    C'** ERROR ** NTI PROGRAM NOT RECOGNIZED'                        
         SPACE 1                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,1,C'MEDIA     NETWORK T.V'                                    
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,NETREP                                                     
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
DEFLIST  DC    AL1(0),C'R',AL1(1)                                               
         DC    AL1(0),C'S',AL1(1)                                               
         DC    AL1(0),C'T',AL1(1)                                               
         DC    AL1(0),C'R',AL1(45)                                              
         DC    AL1(0),C'R',AL1(95)                                              
         DC    AL1(0),C'R',AL1(145)                                             
         DC    X'FF'                                                            
         EJECT                                                                  
*              LITERAL POOL                                                     
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              NERESALL2                                                        
         PRINT ON                                                               
       ++INCLUDE NERESALL3                                                      
         PRINT ON                                                               
         SPACE 1                                                                
*              DSECT TO COVER SCREEN                                            
         SPACE 1                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE NERESF6D                                                       
         EJECT                                                                  
*              LOCAL WORKING STORAGE                                            
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
INDEX    DS    F                                                                
VALS     DS    D                                                                
DISP     DS    F                                                                
FIRSTDEM DS    CL1                                                              
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031NERES06   05/01/02'                                      
         END                                                                    
