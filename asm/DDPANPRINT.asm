*          DATA SET DDPANPRINT AT LEVEL 050 AS OF 08/21/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE PANPRNTA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE EXPAND                                                                 
*INCLUDE PANIC                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE XSORT                                                                  
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'MODULE TO PRINT PANVALET BOOKS'                                 
PANPRINT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**PANP**,=V(REGSAVE),R9                                        
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     R8,=V(BOXAREA)                                                   
         USING BOXD,R8                                                          
*                                                                               
         GOTO1 =V(STXITER),PARA,A(DUMPLIST)                                     
         B     MAIN                                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(PANPRINT),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN     DS    0H                                                               
         LA    R1,TITLPOOL         CLEAR TITLPOOL (LEN=12800)                   
         LA    R0,100                                                           
         XC    0(128,R1),0(R1)                                                  
         LA    R1,128(R1)                                                       
         BCT   R0,*-10                                                          
         EJECT                                                                  
*              CONTROL CARD READING                                             
         SPACE 3                                                                
NEXTBOOK MVC   TITLE,SPACES                                                     
         SPACE 2                                                                
CARD     GOTO1 =V(CARDS),PARA,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BNE   CARD2                                                            
         SPACE 2                                                                
CARD1    GOTO1 =V(PANIC),PARA,=C'CLOSE'                                         
         GOTO1 =V(PRINT),PARA,=C'CLOSE'                                         
         XBASE                                                                  
         SPACE 2                                                                
CARD2    MVC   P(80),C                                                          
         BAS   RE,SCAN                                                          
         CLC   SCANWORD,=C'TITLE'                                               
         BE    CARD                                                             
         ZAP   LINE,=P'75'                                                      
         ZAP   PAGE,=P'1'                                                       
         MVI   FIRST,C'Y'                                                       
         CLC   C(7),=C'NUMBER='                                                 
         BNE   CARD3                                                            
         CLI   C+7,C'0'                                                         
         BL    CARD                                                             
         CLI   C+7,C'9'                                                         
         BH    CARD                                                             
         PACK  FIRSTPGE,C+7(1)                                                  
         B     CARD                                                             
         SPACE 2                                                                
CARD3    DS    0H                                                               
         CLC   C(6),=C'LINES='                                                  
         BNE   CARD3A                                                           
         MVC   DUB(2),=C'00'       CHECK 2 NUMERICS                             
         MVZ   DUB(2),C+6                                                       
         BNE   CARDERR                                                          
         PACK DUB,C+6(2)                                                        
         CP    DUB,=P'50'                                                       
         BL    CARDERR                                                          
         ZAP   MAXLINE,DUB                                                      
         B     CARD                                                             
*                                                                               
CARDERR  MVC   P(24),=C'* ERROR *  INVALID PARAM'                               
         MVC   P+26(80),C                                                       
         GOTO1 =V(PRINTER)                                                      
         XBASE                                                                  
*                                                                               
CARD3A   DS    0H                                                               
         CLC   C(7),=C'LINEUP='                                                 
         BNE   *+12                                                             
         BAS   RE,LINEUP                                                        
         B     CARD                                                             
         CLC   C(10),=C'PAGE=PRINT'                                             
         BNE   *+12                                                             
         MVI   PAGEOPT,C'Y'                                                     
         B     CARD                                                             
         CLC   C(9),=C'PAGESKIP='                                               
         BNE   *+14                                                             
         MVC   PAGESKIP,C+9                                                     
         B     CARD                                                             
         CLC   C(5),=C'MID1='                                                   
         BNE   *+14                                                             
         MVC   MID1(75),C+5                                                     
         B     CARD                                                             
         CLC   C(5),=C'MID2='                                                   
         BNE   *+14                                                             
         MVC   MID2(75),C+5                                                     
         B     CARD                                                             
         CLC   C(5),=C'MID3='                                                   
         BNE   *+14                                                             
         MVC   MID3(75),C+5                                                     
         B     CARD                                                             
         CLC   C(5),=C'MID4='                                                   
         BNE   *+14                                                             
         MVC   MID4(75),C+5                                                     
         B     CARD                                                             
         CLC   C(5),=C'SUB1='                                                   
         BNE   *+14                                                             
         MVC   SUB1(75),C+5                                                     
         B     CARD                                                             
         CLC   C(5),=C'SUB2='                                                   
         BNE   *+14                                                             
         MVC   SUB2(75),C+5                                                     
         B     CARD                                                             
         CLC   C(5),=C'SUB3='                                                   
         BNE   *+14                                                             
         MVC   SUB3(75),C+5                                                     
         B     CARD                                                             
         CLC   C(07),=C'IGNORE='                                                
         BNE   *+14                                                             
         MVC   IGNORE,C+7                                                       
         B     CARD                                                             
         CLC   C(7),=C'DDS=YES'                                                 
         BNE   *+12                                                             
         MVI   DDS,C'Y'                                                         
         B     CARD                                                             
         CLC   C(6),=C'DDS=NO'                                                  
         BNE   *+12                                                             
         MVI   DDS,C'N'                                                         
         B     CARD                                                             
         CLC   C(6),=C'UK=YES'                                                  
         BNE   *+12                                                             
         MVI   UK,C'Y'                                                          
         B     CARD                                                             
         CLC   C(5),=C'UK=NO'                                                   
         BNE   *+12                                                             
         MVI   UK,C'N'                                                          
         B     CARD                                                             
         CLC   C(6),=C'US=YES'                                                  
         BNE   *+12                                                             
         MVI   US,C'Y'                                                          
         B     CARD                                                             
         CLC   C(5),=C'US=NO'                                                   
         BNE   *+12                                                             
         MVI   US,C'N'                                                          
         B     CARD                                                             
         CLC   C(5),=C'FILL='                                                   
         BNE   *+14                                                             
         MVC   FILL,C+5                                                         
         B     CARD                                                             
         CLC   C(11),=C'SEQUENCE=NO'                                            
         BNE   *+12                                                             
         MVI   SEQOPT,C'N'                                                      
         B     CARD                                                             
         CLC   C(12),=C'SEQUENCE=YES'                                           
         BNE   *+12                                                             
         MVI   SEQOPT,C'Y'                                                      
         B     CARD                                                             
         CLC   C(14),=C'SPACING=SINGLE'                                         
         BNE   *+12                                                             
         MVI   SPACOPT,C'1'                                                     
         B     CARD                                                             
         CLC   C(14),=C'SPACING=DOUBLE'                                         
         BNE   *+12                                                             
         MVI   SPACOPT,C'2'                                                     
         B     CARD                                                             
         CLC   C(14),=C'SPACING=TRIPLE'                                         
         BNE   *+12                                                             
         MVI   SPACOPT,C'3'                                                     
         B     CARD                                                             
         CLC   C(10),=C'INCLUDE=NO'                                             
         BNE   *+12                                                             
         MVI   INCOPT,0                                                         
         B     CARD                                                             
         CLC   C(11),=C'INCLUDE=YES'                                            
         BNE   *+12                                                             
         MVI   INCOPT,X'80'                                                     
         B     CARD                                                             
         CLC   C(9),=C'INDEX=YES'                                               
         BNE   *+12                                                             
         MVI   INDEXOPT,C'Y'                                                    
         B     CARD                                                             
         CLC   C(9),=C'INDEX=SEPARATE'                                          
         BNE   *+16                                                             
         MVI   INDEXOPT,C'Y'                                                    
         MVI   INDALONE,C'Y'                                                    
         B     CARD                                                             
         CLC   C(8),=C'INDEX=NO'                                                
         BNE   *+12                                                             
         MVI   INDEXOPT,C'N'                                                    
         B     READ                                                             
         CLC   C(4),=C'BIG='                                                    
         BNE   *+12                                                             
         BAS   RE,THINKBIG                                                      
         B     CARD                                                             
         CLC   C(8),=C'REVERSE='                                                
         BNE   *+14                                                             
         MVC   REVMASK,C+8                                                      
         B     CARD                                                             
         CLC   C(9),=C'IEBUPDTE='  IF IEBUPDTE=YES, THEN THE MEMBER             
         BNE   *+14                 IS ASSUMED TO CONTAIN INPUT TO              
         MVC   IEBUPDTE,C+9         IBM'S IEBUPDTE UTILITY                      
         B     CARD                                                             
         CLC   C(10),=C'STRIPE=YES'                                             
         BNE   *+16                                                             
         MVI   BOXSHADE,4                                                       
         MVI   BOXSHCH1,X'30'                                                   
         B     CARD                                                             
         MVC   SPACING+3(1),SPACOPT                                             
         CLC   C(7),=C'COPIES='                                                 
         BNE   CARD4                                                            
         CLI   C+8,C' '                                                         
         BNE   *+14                                                             
         MVC   C+8(1),C+7                                                       
         MVI   C+7,C'0'                                                         
         MVC   DUB(2),=C'00'       CHECK NUMERIC COPIES                         
         MVZ   DUB(2),C+7                                                       
         CLC   DUB(2),=C'00'                                                    
         BNE   CARD                                                             
         PACK  COPIES,C+7(2)                                                    
         B     CARD                                                             
         SPACE 2                                                                
CARD4    ZAP   COPYWORK,COPIES                                                  
         ZAP   PANSEQ,=P'0'                                                     
         GOTO1 =V(PANIC),PARA,=C'READ',=C'DIRECTORY',C,P                        
         TM    PARA+8,X'10'                                                     
         BO    READ1                                                            
         MVC   SAVEDIR,P                                                        
         B     DIR1                                                             
         SPACE 2                                                                
SAVEDIR  DS    CL80                                                             
         SPACE 2                                                                
DIR1     DS    0H                                                               
         MVC   P,SPACES                                                         
         CLI   SEQOPT,C'N'                                                      
         BNE   DIR1A                                                            
         CLI   SW110,C'Y'                                                       
         BNE   DIR2                                                             
         MVI   MONTHS,C'N'         TELL PRINT80 TO INDENT                       
         B     DIR2                                                             
         SPACE 2                                                                
DIR1A    MVC   P(11),=C'BOOK NAME ='                                            
         MVC   P+12(10),SAVEDIR                                                 
         MVI   SPACING+3,C'2'                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(14),=C'LEVEL NUMBER ='                                         
         MVC   P+15(3),SAVEDIR+10                                               
         MVI   SPACING+3,C'2'                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(11),=C'BOOK TYPE ='                                            
         MVC   P+12(5),SAVEDIR+18                                               
         CLC   P+12(4),=C'ASMB'                                                 
         BNE   *+10                                                             
         MVC   P+12(4),=C'BAL '                                                 
         MVI   SPACING+3,C'2'                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(16),=C'EFFECTIVE DATE ='                                       
         MVI   SPACING+3,C'2'                                                   
         MVC   P+17(8),SAVEDIR+26                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(10),=C'RUN DATE ='                                             
         GOTO1 =V(DATCON),PARA,(5,0),(10,P+11)                                  
         MVI   SPACING+3,C'2'                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(22),=C'NUMBER OF STATEMENTS ='                                 
         LA    R2,SAVEDIR+50                                                    
         EDIT  (C5,(R2)),(5,P+23),ALIGN=LEFT                                    
         GOTO1 =V(PRINTER)                                                      
         SPACE 2                                                                
DIR2     ZAP   PAGE,FIRSTPGE                                                    
         MVI   SPACING+3,C'1'                                                   
         ZAP   LINE,=P'75'                                                      
         SPACE 2                                                                
READ0    CLI   INDEXOPT,C'Y'                                                    
         BNE   READ                                                             
         MVI   PHASE,1                                                          
         L     R1,=V(PRINT)        FIRST TIME THROUGH PATCH TO NOPRINT          
         MVC   BORROW,0(R1)                                                     
         MVC   0(2,R1),=X'07FE'                                                 
         EJECT                                                                  
*              NOW CONTROL READING FILE                                         
         SPACE 3                                                                
READ     DS   0H                                                                
         SR    R2,R2                                                            
         IC    R2,INCOPT                                                        
         GOTO1 =V(PANIC),PARA,((R2),=C'READ'),=C'PAN',C,P                       
         TM    PARA+8,X'80'                                                     
         BZ    READ1                                                            
         TM    PARA+8,X'40'                                                     
         BNO   READAA                                                           
         MVC   P(16),=C'** READ ERROR **   '                                    
         MVC   P+20(10),C                                                       
         GOTO1 =V(PRINTER)                                                      
         B     CARD1                                                            
         SPACE 2                                                                
READAA   MVC   TITLE,SPACES                                                     
         CLI   INDEXOPT,C'Y'                                                    
         BNE   READB                                                            
         CLI   PHASE,1                                                          
         BNE   READA                                                            
         MVI   PHASE,2                                                          
         ZAP   PANSEQ,=P'0'                                                     
         L     R1,=V(PRINT)                                                     
         MVC   0(2,R1),BORROW                                                   
         BAS   RE,CONTENTS                                                      
         B     READ                                                             
         SPACE 2                                                                
READA    MVI   PHASE,1                                                          
         SPACE 2                                                                
READB    BAS   RE,ANYIND                                                        
         CP    COPYWORK,=P'1'                                                   
         BE    NEXTBOOK                                                         
         SP    COPYWORK,=P'1'                                                   
         ZAP   PAGE,=P'1'                                                       
         ZAP   LINE,=P'75'                                                      
         MVI   FIRST,C'Y'                                                       
         MVC   REVMASK,SPACES                                                   
         ZAP   PANSEQ,=P'0'                                                     
         B     READ0                                                            
         SPACE 2                                                                
READ1    DS    0H                                                               
         TM    PARA+8,X'10'                                                     
         BZ    READ2                                                            
         MVC   P(10),C                                                          
         MVC   P+20(18),=C'** BOOK MISSING **'                                  
         GOTO1 =V(PRINTER)                                                      
         B     NEXTBOOK                                                         
         SPACE 2                                                                
FIRST    DC    C'Y'                                                             
         SPACE 2                                                                
READ2    CLC   P(19),=C'*          DATA SET'                                    
         BE    READ                                                             
* CHECK FOR UNDERLINE                                                           
         CLC   P(2),=C'*--'                                                     
         BNE   *+8                                                              
         MVI   P,C'-'                                                           
         AP    PANSEQ,=P'1'                                                     
         MVC   SCANWORD,SPACES                                                  
         CLC   P(10),=C'FILTER=OFF'                                             
         BE    READ2B                                                           
         CLI   FILTER,C'Y'                                                      
         BE    READ                                                             
         BAS   RE,SCAN                                                          
         SPACE 2                                                                
READ2B   DS    0H                                                               
         CLI   IEBUPDTE,C'Y'       MEMBER CONTAINS INPUT TO IEBUPDTE ?          
         BNE   READB1                                                           
         CLC   =C'./ ',P           YES: IS THIS A "./" COMMAND STMT?            
         BNE   READB1                                                           
         ZAP   LINE,=P'75'         YES: FORCE A PAGE EJECT                      
         SPACE 2                                                                
READB1   DS    0H                                                               
         CLC   P(4),=C'BIG='                                                    
         BNE   READB2                                                           
         BAS   RE,THINKBIG                                                      
         ZAP   LINE,=P'75'                                                      
         B     READ                                                             
         SPACE 2                                                                
READB2   CLC   P(8),=C'REVERSE='                                                
         BNE   READB4                                                           
         MVC   REVMASK,P+8                                                      
         B     READ                                                             
         SPACE 2                                                                
READB4   MVC   BIGWORD,SPACES                                                   
         MVC   REVMASK,SPACES                                                   
         CLC   P(10),=C'PAGE=START'                                             
         BE    PAGESTRT                                                         
         CLC   P(8),=C'PAGE=END'                                                
         BE    PAGEEND                                                          
         CLI   PAGEMODE,C'Y'                                                    
         BE    PAGELINE                                                         
         SPACE 2                                                                
READB6   MVC   DUB(5),=5C'0'       IF COLS 72-76 ARE NUMERIC                    
         MVZ   DUB(5),P+72         I ASSUME THIS BOOK IS BAL                    
         CLC   DUB(5),=5C'0'                                                    
         BE    READ3A                                                           
         CLI   SEQOPT,C'N'                                                      
         BE    READ3B                                                           
         UNPK  P+81(5),PANSEQ      SO FOR DATA BOOK I'LL SHOW SEQUENCE          
         OI    P+85,X'F0'                                                       
         B     READ3B                                                           
         SPACE 2                                                                
READ3A   DS    0H                                                               
         CLI   SEQOPT,C'N'                                                      
         BNE   *+10                                                             
         MVC   P+72(8),SPACES                                                   
         SPACE 2                                                                
READ3B   DS    0H                                                               
         CLC   SCANWORD,=C'INDEX'                                               
         BNE   READ3C                                                           
         CLI   SEQOPT,C'N'                                                      
         BE    READ3C                                                           
         MVC   P+90(36),P+9                                                     
         UNPK  P+127(5),PANSEQ                                                  
         OI    P+131,X'F0'                                                      
         SPACE 2                                                                
READ3C   DS    0H                                                               
         CLC   SCANWORD,SPACES                                                  
         BNE   READ4                                                            
         CLC   P(7),=C'FILTER='                                                 
         BNE   READ3Z2                                                          
         CLC   P+7(3),=C'OFF'                                                   
         BNE   READ3D                                                           
         MVI   FILTER,C'N'                                                      
         B     READ3Z                                                           
         SPACE 2                                                                
READ3D   CLC   P+7(3),=C'DDS'                                                   
         BNE   READ3E                                                           
         CLI   DDS,C'Y'                                                         
         BE    *+8                                                              
         MVI   FILTER,C'Y'                                                      
         B     READ3Z                                                           
         SPACE 2                                                                
READ3E   CLC   P+7(2),=C'US'                                                    
         BNE   READ3F                                                           
         CLI   US,C'Y'                                                          
         BE    *+8                                                              
         MVI   FILTER,C'Y'                                                      
         B     READ3Z                                                           
         SPACE 2                                                                
READ3F   CLC   P+7(2),=C'UK'                                                    
         BNE   READ3Z                                                           
         CLI   UK,C'Y'                                                          
         BE    *+8                                                              
         MVI   FILTER,C'Y'                                                      
         SPACE 2                                                                
READ3Z   CLI   SEQOPT,C'N'                                                      
         BE    READ                                                             
         SPACE 2                                                                
READ3Z2  CLI   FILTER,C'Y'                                                      
         BE    READ                                                             
         MVC   SPACING+3(1),SPACOPT                                             
         CLC   IGNORE,SPACES                                                    
         BE    READ3Z8                                                          
         LA    R2,P                                                             
         LA    R3,80                                                            
         LA    R4,1                                                             
         SPACE 2                                                                
READ3Z6  CVD   R4,DUB              CLEAR IGNORE FIELDS (SS,EE)                  
         UNPK  WORK(2),DUB                                                      
         OI    WORK+1,X'F0'                                                     
         CLC   WORK(2),IGNORE                                                   
         BL    READ3Z7                                                          
         CLC   WORK(2),IGNORE+3                                                 
         BH    READ3Z7                                                          
         MVI   0(R2),C' '                                                       
         SPACE 2                                                                
READ3Z7  LA    R2,1(R2)                                                         
         LA    R4,1(R4)                                                         
         BCT   R3,READ3Z6                                                       
         SPACE 2                                                                
READ3Z8  GOTO1 =V(PRINTER)                                                      
         B     READ                                                             
         SPACE 2                                                                
READ4    CLC   SCANWORD,=C'SPACE'                                               
         BNE   READ6                                                            
         CLI   SCANWORD+6,C'1'                                                  
         BL    READ                                                             
         CLI   SCANWORD+6,C'9'                                                  
         BH    READ                                                             
         MVC   P(72),SPACES                                                     
         MVC   SPACING+3(1),SCANWORD+6                                          
         OI    SPACING+3,X'F0'                                                  
         GOTO1 =V(PRINTER)                                                      
         MVC   SPACING+3(1),SPACOPT                                             
         B     READ                                                             
         SPACE 2                                                                
READ6    CLC   SCANWORD,=C'EJECT'                                               
         BE    READ8                                                            
         CLC   SCANWORD,=C'TITLE'                                               
         BNE   READ10                                                           
         SPACE 2                                                                
READ8    ZAP   LINE,=P'75'                                                      
         B     READ                                                             
         SPACE 2                                                                
READ10   CLC   SCANWORD,=C'PRINT'                                               
         BNE   READ                                                             
         CLC   0(12,R2),=C'PRINT SINGLE'                                        
         BNE   *+12                                                             
         MVI   SPACOPT,C'1'                                                     
         B     READ                                                             
         CLC   0(12,R2),=C'PRINT DOUBLE'                                        
         BNE   *+12                                                             
         MVI   SPACOPT,C'2'                                                     
         B     READ                                                             
         CLC   0(12,R2),=C'PRINT TRIPLE'                                        
         BNE   *+12                                                             
         MVI   SPACOPT,C'3'                                                     
         B     READ                                                             
         MVC   SCANWORD,SPACES                                                  
         B     READB4                                                           
         EJECT                                                                  
*              PAGE ROUTINES                                                    
         SPACE 3                                                                
PAGESTRT MVI   PAGEMODE,C'Y'                                                    
         LAY   R2,PAGEBLOC                                                      
         LA    R3,64                                                            
         SPACE 2                                                                
PAGE2    MVC   0(132,R2),SPACES                                                 
         LA    R2,132(R2)                                                       
         BCT   R3,PAGE2                                                         
         CLI   PAGEOPT,C'Y'                                                     
         BE    READB6                                                           
         B     READ                                                             
         SPACE 2                                                                
PAGELINE MVC   WORK(2),=2X'F0'     LINE NUMBER                                  
         MVZ   WORK(2),P+1                                                      
         CLC   WORK(2),=2X'F0'                                                  
         BNE   READ                                                             
         PACK  DUB,P+1(2)                                                       
         CVB   R3,DUB                                                           
         CHI   R3,64                                                            
         BH    READ                                                             
         LTR   R3,R3                                                            
         BZ    READ                                                             
         BCTR  R3,0                                                             
         MHI   R3,132                                                           
         LAY   R2,PAGEBLOC                                                      
         AR    R2,R3                                                            
         CLI   P,C'R'                                                           
         BNE   *+8                                                              
         LA    R2,66(R2)                                                        
         MVC   0(66,R2),P+3                                                     
         CLI   PAGEOPT,C'Y'                                                     
         BE    READB6                                                           
         B     READ                                                             
         SPACE 2                                                                
PAGEEND  MVI   PAGEMODE,C'N'                                                    
         CLI   PAGEOPT,C'Y'                                                     
         BNE   PAGEEND1                                                         
         GOTO1 =V(PRINTER)                                                      
         SPACE 2                                                                
PAGEEND1 DS    0H                                                               
         LAY   R2,PAGEBLOC                                                      
         LA    R3,58                                                            
         GOTO1 =V(PRINT),PARA,SPACES-1,=C'BC01'                                 
         MVC   P(132),SPACES                                                    
         SPACE 2                                                                
PAGEEND2 CLI   SW110,C'Y'                                                       
         BNE   *+14                                                             
         MVC   P+2(110),0(R2)                                                   
         B     *+10                                                             
         MVC   P(132),0(R2)                                                     
         LA    R4,P                                                             
         BCTR  R4,0                                                             
         GOTO1 =V(PRINT),PARA,(R4),=C'BL01'                                     
         LA    R2,132(R2)                                                       
         BCT   R3,PAGEEND2                                                      
         B     READ                                                             
         EJECT                                                                  
*              ROUTINE TO PRINT A LINEUP PATTERN                                
         SPACE 3                                                                
LINEUP   NTR1                                                                   
         LA    R2,60                                                            
         GOTO1 =V(PRINT),PARA,SPACES-1,=C'BC01'                                 
         MVI   MYP,C'X'                                                         
         MVC   MYP+1(79),MYP                                                    
         CLC   C+7(2),=C'80'                                                    
         BE    LINEUP2                                                          
         CLC   C+7(3),=C'YES'                                                   
         BE    LINEUP2                                                          
         MVC   MYP+1(109),MYP                                                   
         MVI   SW110,C'Y'                                                       
         SPACE 2                                                                
LINEUP2  GOTO1 =V(PRINT),PARA,MYP-1,=C'BL01'                                    
         BCT   R2,LINEUP2                                                       
         MVC   P,SPACES                                                         
         XIT1                                                                   
         EJECT                                                                  
*              CONTROL THE BIG LETTER PRINTING FEATURE                          
         SPACE 3                                                                
THINKBIG NTR1                                                                   
         CLC   BIGWORD,SPACES                                                   
         BNE   BIG2                                                             
         GOTO1 =V(PRINT),PARA,SPACES-1,=C'BC01'                                 
         SPACE 2                                                                
BIG2     MVC   BIGWORD,P+4                                                      
         LA    R2,BIGWORD                                                       
         LA    R3,BIGWORK+81                                                    
         LA    R4,8                                                             
         CLC   BIGWORD+0(8),SPACES CENTRE THE WORD                              
         BE    BIG5                                                             
         LA    R5,BIGWORD+7                                                     
         SPACE 2                                                                
BIG3     CLI   0(R5),C' '                                                       
         BNE   BIG4                                                             
         LA    R3,5(R3)                                                         
         BCT   R5,BIG3                                                          
         SPACE 2                                                                
BIG4     GOTO1 =V(EXPAND),PARA,(R2),(72,(R3))                                   
         LA    R2,1(R2)                                                         
         CLI   0(R2),C' '                                                       
         BE    BIG5                                                             
         LA    R3,10(R3)                                                        
         BCT   R4,BIG4                                                          
         SPACE 2                                                                
BIG5     DS    0H                                                               
         CLC   REVMASK,SPACES      REVERSE LOGIC                                
         BE    BIG20                                                            
         LA    R5,REVMASK+71                                                    
         SPACE 2                                                                
BIG6     CLI   0(R5),C' '          FIND END OF MASK                             
         BNE   BIG8                                                             
         BCT   R5,BIG6                                                          
         SPACE 2                                                                
BIG8     LA    R2,BIGWORK                                                       
         LA    R3,800                                                           
         SPACE 2                                                                
BIG10    CLI   0(R2),C' '                                                       
         BNE   BIG12                                                            
         MVC   0(1,R2),REVMASK                                                  
         B     BIG14                                                            
         SPACE 2                                                                
BIG12    MVI   0(R2),C' '                                                       
         SPACE 2                                                                
BIG14    IC    R6,REVMASK          ROTATE MASK                                  
         MVC   REVMASK(71),REVMASK+1                                            
         STC   R6,0(R5)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,BIG10                                                         
         SPACE 2                                                                
BIG20    LA    R2,BIGWORK-1        NOW PRINT AND CLEAR                          
         LA    R3,10                                                            
         SPACE 2                                                                
BIG22    CLI   SEQOPT,C'N'                                                      
         BNE   *+14                                                             
         MVC   BIGP+12(80),0(R2)                                                
         B     *+10                                                             
         MVC   BIGP(80),0(R2)                                                   
         GOTO1 =V(PRINT),PARA,BIGP-1,=C'BL01'                                   
         MVC   0(80,R2),SPACES                                                  
         LA    R2,80(R2)                                                        
         BCT   R3,BIG22                                                         
         XIT1                                                                   
         SPACE 2                                                                
BIGP     DC    CL132' '                                                         
         EJECT                                                                  
*              SCAN FOR KEYWORDS                                                
         SPACE 3                                                                
SCAN     NTR1                                                                   
         LA    R2,P+1                                                           
         MVC   SCANWORD,SPACES                                                  
         CLI   P,C' '                                                           
         BNE   SCANXIT                                                          
         LA    R3,75                                                            
         SPACE 2                                                                
SCAN2    MVC   SCANWORD(7),0(R2)                                                
         CLC   SCANWORD,=C'SPACE'                                               
         BE    SCANXIT                                                          
         CLC   SCANWORD(6),=C'EJECT '                                           
         BE    SCANXIT                                                          
         CLC   SCANWORD,=C'PRINT'                                               
         BE    SCANXIT                                                          
         CLC   SCANWORD(7),=C'TITLE '''                                         
         BE    SCAN4                                                            
         CLC   SCANWORD(7),=C'INDEX '''                                         
         BE    SCAN3                                                            
         MVC   SCANWORD,SPACES                                                  
         CLI   0(R2),C' '                                                       
         BNE   SCANXIT                                                          
         LA    R2,1(R2)                                                         
         BCT   R3,SCAN2                                                         
         B     SCANXIT                                                          
         SPACE 2                                                                
SCAN3    LA    R2,7(R2)                                                         
         LA    R3,INDEX                                                         
         MVC   INDEX,SPACES                                                     
         LA    R4,60                                                            
         B     SCAN6                                                            
         SPACE 2                                                                
SCAN4    LA    R2,7(R2)                                                         
         LA    R3,TITLE                                                         
         MVC   TITLE,SPACES                                                     
         LA    R4,60                                                            
         SPACE 2                                                                
SCAN6    CLI   0(R2),C''''                                                      
         BE    SCAN8                                                            
         MVC   0(1,R3),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,SCAN6                                                         
         SPACE 2                                                                
SCAN8    LA    R2,TITLPOOL                                                      
         LA    R3,199                                                           
         SPACE 2                                                                
SCAN10   OC    0(64,R2),0(R2)                                                   
         BZ    SCAN12                                                           
         LA    R2,64(R2)                                                        
         BCT   R3,SCAN10                                                        
         SPACE 2                                                                
SCAN12   MVC   0(60,R2),TITLE                                                   
         MVC   60(4,R2),PAGE                                                    
         CLC   SCANWORD,=C'TITLE'                                               
         BE    SCANXIT                                                          
         MVC   0(60,R2),INDEX                                                   
         SP    60(4,R2),=P'1'                                                   
         OI    60(R2),X'80'                                                     
         SPACE 2                                                                
SCANXIT  XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              SORT AND PRINT THE INDEX                                         
         SPACE 3                                                                
CONTENTS NTR1                                                                   
         LA    R2,TITLPOOL                                                      
         LA    R3,200                                                           
         MVC   SAVETITL,TITLE                                                   
         MVC   SAVMID1,MID1                                                     
         MVC   SAVMID2,MID2                                                     
         MVC   SAVMID3,MID3                                                     
         MVC   SAVMID4,MID4                                                     
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID3,SPACES                                                      
         MVC   MID4,SPACES                                                      
         MVC   SAVSUB1,SUB1                                                     
         MVC   SAVSUB2,SUB2                                                     
         MVC   SAVSUB3,SUB3                                                     
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   SUB3,SPACES                                                      
         ZAP   LINE,=P'75'                                                      
         MVC   TITLE,=CL60'CONTENTS'                                            
         ZAP   PAGE,=P'1'                                                       
         SPACE 2                                                                
CONT2    OC    0(64,R2),0(R2)                                                   
         BZ    CONT4                                                            
         TM    60(R2),X'80'                                                     
         BO    CONT3                                                            
         MVC   P(60),0(R2)                                                      
         BAS   RE,FILLUP                                                        
         EDIT  (P4,60(R2)),(4,P+61)                                             
         MVI   SPACING+3,C'2'                                                   
         GOTO1 =V(PRINTER)                                                      
         SPACE 2                                                                
CONT3    DS    0H                                                               
         XC    0(64,R2),0(R2)                                                   
         LA    R2,64(R2)                                                        
         BCT   R3,CONT2                                                         
         SPACE 2                                                                
CONT4    MVC   TITLE,SAVETITL                                                   
         ZAP   PAGE,FIRSTPGE                                                    
         ZAP   LINE,=P'75'                                                      
         B     ANYIND6                                                          
         SPACE 2                                                                
ANYIND   NTR1                                                                   
         MVC   SAVMID1,MID1                                                     
         MVC   SAVMID1,MID2                                                     
         MVC   SAVMID3,MID3                                                     
         MVC   SAVMID4,MID4                                                     
         MVC   SAVSUB1,SUB1                                                     
         MVC   SAVSUB2,SUB2                                                     
         MVC   SAVSUB3,SUB3                                                     
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID3,SPACES                                                      
         MVC   MID4,SPACES                                                      
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   SUB3,SPACES                                                      
         SPACE 2                                                                
         SPACE 2                                                                
CONT6    DS    0H                                                               
         LA    R2,TITLPOOL                                                      
         LA    R3,200                                                           
         GOTO1 =V(XSORT),PARA,(R2),(R3),64,60,0                                 
         MVC   SAVETITL,TITLE                                                   
         ZAP   LINE,=P'75'                                                      
         ZAP   PAGE,=P'1'                                                       
         MVC   TITLE,=CL60'INDEX'                                               
         SPACE 2                                                                
ANYIND2  OC    0(64,R2),0(R2)                                                   
         BZ    ANYIND4                                                          
         CLI   INDEXOPT,C'Y'                                                    
         BNE   ANYIND4                                                          
         CLI   INDALONE,C'Y'                                                    
         BNE   ANYIND3                                                          
         TM    60(R2),X'80'                                                     
         BZ    ANYIND4                                                          
         SPACE 2                                                                
ANYIND3  DS    0H                                                               
         MVC   P(60),0(R2)                                                      
         BAS   RE,FILLUP                                                        
         NI    60(R2),X'7F'                                                     
         EDIT  (P4,60(R2)),(4,P+61)                                             
         MVI   SPACING+3,C'2'                                                   
         GOTO1 =V(PRINTER)                                                      
         SPACE 2                                                                
ANYIND4  XC    0(64,R2),0(R2)                                                   
         LA    R2,64(R2)                                                        
         BCT   R3,ANYIND2                                                       
         MVC   TITLE,SAVETITL                                                   
         SPACE 2                                                                
ANYIND6  MVC   MID1,SAVMID1                                                     
         MVC   MID2,SAVMID2                                                     
         MVC   MID3,SAVMID3                                                     
         MVC   MID4,SAVMID4                                                     
         MVC   SUB1,SAVSUB1                                                     
         MVC   SUB2,SAVSUB2                                                     
         MVC   SUB3,SAVSUB3                                                     
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO FILL INDEX AND CONTENTS LINES                         
         SPACE 3                                                                
FILLUP   NTR1                                                                   
         LA    R2,P+59                                                          
         LA    R3,60                                                            
         SPACE 2                                                                
FILLUP2  CLI   0(R2),C' '                                                       
         BNE   FILLUP4                                                          
         MVC   0(1,R2),FILL                                                     
         BCTR  R2,0                                                             
         BCT   R3,FILLUP2                                                       
         SPACE 2                                                                
FILLUP4  XIT1                                                                   
         EJECT                                                                  
*              WORK SPACE                                                       
         SPACE 3                                                                
PARA     DS    6F                                                               
C        DS    CL80                                                             
SCANWORD DS    CL5                                                              
         DS    CL2                                                              
DDS      DC    C'Y'                                                             
UK       DC    C'Y'                                                             
US       DC    C'Y'                                                             
FILTER   DC    C'N'                                                             
FILL     DC    C' '                                                             
INDEXOPT DC    C'N'                                                             
INDALONE DC    C'N'                                                             
IEBUPDTE DC    C'N'                ASSUME THIS ISN'T IEBUPDTE INPUT             
SAVETITL DS    CL60                                                             
SEQOPT   DC    C'Y'                                                             
SW110    DC    C'N'                                                             
SPACOPT  DC    C'1'                                                             
FIRSTPGE DC    P'1'                                                             
INCOPT   DC    X'80'                                                            
COPIES   DC    PL2'1'                                                           
PANSEQ   DC    PL4'0'                                                           
PAGEMODE DC    C'N'                                                             
PAGEOPT  DC    C'N'                                                             
PAGESKIP DC    C'Y'                                                             
COPYWORK DC    PL2'1'                                                           
BORROW   DS    CL2                                                              
PHASE    DS    CL1                                                              
DUB      DS    D                                                                
DUB1     DS    D                                                                
WORK     DS    CL32                                                             
INDEX    DS    CL60                                                             
BIGWORD  DC    CL8' '                                                           
REVMASK  DC    CL72' '                                                          
IGNORE   DC    CL5' '                                                           
SAVMID1  DC    CL132' '                                                         
SAVMID2  DC    CL132' '                                                         
SAVMID3  DC    CL132' '                                                         
SAVMID4  DC    CL132' '                                                         
SAVSUB1  DC    CL132' '                                                         
SAVSUB2  DC    CL132' '                                                         
SAVSUB3  DC    CL132' '                                                         
         DS    C' '                                                             
MYP      DC    CL132' '                                                         
         DC    CL8' '                                                           
BIGWORK  DC    800C' '                                                          
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
TITLPOOL DS    0D                                                               
         DS    12800X'00'                                                       
         DC    64X'00'                                                          
*                                                                               
PAGEBLOC DS    0D                                                               
         DS    64CL132                                                          
         EJECT                                                                  
* INCLUDE DDBIGBOX                                                              
* INCLUDE DDDPRINT                                                              
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050DDPANPRINT08/21/14'                                      
         END                                                                    
