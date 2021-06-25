*          DATA SET DDPHONLIST AT LEVEL 017 AS OF 05/01/02                      
*PHASE PHONLIST,*                                                               
*INCLUDE CARDS                                                                  
*INCLUDE CENTER                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE SCANNER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE SQUASHER                                                               
         TITLE 'PHONLIST - NAME ADDRESS AND TELEPHONE LIST'                     
PHONLIST CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**PHON**,=V(REGSAVE),RA                                        
         SPACE 1                                                                
         OPEN  (MARKIN,(INPUT))                                                 
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(255),SPACES                                             
         GOTO1 =V(SORTER),PARA,SORTCARD,RECCARD                                 
         SPACE 1                                                                
         GOTO1 =V(DATCON),PARA,(5,0),(0,WORK)                                   
         PACK  DUB,WORK+2(2)       TRANSLATE MONTH                              
         CVB   R1,DUB                                                           
         BCTR  R1,0                                                             
         MH    R1,=H'10'                                                        
         LA    R1,MONTABLE(R1)                                                  
         MVC   DATE(9),1(R1)       UP TO 9 CHARACTER LOWER CASE                 
         ZIC   R2,0(R1)            PICK UP LENGTH                               
         LA    R2,DATE+1(R2)                                                    
         MVC   0(2,R2),=C'19'                                                   
         MVC   2(2,R2),WORK        POP IN YEAR                                  
         MVC   HEADA+142(20),DATE                                               
         MVC   HEADU+142(20),DATE                                               
         EJECT                                                                  
*              MAIN PROGRAM FLOW                                                
         SPACE 3                                                                
INIT     GET   MARKIN,IO                                                        
         CLC   IO(6),=C'TITLE='    FIRST RECORD CAN BE TITLE                    
         BNE   INIT2                                                            
         MVC   HEADA+3(50),IO+6   USE THIS INSTEAD OF DDS                       
         GOTO1 =V(CENTER),DMCB,HEADA+3,50                                       
         MVC   HEADU+3(50),HEADA+3                                              
         B     INIT                                                             
         SPACE 1                                                                
INIT2    OC    IO,SPACES                                                        
         L     R2,=A(ABLOCK)                                                    
         BAS   RE,SCAN                                                          
         LA    R4,STACK                                                         
         LA    R5,150                                                           
         SPACE 1                                                                
READ     MVC   IO,SPACES                                                        
         GET   MARKIN,IO                                                        
         L     R2,=A(BBLOCK)                                                    
         BAS   RE,SCAN                                                          
         SPACE 1                                                                
         LA    R3,IO+1                                                          
         MVC   IO,SPACES                                                        
         GOTO1 GETWORD,DMCB,=C'FIRST',(R3)                                      
         MVC   14(11,R4),0(R3)                                                  
         MVI   11(R3),C' '                                                      
         LA    R3,12(R3)                                                        
         GOTO1 GETWORD,DMCB,=C'NAME',(R3)                                       
         MVC   01(12,R4),0(R3)                                                  
         LA    R3,13(R3)                                                        
         GOTO1 GETWORD,DMCB,=C'PROFS',(R3)                                      
         OC    0(8,R3),SPACES                                                   
         MVC   26(08,R4),0(R3)                                                  
         LA    R3,09(R3)                                                        
         GOTO1 GETWORD,DMCB,=C'EXT',(R3)                                        
         MVC   35(04,R4),0(R3)                                                  
         LA    R3,5(R3)                                                         
         GOTO1 GETWORD,DMCB,=C'TELEPHONE',(R3)                                  
         MVC   40(12,R4),0(R3)                                                  
         CLC   1(4,R4),SPACES      DON'T PUT IF NAME IS SPACES                  
         BE    READ2                                                            
         CLC   14(4,R4),SPACES     OR IF FIRST IS SPACES                        
         BE    READ2                                                            
         GOTO1 =V(SORTER),DMCB,=C'PUT',IO                                       
         SPACE 1                                                                
READ2    LA    R4,52(R4)                                                        
         BCT   R5,READ                                                          
         BAS   RE,PAGE                                                          
         LA    R4,STACK                                                         
         LA    R5,150                                                           
         B     READ                                                             
         SPACE 1                                                                
MARKEOF  CLOSE (MARKIN)                                                         
         BAS   RE,PAGE                                                          
         EJECT                                                                  
*              NOW READ THE RECORDS BACK AND PRINT                              
         SPACE 3                                                                
         MVI   TYPE,C'A'                                                        
         LA    R2,STACK                                                         
         LA    R0,150              3 STACKS OF 50                               
         SPACE 1                                                                
LOOP2    GOTO1 =V(SORTER),PARA,=C'GET'                                          
         L     R6,PARA+4                                                        
         LTR   R6,R6                                                            
         BNZ   LOOP4                                                            
         BAS   RE,PAGE                                                          
         B     LOOPEND                                                          
         SPACE 1                                                                
LOOP4    CLC   LASTLET,1(R6)       CHECK CHANGE OF LETTER                       
         BE    LOOP10                                                           
         LA    R2,52(R2)                                                        
         BCT   R0,LOOP6                                                         
         BAS   RE,PAGE                                                          
         LA    R2,STACK                                                         
         LA    R0,150                                                           
         SPACE 1                                                                
LOOP6    MVC   6(1,R2),1(R6)       SHOW NEW LETTER                              
         LA    R2,52(R2)                                                        
         BCT   R0,LOOP8                                                         
         BAS   RE,PAGE                                                          
         LA    R2,STACK                                                         
         LA    R0,150                                                           
         SPACE 1                                                                
LOOP8    LA    R2,52(R2)                                                        
         BCT   R0,LOOP10                                                        
         BAS   RE,PAGE                                                          
         LA    R2,STACK                                                         
         LA    R0,150                                                           
         SPACE 1                                                                
LOOP10   MVC   0(52,R2),0(R6)                                                   
         MVC   LASTLET,1(R6)                                                    
         LA    R2,52(R2)                                                        
         BCT   R0,LOOP2                                                         
         BAS   RE,PAGE                                                          
         LA    R2,STACK                                                         
         LA    R0,150                                                           
         B     LOOP2                                                            
         SPACE 1                                                                
LOOPEND  GOTO1 =V(PRINT),PARA,P-1,=C'BC01'                                      
         GOTO1 =V(PRINT),PARA,=C'CLOSE'                                         
         GOTO1 =V(SORTER),PARA,=C'END'                                          
         XBASE                                                                  
         EJECT                                                                  
*              PRINT OUT PAGE                                                   
         SPACE 3                                                                
PAGE     NTR1                                                                   
         GOTO1 =V(PRINT),PARA,P-1,=C'BC01'                                      
         BAS   RE,BOXSETA                                                       
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P,HEADA                                                          
         CLI   TYPE,C'A'                                                        
         BE    *+10                                                             
         MVC   P,HEADU                                                          
         BASR  RE,RF                                                            
         MVC   P,SPACES                                                         
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         SPACE 1                                                                
         BAS   RE,BOXSET                                                        
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P,PATTERNA                                                       
         CLI   TYPE,C'A'                                                        
         BE    *+10                                                             
         MVC   P,PATTERNU                                                       
         BASR  RE,RF                                                            
         MVC   P,SPACES                                                         
         BASR  RE,RF                                                            
         LA    R2,STACK                                                         
         LA    R3,50                                                            
         MH    R3,=H'52'                                                        
         LR    R4,R3                                                            
         AR    R3,R2                                                            
         AR    R4,R3                                                            
         LA    R0,50                                                            
         SPACE 1                                                                
PAGE2    MVC   P+2(52),0(R2)                                                    
         MVC   0(52,R2),SPACES                                                  
         MVC   P+56(52),0(R3)                                                   
         MVC   0(52,R3),SPACES                                                  
         MVC   P+110(52),0(R4)                                                  
         MVC   0(52,R4),SPACES                                                  
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P,SPACES                                                         
         LA    R2,52(R2)                                                        
         LA    R3,52(R3)                                                        
         LA    R4,52(R4)                                                        
         BCT   R0,PAGE2                                                         
         B     XIT                                                              
         SPACE 1                                                                
BOXSET   NTR1                                                                   
         L     R8,=V(BOXAREA)                                                   
         USING BOXD,R8                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXWIDTH+3,165                                                   
         MVC   BOXCOLS(165),ACOLS                                               
         CLI   TYPE,C'A'                                                        
         BE    *+10                                                             
         MVC   BOXCOLS(165),UCOLS                                               
         MVC   BOXROWS,MYROWS                                                   
         B     XIT                                                              
         SPACE 1                                                                
BOXSETA  NTR1                                                                   
         L     R8,=V(BOXAREA)                                                   
         USING BOXD,R8                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXWIDTH+3,165                                                   
         MVC   BOXCOLS(165),HEADCOLS                                            
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS,C'T'                                                     
         MVI   BOXROWS+2,C'B'                                                   
         B     XIT                                                              
         EJECT                                                                  
*              SCANNING ROUTINES                                                
         SPACE 3                                                                
SCAN     NTR1                                                                   
         LR    R1,R2               CLEAR FIRST                                  
         LA    R0,20                                                            
         SPACE 1                                                                
SCAN2    XC    0(12,R1),0(R1)                                                   
         MVC   12(48,R1),SPACES                                                 
         LA    R1,60(R1)                                                        
         BCT   R0,SCAN2                                                         
         SPACE 1                                                                
         CLC   IO(132),SPACES                                                   
         BE    XIT                                                              
         LA    R1,IO+131                                                        
         LA    R0,132                                                           
         SPACE 1                                                                
SCAN4    CLI   0(R1),C' '          FIND LENGTH OF INPUT                         
         BNE   SCAN6                                                            
         BCTR  R1,0                                                             
         BCT   R0,SCAN4                                                         
         SPACE 1                                                                
SCAN6    STC   R0,HEAD+5                                                        
         GOTO1 =V(SCANNER),DMCB,(38,HEAD),(20,(R2)),0                           
         B     XIT                                                              
         EJECT                                                                  
*              PICK OUT SELECTED WORD                                           
         SPACE 3                                                                
GETWORD  NTR1                                                                   
         LR    R9,R1                                                            
         MVI   0(R9),0                                                          
         LM    R4,R5,0(R1)         A(SEARCH WORD)                               
*                                  A(OUTPUT)                                    
         L     R2,=A(ABLOCK)                                                    
         L     R3,=A(BBLOCK)                                                    
         LA    R0,20                                                            
         SPACE 1                                                                
GW2      ZIC   R1,0(R2)            L'STACKED KEY WORD                           
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R2),0(R4)                                                   
         BE    GW4                                                              
         LA    R2,60(R2)                                                        
         LA    R3,60(R3)                                                        
         BCT   R0,GW2                                                           
         B     XIT                                                              
         SPACE 1                                                                
GW4      ZIC   R1,0(R3)            L'DATA                                       
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         STC   R1,0(R9)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),12(R3)                                                   
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
DUB      DS    D                                                                
WORK     DS    CL32                                                             
DMCB     DS    6F                                                               
SPACES   DS    CL256                                                            
CITY     DS    CL64                                                             
HEAD     DS    CL8                                                              
IO       DS    CL256                                                            
LASTLET  DC    C' '                                                             
TYPE     DC    C'U'                                                             
NEWYORK  DC    X'D585A640E89699926B'                                            
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=52'                                    
PARA     DS    6F                                                               
PFILL    DS    CL1                                                              
P        DC    CL165' '                                                         
P2       DC    CL165' '                                                         
P3       DC    CL165' '                                                         
ACOLS    DS    0F                                                               
         DC    CL2' '                                                           
         DC    C'L'                                                             
         DC    CL11' '                                                          
         DC    C'C'                                                             
         DC    CL12' '                                                          
         DC    C'C'                                                             
         DC    CL8' '                                                           
         DC    C'C'                                                             
         DC    CL4' '                                                           
         DC    C'C'                                                             
         DC    CL12' '                                                          
         DC    C'R'                                                             
         DC    CL1' '                                                           
         DC    C'L'                                                             
         DC    CL11' '                                                          
         DC    C'C'                                                             
         DC    CL12' '                                                          
         DC    C'C'                                                             
         DC    CL8' '                                                           
         DC    C'C'                                                             
         DC    CL4' '                                                           
         DC    C'C'                                                             
         DC    CL12' '                                                          
         DC    C'R'                                                             
         DC    CL1' '                                                           
         DC    C'L'                                                             
         DC    CL11' '                                                          
         DC    C'C'                                                             
         DC    CL12' '                                                          
         DC    C'C'                                                             
         DC    CL8' '                                                           
         DC    C'C'                                                             
         DC    CL4' '                                                           
         DC    C'C'                                                             
         DC    CL12' '                                                          
         DC    C'R'                                                             
         DC    100C' '                                                          
         SPACE 1                                                                
UCOLS    DS    0F                                                               
         DC    CL2' '                                                           
         DC    C'L'                                                             
         DC    CL12' '                                                          
         DC    C'C'                                                             
         DC    CL11' '                                                          
         DC    C'C'                                                             
         DC    CL8' '                                                           
         DC    C'C'                                                             
         DC    CL4' '                                                           
         DC    C'C'                                                             
         DC    CL12' '                                                          
         DC    C'R'                                                             
         DC    CL1' '                                                           
         DC    C'L'                                                             
         DC    CL12' '                                                          
         DC    C'C'                                                             
         DC    CL11' '                                                          
         DC    C'C'                                                             
         DC    CL8' '                                                           
         DC    C'C'                                                             
         DC    CL4' '                                                           
         DC    C'C'                                                             
         DC    CL12' '                                                          
         DC    C'R'                                                             
         DC    CL1' '                                                           
         DC    C'L'                                                             
         DC    CL12' '                                                          
         DC    C'C'                                                             
         DC    CL11' '                                                          
         DC    C'C'                                                             
         DC    CL8' '                                                           
         DC    C'C'                                                             
         DC    CL4' '                                                           
         DC    C'C'                                                             
         DC    CL12' '                                                          
         DC    C'R'                                                             
         DC    100C' '                                                          
         SPACE 1                                                                
HEADCOLS DC    CL2' '                                                           
         DC    C'L'                                                             
         DC    CL51' '                                                          
         DC    C'R'                                                             
         DC    CL53' '                                                          
         DC    CL2' '                                                           
         DC    C'L'                                                             
         DC    CL51' '                                                          
         DC    C'R'                                                             
         DC    CL4' '                                                           
MYROWS   DC    C'     T M'                                                      
         DC    50C' '                                                           
         DC    CL100'B'                                                         
         SPACE 1                                                                
PATTERNA DS    0F                                                               
         DC    CL2' '              GAP                                          
         DC    CL1' '                                                           
         DC    CL11'First Name'                                                 
         DC    CL1' '                                                           
         DC    CL12'   Surname'                                                 
         DC    CL1' '                                                           
         DC    CL8' Profs.'                                                     
         DC    CL1' '                                                           
         DC    CL4'Ext'                                                         
         DC    CL1' '                                                           
         DC    CL12' Home Phone'                                                
         DC    CL1' '                                                           
         DC    CL1' '              GAP                                          
         DC    CL1' '                                                           
         DC    CL11'First Name'                                                 
         DC    CL1' '                                                           
         DC    CL12'   Surname'                                                 
         DC    CL1' '                                                           
         DC    CL8' Profs.'                                                     
         DC    CL1' '                                                           
         DC    CL4'Ext'                                                         
         DC    CL1' '                                                           
         DC    CL12' Home Phone'                                                
         DC    CL1' '                                                           
         DC    CL1' '              GAP                                          
         DC    CL1' '                                                           
         DC    CL11'First Name'                                                 
         DC    CL1' '                                                           
         DC    CL12'   Surname'                                                 
         DC    CL1' '                                                           
         DC    CL8' Profs.'                                                     
         DC    CL1' '                                                           
         DC    CL4'Ext'                                                         
         DC    CL1' '                                                           
         DC    CL12' Home Phone'                                                
         DC    CL100' '                                                         
         SPACE 1                                                                
PATTERNU DS    0F                                                               
         DC    CL2' '              GAP                                          
         DC    CL1' '                                                           
         DC    CL12'   Surname'                                                 
         DC    CL1' '                                                           
         DC    CL11'First Name'                                                 
         DC    CL1' '                                                           
         DC    CL8' Profs.'                                                     
         DC    CL1' '                                                           
         DC    CL4'Ext'                                                         
         DC    CL1' '                                                           
         DC    CL12' Home Phone'                                                
         DC    CL1' '                                                           
         DC    CL1' '              GAP                                          
         DC    CL1' '                                                           
         DC    CL12'   Surname'                                                 
         DC    CL1' '                                                           
         DC    CL11'First Name'                                                 
         DC    CL1' '                                                           
         DC    CL8' Profs.'                                                     
         DC    CL1' '                                                           
         DC    CL4'Ext'                                                         
         DC    CL1' '                                                           
         DC    CL12' Home Phone'                                                
         DC    CL1' '                                                           
         DC    CL1' '              GAP                                          
         DC    CL1' '                                                           
         DC    CL12'   Surname'                                                 
         DC    CL1' '                                                           
         DC    CL11'First Name'                                                 
         DC    CL1' '                                                           
         DC    CL8' Profs.'                                                     
         DC    CL1' '                                                           
         DC    CL4'Ext'                                                         
         DC    CL1' '                                                           
         DC    CL12' Home Phone'                                                
         DC    CL100' '                                                         
         SPACE 1                                                                
HEADA    DC    CL14' '                                                          
         DC    CL28'    DDS Telephone Listing   '                               
         DC    CL73' '                                                          
         DC    CL38'First Name Sequence                    '                    
         DC    CL20' '                                                          
         SPACE 1                                                                
HEADU    DC    CL12' '                                                          
         DC    CL28'    DDS Telephone Listing   '                               
         DC    CL72' '                                                          
         DC    CL38'  Surname Sequence                     '                    
         DC    CL20' '                                                          
         SPACE 1                                                                
MONTABLE DS    0F                                                               
         DC    AL1(7),CL9'January'                                              
         DC    AL1(8),CL9'February'                                             
         DC    AL1(5),CL9'March'                                                
         DC    AL1(5),CL9'April'                                                
         DC    AL1(3),CL9'May'                                                  
         DC    AL1(4),CL9'June'                                                 
         DC    AL1(4),CL9'July'                                                 
         DC    AL1(6),CL9'August'                                               
         DC    AL1(9),CL9'September'                                            
         DC    AL1(7),CL9'October'                                              
         DC    AL1(8),CL9'November'                                             
         DC    AL1(8),CL9'December'                                             
         SPACE 1                                                                
DATE     DC    CL20' '                                                          
MARKIN   DCB   DSORG=PS,EODAD=MARKEOF,RECFM=FB,BLKSIZE=2640,           X        
               DDNAME=MARKIN,MACRF=(GM),LRECL=132                               
         SPACE 1                                                                
         LTORG                                                                  
STACK    DC    200CL52' '                                                       
ABLOCK   DS    20CL60                                                           
BBLOCK   DS    20CL60                                                           
       ++INCLUDE DDBIGBOX                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017DDPHONLIST05/01/02'                                      
         END                                                                    
