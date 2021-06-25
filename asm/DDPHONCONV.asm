*          DATA SET DDPHONCONV AT LEVEL 012 AS OF 05/01/02                      
*PHASE PHONCONV,*                                                               
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
         OPEN  (MARKOUT,(OUTPUT))                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(255),SPACES                                             
*              MAIN PROGRAM FLOW                                                
         SPACE 3                                                                
INIT     GET   MARKIN,IO                                                        
         CLC   IO(6),=C'TITLE='    FIRST RECORD CAN BE TITLE                    
         BE    INIT                                                             
         OC    IO,SPACES                                                        
         L     R2,=A(ABLOCK)                                                    
         BAS   RE,SCAN                                                          
         SPACE 1                                                                
READ     MVC   IO,SPACES                                                        
         GET   MARKIN,IO                                                        
         MVC   P,IO                                                             
         GOTO1 =V(PRINT),DMCB,P-1,=C'BLO1'                                      
         L     R2,=A(BBLOCK)                                                    
         BAS   RE,SCAN                                                          
         SPACE 1                                                                
*        LA    R3,OFIRST                                                        
         MVC   IO,SPACES                                                        
*        GOTO1 GETWORD,DMCB,=C'FIRST',(R3)                                      
         LA    R3,ONAME                                                         
         GOTO1 GETWORD,DMCB,=C'NAME',(R3)                                       
         LA    R3,OPROFS                                                        
         GOTO1 GETWORD,DMCB,=C'PROFS',(R3)                                      
         OC    0(8,R3),SPACES                                                   
         LA    R3,OEXT                                                          
         GOTO1 GETWORD,DMCB,=C'EXT',(R3)                                        
         LA    R3,OHOME                                                         
         GOTO1 GETWORD,DMCB,=C'TELEPHONE',(R3)                                  
*        MVC   OCOMP,=C'DDS'                                                    
*        MVC   ODEPT,=C'CS '                                                    
*        MVC   OFILT,=C'N***'                                                   
*        CLI   OHOME,C' '                                                       
*        BNH   *+14                                                             
*        MVI   OFILT,C'Y'                                                       
*        MVC   ODEPT,=C'SYS'                                                    
         MVC   P,IO                                                             
         GOTO1 =V(PRINT),DMCB,P-1,=C'BLO1'                                      
         PUT   MARKOUT,IO                                                       
         B     READ                                                             
         SPACE 1                                                                
MARKEOF  CLOSE (MARKIN)                                                         
         CLOSE (MARKOUT)                                                        
         GOTO1 =V(PRINT),PARA,=C'CLOSE'                                         
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
IO       DS    0CL255                                                           
         DC    CL01' '                                                          
ONAME    DC    CL34' '                                                          
         DC    CL01' '                                                          
*FIRST   DC    CL12' '                                                          
*        DC    CL01' '                                                          
OPROFS   DC    CL08' '                                                          
         DC    CL01' '                                                          
OEXT     DC    CL06' '                                                          
         DC    CL01' '                                                          
OHOME    DC    CL12' '                                                          
         DC    CL01' '                                                          
*COMP    DC    CL03'DDS'                                                        
         DC    CL01' '                                                          
*DEPT    DC    CL03'CS '                                                        
         DC    CL01' '                                                          
*FILT    DC    CL04' '                                                          
         DC    CL255' '                                                         
         DC    CL07' '                                                          
         DC    CL01' '                                                          
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
MARKOUT  DCB   DSORG=PS,RECFM=FB,BLKSIZE=2640,                         X        
               DDNAME=MARKOUT,MACRF=(PM),LRECL=132                              
         SPACE 1                                                                
         LTORG                                                                  
STACK    DC    200CL52' '                                                       
ABLOCK   DS    20CL60                                                           
BBLOCK   DS    20CL60                                                           
       ++INCLUDE DDBIGBOX                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012DDPHONCONV05/01/02'                                      
         END                                                                    
