*          DATA SET DDLIBXRUS  AT LEVEL 047 AS OF 05/01/02                      
*PHASE LIBXREFA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PANREAD                                                                
*INCLUDE SIXUNPK                                                                
*INCLUDE SORTER                                                                 
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'LIBRARY CROSS-REFERENCE PACKAGE'                                
LIBXREF  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**LBXR**,=V(REGSAVE),R9                                        
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         GOTO1 =V(STXITER),PARA,DUMPLIST                                        
         SPACE 3                                                                
*              INITIALIZATION ROUTINES                                          
         SPACE 3                                                                
INITIAL  L     R2,=A(SORTAREA)                                                  
         GOTO1 =V(SORTER),PARA,SORTCARD,RECCARD,(50,(R2))                       
         GOTO1 =V(CARDS),PARA,C,=C'RE00'                                        
         CLC   C(08),=C'TEST=YES'                                               
         BNE   *+8                                                              
         MVI   TEST,C'Y'                                                        
         EJECT                                                                  
*              INPUT ROUTINES (DIRECTORY HANDLING)                              
         SPACE 3                                                                
INPUT    LA    R3,D                                                             
         USING RCD,R3                                                           
         SPACE 2                                                                
DIRECTRY GOTO1 =V(PANREAD),PARA,SAVEDIR                                         
         SPACE 2                                                                
         TM    0(R1),X'80'                                                      
         BO    OUTPUT                                                           
         CLI   0(R1),1             (PANREAD SETS 1 FOR DIRECTORY)               
         BNE   DIRECTRY                                                         
         B     DY2                                                              
         SPACE 2                                                                
DY2      MVC   BOOK,SAVEDIR                                                     
         CLC   SAVEDIR+10(3),=C'000'                                            
         BE    DIRECTRY            IGNORE SUPERSETS                             
         MVC   SAVEPHAS,SPACES                                                  
         MVC   SAVETITL,SPACES                                                  
         ZAP   SEQUENCE,=P'0'                                                   
         SPACE 2                                                                
DY4      CLC   BOOK(2),=C'RM'      RELOCATABLE BOOK - BUILD RA RECORD           
         BNE   STATEMNT                                                         
         MVC   RECORD,SPACES                                                    
         MVI   RORC,C'R'                                                        
         MVC   NAME,BOOK+2                                                      
         MVI   AORB,C'A'                                                        
         MVC   LEVEL,SAVEDIR+10                                                 
         MVC   DATE,SAVEDIR+34                                                  
         BAS   RE,PUTSORT                                                       
         B     DIRECTRY                                                         
         EJECT                                                                  
*              ROUTINES TO HANDLE *PHASE STATEMENTS                             
         SPACE 3                                                                
STATEMNT GOTO1 =V(PANREAD),PARA,PAN                                             
         TM    0(R1),X'80'         (X'80'=EOF)                                  
         BO    OUTPUT                                                           
         CLI   0(R1),2             (1=DIRECTORY 2=COMMENT 3 STATEMENT)          
         BE    COMCARD                                                          
         BH    ST2                                                              
         MVC   SAVEDIR,PAN                                                      
         B     DY2                                                              
         SPACE 2                                                                
ST2      AP    SEQUENCE,=P'1'                                                   
         CLC   PAN(7),=C'*PHASE '                                               
         BNE   INCLUDE                                                          
         BAS   RE,DIGPHASE                                                      
         CLC   SAVETITL,SPACES                                                  
         BE    PH2                                                              
         MVC   RECORD,SPACES       COMMENT PENDING - RELEASE CA                 
         MVI   RORC,C'C'                                                        
         MVC   NAME,SAVEPHAS                                                    
         MVI   AORB,C'A'                                                        
         MVC   PHASE(50),SAVETITL                                               
         SPACE 2                                                                
PH2      MVC   RECORD,SPACES       BUILD A CB RECORD                            
         MVI   RORC,C'C'                                                        
         MVC   NAME,SAVEPHAS                                                    
         MVI   AORB,C'B'                                                        
         MVC   PANNAME,BOOK                                                     
         MVC   LEVEL,SAVEDIR+10                                                 
         MVC   DATE,SAVEDIR+34                                                  
         BAS   RE,PUTSORT                                                       
         B     STATEMNT                                                         
         EJECT                                                                  
*              ROUTINE TO HANDLE *INCLUDE STATEMENTS                            
         SPACE 3                                                                
INCLUDE  CLC   PAN(07),=C'*INCLUDE '                                            
         BNE   OTHERLNK                                                         
         MVC   RECORD,SPACES       BUILD AN RB RECORD                           
         MVI   RORC,C'R'                                                        
         MVI   AORB,C'B'                                                        
         MVC   PHASE,SAVEPHAS                                                   
         MVC   PANNAME,BOOK                                                     
         MVC   LEVEL,SAVEDIR+10                                                 
         MVC   DATE,SAVEDIR+34                                                  
         MVC   STRIP,SPACES                                                     
         MVC   STRIP(62),PAN+9                                                  
         LA    R4,STRIP                                                         
         LA    R5,62                                                            
         SPACE 2                                                                
INCLUDEC CLI   0(R4),C'$'          CHANGE $ TO S                                
         BNE   *+8                                                              
         MVI   0(R4),C'S'                                                       
         CLI   0(R4),C','          TAKE OUT ,                                   
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         CLI   0(R4),C'('          AND BRACKETS                                 
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         CLI   0(R4),C')'                                                       
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         LA    R4,1(R4)                                                         
         BCT   R5,INCLUDEC                                                      
         LA    R4,STRIP            NOW WRITE AN RB FOR EACH BOOK OR             
*                                      SUB-MODULAR INCLUDE                      
         SPACE 2                                                                
INCLUDED CLC   0(3,R4),SPACES                                                   
         BE    STATEMNT                                                         
         SPACE 2                                                                
INCLUDEE CLI   0(R4),C' '          POSITION R4 TO START OF NAME                 
         BNE   INCLUDEF                                                         
         LA    R4,1(R4)                                                         
         B     INCLUDEE                                                         
         SPACE 2                                                                
INCLUDEF LA    R5,NAME                                                          
         MVC   NAME,SPACES                                                      
         SPACE 2                                                                
INCLUDEG MVC   0(1,R5),0(R4)                                                    
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         CLI   0(R4),C' '                                                       
         BNE   INCLUDEG                                                         
         BAS   RE,PUTSORT                                                       
         B     INCLUDED                                                         
         EJECT                                                                  
*              ROUTINE TO HANDLE OTHER LINKEDIT STATEMENTS                      
         SPACE 3                                                                
OTHERLNK CLC   PAN(08),=C'*ACTION '                                             
         BE    STATEMNT                                                         
         CLC   PAN(07),=C'*ENTRY '                                              
         BE    STATEMNT                                                         
         CLC   PAN(08),=C'*LBLTYP '                                             
         BE    STATEMNT                                                         
         SPACE 2                                                                
         EJECT                                                                  
*              ROUTINE TO HANDLE TITLE CARDS                                    
         SPACE 3                                                                
TITLCARD LA    R5,PAN+1                                                         
         LA    R6,20                                                            
         SPACE 2                                                                
TC2      CLC   0(7,R5),=C' TITLE '                                              
         BE    TC4                                                              
         LA    R5,1(R5)                                                         
         BCT   R6,TC2                                                           
         B     DIRECTRY                                                         
         SPACE 2                                                                
TC4      LA    R5,8(R5)                                                         
         MVC   SAVETITL,SPACES                                                  
         LA    R6,SAVETITL                                                      
         LA    R7,50                                                            
         SPACE 2                                                                
TC6      CLI   0(R5),C''''                                                      
         BE    TC8                                                              
         MVC   0(1,R6),0(R5)                                                    
         LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
         BCT   R7,TC6                                                           
         SPACE 2                                                                
TC8      CLC   SAVEPHAS,SPACES                                                  
         BE    DIRECTRY            MUST HAVE FOUND PREVIOUS *PHASE              
         SPACE 2                                                                
TC10     MVC   RECORD,SPACES       RELEASE A CA RECORD                          
         MVI   RORC,C'C'                                                        
         MVC   NAME,SAVEPHAS                                                    
         MVI   AORB,C'A'                                                        
         MVC   PHASE(50),SAVETITL                                               
         BAS   RE,PUTSORT                                                       
         B     DIRECTRY                                                         
         SPACE 2                                                                
COMCARD  MVC   SAVETITL(50),PAN                                                 
         LA    R4,SAVETITL         CLEAN UP COMMENT                             
         LA    R5,50                                                            
         SPACE 2                                                                
CC1      CLI   0(R4),C''''                                                      
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         LA    R4,1(R4)                                                         
         BCT   R5,CC1                                                           
         CLC   SAVETITL,SPACES                                                  
         BE    STATEMNT                                                         
         SPACE 2                                                                
CC2      CLI   SAVETITL,C' '                                                    
         BNE   STATEMNT                                                         
         MVC   SAVETITL,SAVETITL+1                                              
         MVI   SAVETITL+49,C' '                                                 
         B     CC2                                                              
         EJECT                                                                  
*              ROUTINE TO DIG OUT PHASE NAME                                    
         SPACE 3                                                                
DIGPHASE NTR1                                                                   
         MVC   SAVEPHAS,SPACES                                                  
         LA    R5,PAN+7                                                         
         LA    R6,SAVEPHAS                                                      
         LA    R7,8                                                             
         SPACE 2                                                                
DP2      CLI   0(R5),C' '                                                       
         BE    XIT                                                              
         CLI   0(R5),C','                                                       
         BE    XIT                                                              
         MVC   0(1,R6),0(R5)                                                    
         LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
         BCT   R7,DP2                                                           
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO PUT TO SORTER                                         
         SPACE 3                                                                
PUTSORT  NTR1                                                                   
         GOTO1 =V(SORTER),PARA,=C'PUT',(R3)                                     
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL OUTPUT ROUTINES - CIL CROSS REFERENCE                    
         SPACE 3                                                                
OUTPUT   GOTO1 =V(SORTER),PARA,=C'GET'                                          
         L     R3,PARA+4                                                        
         LTR   R3,R3                                                            
         BZ    FINAL                                                            
         USING RCD,R3                                                           
         MVC   SVSORT,0(R3)                                                     
         CLI   RORC,C'C'                                                        
         BNE   RELO                                                             
         CLC   TITLE(4),=C'CORE'                                                
         BE    CL2                                                              
         ZAP   PAGE,=P'1'                                                       
         ZAP   LINE,=P'75'                                                      
         MVC   TITLE,SPACES                                                     
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   TITLE(34),=C'CORE-IMAGE LIBRARY CROSS-REFERENCE'                 
         MVC   SUB1(30),=C'PHASE NAME  LINK-EDIT   LEVEL '                      
         MVC   SUB2(30),=C'----------  BOOK NAME   NUMBER'                      
         MVC   SUB1+32(24),=C'DATE LAST  PROGRAM TITLE'                         
         MVC   SUB2+32(24),=C' LINKED    -------------'                         
         SPACE 2                                                                
CL2      CLI   AORB,C'A'                                                        
         BNE   OUTPUT                                                           
         MVC   P+43(50),PHASE                                                   
         B     OUTPUT                                                           
         SPACE 2                                                                
         EJECT                                                                  
*              RELOCATABLE REPORT                                               
         SPACE 3                                                                
RELO     CLC   TITLE(4),=C'RELO'                                                
         BE    RL2                                                              
         MVC   TITLE,SPACES                                                     
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   TITLE(35),=C'RELOCATABLE LIBRARY CROSS-REFERENCE'                
         MVC   SUB1(27),=C'----RELOCATABLE DETAILS----'                         
         MVC   SUB2(27),=C'NAME      LEVEL   DATE LAST'                         
         MVC   SUB3(27),=C'          NUMBER   CHANGED '                         
         MVC   SUB1+41(39),=C'--------------INCLUDED BY--------------'          
         MVC   SUB2+41(39),=C'PHASE     PAN NAME    LEVEL   DATE LAST'          
         MVC   SUB3+41(39),=C'NAME                  NUMBER   LINKED  '          
         MVC   P,SPACES                                                         
         ZAP   PAGE,=P'1'                                                       
         ZAP   LINE,=P'75'                                                      
         SPACE 2                                                                
RL2      CLC   NAME,LASTRL                                                      
         BE    RL4                                                              
         CLC   P+41(16),=C'(NOT REFERENCED)'                                    
         BNE   RL3                                                              
         GOTO1 =V(PRINTER)                                                      
         SPACE 2                                                                
RL3      GOTO1 =V(PRINTER)                                                      
         MVC   P(8),NAME                                                        
         MVC   P+11(16),=C'(NOT IN LIBRARY)'                                    
         CLC   NAME(2),=C'IJ'                                                   
         BNE   *+10                                                             
         MVC   P+11(16),=CL16'(IBM MODULE)'                                     
         MVC   P+41(16),=C'(NOT REFERENCED)'                                    
         MVC   LASTRL,NAME                                                      
         SPACE 2                                                                
RL4      CLI   AORB,C'A'                                                        
         BNE   RL6                                                              
         MVC   P+11(16),SPACES                                                  
         MVC   P+11(3),LEVEL                                                    
         CLC   DATE,SPACES           IF EMPTY, SKIP                             
         BE    OUTPUT                SKIP                                       
         GOTO1 =V(DATCON),PARA,(4,DATE),(8,P+18)                                
         B     OUTPUT                                                           
         SPACE 2                                                                
RL6      MVC   P+41(16),SPACES                                                  
         MVC   P+41(8),PHASE                                                    
         MVC   P+51(10),PANNAME                                                 
         MVC   P+64(3),LEVEL                                                    
         CLC   DATE,SPACES           IF EMPTY, SKIP                             
         BE    RL8                                                              
         GOTO1 =V(DATCON),PARA,(4,DATE),(8,P+71)                                
         SPACE 2                                                                
RL8      GOTO1 =V(PRINTER)                                                      
         B     OUTPUT                                                           
         EJECT                                                                  
*              WRAP-UP                                                          
         SPACE 3                                                                
FINAL    DS    0H                                                               
         GOTO1 =V(PRINTER)                                                      
         XBASE                                                                  
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 3                                                                
         EJECT                                                                  
*              ROUTINE TO PRINT A PAGE                                          
         SPACE 3                                                                
EXHAUST  NTR1                                                                   
         L     R2,=A(BUFFER)                                                    
         LA    R3,50                                                            
         SPACE 2                                                                
EXHAUST2 LR    R4,R2                                                            
         LA    R5,P                                                             
         LA    R6,10                                                            
         SPACE 2                                                                
EXHAUST4 MVC   0(10,R5),0(R4)                                                   
         MVC   0(10,R4),SPACES                                                  
         LA    R4,500(R4)                                                       
         LA    R5,11(R5)                                                        
         BCT   R6,EXHAUST4                                                      
         GOTO1 =V(PRINTER)                                                      
         LA    R2,10(R2)                                                        
         BCT   R3,EXHAUST2                                                      
         XIT1                                                                   
         EJECT                                                                  
*              WORK AREAS ETC                                                   
         SPACE 3                                                                
DUB      DS    D                                                                
PARA     DS    6F                                                               
WORK     DS    CL32                                                             
C        DS    CL80                                                             
SVSORT   DS    CL80                                                             
SAVEDIR  DS    CL80                                                             
SAVETITL DS    CL50                                                             
SAVEPHAS DS    CL8                                                              
SEQUENCE DC    PL4'0'                                                           
INBOOKS  DC    PL4'0'                                                           
REPTYP   DS    CL1                                                              
D        DS    CL60                                                             
TEST     DC    C'N'                                                             
COMMA    DC    C'N'                                                             
LASTRL   DC    CL8' '                                                           
PAN      DS    CL80                                                             
BOOK     DC    CL10'NO-ENTRY'                                                   
         DC    CL8' '                                                           
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=60'                                    
STRIP    DC    CL80' '                                                          
DUMPLIST DS    0F                                                               
         DC    A(LIBXREF)                                                       
         DC    V(DUMMY)                                                         
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER SORT RECORDS                                      
         SPACE 3                                                                
RCD      DSECT                                                                  
RECORD   DS    0CL60                                                            
RORC     DS    CL1                                                              
NAME     DS    CL8                                                              
AORB     DS    CL1                                                              
PHASE    DS    CL8                                                              
PANNAME  DS    CL10                                                             
LEVEL    DS    CL3                                                              
DATE     DS    CL8                                                              
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
BUFFER   CSECT                                                                  
         DC    5000C' '                                                         
         SPACE 3                                                                
SORTAREA CSECT                                                                  
         DS    56000C                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047DDLIBXRUS 05/01/02'                                      
         END                                                                    
