*          DATA SET CU1099     AT LEVEL 087 AS OF 05/01/02                      
*PHASE CU1099,*                                                                 
*INCLUDE SPRINT                                                                 
*INCLUDE ACSAVE                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE SQUASHER                                                               
         TITLE 'CU1099 - DDEFCU 1099 INT FORMS AND TAPE'                        
CU1099   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*SK106*,=V(ACSAVE)                                             
         LA    R9,A1099                                                         
         USING D1099T,R9                                                        
         SPACE 3                                                                
         OPEN  (MASTIN,(INPUT))                                                 
         GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   =C'FORMS',C+2                                                    
         BE    NOTP                                                             
         LOAD  EP=CU1099TP                                                      
         ST    R0,T99TP                                                         
         MVC   D1SUR,=C'$OPE'                                                   
         MVC   D1YEAR,C                                                         
         GOTO1 T99TP,DMCB,A1099                                                 
         B     GETMAS                                                           
NOTP     BAS   RE,RELEASE                                                       
         BAS   RE,RELEASE                                                       
         BAS   RE,RELEASE                                                       
         SPACE 3                                                                
GETMAS   GET   MASTIN                                                           
         LR    R2,R1                                                            
         SPACE 1                                                                
         OC    0(80,R2),=80X'40'                                                
         SPACE 1                                                                
         CLI   0(R2),C'N'                                                       
         BE    GM2                                                              
         ABEND 1,DUMP                                                           
GM2      MVC   D1NAME(80),SPACES                                                
         MVC   D1ACCT,1(R2)                                                     
         MVC   D1NAME(19),31(R2)                                                
         MVC   D1NAME+20(1),50(R2)                                              
         MVC   D1NAME+22(19),11(R2)                                             
         GOTO1 =V(SQUASHER),DMCB,D1NAME,40                                      
         MVC   D1NAME2(19),51(R2)                                               
         MVC   D1SUR(19),11(R2)                                                 
         SPACE 1                                                                
         GET   MASTIN                                                           
         LR    R2,R1                                                            
         OC    0(80,R2),=80X'40'                                                
         CLI   0(R2),C'A'                                                       
         BE    GM3                                                              
         ABEND 2                                                                
GM3      MVC   D1ADDR(80),SPACES                                                
         MVC   D1ADDR(28),11(R2)                                                
         MVC   D1CSZ(21),49(R2)                                                 
         MVC   D1CSZ+22(2),71(R2)                                               
         MVC   D1CSZ+25(5),73(R2)                                               
         SPACE 1                                                                
         GET   MASTIN                                                           
         LR    R2,R1                                                            
         OC    0(80,R2),=80X'40'                                                
         CLI   0(R2),C'M'                                                       
         BE    GM4                                                              
         ABEND 3                                                                
GM4      PACK  D1GROSS,29(10,R2)                                                
         PACK  D1TAX,49(10,R2)                                                  
         MVN   D1GROSS+5(1),=X'0F'                                              
         MVN   D1TAX+5(1),=X'0F'                                                
         MVC   D1SS,11(R2)                                                      
         SPACE 1                                                                
GO       CLC   =C'ZERO',C+6                                                     
         BE    GO2                                                              
         CP    D1GROSS,=P'0'                                                    
         BNE   GO2                                                              
         CP    D1TAX,=P'0'                                                      
         BE    GETMAS                                                           
GO2      CLI   C+2,C'T'                                                         
         BE    NOGO1                                                            
         MVC   DNAME,D1NAME                                                     
         MVC   DNAME2,D1NAME2                                                   
         MVC   DADDR1,D1ADDR                                                    
         MVC   DADDR2,D1CSZ                                                     
         MVC   DADDR3,SPACES                                                    
         MVC   DSSNO,D1SS                                                       
         ZAP   DDIV,D1GROSS                                                     
         ZAP   DTAX,D1TAX                                                       
         PERF  RELEASE                                                          
         B     GETMAS                                                           
NOGO1    PERF  TAPERTN                                                          
         B     GETMAS                                                           
         EJECT                                                                  
*        PRINT SETUP..............                                              
*                                                                               
* SETUP FOR FORM 1099-INT PRINTS IN BOX 1 (GROSS DIV)                           
*                                                                               
RELEASE  NTR1                                                                   
         MVC   P+8(33),=C'DONOVAN DATA EMPLOYEES FEDERAL CU'                    
         PERF  PRT                                                              
         PERF  PRT                                                              
         MVC   P+8(16),=C'666 FIFTH AVENUE'                                     
         PERF  PRT,T=2                                                          
         MVC   P+8(18),=C'NEW YORK, NY 10103'                                   
         PERF  PRT,T=2                                                          
         MVC   P+10(10),=C'13-1019113'                                          
         MVC   P+30(4),=C'-  -'                                                 
         MVC   P+27(3),DSSNO                                                    
         MVC   P+31(2),DSSNO+3                                                  
         MVC   P+34(4),DSSNO+5                                                  
         MVZ   P+37(1),=X'F0'                                                   
         EDIT  (P5,DDIV),(9,P+56),2                                             
         PERF  PRT,T=2                                                          
         MVC   P+8(30),DNAME                                                    
         PERF  PRT,                                                             
         MVC   P+8(30),DNAME2                                                   
         PERF  PRT,T=2                                                          
         MVC   P+8(30),DADDR1                                                   
         PERF  PRT                                                              
         MVC   P+8(30),DADDR2                                                   
         GOTO1 =V(SQUASHER),DMCB,P+8,30                                         
         PERF  PRT                                                              
         MVC   P+8(30),DADDR3                                                   
         PERF  PRT,T=3                                                          
         PERF  PRT,T=3                                                          
         PERF  PRT,T=3                                                          
         AP    TOTDIV,DDIV                                                      
         AP    TOTTAX,DTAX                                                      
         AP    TEN99S,=P'1'                                                     
         B     XIT                                                              
         EJECT                                                                  
*        END OF JOB WRAPUP...........                                           
         SPACE 1                                                                
EOJOB    MVC   DNAME(6),=C'*TOTAL'                                              
         MVC   DNAME+6(24),DNAME                                                
         MVC   DNAME2,DNAME                                                     
         MVC   DADDR1,DNAME2                                                    
         MVC   DADDR2(90),DNAME                                                 
         MVC   DSSNO,SPACES                                                     
         ZAP   DDIV,TOTDIV                                                      
         ZAP   DTAX,TOTTAX                                                      
         CLI   C+2,C'T'                                                         
         BE    CLSIT                                                            
         BAS   RE,RELEASE                                                       
         MVC   P+8(29),=C'* TOTAL NO. OF FORMS PRINTED-'                        
         EDIT  (P3,TEN99S),(6,P+38),0,COMMAS=YES                                
         BAS   RE,PRT                                                           
CLSIT    CLOSE (MASTIN,)                                                        
         CLI   C+2,C'T'                                                         
         BE    XZ                                                               
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
         B     XB                                                               
XZ       MVC   D1SUR,=C'$CLO'                                                   
         GOTO1 T99TP,DMCB,A1099                                                 
XB       XBASE                                                                  
         SPACE 3                                                                
PRT      NTR1                                                                   
         GOTO1 =V(PRINT),DMCB,P,PCTL                                            
         MVC   P,SPACES                                                         
         MVC   PCTL,=C'BL01'                                                    
         B     XIT                                                              
         SPACE 2                                                                
TAPERTN  NTR1                                                                   
         MVI   D1TYPE,C'2'                                                      
         MVC   D1YEAR,C                                                         
         GOTO1 T99TP,DMCB,A1099                                                 
XIT      XIT1                                                                   
         EJECT                                                                  
*        CONSTANTS, CSECTS, DSECTS............                                  
         SPACE 1                                                                
TOTDIV   DC    PL5'0'                                                           
TOTTAX   DC    PL5'0'                                                           
TEN99S   DC    PL3'0'                                                           
PCTL     DC    C'BL01'                                                          
ENDFL    DC    C'F'                                                             
DMCB     DS    6F                                                               
WORK     DS    CL20                                                             
DUB      DS    D                                                                
P        DS    CL132                                                            
SPACES   DC    CL132' '                                                         
WKACCUM  DC    PL5'0'                                                           
         SPACE 1                                                                
A1099    DS    294C                                                             
T99TP    DS    A                                                                
D1099    DS    0CL199                                                           
DNAME    DC    CL30'LINEUP************************'                             
DNAME2   DC    CL30'LINEUP************************'                             
DADDR1   DC    CL30'LINEUP************************'                             
DADDR2   DC    CL30'******************************'                             
DADDR3   DC    CL30'******************************'                             
DADDR4   DC    CL30'******************************'                             
DSSNO    DC    CL9'999999999'                                                   
DDIV     DC    PL5'0'                                                           
DTAX     DC    PL5'0'                                                           
DSUR     DC    CL15' '                                                          
C        DS    CL80                                                             
         SPACE 1                                                                
MASTIN   DCB   DDNAME=CU1099IN,DSORG=PS,RECFM=FB,MACRF=GL,             X        
               EODAD=EOJOB,LRECL=80                                             
         SPACE 1                                                                
SREC     DS    0CL365                                                           
SRECSUR  DS    CL15                                                             
SRECREC  DS    350C                                                             
         LTORG                                                                  
         EJECT                                                                  
D1099T   DSECT                FOR 1099 TAPE INTERFACE AREA                      
D1YEAR   DS    CL2                                                              
D1SUR    DS    CL4                                                              
D1TYPE   DS    CL1                                                              
D1SS     DS    CL9                                                              
D1GROSS  DS    PL6                                                              
D1TAX    DS    PL6                                                              
D1EX     DS    PL6                                                              
D1NAME   DS    CL40                                                             
D1NAME2  DS    CL40                                                             
D1ADDR   DS    CL40                                                             
D1CSZ    DS    CL40                                                             
D1ADDR3  DS    CL40                                                             
D1ADDR4  DS    CL40                                                             
D1ACCT   DS    CL10                                                             
         DS    CL10                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'087CU1099    05/01/02'                                      
         END                                                                    
