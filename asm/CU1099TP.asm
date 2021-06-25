*          DATA SET CU1099TP   AT LEVEL 044 AS OF 11/18/86                      
*PHASE CU1099TP,*                                                               
*INCLUDE ADSCAN                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
CU1099TP CSECT                                                                  
         TITLE 'CU1099TP - DDEFCU DIVIDEND 1099 TAPE'                           
         PRINT NOGEN                                                            
         NMOD1 0,**SK206***                                                     
         L     R9,0(R1)                                                         
         USING D1099T,R9                                                        
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         SPACE 3                                                                
         CLC   D1SUR,=C'$OPE'                                                   
         BE    OPEN                                                             
         CLC   D1SUR,=C'$CLO'                                                   
         BE    CLOSIT                                                           
         B     NORM                                                             
OPEN     CLI   D1YEAR,C'D'                                                      
         BE    NOOPEN                                                           
         OPEN  (CUT99TP,(OUTPUT))                                               
NOOPEN   PERF  CAREC                                                            
         PERF  SETHD                                                            
         CLI   D1YEAR,C'A'                                                      
         BNE   *+8                                                              
         PERF  PAREC                                                            
         LA    R5,AREC                                                          
         CLI   D1YEAR,C'D'                                                      
         BNH   XMOD                                                             
         PERF  WTREC                                                            
         B     XMOD                                                             
         SPACE 3                                                                
NORM     PERF  CBREC                                                            
         PERF  PRBREC                                                           
         LA    R5,BREC                                                          
         CLI   D1YEAR,C'D'                                                      
         BNH   *+8                                                              
         PERF  WTREC                                                            
         B     XMOD                                                             
         SPACE 3                                                                
CLOSIT   PERF  CCREC                                                            
         PERF  PCREC                                                            
         LA    R5,CREC                                                          
         CLI   D1YEAR,C'D'                                                      
         BNH   NODC                                                             
         PERF  WTREC                                                            
NODC     PERF  PFREC                                                            
         CLI   D1YEAR,C'D'                                                      
         BNH   XMOD                                                             
         CLOSE CUT99TP                                                          
XMOD     XMOD1                                                                  
         EJECT                                                                  
CAREC    NTR1                                                                   
         MVC   AYEAR,D1YEAR+1                                                   
         B     XIT                                                              
         SPACE 3                                                                
PAREC    NTR1                                                                   
         MVC   P(100),AREC                                                      
         PERF  PRT                                                              
         MVC   P(100),AREC+100                                                  
         PERF  PRT                                                              
         MVC   P(100),AREC+200                                                  
         PERF  PRT                                                              
         MVC   P(60),AREC+300                                                   
         PERF  PRT                                                              
         B     XIT                                                              
PBREC    NTR1                                                                   
         MVC   P(100),BREC                                                      
         PERF  PRT                                                              
         MVC   P(100),BREC+100                                                  
         PERF  PRT                                                              
         MVC   P(100),BREC+200                                                  
         PERF  PRT                                                              
         MVC   P(60),BREC+300                                                   
         PERF  PRT                                                              
         B     XIT                                                              
         EJECT                                                                  
CBREC    NTR1                                                                   
         MVC   BYEAR,D1YEAR                                                     
         MVC   BNMCTL,D1SUR                                                     
         MVC   BTINTYPE,D1TYPE                                                  
         MVC   BSSNO,D1SS                                                       
         UNPK  BGROSS,D1GROSS                                                   
         UNPK  BTAX,D1TAX                                                       
         MVC   BNAME(80),D1NAME                                                 
         MVC   BACCT,D1ACCT                                                     
         LA    R2,D1ADDR                                                        
         LA    R4,4                                                             
LAEX     LA    R1,EXTBL                                                         
NXT      CLI   0(R1),X'FF'                                                      
         BE    KEEP                                                             
         ZIC   R3,0(R1)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),1(R1)                                                    
         BE    BLK                                                              
         AR    R1,R3                                                            
         LA    R1,2(R1)                                                         
         B     NXT                                                              
BLK      MVC   0(40,R2),=40C' '                                                 
KEEP     LA    R2,40(R2)                                                        
         BCT   R4,LAEX                                                          
         LA    R2,D1ADDR                                                        
         LA    R3,BADDR                                                         
         LA    R1,4                                                             
         LA    R4,2                                                             
KP1      CLC   0(40,R2),=40C' '                                                 
         BE    UP2                                                              
         B     TKAD                                                             
UP2      LA    R2,40(R2)                                                        
         BCT   R1,KP1                                                           
         B     DONADDR                                                          
TKAD     MVC   0(40,R3),0(R2)                                                   
         LA    R3,40(R3)                                                        
         BCT   R4,UP2                                                           
DONADDR  LA    R3,BCSZ                                                          
         MVC   SVST,BCSZ+22                                                     
         GOTO1 =V(ADSCAN),DMCB,(40,(R3)),(20,SCANCY),SCANST,(5,SCANZIP)         
         CLI   DMCB+3,1                                                         
         BE    REDO                                                             
         CLI   DMCB+3,2            STATE CODE CHANGED                           
         BNE   *+12                                                             
         PERF  MORESCAN                                                         
         B     TONZT                                                            
         CLI   DMCB+3,3            BAD OR NO ZIP CODE                           
         BNE   YSZP                                                             
         MVC   SCANST,SVST         REPLACE STATE                                
         B     TONZT                                                            
YSZP     MVC   BCSZ(20),SCANCY                                                  
TONZT    MVC   BCSZ+20(19),SPACES                                               
         MVC   BCSZ+29(2),SCANST                                                
         MVC   BCSZ+31(5),SCANZIP                                               
         MVI   NOZIP2,C'0'                                                      
PKCBC    PACK  WORK(5),CBCOUNT                                                  
         AP    WORK(5),=P'1'                                                    
         UNPK  CBCOUNT,WORK(5)                                                  
         CLI   D1YEAR,C'A'                                                      
         BNE   XIT                                                              
         PERF  PBREC                                                            
         B     XIT                                                              
         SPACE 1                                                                
MORESCAN NTR1                                                                   
         LA    R1,SCANCY                                                        
         LA    R2,0                                                             
MS1      CLI   0(R1),C','                                                       
         BE    MS3                                                              
         CLC   0(2,R1),=C'  '                                                   
         BE    MS3                                                              
MS2      LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         B     MS1                                                              
MS3      MVC   BCSZ(30),SPACES                                                  
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   BCSZ(0),SCANCY                                                   
         LA    R3,BCSZ+21                                                       
         MVC   0(2,R3),SCANST                                                   
         MVC   4(5,R3),SCANZIP                                                  
         B     XIT                                                              
         SPACE 1                                                                
REDO     MVC   BCSZ(30),=C'**** PLEASE REFORMAT ADDR ****'                      
         B     PKCBC                                                            
         SPACE 3                                                                
PRBREC   NTR1                                                                   
         MVC   P(10),BACCT                                                      
         MVC   P+12(30),BNAME                                                   
         MVC   P+43(30),BADDR                                                   
         MVC   P2(3),BSSNO                                                      
         MVI   P2+3,C'-'                                                        
         MVC   P2+4(2),BSSNO+3                                                  
         MVI   P2+6,C'-'                                                        
         MVZ   BSSNO+8(1),=X'F0'                                                
         MVC   P2+7(4),BSSNO+5                                                  
         MVC   P2+12(36),BNAME2                                                 
         MVC   P2+43(40),BCSZ                                                   
         LA    R1,2                                                             
         LA    R4,BTOTALS                                                       
         LA    R3,BPAYAMTS                                                      
         LA    R2,P+81                                                          
PKEDIT   PACK  WORK(5),0(10,R3)                                                 
         AP    0(5,R4),WORK(5)                                                  
         EDIT  (P5,WORK),(10,(R2)),2,COMMAS=NO                                  
         LA    R2,15(R2)                                                        
         LA    R3,10(R3)                                                        
         LA    R4,5(R4)                                                         
         BCT   R1,PKEDIT                                                        
         PERF  PRT,T=2                                                          
         B     XIT                                                              
         EJECT                                                                  
CCREC    NTR1                                                                   
         LA    R3,CBTOTS                                                        
         LA    R2,BTOTALS                                                       
         LA    R4,9                                                             
UNPKC    UNPK  0(15,R3),0(5,R2)                                                 
         LA    R2,5(R2)                                                         
         LA    R3,15(R3)                                                        
         BCT   R4,UNPKC                                                         
         B     XIT                                                              
         SPACE 2                                                                
PCREC    NTR1                                                                   
         MVC   P(12),=C'GRAND TOTALS'                                           
         LA    R4,BTOTALS                                                       
         LA    R2,P+81                                                          
         LA    R1,2                                                             
EDTOT    EDIT  (P5,(R4)),(10,(R2)),2,COMMAS=NO                                  
         LA    R4,5(R4)                                                         
         LA    R2,15(R2)                                                        
         BCT   R1,EDTOT                                                         
         PERF  PRT,T=2                                                          
         MVC   P(21),=C'NO. OF PAYEE RECORDS '                                  
         PACK  WORK(6),CBCOUNT                                                  
         EDIT  (P6,WORK),(4,P+22),0                                             
         PERF  PRT,T=2                                                          
         CLI   D1YEAR,C'A'                                                      
         BNE   XIT                                                              
         MVC   P(100),CREC                                                      
         PERF  PRT                                                              
         B     XIT                                                              
PFREC    NTR1                                                                   
         LA    R5,FREC                                                          
         CLI   D1YEAR,C'D'                                                      
         BNH   *+8                                                              
         PERF  WTREC                                                            
         CLI   D1YEAR,C'A'                                                      
         BNE   XIT                                                              
         MVC   P(100),FREC                                                      
         PERF  PRT                                                              
         B     XIT                                                              
         EJECT                                                                  
PRT      NTR1                                                                   
         GOTO1 =V(PRINTER)                                                      
         CLC   P2(20),=40C' '                                                   
         BE    CLR                                                              
         MVC   P(132),P2                                                        
         GOTO1 =V(PRINTER)                                                      
CLR      MVI   P,C' '                                                           
         MVC   P+1(131),P                                                       
         MVC   P2,P                                                             
         B     XIT                                                              
         SPACE 3                                                                
SETHD    NTR1                                                                   
         MVC   MID1(10),=C' ACCOUNT  '                                          
         MVC   MID1+21(5),=C'NAME1'                                             
         MVC   MID1+52(7),=C'ADDRESS'                                           
         MVC   MID2+2(5),=C'ID NO'                                              
         MVC   MID2+21(5),=C'NAME2'                                             
         MVC   MID2+52(11),=C'CITY/ST/ZIP'                                      
         MVC   MID1+83(8),=C'AMOUNT 1'                                          
         MVC   MID2+85(5),=C'GROSS'                                             
         MVC   MID1+99(8),=C'AMOUNT 3'                                          
         MVC   MID2+102(3),=C'FWT'                                              
         MVI   MID3,C'-'                                                        
         MVC   MID3+1(109),MID3                                                 
         MVC   TITLE(32),=C'DDEFCU 1099 INT TAPE REGISTER   '                   
         B     XIT                                                              
         SPACE 3                                                                
WTREC    NTR1                                                                   
         PUT   CUT99TP,(5)                                                      
         B     XIT                                                              
         SPACE 3                                                                
         ANSR                                                                   
SCANCY   DS    CL20                                                             
         DC    CL1' '                                                           
SCANST   DS    CL2                                                              
         DC    CL2' '                                                           
SCANZIP  DS    CL5                                                              
P2       DC    CL132' '                                                         
NOZIP2   DC    C'0'                                                             
BTOTALS  DC    9PL5'0'                                                          
DMCB     DS    6F                                                               
WORK     DS    CL20                                                             
DUB      DS    D                                                                
SVST     DS    CL2                                                              
         EJECT                                                                  
EXTBL    DC    AL1(3),C'C/O'                                                    
         DC    AL1(3),C'C/0'                                                    
         DC    AL1(3),C'APT'                                                    
         DC    AL1(5),C'TOWER'                                                  
         DC    AL1(5),C'PLAZA'                                                  
         DC    AL1(2),C'UA'                                                     
         DC    AL1(10),C'LIVINGSTON'                                            
         DC    AL1(7),C'VILLAGE'                                                
         DC    AL1(7),C'SNEDENS'                                                
         DC    AL1(10),C'THE GREENW'                                            
         DC    AL1(5),C'SUITE'                                                  
         DC    AL1(5),C'COGAN'                                                  
         DC    AL1(6),C'MR ANT'                                                 
         DC    X'FF'                                                            
         SPACE 3                                                                
CUT99TP  DCB   DSORG=PS,MACRF=PM,DDNAME=CUT99TP,RECFM=F,LRECL=360,     X        
               BLKSIZE=360                                                      
         EJECT                                                                  
*  R E C O R D S                                                                
         SPACE 1                                                                
AREC     DS    0CL360                                                           
         DC    C'A'                                                             
AYEAR    DS    CL1                                                              
         DC    CL3'001'                                                         
         DC    CL9'133019113'      CU EIN NUMBER                                
         DC    CL2' '                                                           
         DC    CL1'6'                                                           
         DC    CL9'13          '                                                
         DC    CL1' '                                                           
         DC    CL3'360'                                                         
         DC    CL3'360'                                                         
         DC    CL1' '                                                           
         DC    CL5'19816'          TRANSMITTER CODE                             
         DC    C' '                                                             
         DC    CL40'DONOVAN DATA EMPLOYEES FEDERAL C. U.'                       
         DC    CL39' '                                                          
         DC    C'0'                                                             
         DC    CL40'666 FIFTH AVENUE'                                           
         DC    CL40'NEW YORK, NY 10103'                                         
         DC    CL80'DONOVAN DATA EMPLOYEES FEDERAL C. U. '                      
         DC    CL40'666 FIFTH AVENUE'                                           
         DC    CL40'NEW YORK, NY 10103'                                         
         SPACE 3                                                                
BREC     DS    0CL360                                                           
         DC    C'B'                                                             
BYEAR    DS    CL2                                                              
         DC    CL3' '                                                           
BNMCTL   DS    CL4                                                              
BTINTYPE DS    CL1                                                              
BSSNO    DS    CL9                                                              
BACCT    DS    CL10                                                             
BPAYAMTS DS    0CL30                                                            
BGROSS   DS    CL10                                                             
BTAX     DS    CL10                                                             
BNAME    DS    CL40                                                             
BNAME2   DS    CL40                                                             
BADDR    DS    CL40                                                             
BCSZ     DS    CL40                                                             
         DC    CL148' '                                                         
         DC    CL2' '                                                           
         SPACE 3                                                                
CREC     DS    0CL360                                                           
         DC    C'C'                                                             
CBCOUNT  DC    CL6'000000'                                                      
CBTOTS   DC    9CL15'000000000000000'                                           
         DC    CL218' '                                                         
         SPACE 3                                                                
FREC     DS    0CL360                                                           
         DC    C'F0001001',22C'0',330C' '                                       
         EJECT                                                                  
*    D S E C T S                                                                
         SPACE 1                                                                
D1099T   DSECT               TO COVER DATA FROM CU1099                          
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
         SPACE 3                                                                
*INCLOOD DDDPRINT                                                               
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044CU1099TP  11/18/86'                                      
         END                                                                    
