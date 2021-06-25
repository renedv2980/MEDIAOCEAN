*          DATA SET APGHFIBSI  AT LEVEL 075 AS OF 05/01/02                      
*PHASE ACHFIBSI,+0                                                              
         TITLE 'BATES CLIENT FILTER GROUPINGS'                                  
ACHFIBSI CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         USING R1RECD,R7                                                        
                                                                                
*---------------------------------------------------------------------*         
*                                                                     *         
*---------------------------------------------------------------------*         
HOOKSORT DS    0H                                                               
         CLI   HOOKNUM,1                                                        
         BNE   HK200                                                            
         AH    R7,=H'18'                                                        
                                                                                
         CLI   QOPT7,C'D'                                                       
         BNE   HK010                                                            
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AH    R0,=H'18'                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1B'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         AH    R7,=H'18'                                                        
                                                                                
HK010    CLC   QSELECT(2),=C'  '   IF BLANK THEN NO FILTERING REQUESTED         
         BE    HK110                                                            
                                                                                
         L     RF,=A(HOOKIO)                                                    
         ST    RF,AHOOKIO                                                       
         CLI   HOOKSW,C'Y'                                                      
         BE    HK050                                                            
                                                                                
         MVI   HOOKSW,C'Y'                                                      
                                                                                
         USING OFFRECD,R3                                                       
         L     R3,AHOOKIO                                                       
         MVC   0(42,R3),SPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,QCOMPANY                                                 
         MVC   OFFKOFF,QSELECT                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',AHOOKIO,AHOOKIO                  
         TM    DMCB,X'10'                                                       
         BO    HK110                                                            
                                                                                
         LA    R2,OFFTAB                                                        
         MVC   0(2,R2),QSELECT                                                  
         MVI   2(R2),X'FF'                                                      
                                                                                
         L     R3,AHOOKIO                                                       
         AH    R3,DATADISP                                                      
HK015    CLI   0(R3),0                                                          
         BE    HK050                                                            
         CLI   0(R3),OFLELQ                                                     
         BE    HK020                                                            
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     HK015                                                            
                                                                                
         USING OFLELD,R3                                                        
HK020    LA    R2,OFFTAB                                                        
         LA    R4,OFLNTRY                                                       
         ZIC   R5,OFLLN                                                         
         SH    R5,=H'3'                                                         
HK025    MVC   0(2,R2),0(R4)                                                    
         LA    R2,2(R2)                                                         
         LA    R4,2(R4)                                                         
         BCTR  R5,0                                                             
         BCT   R5,HK025                                                         
         MVI   0(R2),X'FF'                                                      
                                                                                
HK050    DS    0H                                                               
         BAS   RE,SETOFF                                                        
         CLC   THISOFF,=C'  '                                                   
         BE    HK110                                                            
                                                                                
         LA    R2,OFFTAB                                                        
HK052    CLI   0(R2),X'FF'                                                      
         BE    XITNO                                                            
         CLC   0(2,R2),THISOFF                                                  
         BE    HK110                                                            
         LA    R2,2(R2)                                                         
         B     HK052                                                            
                                                                                
                                                                                
HK110    DS    0H                                                               
         LA    R4,FLTRTAB                                                       
HK115    CLI   0(R4),X'FF'                                                      
         BE    HK120                                                            
         CLC   1(1,R4),R1CDE1                                                   
         BE    HK130                                                            
         LA    R4,FLTRLEN(R4)                                                   
         B     HK115                                                            
HK120    MVI   R1CDE1,C'0'                                                      
         B     HK199                                                            
HK130    MVC   R1CDE1(1),0(R4)                                                  
                                                                                
HK199    CLI   QOPT7,C'D'                                                       
         BNE   XIT                                                              
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AH    R0,=H'18'                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1A'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
                                                                                
                                                                                
HK200    CLI   HOOKNUM,2                                                        
         BNE   XIT                                                              
         CLI   QOPT7,C'D'                                                       
         BNE   HK205                                                            
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD2B'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
HK205    DS    0H                                                               
         LA    R4,GRPTAB                                                        
HK210    CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R4),R1CDE1                                                   
         BE    HK220                                                            
         LA    R4,GRPLEN(R4)                                                    
         B     HK210                                                            
HK220    DS    0H                                                               
         MVC   R1NME1,1(R4)                                                     
                                                                                
HK230    DS    0H                                                               
         LA    RE,REPTAB2                                                       
HK311    CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    HK340                                                            
         CLC   R1REPNO,0(RE)       MATCH ON REPORT NUMBER                       
         BE    HK315                                                            
         LA    RE,2(RE)                                                         
         B     HK311                                                            
                                                                                
HK315    DS    0H                                                               
         LA    RE,FLTNAME                                                       
         CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    HK340                                                            
HK318    CLC   0(1,RE),R1CDE2      MATCH ON REPORT NUMBER                       
         BE    HK320                                                            
         LA    RE,FLTNAML(RE)                                                   
         B     HK318                                                            
                                                                                
HK320    DS    0H                                                               
         MVC   R1NME2,1(RE)                                                     
                                                                                
HK340    CLI   QOPT7,C'D'                                                       
         BNE   XIT                                                              
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD2A'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
                                                                                
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        SET CONTRA OFFICE FOR COMPARISON                                       
*----------------------------------------------------------------*              
SETOFF   NTR1                                                                   
         MVC   THISOFF,CURRCON+4                                                
         CLC   CURRCON+1(2),=C'13'                                              
         BE    XIT                                                              
         CLC   CURRCON+1(2),=C'14'                                              
         BE    XIT                                                              
         CLC   CURRCON+1(2),=C'15'                                              
         BE    XIT                                                              
         MVC   THISOFF,CURRCON+3                                                
         CLC   CURRCON+1(2),=C'16'                                              
         BE    XIT                                                              
         MVC   THISOFF,=C'  '                                                   
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------*              
*        DEFINE CONSTANTS                                                       
*----------------------------------------------------------------*              
AHOOKIO  DS    A                                                                
HOOKSW   DC    C'N'                                                             
THISOFF  DC    CL2' '                                                           
OFFTAB   DC    255CL2' '                                                        
                                                                                
*        POSITION 1 - GROUP NUMBER FOR FILTER                                   
*        POSITION 2 - FILTER                                                    
FLTRTAB  DS    0C                                                               
         DC    CL1'0',C' '                                                      
FLTRLEN  EQU   *-FLTRTAB                                                        
         DC    CL1'1',C'A'                                                      
         DC    CL1'1',C'B'                                                      
         DC    CL1'1',C'C'                                                      
         DC    CL1'1',C'D'                                                      
         DC    CL1'1',C'E'                                                      
         DC    CL1'1',C'F'                                                      
         DC    CL1'1',C'G'                                                      
         DC    CL1'1',C'V'                                                      
         DC    CL1'1',C'X'                                                      
*                                                                               
         DC    CL1'2',C'H'                                                      
         DC    CL1'2',C'I'                                                      
         DC    CL1'2',C'J'                                                      
         DC    CL1'2',C'U'                                                      
         DC    CL1'2',C'W'                                                      
*                                                                               
         DC    CL1'3',C'K'                                                      
         DC    CL1'3',C'L'                                                      
         DC    CL1'3',C'M'                                                      
         DC    CL1'3',C'N'                                                      
         DC    CL1'3',C'O'                                                      
*                                                                               
         DC    CL1'4',C'P'                                                      
         DC    CL1'4',C'Q'                                                      
         DC    CL1'4',C'R'                                                      
         DC    CL1'4',C'S'                                                      
         DC    CL1'4',C'T'                                                      
         DC    X'FF'                                                            
                                                                                
GRPTAB   DS    0C                                                               
         DC    CL1'0',CL36'NON-FILTERED'                                        
GRPLEN   EQU   *-GRPTAB                                                         
         DC    CL1'1',CL36'NEW YORK CLIENTS'                                    
         DC    CL1'2',CL36'SPECIALTY UNITS'                                     
         DC    CL1'3',CL36'NON NEW YORK'                                        
         DC    CL1'4',CL36'OTHER'                                               
         DC    X'FF'                                                            
                                                                                
REPTAB2  DS    0F                                                               
         DC    AL1(1,R1ROW2-R1RECD)                                             
         DC    AL1(2,R1ROW2-R1RECD)                                             
         DC    AL1(3,R1ROW2-R1RECD)                                             
*                                                                               
         DC    AL1(4,R1ROW2-R1RECD)                                             
         DC    AL1(5,R1ROW2-R1RECD)                                             
         DC    AL1(6,R1ROW2-R1RECD)                                             
*                                                                               
         DC    AL1(7,R1ROW2-R1RECD)                                             
         DC    AL1(8,R1ROW2-R1RECD)                                             
         DC    AL1(9,R1ROW2-R1RECD)                                             
*                                                                               
         DC    AL1(10,R1ROW2-R1RECD)                                            
         DC    AL1(11,R1ROW2-R1RECD)                                            
         DC    AL1(12,R1ROW2-R1RECD)                                            
         DC    X'FF'                                                            
*                                                                               
                                                                                
FLTNAME  DS    0C                                                               
         DC    CL1' ',CL36'NON-FILTERED'                                        
FLTNAML  EQU   *-FLTNAME                                                        
         DC    CL1'A',CL36'RANDY HACKETT'                                       
         DC    CL1'B',CL36'JANET STANTON'                                       
         DC    CL1'C',CL36'GARY STEELE'                                         
         DC    CL1'D',CL36'LIDA BURPEE(DEVINO)'                                 
         DC    CL1'E',CL36'JOHN CHARLETON'                                      
         DC    CL1'F',CL36'D. KHOSROVANI-H.K.'                                  
         DC    CL1'G',CL36'MICHAEL DUDYNSKAY'                                   
         DC    CL1'H',CL36'JOHN MARCHESE - DIRECT'                              
         DC    CL1'I',CL36'JOHN MARCHESE - 141'                                 
         DC    CL1'J',CL36'STUDIO'                                              
         DC    CL1'K',CL36'BILL ISENBERGER'                                     
         DC    CL1'L',CL36'TIMOTHY HART'                                        
         DC    CL1'M',CL36'JERRY KERR'                                          
         DC    CL1'N',CL36'GARY BASSELL'                                        
         DC    CL1'O',CL36'DAN ROMAN'                                           
         DC    CL1'P',CL36'PERRI PARLINI - TERMINATED CLIENTS'                  
         DC    CL1'Q',CL36'PERRI PARLINI - MISC. CLIENTS'                       
         DC    CL1'R',CL36'JOHN MARCHESE'                                       
         DC    CL1'S',CL36'MISC. UNDER/OVER ABSORBED'                           
         DC    CL1'T',CL36'BERNIE DOLAN'                                        
         DC    CL1'U',CL36'JOHN MARCHESE - INTERACTIVE'                         
         DC    CL1'V',CL36'RICH NEWMAN'                                         
         DC    CL1'W',CL36'MARILYN SILVERMAN'                                   
         DC    CL1'X',CL36'CHRIS CLARK'                                         
         DC    X'FF'                                                            
         LTORG                                                                  
                                                                                
         EJECT                                                                  
*----------------------------------------------------------------*              
*        WORKING STORAGE                                                        
*----------------------------------------------------------------*              
R1RECD   DSECT                                                                  
R1ROW1   DS    XL2                                                              
R1CDE1   DS    XL14                                                             
R1ROW2   DS    XL2                                                              
R1CDE2   DS    XL14                                                             
R1ROW3   DS    XL2                                                              
R1CDE3   DS    XL14                                                             
R1ROW4   DS    XL2                                                              
R1CDE4   DS    XL14                                                             
R1ROW5   DS    XL2                                                              
R1CDE5   DS    XL14                                                             
R1ROW6   DS    XL2                                                              
R1CDE6   DS    XL14                                                             
R1REPNO  DS    XL1                                                              
R1REPCP  DS    XL1                                                              
R1TYPE   DS    XL2                                                              
*                                                                               
R1NME1   DS    CL36                                                             
R1NME2   DS    CL36                                                             
R1NME3   DS    CL36                                                             
R1NME4   DS    CL36                                                             
R1NME5   DS    CL36                                                             
R1NME6   DS    CL36                                                             
*                                                                               
R1COL1   DS    PL8                                                              
R1COL2   DS    PL8                                                              
R1COL3   DS    PL8                                                              
R1COL4   DS    PL8                                                              
R1COL5   DS    PL8                                                              
R1COL6   DS    PL8                                                              
R1LEN    EQU   *-R1RECD                                                         
                                                                                
         CSECT                                                                  
         DS    0F                                                               
HOOKIO   DS    CL2000                                                           
         EJECT                                                                  
                                                                                
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075APGHFIBSI 05/01/02'                                      
         END                                                                    
