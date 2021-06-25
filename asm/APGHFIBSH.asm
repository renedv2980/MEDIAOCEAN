*          DATA SET APGHFIBSH  AT LEVEL 009 AS OF 09/27/01                      
*PHASE ACHFBSHA                                                                 
         TITLE 'BATES CLIENT FILTER GROUPINGS'                                  
ACHFBSH  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
                                                                                
         USING MAND,RA                                                          
         L     RA,0(,R1)                                                        
                                                                                
         USING ACWORKD,RC                                                       
         L     RC,HOOKAWRK                                                      
                                                                                
         USING R1RECD,R7                                                        
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
                                                                                
*---------------------------------------------------------------------*         
*                                                                     *         
*---------------------------------------------------------------------*         
HOOKSORT DS    0H                                                               
         CLI   HOOKNUM,1                                                        
         BNE   HK200                                                            
         AHI   R7,18                                                            
                                                                                
         CLI   QOPT6,C'1'                                                       
         BNE   HK005                                                            
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AHI   R0,18                                                            
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1B'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         AHI   R7,18                                                            
                                                                                
HK005    LA    R4,FLTRTAB                                                       
HK015    CLI   0(R4),X'FF'                                                      
         BE    HK030                                                            
         CLC   0(1,R4),R1CDE1                                                   
         BE    HK020                                                            
         LA    R4,FLTRLEN(,R4)                                                  
         B     HK015                                                            
                                                                                
HK020    MVC   R1CDE1(1),1(R4)                                                  
         MVC   R1CDE2(1),2(R4)                                                  
                                                                                
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         CLI   0(R7),3                                                          
         BE    HK040                                                            
         AHI   R7,18                                                            
         MVC   R1CDE3(1),3(R4)                                                  
         B     HK040                                                            
                                                                                
HK030    MVI   R1CDE1,C'0'                                                      
         MVI   R1CDE2,C'A'                                                      
                                                                                
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         CLI   0(R7),3                                                          
         BE    HK040                                                            
         CLI   0(R7),6                                                          
         BE    HK040                                                            
         AHI   R7,18                                                            
         MVI   R1CDE3,C'A'                                                      
                                                                                
                                                                                
HK040    DS    0H                                                               
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         CLI   0(R7),3                                                          
         BE    HK045                                                            
         CLI   0(R7),6                                                          
         BNE   HK050                                                            
                                                                                
HK045    AHI   R7,18                                                            
         CLI   R1CDE1,C'2'                                                      
         BNE   XITNO                                                            
         CLI   R1CDE2,C'B'                                                      
         BE    HK060                                                            
         CLI   R1CDE2,C'C'                                                      
         BNE   XITNO                                                            
         B     HK060                                                            
                                                                                
HK050    DS    0H                                                               
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         CLI   0(R7),2                                                          
         BE    HK055                                                            
         CLI   0(R7),5                                                          
         BNE   HK060                                                            
                                                                                
HK055    AHI   R7,18                                                            
         CLI   R1CDE1,C'2'                                                      
         BNE   XITNO                                                            
         CLI   R1CDE2,C'C'                                                      
         BNE   XITNO                                                            
         B     HK060                                                            
*MN                                                                             
HK060    DS    0H                                                               
         CLC   QSELECT(4),SPACES                                                
         BE    HK090                                                            
         L     RF,=A(HOOKIO)                                                    
         ST    RF,AHOOKIO                                                       
         TM    HOOKSW,CLILIST                                                   
         BO    HK087                                                            
         OI    HOOKSW,CLILIST                                                   
                                                                                
         LA    R2,CLITAB                                                        
         MVI   0(R2),X'FF'                                                      
                                                                                
         USING LSTRECD,R3                                                       
         L     R3,AHOOKIO                                                       
         MVC   0(42,R3),SPACES                                                  
         MVI   LSTKTYP,LSTKTYPQ                                                 
         MVC   LSTKCPY,QCOMPANY                                                 
         MVC   LSTKLST(4),QSELECT                                               
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',AHOOKIO,AHOOKIO                  
         TM    DMCB+8,X'10'                                                     
         BO    HK087                                                            
                                                                                
         L     R3,AHOOKIO                                                       
         AH    R3,DATADISP                                                      
HK065    CLI   0(R3),0                                                          
         BE    HK087                                                            
         CLI   0(R3),LIDELQ                                                     
         BE    HK070                                                            
HK068    ZIC   R0,1(,R3)                                                        
         AR    R3,R0                                                            
         B     HK065                                                            
                                                                                
         USING LIDELD,R3                                                        
HK070    DS    0H                                                               
         CLC   LIDDLEDG,=C'1C'                                                  
         BNE   HK080                                                            
         CLI   LIDITLN,7                                                        
         BNE   HK068                                                            
         CLI   LIDDALVL,3                                                       
         BNE   HK068                                                            
                                                                                
         SR    R4,R4                                                            
         IC    R4,LIDLN                                                         
         SHI   R4,(LIDDACCS-LIDELD)                                             
                                                                                
         LA    R2,CLITAB                                                        
         LA    R6,LIDDACCS                                                      
HK075    MVC   0(3,R2),4(R6)                                                    
         LA    R6,7(,R6)                                                        
         LA    R2,3(,R2)                                                        
         SHI   R4,7                                                             
         LTR   R4,R4                                                            
         BP    HK075                                                            
         MVI   0(R2),X'FF'                                                      
         B     HK068                                                            
                                                                                
HK080    DS    0H                                                               
         CLC   LIDDLEDG,=C'SJ'                                                  
         BNE   HK068                                                            
         CLI   LIDITLN,3                                                        
         BNE   HK068                                                            
         CLI   LIDDALVL,1                                                       
         BNE   HK068                                                            
                                                                                
         SR    R4,R4                                                            
         IC    R4,LIDLN                                                         
         SHI   R4,(LIDDACCS-LIDELD)                                             
                                                                                
         LA    R2,CLITAB                                                        
         LA    R6,LIDDACCS                                                      
HK085    MVC   0(3,R2),0(R6)                                                    
         LA    R6,3(,R6)                                                        
         LA    R2,3(,R2)                                                        
         SHI   R4,3                                                             
         LTR   R4,R4                                                            
         BP    HK085                                                            
         MVI   0(R2),X'FF'                                                      
         B     HK068                                                            
                                                                                
HK087    LA    R2,CLITAB                                                        
HK087A   CLI   0(R2),X'FF'                                                      
         BE    XITNO                                                            
         CLC   0(3,R2),ACTACC+7                                                 
         BE    HK090                                                            
         LA    R2,3(,R2)                                                        
         B     HK087A                                                           
                                                                                
HK090    DS    0H                                                               
HK099    CLI   QOPT7,C'1'                                                       
         BNE   XIT                                                              
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AHI   R0,18                                                            
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1A'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
*MN                                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                                                                     *         
*---------------------------------------------------------------------*         
HK200    CLI   HOOKNUM,2                                                        
         BNE   XIT                                                              
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         CLI   QOPT6,C'2'                                                       
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
         LA    R4,GRPLEN(,R4)                                                   
         B     HK210                                                            
                                                                                
HK220    DS    0H                                                               
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         MVC   R1NME1,1(R4)                                                     
                                                                                
HK340    DS    0H                                                               
         CLI   R1REPNO,3                                                        
         BE    HK350                                                            
         CLI   R1REPNO,6                                                        
         BNE   HK360                                                            
                                                                                
HK350    CLI   R1CDE1,C'2'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   R1CDE2,C'B'         IT'S EITHER 'B' OR 'C'                       
         BNE   HK355                                                            
         MVC   R1NME2,SPACES                                                    
         MVC   R1NME2(9),=C'TOTAL 141'                                          
         B     HK399                                                            
*                                                                               
HK355    CLI   R1CDE2,C'C'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   R1NME2,SPACES                                                    
         MVC   R1NME2(26),=C'TOTAL CCG.XM NORTH AMERICA'                        
         B     HK399                                                            
*                                                                               
HK360    DS    0H                                                               
         CLI   R1REPNO,2                                                        
         BE    HK365                                                            
         CLI   R1REPNO,5                                                        
         BNE   HK399                                                            
                                                                                
HK365    CLI   R1CDE1,C'2'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   R1CDE2,C'C'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   R1CDE3,C'B'                                                      
         BNE   HK370                                                            
         MVC   R1NME3,SPACES                                                    
         MVC   R1NME3(18),=C'SUBTOTAL CCG.XM NY'                                
         B     HK399                                                            
*                                                                               
HK370    CLI   R1CDE3,C'C'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   R1NME3,SPACES                                                    
         MVC   R1NME3(13),=C'CCG.XM IRVINE'                                     
         B     HK399                                                            
*                                                                               
*                                                                               
HK399    CLI   QOPT7,C'2'                                                       
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
*        DEFINE CONSTANTS                                                       
*----------------------------------------------------------------*              
AHOOKIO  DC    A(HOOKIO)                                                        
HOOK1ST  DC    C'Y'                                                             
LSTNAME  DC    CL36' '                                                          
                                                                                
HOOKSW   DS    C                                                                
CLILIST  EQU   X'80'                                                            
                                                                                
         DS    0F                                                               
         DC    CL8'*CLITAB*'                                                    
CLITAB   DC    255CL3' '                                                        
                                                                                
*        POSITION 1 - GROUP NUMBER FOR FILTER                                   
*        POSITION 2 - FILTER                                                    
FLTRTAB  DS    0C                                                               
         DC    CL1' ',CL3'0AA'                                                  
FLTRLEN  EQU   *-FLTRTAB                                                        
         DC    C'A',C'1AA'                                                      
         DC    C'B',C'1AA'                                                      
         DC    C'C',C'1AA'                                                      
         DC    C'D',C'1AA'                                                      
         DC    C'E',C'1AA'                                                      
         DC    C'F',C'1AA'                                                      
         DC    C'K',C'1AA'                                                      
         DC    C'T',C'1AA'                                                      
         DC    C'V',C'1AA'                                                      
*        DC    C'X',C'1AA'         REMOVED 5/24/01                              
                                                                                
         DC    C'J',C'2AA'                                                      
         DC    C'W',C'2AA'                                                      
         DC    C'H',C'2BA'                                                      
         DC    C'I',C'2BA'                                                      
         DC    C'Y',C'2BA'                                                      
         DC    C'G',C'2CB'                                                      
         DC    C'U',C'2CB'                                                      
         DC    C'Z',C'2CC'                                                      
                                                                                
         DC    C'L',C'3AA'                                                      
         DC    C'M',C'3AA'                                                      
         DC    C'N',C'3AA'                                                      
         DC    C'O',C'3AA'                                                      
         DC    C'X',C'3AA'         ADDED 5/24/01                                
                                                                                
         DC    C'P',C'4AA'                                                      
         DC    C'Q',C'4AA'                                                      
         DC    C'R',C'4AA'                                                      
         DC    C'S',C'4AA'                                                      
         DC    X'FF'                                                            
                                                                                
                                                                                
GRPTAB   DS    0C                                                               
         DC    CL1'0',CL36'NON-FILTERED'                                        
GRPLEN   EQU   *-GRPTAB                                                         
         DC    CL1'1',CL36'NEW YORK CLIENTS'                                    
         DC    CL1'2',CL36'SPECIALTY UNITS'                                     
         DC    CL1'3',CL36'NON NEW YORK'                                        
         DC    CL1'4',CL36'OTHER'                                               
         DC    X'FF'                                                            
                                                                                
FLTNAME  DS    0C                                                               
         DC    CL1' ',CL36'NON-FILTERED'                                        
FLTNAML  EQU   *-FLTNAME                                                        
         DC    CL1'A',CL36'KATHERINE RAVENHALL'                                 
         DC    CL1'B',CL36'HOWARD COURTEMACHE'                                  
         DC    CL1'C',CL36'GARY STEELE'                                         
         DC    CL1'D',CL36'LIDA BURPEE(DEVINO)'                                 
         DC    CL1'E',CL36'JOHN MARCHESE'                                       
         DC    CL1'F',CL36'D. KHOSROVANI-H.K.'                                  
         DC    CL1'G',CL36'CCG.XM - MEDIA INTERACTIVE'                          
         DC    CL1'H',CL36'JAY FARRELL - DIRECT'                                
         DC    CL1'I',CL36'JAY FARRELL - PROMOTIONS'                            
         DC    CL1'J',CL36'STUDIO'                                              
         DC    CL1'K',CL36'STEVE KOST'                                          
         DC    CL1'L',CL36'TIMOTHY HART'                                        
         DC    CL1'M',CL36'JERRY KERR'                                          
         DC    CL1'N',CL36'GARY MCBRIDE'                                        
         DC    CL1'O',CL36'DAN ROMAN'                                           
         DC    CL1'P',CL36'PERRI PARLINI - TERMINATED CLIENTS'                  
         DC    CL1'Q',CL36'PERRI PARLINI - MISC. CLIENTS'                       
         DC    CL1'R',CL36'PAUL LEVINE'                                         
         DC    CL1'S',CL36'MISC. UNDER/OVER ABSORBED'                           
         DC    CL1'T',CL36'ALAN KELLAM'                                         
         DC    CL1'U',CL36'CCG.XM - INTERACTIVE'                                
         DC    CL1'V',CL36'RICH NEWMAN'                                         
         DC    CL1'W',CL36'JANET STANTON - CONSUMER LINK'                       
*        DC    CL1'X',CL36'CHRIS CLARK'                                         
         DC    CL1'X',CL36'GARY GILPIN'                                         
         DC    CL1'Y',CL36'JAY FARRELL - INTEGRATED 141'                        
         DC    CL1'Z',CL36'CCG.XM - IRVINE'                                     
         DC    X'FF'                                                            
         EJECT                                                                  
*----------------------------------------------------------------*              
*        LITERAL POOL                                                           
*----------------------------------------------------------------*              
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
R1COL7   DS    PL8                                                              
R1COL8   DS    PL8                                                              
R1COL9   DS    PL8                                                              
R1COL10  DS    PL8                                                              
R1COL11  DS    PL8                                                              
R1COL12  DS    PL8                                                              
R1COL13  DS    PL8                                                              
R1LEN    EQU   *-R1RECD                                                         
                                                                                
         CSECT                                                                  
         DS    0F                                                               
HOOKIO   DS    CL2000                                                           
         EJECT                                                                  
                                                                                
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009APGHFIBSH 09/27/01'                                      
         END                                                                    
