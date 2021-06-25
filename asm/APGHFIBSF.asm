*          DATA SET APGHFIBSF  AT LEVEL 031 AS OF 05/01/02                      
*PHASE ACHFBSFA                                                                 
         TITLE 'BATES CLIENT FILTER GROUPINGS'                                  
ACHFBSF  CSECT                                                                  
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
         CLI   HOOKTYPE,CMPTHK     PUT HOOK ?                                   
         BNE   HK200               NO                                           
         AHI   R7,18                                                            
                                                                                
         CLI   QOPT6,C'1'                                                       
         BNE   HK010                                                            
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AHI   R0,18                                                            
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1B'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         AHI   R7,18                                                            
                                                                                
*---------------------------------------------------------------------*         
*        IS THERE ANY REQUEST OFFICE/OFFICE LIST FILTER - QSELECT(2)  *         
*---------------------------------------------------------------------*         
HK010    CLC   QSELECT(2),=C'  '   IF BLANK THEN NO FILTERING REQUESTED         
         BE    HK110                                                            
                                                                                
         L     RF,=A(HOOKIO)                                                    
         ST    RF,AHOOKIO                                                       
         TM    HOOKSW,OFFLIST      CONTRA OFFICE LIST RETRIEVED                 
         BO    HK050               YES                                          
         OI    HOOKSW,OFFLIST      NO, BUT SET FOR NEXT TIME IN                 
                                                                                
         LA    R2,OFFTAB           SET UP DEFAULT VALUE IN TABLE                
         MVC   0(2,R2),QSELECT                                                  
         MVI   2(R2),X'FF'                                                      
                                                                                
         USING OFFRECD,R3                                                       
         L     R3,AHOOKIO          READ OFFICE RECORD                           
         MVC   0(42,R3),SPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,QCOMPANY                                                 
         MVC   OFFKOFF,QSELECT                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',AHOOKIO,AHOOKIO                  
         TM    DMCB+8,X'10'        RECORD FOUND ?                               
         BO    HK052               NO                                           
                                                                                
         L     R3,AHOOKIO                                                       
         AH    R3,DATADISP                                                      
HK015    CLI   0(R3),0             GET OFFICE ELEMENTS IN LIST                  
         BE    HK050                                                            
         CLI   0(R3),OFLELQ        X'D2' OFFICE LIST ELEMENT                    
         BE    HK020                                                            
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     HK015                                                            
                                                                                
         USING OFLELD,R3                                                        
HK020    LA    R2,OFFTAB           STORE OFF OFFICE LIST                        
         LA    R4,OFLNTRY                                                       
         ZIC   R5,OFLLN                                                         
         SHI   R5,OFLLN1Q+1                                                     
         EXMVC R5,0(R2),0(R4)      MOVE IN LIST OF OFFICES                      
         AR    R2,R5               POINT TO END                                 
         MVI   1(R2),X'FF'         MARK END OF LIST                             
                                                                                
HK050    DS    0H                                                               
         BAS   RE,SETOFF           GET OFFICE FROM CONTRA                       
         CLC   THISOFF,SPACES                                                   
         BE    HK110               NO OFFICE CODE                               
                                                                                
HK052    LA    R2,OFFTAB           MAKE SURE THIS OFFICE IS IN LIST             
HK052A   CLI   0(R2),X'FF'                                                      
         BE    XITNO               NO, SO EXIT, CC = NO                         
         CLC   0(2,R2),THISOFF     IS  OFFICE MATCH THIS ONE IN LIST ?          
         BE    HK110               YES, MATCHED                                 
         LA    R2,2(,R2)           NO,  SO TRY NEXT                             
         B     HK052A                                                           
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        CLIENT LIST - QSELECT+2(4)                                   *         
*---------------------------------------------------------------------*         
HK110    DS    0H                                                               
         CLC   QSELECT+2(4),SPACES   ANY CLIENT LIST ?                          
         BE    HK150                 NO                                         
         L     RF,=A(HOOKIO)                                                    
         ST    RF,AHOOKIO                                                       
         TM    HOOKSW,CLILIST      DID WE ALREADY PROCESS ?                     
         BO    HK140               YES                                          
         OI    HOOKSW,CLILIST      NO                                           
                                                                                
         LA    R2,CLITAB                                                        
         MVI   0(R2),X'FF'         INITIALIZE TO NONE IN LIST                   
                                                                                
         USING LSTRECD,R3                                                       
         L     R3,AHOOKIO                                                       
         MVC   0(42,R3),SPACES                                                  
         MVI   LSTKTYP,LSTKTYPQ                                                 
         MVC   LSTKCPY,QCOMPANY                                                 
         MVC   LSTKLST(4),QSELECT+2                                             
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',AHOOKIO,AHOOKIO                  
         TM    DMCB+8,X'10'        RECORD FOUND ?                               
         BO    HK140               NO                                           
                                                                                
         L     R3,AHOOKIO                                                       
         AH    R3,DATADISP                                                      
HK115    CLI   0(R3),0                                                          
         BE    HK140               FINISHED                                     
         CLI   0(R3),LIDELQ        X'1F' LIST DATA ELEMENT                      
         BE    HK120               FOUND                                        
HK118    ZIC   R0,1(,R3)                                                        
         AR    R3,R0                                                            
         B     HK115                                                            
                                                                                
         USING LIDELD,R3                                                        
HK120    DS    0H                                                               
         CLC   LIDDLEDG,=C'1C'     PROCESS 1C LEDGER LIST                       
         BNE   HK130                                                            
         CLI   LIDITLN,7           LENGTH OF OFFICE(2),SBU(2),CLIENT(3)         
         BNE   HK118                                                            
         CLI   LIDDALVL,3          LEVEL 3 ONLY                                 
         BNE   HK118                                                            
                                                                                
         SR    R4,R4                                                            
         IC    R4,LIDLN                                                         
         SHI   R4,(LIDDACCS-LIDELD)                                             
                                                                                
         LA    R2,CLITAB                                                        
         LA    R6,LIDDACCS         POINT TO ACCOUNTS                            
HK125    MVC   0(3,R2),4(R6)       SAVE OFF CLIENT PORTION ONLY                 
         LA    R6,7(,R6)           NEXT ACCOUNT                                 
         LA    R2,3(,R2)           NEXT CLIENT IN TABLE                         
         SHI   R4,7                LENGTH LESS 7                                
         LTR   R4,R4               ANY MORE TO PROCESS ?                        
         BP    HK125               YES                                          
         MVI   0(R2),X'FF'         NO, MARK END OF TABLE                        
         B     HK118                                                            
                                                                                
HK130    DS    0H                                                               
         CLC   LIDDLEDG,=C'SJ'     PROCESS SJ CLIENT LIST                       
         BNE   HK118                                                            
         CLI   LIDITLN,3           CLIENT(3)                                    
         BNE   HK118                                                            
         CLI   LIDDALVL,1          LEVEL 1 ONLY                                 
         BNE   HK118                                                            
                                                                                
         SR    R4,R4                                                            
         IC    R4,LIDLN                                                         
         SHI   R4,(LIDDACCS-LIDELD)                                             
                                                                                
         LA    R2,CLITAB                                                        
         LA    R6,LIDDACCS         POINT TO ACCOUNTS                            
HK135    MVC   0(3,R2),0(R6)       SAVE OFF CLIENT IN TABLE                     
         LA    R6,3(,R6)           NEXT CLIENT IN LIST                          
         LA    R2,3(,R2)           NEXT CLIENT IN TABLE                         
         SHI   R4,3                                                             
         LTR   R4,R4               ANY MORE TO PROCESS ?                        
         BP    HK135               YES                                          
         MVI   0(R2),X'FF'         NO, MARK END OF TABLE                        
         B     HK118                                                            
                                                                                
HK140    LA    R2,CLITAB           FIND CLIENT IN TABLE                         
HK140A   CLI   0(R2),X'FF'                                                      
         BE    XITNO               DIDN'T FIND SO EXIT                          
         CLC   0(3,R2),ACTACC+7    DID WE MATCH ON CLIENT ?                     
         BE    HK150               YES                                          
         LA    R2,3(,R2)           NO, TRY AGAIN                                
         B     HK140A                                                           
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        SET GROUP VALUES INTO ROWS                                   *         
*---------------------------------------------------------------------*         
HK150    L     R6,HOOKAREC         ADDR OF SORT RECORD                          
         CLI   HOOKNUM,1           COMPANY LEVEL REPORTS                        
         BE    HK199               DON'T PROCESS COMPANY LEVEL                  
                                                                                
HK160    DS    0H                                                               
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         AHI   R7,18                                                            
         LA    R4,FLTRTAB                                                       
HK165    CLI   0(R4),X'FF'         FIND FILTER VALUE F5 IN TABLE                
         BE    HK180                                                            
         CLC   0(1,R4),R1CDE1      DID IN MATCH ?                               
         BE    HK170               YES                                          
         LA    R4,FLTRLEN(,R4)     NO, TRY NEXT                                 
         B     HK165                                                            
                                                                                
HK170    MVC   R1CDE1(1),1(R4)     SAVE GROUP CODE INTO POSITION                
         CLI   HOOKNUM,2           ONLY ONE ROW TO REPLACE ?                    
         BE    HK190                                                            
         MVC   R1CDE2(1),2(R4)     MOVE 2ND GROUP INTO POSITION                 
         CLI   HOOKNUM,3           ONLY TWO ROWS TO REPLACE ?                   
         BE    HK190                                                            
         MVC   R1CDE3(1),3(R4)     MOVE THIRD GROUP INTO POSITION               
         B     HK190                                                            
                                                                                
HK180    MVI   R1CDE1,C'0'         SET DEFAULT CODE                             
         CLI   HOOKNUM,2           ONLY ONE ROW TO REPLACE ?                    
         BE    HK190                                                            
         MVI   R1CDE2,C'A'         SET DEFAULT CODE                             
         CLI   HOOKNUM,3           ONLY TWO ROWS TO REPLACE ?                   
         BE    HK190                                                            
         MVI   R1CDE3,C'A'         SET DEFAULT CODE                             
                                                                                
HK190    DS    0H                                                               
         CLI   HOOKNUM,4           ONLY THREE ROWS REPLACED                     
         BE    HK195                                                            
         CLI   HOOKNUM,3           ONLY TWO   ROWS REPLACED                     
         BNE   HK199                                                            
                                                                                
HK195    CLI   R1CDE1,C'2'         ONLY KEEP ONES WITH THIS VALUE               
         BNE   XITNO                                                            
         CLI   R1CDE2,C'C'         ONLY KEEP ONES WITH THIS VALUE               
         BNE   XITNO                                                            
                                                                                
HK199    CLI   QOPT7,C'1'                                                       
         BNE   XIT                                                              
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AHI   R0,18                                                            
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1A'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
         EJECT                                                                  
                                                                                
*-------------------------------------------------------------------*           
*        SORTOUT                                                                
*-------------------------------------------------------------------*           
HK200    CLI   HOOKTYPE,CMSROT                                                  
         BNE   XIT                                                              
         CLI   QOPT6,C'2'                                                       
         BNE   HK205                                                            
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD2B'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
HK205    DS    0H                                                               
         LA    R4,GRPTAB           FIND GROUP NAME                              
HK210    CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   0(1,R4),R1CDE1      FIND GROUP IN TABLE                          
         BE    HK220                                                            
         LA    R4,GRPLEN(,R4)      TRY AGAIN                                    
         B     HK210                                                            
                                                                                
HK220    DS    0H                                                               
         MVC   R1NME1,1(R4)        MOVE IN NAME                                 
                                                                                
HK340    DS    0H                                                               
         LA    R4,R1NME3                                                        
         CLI   HOOKNUM,4           3 ROWS OF F5                                 
         BE    HK350               YES                                          
         LA    R4,R1NME2                                                        
         CLI   HOOKNUM,3           2 ROWS OF F5                                 
         BNE   HK360               YES                                          
*                                                                               
HK350    CLI   R1CDE1,C'2'         GROUP 2 RECORDS ONLY                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   R1CDE2,C'B'         IT HAS TO BE EITHER 'B' OR 'C'               
         BNE   HK351                                                            
         MVC   0(L'R1NME2,R4),SPACES                                            
         MVC   0(9,R4),=C'TOTAL 141'                                            
         B     HK360                                                            
*                                                                               
HK351    CLI   R1CDE2,C'C'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   HOOKNUM,3           TWO   ROWS OF F5                             
         BE    HK352                                                            
         CLI   HOOKNUM,4           THREE ROWS OF F5                             
         BNE   HK360                                                            
                                                                                
HK352    MVC   R1NME2,SPACES                                                    
         MVC   R1NME2(26),=C'TOTAL CCG.XM NORTH AMERICA'                        
         B     HK360                                                            
*                                                                               
HK353    DS    0H                                                               
         CLI   R1CDE3,C'B'                                                      
         BNE   HK353C                                                           
         MVC   R1NME3,SPACES                                                    
         MVC   R1NME3(18),=C'SUBTOTAL CCG.XM NY'                                
         B     HK360                                                            
*                                                                               
HK353C   DS    0H                                                               
         CLI   R1CDE3,C'C'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   R1NME3,SPACES                                                    
         MVC   R1NME3(20),=C'TOTAL CCG.XM IRVINE'                               
         B     HK360                                                            
*                                                                               
HK360    CLI   QOPT7,C'2'                                                       
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
         MVC   THISOFF,SPACES                                                   
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------*              
*        DEFINE CONSTANTS                                                       
*----------------------------------------------------------------*              
AHOOKIO  DS    A                                                                
HOOKSW   DC    X'00'                                                            
OFFLIST  EQU   X'80'                                                            
CLILIST  EQU   X'40'                                                            
THISOFF  DC    CL2' '                                                           
                                                                                
         DS    0F                                                               
         DC    CL8'*OFFTAB*'                                                    
OFFTAB   DC    255CL2' '                                                        
                                                                                
         DS    0F                                                               
         DC    CL8'*CLITAB*'                                                    
CLITAB   DC    255CL3'  '                                                       
                                                                                
LSTNAME  DC    CL36' '                                                          
                                                                                
*        POSITION 1 - FILTER                                                    
*        POSITION 2 - GROUP NUMBER FOR FILTER                                   
*        POSITION 3 - SUB GROUP                                                 
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
*        DC    C'X',C'1AA'         REMOVE 5/24/01                               
*                                                                               
         DC    C'J',C'2AA'                                                      
         DC    C'W',C'2AA'                                                      
         DC    C'H',C'2BB'                                                      
         DC    C'I',C'2BB'                                                      
         DC    C'Y',C'2BB'                                                      
         DC    C'G',C'2CB'                                                      
         DC    C'U',C'2CB'                                                      
         DC    C'Z',C'2CC'                                                      
*                                                                               
         DC    C'L',C'3AA'                                                      
         DC    C'M',C'3AA'                                                      
         DC    C'N',C'3AA'                                                      
         DC    C'O',C'3AA'                                                      
         DC    C'X',C'3AA'         MOVE 5/24/01                                 
*                                                                               
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
R1ROW7   DS    XL2                                                              
R1CDE7   DS    XL14                                                             
R1ROW8   DS    XL2                                                              
R1CDE8   DS    XL14                                                             
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
R1NME7   DS    CL36                                                             
R1NME8   DS    CL36                                                             
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
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031APGHFIBSF 05/01/02'                                      
         END                                                                    
