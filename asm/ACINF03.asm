*          DATA SET ACINF03    AT LEVEL 033 AS OF 05/01/02                      
*PHASE T60503A,*,NOAUTO                                                         
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE CO'                         
T60503   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE COMPANY  IN ACCOUNTS INFO PROGRAM         
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60503)                                               
         DC    A(FILTABLE-T60503)                                               
         DC    A(PREHEADS-T60503)                                               
         DC    A(PRETABLE-T60503)                                               
         DC    A(HEADINGS-T60503)                                               
         DC    A(DATTABLE-T60503)                                               
         DC    A(KNTRYPNT-T60503)                                               
         DC    A(FNTRYPNT-T60503)                                               
         DC    A(DNTRYPNT-T60503)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'COMPANY'                                                    
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    X'FF00'                                                          
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'ABBREVIATN'                                                 
         DC    CL2'AB'                                                          
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPABBR-ACCOMPD)                                            
         DC    AL1(L'ACMPABBR)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ET/A'                                                       
         DC    CL2'ET'                                                          
         DC    CL8'ACTUAL'                                                      
         DC    X'40'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPSTA2-ACCOMPD)                                            
         DC    AL1(L'ACMPSTA2)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ET/A'                                                       
         DC    CL2'ET'                                                          
         DC    CL8'DIFFER'                                                      
         DC    X'10'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPSTA2-ACCOMPD)                                            
         DC    AL1(L'ACMPSTA2)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ET/A'                                                       
         DC    CL2'ET'                                                          
         DC    CL8'NO'                                                          
         DC    X'01'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPSTAT-ACCOMPD)                                            
         DC    AL1(L'ACMPSTAT)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ET/A'                                                       
         DC    CL2'ET'                                                          
         DC    CL8'PLAIN'                                                       
         DC    X'80'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPSTA2-ACCOMPD)                                            
         DC    AL1(L'ACMPSTA2)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'BILLEST'                                                     
         DC    X'20'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPSTA2-ACCOMPD)                                            
         DC    AL1(L'ACMPSTA2)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'CHK/DUP'                                                     
         DC    X'08'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPSTA2-ACCOMPD)                                            
         DC    AL1(L'ACMPSTA2)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'CHK/INV'                                                     
         DC    X'40'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPSTAT-ACCOMPD)                                            
         DC    AL1(L'ACMPSTAT)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'COST'                                                        
         DC    X'10'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPSTAT-ACCOMPD)                                            
         DC    AL1(L'ACMPSTAT)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'C/D'                                                         
         DC    X'08'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPSTAT-ACCOMPD)                                            
         DC    AL1(L'ACMPSTAT)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'G/A'                                                         
         DC    X'04'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPSTAT-ACCOMPD)                                            
         DC    AL1(L'ACMPSTAT)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'MTCH/ORD'                                                    
         DC    X'80'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPSTAT-ACCOMPD)                                            
         DC    AL1(L'ACMPSTAT)                                                  
         DC    AL2(0)                                                           
*                                                                               
*&&US                                                                           
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'NEEDVEND'                                                    
         DC    X'04'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPSTA2-ACCOMPD)                                            
         DC    AL1(L'ACMPCD)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'STATUS'        NO CASH DISCOUNT - US ONLY                   
         DC    CL2'ST'                                                          
         DC    CL8'NOCD'                                                        
         DC    X'01'               X'01' BIT IN C'N' (NO)                       
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPCD-ACCOMPD)                                              
         DC    AL1(L'ACMPCD)                                                    
         DC    AL2(0)                                                           
*&&                                                                             
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'NOLABELS'                                                    
         DC    X'02'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPSTAT-ACCOMPD)                                            
         DC    AL1(L'ACMPSTAT)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'OFFC'                                                        
         DC    X'20'                                                            
         DC    AL2(ACOM-GWS)                                                    
         DC    AL2(ACMPSTAT-ACCOMPD)                                            
         DC    AL1(L'ACMPSTAT)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
         EJECT                                                                  
*              PRE-HEADING LINE - TO CONTAIN CONSTANT ELEMENTS OF               
*                                 EXPRESSIONS IN FORM X=Y,A=B                   
*                                                                               
*              CL39      C                      SCREEN COLS  2-40               
*              CL39      C                      SCREEN COLS 41-79               
*                                                                               
PREHEADS DC    CL39'                                       '                    
         DC    CL39'                                       '                    
*                                                                               
*              PRE-HEADING DATA TABLE -  9 BYTE ENTRIES                         
*                                                                               
*              CONTENTS AS FOR DISPLAY DATA TABLE BELOW                         
*                                                                               
PRETABLE DC    X'FF'               END OF PRE-HEADING DATA TABLE                
*                                                                               
*              SCREEN HEADINGS - 2 LINES                                        
*                                                                               
*              CL39      C         FIRST LINE - SCREEN COLS  2-40               
*              CL39      C         2ND   LINE -                                 
*              CL39      C         FIRST LINE - SCREEN COLS 41-79               
*              CL39      C         2ND   LINE                                   
*                                                                               
HEADINGS DC    CL39'COMPANY CODE AND NAME                  '                    
         DC    CL39'---------------------                  '                    
         DC    CL39' ID# STATUS INFORMATION                '                    
         DC    CL39' --- ------------------                '                    
*                                                                               
         EJECT                                                                  
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD                      
         SPACE 1                                                                
DATTABLE DC    AL2(AKEY-GWS)       COMPANY CODE                                 
         DC    AL2(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(EDITCOMP-GWS)                                                
         DC    AL1(2)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ANAM-GWS)       COMPANY NAME                                 
         DC    AL2(ACNMNAME-ACNAMED)                                            
         DC    AL1(L'ACNMNAME)                                                  
         DC    AL2(EDITNAME-GWS)                                                
         DC    AL1(5)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ACOM-GWS)       PRINCIPAL ID NUMBER                          
         DC    AL2(ACMPID-ACCOMPD)                                              
         DC    AL1(L'ACMPID)                                                    
         DC    AL2(EDITBIN4-GWS)                                                
         DC    AL1(41)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ACOM-GWS)       COMPANY STATUS BYTE                          
         DC    AL2(ACMPSTAT-ACCOMPD)                                            
         DC    AL1(L'ACMPSTAT)                                                  
         DC    X'FF00'                                                          
         DC    AL1(46)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
         EJECT                                                                  
KNTRYPNT DS    0D                  HANDLE OPTIONAL COMPANY KEY                  
         NMOD1 0,**COMP**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60503,RB                                                        
         CLI   1(R2),C' '                                                       
         BE    KEXIT                                                            
         GOTO1 VHEXIN,DMCB,(R2),WORK,2                                          
         OC    DMCB+12(4),DMCB+12                                               
         BZ    KERROR                                                           
         LA    R2,WORK                                                          
         MVI   DMCB,0                                                           
         B     KEXIT                                                            
KERROR   MVI   DMCB,1                                                           
         MVI   ERROR,INVALID                                                    
KEXIT    LA    R3,1                                                             
         XIT1  REGS=(R2,R3)        RETURN ADDRESS & LENGTH                      
         SPACE 3                                                                
FNTRYPNT DS    0H                                                               
         EJECT                                                                  
DNTRYPNT DS    0D                                                               
         NMOD1 4,**COMP**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC              RC = GLOBAL W/S                              
         L     RB,APHASE                                                        
         USING T60503,RB                                                        
         L     R9,ACOM                                                          
         USING CPYELD,R9                                                        
         MVI   MYSCANBK,C' '       SET UP STATUS DETAILS                        
         MVC   MYSCANBK+1(159),MYSCANBK                                         
         LA    R2,MYSCANBK                                                      
         LA    R3,34               R3 = NO OF BYTES LEFT IN THIS LINE           
         LA    R4,COTAB1           R4 = A(TABLE) FOR THIS STATUS BYTE           
         LA    R5,MYSCANBK         R5 = POINTER TO OUTPUT AREA                  
         LA    R6,CPYSTAT1         R6 = A(STATUS BYTE)                          
         BAS   RE,GETSTAT          SET UP TEXT FROM 1ST STATUS BYTE             
*                                                                               
         LA    R4,COTAB2           STATUS BYTE 2                                
         LA    R6,CPYSTAT2                                                      
         BAS   RE,GETSTAT                                                       
*                                                                               
         LA    R4,COTAB3           STATUS BYTE 3                                
         LA    R6,CPYSTAT3                                                      
         BAS   RE,GETSTAT                                                       
*                                                                               
         LA    R4,COTAB4           STATUS BYTE 4                                
         LA    R6,CPYSTAT4                                                      
         BAS   RE,GETSTAT                                                       
*                                                                               
*&&US                                                                           
         LA    R4,COTABCD          CPYCDCNO - NO CASH DISCOUNT                  
         LA    R6,CPYCDCNO                                                      
         BAS   RE,GETSTAT                                                       
*&&                                                                             
         CLI   CPYTENO,C' '        CPYTENO - # BYTES FOR TE CONTRA              
         BE    D0                                                               
         MVC   COTABNO(1),CPYTENO                                               
         MVC   COTABNO+7(1),COTABNO                                             
         LA    R4,COTABNO                                                       
         LA    R6,CPYTENO                                                       
         BAS   RE,GETSTAT                                                       
*                                                                               
D0       CLI   CPYDEPTL,C' '       CPYDEPTL- LENGTH OF DEPT CODES               
         BE    D1                                                               
         MVC   COTABDPT(1),CPYDEPTL                                             
         MVC   COTABDPT+7(1),COTABDPT                                           
         OI    COTABDPT+7,X'F0'                                                 
         LA    R4,COTABDPT                                                      
         LA    R6,CPYDEPTL                                                      
         BAS   RE,GETSTAT                                                       
*                                                                               
D1       CLI   CPYLN,CPYLN2Q       IF SHORT ELEMENT, WE'RE DONE                 
         BL    D5                                                               
*                                                                               
         LA    R4,COTAB5           STATUS BYTE 5                                
         LA    R6,CPYSTAT5                                                      
         BAS   RE,GETSTAT                                                       
*                                                                               
         LA    R4,COTAB6           STATUS BYTE 6                                
         LA    R6,CPYSTAT6                                                      
         BAS   RE,GETSTAT                                                       
*                                                                               
         LA    R4,COTAB7           STATUS BYTE 7                                
         LA    R6,CPYSTAT7                                                      
         BAS   RE,GETSTAT                                                       
*                                                                               
         LA    R4,COTAB8           STATUS BYTE 8                                
         LA    R6,CPYSTAT8                                                      
         BAS   RE,GETSTAT                                                       
*                                                                               
         OC    CPYTSD,CPYTSD       CPYTSD - TIMESHEET DAY                       
         BZ    D3                                                               
         MVC   COTABTSD(1),CPYTSD                                               
*                                                                               
         LA    RF,DAYS                                                          
         LA    R0,DAYSLNQ                                                       
         USING DAYSD,RF                                                         
*                                                                               
D2       CLC   DAYNUM,CPYTSD                                                    
         BNE   *+14                                                             
         MVC   COTABTSD+6(3),DAYNAME                                            
         B     D202                                                             
*                                                                               
         LA    RF,DAYQ(RF)                                                      
         BCT   R0,D2                                                            
*                                                                               
D202     LA    R4,COTABTSD                                                      
         LA    R6,CPYTSD                                                        
         BAS   RE,GETSTAT                                                       
*                                                                               
D3       TM    CPYSTAT7,CPYSTMSY   CPYSTMSY - ON TMS                            
         BNO   D4                                                               
         MVC   COTABTMS(1),CPYSTAT7                                             
         MVI   COTABTMS+1,X'03'    RESTORE TO ORIGINAL                          
         MVC   COTABTMS+2(3),=C'TMS'                                            
         CLI   CPYLN,CPYLN3Q       LONGER EL HAS TMS START DATE                 
         BL    D302                                                             
         OC    CPYTMSSD,CPYTMSSD   TMS START DATE?                              
         BZ    D302                NO                                           
         GOTO1 VDATCON,DMCB,(2,CPYTMSSD),(0,WORK)                               
         MVI   COTABTMS+1,X'08'                                                 
         MVI   COTABTMS+3,C'='     CHANGE TO HOLD DATE                          
         MVC   COTABTMS+4(6),WORK                                               
*                                                                               
D302     LA    R4,COTABTMS                                                      
         LA    R6,CPYSTAT7                                                      
         BAS   RE,GETSTAT                                                       
*                                                                               
D4       CLI   CPYLN,CPYLN3Q       IF NOT LONG ELEMENT, WE'RE DONE              
         BL    D5                                                               
*                                                                               
         LA    R4,COTAB9           STATUS BYTE 9                                
         LA    R6,CPYSTAT9                                                      
         BAS   RE,GETSTAT                                                       
*                                                                               
         CLI   CPYTCMP,0                                                        
         BE    D5                                                               
         MVC   COTABTID(1),CPYTCMP CPYTCMP - HEX COMPANY CODE                   
         GOTO1 VHEXOUT,DMCB,CPYTCMP,WORK,1,0,0                                  
         MVC   COTABTID+6(2),WORK                                               
         LA    R4,COTABTID                                                      
         LA    R6,CPYTCMP                                                       
         BAS   RE,GETSTAT                                                       
*                                                                               
D5       DS    0H                                                               
         SR    R4,R4                                                            
         BCTR  R5,0                                                             
         SR    R5,R2               HOW MANY ADDITIONAL LINES REQUIRED ?         
         LA    R3,34                                                            
         BM    *+8                                                              
         DR    R4,R3                                                            
         LR    R4,R5               STORE NUMBER IN R4                           
*                                                                               
T603XIT  XIT1  REGS=(R2,R4)                                                     
         EJECT                                                                  
GETSTAT  NTR1                      ROUTINE TO SET UP A STRING OF STATUS         
         SR    R7,R7               INDICATORS USING R3-R6 AS DETAILED           
         SR    R8,R8               ABOVE                                        
         SPACE 1                                                                
GS0      CLI   0(R4),0             END OF TABLE                                 
         BE    GSXIT                                                            
         IC    R7,0(R4)            IS THIS BIT SET ?                            
         EX    R7,*+8                                                           
         B     *+8                                                              
         TM    0(R6),0                                                          
         BNO   GSBUMP                                                           
         IC    R8,1(R4)            IF SO IS THERE ROOM FOR THE INDICATR         
         CR    R8,R3               ON THIS LINE ?                               
         BL    GS1                                                              
         LA    R5,0(R3,R5)         IF NOT BUMP TO NEXT LINE                     
         LA    R3,34                                                            
GS1      C     R3,=F'34'           INSERT A COMMA UNLESS START OF LINE          
         BE    GS2                                                              
         MVI   0(R5),C','                                                       
         BCTR  R3,0                                                             
         LA    R5,1(R5)                                                         
GS2      SR    R3,R8               REDUCE COUNT OF BYTES AVAILABLE              
         BCTR  R8,0                AND MOVE INDICATOR TO OUTPUT AREA            
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),2(R4)                                                    
         LA    R5,1(R5,R8)                                                      
GSBUMP   LA    R4,L'COTAB(R4)      BUMP TO NEXT TABLE ENTRY                     
         B     GS0                                                              
         SPACE 1                                                                
GSXIT    XIT1  REGS=(R3,R5)                                                     
         EJECT                                                                  
*              STATUS BYTE TABLES                                               
COTAB    DS    0CL10               BIT SETTING/LENGTH/INDICATOR                 
*                                                                               
COTAB1   DC    X'80',AL1(7),C'MTCHORD '      CPYSTAT1                           
         DC    X'40',AL1(5),C'CKINV   '                                         
         DC    X'20',AL1(6),C'OFFICE  '                                         
         DC    X'10',AL1(4),C'COST    '                                         
         DC    X'08',AL1(2),C'CD      '                                         
         DC    X'04',AL1(2),C'GA      '                                         
         DC    X'02',AL1(6),C'LBLS=N  '                                         
         DC    X'01',AL1(6),C'ESTA=N  '                                         
         DC    X'00'                                                            
*                                                                               
COTAB2   DC    X'80',AL1(8),C'ESTA=PLN'      CPYSTAT2                           
         DC    X'40',AL1(8),C'ESTA=ACT'                                         
         DC    X'20',AL1(7),C'BILLEST '                                         
         DC    X'10',AL1(8),C'ESTA=DIF'                                         
         DC    X'08',AL1(6),C'CHKDUP  '                                         
         DC    X'04',AL1(4),C'VEND    '                                         
         DC    X'02',AL1(8),C'SJC/A=SC'                                         
         DC    X'01',AL1(6),C'PAYEST  '                                         
         DC    X'00'                                                            
*                                                                               
COTAB3   DC    X'80',AL1(8),C'SXC/A=CL'      CPYSTAT3                           
         DC    X'40',AL1(2),C'WO      '                                         
         DC    X'20',AL1(5),C'PC=1C   '                                         
         DC    X'10',AL1(4),C'BA=Y    '                                         
         DC    X'08',AL1(8),C'SEC/A=SC'                                         
         DC    X'04',AL1(5),C'PC=SJ   '                                         
         DC    X'02',AL1(3),C'DPS     '                                         
         DC    X'01',AL1(4),C'BS=Y    '                                         
         DC    X'00'                                                            
*                                                                               
COTAB4   DC    X'80',AL1(7),C'%EST=SK '      CPYSTAT4                           
         DC    X'40',AL1(5),C'BBD=Y   '                                         
         DC    X'20',AL1(7),C'NEWPROD '                                         
         DC    X'10',AL1(2),C'MI      '                                         
*        DC    X'08',AL1(0),C'        '                                         
*        DC    X'04',AL1(0),C'        '                                         
*        DC    X'02',AL1(0),C'        '                                         
         DC    X'01',AL1(6),C'NEWOFF  '                                         
         DC    X'00'                                                            
*                                                                               
COTAB5   DC    X'80',AL1(8),C'OFF=PNNL'      CPYSTAT5                           
*        DC    X'40',AL1(0),C'        '                                         
         DC    X'20',AL1(7),C'APP=REG '                                         
         DC    X'10',AL1(7),C'APP=EFF '                                         
         DC    X'08',AL1(7),C'NEWCOST '                                         
         DC    X'04',AL1(6),C'EXPROD  '                                         
         DC    X'02',AL1(6),C'VENCOP  '                                         
         DC    X'01',AL1(4),C'APG$    '                                         
         DC    X'00'                                                            
*                                                                               
COTAB6   DC    X'80',AL1(6),C'ADVPTR  '      CPYSTAT6                           
         DC    X'40',AL1(5),C'RAPTR   '                                         
*        DC    X'20',AL1(0),C'        '                                         
*        DC    X'10',AL1(0),C'        '                                         
*        DC    X'08',AL1(0),C'        '                                         
*        DC    X'04',AL1(0),C'        '                                         
*        DC    X'02',AL1(0),C'        '                                         
*        DC    X'01',AL1(0),C'        '                                         
         DC    X'00'                                                            
*                                                                               
*        DC    X'80',AL1(0),C'        '      CPYSTAT7                           
*        DC    X'40',AL1(0),C'        '                                         
COTAB7   DC    X'20',AL1(7),C'ACTGRPS '                                         
*        DC    X'10',AL1(0),C'        '                                         
*        DC    X'08',AL1(0),C'        '                                         
         DC    X'04',AL1(5),C'JTIME   '                                         
         DC    X'02',AL1(5),C'PERAN   '                                         
         DC    X'01',AL1(7),C'NEWBILL '                                         
         DC    X'00'                                                            
*                                                                               
COTAB8   DC    X'80',AL1(5),C'RLOGO   '      CPYSTAT8                           
*        DC    X'40',AL1(0),C'        '                                         
*        DC    X'20',AL1(0),C'        '                                         
*        DC    X'10',AL1(0),C'        '                                         
*        DC    X'08',AL1(0),C'        '                                         
*        DC    X'04',AL1(0),C'        '                                         
*        DC    X'02',AL1(0),C'        '                                         
*        DC    X'01',AL1(0),C'        '                                         
         DC    X'00'                                                            
*                                                                               
*        DC    X'80',AL1(0),C'        '      CPYSTAT9                           
COTAB9   DC    X'40',AL1(8),C'MINHOURS'                                         
*        DC    X'20',AL1(0),C'        '                                         
*        DC    X'10',AL1(0),C'        '                                         
*        DC    X'08',AL1(0),C'        '                                         
*        DC    X'04',AL1(0),C'        '                                         
*        DC    X'02',AL1(0),C'        '                                         
*        DC    X'01',AL1(0),C'        '                                         
         DC    X'00'                                                            
*                                                                               
COTABCD  DC    X'D5',AL1(6),C'DISC=N  '      CPYCDCNO                           
         DC    X'00'                                                            
*                                                                               
COTABNO  DC    X'FF',AL1(6),C'TENO=   '      CPYTENO                            
         DC    X'00'                                                            
*                                                                               
COTABDPT DC    X'FF',AL1(6),C'DPTL=   '      CPYDEPTL                           
         DC    X'00'                                                            
*                                                                               
COTABTID DC    X'FF',AL1(6),C'TID=    '      CPYTCMP                            
         DC    X'00'                                                            
*                                                                               
COTABTSD DC    X'FF',AL1(7),C'TSD=    '      CPYTSD                             
         DC    X'00'                                                            
*                                                                               
COTABTMS DC    X'FF',AL1(8),C'TMS     '      CPYTMSY                            
         DC    X'00'                                                            
*                                                                               
DAYS     DC    AL1(1),C'MON'                                                    
         DC    AL1(2),C'TUE'                                                    
         DC    AL1(3),C'WED'                                                    
         DC    AL1(4),C'THU'                                                    
         DC    AL1(5),C'FRI'                                                    
         DC    AL1(6),C'SAT'                                                    
         DC    AL1(7),C'SUN'                                                    
DAYSLNQ  EQU   (*-DAYS)/4                                                       
*                                                                               
MYSCANBK DS    8CL30                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              DAY TABLE DSECT                                        *         
***********************************************************************         
*                                                                               
DAYSD    DSECT                                                                  
DAYNUM   DS    XL1                 CORRESPONDING DAY NUMBER FROM GETDAY         
DAYNAME  DS    CL3                 ABBREVIATED DAY                              
DAYQ     EQU   *-DAYSD                                                          
         EJECT                                                                  
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033ACINF03   05/01/02'                                      
         END                                                                    
