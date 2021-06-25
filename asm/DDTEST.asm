*          DATA SET DDTEST     AT LEVEL 003 AS OF 11/02/17                      
*PHASE TESTA        <==                                                         
*INCLUDE CARDS                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE DTCNV                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE MDUMPER    <==                                                         
***CLUDE PDUMPER    <==                                                         
*INCLUDE PERVAL                                                                 
*INCLUDE PERVERT                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE RANDOM                                                                 
*INCLUDE SMTP       <==                                                         
*INCLUDE STXITER                                                                
*INCLUDE TIMBER                                                                 
*                                                                               
         TITLE 'DDTEST - TEST VARIOUS SUBROUTINES'                              
         PRINT NOGEN                                                            
TEST     CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         NBASE 0,TEST,WORK=A(WORK),RA                                           
         ST    RB,SAVERB                                                        
*                                                                               
         CTRY                                                                   
*                                                                               
LOOP     GOTO1 =V(CARDS),PLIST,PDATA,=C'RE00'                                   
         CLC   PDATA(2),=C'/*'                                                  
         BE    EXIT                                                             
         CLC   PDATA(4),=C'PERIOD='                                             
         BE    PERV                                                             
         CLC   PDATA(2),=C'DATE='                                               
         BE    DATE                                                             
         CLC   PDATA(5),=C'CALC='                                               
         BE    CALC                                                             
         CLC   PDATA(5),=C'SUMS='                                               
         BE    SUMS                                                             
         CLC   PDATA(5),=C'CTRY='                                               
         BE    CTRY                                                             
         CLC   PDATA(5),=C'TIME='                                               
         BE    TIME                                                             
         CLC   PDATA(4),=C'DUMP'                                                
         BE    DUMP                                                             
         B     PRNT                                                             
                                                                                
CTRY     MVC   COUNTRY,PDATA+5                                                  
         NI    COUNTRY,X'0F'                                                    
         B     PRNT                                                             
*                                                                               
ERROR    MVC   PDATA+60(11),=CL11'** ERROR **'                                  
*                                                                               
PRNT     GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         B     LOOP                                                             
         ABEND 101                                                              
*                                                                               
EXIT     XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
*CARD FORMAT AS FOLLOWS                                               *         
*PERIOD=XX..... WHERE XX...... IS ANY VALID INPUT TO PERVAL           *         
***********************************************************************         
PERV     CLC   PDATA+7(5),=C'DEATH'                                             
         BNE   *+6                                                              
         DC    H'0'                                                             
PERV1    CLC   PDATA+7(4),=C'WORK'                                              
         BNE   PERV2                                                            
         GOTO1 =V(HEXOUT),DMCB,PERVWRK,PDATA+14,24,=C'TOG'                      
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA(14),SPACES                                                 
         GOTO1 =V(HEXOUT),DMCB,PERVWRK+24,PDATA+14,24,=C'TOG'                   
         B     PRNT                                                             
*                                                                               
PERV2    CLC   PDATA+7(3),=C'OLD'                                               
         BNE   PERV3                                                            
         GOTO1 =V(HEXOUT),DMCB,PERVBLKX+00,PDATA+14,07,=C'TOG'                  
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA(14),SPACES                                                 
         GOTO1 =V(HEXOUT),DMCB,PERVBLKX+28,PDATA+14,28,=C'TOG'                  
         B     PRNT                                                             
*                                                                               
PERV3    CLC   PDATA+7(3),=C'NEW'                                               
         BNE   PERV4                                                            
         GOTO1 =V(HEXOUT),DMCB,PERVBLK+00,PDATA+14,07,=C'TOG'                   
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA(14),SPACES                                                 
         GOTO1 =V(HEXOUT),DMCB,PERVBLK+28,PDATA+14,28,=C'TOG'                   
         B     PRNT                                                             
*                                                                               
PERV4    CLC   PDATA+7(3),=C'COMP'                                              
         BNE   PERVGO                                                           
         MVC   PDATA+14(4),=C'SAME'                                             
         CLC   PERVBLK(56),PERVBLKX                                             
         BE    PRNT                                                             
         MVC   PDATA+14(9),=C'DIFFERENT'                                        
         B     PRNT                                                             
*                                                                               
PERVGO   MVC   PERVEYE,=C'PVPVPVPV'                                             
         LA    R5,PERVBLK                                                       
         USING PERVALD,R5                                                       
         XC    PVALOUTB,PVALOUTB                                                
         MVC   PVALCPER,SPACES                                                  
         MVC   PVALESTA,SPACES                                                  
         MVC   PVALEEND,SPACES                                                  
         MVC   PERVBLKX,PERVBLK                                                 
         MVI   PVOPT1,30           INPUT LENGTH                                 
         MVC   PVOPT2,COUNTRY      COUNTRY CODE                                 
*                                                                               
PERVGO1  CLC   PDATA+4(2),=C'OD'   PERIOD=NORMAL CALL                           
         BE    PERVGOGO                                                         
PERVGO2  CLI   PDATA+4,C'D'        ....D.=VALIDATE FOR DDMM                     
         BNE   PERVGO3                                                          
         OI    PVOPT1,PVINDDMM                                                  
PERVGO3  CLI   PDATA+4,C'X'        ....X.=EXTENDED DDMM BINARY/96-97            
         BNE   PERVGO4                                                          
         OI    PVOPT1,PVINXTND                                                  
         MVI   PVALIND1,PVALBVP+PVALDDMM                                        
         MVC   PVALPERD(6),=X'60070161061E'                                     
PERVGO4  CLI   PDATA+4,C'Y'        ....Y.=EXTENDED DDMM COMPRD/97-98            
         BNE   PERVGO5                                                          
         OI    PVOPT1,PVINXTND                                                  
         MVI   PVALIND1,PVALCVP+PVALDDMM                                        
         MVC   PVALPERD(4),=X'C2E1C4DE'                                         
PERVGO5  CLI   PDATA+4,C'Z'        ....Z.=EXTENDED MMYY PACKED/99-00            
         BNE   PERVGO6                                                          
         OI    PVOPT1,PVINXTND                                                  
         MVI   PVALIND1,PVALPVP                                                 
         MVC   PVALPERD(6),=X'990701A00630'                                     
         B     PERVGOGO                                                         
PERVGO6  CLI   PDATA+5,C'S'        .....S=SINGLE DATE ONLY                      
         BNE   PERVGO7                                                          
         OI    PVOPT2,PVINSGLO                                                  
PERVGO7  CLI   PDATA+5,C'T'        .....T=SINGLE DATE/RETURN SINGLE             
         BNE   PERVGO8                                                          
         OI    PVOPT2,PVINSGLO+PVINSGLS                                         
PERVGO8  CLI   PDATA+5,C'U'        .....U=RETURN SINGLE                         
         BNE   PERVGO9                                                          
         OI    PVOPT2,PVINSGLS                                                  
PERVGO9  EQU   *                                                                
*                                                                               
PERVGOGO GOTO1 =V(PERVAL),PLIST,(PVOPT1,PDATA+7),(PVOPT2,(R5))                  
         LA    RE,72(RD)                                                        
         MVC   PERVWRK,76(RE)                                                   
         GOTO1 =V(HEXOUT),DMCB,PLIST+4,PDATA+37,1,=C'TOG'                       
         MVC   PDATA+40(17),PVALCPER                                            
         B     PRNT                                                             
         EJECT                                                                  
***********************************************************************         
*CARD FORMAT AS FOLLOWS                                               *         
*DATE=XX....... WHERE XX...... IS ANY VALID INPUT DATE                *         
***********************************************************************         
DATE     CLC   PDATA+5(5),=C'TODAY'                                             
         BNE   DATE0                                                            
         GOTO1 =V(DATCON),PLIST,(5,0),(0,DATEIN)                                
         CLC   PDATA+10(4),=C'REAL'                                             
         BE    DATER                                                            
         CLC   PDATA+10(3),=C'ALL'                                              
         BE    DATEA                                                            
         B     DATE1                                                            
                                                                                
***********************************************************************         
* TEST OF REAL DATE/TIME CALL (5,1)                                   *         
***********************************************************************         
DATER    MVC   DATETIME,SPACES                                                  
         GOTO1 =V(DATCON),PLIST,(5,0),(26,DATETIME)                             
         SR    RE,RE                                                            
         ICM   RE,7,PLIST+1                                                     
         MVC   DATEPACK(8),0(RE)   PL4=JULIAN DATE,XL4=TIME IN SECS             
         MVC   PDATA+20(4),=C'LPAR'                                             
         MVC   PDATA+30(6),DATEIN                                               
         GOTO1 =V(HEXOUT),PLIST,DATEPACK,PDATA+40,8,=C'TOG'                     
         MVC   PDATA+60(16),DATETIME                                            
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA(30),SPACES                                                 
*                                                                               
         GOTO1 =V(DATCON),PLIST,(5,1),(0,DATEINR)                               
         MVC   DATETIME,SPACES                                                  
         GOTO1 =V(DATCON),PLIST,(5,1),(26,DATETIME)                             
         SR    RE,RE                                                            
         ICM   RE,7,PLIST+1                                                     
         MVC   DATEPACK(8),0(RE)   PL4=JULIAN DATE,XL4=TIME IN SECS             
         MVC   PDATA+20(4),=C'REAL'                                             
         MVC   PDATA+30(6),DATEINR                                              
         GOTO1 =V(HEXOUT),PLIST,DATEPACK,PDATA+40,8,=C'TOG'                     
         MVC   PDATA+60(16),DATETIME                                            
         B     PRNT                                                             
                                                                                
***********************************************************************         
* TEST OF ALL OUTPUT TYPES FROM TODAYS DATE                           *         
***********************************************************************         
DATEA    LA    R5,DATEAT           R5=A(TABLE OF ALL OUTPUT TYPES)              
*                                                                               
DATEA1   MVC   DATEMUL,SPACES                                                   
         LLC   R0,0(R5)                                                         
         GOTO1 =V(DATCON),PLIST,(X'00',DATEIN),((R0),DATEMUL)                   
         GOTO1 =V(HEXOUT),PLIST,0(R5),PDATA+20,1,=C'TOG'                        
         CLI   2(R5),C'A'                                                       
         BNE   DATEA2                                                           
         MVC   PDATA+40(14),DATEMUL                                             
         B     DATEA3                                                           
DATEA2   LLC   R6,1(R5)                                                         
         GOTO1 =V(HEXOUT),PLIST,DATEMUL,PDATA+40,(R6),=C'TOG'                   
DATEA3   GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA,SPACES                                                     
*                                                                               
DATEA4   LA    R5,4(R5)            BUMP TO NEXT OUTPUT TYPE ENTRY               
         CLI   0(R5),X'FF'                                                      
         BNE   DATEA1                                                           
*                                                                               
DATEA5   MVC   PDATA+20(5),=C'00 IN'                                            
         GOTO1 =V(DATCON),PLIST,(0,DATEMUL+00),(23,PDATA+40)                    
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA+20(5),=C'01 IN'                                            
         GOTO1 =V(DATCON),PLIST,(1,DATEMUL+06),(23,PDATA+40)                    
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA+20(5),=C'02 IN'                                            
         GOTO1 =V(DATCON),PLIST,(2,DATEMUL+09),(23,PDATA+40)                    
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA+20(5),=C'03 IN'                                            
         GOTO1 =V(DATCON),PLIST,(3,DATEMUL+11),(23,PDATA+40)                    
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         B     LOOP                                                             
*                                                                               
DATEAT   DS    0XL4                        VALUE,LENGTH,TYPE,N/D                
         DC    AL1(00),AL1(06),C'B',AL1(0)                                      
         DC    AL1(01),AL1(03),C'B',AL1(0)                                      
         DC    AL1(02),AL1(02),C'B',AL1(0)                                      
         DC    AL1(03),AL1(03),C'B',AL1(0)                                      
         DC    AL1(04),AL1(06),C'A',AL1(0)                                      
         DC    AL1(05),AL1(05),C'A',AL1(0)                                      
         DC    AL1(06),AL1(08),C'A',AL1(0)                                      
         DC    AL1(07),AL1(06),C'A',AL1(0)                                      
         DC    AL1(08),AL1(08),C'A',AL1(0)                                      
         DC    AL1(09),AL1(06),C'A',AL1(0)                                      
         DC    AL1(10),AL1(08),C'A',AL1(0)                                      
         DC    AL1(11),AL1(08),C'A',AL1(0)                                      
         DC    AL1(12),AL1(05),C'A',AL1(0)                                      
         DC    AL1(13),AL1(08),C'A',AL1(0)                                      
         DC    AL1(14),AL1(06),C'A',AL1(0)                                      
         DC    AL1(15),AL1(04),C'B',AL1(0)                                      
         DC    AL1(16),AL1(05),C'A',AL1(0)                                      
         DC    AL1(17),AL1(08),C'A',AL1(0)                                      
         DC    AL1(18),AL1(06),C'A',AL1(0)                                      
         DC    AL1(19),AL1(03),C'B',AL1(0)                                      
         DC    AL1(20),AL1(08),C'A',AL1(0)                                      
         DC    AL1(21),AL1(10),C'A',AL1(0)                                      
         DC    AL1(22),AL1(08),C'A',AL1(0)                                      
         DC    AL1(23),AL1(10),C'A',AL1(0)                                      
         DC    AL1(24),AL1(04),C'B',AL1(0)                                      
         DC    AL1(25),AL1(06),C'B',AL1(0)                                      
         DC    AL1(26),AL1(14),C'A',AL1(0)                                      
         DC    AL1(30),AL1(02),C'B',AL1(0)                                      
         DC    AL1(31),AL1(14),C'B',AL1(0)                                      
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* TEST OF OTHER DATCON TYPES FROM AN INPUT DATE                       *         
***********************************************************************         
DATE0    GOTO1 =V(DATVAL),PLIST,(0,PDATA+5),DATEIN                              
*                                                                               
DATE1    MVC   PDATA+20(6),DATEIN                                               
         CLC   PLIST(4),=F'0'                                                   
         BNE   DATE2                                                            
         MVC   PDATA+20(6),=C'??????'                                           
         B     PRNT                                                             
DATE2    MVC   PDATA+18(2),=C'19'                                               
         CLI   PDATA+20,C'9'                                                    
         BNH   DATE3                                                            
         MVC   PDATA+18(2),=C'20'                                               
         LLC   R0,PDATA+20                                                      
         AHI   R0,-10               R0=20TH CENTUARY DECADE                     
         STC   R0,PDATA+20                                                      
DATE3    TM    4(R1),X'40'                                                      
         BZ    DATE3X                                                           
         MVI   PDATA+17,C'*'                                                    
DATE3X   EQU   *                                                                
*                                                                               
         MVI   PDATA+28,C'A'                                                    
         L     RF,=V(ADDAY)                                                     
         GOTO1 (RF),PLIST,(C'D',DATEIN),(X'00',PDATA+30),F'0'                   
         GOTO1 (RF),PLIST,(C'D',DATEIN),(X'00',PDATA+40),F'-1'                  
         GOTO1 (RF),PLIST,(C'D',DATEIN),(X'00',PDATA+50),F'+1'                  
         GOTO1 (RF),PLIST,(C'M',DATEIN),(X'00',PDATA+60),F'-1'                  
         GOTO1 (RF),PLIST,(C'M',DATEIN),(X'00',PDATA+70),F'+1'                  
         GOTO1 (RF),PLIST,(C'Y',DATEIN),(X'00',PDATA+80),F'-1'                  
         GOTO1 (RF),PLIST,(C'Y',DATEIN),(X'00',PDATA+90),F'+1'                  
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA(30),SPACES                                                 
*                                                                               
         MVI   PDATA+28,C'D'                                                    
         L     RF,=V(DATCON)                                                    
         GOTO1 (RF),PLIST,(X'30',DATEIN),(0,PDATA+30),(06,0),F'0'               
         GOTO1 (RF),PLIST,(X'30',DATEIN),(0,PDATA+40),(06,0),F'-1'              
         GOTO1 (RF),PLIST,(X'30',DATEIN),(0,PDATA+50),(06,0),F'+1'              
         GOTO1 (RF),PLIST,(X'30',DATEIN),(0,PDATA+60),(07,0),F'-1'              
         GOTO1 (RF),PLIST,(X'30',DATEIN),(0,PDATA+70),(07,0),F'+1'              
         GOTO1 (RF),PLIST,(X'30',DATEIN),(0,PDATA+80),(10,0),F'-1'              
         GOTO1 (RF),PLIST,(X'30',DATEIN),(0,PDATA+90),(10,0),F'+1'              
         B     PRNT                                                             
         EJECT                                                                  
***********************************************************************         
*CARD FORMAT AS FOLLOWS                                               *         
*TIME=XX....... WHERE XX...... IS ANY VALID INPUT TIME PERIOD         *         
***********************************************************************         
TIME     LA    RF,PDATA+5                                                       
         LA    R0,15                                                            
TIME1    CLI   0(RF),C' '                                                       
         BE    TIME2                                                            
         LA    RF,1(RF)                                                         
         BCT   R0,TIME1                                                         
         LA    R0,15                                                            
         B     TIME3                                                            
TIME2    LA    RE,PDATA+5                                                       
         SR    RF,RE                                                            
         LR    R0,RF               R0=L'INPUT                                   
*                                                                               
TIME3    XC    FULL,FULL                                                        
         GOTO1 =V(TIMVAL),DMCB,((R0),PDATA+5),FULL                              
         GOTO1 =V(HEXOUT),PLIST,DMCB,PDATA+30,1,=C'TOG'                         
         GOTO1 =V(HEXOUT),PLIST,FULL,PDATA+32,4,=C'TOG'                         
         XC    FULL,FULL                                                        
         GOTO1 =V(TIMBER),DMCB,(X'90',(R0)),FULL,PDATA+5                        
         GOTO1 =V(HEXOUT),PLIST,DMCB,PDATA+44,1,=C'TOG'                         
         GOTO1 =V(HEXOUT),PLIST,FULL,PDATA+46,4,=C'TOG'                         
         B     PRNT                                                             
         EJECT                                                                  
***********************************************************************         
*CARD FORMAT AS FOLLOWS                                               *         
*CALC=NNNNN!NNNNN WHERE ! IS + OR - OR * OR /                         *         
***********************************************************************         
CALC     PACK  DUB,PDATA+5(5)                                                   
         CVB   R1,DUB                                                           
         ZAP   NUM1,DUB                                                         
         PACK  DUB,PDATA+11(5)                                                  
         CVB   RF,DUB                                                           
         ZAP   NUM2,DUB                                                         
         MVC   OPERATOR,PDATA+10                                                
*                                                                               
CALCWHAT CLI   OPERATOR,C'+'       TEST WHAT CALCULATION                        
         BE    CALCA                                                            
         CLI   OPERATOR,C'-'                                                    
         BE    CALCS                                                            
         CLI   OPERATOR,C'*'                                                    
         BE    CALCM                                                            
         CLI   OPERATOR,C'/'                                                    
         BE    CALCD                                                            
         B     PRNT                                                             
*                                                                               
CALCA    AR    R1,RF                                                            
         CVD   R1,ANSBIN                                                        
         ZAP   ANSWER,NUM1                                                      
         AP    ANSWER,NUM2                                                      
         B     CALCOUT                                                          
*                                                                               
CALCS    SR    R1,RF                                                            
         CVD   R1,ANSBIN                                                        
         ZAP   ANSWER,NUM1                                                      
         SP    ANSWER,NUM2                                                      
         B     CALCOUT                                                          
*                                                                               
CALCM    MR    R0,RF                                                            
         CVD   R1,ANSBIN                                                        
         ZAP   ANSWER,NUM1                                                      
         MP    ANSWER,NUM2                                                      
         B     CALCOUT                                                          
*                                                                               
CALCD    SR    R0,R0                                                            
         DR    R0,RF                                                            
         CVD   R1,ANSBIN                                                        
         ZAP   DUB,NUM1                                                         
         DP    DUB,NUM2                                                         
         ZAP   ANSWER,DUB(4)                                                    
         B     CALCOUT                                                          
*                                                                               
CALCOUT  MVI   PDATA+16,C'='                                                    
         MVI   PDATA+17,C'+'                                                    
         CP    ANSBIN,=P'0'                                                     
         BNM   *+8                                                              
         MVI   PDATA+17,C'-'                                                    
         MVI   PDATA+26,C'+'                                                    
         CP    ANSWER,=P'0'                                                     
         BNM   *+8                                                              
         MVI   PDATA+26,C'-'                                                    
         OI    ANSBIN+7,X'0F'                                                   
         OI    ANSWER+7,X'0F'                                                   
         UNPK  PDATA+18(8),ANSBIN                                               
         UNPK  PDATA+27(8),ANSWER                                               
         B     PRNT                                                             
         EJECT                                                                  
***********************************************************************         
*CARD FORMAT AS FOLLOWS                                               *         
*SUMS= NNNNNNNN ! NNNNNNNN                                            *         
*ALLOW SPACES BETWEEN FIELDS AND COMMAS IN NUMBERS                    *         
***********************************************************************         
SUMS     BAS   RE,CLEANIT          REMOVE ALL REDUNDANT CHRS                    
*                                                                               
         LA    R2,PDATA+5          R2=A(FIRST CHR)                              
         LR    R1,R2               R1=A(NEXT CHR)                               
         LA    R0,9                R0=MAX LEM OF NUMERIC FIELD (PLUS 1)         
SUMS1    CLI   0(R1),C'9'          TEST NUMERIC                                 
         BH    ERROR                                                            
         CLI   0(R1),C'0'                                                       
         BL    SUMS3                                                            
SUMS2    LA    R1,1(R1)            ON TO NEXT CHR IN STRING                     
         BCT   R0,SUMS1                                                         
         B     ERROR               NUMERIC FIELD TOO LONG                       
SUMS3    CLI   0(R1),C'+'                                                       
         BE    SUMS4                                                            
         CLI   0(R1),C'-'                                                       
         BE    SUMS4                                                            
         CLI   0(R1),C'*'                                                       
         BE    SUMS4                                                            
         CLI   0(R1),C'/'                                                       
         BE    SUMS4                                                            
         B     ERROR                                                            
SUMS4    ST    R1,AOPER            SAVE A(OPPERATOR CHR)                        
         MVC   OPERATOR,0(R1)                                                   
         SR    R1,R2               R1=LEN OF NUM FIELD                          
         BNP   ERROR                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         ZAP   NUM1,DUB                                                         
*                                                                               
         L     R2,AOPER            R2=A(FIRST CHR OF SECOND FIELD)              
         LA    R2,1(R2)                                                         
         LR    R1,R2               R1=A(NEXT CHR)                               
         LA    R0,9                R0=MAX LEM OF NUMERIC FIELD (PLUS 1)         
SUMS5    CLI   0(R1),C'9'          TEST NUMERIC                                 
         BH    ERROR                                                            
         CLI   0(R1),C'0'                                                       
         BL    SUMS7                                                            
SUMS6    LA    R1,1(R1)            ON TO NEXT CHR IN STRING                     
         BCT   R0,SUMS5                                                         
         B     ERROR               NUMERIC FIELD TOO LONG                       
SUMS7    CLI   0(R1),C' '                                                       
         BNE   ERROR                                                            
         SR    R1,R2               R1=LEN OF NUM FIELD                          
         BNP   ERROR                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         ZAP   NUM2,DUB                                                         
*                                                                               
SUMS8    ZAP   DUB,NUM1            SET UP TO LOOK LIKE CALC                     
         CVB   R1,DUB                                                           
         ZAP   DUB,NUM2                                                         
         CVB   RF,DUB                                                           
         B     CALCWHAT                                                         
*                                                                               
CLEANIT  ST    RE,SAVERE           CLEAN UP ALL WHITE SPACE CHRS                
         MVI   CLEANED,C' '                                                     
         MVC   CLEANED+1(L'CLEANED-1),CLEANED                                   
         LA    R2,PDATA            R2=A(NEXT INPUT CHARACTER)                   
         LA    R3,CLEANED          R3=A(NEXT CLEANED OUTPUT CHR)                
         LA    R0,L'CLEANED        R0=MAX LEM OF OUTPUT DATA                    
CLEAN1   CLI   0(R2),C' '                                                       
         BE    CLEAN2                                                           
         CLI   0(R2),C','                                                       
         BE    CLEAN2                                                           
         MVC   0(1,R3),0(R2)                                                    
         LA    R3,1(R3)                                                         
CLEAN2   LA    R2,1(R2)                                                         
         BCT   R0,CLEAN1                                                        
         MVC   PDATA(L'CLEANED),CLEANED                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*DUMP OPTIONS                                                         *         
***********************************************************************         
DUMP     LA    R3,TIOT                                                          
         EXTRACT (3),'S',FIELDS=TIOT                                            
         L     R3,TIOT                                                          
         AHI   R3,24               R3=A(TIOT TABLE)                             
         MVC   DUMPDD,SPACES                                                    
         MVI   ERRXITF,0                                                        
*                                                                               
DUMP1    CLI   0(R3),0             TEST END OF TIOT TABLE                       
         BE    DUMP3                                                            
         CLC   4(8,R3),=C'SYSMDUMP'                                             
         BE    DUMP2                                                            
         CLC   4(8,R3),=C'SYSUDUMP'                                             
         BE    DUMP2                                                            
         LLC   R0,0(R3)            BUMP TO NEXT DD CARD ENTRY                   
         AR    R3,R0                                                            
         B     DUMP1                                                            
DUMP2    MVC   DUMPDD,4(R3)        EXTRACT DUMP FILE DDNAME                     
*                                                                               
DUMP3    MVC   PDATA+08(8),DUMPDD                                               
         GOTO1 =V(PRINT),PLMDP,PLINE,=C'BL01'                                   
*                                                                               
DUMP4    CLI   PDATA+7,C'S'        DUMP=**S TO SIMULATE SOON DUMP               
         BNE   DUMP4A                                                           
         LT    RE,=V(PDMPSOON)                                                  
         BZ    DUMP5                                                            
         L     RF,=A(MASTC)        MOVE DATA FROM MASTC TO V(PDMPSOON)          
         USING MASTD,RF                                                         
         MVC   STUSERID,MCUSERID                                                
         MVC   STPROG,MCPROG                                                    
         MVC   STREPID,=C'RPC'                                                  
         MVC   STREPNO,=H'123'                                                  
         MVC   STDESTID,MCDESTID                                                
         MVC   STOVSYS,MCOVSYS                                                  
         MVC   STOVAGYA,MCUSER                                                  
         MVC   0(32,RE),STXDATA                                                 
         MVC   0(32,RE),STXDATA1   *DEBUG*                                      
         B     DUMP5                                                            
DUMP4A   CLI   PDATA+7,C'C'        DUMP=**C TO SIMULATE CONTROL SYSTEM          
         BNE   DUMP5                                                            
         LT    RE,=V(PDMPSOON)                                                  
         BZ    DUMP5                                                            
         XC    0(32,RE),0(RE)                                                   
         MVC   13(2,RE),=C'PP'     SET PDMPPRG TO PP                            
         MVI   21(RE),X'0A'        SET PDMPSYS TO CONTROL SYSTEM                
         DROP  RF                                                               
*                                                                               
DUMP5    CLI   PDATA+5,C'M'        DUMP=M TO USE MDUMPER                        
         BE    DUMPM                                                            
         CLI   PDATA+5,C'P'        DUMP=P TO USE PDUMPER                        
         BE    DUMPP                                                            
         CLI   PDATA+5,C'S'        DUMP=S TO USE STXITER                        
         BE    DUMPS                                                            
         B     ERROR                                                            
*                                                                               
DUMPM    LT    RF,=V(MDUMPER)      MDUMPER                                      
         BZ    DUMPDCH0                                                         
*                                                                               
DUMPM2   XC    PLMDP(24),PLMDP     SET MDUMPER PLIST FOR INIT                   
         LA    RF,=CL8'INIT'                                                    
         ST    RF,PLMDP+0                                                       
         OI    PLMDP,X'80'         SET RETURN DSN IN P4                         
*                                                                               
         MVC   PDATA+08(8),=CL8'CALLINIT'                                       
         SR    R5,R5                                                            
         ICM   R5,7,PLMDP+1        R5=A(ACTION) IN PARAM1                       
         BZ    *+10                                                             
         MVC   PDATA+17(8),0(R5)                                                
         GOTO1 =V(HEXOUT),DMCB,PLMDP+0,PDATA+31,4,=C'TOG'                       
         GOTO1 =V(HEXOUT),DMCB,PLMDP+4,PDATA+40,4,=C'TOG'                       
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA+17(8),SPACES                                               
         MVC   PDATA+31(17),SPACES                                              
*                                                                               
         GOTO1 =V(MDUMPER),PLMDP                                                
         SAM24                                                                  
         SAC   0                                                                
*                                                                               
         MVC   PDATA+08(8),=CL8'RETNINIT'                                       
         SR    R5,R5                                                            
         ICM   R5,7,PLMDP+1        R5=A(ACTION) IN PARAM1                       
         BZ    *+10                                                             
         MVC   PDATA+17(8),0(R5)                                                
         SR    R5,R5                                                            
         ICM   R5,7,PLMDP+5        R5=RETURN COUNT IN PARAM2                    
         GOTO1 =V(HEXOUT),DMCB,PLMDP+0,PDATA+31,4,=C'TOG'                       
         GOTO1 =V(HEXOUT),DMCB,PLMDP+4,PDATA+40,4,=C'TOG'                       
         LT    RF,PLMDP+12                                                      
         BZ    *+10                                                             
         MVC   PDATA+50(44),0(RF)                                               
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA+17(8),SPACES                                               
         MVC   PDATA+31(17),SPACES                                              
         MVC   PDATA+50(44),SPACES                                              
         LTR   R5,R5                                                            
         BNZ   EXIT                EXIT IF RETURN FROM SUICIDE                  
*                                                                               
ERRXIT1  TM    ERRXITF,X'01'                                                    
         BZ    ERRXIT2                                                          
         MVC   PDATA+10(8),=CL8'REBORN  '                                       
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         B     LOOP                                                             
*                                                                               
ERRXIT2  MVC   PDATA+08(8),=CL8'SUICIDE '                                       
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
*                                                                               
         DC    H'0'                                                             
*                                                                               
         MVC   PDATA+08(8),=CL8'RETNSUIC'                                       
         SR    R5,R5                                                            
         ICM   R5,7,PLMDP+1                                                     
         BZ    *+10                                                             
         MVC   PDATA+17(8),0(R5)                                                
         GOTO1 =V(HEXOUT),DMCB,PLMDP+0,PDATA+31,4,=C'TOG'                       
         GOTO1 =V(HEXOUT),DMCB,PLMDP+4,PDATA+40,4,=C'TOG'                       
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA+17(8),SPACES                                               
         MVC   PDATA+31(17),SPACES                                              
*                                                                               
         TM    ERRXITF,X'01'                                                    
         BZ    LOOP                                                             
         MVC   PDATA+10(8),=CL8'MDUMPERX'                                       
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         B     LOOP                                                             
*                                                                               
DUMPDCH0 DC    H'0'                                                             
*                                                                               
DUMPP    LT    RF,=V(PDUMPER)      PDUMPER                                      
         BZ    DUMPDCH0                                                         
         L     R5,=A(PSWREGS)      BUILD A DUMMY PSW                            
         LR    R0,RB                                                            
         AHI   R0,1024                                                          
         ST    R0,4(R5)                                                         
         AHI   R0,7168                                                          
         STM   R0,RF,8(R5)                                                      
*                                                                               
         GOTO1 =V(PDUMPER),PLMDP,(X'80',(R5)),(RB),(R0),0                       
         LT    RF,PLMDP+12                                                      
         BZ    LOOP                                                             
         MVC   PDATA+50(44),0(RF)                                               
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA+50(44),SPACES                                              
         B     LOOP                                                             
*                                                                               
DUMPS    LT    RF,=V(STXITER)      DUMP=S FOR STXITER TO CALL PDUMPER           
         BZ    DUMPDCH0                                                         
         MVC   STXITYPE,PDATA+6    DUMP=SM FOR STXITER TO CALL MDUMPER          
         L     RE,=A(MASTC)                                                     
         MVC   1(150,RE),PDATA+6   SET DUMP TYPE IN MCDUMP IN V(MASTC)          
         XC    PLMDP(24),PLMDP                                                  
         XC    STXLST,STXLST                                                    
         LA    RF,STXLST                                                        
         L     RE,=A(WORK)         FIRST 1K OF WORK                             
         AHI   RE,-8                                                            
         ST    RE,0(RF)                                                         
         AHI   RE,1024                                                          
         ST    RE,4(RF)                                                         
         LA    RF,8(RF)                                                         
         L     RE,=A(WORKHALF)     MIDDLE 1K OF WORK                            
         AHI   RE,-8                                                            
         ST    RE,0(RF)                                                         
         AHI   RE,1024                                                          
         ST    RE,4(RF)                                                         
         OI    4(RF),X'80'         SET END OF LIST                              
         LA    RF,STXLST                                                        
         ST    RF,PLMDP+0                                                       
*                                                                               
         MVC   PDATA+08(8),=CL8'CALLSTXI'                                       
         GOTO1 =V(HEXOUT),DMCB,PLMDP+0,PDATA+31,4,=C'TOG'                       
         GOTO1 =V(HEXOUT),DMCB,PLMDP+4,PDATA+40,4,=C'TOG'                       
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA+31(17),SPACES                                              
*                                                                               
         GOTO1 =V(STXITER),PLMDP                                                
*                                                                               
         MVC   PDATA+08(8),=CL8'RETNSTXI'                                       
         GOTO1 =V(HEXOUT),DMCB,PLMDP+0,PDATA+31,4,=C'TOG'                       
         GOTO1 =V(HEXOUT),DMCB,PLMDP+4,PDATA+40,4,=C'TOG'                       
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
         MVC   PDATA+31(17),SPACES                                              
*                                                                               
         MVC   PDATA+08(8),=CL8'SUICIDE '                                       
         GOTO1 =V(PRINT),DMCB,PLINE,=C'BL01'                                    
*                                                                               
         DC    H'0'                                                             
         B     EXIT                                                             
*                                                                               
         DS    0D                                                               
         DROP  RB                                                               
         USING *,RB                                                             
ERRXIT   LR    RB,RF               ESTABLISH BASE                               
         L     R2,0(R1)                                                         
         USING SDWA,R2             R2=SDWA                                      
         ST    R1,MYR1                                                          
         ST    RE,MYRE                                                          
         OI    ERRXITF,X'01'                                                    
*                                                                               
         ICM   R1,15,SDWAGR11      FIND ABENDING RB                             
         JZ    *+2                                                              
*                                                                               
         CLC   22(8,R1),=C'**DMRC**' DIED IN DMRC - END JOB                     
         JE    ERREOJ                                                           
         CLC   SDWAABCC+2(2),=H'990' U990 JUST MESSAGE                          
         JNE   ERRRET                                                           
*                                                                               
         XC    MLINE,MLINE         SEND ALLOCATION MESSAGE                      
         MVC   MLINE(2),=X'0020'                                                
         MVC   MLINE+2(L'MESS990),MESS990                                       
         XR    RF,RF                                                            
         ICM   RF,15,SDWAGR02                                                   
         MVC   MLINE+22(8),22(RF)                                               
*                                                                               
         WTO   TEXT=MLINE,MCSFLAG=HRDCPY                                        
         L     R1,MYR1                                                          
         LA    RF,=C'NODUMP'       IF ALLOC ERR RETURN NODUMP                   
         ST    RF,4(R1)                                                         
         J     ERRRET                                                           
*                                                                               
ERREOJ   L     R1,MYR1             IF ERREOJ RETURN ENDJOB                      
         LA    RF,=C'ENDJOB'                                                    
         ST    RF,4(R1)                                                         
*                                                                               
ERRRET   L     R1,MYR1             RESTORE R1 AND RE                            
         L     RE,MYRE                                                          
         BR    RE                                                               
*                                                                               
MYR1     DC    A(0)                SAVED R1 AND RE                              
MYRE     DC    A(0)                                                             
MLINE    DC    CL64' '                                                          
MESS990  DC    C'ALLOCATION ERROR ON XXXXXXXX    '                              
ERRXITF  DC    XL4'00'                                                          
         EJECT                                                                  
         LTORG                                                                  
SPACES   DC    CL132' '                                                         
COUNTRY  DS    0X                                                               
*&&UK*&& DC    AL1(1)                                                           
*&&US*&& DC    AL1(2)                                                           
*                                                                               
DUB      DS    D                                                                
TIOT     DS    D                                                                
DUMPDD   DS    CL8                                                              
         DS    CL6                                                              
PVOPT1   DS    X                                                                
PVOPT2   DS    X                                                                
PERVEYE  DS    CL8                                                              
PERVBLK  DS    XL80                                                             
PERVBLKX DS    XL80                                                             
PERVWRK  DS    XL48                                                             
         DS    0D                                                               
ANSBIN   DS    PL8                                                              
FULL     DS    F                                                                
HALF     DS    H                                                                
DATEIN   DS    CL6                                                              
         DS    CL2                                                              
DATEINR  DS    CL6                                                              
*                                                                               
DATEPACK DS    PL4                 RETURNED FROM TYPE 5 CALL                    
DATESECS DS    XL4                                                              
*                                                                               
         DS    0D                                                               
DATETIME DC    XL16'00'                                                         
DATEMID  DS    CL12                                                             
DATEOUT  DS    CL12                                                             
DATEMUL  DS    CL16                                                             
NUM1     DS    PL5                                                              
NUM2     DS    PL5                                                              
ANSWER   DS    PL10                                                             
PLIST    DS    6F                                                               
PLISTX   DS    6F                                                               
PLMDP    DC    6F'0'                                                            
SAVERB   DS    F                                                                
STXLST   DS    XL20                                                             
DMCB     DS    6F                                                               
AOPER    DS    A                                                                
SAVERE   DS    A                                                                
OPERATOR DS    CL1                                                              
CLEANED  DS    CL80                                                             
*                                                                               
         DS    0D                                                               
         DC    C'PSWREGS '                                                      
PSWREGS  DC    D'0',16F'0'                                                      
*                                                                               
STXDATA  DC    XL32'00'            DATA FOR V(PDMPSOON)                         
         ORG   STXDATA                                                          
STUSERID DS    CL8                 FIRST 8 CHRS OF ALPHA USER ID                
STREPID  DS    CL3                 PQ REPORT ID                                 
STREPNO  DS    XL2                 PQ REPORT NUMBER                             
STPROG   DS    CL2                 PROGRAM ID                                   
STCC     DS    XL3                 COMPLETION CODE X'SSSUUU'                    
STDESTID DS    XL2                 USER ID NUMBER                               
STSENUM  DS    XL1                 SYSTEM SE NUMBER                             
STOVSYS  DS    XL1                 OVERLAY SYSTEM ID                            
STOVAGYA DS    CL2                 AGENCY ALPHA                                 
         DS    XL8                 N/D                                          
*                                                                               
STXDATA1 DS    0CL32                                                            
         DC    CL8'USERNAME'                                                    
         DC    CL3'RPC'                                                         
         DC    AL2(12345)                                                       
         DC    CL2'PP'                                                          
         DC    XL3'001002'                                                      
         DC    XL2'0011'                                                        
         DC    XL1'0A'                                                          
         DC    XL1'0A'                                                          
         DC    CL2'AA'                                                          
         DC    XL8'00'                                                          
*                                                                               
PLINE    DC    XL1'00'                                                          
PDATA    DC    CL132' '                                                         
*                                                                               
TABLE    DS    0CL(TABLELQ)                                                     
         DC    CL8'FIRST   ',X'01'                                              
         DC    CL8'SECOND  ',X'02'                                              
         DC    CL8'THIRD   ',X'03'                                              
         DC    CL8'FOURTH  ',X'04'                                              
         DC    CL8'FIFTH   ',X'05'                                              
         DC    CL8'SIXTH   ',X'06'                                              
TABLEX   DC    CL8'        ',X'00'                                              
TABLELQ  EQU   *-TABLEX                                                         
*                                                                               
         DS    0D                                                               
         DC    CL8'STXITYPE'                                                    
STXITYPE DC    C' '                SET TO C'M' FOR MDUMPER                      
         DC    CL7' '                                                           
         DS    0D                                                               
         DC    CL8'*SSBSSB*'                                                    
SSB      DC    X'0000',X'FF',X'00',X'00'                                        
         DC    X'000080'           EXTENDED UTL                                 
         DC    CL8' '                                                           
         DC    A(0),A(0),A(0),A(0),A(0),A(0)                                    
         DC    XL5'00'                                                          
         DC    C'T'                SSODSPAC                                     
         DC    XL2'00'                                                          
         DC    A(MASTC)            SSOMASTC A(MASTC)                            
         DC    XL12'00'                                                         
         DC    XL192'00'                                                        
*                                                                               
         DC    CL8'*UTLUTL*'                                                    
UTL      DC    F'0'                                                             
         DC    X'0A'               SE NUM                                       
         DC    XL3'00',127F'0'                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'*MASTC**'                                                    
MASTC    DC    8192X'00'                                                        
MASTCX   EQU   *                                                                
         ORG   MASTC+(MCDUMP-MASTD)                                             
         DC    C'M'                                                             
         ORG   MASTC+(MCUSERID-MASTD)                                           
         DC    C'SJR     '                                                      
         ORG   MASTC+(MCPROG-MASTD)                                             
         DC    C'40'                                                            
         ORG   MASTC+(MCDESTID-MASTD)                                           
         DC    AL2(17)                                                          
         ORG   MASTC+(MCOVSYS-MASTD)                                            
         DC    X'0A'                                                            
         ORG   MASTC+(MCUSER-MASTD)                                             
         DC    C'AA'                                                            
         ORG   MASTCX                                                           
*                                                                               
         DS    0D                                                               
         DC    C'WKWKWKWK'                                                      
WORK     DC    2000D'0'                                                         
         DC    C'HALFWORK'                                                      
WORKHALF DC    2000D'0'                                                         
WORKX    EQU   *                                                                
*                                                                               
*IHASDWA MACRO                                                                  
         IHASDWA GR32=YES                                                       
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FASSB                                                          
         ORG  SSBD                                                              
       ++INCLUDE FASSBOFF                                                       
         ORG                                                                    
       ++INCLUDE DDMASTD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DDTEST    11/02/17'                                      
         END                                                                    
