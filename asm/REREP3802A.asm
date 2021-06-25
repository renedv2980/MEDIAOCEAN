*          DATA SET REREP3802A AT LEVEL 038 AS OF 05/01/02                      
*PHASE RE3802A,*                                                                
***********************************************************************         
* HISTORY OF CHANGES                                                  *         
***********************************************************************         
* NOV19/91 (BU ) --- ADJUST FOR USE OF VALUENEW FACILITY              *         
*                                                                     *         
* MAR27/92 (BU ) --- MAKE COMPATIBLE WITH VALU2NEW                    *         
*                    REREPRGEQU ---> REREPRGEQA                       *         
*                                                                     *         
* OCT04/95 (SKU) --- REINITIALIZE ONEFLAG                             *         
*                                                                     *         
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                  *         
*                                                                     *         
* MAY22/97 (BU ) --- UPGRADE FOR YR 2000                              *         
*                                                                   * *         
* JAN23/98 (JRD) --- 4K CONTRACTS                                   * *         
*                                                                   * *         
* APR01/99 (BU ) --- DISPLAY DATA OPTION                            * *         
*                                                                   * *         
*                                                                   * *         
*                    *** END TOMBSTONE  ***                           *         
***********************************************************************         
         TITLE 'WEEKLY SALES REPORT'                                            
RE3802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE3802                                                       
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         LA    R9,MYSPACE                                                       
         USING MYD,R9                                                           
         EJECT                                                                  
*              FIRST FOR REQUEST CONTROL - SET UP VALUES                        
         SPACE 3                                                                
         CLI   MODE,REQFRST                                                     
         BNE   WR30                                                             
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*                                     SPECIAL YR 2000 FORMAT                    
         MVC   MYH6(111),SPACES                                                 
         MVI   RCSUBPRG,0                                                       
         XC    ACCUMS(240),ACCUMS                                               
         XC    ACCUMS+240(240),ACCUMS+240                                       
         MVC   MYH8+1(219),MYH8                                                 
         MVI   TYPE,1                                                           
         CLI   QSEQ,C'M'                                                        
         BE    WR2                                                              
         MVI   TYPE,4                                                           
         CLC   QSEQ(2),=C'S '                                                   
         BE    WR2                                                              
         MVI   TYPE,5                                                           
         CLC   QSEQ(2),=C'SS'                                                   
         BE    WR2                                                              
         MVI   TYPE,2                                                           
         CLI   QSEQ+1,C' '                                                      
         BE    WR2                                                              
         MVI   TYPE,3                                                           
         SPACE 2                                                                
WR2      MVC   MYPER(20),SPACES                                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(8,MYPER)                                 
         MVI   MYPER+9,C'-'                                                     
         GOTO1 DATCON,DMCB,(0,QEND),(8,MYPER+11)                                
         CLI   TYPE,2                                                           
         BL    WR4                                                              
         BE    WR6                                                              
         CLI   TYPE,4                                                           
         BL    WR8                                                              
         BE    WR10                                                             
         BH    WR12                                                             
         SPACE                                                                  
WR4      MVC   MYH6+1(6),=C'OFFICE'                                             
         MVC   MYH6+87(21),=C'SALESPERSON BY OFFICE'                            
         MVC   MYH8+1(11),=C'SALESPERSON'                                       
         MVC   MYH9+1(11),=20C'-'                                               
         B     WR20                                                             
         SPACE 2                                                                
WR6      MVC   MYH6+1(6),=C'OFFICE'                                             
         MVC   MYH6+87(17),=C'STATION BY OFFICE'                                
         MVC   MYH8+1(7),=C'STATION'                                            
         MVC   MYH9+1(7),=20C'-'                                                
         B     WR20                                                             
         SPACE 2                                                                
WR8      MVC   MYH6+53(12),=C'OFFICE RECAP'                                     
         MVC   MYH8+1(6),=C'OFFICE'                                             
         MVC   MYH9+1(6),=20C'-'                                                
         B     WR20                                                             
         SPACE 2                                                                
WR10     MVC   MYH6+1(7),=C'STATION'                                            
         MVC   MYH6+87(17),=C'OFFICE BY STATION'                                
         MVC   MYH8+1(6),=C'OFFICE'                                             
         MVC   MYH9+1(6),=20C'-'                                                
         B     WR20                                                             
         SPACE 2                                                                
WR12     MVC   MYH6+52(13),=C'STATION RECAP'                                    
         MVC   MYH8+1(7),=C'STATION'                                            
         MVC   MYH9+1(7),=20C'-'                                                
         SPACE 2                                                                
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
*                                                                               
WR20     EQU   *                                                                
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     WREXT                                                            
         EJECT                                                                  
*              OTHER FIRST TIME ROUTINES                                        
         SPACE 3                                                                
WR30     CLI   MODE,OFFFRST                                                     
         BNE   WR34                                                             
         CLI   TYPE,2                                                           
         BH    WR32                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   MYH6+8(20),ROFFNAME                                              
         B     WREXT                                                            
         SPACE 2                                                                
WR32     CLI   TYPE,4                                                           
         BH    WREXT                                                            
         MVC   P+1(20),ROFFNAME                                                 
         B     WREXT                                                            
         SPACE 2                                                                
WR34     CLI   MODE,STAFRST                                                     
         BNE   WR40                                                             
         MVC   WORK(4),RSTAKSTA                                                 
         MVC   WORK+4(3),=C'-TV'                                                
         CLI   RSTAKSTA+4,C' '                                                  
         BE    WR36                                                             
         MVC   WORK+5(1),RSTAKSTA+4                                             
         MVI   WORK+6,C'M'                                                      
         SPACE 2                                                                
WR36     CLI   TYPE,2                                                           
         BE    WR38                                                             
         CLI   TYPE,5                                                           
         BE    WR38                                                             
         CLI   TYPE,4                                                           
         BNE   WREXT                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   MYH6+9(7),WORK                                                   
         B     WREXT                                                            
         SPACE 2                                                                
WR38     MVC   P+1(7),WORK                                                      
         B     WREXT                                                            
         SPACE 2                                                                
WR40     CLI   MODE,MANFRST                                                     
         BNE   WR41                                                             
         CLI   TYPE,1                                                           
         BNE   WREXT                                                            
         MVC   P+1(20),RSALNAME                                                 
         B     WREXT                                                            
         SPACE 2                                                                
WR41     CLI   MODE,TEAMFRST                                                    
         BNE   WR42                                                             
         CLI   TYPE,1                                                           
         BNE   WREXT                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   WORK,SPACES                                                      
         MVC   WORK(9),=C'DIVISION-'                                            
         MVC   WORK+9(10),RTEMDVNM                                              
         CLI   RTEMKTEM+1,C' '                                                  
         BE    WR41A                                                            
         LA    R1,WORK+19                                                       
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,3(R1)                                                         
         MVC   0(5,R1),=C'TEAM-'                                                
         MVC   5(10,R1),RTEMNAME                                                
WR41A    MVC   MYH6+50(36),WORK                                                 
         B     WREXT                                                            
         EJECT                                                                  
*              POST VALUES FOR CONTRACT                                         
         SPACE 3                                                                
WR42     EQU   *                                                                
         CLI   MODE,PROCCONT                                                    
         BNE   WR50                                                             
         CLI   ONEFLAG,C'Y'        FIRST-TIME FLAG SET?                         
         BE    WR420002            YES                                          
         MVI   ONEFLAG,C'Y'        NO  - SET IT                                 
         BAS   RE,SETMONS          SET MONTH INFO                               
WR420002 EQU   *                                                                
         L     R2,AANEWMON                                                      
         LA    R3,ACCUMS                                                        
         LA    R4,4                                                             
         MVI   PRTFLG,C'Y'                                                      
         SPACE 2                                                                
WR43     BAS   RE,ADDBLOCK                                                      
         LA    R3,120(R3)                                                       
         BCT   R4,WR43                                                          
         B     WREXT                                                            
         SPACE 2                                                                
ADDBLOCK NTR1                                                                   
         LA    R4,4                                                             
         SPACE 2                                                                
WR44     BAS   RE,ADDCOL           FOUR DISCRETE MONTHLY COLUMNS                
         LA    R2,NEXTBUCK(R2)                                                  
         LA    R3,4(R3)                                                         
         BCT   R4,WR44                                                          
         SPACE 2                                                                
         LA    R4,20                                                            
         SPACE 2                                                                
WR46     BAS   RE,ADDCOL           THE OTHERS GO INTO COLUMN 5                  
         LA    R2,NEXTBUCK(R2)                                                  
         BCT   R4,WR46                                                          
         SPACE 2                                                                
         L     R2,AANEWMON                                                      
         LA    R3,4(R3)                                                         
         LA    R4,24                                                            
         SPACE 2                                                                
WR48     BAS   RE,ADDCOL           ADD ALL INTO COLUMN 6                        
         LA    R2,NEXTBUCK(R2)                                                  
         BCT   R4,WR48                                                          
         B     XIT                                                              
         SPACE 2                                                                
ADDCOL   DS    0H             CURRENT SALES                                     
         LR    R6,R2               SET A(BUCKETS WITHIN MONTH)                  
         LA    R6,BUCKDISP(R6)                                                  
*                                                                               
         CLI   PRTFLG,C'Y'         PRINT IT?                                    
         BNE   ADCO0020            NO                                           
*                                  YES                                          
         CLC   =C'PRINTIT',QUESTOR PRINT CONTRACT INFO?                         
         BNE   ADCO0020            NO                                           
         C     R4,=F'4'            NTH MONTH? (BACKWARD: ON BCT)                
         BNE   ADCO0020            NO                                           
         ST    RE,SAVER14          SAVE REGISTER RE                             
         BAS   RE,PRNTINFO         YES                                          
         L     RE,SAVER14          RELOAD REGISTER RE                           
         MVI   PRTFLG,C'N'         TURN OFF PRINT FLAG                          
         B     ADCO0020                                                         
PRTFLG   DS    CL1                                                              
SAVER14  DS    F                                                                
TESTACC  DS    F                                                                
         EJECT                                                                  
PRNTINFO NTR1                                                                   
         OC    GROSSORD(4,R6),GROSSORD(R6)                                      
         BNZ   PINF0020                                                         
         OC    TOTORD(4,R6),TOTORD(R6)                                          
         BNZ   PINF0020                                                         
         OC    CUASATIN(4,R6),CUASATIN(R6)                                      
         BZ    PINF0040                                                         
PINF0020 EQU   *                                                                
         XC    P,P                 CLEAR PRINT LINE                             
         GOTO1 HEXOUT,DMCB,RCONKCON,P+1,4,=C'TOG'                               
         MVC   P+12(6),0(R2)                                                    
         L     R5,GROSSORD(R6)     GET CURR ORD ACTIV AMT                       
         MVC   P+21(08),=C'CURR ORD'                                            
         EDIT  (R5),(10,P+32),MINUS=YES                                         
         L     R5,TOTORD(R6)       GET CURR AS AT ORD AMT                       
         MVC   P+45(08),=C'CURR INV'                                            
         TM    FLAG6(R2),X'01'     TEST FOR ANY CURR INV DATA                   
         BZ    *+8                 IF ANY                                       
         L     R5,CUASATIN(R6)     GET CURR AS AT INV AMT                       
         EDIT  (R5),(10,P+56),MINUS=YES                                         
         L     RF,TESTACC                                                       
         AR    RF,R5                                                            
         ST    RF,TESTACC                                                       
         EDIT  TESTACC,(10,P+70),MINUS=YES                                      
         GOTO1 REPORT                                                           
PINF0040 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
ADCO0020 EQU   *                                                                
*                                                                               
         L     R5,GROSSORD(R6)     GET CURR ORD ACTIV AMT                       
         A     R5,0(R3)                                                         
         ST    R5,0(R3)                                                         
         SPACE 1                                                                
*                             PRIOR SALES                                       
         L     R5,PRACTORD(R6)     GET PRI ORD ACTIV AMT                        
         A     R5,24(R3)                                                        
         ST    R5,24(R3)                                                        
         SPACE 1                                                                
*                             CURRENT ESTIMATE                                  
         L     R5,TOTORD(R6)       GET CURR AS AT ORD AMT                       
         TM    FLAG6(R2),X'01'     TEST FOR ANY CURR INV DATA                   
         BZ    *+8                 IF ANY                                       
         L     R5,CUASATIN(R6)     GET CURR AS AT INV AMT                       
         A     R5,48(R3)                                                        
         ST    R5,48(R3)                                                        
         SPACE 1                                                                
*                             PRIOR ESTIMATE                                    
         L     R5,PRASATOR(R6)     GET PRI ORD AS AT AMT                        
         TM    FLAG6(R2),X'04'     TEST FOR AS AT PRI INV DATA                  
         BZ    *+8                 IF ANY                                       
         L     R5,PRASATIN(R6)     GET PRI AS AT INV AMT                        
         A     R5,72(R3)                                                        
         ST    R5,72(R3)                                                        
         SPACE 1                                                                
*                             PRIOR ACTUAL                                      
         L     R5,PRTOTORD(R6)     GET PRI TOT ORD AMT                          
         TM    FLAG6(R2),X'02'     TEST FOR ANY PRI INV DATA                    
         BZ    *+8                 IF ANY                                       
         L     R5,PRTOTINV(R6)     GET PRI TOT INV AMT                          
         A     R5,96(R3)                                                        
         ST    R5,96(R3)                                                        
         SPACE 1                                                                
         BR    RE                                                               
         EJECT                                                                  
*              LAST TIME ROUTINES                                               
         SPACE 3                                                                
WR50     CLI   MODE,MANLAST                                                     
         BNE   WR52                                                             
         CLI   TYPE,1                                                           
         BNE   WREXT                                                            
         LA    R2,1                                                             
         B     WR70                                                             
         SPACE 2                                                                
WR52     CLI   MODE,STALAST                                                     
         BNE   WR56                                                             
         CLI   TYPE,2                                                           
         BE    WR54                                                             
         CLI   TYPE,5                                                           
         BE    WR54                                                             
         CLI   TYPE,4                                                           
         BNE   WREXT                                                            
         MVC   P+1(20),=CL20'STATION TOTALS'                                    
         LA    R2,2                                                             
         B     WR70                                                             
         SPACE 2                                                                
WR54     LA    R2,1                                                             
         B     WR70                                                             
         SPACE 2                                                                
WR56     CLI   MODE,TEAMLAST                                                    
         BNE   WR58                                                             
         CLI   TYPE,1                                                           
         BNE   WREXT                                                            
         MVC   P+1(20),RTEMDVNM                                                 
         LA    R2,2                                                             
         B     WR70                                                             
         SPACE 2                                                                
WR58     CLI   MODE,OFFLAST                                                     
         BNE   WR62                                                             
         CLI   TYPE,2                                                           
         BH    WR60                                                             
         MVC   P+1(20),=CL20'OFFICE TOTALS'                                     
         LA    R2,2                                                             
         CLI   TYPE,2                                                           
         BE    WR70                                                             
         LA    R2,3                                                             
         B     WR70                                                             
         SPACE 2                                                                
WR60     CLI   TYPE,4                                                           
         BH    WREXT                                                            
         LA    R2,1                                                             
         B     WR70                                                             
         SPACE 2                                                                
WR62     CLI   MODE,REQLAST                                                     
         BNE   WREXT                                                            
         MVI   ONEFLAG,C'N'        RESET                                        
         MVC   P+1(20),=CL20'REQUEST TOTALS'                                    
         LA    R2,4                                                             
         EJECT                                                                  
*              TOTALLING ROUTINES                                               
         SPACE 3                                                                
WR70     BCTR  R2,0                                                             
         MH    R2,=H'120'                                                       
         LA    R2,ACCUMS(R2)                                                    
         OC    0(120,R2),0(R2)                                                  
         BZ    WREXT                                                            
         CLI   LINE,49                                                          
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,LINTITLE                                                      
         LA    R4,5                                                             
         SPACE 2                                                                
WR72     DS    0H                                                               
         MVC   P+22(16),0(R3)                                                   
         BAS   RE,FORMAT                                                        
         BAS   RE,PRINTEM                                                       
         SPACE 2                                                                
WR74     XC    0(24,R2),0(R2)                                                   
         LA    R2,24(R2)                                                        
         LA    R3,16(R3)                                                        
         BCT   R4,WR72                                                          
         CLI   MODE,REQLAST                                                     
         BNE   WREXT                                                            
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEFUT,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     WREXT                                                            
         SPACE 2                                                                
FORMAT   NTR1                                                                   
         LA    R3,P+38                                                          
         LA    R4,6                                                             
         SPACE 2                                                                
WR76     OC    0(4,R2),0(R2)                                                    
         BZ    WR78                                                             
         EDIT  (4,(R2)),(12,(R3)),COMMAS=YES,MINUS=YES                          
         SPACE 2                                                                
WR78     LA    R2,4(R2)                                                         
         LA    R3,12(R3)                                                        
         BCT   R4,WR76                                                          
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
WREXT    XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
*   ONE-TIME ROUTINE TO SET HEADINGS - LOCATED HERE BECAUSE MONTH               
*     TABLE HAS TO BE LOADED BY 'REPORTER' BEFORE THIS ROUTINE                  
*     CAN BE DONE.  MOVED FROM UNDER 'REQFRST'.                                 
*                                                                               
SETMONS  NTR1                                                                   
         L     R2,ANEWMON          LOCATE START MONTH IN TABLE                  
SM02     EQU   *                                                                
         CLC   0(4,R2),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    SM04                FOUND                                        
         LA    R2,NEXTBUCK(R2)     BUMP TO NEXT BUCKET                          
         B     SM02                GO BACK FOR NEXT                             
SM04     EQU   *                                                                
         ST    R2,AANEWMON         SAVE START ADDRESS                           
*                                  FORMAT MIDS                                  
         LA    R3,MYH8+41                                                       
         LA    R4,MYH9+41                                                       
         LA    R5,4                                                             
SM06     EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,(R2)),(6,(R3))                                    
         GOTO1 GETBROAD,DMCB,(R2),WORK                                          
         MVC   7(2,R3),=C'28'                                                   
         CLI   DMCB,4                                                           
         BE    SM08                                                             
         MVC   7(2,R3),=C'35'                                                   
         SPACE 2                                                                
SM08     MVC   WORK(6),0(R2)                                                    
         PACK  DUB,WORK(2)                                                      
         SP    DUB,=P'1'                                                        
         UNPK  WORK(2),DUB                                                      
         OI    WORK+1,X'F0'                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(6,(R4))                                    
         GOTO1 GETBROAD,DMCB,WORK,WORK+10                                       
         MVC   7(2,R4),=C'28'                                                   
         CLI   DMCB,4                                                           
         BE    SM10                                                             
         MVC   7(2,R4),=C'35'                                                   
         SPACE 2                                                                
SM10     LA    R2,NEXTBUCK(R2)                                                  
         LA    R3,12(R3)                                                        
         LA    R4,12(R4)                                                        
         BCT   R5,SM06                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT                                                 
         SPACE 3                                                                
PRINTEM  NTR1                                                                   
         MVC   HEAD4+50(20),MYPER                                               
         MVC   HEAD6(110),MYH6                                                  
         MVC   HEAD8(110),MYH8                                                  
         MVC   HEAD9(110),MYH9                                                  
         CH    R4,=H'1'                                                         
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*              TITLES AND LITERALS                                              
         SPACE 3                                                                
LINTITLE DS    0H                                                               
         DC    CL16'CURRENT SALES'                                              
         DC    CL16'PRIOR SALES'                                                
         DC    CL16'CURRENT ESTIMATE'                                           
         DC    CL16'PRIOR ESTIMATE'                                             
         DC    CL16'PRIOR ACTUAL'                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
MYSPACE  DS    0D                                                               
         DS    6000C                                                            
         EJECT                                                                  
MYD      DSECT                                                                  
ACCUMS   DS    4CL120                                                           
TYPE     DS    CL1                                                              
MYPER    DS    CL20                                                             
MYH6     DS    CL110                                                            
MYH8     DS    CL110                                                            
MYH9     DS    CL110                                                            
AANEWMON DS    F                   A(START OF REQUEST DATES)                    
ONEFLAG  DC    CL1'N'              ONE-TIME SWITCH                              
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
         SPACE 2                                                                
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPMODES                                                     
*              STORAGE FOR MODULE                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038REREP3802A05/01/02'                                      
         END                                                                    
