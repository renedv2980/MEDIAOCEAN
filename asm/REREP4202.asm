*          DATA SET REREP4202  AT LEVEL 023 AS OF 05/01/02                      
*PHASE RE4202C,*                                                                
         TITLE 'SALESMAN SUMMARY'                                               
***********************************************************************         
* HISTORY OF CHANGES                                                  *         
***********************************************************************         
*  NOV21/91 (BU ) --- INSERT VALUENEW FACILITY IN PLACE OF VALUEMON   *         
*                                                                     *         
*  MAR27/92 (BU ) --- MAKE COMPATIBLE WITH VALU2NEW                   *         
*                     REREPRGEQU ---> REREPRGEQA                      *         
*                                                                     *         
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                  *         
*                                                                     *         
* MAY22/97 (BU ) --- UPGRADE FOR YR 2000                              *         
*                                                                   * *         
* JAN26/98 (JRD) --- 4K CONTRACTS                                   * *         
*                                                                   * *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
RE4202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE4202                                                       
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         EJECT                                                                  
*              CONTROL OF MODE SETTINGS                                         
         SPACE 3                                                                
         CLI   MODE,REQFRST                                                     
         BNE   SUM2                                                             
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*                                     SPECIAL YR 2000 FORMAT                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,0                                                       
         GOTO1 ROLLER,DMCB,0,ACCUMS,5,13                                        
         B     SUMEXT                                                           
         SPACE 2                                                                
SUM2     CLI   MODE,TEAMFRST                                                    
         BNE   SUM4                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     SUMEXT                                                           
         SPACE 2                                                                
SUM4     CLI   MODE,PROCCONT                                                    
         BNE   SUM6                                                             
         MVC   P+1(3),RCONSAL                                                   
         MVC   P+11(20),RSALNAME                                                
         BAS   RE,POST                                                          
         B     SUMEXT                                                           
         SPACE 2                                                                
SUM6     CLI   MODE,MANLAST                                                     
         BNE   SUM8                                                             
         GOTO1 ROLLER,DMCB,6,ACCUMS                                             
         LA    R2,1                                                             
         B     TOTL0002                                                         
         SPACE 2                                                                
SUM8     LA    R2,1                                                             
         CLI   MODE,TEAMLAST                                                    
         BNE   SUM10                                                            
         MVC   P,SPACES                                                         
         GOTO1 REPORT              SKIP A LINE                                  
         B     TOTALS                                                           
SUM10    LA    R2,2                                                             
         CLI   MODE,DIVLAST                                                     
         BE    TOTALS                                                           
         LA    R2,3                                                             
         CLI   MODE,OFFLAST                                                     
         BE    TOTALS                                                           
         LA    R2,4                                                             
         CLI   MODE,REQLAST                                                     
         BE    TOTALS                                                           
         B     SUMEXT                                                           
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
*              TOTALLING AND EXIT                                               
         SPACE 3                                                                
TOTALS   LR    R3,R2                                                            
         LA    R2,1(R2)                                                         
         BCTR  R3,R0                                                            
         MH    R3,=H'15'                                                        
         LA    R3,TOTNAMES(R3)                                                  
         MVC   P+08(15),0(R3)                                                   
         SPACE 2                                                                
TOTL0002 GOTO1 ROLLER,DMCB,1,ACCUMS,(R2)                                        
         L     R2,DMCB                                                          
         OC    0(52,R2),0(R2)                                                   
         BNZ   TOTL0008                                                         
         MVC   P,SPACES                                                         
         B     SUMEXT                                                           
         SPACE 2                                                                
TOTL0008 LA    R3,P+32                                                          
         L     R4,ANEWMON          A(NEW MONTH TABLE)                           
         LA    R5,HEAD10+34                                                     
         LA    R6,6                                                             
         L     R8,AMONINFO         MONTHS 1-6 IN MONINFO TABLE                  
         SPACE 1                                                                
TOTL0012 EQU   *                                                                
         CLC   0(4,R4),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    TOTL0020            FOUND - BEGIN TO PULL FIGURES                
         BL    TOTL0016            NOT FOUND - CONTINUE                         
         DC    H'0'                SHOULDN'T HAPPEN                             
TOTL0016 EQU   *                                                                
         LA    R4,NEXTBUCK(R4)     BUMP TO NEXT BUCKET                          
         B     TOTL0012            GO BACK FOR NEXT                             
TOTL0020 EQU   *                                                                
         BAS   RE,TOTL0024                                                      
         LA    R3,PSECOND+32                                                    
         LA    R5,HEAD11+34                                                     
         LA    R6,6                                                             
         LA    R8,96(R8)           MONTHS 7-12 IN MONINFO TABLE                 
         SPACE 1                                                                
         BAS   RE,TOTL0024                                                      
         EDIT  (4,(R2)),(11,P+98),COMMAS=YES,FLOAT=-                            
         XC    0(4,R2),0(R2)                                                    
         CLI   MODE,REQLAST                                                     
         BNE   *+12                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     *+8                                                              
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     SUMEXT                                                           
         SPACE 2                                                                
TOTL0024 NTR1                                                                   
         SPACE 2                                                                
TOTL0028 EQU   *                                                                
         CLI   0(R4),0                                                          
         BE    TOTL0032                                                         
         CLC   0(4,R4),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BH    TOTL0032            TABLE > END DATE - EXIT                      
         GOTO1 DATCON,DMCB,(0,(R4)),(6,(R5))                                    
         MVC   7(2,R5),=C'28'      SET # WEEKS = 4 (28 DAYS)                    
         CLI   2(R8),28            COMPARE MONINFO TABLE                        
         BE    *+10                                                             
         MVC   7(2,R5),=C'35'      SET # WEEKS = 5 (35 DAYS)                    
         OC    0(4,R2),0(R2)                                                    
         BZ    TOTL0032                                                         
         EDIT  (4,(R2)),(11,(R3)),COMMAS=YES,FLOAT=-                            
         XC    0(4,R2),0(R2)                                                    
         SPACE 2                                                                
TOTL0032 LA    R2,4(R2)                                                         
         LA    R3,11(R3)                                                        
         LA    R4,NEXTBUCK(R4)                                                  
         LA    R5,11(R5)                                                        
         LA    R8,16(R8)           SKIP TO NEXT MONTH                           
         BCT   R6,TOTL0028                                                      
         XIT1  REGS=(R2,R4)                                                     
         SPACE 2                                                                
SUMEXT   CLI   MODE,REQLAST                                                     
         BNE   SUMEXIT                                                          
         MVI   RCSUBPRG,2                                                       
         MVI   FORCEFUT,C'Y'                                                    
         GOTO1 REPORT                                                           
SUMEXIT  XMOD1 1                                                                
         EJECT                                                                  
*              ROUTINE TO POST VALUES INTO LINE 1                               
         SPACE 3                                                                
POST     NTR1                                                                   
         LA    R2,ACCUMS+8                                                      
         L     R4,ANEWMON          A(NEW MONTH TABLE)                           
*                                                                               
POST0002 EQU   *                                                                
         CLC   0(4,R4),QFASTART    TABLE ENTRY VS REQ START DTE                 
         BE    POST0008            FOUND - BEGIN TO PULL FIGURES                
         BL    POST0004                                                         
         DC    H'0'                SHOULDN'T HAPPEN                             
POST0004 EQU   *                                                                
         LA    R4,NEXTBUCK(R4)     BUMP TO NEXT BUCKET                          
         B     POST0002            GO BACK FOR NEXT                             
         SPACE 2                                                                
POST0008 CLI   0(R3),0                                                          
         BE    XIT                                                              
         CLC   0(4,R4),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BH    XIT                 TABLE > END DATE - EXIT                      
         LR    R6,R4               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROLS                          
*                                  THIS IS A CURRENT ESTIMATE BUCKET            
         L     R5,TOTORD(R6)       GET CURR AS AT ORD AMT                       
         TM    FLAG6(R4),X'01'     ANY CURR AS AT INV?                          
         BZ    *+8                 NO                                           
         L     R5,CUASATIN(R6)     CURR AS AT INV AMT                           
         A     R5,0(R2)                                                         
         ST    R5,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R4,NEXTBUCK(R4)                                                  
         B     POST0008                                                         
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALUES FOR PROGRAM                                               
         SPACE 3                                                                
ACCUMS   DS    D                                                                
         DS    260C                                                             
         SPACE 2                                                                
*                                                                               
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
*                                                                               
TOTNAMES DS    0H                                                               
         DC    CL15'TEAM TOTALS'                                                
         DC    CL15'DIVISION TOTALS'                                            
         DC    CL15'OFFICE TOTALS'                                              
         DC    CL15'REQUEST TOTALS'                                             
         EJECT                                                                  
         LTORG                                                                  
         DS     5000C                                                           
         EJECT                                                                  
       ++INCLUDE REMONARCHD                                                     
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023REREP4202 05/01/02'                                      
         END                                                                    
