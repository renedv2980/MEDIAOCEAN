*          DATA SET REREP4002A AT LEVEL 036 AS OF 05/01/02                      
*PHASE RE4002C,*                                                                
         TITLE 'SALESMAN REPORT'                                                
**********************************************************************          
* HISTORY OF CHANGES                                                 *          
**********************************************************************          
* NOV21/91 (BU ) --- ADD FACILITIES TO USE VALUENEW INSTEAD OF       *          
*                    VALUEMON                                        *          
*                                                                    *          
* MAR27/92 (BU ) --- MAKE COMPATIBLE WITH VALU2NEW                   *          
*                    REREPRGEQU ---> REREPRGEQA                      *          
*                                                                    *          
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                 *          
*                                                                   **          
* JAN23/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
* MAY06/98 (BU ) --- FIX EDIT FOR TOTALS                            *           
*                                                                   *           
* OCT23/98 (BU ) --- OPTION2: EST/BOOKED $ ONLY                     *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                    ***   END  TOMBSTONE   ***                      *          
**********************************************************************          
RE4002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE4002                                                       
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         EJECT                                                                  
*              CONTROLLING MODE SETTINGS                                        
         SPACE 3                                                                
         CLI   MODE,REQFRST                                                     
         BNE   SR2                                                              
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*                                     SPECIAL YR 2000 FORMAT                    
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         B     SREXT                                                            
         SPACE 2                                                                
SR2      CLI   MODE,STAFRST                                                     
         BNE   SR4                                                              
         MVC   P+1(4),RSTAKSTA                                                  
         LA    R1,P+4                                                           
         CLI   0(R1),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MVC   0(3,R1),=C'-TV'                                                  
         CLI   RSTAKSTA+4,C' '                                                  
         BE    SREXT                                                            
         MVC   1(1,R1),RSTAKSTA+4                                               
         MVI   2(R1),C'M'                                                       
         B     SREXT                                                            
         SPACE 2                                                                
SR4      CLI   MODE,PROCCONT                                                    
         BNE   SR6                                                              
         BAS   RE,POST                                                          
         OC    TOTABLE(4),TOTABLE                                               
         BZ    SREXT                                                            
         BAS   RE,FORMAT                                                        
         GOTO1 REPORT                                                           
         B     SREXT                                                            
         SPACE 2                                                                
SR6      CLI   MODE,STALAST                                                     
         BNE   SR10                                                             
         CLC   P(2),SPACES                                                      
         BE    SR8                                                              
         MVC   P,SPACES                                                         
         B     SREXT                                                            
         SPACE 2                                                                
SR8      GOTO1 REPORT                                                           
         LA    R2,1                                                             
         B     SR12                                                             
         SPACE 2                                                                
SR10     LA    R2,2                                                             
         CLI   MODE,MANLAST                                                     
         BE    SR12                                                             
         LA    R2,3                                                             
         CLI   MODE,TEAMLAST                                                    
         BE    SR12                                                             
         LA    R2,4                                                             
         CLI   MODE,DIVLAST                                                     
         BE    SR12                                                             
         LA    R2,5                                                             
         CLI   MODE,OFFLAST                                                     
         BE    SR12                                                             
         LA    R2,6                                                             
         CLI   MODE,REQLAST                                                     
         BE    SR12                                                             
         CLI   MODE,MANFRST                                                     
         BNE   SREXT                                                            
         MVI   FORCEHED,C'Y'                                                    
         B     SREXT                                                            
         EJECT                                                                  
*              TOTALLING AND EXIT                                               
         SPACE 2                                                                
SR12     LR    R3,R2                                                            
         SLL   R2,2                                                             
         LA    R2,TOTABLE(R2)                                                   
         OC    0(4,R2),0(R2)                                                    
         BZ    SREXT                                                            
         BCTR  R3,0                                                             
         MH    R3,=H'17'                                                        
         LA    R3,TOTNAMES(R3)                                                  
         MVC   P+76(17),0(R3)                                                   
         EDIT  (4,(R2)),(11,P+97),COMMAS=YES,FLOAT=-                            
         XC    0(4,R2),0(R2)                                                    
         CLI   MODE,REQLAST                                                     
         BNE   *+12                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     *+8                                                              
         MVI   SPACING,3                                                        
         GOTO1 REPORT                                                           
         SPACE 2                                                                
SREXT    CLI   MODE,REQLAST                                                     
         BNE   SREXIT                                                           
         MVI   RCSUBPRG,2                                                       
         MVI   FORCEFUT,C'Y'                                                    
         GOTO1 REPORT                                                           
SREXIT   XMOD1 1                                                                
         SPACE 2                                                                
TOTABLE  DC    10F'0'                                                           
         SPACE 2                                                                
TOTNAMES DS    0H                                                               
         DC    CL17'STATION TOTAL'                                              
         DC    CL17'SALESPERSON TOTAL'                                          
         DC    CL17'TEAM TOTAL'                                                 
         DC    CL17'DIVISION TOTAL'                                             
         DC    CL17'OFFICE TOTAL'                                               
         DC    CL17'REQUEST TOTAL'                                              
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
*              POST VALUES                                                      
         SPACE 3                                                                
POST     NTR1                                                                   
         SR    R4,R4                                                            
         L     R2,ANEWMON          A(NEW MONTH TABLE)                           
POST0002 EQU   *                                                                
         CLC   0(4,R2),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST0008            FOUND - BEGIN TO PULL FIGURES                
         BL    POST0004            NOT FOUND - CHECK NEXT                       
         DC    H'0'                SHOULD NOT HAPPEN                            
POST0004 EQU   *                                                                
         LA    R2,NEXTBUCK(R2)     BUMP TO NEXT BUCKET                          
         B     POST0002            GO BACK FOR NEXT                             
POST0008 EQU   *                                                                
         CLI   0(R2),0             ANY MORE BUCKETS?                            
         BE    POST0010            NO                                           
         CLC   0(4,R2),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BH    POST0010            TABLE > END DATE - EXIT'                     
*                                                                               
         LR    R6,R2               SET R6 TO A(BUCKETS IN MONTH                 
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROLS                          
         L     R5,TOTORD(R6)       GET AS AT ORDERED BUCKET                     
         CLI   QOPTION2,C'B'       BOOKED DOLLARS ONLY?                         
         BE    POST0009            YES                                          
         TM    FLAG6(R2),X'01'     LOOK FOR CURR INV AMT                        
         BZ    POST0009            NOT FOUND                                    
         L     R5,CUASATIN(R6)     FOUND -USE CURR AS AT INV AMT                
POST0009 EQU   *                                                                
         AR    R4,R5                                                            
         LA    R2,NEXTBUCK(R2)                                                  
         B     POST0008                                                         
POST0010 EQU   *                                                                
         SPACE 2                                                                
         LA    R2,TOTABLE                                                       
         LA    R3,10                                                            
         SPACE 2                                                                
POST0012 LR    R5,R4                                                            
         A     R5,0(R2)                                                         
         ST    R5,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R3,POST0012                                                      
         B     XIT                                                              
         EJECT                                                                  
*              FORMAT VALUES FOR CONTRACT                                       
         SPACE 3                                                                
FORMAT   NTR1                                                                   
FORMAT1  UNPK  P+10(8),RCONKCON(5)                                              
         MVI   P+17,C' '                                                        
         LA    R1,P+10                                                          
         CLI   0(R1),C'0'                                                       
         BNE   *+16                                                             
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         B     *-16                                                             
         MVC   P+20(4),RCONKADV                                                 
         MVC   P+25(20),RADVNAME                                                
         MVC   P+46(4),RCONKAGY                                                 
         MVC   P+50(2),RCONKAOF                                                 
         MVC   P+53(20),RAGYNAM1                                                
         EDIT  (4,TOTABLE),(11,P+97),COMMAS=YES,FLOAT=-                         
         XC    TOTABLE(4),TOTABLE                                               
         CLC   RCONPRD,SPACES                                                   
         BE    FORMAT2                                                          
         MVC   P+76(20),RPRDNAME                                                
         CLC   RPRDNAME,SPACES                                                  
         BE    XIT                                                              
         MVC   P+76(20),SPACES                                                  
         MVC   P+76(3),RCONPRD                                                  
         B     XIT                                                              
         SPACE 2                                                                
FORMAT2  LA    R2,RCONELEM                                                      
         SR    R3,R3                                                            
         SPACE 2                                                                
FORMAT4  CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLI   0(R2),X'05'                                                      
         BNE   FORMAT6                                                          
         MVC   P+76(20),2(R2)                                                   
         B     XIT                                                              
         SPACE 2                                                                
FORMAT6  IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     FORMAT4                                                          
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
QFASTART DS    CL6                 YR 2000 DATE FOR COMPARES                    
QFAEND   DS    CL6                 YR 2000 DATE FOR COMPARES                    
FILLER   DS    2000C                                                            
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036REREP4002A05/01/02'                                      
         END                                                                    
