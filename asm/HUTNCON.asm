*          DATA SET HUTNCON    AT LEVEL 056 AS OF 01/05/84                      
*PHASE NSIHCON,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE DEMTIME                                                                
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
         PRINT NOGEN                                                            
HUTNCON  CSECT                                                                  
         NBASE 500,HUTNCON,=V(REGSAVE)                                          
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         OPEN  IN1,OUT                                                          
GET      GET   IN1,INREC                                                        
         LA    R4,NEPTAB                                                        
         TM    ITZ,X'04'           PACIFIC OR ALASKA/HAWAII                     
         BNZ   HVTZ                                                             
         TM    ITZ,X'02'           CENTRAL OR MOUNTIAN                          
         BZ    *+8                  NO - USE EASTERN AND PACIFIC TABLES         
         LA    R4,NCPTAB                                                        
HVTZ     PACK  DUB,IDPT                                                         
         CVB   R9,DUB                                                           
         STC   R9,IDPT                                                          
GETIM    CLI   0(R4),0                                                          
         BNE   *+8                                                              
         B     GET                                                              
         CLC   0(1,R4),IDPT                                                     
         BE    MVTZ                                                             
         LA    R4,6(R4)                                                         
         B     GETIM                                                            
MVTZ     MVC   ODAY,1(R4)                                                       
         LA    R6,2(R4)                                                         
         GOTO1 =V(HRTOQH),DUB,(0,(R6)),ODAY+1                                   
         LA    R6,4(R4)                                                         
         GOTO1 =V(HRTOQH),DUB,(0,(R6)),ODAY+2                                   
         MVI   OCDE,C'H'                                                        
         MVI   ODCDE,X'01'                                                      
         MVC   OMED(3),=C'TNN0'                                                 
         MVI   OAGY+2,0                                                         
         XC    DUB,DUB                                                          
         MVC   DUB+6(2),IMRKT                                                   
         CVB   R9,DUB                                                           
         STH   R9,DUB                                                           
         MVC   OMKT,DUB                                                         
         EDIT  OMKT,(3,P)                                                       
         MVC   CHAR,ODAY                                                        
         EDIT  CHAR,(2,P+4)                                                     
         MVC   CHAR,ODAY+1                                                      
         EDIT  CHAR,(2,P+7)                                                     
         MVC   CHAR,ODAY+2                                                      
         EDIT  CHAR,(2,P+10)                                                    
         MVC   ODATE,=C'790312'                                                 
         MVC   OREC,=X'003700'                                                  
         XC    OMNAME,OMNAME                                                    
         LA    R5,11                                                            
         LA    R6,HCODES                                                        
         LA    R7,IHUT                                                          
         LA    R9,1                                                             
         LA    R3,OHUTS                                                         
         ST    R7,DEMPOINT                                                      
HUTLOOP  CH    R9,=H'12'                                                        
         BH    HUTLOPX                                                          
         LR    RE,R9                                                            
         BCTR  RE,0                                                             
         MH    RE,=H'3'                                                         
         LA    RE,CNVTAB(RE)                                                    
         ZIC   RF,1(RE)                                                         
         BCTR  RF,0                                                             
         MH    RF,=H'2'                                                         
         A     RF,DEMPOINT                                                      
         XC    DUB,DUB                                                          
         MVC   DUB+6(2),0(RF)                                                   
         CVB   R0,DUB                                                           
         STCM  R0,3,0(R3)                                                       
         LTR   R0,R0                                                            
         BNZ   HUTLOP2                                                          
         ZIC   RF,2(RE)                                                         
         BCTR  RF,0                                                             
         MH    RF,=H'2'                                                         
         A     RF,DEMPOINT                                                      
         XC    DUB,DUB                                                          
         MVC   DUB+6(2),0(RF)                                                   
         CVB   R0,DUB                                                           
         STCM  R0,3,0(R3)                                                       
HUTLOP2  LA    R9,1(R9)                                                         
         LA    R3,2(R3)            BUMP TO NEXT OUTPUT                          
         B     HUTLOOP                                                          
HUTLOPX  DS    0H'0'                                                            
         EJECT                                                                  
*CONVERT THE RAW HUT/PUT VALUES TO INDICES                                      
*USING NOVEMBER AS BASE                                                         
INDEX    DS    0H                                                               
         XR    R0,R0                                                            
         ICM   R0,3,OHUTS+20       NOV VALUE                                    
         LTR   R0,R0                                                            
         BZ    INDEXZER            BASE IS ZERO                                 
         LA    R1,12                                                            
         LA    R2,OHUTS                                                         
INDEX2   XR    RF,RF                                                            
         ICM   RF,3,0(R2)          RAW HUT VALUE IN RF                          
         M     RE,=F'100'          X 100                                        
         SLDA  RE,1                X 2 FOR ROUNDING                             
         DR    RE,R0               DIVIDE BY BASE(NOV) VALUE                    
         AH    RF,=H'1'                                                         
         SRA   RF,1                ROUND                                        
         CH    RF,=H'255'          INDICES ARE ABOVE 255                        
         BL    *+8                                                              
         LH    RF,=H'100'           SET TO 100                                  
         CH    RF,=H'25'           INDICES BELOW 25                             
         BH    *+8                                                              
         LA    RF,25                SET TO 25                                   
         STCM  RF,3,0(R2)          REPLACE RAW VALUE IN INDEX                   
         LA    R2,2(R2)            POINT TO NEXT RAW VALUE                      
         BCT   R1,INDEX2                                                        
         B     INDEXOUT                                                         
INDEXZER LA    R1,12               FOR A BASE VALUE OF ZERO                     
         LA    R2,OHUTS            MAKE ALL INDICES TO ZERO                     
         LA    R0,100                                                           
         STCM  R0,3,0(R2)                                                       
         LA    R2,2(R2)                                                         
         BCT   R1,*-8                                                           
         B     INDEXOUT                                                         
INDEXOUT DS    0H'0'                                                            
         SPACE 2                                                                
         MVC   ODEM,0(R6)                                                       
         MVI   ODAY,X'15'                                                       
PUT2     MVI   CARDD,C' '                                                       
         MVC   CARDD+1(79),CARDD                                                
         MVC   CMED(3),=C'TN0'     1980 HUTS (USE FOR 1981)                     
         MVC   CMED(3),=C'TN1'     1981 HUTS (USE FOR 1982)                     
         MVC   CMED(3),=C'TNJ'     1982 HUTS (USE FOR 1983)                     
         MVC   CMED(3),=C'TN3'     1983 HUTS (USE FOR 1984)                     
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),ODEM                                                   
         LH    RE,HALF                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CAUD,DUB                                                         
         MVC   HALF,OMKT                                                        
         LH    RE,HALF                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CMKT,DUB                                                         
         CLI   ODAY,X'15'                                                       
         BNE   *+10                                                             
         MVC   CDAY,=C'M-F'                                                     
         CLI   ODAY,X'66'                                                       
         BNE   *+10                                                             
         MVC   CDAY,=C'SAT'                                                     
         CLI   ODAY,X'77'                                                       
         BNE   *+10                                                             
         MVC   CDAY,=C'SUN'                                                     
         SR    RE,RE                                                            
         IC    RE,OSTART                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CSTQH,DUB                                                        
         IC    RE,OEND                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CENDQH,DUB                                                       
         IC    RE,ODEM                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB,DUB                                                          
         MVI   CACT,C'A'                                                        
         LA    R8,12                                                            
         LA    RF,OHUTS                                                         
         LA    R3,CMONS+1                                                       
CNVADJ   ICM   RE,3,0(RF)                                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB                                                      
         LA    RF,2(RF)                                                         
         LA    R3,4(R3)                                                         
         BCT   R8,CNVADJ                                                        
         PUT   OUT,CARDD                                                        
         CLI   ODAY,X'15'                                                       
         BNE   NOPRINT                                                          
         XC    P,P                                                              
         MVC   P(80),CARDD                                                      
         ZIC   RE,IDPT                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+81(2),DUB+6(2)                                                 
         GOTO1 =V(PRINTER)                                                      
NOPRINT  CLI   ODAY,X'15'                                                       
         BNE   *+12                                                             
         MVI   ODAY,X'66'                                                       
         B     PUT2                                                             
         CLI   ODAY,X'66'                                                       
         BNE   *+12                                                             
         MVI   ODAY,X'77'                                                       
         B     PUT2                                                             
         LA    R3,OHUTS            SET FOR NEW RECORD                           
         LA    R9,1                                                             
         LA    R6,1(R6)                                                         
         L     RE,DEMPOINT                                                      
         LA    RE,14(RE)                                                        
         ST    RE,DEMPOINT                                                      
         BCT   R5,HUTLOOP                                                       
         B     GET                                                              
ENDJOB   CLOSE IN1,OUT                                                          
         EOJ                                                                    
         EJECT                                                                  
* MONTH/NORMAL INPUT SLOT/INPUT SLOT IF NORMAL=0                                
CNVTAB   DC    AL1(01),AL1(03),AL1(04)                                          
         DC    AL1(02),AL1(04),AL1(04)                                          
         DC    AL1(03),AL1(05),AL1(04)                                          
         DC    AL1(04),AL1(06),AL1(06)                                          
         DC    AL1(05),AL1(06),AL1(06)                                          
         DC    AL1(06),AL1(06),AL1(06)                                          
         DC    AL1(07),AL1(07),AL1(07)                                          
         DC    AL1(08),AL1(07),AL1(07)                                          
         DC    AL1(09),AL1(07),AL1(07)                                          
         DC    AL1(10),AL1(01),AL1(02)                                          
         DC    AL1(11),AL1(02),AL1(02)                                          
         DC    AL1(12),AL1(02),AL1(02)                                          
*                                                                               
NEPTAB   DC    AL1(11),X'17',AL2(900),AL2(1200)                                 
         DC    AL1(12),X'17',AL2(1200),AL2(1630)                                
         DC    AL1(14),X'17',AL2(1630),AL2(1800)                                
         DC    AL1(15),X'17',AL2(1800),AL2(1930)                                
         DC    AL1(18),X'17',AL2(1930),AL2(2000)                                
         DC    AL1(30),X'17',AL2(2000),AL2(2100)                                
         DC    AL1(31),X'17',AL2(2100),AL2(2300)                                
         DC    AL1(21),X'17',AL2(2300),AL2(2330)                                
         DC    AL1(22),X'17',AL2(2330),AL2(0100)                                
         DC    H'0'                                                             
NCPTAB   DC    AL1(11),X'17',AL2(0900),AL2(1200)                                
         DC    AL1(12),X'17',AL2(1200),AL2(1530)                                
         DC    AL1(14),X'17',AL2(1530),AL2(1700)                                
         DC    AL1(15),X'17',AL2(1700),AL2(1830)                                
         DC    AL1(18),X'17',AL2(1830),AL2(1900)                                
         DC    AL1(30),X'17',AL2(1900),AL2(2000)                                
         DC    AL1(31),X'17',AL2(2000),AL2(2200)                                
         DC    AL1(21),X'17',AL2(2200),AL2(2230)                                
         DC    AL1(22),X'17',AL2(2230),AL2(2400)                                
         DC    H'0'                                                             
         DC    H'0'                                                             
DEMPOINT DS    F                                                                
HALF     DS    H                                                                
INREC    DS    0C                                                               
IMRKT    DS    CL2                                                              
ITZ      DS    CL1                                                              
IMKTYPE  DS    CL1                                                              
IDPT     DS    CL2                                                              
         DS    CL50                                                             
IHUT     DS    CL154                                                            
         DS    CL300                                                            
         DS    CL1090                                                           
         DS    CL100                                                            
OREC     DS    CL4                                                              
OCDE     DS    CL1                 H                                            
ODCDE    DS    CL1                 1                                            
OMED     DS    CL1                 T                                            
OSRC     DS    CL1                 N                                            
OAGY     DS    CL2                 TB                                           
         DS    CL1                 0                                            
OMKT     DS    CL2                                                              
ODEM     DS    CL1                                                              
ODAY     DS    CL1                                                              
OSTART   DS    CL1                                                              
OEND     DS    CL1                                                              
OHUTS    DS    CL24                                                             
ODATE    DS    CL6                                                              
OMNAME   DS    CL20                                                             
DUB      DS    D                                                                
WORK     DS    CL20                                                             
CHAR     DS    C                                                                
CARDD    DS    0H                                                               
CMED     DS    CL1                 MEDIA                                        
CRAT     DS    CL1                 SERVICE                                      
CSRC     DS    CL1                 SOURCE                                       
         DS    CL1                                                              
CMKT     DS    CL4                 MARKET                                       
         DS    CL1                                                              
CDAY     DS    CL3                 DAY M-F,SAT,SUN                              
         DS    CL1                                                              
CSTQH    DS    CL2                 START QH                                     
         DS    CL1                                                              
CENDQH   DS    CL2                 END QH                                       
         DS    CL1                                                              
CAUD     DS    CL2                 AUDIENCE TYPE                                
         DS    CL1                                                              
CACT     DS    CL1                 ACTION CODE                                  
         DS    CL1                                                              
CMONS    DS    CL48                MONTHS                                       
         DS    CL8                 SPARE                                        
HCODES   DC    X'080102030A04050B070C06'                                        
         LTORG                                                                  
IN1      DTFMT BLKSIZE=6500,RECSIZE=1300,RECFORM=FIXBLK,TYPEFLE=INPUT, X        
               FILABL=STD,REWIND=UNLOAD,WORKA=YES,EOFADDR=ENDJOB,      X        
               IOAREA1=IN1A,DEVADDR=SYS004                                      
OUT      DTFMT BLKSIZE=8000,RECSIZE=080,TYPEFLE=OUTPUT,RECFORM=FIXBLK, X        
               FILABL=STD,WORKA=YES,REWIND=UNLOAD,DEVADDR=SYS005,      X        
               IOAREA1=OUT1                                                     
IN1A     DS    CL14000                                                          
OUT1     DS    CL8000                                                           
       ++INCLUDE DDDPRINT                                                       
