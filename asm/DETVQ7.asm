*          DATA SET DETVQ7     AT LEVEL 006 AS OF 03/25/16                      
*PROCESS USING(WARN(15))                                                        
*PHASE DETVQ7A                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*                                                                               
***********************************************************************         
* TVQ CONVERSION BEFORE 2008                       - DETVQ5                     
* TVQ CONVERSION STARTING WITH THE 2008 SCHEDULE.  - DETVQ6           *         
* TVQ CONVERSION STARTING WITH THE 2010 SCHEDULE.  - DETVQ7           *         
*                                                                               
***********************************************************************         
                                                                                
         TITLE 'TVQ/FORETEL CONVERSION'                                         
***********************************************************************         
* THE PROGRAM PROCESSES THE 15 INPUT FILES IN THIS ORDER:             *         
* 1 - ADULTS PRIMETIME          INA1                                  *         
* 2 - ADULTS DAYTIME            INA2                                  *         
* 3 - ADULTS NEWS               INA3                                  *         
* 4 - ADULTS LATE NIGHT         INA4                                  *         
* 5 - MEN PRIMETIME             INM1                                  *         
* 6 - MEN DAYTIME               INM2                                  *         
* 7 - MEN NEWS                  INM3                                  *         
* 8 - MEN LATE NIGHT            INM4                                  *         
* 9 - WOMEN PRIMETIME           INW1                                  *         
* 10- WOMEN DAYTIME             INW2                                  *         
* 11- WOMEN NEWS                INW3                                  *         
* 12- WOMEN LATE NIGHT          INW4                                  *         
MAXFILQ  EQU   12                                                               
***********************************************************************         
DETVQ    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DETVQ,=V(REGSAVE),R6                                           
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE,=CL60'TVQ/FORETEL CONVERSION'                              
         MVI   P,0                                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         L     RE,=A(PROGBUFF)                                                  
         L     RF,=F'220000'                                                    
         XCEF                                                                   
*                                                                               
         L     RE,=A(SUMTABS)                                                   
         LA    RF,SUMTABE-SUMTABS                                               
         XCEF                                                                   
*                                                                               
         MVI   RETCODE,RETOK       INITIALIZE RETURN CODE TO OK                 
*                                                                               
*GET YEAR/MONTH FROM JCL CARD AND BUILD BOOK=YYMM                               
         GOTO1 =V(CARDS),DMCB,WORK2,=C'RE00'     CARD=YEAR/MONTH                
         LA    R1,WORK2                                                         
*                                                                               
         MVI   YRLN,0                                                           
         SR    R2,R2                                                            
         LA    R5,YEAR                                                          
CARD1    C     R1,=A(WORK2+70)      YEAR                                        
         BH    CARDINV                                                          
         CLI   0(R1),C' '                                                       
         BE    CARDINV                                                          
         CLI   0(R1),C'/'                                                       
         BE    CARD3                                                            
         TM    0(R1),X'F0'                                                      
         BNO   CARDINV                                                          
         MVC   0(1,R5),0(R1)                                                    
         AHI   R2,1                                                             
         LA    R5,1(R5)                                                         
         LA    R1,1(R1)                                                         
         B     CARD1                                                            
CARD3    CHI   R2,4                                                             
         BNE   CARDINV                                                          
*                                                                               
         LA    R5,MONTH            MONTH                                        
         SR    R2,R2                                                            
         LA    R1,1(R1)            GO PAST '/'                                  
CARD5    C     R1,=A(WORK2+70)                                                  
         BH    CARDINV                                                          
         CLI   0(R1),C' '                                                       
         BE    CARD7                                                            
         MVC   0(1,R5),0(R1)                                                    
         AHI   R2,1                                                             
         LA    R5,1(R5)                                                         
         LA    R1,1(R1)                                                         
         B     CARD5                                                            
CARD7    CHI   R2,3                                                             
         BNE   CARDINV                                                          
*                                                                               
         MVC   WORK(2),YEAR+2                                                   
         MVC   WORK+2(4),=C'0101'                                               
         GOTO1 =V(DATCON),DMCB,(0,WORK),(3,FULL)                                
         MVC   BOOK(1),FULL         -YEAR-                                      
*                                                                               
         LA    RE,MONTHTAB                                                      
         USING MONTHTD,RE                                                       
CARD10   CLI   0(RE),X'FF'                                                      
         BE    CARDINV                                                          
         CLC   MONTH,MONALPH                                                    
         BE    *+12                                                             
         LA    RE,MONTHTDL(RE)                                                  
         B     CARD10                                                           
         MVC   BOOK+1(1),MONNUM     -MONTH-                                     
         B     CARDX                                                            
*                                                                               
CARDINV  MVC   P,SPACES                                                         
         MVC   P(24),=C'INVALID YEAR/MONTH INPUT'                               
         GOTO1 =V(PRINTER)              PRINT ERROR MESSAGE                     
         DC    H'0'                     INVALID INPUT                           
*                                                                               
CARDX    DS    0H                                                               
*                                                                               
         OPEN  (INA1,(INPUT))           OPEN INPUT FILES                        
         OPEN  (INA2,(INPUT))                                                   
         OPEN  (INA3,(INPUT))                                                   
         OPEN  (INA4,(INPUT))                                                   
         OPEN  (INM1,(INPUT))                                                   
         OPEN  (INM2,(INPUT))                                                   
         OPEN  (INM3,(INPUT))                                                   
         OPEN  (INM4,(INPUT))                                                   
         OPEN  (INW1,(INPUT))                                                   
         OPEN  (INW2,(INPUT))                                                   
         OPEN  (INW3,(INPUT))                                                   
         OPEN  (INW4,(INPUT))                                                   
         OPEN  (OUT,(OUTPUT))         OPEN OUTPUT FILE                          
*                                                                               
         MVI   FILESW,1             GET RECORD FROM FILE AND SET FORMAT         
GET      MVI   CLEARS,C' '                                                      
         MVC   CLEARS+1(CLEARE-CLEARS-1),CLEARS                                 
GETA1    CLI   FILESW,1                                                         
         BNE   GETA2                                                            
         GET   INA1,INREC                                                       
         MVI   FILETYP,ADULTS        -- ADULTS --                               
         MVI   DAYPART,DPRIME          PRIMETIME                                
         B     GETX                                                             
GETA2    CLI   FILESW,2                                                         
         BNE   GETA3                                                            
         GET   INA2,INREC                                                       
         MVI   FILETYP,ADULTS        -- ADULTS --                               
         MVI   DAYPART,DDAY            DAYTIME                                  
         B     GETX                                                             
GETA3    CLI   FILESW,3                                                         
         BNE   GETA4                                                            
         GET   INA3,INREC                                                       
         MVI   FILETYP,ADULTS        -- ADULTS --                               
         MVI   DAYPART,DNEWS           NEWS                                     
         B     GETX                                                             
GETA4    CLI   FILESW,4                                                         
         BNE   GETM1                                                            
         GET   INA4,INREC                                                       
         MVI   FILETYP,ADULTS        -- ADULTS --                               
         MVI   DAYPART,DLATE           LATE NIGHT                               
         B     GETX                                                             
GETM1    CLI   FILESW,5                                                         
         BNE   GETM2                                                            
         GET   INM1,INREC                                                       
         MVI   FILETYP,MEN           -- MEN --                                  
         MVI   DAYPART,DPRIME          PRIMETIME                                
         B     GETX                                                             
GETM2    CLI   FILESW,6                                                         
         BNE   GETM3                                                            
         GET   INM2,INREC                                                       
         MVI   FILETYP,MEN           -- MEN --                                  
         MVI   DAYPART,DDAY            DAYTIME                                  
         B     GETX                                                             
GETM3    CLI   FILESW,7                                                         
         BNE   GETM4                                                            
         GET   INM3,INREC                                                       
         MVI   FILETYP,MEN           -- MEN --                                  
         MVI   DAYPART,DNEWS           NEWS                                     
         B     GETX                                                             
GETM4    CLI   FILESW,8                                                         
         BNE   GETW1                                                            
         GET   INM4,INREC                                                       
         MVI   FILETYP,MEN           -- MEN --                                  
         MVI   DAYPART,DLATE           LATE NIGHT                               
         B     GETX                                                             
GETW1    CLI   FILESW,9                                                         
         BNE   GETW2                                                            
         GET   INW1,INREC                                                       
         MVI   FILETYP,WOMEN         -- WOMEN --                                
         MVI   DAYPART,DPRIME          PRIMETIME                                
         B     GETX                                                             
GETW2    CLI   FILESW,10                                                        
         BNE   GETW3                                                            
         GET   INW2,INREC                                                       
         MVI   FILETYP,WOMEN         -- WOMEN --                                
         MVI   DAYPART,DDAY            DAYTIME                                  
         B     GETX                                                             
GETW3    CLI   FILESW,11                                                        
         BNE   GETW4                                                            
         GET   INW3,INREC                                                       
         MVI   FILETYP,WOMEN         -- WOMEN --                                
         MVI   DAYPART,DNEWS           NEWS                                     
         B     GETX                                                             
GETW4    CLI   FILESW,12                                                        
         BNE   GETINV                                                           
         GET   INW4,INREC                                                       
         MVI   FILETYP,WOMEN         -- WOMEN --                                
         MVI   DAYPART,DLATE           LATE NIGHT                               
         B     GETX                                                             
GETINV   DC    H'0'                    INVALID FILE                             
                                                                                
*                                                                               
GETX     DS    0C                                                               
         CLC   INREC+4(8),=C',,,,,,,,'  SKIP BLANK LINES                        
         BE    GET                                                              
         SR    R3,R3                                                            
         ICM   R3,3,INREC                                                       
         LA    R1,INREC                                                         
         LA    R1,0(R3,R1)                                                      
         MVI   0(R1),C','            ADD A COMMA AT END OF LINE                 
         CH    R3,=H'10'                                                        
         BL    GET                   LINE TOO SHORT. SKIP IT                    
         SH    R3,=H'4'                                                         
         CHI   R3,L'P                                                           
         BNH   *+8                                                              
         LHI   R3,L'P-1              DON'T OVERFLOW PRINTLINE                   
         MVC   P,SPACES                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),INREC+4          P HOLDS INPUT RECORD                       
*                                                                               
         XC    ONUMS,ONUMS              PARSE INPUT RECORD                      
PARSE    DS    0C                                                               
         XC    ARDEF,ARDEF                                                      
*                                                                               
         LA    R1,RECDEFS                                                       
RDEF     CLI   0(R1),X'FF'                                                      
         BE    RDEFX                                                            
         CLC   FILETYP,1(R1)                                                    
         BE    RDEF2                                                            
         ZIC   R3,0(R1)                                                         
         AR    R1,R3                                                            
         B     RDEF                                                             
RDEF2    LA    R1,2(R1)            GET PAST CONTROL INFO                        
         ST    R1,ARDEF                                                         
         MVC   RFIELD,0(R1)                                                     
RDEFX    DS    0C                                                               
*                                                                               
         LA    R1,INREC+4                                                       
         ICM   R3,3,INREC                                                       
         SH    R3,=H'4'                                                         
         LA    R3,1(R3)            1 EXTRA FOR END                              
         LA    R8,WORK2                                                         
         CLI   RFIELD,PNAM                                                      
         BNE   *+8                                                              
         LA    R8,PNAME                                                         
         CLI   RFIELD,TQNET                                                     
         BNE   *+8                                                              
         LA    R8,TVQNET                                                        
         CLI   RFIELD,TQPTYP                                                    
         BNE   *+8                                                              
         LA    R8,TVQPTYP                                                       
         CLI   RFIELD,TQNTI                                                     
         BNE   *+8                                                              
         LA    R8,TVQNTI                                                        
         CLI   RFIELD,TQDPT                                                     
         BNE   *+8                                                              
         LA    R8,TVQDPT                                                        
         ST    R8,SAVER8                                                        
         SR    R4,R4                                                            
         MVI   NUMSW,1                                                          
         MVC   PNAME,SPACES                                                     
         MVC   WORK2,SPACES                                                     
PARSE1   CLI   0(R1),C'"'                                                       
         BNE   PARSE5                                                           
         LA    R1,1(R1)                                                         
PARSE2   TM    0(R1),X'80'                                                      
         BZ    *+8                                                              
         OI    0(R1),X'40'                                                      
         CLI   RFIELD,200                                                       
         BL    *+10                                                             
         MVC   0(1,R8),0(R1)                                                    
         TM    0(R1),X'F0'                                                      
         BO    *+8                                                              
         MVI   ALPHA,1                                                          
         NI    0(R1),X'0F'                                                      
         ZIC   RE,0(R1)                                                         
         MH    R4,=H'10'                                                        
         AR    R4,RE                                                            
         LA    R1,1(R1)                                                         
         LA    R8,1(R8)                                                         
         CLI   0(R1),C'"'                                                       
         BE    *+8                                                              
         BCT   R3,PARSE2                                                        
         SR    R4,R4                                                            
         LA    R1,1(R1)                                                         
         BCT   R3,PARSE1                                                        
PARSE5   CLI   0(R1),C','                                                       
         BE    PARSE7                                                           
         TM    0(R1),X'80'                                                      
         BZ    *+8                                                              
         OI    0(R1),X'40'                                                      
         CLI   RFIELD,200                                                       
         BL    *+10                                                             
         MVC   0(1,R8),0(R1)                                                    
         TM    0(R1),X'F0'                                                      
         BO    *+8                                                              
         MVI   ALPHA,1                                                          
         NI    0(R1),X'0F'                                                      
         ZIC   RE,0(R1)                                                         
         MH    R4,=H'10'                                                        
         AR    R4,RE                                                            
         LA    R1,1(R1)                                                         
         LA    R8,1(R8)                                                         
         BCT   R3,PARSE1                                                        
         B     PARSEX                                                           
PARSE7   DS    0C                                                               
         L     RE,ARDEF                                                         
         CLI   0(RE),0                                                          
         BE    PARSE7A                                                          
         CLI   0(RE),199                                                        
         BH    PARSE7A                                                          
         CLI   ALPHA,1                                                          
         BE    PARSE7A                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,ONUMS-1(RF)           DISPLACEMENTS START AT 1                
         STC   R4,0(RF)                 PUT IT IN LAST BYTE                     
         B     PARSE7A                                                          
*                                                                               
PARSE7A  LA    RE,1(RE)                                                         
         ST    RE,ARDEF                                                         
         MVC   RFIELD,0(RE)                                                     
*                                                                               
         SR    R4,R4                                                            
         MVI   ALPHA,0                                                          
         LA    R8,WORK2                                                         
         CLI   RFIELD,PNAM                                                      
         BNE   *+8                                                              
         LA    R8,PNAME                                                         
         CLI   RFIELD,TQNET                                                     
         BNE   *+8                                                              
         LA    R8,TVQNET                                                        
         CLI   RFIELD,TQPTYP                                                    
         BNE   *+8                                                              
         LA    R8,TVQPTYP                                                       
         CLI   RFIELD,TQNTI                                                     
         BNE   *+8                                                              
         LA    R8,TVQNTI                                                        
         CLI   RFIELD,TQDPT                                                     
         BNE   *+8                                                              
         LA    R8,TVQDPT                                                        
         LA    R1,1(R1)                                                         
         CLI   RFIELD,X'FF'                                                     
         BE    PARSEX                                                           
         BCT   R3,PARSE1                                                        
PARSEX   DS    0C                                                               
*                                                                               
         MVI   TITLEQ,0               DETERMINE IF RECORD IS A TITLE            
         CLC   PNAME,SPACES           THIS IS A TITLE IF                        
         BE    TITLES                  FRST FLD=BLANK OR                        
         CLC   TVQNET,SPACES           FRST FLD=NOBLANK,SECND FLD=BLANK         
         BE    TITLES                                                           
         CLC   =C'NETWORK',TVQNET        OR                                     
         BE    TITLES                  NETWORK FIELD SAYS 'NETWORK'             
         B     *+8                                                              
TITLES   MVI   TITLEQ,1                                                         
*                                                                               
         CLI   TITLEQ,1               SKIP TITLES                               
         BE    NETVALX                                                          
*                                                                               
NETVAL   LA    R3,NETTAB              VALIDATE INPUT NETWORK                    
NETVAL1  CLI   0(R3),X'FF'                                                      
         BE    NETINVAL                                                         
         CLC   TVQNET,0(R3)                                                     
         BNE   NETVAL2                                                          
         MVC   DDSNET,L'TVQNET(R3)                                              
         B     NETVALX                                                          
NETVAL2  LA    R3,L'NETTAB(R3)                                                  
         B     NETVAL1                                                          
NETINVAL MVC   P,SPACES                                                         
         MVC   P(31),=C'***** ERROR * INVALID NETWORK: '                        
         MVC   P+31(L'TVQNET),TVQNET                                            
         GOTO1 =V(PRINTER)            PRINT INVALID ERROR                       
         MVI   RETCODE,RETERR         SET RETURN CODE                           
         B     GET                    SKIP RECORD                               
NETVALX  DS    0C                                                               
*                                                                               
*BUILD TABLE OF RECORDS AND ACCUMULATE ONUMS                                    
TABPROG  CLI   TITLEQ,1               DON'T PROCESS TITLES                      
         BE    TABPROGX                                                         
*                                                                               
         BAS   RE,CKNTIN              CK IF PROGRAM NUMBER IS NUMERIC           
         BE    *+10                                                             
         MVC   TVQNTI,=5C' '          NO. REPLACE BY SPACES                     
*                                                                               
         XC    WARNSW,WARNSW          CLEAR WARNING FLAG                        
         L     RE,=A(PROGBUFF)                                                  
         MVI   MEDTYP,MEDBROQ                                                   
         CLC   DDSNET(3),=C'SYN'                                                
         BNE   *+8                                                              
         MVI   MEDTYP,MEDSYNQ                                                   
         CLC   DDSNET(3),=C'CBL'                                                
         BNE   *+8                                                              
         MVI   MEDTYP,MEDCBLQ                                                   
         CLC   DDSNET(4),=C'NICK'                                               
         BNE   *+8                                                              
         MVI   MEDTYP,MEDCBLQ                                                   
         CLI   PNAME,C' '                                                       
         BE    TABPROGX                                                         
TABPROG1 CLI   0(RE),0                                                          
         BE    TABPRO2A                                                         
         CLC   MEDTYP,L'PNAME(RE)                                               
         BNE   TABNEXT                                                          
         CLC   DAYPART,L'PNAME+L'MEDTYP(RE)                                     
         BNE   TABNEXT                                                          
         CLC   TVQNTI,=5C' '                                                    
         BNE   TABPRO1B                                                         
*                                          - NO PROGRAM # -                     
TABPRO1A CLC   PNAME,0(RE)                 COMPARE FOR PROG NAME                
         BNE   TABNEXT                                                          
         CLC   DDSNET,TBQNET-TBSTART(RE)   SAME NETWORK?                        
         BNE   TABNEXT                                                          
         CLC   TBQNTI-TBSTART(5,RE),=CL5' '  NO NTI # IN TABLE EITHER           
         BE    TABPRO2B                                                         
         MVI   WARNSW,1                                                         
         B     TABNEXT                                                          
*                                           - PROGRAM # PRESENT -               
TABPRO1B CLC   DDSNET,TBQNET-TBSTART(RE)   SAME NETWORK?                        
         BNE   TABNEXT                                                          
         CLC   TVQNTI,TBQNTI-TBSTART(RE)   COMPARE FOR PROG#                    
         BE    TABPR1B                                                          
         CLC   PNAME,0(RE)                                                      
         BNE   TABNEXT                     SAME PROGRAM NAME, BUT               
         CLC   TBQNTI-TBSTART(5,RE),=CL5' '                                     
         BNE   *+12                                                             
         MVI   WARNSW,1                    SOME HAVE NO NTI#                    
         B     TABNEXT                                                          
         CLC   TVQDPT,TBQDPT-TBSTART(RE)                                        
         BNE   TABNEXT                                                          
         MVI   WARNSW,3                    SAME PROG NAME, DIFF NTI#            
         B     TABNEXT                                                          
TABPR1B  CLC   PNAME,0(RE)                WHEN SAME PROG#, SAME NETWORK         
         BE    TABPRO2B                      SAME PROGRAM NAME - OK             
         MVI   WARNSW,2                      DIFF PROG NAME-PRINT WARNG         
*                                                                               
TABNEXT  LA    RE,LNBUFF(RE)                                                    
         B     TABPROG1                                                         
TABPRO2A MVC   0(L'TBPNAME,RE),PNAME        BUILD TABLE ENTRY                   
         LA    RE,L'TBPNAME(RE)                                                 
         MVC   0(L'MEDTYP,RE),MEDTYP                                            
         LA    RE,L'MEDTYP(RE)                                                  
         MVC   0(L'DAYPART,RE),DAYPART                                          
         LA    RE,L'DAYPART(RE)                                                 
         MVC   0(L'TBQNET,RE),DDSNET                                            
         LA    RE,L'TBQNET(RE)                                                  
         MVC   0(L'TBQPTYP,RE),TVQPTYP                                          
         LA    RE,L'TBQPTYP(RE)                                                 
         MVC   0(L'TBQNTI,RE),TVQNTI                                            
         LA    RE,L'TBQNTI(RE)                                                  
         MVC   0(L'TBQDPT,RE),TVQDPT                                            
         LA    RE,L'TBQDPT(RE)                                                  
         B     *+8                                                              
TABPRO2B LA    RE,TBIQ-TBSTART(RE)     UPDATE ONUMS IN TABLE                    
         LA    RF,ONUMS                                                         
         LA    R2,L'ONUMS                                                       
TABPROG3 ZIC   R0,0(RF)                                                         
         ZIC   R1,0(RE)                                                         
         AR    R1,R0                                                            
         STC   R1,0(RE)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R2,TABPROG3                                                      
*                                                                               
TABPROGX DS    0C                                                               
         BAS   RE,PRTWARN             PRINT WARNINGS, IF ANY                    
*                                                                               
PARSEX6  XC    WARNSW,WARNSW          CLEAR WARNING FLAG                        
         B     GET                                                              
*                                                                               
ENDJOB   DS    0C                                                               
         B     CLSE                                                             
CLSE     CLI   FILESW,MAXFILQ      TOTAL NUMBER OF FILES                        
         BE    CLSE2                                                            
         ZIC   RE,FILESW                                                        
         LA    RE,1(RE)                                                         
         STC   RE,FILESW                                                        
         B     GET                                                              
*                                                                               
CLSE2    CLOSE (INA1)                                                           
         CLOSE (INA2)                                                           
         CLOSE (INA3)                                                           
         CLOSE (INA4)                                                           
         CLOSE (INM1)                                                           
         CLOSE (INM2)                                                           
         CLOSE (INM3)                                                           
         CLOSE (INM4)                                                           
         CLOSE (INW1)                                                           
         CLOSE (INW2)                                                           
         CLOSE (INW3)                                                           
         CLOSE (INW4)                                                           
         EJECT                                                                  
* GET RECORDS FROM TABLE AND CREATE OUTPUT                                      
         L     R8,=A(PROGBUFF)                                                  
OUTPROG  CLI   0(R8),0                                                          
         BE    OUTPROGX                                                         
         MVC   TBSTART(TBEND-TBSTART),0(R8)                                     
*                                                                               
         CLI   TBQNTI,C' '         NTI NUM OR NETWORK MISSING                   
         BE    *+8                  FORCE TO TVQ STATION                        
         CLI   TBQNET,C' '                                                      
         BE    *+8                                                              
         CLI   TBQNET,0                                                         
         BNE   HVTVQNET                                                         
         MVC   TBQNET,=C' NET  '                                                
         MVI   TBQNET,C'N'                                                      
         CLI   TBNET,1                                                          
         BNE   *+8                                                              
         MVI   TBQNET,C'S'                                                      
         CLI   TBNET,2                                                          
         BNE   *+8                                                              
         MVI   TBQNET,C'C'                                                      
HVTVQNET DS    0C                                                               
*                                                                               
         LA    RF,DPTSTAB                                                       
OUTDPT   CLI   0(RF),X'FF'                                                      
         BE    OUTDPT4                                                          
         CLC   TBDPT,0(RF)                                                      
         BE    OUTDPT4                                                          
         LA    RF,8(RF)                                                         
         B     OUTDPT                                                           
OUTDPT4  MVC   DPT3(3),1(RF)                                                    
         L     RF,4(RF)                                                         
         ST    RF,ADPTCUME                                                      
         XC    DNUMS,DNUMS         SET DAYPART FOR OUTPUT                       
         MVI   MULTISW,C'N'                                                     
         L     RF,ADPTCUME                                                      
         LA    RE,DNUMS                                                         
         LA    R1,IMAXQ+TMAXQ+FMAXQ                                             
DPMOVE   MVC   0(1,RE),3(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,DPMOVE                                                        
*                                                                               
MULTIPRG SR    RE,RE                                                            
         LA    RF,TBQNTI                                                        
CNVPNUM  C     RF,=A(TBQNTI+L'TBQNTI-1)    A(LAST BYTE)                         
         BH    CNVPNUMX                                                         
         TM    0(RF),X'F0'                                                      
         BNO   CNVPNUMX                                                         
         MH    RE,=H'10'                                                        
         NI    0(RF),X'0F'                                                      
         ZIC   R1,0(RF)                                                         
         AR    RE,R1                                                            
         OI    0(RF),X'F0'                                                      
         LA    RF,1(RF)                                                         
         B     CNVPNUM                                                          
CNVPNUMX STCM  RE,3,PNUM                                                        
         OC    PNUM,PNUM           ANY PROGRAM NUMBER                           
         BNZ   PNUMOK               OK                                          
         ZIC   RE,NOMATCH          NO - ASSIGN TEMP ONE                         
         LA    RE,1(RE)                                                         
         STC   RE,NOMATCH                                                       
         AH    RE,=H'28000'        SET TO A RANGE                               
         STCM  RE,3,PNUM                                                        
         MVC   P,SPACES                                                         
         LA    R3,P                                                             
         MVC   0(20,R3),=C'ASSIGNED PROGRAM NO:'                                
         LA    R3,20(R3)                                                        
         EDIT  PNUM,(6,(R3))                                                    
         LA    R3,6(R3)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         MVC   0(L'TBQNET,R3),TBQNET                                            
         LA    R3,L'TBQNET(R3)                                                  
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         MVC   0(L'TBPNAME,R3),TBPNAME                                          
         GOTO1 =V(PRINTER)         PRINT ASSIGNED PROGRAM NUMBERS               
PNUMOK   DS    0C                                                               
         SPACE 1                                                                
         L     RE,=A(OUTREC)                                                    
         L     RF,=F'1004'                                                      
         XCEF                                                                   
         L     RE,=A(OUTDATA)                                                   
         USING PMKEY,RE                                                         
         LA    R9,PMPNUM                                                        
         ST    R9,APMPNUM                                                       
         MVC   PMKEY(3),=C'QNN'                                                 
         MVC   PMBOOK,BOOK                                                      
         CLC   =C'UPN ',TBQNET     HANDLE CALL LETTER CHANGES                   
         BNE   *+10                                                             
         MVC   TBQNET(4),=C'PAR '                                               
         CLC   =C'ION ',TBQNET                                                  
         BNE   *+10                                                             
         MVC   TBQNET(4),=C'PAX '                                               
         CLC   =C'UMA ',TBQNET                                                  
         BNE   *+10                                                             
         MVC   TBQNET(4),=C'TF  '                                               
         MVC   PMSTAT(4),TBQNET                                                 
         MVI   PMSTAT+4,C'N'       LAST CHAR OF STATION IS 'N' EXCEPT:          
         CLI   TBNET,1               SYND -> LAST CHAR = 'M'                    
         BNE   *+8                                                              
         MVI   PMSTAT+4,C'M'                                                    
         CLI   TBNET,2               CABLE AND NICK -> LAST CHAR = 'C'          
         BNE   *+8                                                              
         MVI   PMSTAT+4,C'C'                                                    
         MVI   PMBTYP,C'U'                                                      
         MVC   PMPNUM,PNUM                                                      
         MVC   PMRLEN,=H'23'                                                    
         LA    RF,PMDATA                                                        
         USING MARELEM,RF                                                       
         MVC   0(6,RF),=X'010800C800C4'                                         
         LR    R2,RE                                                            
         LR    R3,RF                                                            
         GOTO1 =V(DATCON),DMCB,(5,0),(2,HALF)                                   
         LR    RE,R2                    RESTORE REGISTERS                       
         LR    RF,R3                                                            
         MVC   MARDATE,HALF                                                     
         ICM   R4,3,PMRLEN                                                      
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         DROP  RF                                                               
         USING PHTELEM,RF                                                       
         LA    R9,PHTDTYP                                                       
         ST    R9,APHTDTYP                                                      
         MVI   PHTCODE,PHTCODEQ                                                 
         MVI   PHTLEN,PHTLNEQ                                                   
         MVI   PHTDTYP,X'09'                                                    
         CLI   MULTISW,C'Y'                                                     
         BNE   *+8                                                              
         MVI   PHTDTYP,X'31'                                                    
         MVC   PHTPTYP(1),DPT3                                                  
         ZIC   R5,1(RF)                                                         
         AR    RF,R5                                                            
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         DROP  RF                                                               
         USING PHELEM,RF                                                        
         LA    R9,PHPNUM                                                        
         ST    R9,APHPNUM                                                       
         MVC   PHPNUM,PNUM                                                      
         MVI   PHCODE,PHCODEQ                                                   
         MVI   PHELN,PHELNEQ                                                    
         MVI   PHDWKS,X'0F'                                                     
         MVC   PHDBOOK,BOOK                                                     
         ICM   R4,3,PMRLEN                                                      
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         DROP  RF                                                               
         USING PPNELEM,RF                                                       
         MVI   PPNCODE,PPNCODEQ                                                 
         MVI   PPNELN,24+2                                                      
         MVC   PPNNME(24),TBPNAME                                               
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         DROP  RF                                                               
         USING NTELEM,RF                                                        
         MVC   0(10,RF),=X'221F24283C05DC06407C'                                
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         DROP  RF                                                               
         USING SLELEM,RF                                                        
         MVC   SLCODE(3),=X'230300'                                             
         MVI   SLSECT,171                                                       
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         DROP  RF                                                               
         USING PPEELEM,RF                                                       
         MVI   0(RF),X'41'                                                      
         MVI   1(RF),3+IMAXQ                                                    
         MVI   2(RF),X'41'                                                      
         MVC   3(IMAXQ,RF),TBIQ                                                 
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         MVI   0(RF),X'43'                                                      
         MVI   1(RF),3+TMAXQ                                                    
         MVI   2(RF),X'41'                                                      
         MVC   3(TMAXQ,RF),TBTVQ                                                
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         MVI   0(RF),X'45'                                                      
         MVI   1(RF),3+FMAXQ                                                    
         MVI   2(RF),X'41'                                                      
         MVC   3(FMAXQ,RF),TBFT                                                 
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         DROP  RF                                                               
         MVC   0(2,RF),=X'5E07'        '5E' ELEMENT                             
         MVC   2(3,RF),=C'TVQ'                                                  
         MVC   5(2,RF),=AL2(OCT_07)     NEW DEMDISP TABLE START DATE            
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         LA    R4,4(R4)                                                         
         STCM  R4,3,OUTREC                                                      
         PUT   OUT,OUTREC                                                       
         L     R9,=A(DP1)                                                       
OUTPROG5 CLC   0(2,R9),=X'FFFF'                                                 
         BE    OUTPROG8                                                         
         CLC   2(4,R9),TBQNET                                                   
         BNE   OUTPROG7                                                         
         CLC   6(2,R9),PNUM                                                     
         BNE   OUTPROG7                                                         
         LA    R9,6(R9)                                                         
OUTPROG6 CLC   0(2,R9),=X'FFFF'    REPLICATE PROGRAM                            
         BE    OUTPROG8                                                         
         L     RF,APMPNUM                                                       
         MVC   0(2,RF),PNUM                                                     
         L     RF,APHTDTYP                                                      
         MVI   0(RF),X'31'                                                      
         L     RF,APHPNUM                                                       
         MVC   0(2,RF),PNUM                                                     
         PUT   OUT,OUTREC                                                       
         LA    R9,2(R9)                                                         
         B     OUTPROG6                                                         
*                                                                               
OUTPROG7 SR    RF,RF                                                            
         ICM   RF,3,0(R9)          TRY NEXT PROGRAM                             
         AR    R9,RF                                                            
         B     OUTPROG5                                                         
*                                                                               
OUTPROG8 LA    R8,LNBUFF(R8)                                                    
         B     OUTPROG                                                          
*                                                                               
OUTPROGX CLOSE (OUT)                                                            
*                                                                               
* RETURN CODE IS PASSED TO BE CHECKED BY THE SUBSEQUENT JOB STEPS.              
*                                                                               
* FOR EXAMPLE, IF A ERROR IS GENERATED THE RETURN CODE INDICATES                
* THAT THE LOAD STEP SHOULDN'T RUN.                                             
*                                                                               
* WARNINGS ARE LESS SEVERE THAT ERRORS AND HAVE A DIFFERENT RET CODE.           
* HOWEVER, DATA WILL NOT BE LOADED UNLESS CLIENT CONFIRMS THEY REALLY           
* WANT IT THAT WAY.  IN THAT CASE, THE "COND" STATEMENT IN THE JCL              
* JOB SHOULD BE CHANGED TO ALLOW DATA TO BE LOADED REGARDLESS OF THE            
* WARNINGS.                                                                     
*                                                                               
EXIT     ZIC   RF,RETCODE                                                       
         XBASE RC=(RF)                                                          
         EJECT                                                                  
***********************************************************************         
* CKNTIN - CHECK IF PROGRAM NUMBER IS VALID NUMERIC                             
***********************************************************************         
CKNTIN   NTR1                                                                   
         LA    RE,TVQNTI                                                        
         LA    RF,TVQNTI+L'TVQNTI                                               
*                                                                               
CKNTI5   CR    RE,RF                                                            
         BNL   VALNUM                                                           
         CLI   0(RE),C' '            SPACES ARE VALID                           
         BE    CKNTI8                                                           
         TM    0(RE),X'F0'           NUMERIC CHAR?                              
         BNO   INVNUM                                                           
CKNTI8   LA    RE,1(RE)                                                         
         B     CKNTI5                                                           
*                                                                               
VALNUM   CR    RB,RB                                                            
         B     XIT                                                              
INVNUM   CHI   RB,0                                                             
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PRTWARN - PRINT WARNINGS                                                      
***********************************************************************         
PRTWARN  NTR1                                                                   
WARN1    CLI   WARNSW,1                                                         
         BNE   WARN2                                                            
**       GOTO1 =V(PRINTER)         PRINT PROBLEM LINE                           
         MVC   P,SPACES                                                         
         MVC   P(46),=C'***** WARNING * SAME PROGRAM W/ AND W/O NTI#: '         
         MVC   P+46(L'PNAME),PNAME                                              
         B     WARNPRNT                                                         
*                                                                               
WARN2    CLI   WARNSW,2                                                         
         BNE   WARN3                                                            
**       GOTO1 =V(PRINTER)         PRINT PROBLEM LINE                           
         MVC   P,SPACES                                                         
         MVC   P(42),=C'***** WARNING * DUPLICATE PROGRAM NUMBER: '             
         MVC   P+42(L'TVQNTI),TVQNTI                                            
         B     WARNPRNT                                                         
*                                                                               
WARN3    CLI   WARNSW,3                                                         
         BNE   WARNINGX                                                         
**       GOTO1 =V(PRINTER)         PRINT PROBLEM LINE                           
         MVC   P,SPACES                                                         
         MVC   P(45),=C'***** WARNING * SAME PROGRAM W/ DIFF NTI #S: '          
         MVC   P+45(L'PNAME),PNAME                                              
         B     WARNPRNT                                                         
*                                                                               
WARNPRNT GOTO1 =V(PRINTER)         PRINT WARNING MESSAGE                        
*** DEIS CHANGED THESE INSTRUCTIONS. WE DON'T KNOW FOR SURE WHY                 
*** CERTAIN CONDITIONS WERE DEEMED WORTHY OF AN ERROR AS OPPOSED TO A           
*** WARNING, BUT WE DO BELIEVE THAT EACH TIME WE REPORTED A WARNING,            
*** CARAT REISSUED CORRECTED FILES WHICH CLEARED THE WARNING. SO TO             
*** MAKE MATTERS SIMPLER, WE ARE NOW CATEGORIZING ALL WARNINGS AS               
*** ERRORS, WITH A CORRESPONDING RETURN CODE OF 8.                              
*********CLI   RETCODE,RETWRN      DON'T OVERRIDE MORE SEVERE RET CODES         
*********BH    *+8                                                              
*********MVI   RETCODE,RETWRN      RETURN CONDITION CODE FOR WARNING            
         MVI   RETCODE,RETERR      RETURN CONDITION CODE FOR ERROR              
*                                                                               
WARNINGX B     XIT                                                              
         EJECT                                                                  
LNBUFF   EQU   TBEND-TBSTART                                                    
LNBUF2   EQU   CLEARE-CLEARS                                                    
         DC    CL8'**WORK**'                                                    
WARNSW   DC    X'0'                                                             
SAVER8   DC    A(0)                                                             
ARDEF    DC    A(0)                                                             
NOMATCH  DC    X'0'                                                             
FILESW   DS    X                                                                
TITLEQ   DS    X                                                                
ALPHA    DS    X                                                                
NUMSW    DS    X                                                                
RFIELD   DS    X                                                                
MEDTYP   DS    X                                                                
MEDBROQ  EQU   0                   BROADCAST                                    
MEDSYNQ  EQU   1                   SYNDICATION                                  
MEDCBLQ  EQU   2                   CABLE                                        
DAYPART  DS    X                                                                
MULTISW  DS    X                                                                
DPT3     DS    CL3                                                              
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
PNUM     DS    H                                                                
ADPTCUME DS    F                                                                
BOOK     DS    XL2                                                              
YRLN     DS    HL1                                                              
CYLN     DS    HL1                                                              
YEAR     DS    CL4                                                              
CYCLE    DS    CL4                                                              
MONTH    DS    CL3                                                              
WORK     DS    CL20                                                             
WORK2    DS    CL80                                                             
PNAME    DS    CL(PNAMLQ)                                                       
PNAMLQ   EQU   75                                                               
RETCODE  DS    X                   RETURN CODE                                  
RETOK    EQU   0                   OK                                           
RETWRN   EQU   4                   WARNING                                      
RETERR   EQU   8                   ERROR                                        
FILETYP  DS    X                                                                
ADULTS   EQU   1                                                                
MEN      EQU   2                                                                
WOMEN    EQU   3                                                                
         DC    CL8'*CLEARS*'                                                    
CLEARS   DS    0C                                                               
TVQNAME  DS    CL60                                                             
TVQNET   DS    CL10                                                             
DDSNET   DS    CL4                                                              
TVQPTYP  DS    CL4                                                              
TVQNTI   DS    CL5                                                              
TVQDPT   DS    CL6                                                              
CLEARE   DS    0C                                                               
*                                                                               
TBSTART  DS    0C                                                               
TBPNAME  DS    CL(PNAMLQ)                                                       
TBNET    DS    CL1                                                              
TBDPT    DS    CL1                                                              
TBQNET   DS    CL4                                                              
TBQPTYP  DS    CL4                                                              
TBQNTI   DS    CL5                                                              
TBQDPT   DS    CL6                                                              
TBIQ     DS    XL(IMAXQ)                                                        
TBTVQ    DS    XL(TMAXQ)                                                        
TBFT     DS    XL(FMAXQ)                                                        
TBEND    DS    0C                                                               
P2       DS    CL132                                                            
         DS    300C                                                             
         DC    CL8'**IREC**'                                                    
INREC    DS    CL800                                                            
         DC    CL8'**ONUM**'                                                    
ONUMS    DS    XL(IMAXQ+TMAXQ+FMAXQ)                                            
OPIQ     EQU   ONUMS                                                            
OPTVQ    EQU   ONUMS+IMAXQ                                                      
OPFT     EQU   ONUMS+IMAXQ+TMAXQ                                                
         DC    CL8'**DNUM**'                                                    
DNUMS    DS    XL(IMAXQ+TMAXQ+FMAXQ)                                            
DPIQ     EQU   DNUMS                                                            
DPTVQ    EQU   DNUMS+IMAXQ                                                      
DPFT     EQU   DNUMS+IMAXQ+TMAXQ                                                
         LTORG                                                                  
         EJECT                                                                  
INA1     DCB   DDNAME=INA1,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
INA2     DCB   DDNAME=INA2,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
INA3     DCB   DDNAME=INA3,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
INA4     DCB   DDNAME=INA4,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
INA5     DCB   DDNAME=INA5,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
INM1     DCB   DDNAME=INM1,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
INM2     DCB   DDNAME=INM2,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
INM3     DCB   DDNAME=INM3,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
INM4     DCB   DDNAME=INM4,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
INM5     DCB   DDNAME=INM5,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
INW1     DCB   DDNAME=INW1,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
INW2     DCB   DDNAME=INW2,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
INW3     DCB   DDNAME=INW3,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
INW4     DCB   DDNAME=INW4,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
INW5     DCB   DDNAME=INW5,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00500,                                            X        
               MACRF=GM                                                         
*                                                                               
OUT      DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=01004,                                            X        
               BLKSIZE=05000,                                          X        
               MACRF=PM                                                         
         EJECT                                                                  
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
MONTHTAB DC    C'JAN',AL1(01)                                                   
         DC    C'FEB',AL1(02)                                                   
         DC    C'MAR',AL1(03)                                                   
         DC    C'APR',AL1(04)                                                   
         DC    C'MAY',AL1(05)                                                   
         DC    C'JUN',AL1(06)                                                   
         DC    C'JUL',AL1(07)                                                   
         DC    C'AUG',AL1(08)                                                   
         DC    C'SEP',AL1(09)                                                   
         DC    C'OCT',AL1(10)                                                   
         DC    C'NOV',AL1(11)                                                   
         DC    C'DEC',AL1(12)                                                   
         DC    X'FF'                                                            
*                                                                               
NETTAB   DS    0CL(L'TVQNET+L'DDSNET)                                           
         DC    CL10'ABC       ',CL4'ABC '                                       
         DC    CL10'CW        ',CL4'CW  '                                       
         DC    CL10'CBS       ',CL4'CBS '                                       
         DC    CL10'FOX       ',CL4'FOX '                                       
         DC    CL10'MY NET    ',CL4'MNT '                                       
         DC    CL10'MYNETWORK ',CL4'MNT '                                       
         DC    CL10'NBC       ',CL4'NBC '                                       
         DC    CL10'NICK      ',CL4'NICK'                                       
         DC    CL10'PBS       ',CL4'PBS '                                       
         DC    CL10'SYN       ',CL4'SYN '                                       
         DC    CL10'UPN       ',CL4'UPN '                                       
         DC    CL10'ION       ',CL4'ION '                                       
         DC    CL10'THE WB    ',CL4'WB  '                                       
         DC    CL10'UMA       ',CL4'UMA '                                       
         DC    X'FF'                                                            
*                                                                               
         DS    F                                                                
DPTSTAB  DC    AL1(DPRIME),C'PRI',A(PRIME)       DAYPARTS TABLE                 
         DC    AL1(DDAY),C'DAY',A(DAY)                                          
         DC    AL1(DNEWS),C'NWS',A(NEWS)                                        
         DC    AL1(DSPORT),C'SPO',A(SPORTS)                                     
         DC    AL1(DLATE),C'LAT',A(LATE)                                        
         DC    X'FF',C'MSC',A(MISC)                                             
*                                                                               
DPRIME   EQU   1                                                                
DDAY     EQU   2                                                                
DNEWS    EQU   3                                                                
DSPORT   EQU   4                                                                
DLATE    EQU   5                                                                
*                                                                               
         EJECT                                                                  
RECDEFS  DS    0C                                                               
*                                                                               
RADULTS  DC    AL1(RADULTSE-*)              -ADULTS-                            
         DC    AL1(ADULTS)                                                      
         DC    AL1(PNAM)                   PROGRAM NAME                         
         DC    AL1(TQNET)                  NETWORK                              
         DC    AL1(TQPTYP)                 PROGRAM TYPE                         
         DC    AL1(TQNTI)                  NTI NUMBER                           
         DC    AL1(TQDPT)                  DAYPART                              
         DC    AL1(IV1299,TV1299,FV1299)   DEMOS                                
         DC    AL1(IV1217,TV1217,FV1217)                                        
         DC    AL1(IV1224,TV1224,FV1224)                                        
         DC    AL1(IV1234,TV1234,FV1234)                                        
         DC    AL1(IV1899,TV1899,FV1899)                                        
         DC    AL1(IV1834,TV1834,FV1834)                                        
         DC    AL1(IV3549,TV3549,FV3549)                                        
         DC    AL1(IV5099,TV5099,FV5099)                                        
         DC    AL1(IV1849,TV1849,FV1849)                                        
         DC    AL1(IV2554,TV2554,FV2554)                                        
         DC    AL1(IV2549,TV2549,FV2549)                                        
         DC    AL1(IV3599,TV3599,FV3599)                                        
         DC    AL1(IV3564,TV3564,FV3564)                                        
         DC    X'FF'                                                            
RADULTSE DS    0C                                                               
*                                                                               
RWOMEN   DC    AL1(RWOMENE-*)               -WOMEN-                             
         DC    AL1(WOMEN)                                                       
         DC    AL1(PNAM)                   PROGRAM NAME                         
         DC    AL1(TQNET)                  NETWORK                              
         DC    AL1(TQPTYP)                 PROGRAM TYPE                         
         DC    AL1(TQNTI)                  NTI NUMBER                           
         DC    AL1(TQDPT)                  DAYPART                              
         DC    AL1(IW1299,TW1299,FW1299)   DEMOS                                
         DC    AL1(IW1217,TW1217,FW1217)                                        
         DC    AL1(IW1224,TW1224,FW1224)                                        
         DC    AL1(IW1234,TW1234,FW1234)                                        
         DC    AL1(IW1899,TW1899,FW1899)                                        
         DC    AL1(IW1834,TW1834,FW1834)                                        
         DC    AL1(IW3549,TW3549,FW3549)                                        
         DC    AL1(IW5099,TW5099,FW5099)                                        
         DC    AL1(IW1849,TW1849,FW1849)                                        
         DC    AL1(IW2554,TW2554,FW2554)                                        
         DC    AL1(IW2549,TW2549,FW2549)                                        
         DC    AL1(IW3599,TW3599,FW3599)                                        
         DC    AL1(IW3564,TW3564,FW3564)                                        
         DC    X'FF'                                                            
RWOMENE  DS    0C                                                               
*                                                                               
RMEN     DC    AL1(RMENE-*)                 -MEN-                               
         DC    AL1(MEN)                                                         
         DC    AL1(PNAM)                   PROGRAM NAME                         
         DC    AL1(TQNET)                  NETWORK                              
         DC    AL1(TQPTYP)                 PROGRAM TYPE                         
         DC    AL1(TQNTI)                  NTI NUMBER                           
         DC    AL1(TQDPT)                  DAYPART                              
         DC    AL1(IM1299,TM1299,FM1299)   DEMOS                                
         DC    AL1(IM1217,TM1217,FM1217)                                        
         DC    AL1(IM1224,TM1224,FM1224)                                        
         DC    AL1(IM1234,TM1234,FM1234)                                        
         DC    AL1(IM1899,TM1899,FM1899)                                        
         DC    AL1(IM1834,TM1834,FM1834)                                        
         DC    AL1(IM3549,TM3549,FM3549)                                        
         DC    AL1(IM5099,TM5099,FM5099)                                        
         DC    AL1(IM1849,TM1849,FM1849)                                        
         DC    AL1(IM2554,TM2554,FM2554)                                        
         DC    AL1(IM2549,TM2549,FM2549)                                        
         DC    AL1(IM3599,TM3599,FM3599)                                        
         DC    AL1(IM3564,TM3564,FM3564)                                        
         DC    X'FF'                                                            
RMENE    DS    0C                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
PNAM     EQU   200                                                              
TQNAME   EQU   201                                                              
TQNET    EQU   202                                                              
TQPTYP   EQU   203                                                              
TQDPT    EQU   204                                                              
NTNET    EQU   205                                                              
NTNTI    EQU   206                                                              
NTFILT   EQU   207                                                              
NTLP     EQU   208                                                              
TQNTI    EQU   209                                                              
NULL     EQU   0                                                                
*                                                                               
**** IQ DISPLACEMENTS IN DEMDISP                                                
*                                                                               
IV1299   EQU   1                   AV1299                                       
IV1217   EQU   2                   AV1217                                       
IV1224   EQU   3                   AV1224                                       
IV1234   EQU   4                   AV1234                                       
IV1899   EQU   5                   AV1899                                       
IV1834   EQU   6                   AV1834                                       
IV3549   EQU   7                   AV3549                                       
IV5099   EQU   8                   AV5099                                       
IV1849   EQU   9                   AV1849                                       
IV2554   EQU   10                  AV2554                                       
IV2549   EQU   11                  AV2549                                       
IV3599   EQU   12                  AV3599                                       
IV3564   EQU   13                  AV3564                                       
IM1299   EQU   14                  AM1299                                       
IM1217   EQU   15                  AM1217                                       
IM1224   EQU   16                  AM1224                                       
IM1234   EQU   17                  AM1234                                       
IM1899   EQU   18                  AM1899                                       
IM1834   EQU   19                  AM1834                                       
IM3549   EQU   20                  AM3549                                       
IM5099   EQU   21                  AM5099                                       
IM1849   EQU   22                  AM1849                                       
IM2554   EQU   23                  AM2554                                       
IM2549   EQU   24                  AM2549                                       
IM3599   EQU   25                  AM3599                                       
IM3564   EQU   26                  AM3564                                       
IW1299   EQU   27                  AW1299                                       
IW1217   EQU   28                  AW1217                                       
IW1224   EQU   29                  AW1224                                       
IW1234   EQU   30                  AW1234                                       
IW1899   EQU   31                  AW1899                                       
IW1834   EQU   32                  AW1834                                       
IW3549   EQU   33                  AW3549                                       
IW5099   EQU   34                  AW5099                                       
IW1849   EQU   35                  AW1849                                       
IW2554   EQU   36                  AW2554                                       
IW2549   EQU   37                  AW2549                                       
IW3599   EQU   38                  AW3599                                       
IW3564   EQU   39                  AW3564                                       
IMAXQ    EQU   39                  CURRENT MAX OF IQ FACTORS                    
*                                                                               
**** TVQ DISPLACEMENTS IN DEMDISP                                               
*                                                                               
TV1299   EQU   IMAXQ+1             AV1299                                       
TV1217   EQU   IMAXQ+2             AV1217                                       
TV1224   EQU   IMAXQ+3             AV1224                                       
TV1234   EQU   IMAXQ+4             AV1234                                       
TV1899   EQU   IMAXQ+5             AV1899                                       
TV1834   EQU   IMAXQ+6             AV1834                                       
TV3549   EQU   IMAXQ+7             AV3549                                       
TV5099   EQU   IMAXQ+8             AV5099                                       
TV1849   EQU   IMAXQ+9             AV1849                                       
TV2554   EQU   IMAXQ+10            AV2554                                       
TV2549   EQU   IMAXQ+11            AV2549                                       
TV3599   EQU   IMAXQ+12            AV3599                                       
TV3564   EQU   IMAXQ+13            AV3564                                       
TM1299   EQU   IMAXQ+14            AM1299                                       
TM1217   EQU   IMAXQ+15            AM1217                                       
TM1224   EQU   IMAXQ+16            AM1224                                       
TM1234   EQU   IMAXQ+17            AM1234                                       
TM1899   EQU   IMAXQ+18            AM1899                                       
TM1834   EQU   IMAXQ+19            AM1834                                       
TM3549   EQU   IMAXQ+20            AM3549                                       
TM5099   EQU   IMAXQ+21            AM5099                                       
TM1849   EQU   IMAXQ+22            AM1849                                       
TM2554   EQU   IMAXQ+23            AM2554                                       
TM2549   EQU   IMAXQ+24            AM2549                                       
TM3599   EQU   IMAXQ+25            AM3599                                       
TM3564   EQU   IMAXQ+26            AM3564                                       
TW1299   EQU   IMAXQ+27            AW1299                                       
TW1217   EQU   IMAXQ+28            AW1217                                       
TW1224   EQU   IMAXQ+29            AW1224                                       
TW1234   EQU   IMAXQ+30            AW1234                                       
TW1899   EQU   IMAXQ+31            AW1899                                       
TW1834   EQU   IMAXQ+32            AW1834                                       
TW3549   EQU   IMAXQ+33            AW3549                                       
TW5099   EQU   IMAXQ+34            AW5099                                       
TW1849   EQU   IMAXQ+35            AW1849                                       
TW2554   EQU   IMAXQ+36            AW2554                                       
TW2549   EQU   IMAXQ+37            AW2549                                       
TW3599   EQU   IMAXQ+38            AW3599                                       
TW3564   EQU   IMAXQ+39            AW3564                                       
TMAXQ    EQU   39                  CURRENT MAX OF TVQ FACTORS                   
*                                                                               
**** FT DISPLACEMENTS IN DEMDISP                                                
*                                                                               
FV1299   EQU   IMAXQ+TMAXQ+1       AV1299                                       
FV1217   EQU   IMAXQ+TMAXQ+2       AV1217                                       
FV1224   EQU   IMAXQ+TMAXQ+3       AV1224                                       
FV1234   EQU   IMAXQ+TMAXQ+4       AV1234                                       
FV1899   EQU   IMAXQ+TMAXQ+5       AV1899                                       
FV1834   EQU   IMAXQ+TMAXQ+6       AV1834                                       
FV3549   EQU   IMAXQ+TMAXQ+7       AV3549                                       
FV5099   EQU   IMAXQ+TMAXQ+8       AV5099                                       
FV1849   EQU   IMAXQ+TMAXQ+9       AV1849                                       
FV2554   EQU   IMAXQ+TMAXQ+10      AV2554                                       
FV2549   EQU   IMAXQ+TMAXQ+11      AV2549                                       
FV3599   EQU   IMAXQ+TMAXQ+12      AV3599                                       
FV3564   EQU   IMAXQ+TMAXQ+13      AV3564                                       
FM1299   EQU   IMAXQ+TMAXQ+14      AM1299                                       
FM1217   EQU   IMAXQ+TMAXQ+15      AM1217                                       
FM1224   EQU   IMAXQ+TMAXQ+16      AM1224                                       
FM1234   EQU   IMAXQ+TMAXQ+17      AM1234                                       
FM1899   EQU   IMAXQ+TMAXQ+18      AM1899                                       
FM1834   EQU   IMAXQ+TMAXQ+19      AM1834                                       
FM3549   EQU   IMAXQ+TMAXQ+20      AM3549                                       
FM5099   EQU   IMAXQ+TMAXQ+21      AM5099                                       
FM1849   EQU   IMAXQ+TMAXQ+22      AM1849                                       
FM2554   EQU   IMAXQ+TMAXQ+23      AM2554                                       
FM2549   EQU   IMAXQ+TMAXQ+24      AM2549                                       
FM3599   EQU   IMAXQ+TMAXQ+25      AM3599                                       
FM3564   EQU   IMAXQ+TMAXQ+26      AM3564                                       
FW1299   EQU   IMAXQ+TMAXQ+27      AW1299                                       
FW1217   EQU   IMAXQ+TMAXQ+28      AW1217                                       
FW1224   EQU   IMAXQ+TMAXQ+29      AW1224                                       
FW1234   EQU   IMAXQ+TMAXQ+30      AW1234                                       
FW1899   EQU   IMAXQ+TMAXQ+31      AW1899                                       
FW1834   EQU   IMAXQ+TMAXQ+32      AW1834                                       
FW3549   EQU   IMAXQ+TMAXQ+33      AW3549                                       
FW5099   EQU   IMAXQ+TMAXQ+34      AW5099                                       
FW1849   EQU   IMAXQ+TMAXQ+35      AW1849                                       
FW2554   EQU   IMAXQ+TMAXQ+36      AW2554                                       
FW2549   EQU   IMAXQ+TMAXQ+37      AW2549                                       
FW3599   EQU   IMAXQ+TMAXQ+38      AW3599                                       
FW3564   EQU   IMAXQ+TMAXQ+39      AW3564                                       
FMAXQ    EQU   39                  CURRENT MAX OF FT FACTORS                    
*                                                                               
APMPNUM  DS    A                                                                
APHTDTYP DS    A                                                                
APHPNUM  DS    A                                                                
         DS    0F                                                               
         DC    CL8'*OUTREC*'                                                    
OUTREC   DS    XL4                                                              
OUTDATA  DS    XL1000                                                           
         EJECT                                                                  
* PROGRAM EXTENSIONS                                                            
DP1      DC    AL2(DP2-DP1)        ABC COLLEGE BASKETBALL                       
         DC    CL4'ABC'                                                         
         DC    AL2(2541)           BASE                                         
         DC    AL2(287,1629,287)                                                
         DC    X'FFFF'                                                          
DP2      DC    AL2(DP3-DP2)        ABC NFL PRE-SEASON GAME                      
         DC    CL4'ABC'                                                         
         DC    AL2(34665)          BASE                                         
         DC    AL2(34823)                                                       
         DC    X'FFFF'                                                          
DP3      DC    AL2(DP4-DP3)        ABC PGA TOUR                                 
         DC    CL4'ABC'                                                         
         DC    AL2(1401)           BASE                                         
         DC    AL2(22625,22627,2528,36261,36263,6689,37671,37672)               
         DC    AL2(55917,55855,1611,1612,114,1797,1792,32352,32335)             
         DC    X'FFFF'                                                          
DP4      DC    AL2(DP5-DP4)        CBS COLLEGE BASKETBALL                       
         DC    CL4'CBS'                                                         
         DC    AL2(2672)           BASE                                         
         DC    AL2(30632,56695,2672,30632,56695,2799,315,361,1598)              
         DC    AL2(1599,1601,2848,25366,1350,2768,2769,25147,25148)             
         DC    AL2(25136,25137,2754,2755,24972,24973,24974,11814)               
         DC    AL2(24976,24977,24978,24946,24947,24940,24941,24948)             
         DC    AL2(24949,24942,24943,25146,30632,483,3016,3014,1408)            
         DC    X'FFFF'                                                          
DP5      DC    AL2(DP6-DP5)        FOX NFL PRE-SEASON GAME                      
         DC    CL4'FOX'                                                         
         DC    AL2(53186)          BASE                                         
         DC    AL2(53505)                                                       
         DC    X'FFFF'                                                          
DP6      DC    AL2(DP7-DP6)        FOX NHL HOCKEY                               
         DC    CL4'FOX'                                                         
         DC    AL2(333)            BASE                                         
         DC    X'FFFF'                                                          
DP7      DC    AL2(DP8-DP7)        FOX SATURDAY BASEBALL                        
         DC    CL4'FOX'                                                         
         DC    AL2(713)            BASE                                         
         DC    X'FFFF'                                                          
DP8      DC    AL2(DP9-DP8)        NBC COLLEGE FOOTBALL                         
         DC    CL4'NBC'                                                         
         DC    AL2(35223)          BASE                                         
         DC    AL2(261)                                                         
         DC    X'FFFF'                                                          
DP9      DC    AL2(DP10-DP9)       NBC COLLEGE BASKETBALL                       
         DC    CL4'NBC'                                                         
         DC    AL2(3717)           BASE                                         
         DC    X'FFFF'                                                          
DP10     DC    AL2(DP11-DP10)      NBC NBA GAME                                 
         DC    CL4'NBC'                                                         
         DC    AL2(1221)           BASE                                         
         DC    AL2(1223,63839,1499,2687,23982,24178,25482,38922)                
         DC    AL2(23982,24178,23982,24178,23982,24178)                         
         DC    X'FFFF'                                                          
DP11     DC    AL2(DP12-DP11)      NBC WNBA GAME                                
         DC    CL4'NBC'                                                         
         DC    AL2(1992)           BASE                                         
         DC    AL2(2175)                                                        
         DC    X'FFFF'                                                          
DP12     DC    AL2(DP13-DP12)      NBC NFL PRE-SEASON                           
         DC    CL4'NBC'                                                         
         DC    AL2(27088)          BASE                                         
         DC    AL2(43672)                                                       
         DC    X'FFFF'                                                          
DP13     DC    AL2(DP14-DP13)      NBC PGA TOUR                                 
         DC    CL4'NBC'                                                         
         DC    AL2(28436)          BASE                                         
         DC    AL2(28463,65211,65225,2673,2674,24296,24297,48445)               
         DC    AL2(48392,7479,7489,48798,48815,545,535)                         
         DC    X'FFFF'                                                          
DP14     DC    AL2(DP15-DP14)      OLYMPIC WINTERFEST                           
         DC    CL4'CBS'                                                         
         DC    AL2(02761)          BASE                                         
         DC    AL2(2512,2563)                                                   
         DC    X'FFFF'                                                          
DP15     DC    X'FFFF'                                                          
         EJECT                                                                  
         DC    CL8'**TOTS**'                                                    
SUMTABS  DS    0C                                                               
PRIME    DS    XL(4*(IMAXQ+TMAXQ+FMAXQ))                                        
DAY      DS    XL(4*(IMAXQ+TMAXQ+FMAXQ))                                        
NEWS     DS    XL(4*(IMAXQ+TMAXQ+FMAXQ))                                        
SPORTS   DS    XL(4*(IMAXQ+TMAXQ+FMAXQ))                                        
LATE     DS    XL(4*(IMAXQ+TMAXQ+FMAXQ))                                        
MISC     DS    XL(4*(IMAXQ+TMAXQ+FMAXQ))                                        
SUMTABE  DS    0C                                                               
*                                                                               
         DS    0D                                                               
         DS    C'PROGBUFF'                                                      
PROGBUFF DS    220000C                                                          
         DC    X'00'                                                            
                                                                                
MONTHTD  DSECT                                                                  
MONALPH  DS    CL3                                                              
MONNUM   DS    AL1                                                              
MONTHTDL EQU   *-MONTHTD                                                        
                                                                                
         EJECT                                                                  
* ++INCLUDE DDDPRINT                                                            
* ++INCLUDE DEDEMFILE                                                           
* ++INCLUDE DDMONYREQU                                                          
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDMONYREQU                                                     
         EJECT                                                                  
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DETVQ7    03/25/16'                                      
         END                                                                    
