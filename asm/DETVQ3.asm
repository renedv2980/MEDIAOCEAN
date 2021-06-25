*          DATA SET DETVQ3     AT LEVEL 204 AS OF 08/23/00                      
*PHASE DETVQ3A                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE DEMTIME                                                                
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
TAPECOPY CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 500,TAPECOPY,=V(REGSAVE),R6,R7                                   
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     RE,=A(PROGBUFF)                                                  
         L     RF,=F'120000'                                                    
         XCEF                                                                   
         L     RE,=A(PROGBUF2)                                                  
         L     RF,=F'200000'                                                    
         XCEF                                                                   
         L     RE,=A(SUMTABS)                                                   
         LA    RF,SUMTABE-SUMTABS                                               
         XCEF                                                                   
*                                                                               
         LA    R1,PARSTAB                                                       
PAR1     LA    R2,5(R1)                                                         
         SR    R3,R3                                                            
         CLI   0(R1),X'FF'                                                      
         BE    PARX                                                             
PAR2     CLI   0(R2),C'!'                                                       
         BE    PAR3                                                             
         CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R3,1(R3)                                                         
         LA    R2,1(R2)                                                         
         B     PAR2                                                             
PAR3     LA    R3,6(R3)                                                         
         STC   R3,3(R1)                                                         
         AR    R1,R3                                                            
         B     PAR1                                                             
PARX     DS    0H                                                               
         OPEN  (INA1,(INPUT))                                                   
         OPEN  (INA2,(INPUT))                                                   
         OPEN  (INA3,(INPUT))                                                   
         OPEN  (INA4,(INPUT))                                                   
         OPEN  (INA5,(INPUT))                                                   
         OPEN  (INA6,(INPUT))                                                   
         OPEN  (INM1,(INPUT))                                                   
         OPEN  (INM2,(INPUT))                                                   
         OPEN  (INM3,(INPUT))                                                   
         OPEN  (INM4,(INPUT))                                                   
         OPEN  (INM5,(INPUT))                                                   
         OPEN  (INM6,(INPUT))                                                   
         OPEN  (INW1,(INPUT))                                                   
         OPEN  (INW2,(INPUT))                                                   
         OPEN  (INW3,(INPUT))                                                   
         OPEN  (INW4,(INPUT))                                                   
         OPEN  (INW5,(INPUT))                                                   
         OPEN  (INW6,(INPUT))                                                   
         OPEN  (IN5,(INPUT))                                                    
         MVI   GETSW,1                                                          
*        OPEN  (OUT,(OUTPUT))                                                   
GET      MVI   CLEARS,C' '                                                      
         MVC   CLEARS+1(CLEARE-CLEARS-1),CLEARS                                 
         CLI   GETSW,1                                                          
         BNE   GETA2                                                            
         GET   INA1,INREC                                                       
         B     GETX                                                             
GETA2    CLI   GETSW,2                                                          
         BNE   GETA3                                                            
         GET   INA2,INREC                                                       
         B     GETX                                                             
GETA3    CLI   GETSW,3                                                          
         BNE   GETA4                                                            
         GET   INA3,INREC                                                       
         B     GETX                                                             
GETA4    CLI   GETSW,4                                                          
         BNE   GETA5                                                            
         GET   INA4,INREC                                                       
         B     GETX                                                             
GETA5    CLI   GETSW,5                                                          
         BNE   GETA6                                                            
         GET   INA5,INREC                                                       
         B     GETX                                                             
GETA6    CLI   GETSW,6                                                          
         BNE   GETA8                                                            
         GET   INA6,INREC                                                       
         B     GETX                                                             
GETA8    DS    0C                                                               
         CLI   GETSW,7                                                          
         BNE   GETM2                                                            
         GET   INM1,INREC                                                       
         B     GETX                                                             
GETM2    CLI   GETSW,8                                                          
         BNE   GETM3                                                            
         GET   INM2,INREC                                                       
         B     GETX                                                             
GETM3    CLI   GETSW,9                                                          
         BNE   GETM4                                                            
         GET   INM3,INREC                                                       
         B     GETX                                                             
GETM4    CLI   GETSW,10                                                         
         BNE   GETM5                                                            
         GET   INM4,INREC                                                       
         B     GETX                                                             
GETM5    CLI   GETSW,11                                                         
         BNE   GETM6                                                            
         GET   INM5,INREC                                                       
         B     GETX                                                             
GETM6    CLI   GETSW,12                                                         
         BNE   GETM8                                                            
         GET   INM6,INREC                                                       
         B     GETX                                                             
GETM8    DS    0C                                                               
         CLI   GETSW,13                                                         
         BNE   GETW2                                                            
         GET   INW1,INREC                                                       
         B     GETX                                                             
GETW2    CLI   GETSW,14                                                         
         BNE   GETW3                                                            
         GET   INW2,INREC                                                       
         B     GETX                                                             
GETW3    CLI   GETSW,15                                                         
         BNE   GETW4                                                            
         GET   INW3,INREC                                                       
         B     GETX                                                             
GETW4    CLI   GETSW,16                                                         
         BNE   GETW5                                                            
         GET   INW4,INREC                                                       
         B     GETX                                                             
GETW5    CLI   GETSW,17                                                         
         BNE   GETW6                                                            
         GET   INW5,INREC                                                       
         B     GETX                                                             
GETW6    CLI   GETSW,18                                                         
         BNE   GETW7                                                            
         GET   INW6,INREC                                                       
         B     GETX                                                             
GETW7    CLI   GETSW,19                                                         
         BNE   GETW8                                                            
         GET   IN5,INREC                                                        
GETW8    DS    0C                                                               
         MVC   PNAME(07),=C'****IN5'                                            
         MVC   HX(2),=X'0001'                                                   
         B     GETX                                                             
GETX     DS    0C                                                               
*        MVC   P(120),INREC+4                                                   
*        GOTO1 =V(PRINTER)                                                      
         CLC   INREC+4(8),=C',,,,,,,,'                                          
         BE    GET                                                              
         ICM   R3,3,INREC                                                       
         LA    R1,0(R3,R1)                                                      
         MVI   0(R1),C','                                                       
         CH    R3,=H'10'                                                        
         BL    GET                                                              
         SH    R3,=H'4'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),INREC+4                                                     
*                                                                               
         LA    R1,PARSTAB                                                       
         MVI   MATCH,0                                                          
*                                                                               
         LA    R2,P                                                             
         CLI   0(R2),C' '                                                       
         BNE *+12                                                               
         LA    R2,1(R2)                                                         
         B     *-12                                                             
*                                                                               
         XC    ONUMS,ONUMS                                                      
MATCH1   CLI   0(R1),X'FF'                                                      
         BE    MATCHX                                                           
         SR    R3,R3                                                            
         IC    R3,3(R1)                                                         
         SH    R3,=H'7'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),5(R1)                                                    
         BE    MATCH2                                                           
         AH    R3,=H'7'                                                         
         AR    R1,R3                                                            
         B     MATCH1                                                           
MATCH2   MVC   P2+6(125),P                                                      
         MVC   P(132),P2                                                        
         MVI   P2,C' '                                                          
         MVC   P2+1(131),P2                                                     
         MVC   P(2),0(R1)                                                       
         MVC   P+2(4),=C'****'                                                  
         CLC   =C'HN',0(R1)        NET/SYND/CAN                                 
         BNE   *+10                                                             
         MVC   HN,4(R1)                                                         
         CLC   =C'HX',0(R1)                                                     
         BNE   *+10                                                             
         MVC   HX,4(R1)                                                         
         CLC   =C'HA',0(R1)                                                     
         BNE   *+10                                                             
         MVC   HA,4(R1)                                                         
         CLC   =C'GS',0(R1)                                                     
         BNE   *+10                                                             
         MVC   GS,4(R1)                                                         
         CLC   =C'HA',0(R1)                                                     
         BNE   *+14                                                             
         MVC   P+5(1),4(R1)                                                     
         OI    P+5,X'F0'                                                        
         MVC   P+4(1),HX                                                        
         OI    P+4,X'F0'                                                        
         MVI   MATCH,1                                                          
MATCHX   DS    0C                                                               
*                                                                               
PARSE    DS    0C                                                               
         LA    R2,P                                                             
         XC    ARDEF,ARDEF                                                      
         CLI   MATCH,1                                                          
         BE    PARSEX                                                           
*                                                                               
         LA    R1,RECDEFS                                                       
RDEF     CLI   0(R1),X'FF'                                                      
         BE    RDEFX                                                            
         CLC   HX(2),1(R1)                                                      
         BE    RDEF2                                                            
         ZIC   R3,0(R1)                                                         
         AR    R1,R3                                                            
         B     RDEF                                                             
RDEF2    LA    R1,3(R1)            GET PAST CONTROL INFO                        
         ST    R1,ARDEF                                                         
         MVC   RFIELD,0(R1)                                                     
RDEFX    DS    0C                                                               
*                                                                               
         MVI   P,C' '                                                           
         MVC   P+1(131),P                                                       
         LA    R1,INREC+4                                                       
         ICM   R3,3,INREC                                                       
         SH    R3,=H'4'                                                         
         CLI   GETSW,19                                                         
         BE    *+8                                                              
         LA    R3,1(R3)            1 EXTRA FOR END                              
         LA    R8,WORK2                                                         
         CLI   RFIELD,PNAM                                                      
         BNE   *+8                                                              
         LA    R8,PNAME                                                         
         CLI   RFIELD,TQNAME                                                    
         BNE   *+8                                                              
         LA    R8,TVQNAME                                                       
         CLI   RFIELD,TQNET                                                     
         BNE   *+8                                                              
         LA    R8,TVQNET                                                        
         CLI   RFIELD,TQPTYP                                                    
         BNE   *+8                                                              
         LA    R8,TVQPTYP                                                       
         CLI   RFIELD,TQDPT                                                     
         BNE   *+8                                                              
         LA    R8,TVQDPT                                                        
         CLI   RFIELD,NTNET                                                     
         BNE   *+8                                                              
         LA    R8,NTINET                                                        
         CLI   RFIELD,NTNTI                                                     
         BNE   *+8                                                              
         LA    R8,NTINTI                                                        
         CLI   RFIELD,NTFILT                                                    
         BNE   *+8                                                              
         LA    R8,NTIFILT                                                       
         CLI   RFIELD,NTLP                                                      
         BNE   *+8                                                              
         LA    R8,NTILP                                                         
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
         MVC   0(1,R2),0(R1)                                                    
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
         LA    R2,1(R2)                                                         
         LA    R8,1(R8)                                                         
         CLI   0(R1),C'"'                                                       
         BE    *+8                                                              
         BCT   R3,PARSE2                                                        
         MVI   0(R2),C'º'                                                       
         SR    R4,R4                                                            
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         BCT   R3,PARSE1                                                        
PARSE5   CLI   0(R1),C','                                                       
         BE    PARSE7                                                           
         TM    0(R1),X'80'                                                      
         BZ    *+8                                                              
         OI    0(R1),X'40'                                                      
         MVC   0(1,R2),0(R1)                                                    
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
         LA    R2,1(R2)                                                         
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
         LA    RF,ONUMS(RF)                                                     
         STC   R4,0(RF)                                                         
         B     PARSE7A                                                          
*                                                                               
PARSE7A  LA    RE,1(RE)                                                         
         ST    RE,ARDEF                                                         
         MVC   RFIELD,0(RE)                                                     
*                                                                               
         MVI   0(R2),C'º'                                                       
         SR    R4,R4                                                            
         MVI   ALPHA,0                                                          
         LA    R8,WORK2                                                         
         CLI   RFIELD,PNAM                                                      
         BNE   *+8                                                              
         LA    R8,PNAME                                                         
         CLI   RFIELD,TQNAME                                                    
         BNE   *+8                                                              
         LA    R8,TVQNAME                                                       
         CLI   RFIELD,TQNET                                                     
         BNE   *+8                                                              
         LA    R8,TVQNET                                                        
         CLI   RFIELD,TQPTYP                                                    
         BNE   *+8                                                              
         LA    R8,TVQPTYP                                                       
         CLI   RFIELD,TQDPT                                                     
         BNE   *+8                                                              
         LA    R8,TVQDPT                                                        
         CLI   RFIELD,NTNET                                                     
         BNE   *+8                                                              
         LA    R8,NTINET                                                        
         CLI   RFIELD,NTNTI                                                     
         BNE   *+8                                                              
         LA    R8,NTINTI                                                        
         CLI   RFIELD,NTLP                                                      
         BNE   *+8                                                              
         LA    R8,NTILP                                                         
         CLI   RFIELD,NTFILT                                                    
         BNE   *+8                                                              
         LA    R8,NTIFILT                                                       
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         CLI   RFIELD,X'FF'                                                     
         BE    PARSEX                                                           
         BCT   R3,PARSE1                                                        
PARSEX   DS    0C                                                               
*                                                                               
TABPROG  CLI   MATCH,0                                                          
         BNE   TABPROGX                                                         
         CLI   GETSW,19                                                         
         BE    TVQPROG                                                          
         L     RE,=A(PROGBUFF)                                                  
         MVI   HN,0                                                             
         CLC   TVQNET(3),=C'SYN'                                                
         BNE   *+8                                                              
         MVI   HN,1                                                             
         CLC   TVQNET(3),=C'CBL'                                                
         BNE   *+8                                                              
         MVI   HN,2                                                             
         CLC   TVQNET(4),=C'NICK'                                               
         BNE   *+8                                                              
         MVI   HN,2                                                             
         CLI   PNAME,C' '                                                       
         BE    TABPROGX                                                         
TABPROG1 CLI   0(RE),0                                                          
         BE    TABPROG2                                                         
         CLC   HN(2),L'PNAME(RE)                                                
         BNE   *+10                                                             
         CLC   PNAME,0(RE)                                                      
         BE    TABPROG2                                                         
*        LA    RE,L'PNAME+L'ONUMS(RE)                                           
         LA    RE,LNBUFF(RE)                                                    
         B     TABPROG1                                                         
TABPROG2 MVC   0(L'PNAME,RE),PNAME                                              
         MVC   L'PNAME(2,RE),HN                                                 
         LA    RF,ONUMS                                                         
         LA    RE,L'PNAME+2(RE)                                                 
         LA    R2,L'ONUMS                                                       
TABPROG3 ZIC   R0,0(RF)                                                         
         ZIC   R1,0(RE)                                                         
         AR    R1,R0                                                            
         STC   R1,0(RE)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R2,TABPROG3                                                      
TABPROGX DS    0C                                                               
*        CLI   MATCH,1                                                          
*        BE    *+6                                                              
*        DC    H'0'                                                             
         CLI   MATCH,1                                                          
*        BNE   PARSEX6                                                          
         GOTO1 =V(PRINTER)                                                      
PARSEX6  MVI   P,C' '                                                           
         MVC   P+1(131),P                                                       
         B     GET                                                              
TVQPROG DS    0C                                                                
         L     RE,=A(PROGBUF2)                                                  
         CLI   TVQNAME,C' '                                                     
         BE    TVQPROGX                                                         
TVQPROG1 CLI   0(RE),0                                                          
         BE    TVQPROG2                                                         
         CLC   TVQNET(3),60(RE)                                                 
         BNE   *+14                                                             
         CLC   TVQNAME,0(RE)                                                    
         BE    TVQPROG2                                                         
         LA    RE,LNBUF2(RE)                                                    
         B     TVQPROG1                                                         
TVQPROG2 MVC   0(CLEARE-CLEARS,RE),TVQNAME                                      
TVQPROGX DS    0C                                                               
*        CLI   MATCH,1                                                          
*        BE    *+6                                                              
*        DC    H'0'                                                             
         CLI   MATCH,1                                                          
*        BNE   PARSEX6                                                          
         GOTO1 =V(PRINTER)                                                      
         B     GET                                                              
ENDJOB   DS    0C                                                               
         B     CLSE                                                             
CLSE     CLI   GETSW,19                                                         
         BE    CLSE2                                                            
         ZIC   RE,GETSW                                                         
         LA    RE,1(RE)                                                         
         STC   RE,GETSW                                                         
         B     GET                                                              
*                                                                               
CLSE2    CLOSE (INA1)                                                           
         CLOSE (INA2)                                                           
         CLOSE (INA3)                                                           
         CLOSE (INA4)                                                           
         CLOSE (INA5)                                                           
         CLOSE (INA6)                                                           
         CLOSE (INM1)                                                           
         CLOSE (INM2)                                                           
         CLOSE (INM3)                                                           
         CLOSE (INM4)                                                           
         CLOSE (INM5)                                                           
         CLOSE (INM6)                                                           
         CLOSE (INW1)                                                           
         CLOSE (INW2)                                                           
         CLOSE (INW3)                                                           
         CLOSE (INW4)                                                           
         CLOSE (INW5)                                                           
         CLOSE (INW6)                                                           
         CLOSE (IN5)                                                            
         OPEN  (OUT,(OUTPUT))                                                   
         EJECT                                                                  
* TABLES ALL BUILT NOW MERGE NTI AND CALC TVQ INDEXES                           
         L     R8,=A(PROGBUFF)                                                  
         L     RF,=A(PROGBUF2)                                                  
MATPROG  CLC   0(24,R8),0(RF)                                                   
         BNE   MATPROG2                                                         
         MVC   CLEARS(CLEARE-CLEARS),0(RF)                                      
         MVC   P(24),0(R8)                                                      
         MVC   TBSTART(TBEND-TBSTART),0(R8)                                     
         CLI   TBNET,0                                                          
         BNE   MATPSC                                                           
         CLC   TVQNET(3),=C'SYN'                                                
         BE    MATPROG2                                                         
         CLC   TVQNET(3),=C'CBL'                                                
         BE    MATPROG2                                                         
MATPSC   CLI   TBNET,1                                                          
         BNE   *+14                                                             
         CLC   TVQNET(3),=C'SYN'                                                
         BNE   MATPROG2                                                         
         CLI   TBNET,2                                                          
         BNE   *+14                                                             
         CLC   TVQNET(3),=C'CBL'                                                
         BNE   MATPROG2                                                         
*                                                                               
*        CLI   TBNET,1             FORCE IQ FOR SYND AND CABLE                  
*        BE    *+8                                                              
*        CLI   TBNET,2                                                          
*        BNE   *+10                                                             
*        MVC   TBIQ+1(L'TBIQ-1),TBTVQ                                           
*                                                                               
         LA    RF,L'PNAME(R8)                                                   
         MVC   P+24(2),0(RF)                                                    
         OI    P+24,X'F0'                                                       
         OI    P+25,X'F0'                                                       
         MVC   P+27(100),CLEARS                                                 
*                                                                               
         B     MATPROG4                                                         
MATPROG2 CLI   0(R8),0                                                          
         BE    MATPROGX                                                         
         LA    RF,LNBUF2(RF)                                                    
         CLI   0(RF),0                                                          
         BNE   MATPROG                                                          
         MVC   P(24),0(R8)                                                      
         LA    RF,L'PNAME(R8)                                                   
         MVC   P+24(2),0(RF)                                                    
         OI    P+24,X'F0'                                                       
         OI    P+25,X'F0'                                                       
         MVC   P+27(16),=C'****NO MATCH****'                                    
MATPROG4 GOTO1 =V(PRINTER)                                                      
         L     RF,=A(PROGBUF2)                                                  
         LA    R8,LNBUFF(R8)                                                    
         B     MATPROG2                                                         
MATPROGX DS    0C                                                               
         SPACE 2                                                                
         MVC   P+2(09),=C'TAPE COPY'                                            
         GOTO1 =V(PRINTER)                                                      
* TABLES ALL BUILT NOW MERGE NTI AND CREAT OUTPUT                               
         L     R8,=A(PROGBUFF)                                                  
         L     RF,=A(PROGBUF2)                                                  
OUTPROG  CLC   0(24,R8),0(RF)                                                   
         BNE   OUTPROG2                                                         
         MVC   CLEARS(CLEARE-CLEARS),0(RF)                                      
         MVC   TBSTART(TBEND-TBSTART),0(R8)                                     
         CLI   TBNET,0                                                          
         BNE   OUTPSC                                                           
         CLC   TVQNET(3),=C'SYN'                                                
         BE    OUTPROG2                                                         
         CLC   TVQNET(3),=C'CBL'                                                
         BE    OUTPROG2                                                         
OUTPSC   CLI   TBNET,1                                                          
         BNE   *+14                                                             
         CLC   TVQNET(3),=C'SYN'                                                
         BNE   OUTPROG2                                                         
         CLI   TBNET,2                                                          
         BNE   *+14                                                             
         CLC   TVQNET(3),=C'CBL'                                                
         BNE   OUTPROG2                                                         
*                                                                               
         CLC   NTINET(3),=C'TVQ'   OLD MISSING STATION                          
         BE    *+8                                                              
         CLI   NTINTI,C' '         NTI NUM OR NETWORK MISSING                   
         BE    *+8                  FORCE TO TVQ STATION                        
         CLI   NTINET,C' '                                                      
         BE    *+8                                                              
         CLI   NTINET,0                                                         
         BNE   HVTVQNET                                                         
         MVC   NTINET,=C' NET  '                                                
         MVI   NTINET,C'N'                                                      
         CLI   TBNET,1                                                          
         BNE   *+8                                                              
         MVI   NTINET,C'S'                                                      
         CLI   TBNET,2                                                          
         BNE   *+8                                                              
         MVI   NTINET,C'C'                                                      
HVTVQNET DS    0C                                                               
*                                                                               
         CLI   NTILP,X'0'                                                       
         BE    *+8                                                              
         CLI   NTILP,C' '                                                       
         BNE   *+10                                                             
         MVC   NTILP(24),TBPNAME                                                
HVTVQPN  DS    0C                                                               
*                                                                               
         MVC   P(30),TBPNAME                                                    
         MVC   P+31(4),NTINET                                                   
         MVC   P+37(5),NTINTI                                                   
         MVC   P+42(24),NTILP                                                   
         LA    RF,DPTSTAB                                                       
OUTDPT   CLI   0(RF),X'FF'                                                      
         BE    OUTDPT4                                                          
         CLC   TBDPT,0(RF)                                                      
         BE    OUTDPT4                                                          
         LA    RF,8(RF)                                                         
         B     OUTDPT                                                           
OUTDPT4  MVC   P+73(3),1(RF)                                                    
         MVC   DPT3(3),1(RF)                                                    
         L     RF,4(RF)                                                         
         ST    RF,ADPTCUME                                                      
         EDIT  (B4,4(RF)),(5,P+80)                                              
         EDIT  (B1,TBIQ+1),(5,P+88)                                             
         EDIT  (B1,TBTVQ),(5,P+94)                                              
         B     OUTPROG4                                                         
OUTPROG2 CLI   0(R8),0                                                          
         BE    OUTPROGX                                                         
         LA    RF,LNBUF2(RF)                                                    
         CLI   0(RF),0                                                          
         BNE   OUTPROG                                                          
         B     OUTPROG5                                                         
         MVC   P(24),0(R8)                                                      
         LA    RF,L'PNAME(R8)                                                   
         MVC   P+24(2),0(RF)                                                    
         OI    P+24,X'F0'                                                       
         OI    P+25,X'F0'                                                       
         MVC   P+27(16),=C'****NO MATCH****'                                    
OUTPROG4 XC    DNUMS,DNUMS         SET DAYPART FOR OUTPUT                       
         MVI   MULTISW,C'N'                                                     
         L     RF,ADPTCUME                                                      
         LA    RE,DNUMS                                                         
         LA    R1,100                                                           
DPMOVE   MVC   0(1,RE),3(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,DPMOVE                                                        
*                                                                               
         GOTO1 =V(PRINTER)                                                      
MULTIPRG SR    RE,RE                                                            
         LA    RF,NTINTI                                                        
CNVPNUM  TM    0(RF),X'F0'         BINARY PROG NUMBER FOR KEY                   
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
         MVC   PMBOOK,=X'6305'                                                  
         CLC   =C'UPN ',NTINET                                                  
         BNE   *+10                                                             
         MVC   NTINET(4),=C'PAR '                                               
         MVC   PMSTAT(4),NTINET                                                 
         MVI   PMSTAT+4,C'N'                                                    
         CLI   TBNET,1                                                          
         BNE   *+8                                                              
         MVI   PMSTAT+4,C'M'                                                    
         CLI   TBNET,2             BYPASS CABLE                                 
         BNE   *+8                                                              
         MVI   PMSTAT+4,C'C'                                                    
         MVI   PMBTYP,C'U'                                                      
         MVC   PMPNUM,PNUM                                                      
*        OC    PNUM,PNUM           MUST BE EQUATED                              
*        BZ    OUTPROG5                                                         
         MVC   PMRLEN,=H'23'                                                    
         LA    RF,PMDATA                                                        
         USING MARELEM,RF                                                       
         MVC   0(8,RF),=X'010800C800C452'                                       
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
*        MVC   PHTPTYP(2),NTIFILT                                               
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
         MVC   PHDWKS,X'0F'                                                     
         MVC   PHDBOOK,=X'6305'                                                 
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
         CLI   TBPNAME,0                                                        
         BNE   *+10                                                             
         MVC   PPNNME(24),TVQNAME                                               
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
         MVI   1(RF),3+40                                                       
         MVI   2(RF),X'41'                                                      
         MVC   3(40,RF),TBIQ+1                                                  
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         MVI   0(RF),X'43'                                                      
         MVI   1(RF),3+40                                                       
         MVI   2(RF),X'41'                                                      
         MVC   3(40,RF),TBTVQ                                                   
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         MVI   0(RF),X'45'                                                      
         MVI   1(RF),3+40                                                       
         MVI   2(RF),X'41'                                                      
         MVC   3(40,RF),TBFT                                                    
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         DROP  RF                                                               
         MVC   0(7,RF),=X'5E07D5D5D56209'                                       
         MVC   2(3,RF),=C'TVQ'                                                  
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         LA    R4,4(R4)                                                         
         STCM  R4,3,OUTREC                                                      
*        CLC   NTINET(3),=C'ABC'                                                
*        BNE   OUTPROG8                                                         
         PUT   OUT,OUTREC                                                       
         L     R9,=A(DP1)                                                       
OUTPROG5 CLC   0(2,R9),=X'FFFF'                                                 
         BE    OUTPROG8                                                         
         CLC   2(4,R9),NTINET                                                   
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
*        DC    H'0'                                                             
*                                                                               
OUTPROG8 L     RF,=A(PROGBUF2)                                                  
         MVC   P,SPACES                                                         
         LA    R8,LNBUFF(R8)                                                    
         B     OUTPROG2                                                         
OUTPROGX DS    0C                                                               
         CLOSE (OUT)                                                            
         XBASE                                                                  
         EJECT                                                                  
LNBUFF   EQU   L'PNAME+L'HN+L'GS+L'ONUMS                                        
LNBUF2   EQU   CLEARE-CLEARS                                                    
         DC    CL8'**WORK**'                                                    
SAVER8   DC    A(0)                                                             
ARDEF    DC    A(0)                                                             
NOMATCH  DC    X'0'                                                             
GETSW    DS    C                                                                
MATCH    DS    C                                                                
ALPHA    DS    C                                                                
NUMSW    DS    C                                                                
RFIELD   DS    C                                                                
HX       DS    C                                                                
HA       DS    C                                                                
HN       DS    C                                                                
GS       DS    C                                                                
MULTISW  DS    C                                                                
DPT3     DS    CL3                                                              
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
PNUM     DS    H                                                                
ADPTCUME DS    F                                                                
WORK     DS    CL20                                                             
WORK2    DS    CL80                                                             
PNAME    DS    CL50                                                             
         DC    CL8'*CLEARS*'                                                    
CLEARS   DS    0C                                                               
TVQNAME  DS    CL60                                                             
TVQNET   DS    CL4                                                              
TVQPTYP  DS    CL4                                                              
TVQDPT   DS    CL6                                                              
NTINET   DS    CL6                                                              
NTINTI   DS    XL5                                                              
NTIFILT  DS    XL2                                                              
NTILP    DS    CL24                                                             
CLEARE   DS    0C                                                               
TBSTART  DS    0C                                                               
TBPNAME  DS    CL50                                                             
TBNET    DS    CL1                                                              
TBDPT    DS    CL1                                                              
TBIQ     DS    XL50                                                             
TBTVQ    DS    XL50                                                             
TBFT     DS    XL50                                                             
TBEND    DS    0C                                                               
P2       DS    CL132                                                            
         DS    200C                                                             
         DC    CL8'**IREC**'                                                    
INREC    DS    CL800                                                            
         DC    CL8'**ONUM**'                                                    
ONUMS    DS    XL150                                                            
         DC    CL8'**DNUM**'                                                    
DNUMS    DS    XL150                                                            
DPIQ     EQU   DNUMS                                                            
DPTVQ    EQU   DNUMS+50                                                         
DPFT     EQU   DNUMS+100                                                        
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
INA1     DCB   DDNAME=INA1,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INA2     DCB   DDNAME=INA2,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INA3     DCB   DDNAME=INA3,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INA4     DCB   DDNAME=INA4,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INA5     DCB   DDNAME=INA5,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INA6     DCB   DDNAME=INA6,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INM1     DCB   DDNAME=INM1,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INM2     DCB   DDNAME=INM2,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INM3     DCB   DDNAME=INM3,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INM4     DCB   DDNAME=INM4,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INM5     DCB   DDNAME=INM5,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INM6     DCB   DDNAME=INM6,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INW1     DCB   DDNAME=INW1,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INW2     DCB   DDNAME=INW2,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INW3     DCB   DDNAME=INW3,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INW4     DCB   DDNAME=INW4,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INW5     DCB   DDNAME=INW5,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
INW6     DCB   DDNAME=INW6,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
IN5      DCB   DDNAME=IN5,                                             X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               BLKSIZE=07000,                                          X        
               MACRF=GM                                                         
*                                                                               
OUT      DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=01004,                                            X        
               BLKSIZE=05000,                                          X        
               MACRF=PM                                                         
         EJECT                                                                  
PARSTAB  DS    0C                                                               
         DC    C'HD',AL1(00,00,00)                                              
         DC    C',   ,   ,,   ,   ,,   ,   ,,!'                                 
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'** Demographic data unavailable due!'                          
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'TvQ #2 1997/1998 Season!'                                      
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'"1998-99 TVQ#1, CARAT PROPRIET!'                               
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'"1998-99 TVQ#2, CARAT PROPRIET!'                               
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'"1998-99 TVQ#3, CARAT PROPRIET!'                               
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'"1998-99 TVQ#4, CARAT PROPRIET!'                               
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'"1998-99 TVQ#5, CARAT PROPRIET!'                               
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'1998-99 TVQ #1 SUMMARY REPORT,,,,,,,,!'                        
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'1998-99 TVQ#2 SUMMARY REPORT,,,,,,,,!'                         
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'1998-99 TVQ#3 SUMMARY REPORT,,,,,,,,!'                         
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'1997-98 TVQ #2 SUMMARY REPORT!'                                
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'TvQ #3 1997/1998 Season!'                                      
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'1997-98 TVQ #3 SUMMARY REPORT!'                                
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'TvQ #4 1997/1998 Season!'                                      
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'1997-98 TVQ #4 SUMMARY REPORT!'                                
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'TvQ #5 1997/1998 Season!'                                      
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'1997-98 TVQ #5 SUMMARY!'                                       
*        DC    C'HD',AL1(00,00,00)                                              
*        DC    C'1997-98 TVQ #6 SUMMARY REPORT!'                                
*        DC    C'HD',AL1(00,00,00)                                              
*        DC    C'1997-98 TVQ #8 SUMMARY REPORT!'                                
*        DC    C'HD',AL1(00,00,00)                                              
*        DC    C'1997-98 TVQ #7 SUMMARY REPORT!'                                
*        DC    C'HD',AL1(00,00,00)                                              
*        DC    C'TvQ #7 1997/1998 Season,,,!'                                   
*        DC    C'HD',AL1(00,00,00)                                              
*        DC    C'TvQ #8 1997/1998 Season!'                                      
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'IQ vs. TVQ!'                                                   
         DC    C'HX',AL1(01,00,01)                                              
         DC    C',Total,,,Kids,,,Teens,,,,,,,,,Adul!'                           
         DC    C'HX',AL1(01,00,01)                                              
         DC    C',       ,  ,,Total,,,,Kids,,,,Teen!'                           
         DC    C'HX',AL1(01,00,01)                                              
         DC    C',       ,,,, Total,,,,Kids,,,,Teens,,,!'                       
         DC    C'HX',AL1(01,00,01)                                              
         DC    C',       ,,,,TOTAL,,,,KIDS,,,,TEENS,,,,,!'                      
         DC    C'HX',AL1(01,00,01)                                              
         DC    C',       ,  ,,,Total,,,,Kids,,,,Teens,,,,,!'                    
                                                                                
         DC    C'HA',AL1(00,00,01) 1                                            
         DC    C',18+,,,18-34,,,35-49,,,50+,,,18-49,,,25-54,!'                  
                                                                                
         DC    C'HA',AL1(00,00,01) 1                                            
         DC    C',       ,Pgm.,,18+,,,,18-34,,,,35-49,,,,50+,,!'                
                                                                                
         DC    C'HA',AL1(00,00,01) 1                                            
         DC    C',       ,Pgm,,,18+,,,,18-34,,,,35!'                            
                                                                                
         DC    C'HA',AL1(00,00,01) 1                                            
         DC    C',,Pgm.,,,18+,,,,18-34,,,,35-49,,,,50+,,!'                      
                                                                                
         DC    C'HA',AL1(00,00,01) 1                                            
         DC    C',       ,Pgm,,18+,,,,18-34,,,,35-49!'                          
                                                                                
         DC    C'HA',AL1(00,00,01) 1                                            
         DC    C',       ,Pgm.,,,18+,,,,18-34,,,,35-49,,,,50+,,,!'              
                                                                                
         DC    C'HA',AL1(00,00,02) 2                                            
         DC    C',18-34,,,18-49,,,,,,,,,,,,,,,,!'                               
                                                                                
         DC    C'HA',AL1(00,00,03) 3                                            
         DC    C',Sample,,,6-11,,,12-17,,,6-17,,,12-34,,,18+,,,18-34!'          
                                                                                
         DC    C'HA',AL1(00,00,04) 14                                           
         DC    C',       ,Pgm,,Sample,,,,6-11,,,,12!'                           
                                                                                
         DC    C'HA',AL1(00,00,03) 3                                            
         DC    C',       ,Pgm.,,,SAMPLE,,,,6-11,,,,12-17,,,,6-17,!'             
         DC    C'HA',AL1(00,00,03) 3                                            
         DC    C',       ,Pgm.,,,Sample,,,,6-11,,,,12-17,,,,6-17,!'             
                                                                                
         DC    C'HA',AL1(00,00,03) 3                                            
         DC    C',,Pgm.,,,Sample,,,,6-11,,,,12-17,,,,6-17,!'                    
                                                                                
         DC    C'HA',AL1(00,00,03) 3                                            
         DC    C',,Pgm,,,Sample,,,,6-11,,,,12-17,,,,6-17,!'                     
                                                                                
         DC    C'HA',AL1(00,00,04) 4                                            
         DC    C',Sample,,,6-11,,,12-17,,,6-17,,,12-34,,,18-34,,,!'             
                                                                                
         DC    C'HA',AL1(00,00,05) 5                                            
         DC    C',12+,,,12-17,,,12-34,,,18+,,,18-34!'                           
                                                                                
         DC    C'HA',AL1(00,00,07) 7                                            
         DC    C',,Sample,6-11,12-17,,18-34,35-49,50+,!'                        
                                                                                
         DC    C'HD',AL1(00,00,00)                                              
         DC    C',IQ,TvQ,,IQ,TvQ,,IQ,TvQ,,IQ,!'                                 
         DC    C'HD',AL1(00,00,00)                                              
         DC    C'                       ,IQ,TVQ,,IQ ,TVQ,,!'                    
         DC    C'GS',AL1(01,00,01)                                              
         DC    C'PRIMETIME,,,,!'                                                
         DC    C'GS',AL1(01,00,01)                                              
         DC    C'PRIMETIME    ,,,,,!'                                           
         DC    C'GS',AL1(01,00,01)                                              
         DC    C'PRIMETIME,       ,  ,,,!'                                      
         DC    C'GS',AL1(01,00,01)                                              
         DC    C'PRIMETIME    !'                                                
         DC    C'GE',AL1(01,00,01)                                              
         DC    C'PRIMETIME AVERAGE,!'                                           
         DC    C'GS',AL1(02,00,02)                                              
         DC    C'CHILDRENS PROGRAMS,,,,,,!'                                     
         DC    C'GS',AL1(02,00,02)                                              
         DC    C'CHILDREN''S PROGRAMS,,,,,,!'                                   
         DC    C'GE',AL1(02,00,02)                                              
         DC    C'CHILDREN''S AVERAGE,,,,!'                                      
         DC    C'GE',AL1(02,00,02)                                              
         DC    C'M - F CHILDRENS AVERAGE,!'                                     
         DC    C'GE',AL1(02,00,02)                                              
         DC    C'M - F CHILDREN''S AVERAGE,!'                                   
         DC    C'GS',AL1(03,00,03)                                              
         DC    C'M-F DAYTIME,,,,!'                                              
         DC    C'GS',AL1(03,00,03)                                              
         DC    C'M - F DAYTIME,,,,,,!'                                          
         DC    C'GE',AL1(03,00,03)                                              
         DC    C'M - F DAYTIME AVERAGE,!'                                       
         DC    C'GE',AL1(03,00,03)                                              
         DC    C'M-F DAYTIME AVERAGE,!'                                         
         DC    C'GE',AL1(03,00,03)                                              
         DC    C'M - F AFTERNOON,,,!'                                           
         DC    C'GS',AL1(04,00,04)                                              
         DC    C'M - F MORNING NEWS PROGRAMS,!'                                 
         DC    C'GS',AL1(04,00,04)                                              
         DC    C'NEWS PROGRAMS,,,,,,,,,,,,,!'                                   
         DC    C'GE',AL1(04,00,04)                                              
         DC    C'NEWS AVERAGE,,,,!'                                             
         DC    C'GE',AL1(04,00,04)                                              
         DC    C'M - F MORNING NEWS AVERAGE,!'                                  
         DC    C'GS',AL1(05,00,05)                                              
         DC    C'M - F EVENING NEWS PROGRAMS,!'                                 
         DC    C'GE',AL1(05,00,05)                                              
         DC    C'M - F EVENING NEWS AVERAGE,!'                                  
         DC    C'GS',AL1(06,00,06)                                              
         DC    C'SAT/SUN DAYTIME NEWS PROGRAMS,!'                               
         DC    C'GE',AL1(06,00,06)                                              
         DC    C'SAT/SUN DAYTIME NEWS AVERAGE,!'                                
         DC    C'GE',AL1(06,00,06)                                              
         DC    C'SAT/SUN DAYTIME NEWS AVG.!'                                    
         DC    C'GS',AL1(07,00,07)                                              
         DC    C'SAT/SUN CHILDRENS PROGRAMS,!'                                  
         DC    C'GS',AL1(07,00,07)                                              
         DC    C'SAT/SUN CHILDREN''S PROGRAMS,!'                                
         DC    C'GE',AL1(07,00,07)                                              
         DC    C'SAT/SUN CHILDRENS AVERAGE,!'                                   
         DC    C'GS',AL1(08,00,08)                                              
         DC    C'OTHER,,,,,!'                                                   
         DC    C'GS',AL1(08,00,08)                                              
         DC    C'Other,,,,,,,,,,,,!'                                            
         DC    C'GE',AL1(08,00,08)                                              
         DC    C'OTHER AVERAGE,,,!'                                             
         DC    C'GS',AL1(09,00,09)                                              
         DC    C'SPORTS PROGRAMS,,,,!'                                          
         DC    C'GS',AL1(09,00,09)                                              
         DC    C'SPORTS,,,,,,,!'                                                
         DC    C'GE',AL1(09,00,09)                                              
         DC    C'SPORTS AVERAGE,!'                                              
         DC    C'GS',AL1(10,00,10)                                              
         DC    C'WEEKEND LATE NIGHT PROGRAMS,,!'                                
         DC    C'GE',AL1(10,00,10)                                              
         DC    C'WEEKEND LATE NIGHT AVERAGE,!'                                  
         DC    C'GS',AL1(11,00,11)                                              
         DC    C'M - F LATE NIGHT PROGRAMS,,!'                                  
         DC    C'GS',AL1(11,00,11)                                              
         DC    C'LATE NIGHT PROGRAMS,,,,,,,!'                                   
         DC    C'GS',AL1(11,00,11)                                              
         DC    C'LATENIGHT,,,,,!'                                               
         DC    C'GE',AL1(11,00,11)                                              
         DC    C'M - F LATE NIGHT AVERAGE,!'                                    
         DC    C'GE',AL1(11,00,11)                                              
         DC    C'LATE NIGHT AVERAGE,,,!'                                        
         DC    C'HD',AL1(09,00,00)                                              
         DC    C'SOURCE: MARKETING EVALUA!'                                     
         DC    C'HD',AL1(09,00,00)                                              
         DC    C'SOURCE:!'                                                      
         DC    C'HD',AL1(09,00,00)                                              
         DC    C'SOURCE:  MARKETING EVALUA!'                                    
         DC    C'HD',AL1(09,00,00)                                              
         DC    C',Network,Type,Daypart,IQ,TVQ,FT,,IQ,TVQ,FT!'                   
         DC    C'HD',AL1(09,00,00)                                              
         DC    C',Network,Type,Daypart,, IQ,TVQ,FT,, IQ!'                       
         DC    C'HD',AL1(09,00,00)                                              
         DC    C',Network,Type,Daypart,IQ,TVQ,FT,,IQ,!'                         
         DC    C'HD',AL1(09,00,00)                                              
         DC    C',Network,Type,Daypart,, IQ,TVQ,FT,, IQ!'                       
         DC    C'HD',AL1(09,00,00)                                              
         DC    C',Network,Type,Daypart,,IQ,TVQ,FT,,!'                           
         DC    C'HD',AL1(09,00,00)                                              
         DC    C',NETWORK,TYPE,DAYPART,, IQ,TVQ,FT,, IQ!'                       
         DC    C'HD',AL1(09,00,00)                                              
         DC    C'Source: Marketing Evalua!'                                     
         DC    C'HX',AL1(09,00,01)                                              
         DC    C',Total,,,Teens,,,,,,Adul!'                                     
*        DC    C'HA',AL1(09,00,00)                                              
*        DC    C',12+,,,12-17,,,12-34,!'                                        
         DC    C'HB',AL1(09,00,00)                                              
         DC    C'M - F AFTERNOON,IQ,TvQ,!'                                      
*    MEN                                                                        
         DC    C'HX',AL1(09,00,03)                                              
         DC    C',Men,,,,,,,,,!'                                                
         DC    C'HX',AL1(09,00,06)                                              
         DC    C',,,,,MEN,,,,,,,,!'                                             
         DC    C'HX',AL1(09,00,03)                                              
         DC    C',,,Men,,,,,!'                                                  
         DC    C'HX',AL1(09,00,03)                                              
         DC    C',       ,  ,,MEN,,!'                                           
         DC    C'HX',AL1(09,00,06)                                              
         DC    C',       ,  ,,,MEN,,!'                                          
         DC    C'HX',AL1(09,00,06)                                              
         DC    C',,,,MEN,,,,,,,,,,!'                                            
*        DC    C'HA',AL1(09,00,00),C',,18+,,,18-34,,,35-49,,,50+!'              
*        DC    C'HA',AL1(09,00,00),C',18+,,,18-34,,,35-49,,,50+!'               
*        DC    C'HA',AL1(09,00,00),C',18-34,,,18-49,,,,,,,,,,,!'                
         DC    C'HM',AL1(09,00,00)                                              
         DC    C',IQ,TvQ,,IQ,TvQ,,,,!'                                          
*    WOMEN                                                                      
         DC    C'HX',AL1(09,00,02)                                              
         DC    C',Women,,,,!'                                                   
         DC    C'HX',AL1(09,00,05)                                              
         DC    C',,,,,WOMEN,,,!'                                                
         DC    C'HX',AL1(09,00,05)                                              
         DC    C',,,,WOMEN,,,,,,,,!'                                            
         DC    C'HX',AL1(09,00,02)                                              
         DC    C',,,Women,,,!'                                                  
         DC    C'HX',AL1(09,00,02)                                              
         DC    C',       ,  ,,WOMEN,,,,,!'                                      
         DC    C'HX',AL1(09,00,05)                                              
         DC    C',       ,  ,,,WOMEN,,,,!'                                      
*    syndicator/cable                                                           
         DC    C'HS',AL1(09,00,00)                                              
         DC    C'SUMMARY OF TVQ SCORES FOR SYND!'                               
         DC    C'HS',AL1(09,00,00)                                              
         DC    C'TvQ #6 1997/1998 Season,,!'                                    
         DC    C'HX',AL1(09,00,04)                                              
         DC    C',,Total,Kids,Teens,,Adults,,!'                                 
         DC    C'HN',AL1(09,00,01)                                              
         DC    C'SYNDICATED PROGRAMS,,,,!'                                      
         DC    C'HN',AL1(09,00,00)                                              
         DC    C'SYNDICATED AVERAGE,!'                                          
         DC    C'HS',AL1(09,00,00)                                              
         DC    C'SOURCE:,Marketing Eva!'                                        
         DC    C'HN',AL1(09,00,02)                                              
         DC    C'CABLE PROGRAMS,,,!'                                            
         DC    C'HN',AL1(09,00,00)                                              
         DC    C'CABLE AVERAGE,!'                                               
         DC    X'FFFF'                                                          
         EJECT                                                                  
         DS    F                                                                
DPTSTAB  DC    AL1(1),C'PRI',A(PRIME)                                           
         DC    AL1(2),C'KID',A(KID)                                             
         DC    AL1(3),C'DAY',A(DAY)                                             
         DC    AL1(4),C'NWS',A(NEWS)                                            
         DC    AL1(5),C'NWS',A(NEWS)                                            
         DC    AL1(6),C'NWS',A(NEWS)                                            
         DC    AL1(7),C'KID',A(KIDS)                                            
         DC    AL1(8),C'KID',A(KIDS)                                            
         DC    AL1(9),C'SPO',A(SPORTS)                                          
         DC    AL1(10),C'LAT',A(LATE)                                           
         DC    AL1(11),C'LAT',A(LATE)                                           
         DC    X'FF',C'MSC',A(MISC)                                             
         EJECT                                                                  
RECDEFS  DS    0C                                                               
R01      DC    AL1(R01E-*)                                                      
         DC    AL1(0,1)                                                         
*        DC    AL1(NULL)           PROG NAME                                    
         DC    AL1(TQNAME)                                                      
         DC    AL1(TQNET)                                                       
         DC    AL1(TQPTYP)                                                      
         DC    AL1(TQDPT)                                                       
         DC    AL1(NULL)                                                        
         DC    AL1(NULL)                                                        
         DC    AL1(NTNET)                                                       
         DC    AL1(NTNTI)                                                       
         DC    AL1(NTFILT)                                                      
         DC    AL1(NTLP)                                                        
         DC    X'FF'                                                            
R01E     DS    0C                                                               
R13      DC    AL1(R13E-*)                                                      
         DC    AL1(1,3)                                                         
         DC    AL1(PNAM)           PROG NAME                                    
         DC    AL1(TQNET)                                                       
         DC    AL1(TQPTYP)                                                      
         DC    AL1(TQDPT)                                                       
         DC    AL1(NULL)                                                        
         DC    AL1(ISAMP,TSAMP,FSAMP,NULL)                                      
         DC    AL1(IV0611,TV0611,FV0611,NULL)                                   
         DC    AL1(IV1217,TV1217,FV1217,NULL)                                   
         DC    AL1(IV0617,TV0617,FV0617,NULL)                                   
         DC    AL1(IV1234,TV1234,FV1234,NULL)                                   
         DC    AL1(IV1899,TV1899,FV1899,NULL)                                   
         DC    AL1(IV1834,TV1834,FV1834,NULL)                                   
         DC    AL1(IV3549,TV3549,FV3549,NULL)                                   
         DC    AL1(IV5099,TV5099,FV5099,NULL)                                   
         DC    AL1(IV1849,TV1849,FV1849,NULL)                                   
         DC    AL1(IV2554,TV2554,FV2554,NULL)                                   
         DC    AL1(IV2549,TV2549,FV2549,NULL)                                   
         DC    AL1(IV3599,TV3599,FV3599,NULL)                                   
         DC    AL1(IV3564,TV3564,FV3564,NULL)                                   
         DC    X'FF'                                                            
R13E     DS    0C                                                               
R14      DC    AL1(R14E-*)                                                      
         DC    AL1(1,4)                                                         
         DC    AL1(PNAM)           PROG NAME                                    
         DC    AL1(TQNET)                                                       
         DC    AL1(TQPTYP)                                                      
         DC    AL1(TQDPT)                                                       
         DC    AL1(ISAMP,TSAMP,FSAMP,NULL)                                      
         DC    AL1(IV0611,TV0611,FV0611,NULL)                                   
         DC    AL1(IV1217,TV1217,FV1217,NULL)                                   
         DC    AL1(IV0617,TV0617,FV0617,NULL)                                   
         DC    AL1(IV1234,TV1234,FV1234,NULL)                                   
         DC    AL1(IV1899,TV1899,FV1899,NULL)                                   
         DC    AL1(IV1834,TV1834,FV1834,NULL)                                   
         DC    AL1(IV3549,TV3549,FV3549,NULL)                                   
         DC    AL1(IV5099,TV5099,FV5099,NULL)                                   
         DC    AL1(IV1849,TV1849,FV1849,NULL)                                   
         DC    AL1(IV2554,TV2554,FV2554,NULL)                                   
         DC    AL1(IV2549,TV2549,FV2549,NULL)                                   
         DC    AL1(IV3599,TV3599,FV3599,NULL)                                   
         DC    AL1(IV3564,TV3564,FV3564,NULL)                                   
         DC    X'FF'                                                            
R14E     DS    0C                                                               
R21      DC    AL1(R21E-*)                                                      
         DC    AL1(2,1)                                                         
         DC    AL1(PNAM)           PROG NAME                                    
         DC    AL1(TQNET)                                                       
         DC    AL1(TQPTYP)                                                      
         DC    AL1(TQDPT)                                                       
         DC    AL1(IW1899,TW1899,FW1899,NULL)                                   
         DC    AL1(IW1834,TW1834,FW1834,NULL)                                   
         DC    AL1(IW3549,TW3549,FW3549,NULL)                                   
         DC    AL1(IW5099,TW5099,FW5099,NULL)                                   
         DC    AL1(IW1849,TW1849,FW1849,NULL)                                   
         DC    AL1(IW2554,TW2554,FW2554,NULL)                                   
         DC    AL1(IW2549,TW2549,FW2549,NULL)                                   
         DC    AL1(IW3599,TW3599,FW3599,NULL)                                   
         DC    AL1(IW3564,TW3564,FW3564,NULL)                                   
         DC    X'FF'                                                            
R21E     DS    0C                                                               
R31      DC    AL1(R31E-*)                                                      
         DC    AL1(3,1)                                                         
         DC    AL1(PNAM)           PROG NAME                                    
         DC    AL1(TQNET)                                                       
         DC    AL1(TQPTYP)                                                      
         DC    AL1(TQDPT)                                                       
         DC    AL1(IM1899,TM1899,FM1899,NULL)                                   
         DC    AL1(IM1834,TM1834,FM1834,NULL)                                   
         DC    AL1(IM3549,TM3549,FM3549,NULL)                                   
         DC    AL1(IM5099,TM5099,FM5099,NULL)                                   
         DC    AL1(IM1849,TM1849,FM1849,NULL)                                   
         DC    AL1(IM2554,TM2554,FM2554,NULL)                                   
         DC    AL1(IM2549,TM2549,FM2549,NULL)                                   
         DC    AL1(IM3599,TM3599,FM3599,NULL)                                   
         DC    AL1(IM3564,TM3564,FM3564,NULL)                                   
         DC    X'FF'                                                            
R31E     DS    0C                                                               
R51      DC    AL1(R51E-*)                                                      
         DC    AL1(5,1)                                                         
         DC    AL1(PNAM)           PROG NAME                                    
         DC    AL1(TQNET)                                                       
         DC    AL1(TQPTYP)                                                      
         DC    AL1(TQDPT)                                                       
*        DC    AL1(NULL)                                                        
         DC    AL1(IW1899,TW1899,FW1899,NULL)                                   
         DC    AL1(IW1834,TW1834,FW1834,NULL)                                   
         DC    AL1(IW3549,TW3549,FW3549,NULL)                                   
         DC    AL1(IW5099,TW5099,FW5099,NULL)                                   
         DC    AL1(IW1849,TW1849,FW1849,NULL)                                   
         DC    AL1(IW2554,TW2554,FW2554,NULL)                                   
         DC    AL1(IW2549,TW2549,FW2549,NULL)                                   
         DC    AL1(IW3599,TW3599,FW3599,NULL)                                   
         DC    AL1(IW3564,TW3564,FW3564,NULL)                                   
         DC    X'FF'                                                            
R51E     DS    0C                                                               
R61      DC    AL1(R61E-*)                                                      
         DC    AL1(6,1)                                                         
         DC    AL1(PNAM)           PROG NAME                                    
         DC    AL1(TQNET)                                                       
         DC    AL1(TQPTYP)                                                      
         DC    AL1(TQDPT)                                                       
         DC    AL1(NULL)                                                        
         DC    AL1(IM1899,TM1899,FM1899,NULL)                                   
         DC    AL1(IM1834,TM1834,FM1834,NULL)                                   
         DC    AL1(IM3549,TM3549,FM3549,NULL)                                   
         DC    AL1(IM5099,TM5099,FM5099,NULL)                                   
         DC    AL1(IM1849,TM1849,FM1849,NULL)                                   
         DC    AL1(IM2554,TM2554,FM2554,NULL)                                   
         DC    AL1(IM2549,TM2549,FM2549,NULL)                                   
         DC    AL1(IM3599,TM3599,FM3599,NULL)                                   
         DC    AL1(IM3564,TM3564,FM3564,NULL)                                   
         DC    X'FF'                                                            
R61E     DS    0C                                                               
R47      DC    AL1(R47E-*)                                                      
         DC    AL1(4,7)                                                         
         DC    AL1(PNAM)           PROG NAME                                    
         DC    AL1(NULL)                                                        
         DC    AL1(TSAMP)                                                       
         DC    AL1(TV0611)                                                      
         DC    AL1(TV1217)                                                      
         DC    AL1(NULL)                                                        
         DC    AL1(TV1834)                                                      
         DC    AL1(TV3549)                                                      
         DC    AL1(TV5099)                                                      
         DC    AL1(TV1849)                                                      
         DC    AL1(TV1899)                                                      
         DC    AL1(NULL)                                                        
         DC    AL1(TM0699)                                                      
         DC    AL1(TM1834)                                                      
         DC    AL1(TM3549)                                                      
         DC    AL1(TM5099)                                                      
         DC    AL1(TM1849)                                                      
         DC    AL1(TM1899)                                                      
         DC    AL1(NULL)                                                        
         DC    AL1(TF0699)                                                      
         DC    AL1(TW1834)                                                      
         DC    AL1(TW3549)                                                      
         DC    AL1(TW5099)                                                      
         DC    AL1(TW1849)                                                      
         DC    AL1(TW1899)                                                      
         DC    AL1(TW2554)                                                      
         DC    AL1(NULL)                                                        
         DC    X'FF'                                                            
R47E     DS    0C                                                               
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
NULL     EQU   0                                                                
****IQ DISPLACEMENTS                                                            
ISAMP    EQU   1                                                                
IV0611   EQU   2                                                                
IF0699   EQU   3                                                                
IM0699   EQU   4                                                                
IV1217   EQU   5                                                                
IV0617   EQU   6                                                                
IV1234   EQU   7                                                                
IV1299   EQU   8                                                                
IV1899   EQU   9                                                                
IV1834   EQU   10                                                               
IV3549   EQU   11                                                               
IV5099   EQU   12                                                               
IV1849   EQU   13                                                               
IV2554   EQU   14                                                               
IV2549   EQU   15                                                               
IV3599   EQU   16                                                               
IV3564   EQU   17                                                               
IM1899   EQU   18                                                               
IM1834   EQU   19                                                               
IM3549   EQU   20                                                               
IM5099   EQU   21                                                               
IM1849   EQU   22                                                               
IM2554   EQU   23                                                               
IM2549   EQU   24                                                               
IM3599   EQU   25                                                               
IM3564   EQU   26                                                               
IW1899   EQU   27                                                               
IW1834   EQU   28                                                               
IW3549   EQU   29                                                               
IW5099   EQU   30                                                               
IW1849   EQU   31                                                               
IW2554   EQU   32                                                               
IW2549   EQU   33                                                               
IW3599   EQU   34                                                               
IW3564   EQU   35                                                               
****TVQ DISPLACEMENTS                                                           
TSAMP    EQU   50                                                               
TV0611   EQU   51                                                               
TF0699   EQU   52                                                               
TM0699   EQU   53                                                               
TV1217   EQU   54                                                               
TV0617   EQU   55                                                               
TV1234   EQU   56                                                               
TV1299   EQU   57                                                               
TV1899   EQU   58                                                               
TV1834   EQU   59                                                               
TV3549   EQU   60                                                               
TV5099   EQU   61                                                               
TV1849   EQU   62                                                               
TV2554   EQU   63                                                               
TV2549   EQU   64                                                               
TV3599   EQU   65                                                               
TV3564   EQU   66                                                               
TM1899   EQU   67                                                               
TM1834   EQU   68                                                               
TM3549   EQU   69                                                               
TM5099   EQU   70                                                               
TM1849   EQU   71                                                               
TM2554   EQU   72                                                               
TM2549   EQU   73                                                               
TM3599   EQU   74                                                               
TM3564   EQU   75                                                               
TW1899   EQU   76                                                               
TW1834   EQU   77                                                               
TW3549   EQU   78                                                               
TW5099   EQU   79                                                               
TW1849   EQU   80                                                               
TW2554   EQU   81                                                               
TW2549   EQU   82                                                               
TW3599   EQU   83                                                               
TW3564   EQU   84                                                               
****FT DISPLACEMENTS                                                            
FSAMP    EQU   100                                                              
FV0611   EQU   101                                                              
FF0699   EQU   102                                                              
FM0699   EQU   103                                                              
FV1217   EQU   104                                                              
FV0617   EQU   105                                                              
FV1234   EQU   106                                                              
FV1299   EQU   107                                                              
FV1899   EQU   108                                                              
FV1834   EQU   109                                                              
FV3549   EQU   110                                                              
FV5099   EQU   111                                                              
FV1849   EQU   112                                                              
FV2554   EQU   113                                                              
FV2549   EQU   114                                                              
FV3599   EQU   115                                                              
FV3564   EQU   116                                                              
FM1899   EQU   117                                                              
FM1834   EQU   118                                                              
FM3549   EQU   119                                                              
FM5099   EQU   120                                                              
FM1849   EQU   121                                                              
FM2554   EQU   122                                                              
FM2549   EQU   123                                                              
FM3599   EQU   124                                                              
FM3564   EQU   125                                                              
FW1899   EQU   126                                                              
FW1834   EQU   127                                                              
FW3549   EQU   128                                                              
FW5099   EQU   129                                                              
FW1849   EQU   130                                                              
FW2554   EQU   131                                                              
FW2549   EQU   132                                                              
FW3599   EQU   133                                                              
FW3564   EQU   134                                                              
APMPNUM  DS    A                                                                
APHTDTYP DS    A                                                                
APHPNUM  DS    A                                                                
         DS    0F                                                               
         DC    CL8'*OUTREC*'                                                    
OUTREC   DS    XL4                                                              
OUTDATA  DS    XL1000                                                           
         DC    CL8'**TOTS**'                                                    
SUMTABS  DS    0C                                                               
PRIME    DS    XL400                                                            
KID      DS    XL400                                                            
DAY      DS    XL400                                                            
NEWS     DS    XL400                                                            
KIDS     DS    XL400                                                            
SPORTS   DS    XL400                                                            
LATE     DS    XL400                                                            
MISC     DS    XL400                                                            
SUMTABE  DS    0C                                                               
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
         DS    C'PROGBUFF'                                                      
PROGBUFF DS    120000C                                                          
         DS    C'PROGBUF2'                                                      
PROGBUF2 DS    200000C                                                          
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DEDEMFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'204DETVQ3    08/23/00'                                      
         END                                                                    
