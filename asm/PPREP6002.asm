*          DATA SET PPREP6002  AT LEVEL 071 AS OF 11/20/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 046188.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE PP6002A                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE PPRDSHR                                                                
*INCLUDE GETCOST                                                                
*INCLUDE CHOPPER                                                                
         TITLE 'PP6002 - PRINTPAK  MEDIA SCHEDULE REPORT'                       
*                                                                               
*    CHANGE LOG                                                                 
*                                                                               
* BPLA 01/05/2012  RELINK WITH NEW GETCOST - CANADIAN QST CHG                   
*                                                                               
* SMYE 06/19/03  CHANGE READ, HIGH AND SEQ LABELS TO READ60,                    
*                HIGH60 AND SEQ60 (DUPLICATED DMGR ADDRESS LABELS)              
*                                                                               
* BPLA  6/96     CHECK FOR MEDIA NAME OVERRIDE ELEMENT                          
*                IN CLIENT HEADER                                               
*                                                                               
* SMYE 12/13/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                       
*                                                                               
* BPLA  5/95     CHANGES FOR NEW GETCOST FEATURE                                
*                READ AND SAVE B2 PROFILE FOR EACH CLIENT                       
*                PASS NEW VALUE TO GETCOST IF B2PROF+12 IS 'Y' OR 'S'           
*                (SUBTRACT TAX FROM BILLING FORMULA BASE)                       
*                                                                               
*    IN LEVLO  EVEN IF THERE ARE NO INSERTIONS OR $ STILL                       
*              CLEAR ATOTS  (FREE TEST BUYS CAUSED A PROBLEM)                   
*                                                                               
* QOPT1        LEVEL OPT  P=PUB,M=MKT,D=DST,R=REG,E=EST,B=PRD,V=DIV             
* QOPT2        SPACE OPT  S=SPACE,C=COST+SPACE,5=NONE                           
* QOPT3        WEEKS      W=WEEKS                                               
* QOPT4        DOLLARS    S=SUPPRESS COSTS                                      
* QOPT5        DATE OPT   S=SUPPRESS DAYS                                       
*              J=JOBS                                                           
*              A=JOB+DATE                                                       
*              B=PRD+DATE                                                       
*                                                                               
* QOPT6        PAGE OPT  (SAME AS LEVEL OPT)                                    
*                                                                               
* QOPT7        USED IN REQ CARD FOR TEST INSERTIONS                             
*                                                                               
*                                                                               
PP6002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PP60                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP60WRKD,R8                                                      
         LA    R7,4095(RB)                                                      
         LA    R7,1(R7)                                                         
         USING PP6002+4096,R7                                                   
         SPACE 3                                                                
         CLI   MODE,RUNFRST                                                     
         BE    FIRST                                                            
         CLI   LASTMODE,LBUYPUB                                                 
         BNE   *+8                                                              
         BAS   RE,LBPUB                                                         
         JIF   MODE,=,PROCBUY,PRBUY,JUMP=N                                      
         JIF   MODE,=,LBUYMKT,LBMKT,JUMP=N                                      
         JIF   MODE,=,LBUYDST,LBDST,JUMP=N                                      
         JIF   MODE,=,LBUYREG,LBREG,JUMP=N                                      
         JIF   MODE,=,LBUYEST,LBEST,JUMP=N                                      
         JIF   MODE,=,LBUYPRO,LBPRD,JUMP=N                                      
         JIF   MODE,=,LBUYDIV,LBDIV,JUMP=N                                      
         JIF   MODE,=,LBUYCLI,LBCLT,JUMP=N                                      
         JIF   MODE,=,LBUYREQ,LBREQ,JUMP=N                                      
         JIF   MODE,=,FBUYPUB,FBPUB,JUMP=N                                      
         JIF   MODE,=,FBUYMKT,FBMKT,JUMP=N                                      
         JIF   MODE,=,FBUYDST,FBDST,JUMP=N                                      
         JIF   MODE,=,FBUYREG,FBREG,JUMP=N                                      
         JIF   MODE,=,FBUYEST,FBEST,JUMP=N                                      
         JIF   MODE,=,FBUYPRO,FBPRO,JUMP=N                                      
         JIF   MODE,=,FBUYDIV,FBDIV,JUMP=N                                      
         JIF   MODE,=,FBUYCLI,FBCLI,JUMP=N                                      
         JIF   MODE,=,FBUYREQ,FBREQ,JUMP=N                                      
EXIT     DS    0H                                                               
         MVC   LASTMODE,MODE                                                    
         XIT1                                                                   
         SPACE 3                                                                
FIRST    DS    0H                                                               
         LA    RE,PP60WRK                                                       
         LA    RF,PP60WRKL                                                      
         XCEF                                                                   
*                                                                               
         RELOC (R2)                                                             
         LA    R3,ADDRS                                                         
         LA    R4,ACONS                                                         
         LA    R0,(ACONSX-ACONS)/4                                              
FIRST2   DS    0H                                                               
         L     RF,0(R4)                                                         
         AR    RF,R2                                                            
         ST    RF,0(R3)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,FIRST2                                                        
*                                                                               
         MVI   DASHES,C'-'                                                      
         MVC   DASHES+1(L'DASHES-1),DASHES                                      
         B     EXIT                                                             
         SPACE 3                                                                
ACONS    DS    0F                                                               
         DC    A(DTL)                                                           
         DC    A(SETHEAD)                                                       
         DC    A(DATOUT)                                                        
         DC    A(POST)                                                          
         DC    V(BINSRCH)                                                       
         DC    A(SETSPCE)                                                       
         DC    A(DOLFMT)                                                        
         DC    A(GETSPCE)                                                       
         DC    A(NPROC)                                                         
         DC    A(ROLLUP)                                                        
         DC    A(PRTALL)                                                        
         DC    V(PPRDSHR)                                                       
         DC    V(CHOPPER)                                                       
         DC    V(GETCOST)                                                       
         DC    A(PRINTIT)                                                       
         DC    A(JOBLST)                                                        
         DC    A(JOBLSTX)                                                       
         DC    A(DTLINS)                                                        
         DC    A(COMTAB)                                                        
         DC    A(SPCTAB)                                                        
         DC    A(SPCTOTS)                                                       
         DC    A(SPCTAB2)                                                       
         DC    A(SPCTOTS2)                                                      
         DC    A(PUBTOTS)                                                       
         DC    A(VENTOTS)                                                       
         DC    A(MKTTOTS)                                                       
         DC    A(DSTTOTS)                                                       
         DC    A(REGTOTS)                                                       
         DC    A(ESTTOTS)                                                       
         DC    A(PRDTOTS)                                                       
         DC    A(DIVTOTS)                                                       
         DC    A(CLTTOTS)                                                       
         DC    A(REQTOTS)                                                       
ACONSX   EQU   *                                                                
         EJECT                                                                  
         SPACE 3                                                                
FBEST    DS    0H                                                               
         MVI   RISW,0              SHOW 'APPARENT' INS COUNTS                   
         TM    ESTSW,X'80'                                                      
         BZ    *+8                                                              
         BAS   RE,SETEST                                                        
         OC    PROF,PROF                                                        
         BZ    FBEST2                                                           
         CLC   QEST,=C'ALL'                                                     
         BNE   EXIT                                                             
FBEST2   DS    0H                                                               
         GOTO1 ANPROC                                                           
         MVI   BFSW,C'Y'                                                        
         B     EXIT                                                             
         SPACE 2                                                                
FBPUB    DS    0H                                                               
         BAS   RE,SETPUB                                                        
         B     EXIT                                                             
         SPACE 2                                                                
FBMKT    DS    0H                                                               
         BAS   RE,SETMKT                                                        
         B     EXIT                                                             
         SPACE 2                                                                
FBDST    DS    0H                                                               
         BAS   RE,SETDST                                                        
         B     EXIT                                                             
         SPACE 2                                                                
FBREG    DS    0H                                                               
         BAS   RE,SETREG                                                        
         B     EXIT                                                             
         SPACE 2                                                                
FBPRO    DS    0H                                                               
         TM    PRDSW,X'80'                                                      
         BZ    *+8                                                              
         BAS   RE,SETPRD                                                        
         JIF   LEVSW,=,C'B',OR,C'V',OR,C'C',EXIT,JUMP=N                         
         MVI   BFSW,C'Y'           PRINT COST FORMULA                           
         B     EXIT                                                             
         SPACE 2                                                                
FBDIV    DS    0H                                                               
         TM    DIVSW,X'80'                                                      
         BZ    *+8                                                              
         BAS   RE,SETDIV                                                        
         B     EXIT                                                             
         SPACE 2                                                                
FBCLI    DS    0H                                                               
*            FIRST TRY TO READ B2 PROFILE (NEEDED FOR GETCOST)                  
*                                                                               
         XC    B2PROF,B2PROF                                                    
         XC    WORK,WORK                                                        
         MVC   WORK(12),=CL12'P000'                                             
         MVC   WORK+2(2),=C'B2'                                                 
         MVC   WORK+4(2),QAGENCY                                                
         MVC   WORK+6(1),QMEDIA                                                 
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,B2PROF,DATAMGR                                 
*                                                                               
         MVC   TAXOPT,B2PROF+12                                                 
         CLI   TAXOPT,C' '                                                      
         BH    *+8                                                              
         MVI   TAXOPT,C'N'  FOR DEFAULT                                         
*                                                                               
         TM    CLTSW,X'80'                                                      
         BZ    *+8                                                              
         BAS   RE,SETCLI                                                        
         B     EXIT                                                             
         SPACE 2                                                                
FBREQ    DS    0H                                                               
*****                                                                           
         MVI   FCRDTEST,C'N'                                                    
         CLI   QOPT7,C'Y'                                                       
         BNE   FBREQ10                                                          
         MVI   FCRDTEST,C'Y'                                                    
*****                                                                           
FBREQ10  GOTO1 ANPROC                                                           
         B     EXIT                                                             
         SPACE 3                                                                
PRBUY    DS    0H                                                               
         GOTO1 APOST                                                            
         B     EXIT                                                             
         SPACE 3                                                                
LBPUB    NTR1                      LAST FOR PUB/VENDOR                          
         CLI   LEVSW,C'P'                                                       
         BNE   EXIT                                                             
         LH    RF,PBRCNT                                                        
         LA    RF,1(RF)                                                         
         STH   RF,PBRCNT                                                        
         LH    RF,EDITCNT                                                       
         LA    RF,1(RF)                                                         
         STH   RF,EDITCNT                                                       
         CLI   PUBOPT,C'V'                                                      
         BE    LBPUB4                                                           
         CLI   MODE,LBUYVEN                                                     
         BNE   LBPUB6                                                           
         CLI   EDITCNT+1,1                                                      
         BE    LBPUB4                                                           
         MVI   EDVENSW,C'E'                                                     
         BAS   RE,ENDPUB                                                        
LBPUB4   DS    0H                                                               
         MVI   EDVENSW,C'V'                                                     
         BAS   RE,ENDPUB                                                        
         XC    EDITCNT,EDITCNT                                                  
         B     LBPUB8                                                           
LBPUB6   DS    0H                                                               
         MVI   EDVENSW,C'E'                                                     
         BAS   RE,ENDPUB                                                        
LBPUB8   DS    0H                                                               
         B     EXIT                                                             
         SPACE 3                                                                
ENDPUB   NTR1                                                                   
         CLI   PAGOPT,C'P'                                                      
         BNE   *+8                                                              
         MVI   NEWPAGE,C'Y'                                                     
         SPACE 2                                                                
         CLI   EDVENSW,C'E'                                                     
         BE    ENDP2                                                            
         CLI   EDITCNT+1,1                                                      
         BNH   ENDP2                                                            
         MVC   ATOTS,AVENTOTS                                                   
         LA    RF,VENTWS                                                        
         ST    RF,ATOTWDS                                                       
         B     LEVLO                                                            
ENDP2    DS    0H                                                               
         MVC   ATOTS,APUBTOTS                                                   
         LA    RF,EDTTWS                                                        
         CLI   EDVENSW,C'E'                                                     
         BE    *+14                                                             
         MVC   ATOTS,AVENTOTS                                                   
         LA    RF,VENTWS                                                        
         ST    RF,ATOTWDS                                                       
         LA    RF,PUBN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBMKT    DS    0H                  LAST FOR MARKET                              
         CLI   PAGOPT,C'M'                                                      
         BNE   *+8                                                              
         MVI   NEWPAGE,C'Y'                                                     
         MVC   ATOTS,AMKTTOTS                                                   
         CLI   LEVSW,C'M'                                                       
         BE    LBMKT2                                                           
         JIF   LEVSW,NE,C'P',EXIT,JUMP=N                                        
         CLI   PBRCNT+1,1                                                       
         BH    LBMKT2                                                           
*                                  IF ONLY ONE VEN FOR MKT                      
*                                  PRINT VEN MONTHLY $ NOW                      
         MVI   LNEED,X'80'                                                      
         GOTO1 APRTALL,DMCB,0                                                   
         XC    PBRCNT,PBRCNT                                                    
         B     LEVLO4                                                           
LBMKT2   DS    0H                                                               
         LA    RF,MKTTWS                                                        
         ST    RF,ATOTWDS                                                       
         XC    PBRCNT,PBRCNT                                                    
         JIF   LEVSW,=,C'P',LEVLO,JUMP=N                                        
         JIF   LEVSW,NE,C'M',EXIT,JUMP=N                                        
         BAS   RE,SETMKT                                                        
         LA    RF,MKTN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBDST    DS    0H                  LAST FOR DISTRICT                            
         CLI   QDIST,C' '                                                       
         BE    EXIT                                                             
         BAS   RE,SETDST                                                        
         MVC   ATOTS,ADSTTOTS                                                   
         MVC   W(NML*3),DSTN       DIST NAME                                    
         MVC   W+NML*3(L'DSTTWS),DSTTWS     **                                  
         LA    RF,W                                                             
         ST    RF,ATOTWDS                                                       
         MVI   NEWPAGE,C'Y'                                                     
         JIF   LEVSW,=,C'P',OR,C'M',LEVLO,JUMP=N                                
         CLI   PAGOPT,C'D'                                                      
         BE    *+8                                                              
         MVI   NEWPAGE,C'N'                                                     
         JIF   LEVSW,NE,C'D',EXIT,JUMP=N                                        
         LA    RF,DSTN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBREG    DS    0H                  LAST FOR REGION                              
         CLI   QREGION,C' '                                                     
         BE    EXIT                                                             
         BAS   RE,SETREG                                                        
         MVC   ATOTS,AREGTOTS                                                   
         MVC   W(NML*3),REGN       REG NAME                                     
         MVC   W+NML*3(L'REGTWS),REGTWS       **                                
         LA    RF,W                                                             
         ST    RF,ATOTWDS                                                       
         MVI   NEWPAGE,C'Y'                                                     
         JIF   LEVSW,=,C'P',OR,C'M',OR,C'D',LEVLO,JUMP=N                        
         CLI   PAGOPT,C'R'                                                      
         BE    *+8                                                              
         MVI   NEWPAGE,C'N'                                                     
         JIF   LEVSW,NE,C'R',EXIT,JUMP=N                                        
         LA    RF,REGN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBEST    DS    0H                  LAST FOR ESTIMATE                            
         MVI   RISW,C'R'           SHOW ONLY 'REAL' INS COUNTS                  
         CLI   QEST,C' '                                                        
         BE    EXIT                                                             
         CLI   QESTEND,C' '                                                     
         BNE   EXIT                                                             
*                                                                               
         MVC   ATOTS,AESTTOTS                                                   
         LA    RF,ESTTWS                                                        
         ST    RF,ATOTWDS                                                       
         MVI   NEWPAGE,C'Y'                                                     
         JIF   LEVSW,=,C'P',OR,C'M',OR,C'R',OR,C'D',LEVLO,JUMP=N                
         CLI   PAGOPT,C'E'                                                      
         BE    *+8                                                              
         MVI   NEWPAGE,C'N'                                                     
         JIF   LEVSW,NE,C'E',EXIT,JUMP=N                                        
         BAS   RE,SETEST                                                        
         LA    RF,ESTN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBPRD    DS    0H                  LAST FOR PRODUCT                             
         CLI   QPRODUCT,C' '                                                    
         BE    EXIT                                                             
         MVC   ATOTS,APRDTOTS                                                   
         LA    RF,PRDTWS                                                        
         ST    RF,ATOTWDS                                                       
         MVI   NEWPAGE,C'Y'                                                     
         JIF   LEVSW,=,C'P',OR,C'M',OR,C'D',OR,C'R',OR,C'E',LEVLO,     +        
               JUMP=N                                                           
         CLI   PAGOPT,C'B'                                                      
         BE    *+8                                                              
         MVI   NEWPAGE,C'N'                                                     
         JIF   LEVSW,NE,C'B',EXIT,JUMP=N                                        
         BAS   RE,SETPRD                                                        
         LA    RF,PRDN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBDIV    DS    0H                                                               
         CLI   QDIV,C' '                                                        
         BE    EXIT                                                             
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    LBDIV2                                                           
         CLI   QPRODUCT,C' '                                                    
         BNE   EXIT                                                             
LBDIV2   DS    0H                                                               
         MVC   ATOTS,ADIVTOTS                                                   
         LA    RF,DIVTWS                                                        
         ST    RF,ATOTWDS                                                       
         MVI   NEWPAGE,C'Y'                                                     
         JIF   LEVSW,NE,C'V',LEVLO,JUMP=N                                       
         CLI   PAGOPT,C'V'                                                      
         BE    *+8                                                              
         MVI   NEWPAGE,C'N'                                                     
         BAS   RE,SETDIV                                                        
         LA    RF,DIVN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBCLT    DS    0H                  LAST FOR CLIENT                              
         CLI   QPRODUCT,C' '                                                    
         BE    LBCLT2                                                           
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   EXIT                                                             
LBCLT2   DS    0H                                                               
         CLC   QDIV,=C'ALL'                                                     
         BE    LBCLT4                                                           
         CLI   QDIV,C' '                                                        
         BNE   EXIT                                                             
LBCLT4   DS    0H                                                               
         MVC   ATOTS,ACLTTOTS                                                   
         LA    RF,CLTTWS                                                        
         ST    RF,ATOTWDS                                                       
*                                                                               
         JIF   LEVSW,NE,C'C',LEVLO,JUMP=N                                       
         BAS   RE,SETCLI                                                        
         LA    RF,CLTN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBREQ    DS    0H                  LAST FOR REQ                                 
         CLI   LEVSW,C'C'                                                       
         BNE   EXIT                                                             
         MVC   ATOTS,AREQTOTS                                                   
         LA    RF,REQTWS                                                        
         ST    RF,ATOTWDS                                                       
         BAS   RE,SETRPT                                                        
         LA    RF,RPTN                                                          
         ST    RF,ADESC                                                         
*                                                                               
         MVC   BSPARS(24),BSPARS2                                               
         MVI   TABSW,2                                                          
         B     LEVEQ1                                                           
         SPACE 3                                                                
LEVEQ    DS    0H                  LEVSW EQUAL TO MODE                          
         MVC   BSPARS(24),BSPARS1                                               
         MVI   TABSW,1                                                          
LEVEQ1   DS    0H                                                               
         OC    BSPARS+8(4),BSPARS+8     TEST ANY DATA                           
         BZ    EXIT                                                             
         MVI   LNEED,8                                                          
*                                  PRINT ANY HIGHER BREAK DESCS                 
         LA    R3,DIVN                                                          
         LA    R4,NML*4                                                         
         LCR   R4,R4                                                            
         LA    R5,MKTN                                                          
LEVEQ2   DS    0H                                                               
         C     R3,ADESC                                                         
         BNH   LEVEQ3B                                                          
         CLC   0(NML*4,R3),SPACES                                               
         BE    LEVEQ3              NONE                                         
         GOTO1 APRTALL,DMCB,(R3),SPACES,SPACES                                  
         MVC   0(NML*4,R3),SPACES       CLEAR                                   
LEVEQ3   DS    0H                                                               
         BXH   R3,R4,LEVEQ2                                                     
*                                                                               
LEVEQ3B  DS    0H                                                               
         CLI   SPCOPT,0                                                         
         BE    LEVEQ4                                                           
         GOTO1 APRTALL,DMCB,ADESC,SPACES,SPACES                                 
         GOTO1 AGETSPC                                                          
         B     LEVLO                                                            
*                                                                               
LEVEQ4   DS    0H                                                               
         GOTO1 AGETSPC                                                          
         MVI   LNEED,X'80'                                                      
         GOTO1 APRTALL,DMCB,0      PRINT MONTHLY $                              
         B     LEVLO4                                                           
         SPACE 3                                                                
LEVLO    DS    0H                  LEVSW 'LOWER' THAN MODE                      
         CLI   ATOTS,0             NO TOTALS AT THIS LEVEL                      
         BE    LEVLO9                                                           
         L     RF,ATOTS                                                         
         OC    0(20,RF),0(RF)      NO $, NO INSERTIONS                          
         BZ    LEVLO8              MUST STILL CLEAR ATOTS                       
*                                                                               
         MVI   TOTSW,1                                                          
         GOTO1 ADATOUT,DMCB,ATOTS                                               
         MVI   TOTSW,1                                                          
         GOTO1 ADOLFMT,DMCB,ATOTS                                               
         GOTO1 APRTALL,DMCB,ATOTWDS,ADTLINS,DOLLNS                              
LEVLO4   DS    0H                                                               
         CLI   NEWPAGE,C'Y'                                                     
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         LA    R3,APUBTOTS                                                      
         LA    R4,4                                                             
         LA    R5,ADDRSX-1                                                      
         CLC   1(3,R3),ATOTS+1                                                  
         BH    *+12                                                             
         BXLE  R3,R4,*-10                                                       
         B     LEVLO8                                                           
         CLI   0(R3),0                                                          
         BNE   LEVLO6                                                           
         BXLE  R3,R4,*-8                                                        
         B     LEVLO8                                                           
*                                                                               
LEVLO6   DS    0H                                                               
         MVC   DMCB+4(4),0(R3)                                                  
         GOTO1 AROLLUP,DMCB,ATOTS                                               
*                                                                               
LEVLO8   DS    0H                                                               
         L     RE,ATOTS                                                         
         LA    RF,TOTL                                                          
         XCEF                                                                   
*                                                                               
         MVC   ANXTTOT1,ASPCTOT1                                                
         MVC   ANXTCOM,ACOMTAB                                                  
         XC    BSPARS1+8(4),BSPARS1+8     RESET BSPARS                          
*                                                                               
         CLI   TABSW,2                                                          
         BNE   LEVLO9                                                           
         MVC   ANXTTOT2,ASPCTOT2                                                
         XC    BSPARS2+8(4),BSPARS2+8                                           
*                                                                               
*                                                                               
LEVLO9   DS    0H                                                               
         MVI   NEWPAGE,C'N'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*                                  SET PUB NAME, ETC                            
         SPACE 2                                                                
SETPUB   NTR1                                                                   
         SPACE 2                                                                
         MVI   PUBN,C' '                                                        
         MVC   PUBN+1(256),PUBN                                                 
         MVC   PUBN+257(L'PUBN-257),PUBN                                        
         LA    R4,PUBN                                                          
         CLI   LEVSW,C'P'                                                       
         BNE   SP36                                                             
         CLI   QMEDIA,C'N'                                                      
         BNE   SP20                                                             
*                                  NEWSPAPER                                    
         MVC   0(2,R4),PUBSTATE                                                 
         MVI   2(R4),C','                                                       
         MVC   4(16,R4),PUBCITY                                                 
         MVC   NML(20,R4),PUBNAME                                               
         LA    R4,NML*2(R4)                                                     
         MVC   0(20,R4),PUBZNAME                                                
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,NML(R4)                                                       
*                                  UNDERLINE PUB NAME                           
         LR    R5,R4                                                            
         SH    R5,=Y(NML+1)                                                     
         MVC   0(NML,R4),DASHES                                                 
SP19     DS    0H                                                               
         CLI   0(R5),C' '                                                       
         BH    SP19B                                                            
         CLI   NML(R5),C' '                                                     
         BH    SP19B                                                            
         MVI   NML*2(R5),C' '                                                   
         BCT   R5,SP19                                                          
SP19B    DS    0H                                                               
         LA    R4,NML(R4)                                                       
         LR    R5,R4                                                            
         CLI   PUBKPUB+5,0                                                      
         BE    SP30                                                             
         GOTO1 PUBEDIT,DMCB,PBUYKPUB,(C'E',WORK)                                
*                                                                               
         MVC   0(11,R4),WORK                                                    
         LA    R5,12(R4)                                                        
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
*                                                                               
         LA    R5,2(R5)                                                         
         B     SP30                                                             
*                                  MAGAZINES                                    
SP20     DS    0H                                                               
         MVC   0(20,R4),PUBNAME                                                 
         LA    R4,NML(R4)                                                       
         MVC   0(20,R4),PUBZNAME                                                
         CLI   QMEDIA,C'O'         FOR OUTDOOR USE ST, MKT                      
         BNE   SP21                                                             
         MVC   0(2,R4),PUBSTACD    TRY STACD                                    
         CLI   PUBSTACD,C' '                                                    
         BNH   SP20B                                                            
         CLI   PUBSTACD,C'0'       UNLESS NUMERIC                               
         BL    *+10                                                             
SP20B    DS    0H                                                               
         MVC   0(2,R4),PUBSTATE                                                 
         MVI   2(R4),C','                                                       
         MVI   3(R4),C' '                                                       
         MVC   4(20,R4),PUBZNAME                                                
SP21     DS    0H                                                               
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,NML(R4)                                                       
*                                  UNDERLINE PUB NAME                           
         LR    R5,R4                                                            
         SH    R5,=Y(NML+1)                                                     
         MVC   0(NML,R4),DASHES                                                 
SP21A    DS    0H                                                               
         CLI   0(R5),C' '                                                       
         BH    SP21B                                                            
         CLI   NML(R5),C' '                                                     
         BH    SP21B                                                            
         MVI   NML*2(R5),C' '                                                   
         BCT   R5,SP21A                                                         
SP21B    DS    0H                                                               
         LA    R4,NML(R4)                                                       
         LR    R5,R4                                                            
         CLI   QMEDIA,C'O'         NO FREQ FOR OUTDOOR                          
         BE    SP22                                                             
         MVI   ELCODE,X'20'                                                     
         LA    R2,PUBREC+33                                                     
         BAS   RE,SPNXTEL                                                       
         BNE   SP22                                                             
         USING PUBGENEL,R2                                                      
         CLI   PUBMFREQ,C' '                                                    
         BNH   SP22                                                             
         MVC   0(5,R4),=C'PUBL='                                                
         MVC   5(2,R4),PUBMFREQ                                                 
         LA    R5,8(R4)                                                         
SP22     DS    0H                                                               
         B     SP30                                                             
*                                  TRY FOR CLIENT NUMBER                        
SP30     DS    0H                                                               
         MVI   ELCODE,X'14'                                                     
         LA    R2,PUBREC+33                                                     
         BAS   RE,SPFSTEL                                                       
         B     *+8                                                              
SP31     DS    0H                                                               
         BAS   RE,SPNXTEL                                                       
         BNE   SP33                                                             
         USING PUBREPEL,R2                                                      
         CLC   PUBRPOFF,PCLTKCLT                                                
         BNE   SP31                                                             
         OC    PUBCVEN,PUBCVEN                                                  
         BZ    SP33                                                             
         CLC   PUBCVEN,SPACES                                                   
         BE    SP33                                                             
         CLI   PUBCVEN,X'FF'                                                    
         BE    SP33                                                             
         CLI   PUBCVEN,X'99'                                                    
         BH    SP32                                                             
         IC    R3,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R3),PUBCVEN),1(R5)                                
         B     SP32B                                                            
SP32     DS    0H                                                               
         MVC   1(12,R5),PUBCVEN                                                 
SP32B    DS    0H                                                               
         MVI   0(R5),C'('                                                       
         LA    R5,18(R5)                                                        
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C')'                                                       
*                                  VENDOR NO.                                   
SP33     DS    0H                                                               
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,NML(R4)                                                       
         IC    R2,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R2),PBUYKPUB),1(R4)                               
*                                                                               
         MVI   0(R4),C'('                                                       
         LA    R5,19(R4)                                                        
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C')'                                                       
         LA    R5,3(R5)                                                         
*                                                                               
*                                  CIRC                                         
         CLI   CIRCOPT,C'S'                                                     
         BE    SP36                                                             
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'30'                                                     
         MVC   X,SPACES                                                         
         MVC   X(5),=C'CIRC='                                                   
         XC    FULL,FULL                                                        
SP34     DS    0H                                                               
         BAS   RE,SPNXTEL                                                       
         USING PUBCIREL,R2                                                      
         BNE   SP34F                                                            
         CLC   PUBCDAT,BEND        TEST AFTER END                               
         BH    SP34G                                                            
         CLC   PUBCDAT,BSTART      TEST BEFORE START                            
         BH    SP34D                                                            
         BE    SP34E                                                            
         ST    R2,FULL             SAVE LATEST                                  
         B     SP34                                                             
*                                                                               
SP34D    DS    0H                                                               
         ST    R2,WORD                                                          
         L     R2,FULL             DO LATEST                                    
         LTR   R2,R2                                                            
         BNP   *+8                                                              
         BAS   RE,SETCIRC                                                       
         L     R2,WORD                                                          
*                                                                               
SP34E    DS    0H                                                               
         XC    FULL,FULL           CLEAR LATEST                                 
         BAS   RE,SETCIRC                                                       
         B     SP34                                                             
*                                                                               
SP34F    DS    0H                                                               
         L     R2,FULL                                                          
         LTR   R2,R2                                                            
         BZ    SP34Z                                                            
         BAS   RE,SETCIRC                                                       
         B     SP34Z                                                            
*                                                                               
SP34G    DS    0H                                                               
         CLI   X,C'C'                                                           
         BNE   *+8                                                              
         BAS   RE,SETCIRC                                                       
         B     SP34                                                             
*                                                                               
SP34Z    DS    0H                                                               
*                                                                               
SP36     DS    0H                                                               
         MVI   RDSHRFST,1          PRIMARY REG/DST                              
         CLC   QREGION(6),SPACES                                                
         BE    SP40                                                             
         XC    WORK(12),WORK                                                    
         MVC   WORK(3),PCLTKCLT                                                 
         MVC   WORK+3(3),PDIVKDIV                                               
         CLI   QDIV,C' '                                                        
         BNE   *+10                                                             
         MVC   WORK+3(3),PPRDDIV                                                
         CLC   QPUB+1(3),=C'RD='                                                
         BNE   *+16                                                             
         MVC   WORK(3),QPUB+4                                                   
         MVC   WORK+3(3),=C'000'                                                
         CLI   QREGION,C' '                                                     
         BE    *+10                                                             
         MVC   WORK+6(3),PREGKREG                                               
         CLI   QDIST,C' '                                                       
         BE    *+10                                                             
         MVC   WORK+9(3),PDSTKDST                                               
*                                  NOTE- DTLINS USED HERE AS WORK AREA          
         GOTO1 ARDSHR,DMCB,ALTLREC,(1,WORK),ADTLINS                             
*                                                                               
         L     RF,ADTLINS                                                       
         MVC   RDSHRFST,0(RF)                                                   
SP40     DS    0H                                                               
SPX      DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
SPNXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
SPFSTEL  DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    SPNXTELX                                                         
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     SPNXTEL                                                          
SPNXTELX DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
SETCIRC  DS    0H                                                               
         USING PUBCIREL,R2                                                      
         LA    R6,X+5                                                           
         CLI   X,C'C'                                                           
         BE    *+8                                                              
         LA    R6,X                                                             
         EDIT  (P5,PUBCIR1),(17,0(R6)),COMMAS=YES,ALIGN=LEFT                    
         AR    R6,R0                                                            
         CLI   CIRCOPT,C'D'                                                     
         BNE   SC10                                                             
*                                                                               
         OC    PUBCDAT,PUBCDAT                                                  
         BNZ   SC4                                                              
         CLI   PUBCSRC,C' '                                                     
         BNH   SC10                                                             
*                                                                               
SC4      DS    0H                                                               
         MVI   1(R6),C'('                                                       
         LA    R3,2(R6)                                                         
         OC    PUBCDAT,PUBCDAT                                                  
         BZ    SC5                                                              
         MVI   BYTE,5              MMMDD/YY                                     
         LA    R3,11(R6)                                                        
         CLI   PUBCDAT+2,0                                                      
         BNE   *+12                                                             
         MVI   BYTE,9              MMM/YY                                       
         LA    R3,9(R6)                                                         
         LR    R0,RE                                                            
         GOTO1 DATCON,DMCB,(3,PUBCDAT),(BYTE,2(R6))                             
         LR    RE,R0                                                            
SC5      DS    0H                                                               
         MVC   0(4,R3),PUBCSRC                                                  
         LA    R6,16(R6)                                                        
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         MVI   1(R6),C')'                                                       
*                                                                               
SC10     DS    0H                                                               
         LA    R0,X                                                             
         SR    R6,R0               R6 HAS LENGTH                                
         AR    R6,R5                                                            
         LA    RF,NML(R4)                                                       
         CR    RF,R6                                                            
         BNH   SC12                                                             
         MVC   0(27,R5),X                                                       
         B     SCX                                                              
SC12     DS    0H                                                               
         CLI   X,C'C'                                                           
         BNE   SC11                                                             
         MVC   0(5,R5),X                                                        
         MVC   X(27),X+5                                                        
         MVC   X+27(5),SPACES                                                   
         LA    R4,NML(R4)                                                       
SC11     DS    0H                                                               
         MVC   0(27,R4),X                                                       
SCX      DS    0H                                                               
         LA    R4,NML(R4)                                                       
         LR    R5,R4                                                            
         MVC   X,SPACES                                                         
         BR    RE                                                               
         EJECT                                                                  
SETMKT   DS    0H                                                               
         MVC   MKTN,SPACES                                                      
         CLI   QMEDIA,C'O'                                                      
         BNE   SETMKT2                                                          
         MVC   MKTN(2),PUBSTACD    USE STACD                                    
         CLI   PUBSTACD,C' '       IF THERE                                     
         BNH   *+12                                                             
         CLI   PUBSTACD,C'0'       UNLESS NUMERIC                               
         BL    *+10                                                             
         MVC   MKTN(2),PUBSTATE                                                 
         MVI   MKTN+2,C','                                                      
         MVC   MKTN+4(20),PUBZNAME                                              
         BR    RE                                                               
SETMKT2  DS    0H                                                               
         MVC   MKTN(2),PUBSTATE                                                 
         MVI   MKTN+2,C','                                                      
         MVC   MKTN+4(16),PUBCITY                                               
         BR    RE                                                               
         SPACE 3                                                                
SETDST   DS    0H                                                               
         MVC   DSTN,SPACES                                                      
         CLI   QDIST,C' '                                                       
         BER   RE                                                               
         MVC   DSTN1(8),=C'DISTRICT'                                            
         MVC   DSTN1+9(3),PDSTKDST                                              
         MVC   DSTN2(20),PDSTNAME                                               
         MVC   DSTN3,DASHES                                                     
         LA    RF,DSTN3-1                                                       
SETDST2  DS    0H                                                               
         CLI   0(RF),C' '                                                       
         BHR   RE                                                               
         MVI   NML(RF),C' '                                                     
         BCT   RF,SETDST2                                                       
         SPACE 3                                                                
SETREG   DS    0H                                                               
         MVC   REGN,SPACES                                                      
         CLI   QREGION,C' '                                                     
         BER   RE                                                               
         MVC   REGN1(6),=C'REGION'                                              
         MVC   REGN1+7(3),PREGKREG                                              
         MVC   REGN2(20),PREGNAME                                               
         MVC   REGN3,DASHES                                                     
         LA    RF,REGN3-1                                                       
SETREG2  DS    0H                                                               
         CLI   0(RF),C' '                                                       
         BHR   RE                                                               
         MVI   NML(RF),C' '                                                     
         BCT   RF,SETREG2                                                       
         SPACE 3                                                                
SETEST   DS    0H                                                               
         MVC   ESTN,SPACES                                                      
         CLC   QEST,=C'ALL'                                                     
         BNER  RE                                                               
         MVC   ESTN1(8),=C'ESTIMATE'                                            
         MVC   HALF,PESTKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTN1+9(3),DUB                                                   
         MVC   ESTN2(20),PESTNAME                                               
         BR    RE                                                               
         SPACE 3                                                                
SETPRD   DS    0H                                                               
         MVC   PRDN,SPACES                                                      
         CLC   QPRODUCT,=C'ALL'                                                 
         BNER  RE                                                               
         MVC   PRDN1(7),=C'PRODUCT'                                             
         MVC   PRDN1+9(3),PPRDKPRD                                              
         MVC   PRDN2(20),PPRDNAME                                               
         BR    RE                                                               
         SPACE 3                                                                
SETDIV   DS    0H                                                               
         MVC   DIVN,SPACES                                                      
         CLC   QDIV,=C'ALL'                                                     
         BNER  RE                                                               
         MVC   DIVN1(8),=C'DIVISION'                                            
         MVC   DIVN1+9(3),PDIVKDIV                                              
         MVC   DIVN2(20),PDIVNAME                                               
         BR    RE                                                               
         SPACE 2                                                                
SETCLI   DS    0H                                                               
         MVC   CLTN,SPACES                                                      
         MVC   CLTN1(6),=C'CLIENT'                                              
         MVC   CLTN1+9(3),PCLTKCLT                                              
         MVC   CLTN2(20),PCLTNAME                                               
         BR    RE                                                               
         SPACE 2                                                                
SETRPT   DS    0H                                                               
         MVC   RPTN,SPACES                                                      
         MVC   RPTN1(06),=C'REPORT'                                             
         MVC   RPTN2(06),DASHES                                                 
         BR    RE                                                               
         SPACE 3                                                                
         EJECT                                                                  
*                                  FORMAT SPACES,DATE,+DOLLARS                  
         SPACE 2                                                                
GETSPCE  NTR1                                                                   
         SPACE 2                                                                
         USING STSD,R3                                                          
         L     R3,BSPARS+4                                                      
         L     R4,BSPARS+8                                                      
         LTR   R4,R4                                                            
         BZ    GSX                                                              
GS2      DS    0H                                                               
         L     R5,ADESC                                                         
         CLI   SPCOPT,0                                                         
         BE    GS4                                                              
         GOTO1 ASETSPC,DMCB,(R3)                                                
         LA    R5,SPCN                                                          
GS4      DS    0H                                                               
         MVI   TOTSW,0                                                          
         MVC   DMCB(4),STADDR                                                   
         MVI   DMCB,C'D'           DATES, NOT COUNTS                            
         GOTO1 ADATOUT,DMCB                                                     
         MVC   DMCB(4),STADDR                                                   
         GOTO1 ADOLFMT,DMCB                                                     
         GOTO1 APRTALL,DMCB,(R5),ADTLINS,DOLLNS                                 
         CLI   TABSW,2             DONT TOTAL HERE IS 2(D TAB                   
         BE    GS5                                                              
         MVC   DMCB(4),STADDR                                                   
         GOTO1 AROLLUP,DMCB,,ATOTS                                              
GS5      DS    0H                                                               
         A     R3,BSPARS+12                                                     
         BCT   R4,GS2                                                           
*                                                                               
GSX      DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
ROLLUP   NTR1                                                                   
         SPACE 2                                                                
         LM    R1,R2,0(R1)                                                      
         LM    R3,R6,0(R1)                                                      
         A     R3,00(R2)                                                        
         A     R4,04(R2)                                                        
         A     R5,08(R2)                                                        
         A     R6,12(R2)                                                        
         STM   R3,R6,0(R2)                                                      
         LH    RF,16(R1)                                                        
         AH    RF,16(R2)                                                        
         STH   RF,16(R2)                                                        
         LH    RF,18(R1)                                                        
         AH    RF,18(R2)                                                        
         STH   RF,18(R2)                                                        
         LA    R0,MAXMOS                                                        
         LA    R1,24(R1)                                                        
         LA    R2,24(R2)                                                        
RU2      DS    0H                                                               
         LH    RF,32(R1)                                                        
         AH    RF,32(R2)                                                        
         STH   RF,32(R2)                                                        
         LH    RF,34(R1)                                                        
         AH    RF,34(R2)                                                        
         STH   RF,34(R2)                                                        
         LA    R1,36(R1)                                                        
         LA    R2,36(R2)                                                        
         BCT   R0,RU2                                                           
RUX      DS    0H                                                               
         XIT                                                                    
         EJECT                                                                  
*                                  FORMAT $ AMTS                                
         SPACE 3                                                                
EDTTWS   DC    CL54'**EDITION TOTAL**'                                          
VENTWS   DC    CL54'**VENDOR TOTAL**'                                           
MKTTWS   DC    CL54'**MARKET TOTAL**'                                           
DSTTWS   DC    CL54'**DISTRICT TOTAL**'                                         
REGTWS   DC    CL54'**REGION TOTAL**'                                           
ESTTWS   DC    CL54'**ESTIMATE TOTAL**'                                         
PRDTWS   DC    CL54'**PRODUCT TOTAL**'                                          
DIVTWS   DC    CL54'**DIVISION TOTAL**'                                         
CLTTWS   DC    CL54'**CLIENT TOTAL**'                                           
REQTWS   DC    CL54'**REPORT TOTAL**'                                           
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
DOLFMT   CSECT                                                                  
         NMOD1 0,DOLFMT                                                         
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         MVC   DOLLNS,SPACES                                                    
         LA    R4,DOLLNS                                                        
         LA    R5,DOLLS                                                         
         LA    R6,2                                                             
         L     R3,0(R1)            ADDR ($)                                     
         USING DOLLD,R3                                                         
         MVI   BYTE,0                                                           
DF2      DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    DF4                                                              
         SR    RF,RF                                                            
         IC    RF,0(R5)                                                         
         SLL   RF,2                                                             
         BAS   RE,DFBRTAB-4(RF)                                                 
DF4      DS    0H                                                               
         LA    R4,13(R4)                                                        
         LA    R5,1(R5)                                                         
         BCT   R6,DF2                                                           
*                                                                               
DF12     DS    0H                                                               
*                                                                               
DFX      DS    0H                                                               
         MVI   TOTSW,0                                                          
         XIT1                                                                   
DFBRTAB  B     DFGRS                                                            
         B     DFNET                                                            
         B     DFCD                                                             
         B     DFGLCD                                                           
         B     DFNLCD                                                           
         B     DFCOST                                                           
*                                                                               
         SPACE 2                                                                
DFGRS    L     R0,TGRS                                                          
         BAS   R7,DFFMT                                                         
         MVC   0(13,R4),X                                                       
         BR    RE                                                               
*                                                                               
DFNET    L     R0,TGRS                                                          
         S     R0,TAC                                                           
         BAS   R7,DFFMT                                                         
         MVC   0(13,R4),X                                                       
         BR    RE                                                               
*                                                                               
DFCD     L     R0,TCD                                                           
         BAS   R7,DFFMT                                                         
         MVC   0(13,R4),X                                                       
         BR    RE                                                               
*                                                                               
DFGLCD   L     R0,TGRS                                                          
         BAS   R7,DFFMT                                                         
         MVC   0(13,R4),X                                                       
         S     R0,TCD                                                           
         BAS   R7,DFFMT                                                         
         MVC   DOLLNL(13,R4),X                                                  
         BR    RE                                                               
*                                                                               
DFNLCD   L     R0,TGRS                                                          
         S     R0,TAC                                                           
         BAS   R7,DFFMT                                                         
         MVC   0(13,R4),X                                                       
         S     R0,TCD                                                           
         BAS   R7,DFFMT                                                         
         MVC   DOLLNL(13,R4),X                                                  
         BR    RE                                                               
*                                                                               
DFCOST   DS    0H                                                               
         L     R0,TCOST                                                         
         BAS   R7,DFFMT                                                         
         MVC   0(13,R4),X                                                       
         BR    RE                                                               
*                                                                               
*                                                                               
         SPACE 2                                                                
DFFMT    DS    0H                                                               
         EDIT  (R0),(13,X),2,COMMAS=YES,FLOAT=-                                 
*                                                                               
         MVC   X+2(10),X+3                                                      
         MVI   X+12,C' '                                                        
         CLI   TOTSW,0                                                          
         BER   R7                                                               
         MVI   X+12,C'*'                                                        
         BR    R7                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                  PROCESS REQUEST PARAMS                       
         SPACE 2                                                                
NPROC    CSECT                                                                  
         NMOD1 0,NPROC                                                          
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         MVI   TWOTABS,C'N'        NO 2(D SPACE TAB                             
         CLI   LEVSW,C'C'          UNLESS CLIENT RUN                            
         BNE   *+8                                                              
         MVI   TWOTABS,C'Y'                                                     
*                                                                               
         MVI   BFSW,C'N'                                                        
         CLI   MODE,FBUYEST                                                     
         BE    NP5                                                              
         MVC   PAGE,=H'1'                                                       
         MVC   P,SPACES                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   SAVMAX,MAXLINES                                                  
         XC    EDITCNT,EDITCNT                                                  
         XC    PBRCNT,PBRCNT                                                    
         L     R3,AJOBLST                                                       
         MVI   0(R3),X'FF'         CLEAR JOBLIST                                
         MVI   NEWPAGE,C'N'                                                     
         XC    PROF,PROF                                                        
         MVC   MKTN,SPACES                                                      
         MVC   DSTN,SPACES                                                      
         MVC   REGN,SPACES                                                      
         MVC   ESTN,SPACES                                                      
         MVC   PRDN,SPACES                                                      
         MVC   DIVN,SPACES                                                      
         MVI   MTOTLNS,C' '                                                     
         MVC   MTOTLNS+1(L'MTOTLNS-1),MTOTLNS                                   
         MVC   LEVSW,QOPT1                                                      
*                                  FIX UP REQUEST                               
         CLI   LEVSW,C'P'                                                       
         BE    *+10                                                             
         MVC   QSORT,SPACES                                                     
         CLI   LEVSW,C'M'                                                       
         BNE   *+10                                                             
         MVC   QSORT,=C'07'                                                     
*                                                                               
*                                  SET SWITCHES TO CONTROL                      
*                                  EST, PRD, DIV, CLT IN STUBS AND              
*                                  HEADLINES                                    
*                                                                               
         MVI   ESTSW,0                                                          
         CLI   LEVSW,C'E'                                                       
         BNE   NP2B                                                             
         CLI   QEST,C'0'                                                        
         BL    NP2                                                              
         CLI   QESTEND,C' '                                                     
         BE    NP2B                                                             
NP2      DS    0H                                                               
         OI    ESTSW,X'80'         EST IN STUBS                                 
NP2B     DS    0H                                                               
         CLC   QEST,=C'ALL'                                                     
         BNE   NP2D                                                             
         JIF   LEVSW,=,C'E',OR,C'B',OR,C'V',OR,C'C',NP2F,JUMP=N                 
NP2D     DS    0H                                                               
         OI    ESTSW,X'08'         EST IN HEADLINES                             
NP2F     DS    0H                                                               
         MVI   PRDSW,0                                                          
         CLI   LEVSW,C'B'                                                       
         BNE   NP2J                                                             
         OI    PRDSW,X'80'         PRD IN STUBS                                 
         B     NP2N                                                             
NP2J     DS    0H                                                               
         CLI   QPRODUCT,C' '                                                    
         BE    NP2N                                                             
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   NP2M                                                             
         JIF   LEVSW,=,C'B',OR,C'V',OR,C'C',NP2N,JUMP=N                         
NP2M     DS    0H                                                               
         OI    PRDSW,X'08'         PRD IN HEADLINES                             
NP2N     DS    0H                                                               
         CLI   QPRODUCT,C' '                                                    
         BNE   *+8                                                              
         MVI   FCRDACTV,C'Y'       READ ONLY ACTIVE IF QPROD BLACK              
         MVI   DIVSW,0                                                          
         CLI   LEVSW,C'V'                                                       
         BNE   NP2R                                                             
         OI    DIVSW,X'80'         DIV IN STUBS                                 
         B     NP2U                                                             
NP2R     DS    0H                                                               
         CLI   QDIV,C' '                                                        
         BE    NP2U                                                             
         OI    DIVSW,X'08'         DIV IN HEADLINES                             
NP2U     DS    0H                                                               
         MVI   CLTSW,X'80'         CLT IN STUBS                                 
         JIF   LEVSW,=,C'C',NP2V,JUMP=N                                         
         MVI   CLTSW,X'08'         CLT IN HEADS                                 
NP2V     DS    0H                                                               
         MVI   SPCOPT,0                                                         
         MVI   COSTOPT,0                                                        
         CLI   QOPT2,C' '                                                       
         BE    NP4                                                              
         CLI   LEVSW,C'P'          SPACE ONLY IF PUBS SHOWN                     
         BE    NP2W                                                             
         CLI   LEVSW,C'M'                                                       
         BNE   NP4                                                              
NP2W     DS    0H                                                               
         MVI   SPCOPT,C'Y'         SPACE                                        
         CLI   QOPT2,C'S'                                                       
         BE    NP4                                                              
         MVI   COSTOPT,C'Y'        COST                                         
NP4      DS    0H                                                               
         MVC   WEEKSW,QOPT3                                                     
         CLI   WEEKSW,C' '                                                      
         BH    *+8                                                              
         MVI   WEEKSW,0                                                         
*                                                                               
         MVI   DOLLOPT,0                                                        
         CLI   QOPT4,C'S'                                                       
         BNE   *+8                                                              
         MVI   DOLLOPT,C'S'        SUPPRESS $                                   
*                                                                               
         MVI   DATSW,0                                                          
         MVI   DATSW2,0                                                         
         CLI   QOPT5,C' '                                                       
         BE    *+10                                                             
         MVC   DATSW,QOPT5                                                      
         CLI   QOPT5,C'A'          A = JOBS AND DATES                           
         BNE   *+12                                                             
         MVI   DATSW,C'J'                                                       
         MVI   DATSW2,C'D'                                                      
         CLI   QOPT5,C'B'          B = PRDS AND DATES                           
         BNE   *+12                                                             
         MVI   DATSW,C'P'                                                       
         MVI   DATSW2,C'D'                                                      
         B     NP11                                                             
*                                  FIRST FOR ESTIMATE                           
NP5      DS    0H                                                               
         MVI   PAGOPT,0                                                         
         MVC   COMFILT,PROGPROF+6       SET COMMENT FILTER                      
         OC    PROGPROF,PROGPROF        TEST PROFILE PRESENT                    
         BZ    NP5N                                                             
*                                  SET PROF OPTS IN OLD POSITIONS               
         MVC   PROF+19(1),PROGPROF+0     $ COLS                                 
         MVC   PROF+31(1),PROGPROF+2     JOB                                    
*                                       CIRC                                    
         MVI   PROF+30,C'S'        SUPPRESS                                     
         CLI   PROGPROF+3,C'N'                                                  
         BE    NP5D                                                             
         MVC   PROF+30(1),PROGPROF+3                                            
NP5D     DS    0H                                                               
         MVC   MTHOPT,PROGPROF+4   PRINT $/MON FOR PUB                          
         MVI   PROF+27,C'0'        VENDOR/EDIT TOTALS                           
         CLI   PROGPROF+1,C'N'                                                  
         BE    *+8                                                              
         MVI   PROF+27,C'V'                                                     
*                                                                               
         CLI   PROGPROF+7,C'Y'     TEST SPECIAL PAGE SKIPPING                   
         BNE   NP5F                                                             
         MVI   PAGOPT,C'P'         PUB                                          
         CLC   QSORT,=C'07'                                                     
         BNE   *+8                                                              
         MVI   PAGOPT,C'M'                                                      
NP5F     DS    0H                                                               
         B     NP6                                                              
*                                                                               
NP5N     DS    0H                                                               
         MVC   PROF,PCLTPROF                                                    
         CLC   QEST,SPACES                                                      
         BE    NP6                                                              
         CLI   PESTPROF+18,C'0'                                                 
         BE    NP6                                                              
         MVC   PROF,PESTPROF                                                    
*                                                                               
NP6      DS    0H                                                               
         CLI   DOLLOPT,C'S'                                                     
         BE    *+10                                                             
         MVC   DOLLOPT,PROF+19          $                                       
         MVI   PUBOPT,C'V'         ONLY VENDOR TOTALS                           
         CLI   PROF+27,C'0'                                                     
         BE    NP8                                                              
         CLI   PROF+27,C'T'                                                     
         BE    NP8                                                              
         MVI   PUBOPT,C'E'         EDIT + VENDOR TOTALS                         
NP8      DS    0H                                                               
         MVI   JOBOPT,0                                                         
         CLI   SPCOPT,0                                                         
         BE    NP10                                                             
         CLI   PROF+31,C'0'                                                     
         BE    NP10                                                             
         CLI   PROF+31,C'N'                                                     
         BE    NP10                                                             
         CLC   QPUB(2),=C' J'                                                   
         CLI   PROF+31,C'N'                                                     
         BE    NP10                                                             
         MVC   JOBOPT,PROF+31                                                   
         CLI   DATSW,C'J'          SEE IF SHOWING JOB IN GRID                   
         BNE   *+8                                                              
         MVI   JOBOPT,0            NO-OP JOB,COPY,CAPTION DISPLAY               
NP10     DS    0H                                                               
         MVI   CIRCOPT,0                                                        
         CLI   PROF+30,C'0'                                                     
         BE    *+10                                                             
         MVC   CIRCOPT,PROF+30                                                  
*                                                                               
         GOTO1 ADTL                                                             
         GOTO1 ASETHEAD                                                         
*                                                                               
         B     NPX                                                              
*                                  SET TOTAL SWITCHES                           
NP11     DS    0H                                                               
         LA    R3,APUBTOTS                                                      
         LA    R4,4                                                             
         LA    R5,ADDRSX-1                                                      
         MVI   0(R3),0                                                          
         BXLE  R3,R4,*-4                                                        
*                                                                               
         MVI   APUBTOTS,1                                                       
         MVI   AVENTOTS,1                                                       
         CLC   QSORT,=C'07'                                                     
         BNE   *+8                                                              
         MVI   AMKTTOTS,1                                                       
         CLI   QDIST,C' '                                                       
         BE    *+8                                                              
         MVI   ADSTTOTS,1                                                       
         CLI   QREGION,C' '                                                     
         BE    *+8                                                              
         MVI   AREGTOTS,1                                                       
         CLI   QEST,C' '                                                        
         BE    NP13                                                             
         CLI   QESTEND,C' '                                                     
         BNE   NP13                                                             
         MVI   AESTTOTS,1                                                       
         CLC   QEST,=C'ALL'                                                     
         BNE   NP13                                                             
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    NP13                                                             
         CLI   QPRODUCT,C' '                                                    
         BNE   NP14                IF ONE EST - NO HIGHER TOTALS                
NP13     DS    0H                                                               
         CLI   QPRODUCT,C' '                                                    
         BE    NP13B                                                            
         MVI   APRDTOTS,1                                                       
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   NP14                                                             
NP13B    DS    0H                                                               
*                                                                               
         MVI   ACLTTOTS,1                                                       
         CLI   QDIV,C' '                                                        
         BE    *+8                                                              
         MVI   ADIVTOTS,1                                                       
NP14     DS    0H                                                               
         CLI   LEVSW,C'C'                                                       
         BNE   *+8                                                              
         MVI   AREQTOTS,1                                                       
*                                                                               
         L     RE,ASPCTAB1                                                      
         L     RF,=A(ACCSL)                                                     
         XCEF                                                                   
*                                                                               
*                                  SET BSPARS                                   
         SR    R1,R1                                                            
         L     R2,ASPCTAB1                                                      
         SR    R3,R3                                                            
         LA    R4,STL                                                           
         LA    R5,STKL                                                          
         LA    R6,NSPC                                                          
         STM   R1,R6,BSPARS1                                                    
*                                                                               
         L     R2,ASPCTAB2                                                      
         STM   R1,R6,BSPARS2                                                    
*                                                                               
         MVC   ANXTTOT1,ASPCTOT1                                                
         MVC   ANXTCOM,ACOMTAB                                                  
         MVC   ANXTTOT2,ASPCTOT2                                                
NPX      DS    0H                                                               
         XIT                                                                    
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                  FORMAT SPACE DESC                            
         SPACE 2                                                                
SETSPCE  CSECT                                                                  
         NMOD1 0,SETSPCE                                                        
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         LA    R4,SPCN                                                          
         LA    R4,SPCN+1           INDENT                                       
         MVI   SPCN,C' '                                                        
         MVC   SPCN+1(256),SPCN                                                 
         MVC   SPCN+257(L'SPCN-257),SPCN                                        
         CLI   SPCOPT,0                                                         
         BE    SSX                                                              
         L     R3,0(R1)                                                         
         USING STSD,R3                                                          
         CLI   QMEDIA,C'N'                                                      
         BE    SS20                                                             
*                                  MAG AND OUTDOOR                              
         CLI   STDSPACE,0                                                       
         BE    SSX                                                              
         MVC   0(17,R4),STDSPACE                                                
         MVC   NML(17,R4),STDSPACE+17                                           
SS4      DS    0H                                                               
         CLI   COSTOPT,0                                                        
         BE    SS30                                                             
         EDIT  (B4,STCOST),(13,X),2,COMMAS=YES,FLOAT=$,ALIGN=LEFT               
*                                                                               
         LR    R5,R0                                                            
         LA    R7,NML-2(R4)                                                     
         CLI   0(R7),C' '                                                       
         BH    *+8                                                              
         BCT   R7,*-8                                                           
         LA    RF,NML-2(R4)                                                     
         SR    RF,R7                                                            
         CR    RF,R5               WILL $ FIT ON SMAE LINE                      
         BNE   *+8                 YES                                          
         LA    R7,NML-3(R4)        NO                                           
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R7),X                                                        
         B     SS30                                                             
*                                  NEWSPAPER FORMAT                             
SS20     DS    0H                                                               
         CLI   STDSPC2,C' '                                                     
         BNH   SS20D                                                            
*                                  ELSE MUST BE SPACE BUY                       
         MVC   0(8,R4),STDSPC2                                                  
         LA    R5,9(R4)                                                         
         B     SS23                                                             
SS20D    DS    0H                                                               
         LA    R0,1                                                             
         MVI   0(R4),C'0'                                                       
         OC    STUNITS,STUNITS                                                  
         BE    SS21                                                             
         EDIT  (B4,STUNITS),(5,0(R4)),ALIGN=LEFT                                
SS21     DS    0H                                                               
         LR    R5,R4                                                            
         AR    R5,R0                                                            
         MVC   1(5,R5),=C'LINES'                                                
         LA    RF,5(R5)                                                         
         CLI   STUNIND,C'L'                                                     
         BE    *+14                                                             
         MVC   1(6,R5),=C'INCHES'                                               
         LA    RF,6(R5)                                                         
         CLC   STUNITS,=F'1'                                                    
         BNE   *+8                                                              
         MVI   0(RF),C' '                                                       
*                                                                               
         LA    R5,8(R5)                                                         
SS23     DS    0H                                                               
         CLI   STPREM,0                                                         
         BE    SS24                                                             
         MVC   0(1,R5),STPREM                                                   
         OI    0(R5),X'F0'                                                      
         MVI   1(R5),C'C'                                                       
         LA    R5,3(R5)                                                         
*                                                                               
SS24     DS    0H                                                               
         CLI   COSTOPT,0                                                        
         BE    SS30                                                             
         EDIT  (B4,STCOST),(13,X),2,COMMAS=YES,FLOAT=$,ALIGN=LEFT               
*                                                                               
         MVC   0(13,R5),X                                                       
*                                  DO ANY COMMENTS                              
SS30     DS    0H                                                               
         CLI   STCOMX,0                                                         
         BE    SS40                                                             
*                                  FIND FIRST BLANK LINE                        
         LA    R4,SPCN                                                          
         CLI   1(R4),C' '                                                       
         BNH   *+12                                                             
         LA    R4,NML(R4)                                                       
         B     *-12                                                             
*                                                                               
         XC    SAVDAT,SAVDAT                                                    
         L     R5,ACOMTAB                                                       
SS32     DS    0H                                                               
         C     R5,ANXTCOM          TEST END                                     
         BNL   SS39                                                             
         CLC   STCOMX,0(R5)       TEST COMMENT FOR THIS SPACE                   
         BE    SS34                                                             
SS33     DS    0H                                                               
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     SS32                                                             
*                                                                               
SS34     DS    0H                                                               
         MVC   X,SPACES                                                         
         MVC   X2,SPACES                                                        
         LA    RF,X                                                             
         CLC   SAVDAT,2(R5)                                                     
         BE    SS34D                                                            
         MVC   SAVDAT,2(R5)                                                     
         GOTO1 DATCON,DMCB,(3,2(R5)),(5,X)                                      
         LA    RF,X+5                                                           
         MVC   X+5(3),SPACES                                                    
         MVI   0(RF),C'-'                                                       
SS34D    DS    0H                                                               
         ZIC   RE,1(R5)                                                         
         SH    RE,=H'7'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RF),6(R5)                                                    
*                                                                               
         GOTO1 ACHOPPER,DMCB,(55,X),(26,X+60),3                                 
         LA    RF,X+60                                                          
SS35     DS    0H                                                               
         MVC   1(NML-1,R4),0(RF)                                                
         LA    R4,NML(R4)                                                       
         LA    RF,NML-1(RF)                                                     
         CLC   0(NML-1,RF),SPACES                                               
         BNE   SS35                                                             
*                                                                               
         B     SS33                                                             
*                                                                               
SS39     DS    0H                                                               
*                                  JOB INFO                                     
SS40     DS    0H                                                               
         CLI   JOBOPT,0                                                         
         BE    SS60                                                             
         CLI   STPRDJOB,0          NO JOB                                       
         BE    SS60                                                             
         CLI   DATSW,C'J'                                                       
         BE    SS60                                                             
         CLI   JOBOPT,C'A'         AD NO. ONLY                                  
         BE    SS42                DONT NEED JOBREC                             
         XC    WORK,WORK                                                        
         MVC   WORK(3),RCSVAGY                                                  
         MVI   WORK+3,X'15'                                                     
         MVC   WORK+4(3),PCLTKCLT                                               
         MVC   WORK+7(9),STPRDJOB                                               
         L     R3,AJOBLST                                                       
SS40B    DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    SS41                                                             
         CLC   WORK(25),0(R3)                                                   
         BE    SS42                                                             
         LA    R3,102(R3)                                                       
         B     SS40B                                                            
SS41     DS    0H                  READ JOB REC                                 
         XC    WORK(64),KEY                                                     
         XC    KEY(64),WORK                                                     
         XC    WORK(64),KEY                                                     
*                                                                               
         BAS   RE,READ60                                                        
         LA    R0,PJOBREC                                                       
         ST    R0,AREC                                                          
         BAS   RE,GET                                                           
         MVC   KEY,WORK                                                         
         CLC   QSORT,SPACES        IF SORTING                                   
         BNE   *+8                 DONT RESTORE SEQ                             
         BAS   RE,HIGH60           RESTORE SEQ                                  
*                                                                               
         C     R3,AJOBLSTX                                                      
         BL    *+8                                                              
         SH    R3,=H'102'          USE LAST SPACE OVER                          
         MVC   0(102,R3),PJOBREC                                                
         MVI   102(R3),X'FF'                                                    
*                                                                               
SS42     DS    0H                                                               
         LA    R4,SPCN+1                                                        
         CLI   0(R4),C' '                                                       
         BNH   *+12                FIRST BLANK LINE                             
         LA    R4,NML(R4)                                                       
         B     *-12                                                             
*                                  A=AD NO.,   B=+CAPTION                       
*                                  1=COPY NO., 2=+CAPTION                       
         TM    JOBOPT,X'30'                                                     
         BNZ   SS44                                                             
         MVC   0(8,R4),=C'AD NO. ='                                             
         MVC   9(6,R4),PJOBKJOB-PJOBREC(R3)                                     
         CLI   JOBOPT,C'A'                                                      
         BNE   *+10                                                             
         MVC   9(6,R4),STPRDJOB+3                                               
         B     SS46                                                             
*                                                                               
SS44     DS    0H                                                               
         MVC   0(6,R4),=C'COPY ='                                               
         MVC   7(17,R4),PJOBCPY-PJOBREC(R3)                                     
*                                                                               
SS46     DS    0H                                                               
         TM    JOBOPT,X'02'                                                     
         BZ    SS48                                                             
         LA    R4,NML(R4)                                                       
         MVC   0(25,R4),PJOBCAP1-PJOBREC(R3)                                    
         MVC   NML(25,R4),PJOBCAP2-PJOBREC(R3)                                  
*                                                                               
SS48     DS    0H                                                               
*                                                                               
SS60     DS    0H                                                               
SSX      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                  DATAMGR                                      
HIGH60   LA    R0,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     DIR                                                              
*                                                                               
READ60   LA    R0,DMREAD                                                        
         B     DIR                                                              
*                                                                               
SEQ60    LA    R0,DMRSEQ                                                        
*                                                                               
DIR      DS    0H                                                               
         ST    R0,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,,PRTDIR,KEY,KEY                                     
*                                                                               
         B     DMCHK                                                            
         SPACE 3                                                                
GET      LA    R0,GETREC                                                        
*                                                                               
FIL      DS    0H                                                               
         ST    R0,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,,PRTFILE,KEY+27,AREC,DMWORK                         
*                                                                               
DMCHK    DS    0H                                                               
         LR    RE,R0                                                            
         MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,DMCB+8                                                      
         BZR   RE                                                               
         DC    H'0'                                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                  FORMAT DATE ARRAY                            
         SPACE 2                                                                
DATOUT   CSECT                                                                  
         NMOD1 0,DATOUT                                                         
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
*                                  CLEAR DTLINS                                 
         L     R4,ADTLINS                                                       
         LA    R0,DTLINN                                                        
         L     R2,0(R1)            A(TOTS)                                      
         MVC   0(DTLINL,R4),SPACES                                              
         LA    R4,DTLINL(R4)                                                    
         BCT   R0,*-10                                                          
         SH    R4,=Y(DTLINL*3)                                                  
         ST    R4,ALASTLIN         A(LAST USABLE LINE)                          
         CLI   DATSW,C'S'                                                       
         BE    DT20                                                             
         CLI   0(R1),C'D'          SET IN GETSPCE - DATES ONLY AT               
         BNE   DT20                LOWEST LEVEL ON REPORT                       
         L     R4,ADTLINS          FIRST 'MONTH'                                
         LA    R5,24(R2)           POINT PAST TOTALS                            
         L     R6,NMOS                                                          
DT6      DS    0H                                                               
         OC    0(32,R5),0(R5)                                                   
         BZ    DT14                NO DAYS                                      
         LR    RE,R4                                                            
         LR    R7,R5                                                            
         LA    R0,32               MAX DAYS                                     
         SR    R1,R1                                                            
         CLI   DATSW,C'P'                                                       
         BNE   DT7F                                                             
*                                  PRD'S NOT DATES                              
         LA    R3,8                                                             
DT7      DS    0H                                                               
         OC    0(4,R7),0(R7)                                                    
         BZ    DT14                NO MORE PRDS                                 
         CLI   DATSW2,C'D'         IF DATES ALSO                                
         BNE   DT7C                                                             
         CLC   0(3,R7),=C'***'                                                  
         BE    DT7C                                                             
*                                  SET PRD AND DATE                             
         LR    RF,R7                                                            
         SH    RF,=H'4'                                                         
         CLC   0(3,RF),0(R7)                                                    
         BE    *+14                DONT REPEAT PRD IF SAME AS BEFORE            
         MVC   2(3,RE),0(R7)                                                    
         LA    RE,DTLINL(RE)                                                    
*                                                                               
         MVI   2(RE),C'-'                                                       
         EDIT  (B1,3(R7)),(2,3(RE)),FILL=0                                      
         LA    RE,DTLINL(RE)                                                    
         B     DT7E                                                             
*                                                                               
DT7C     DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,3(R7)                                                         
DT7D     DS    0H                                                               
         C     RE,ALASTLIN                                                      
         BNL   DT15                OVERFLOW                                     
         MVC   2(3,RE),0(R7)                                                    
         LA    RE,DTLINL(RE)                                                    
         BCT   RF,DT7D                                                          
*                                                                               
DT7E     DS    0H                                                               
         LA    R7,4(R7)                                                         
         BCT   R3,DT7                                                           
         B     DT14                                                             
*                                                                               
DT7F     DS    0H                                                               
         CLI   DATSW,C'J'                                                       
         BNE   DT8                                                              
*                                  JOBS NOT DATES                               
*                                                                               
         LA    R3,4                                                             
DT7H     DS    0H                                                               
         OC    0(7,R7),0(R7)                                                    
         BZ    DT14                                                             
         C     RE,ALASTLIN                                                      
         BNL   DT15                OVERFLOW                                     
*                                                                               
         LR    RF,R7                                                            
         SH    RF,=H'7'                                                         
         CLC   0(6,RF),0(R7)                                                    
         BE    DT7H4               DONT REPEAT JOB IF SAME AS BEFORE            
         MVC   2(3,RE),0(R7)                                                    
         LA    RE,DTLINL(RE)                                                    
         MVC   2(3,RE),3(R7)                                                    
         OC    2(3,RE),SPACES                                                   
         CLC   2(3,RE),SPACES                                                   
         BE    *+8                                                              
         LA    RE,DTLINL(RE)                                                    
         C     RE,ALASTLIN                                                      
         BNL   DT15                OVERFLOW                                     
*                                                                               
DT7H4    DS    0H                                                               
         CLI   DATSW2,C'D'         IF DATES ALSO                                
         BNE   DT7I                                                             
         CLC   0(3,R7),=3C'*'                                                   
         BE    DT7I                                                             
*                                  SET DATE HERE                                
         MVI   2(RE),C'-'                                                       
         EDIT  (B1,6(R7)),(2,3(RE)),FILL=0                                      
         B     DT7J                                                             
*                                                                               
DT7I     DS    0H                                                               
         MVI   0(RE),0                                                          
         CLI   6(R7),1                                                          
         BE    DT7J                                                             
*                                                                               
         EDIT  (B1,6(R7)),(3,1(RE))                                             
         MVI   4(RE),C')'                                                       
         LA    RF,3(RE)                                                         
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   0(RF),C'('                                                       
DT7J     DS    0H                                                               
         LA    R7,7(R7)                                                         
         LA    RE,DTLINL(RE)                                                    
         BCT   R3,DT7H                                                          
*                                                                               
         B     DT14                                                             
DT8      DS    0H                                                               
         CLI   0(R7),0                                                          
         BE    DT12                NOTHING THIS DAY                             
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB                                                      
         MVC   BYTE,0(R7)                                                       
         NI    BYTE,X'7F'          STRIP HOB                                    
         SR    RF,RF                                                            
         IC    RF,BYTE            NO. OF TIMES THIS DAY                         
DT10     DS    0H                                                               
         C     RE,ALASTLIN                                                      
         BNL   DT15                OVERFLOW                                     
         MVI   3(RE),C'X'                                                       
         CLI   DATSW,C'X'                                                       
         BE    DT10B                                                            
         MVC   2(2,RE),WORK                                                     
         TM    0(R7),X'80'         TEST PRIOR                                   
         BZ    *+8                                                              
         MVI   1(RE),C'P'          PRIOR MONTH                                  
DT10B    DS    0H                                                               
         LA    RE,DTLINL(RE)                                                    
         BCT   RF,DT10                                                          
DT12     DS    0H                                                               
         LA    R7,1(R7)            NEXT DAY                                     
         LA    R1,1(R1)                                                         
         BCT   R0,DT8                                                           
*                                                                               
DT14     DS    0H                                                               
         LA    R4,5(R4)            NEXT 'MONTH'                                 
         LA    R5,36(R5)                                                        
         BCT   R6,DT6                                                           
*                                                                               
*                                  IF DOING DATES - NO TOTAL                    
*                                  ROW, ONLY TOTAL COLUMN                       
         L     R4,ADTLINS                                                       
         L     RF,NMOS                                                          
         MH    RF,=H'5'                                                         
         AR    R4,RF                                                            
         B     DT25                                                             
*                                                                               
DT15     DS    0H                                                               
*                                  LINES OVERFLOW                               
         L     RE,ALASTLIN                                                      
         MVC   0(72,RE),=C'*** TOO MANY LINES - SOME DETAIL NOT PRINTEDX        
               , BUT TOTALS ARE CORRECT ***'                                    
*                                                                               
         B     DT14                                                             
*                                                                               
DT20     DS    0H                                                               
*                                  FIND FIRST BLANK DTLIN                       
         L     R4,ADTLINS                                                       
         LA    R0,DTLINN                                                        
         CLC   0(DTLINL,R4),SPACES                                              
         BE    *+14                                                             
         LA    R4,DTLINL(R4)                                                    
         BCT   R0,*-14                                                          
         DC    H'0'                TOO MANY DATES                               
*                                                                               
         SH    R0,=Y(DTLINN+1)                                                  
         LCR   R0,R0                                                            
         ST    R0,DTNEED           NO. OF LINES USED                            
         LA    R5,24(R2)           POINT PAST TOTALS                            
         L     R6,NMOS                                                          
DT22     DS    0H                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),32(R5)    'APPARENT' INSERTIONS                        
         CLI   RISW,C'R'                                                        
         BNE   *+10                                                             
         MVC   FULL+2(2),34(R5)    'REAL' INSERTIONS                            
         OC    FULL,FULL                                                        
         BZ    DT24                                                             
         EDIT  (B4,FULL),(5,WORK)                                               
*                                                                               
         OI    WORK+4,C'0'                                                      
         MVC   1(4,R4),WORK+1                                                   
         CLI   WORK,C' '           DONT CLOBBER * WITH SPACES                   
         BE    *+10                                                             
         MVC   0(1,R4),WORK                                                     
         CLI   TOTSW,1                                                          
         BNE   *+8                                                              
         MVI   5(R4),C'*'                                                       
*                                                                               
DT24     DS    0H                                                               
         LA    R4,5(R4)                                                         
         LA    R5,36(R5)                                                        
         BCT   R6,DT22                                                          
DT25     DS    0H                                                               
*                                  TOTAL INSERTIONS                             
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),16(R2)    'APPARENT' INSERTIONS                        
         CLI   RISW,C'R'                                                        
         BNE   *+10                                                             
         MVC   FULL+2(2),18(R2)    'REAL' INSERTIONS                            
         EDIT  (B4,FULL),(5,WORK)                                               
*                                                                               
         OI    WORK+4,C'0'                                                      
         MVC   1(5,R4),WORK                                                     
         CLI   TOTSW,1                                                          
         BNE   *+8                                                              
         MVI   6(R4),C'*'                                                       
*                                                                               
*                                  DO MONTH TOTS IF ANY                         
         MVI   MTOTLNS,C' '                                                     
         MVC   MTOTLNS+1(L'MTOTLNS-1),MTOTLNS                                   
         LA    RF,TOTL-MAXMOS*16(R2)     POINT TO FIRST MONTH                   
         OC    0(MAXMOS*16,RF),0(RF)                                            
         BZ    DTX                 NONE                                         
*                                                                               
         IC    RE,LNEED                                                         
         LA    RE,3(RE)                                                         
         STC   RE,LNEED                                                         
*                                                                               
         LA    R4,MTOTLN1                                                       
         LR    R5,RF                                                            
         LA    R6,(MAXMOS+1)/2                                                  
DT27     DS    0H                                                               
         BAS   RE,MTEDT                                                         
         LA    R4,10(R4)                                                        
         LA    R5,16*2(R5)                                                      
         BCT   R6,DT27                                                          
*                                                                               
         LA    R4,MTOTLN2+5                                                     
         LA    R5,16(RF)                                                        
         LA    R6,MAXMOS/2                                                      
DT28     DS    0H                                                               
         BAS   RE,MTEDT                                                         
         LA    R4,10(R4)                                                        
         LA    R5,16*2(R5)                                                      
         BCT   R6,DT28                                                          
*                                                                               
*                                  PUT MONTHLY $ ON 1 LN IF POSS.               
         LA    R4,MTOTLN1+1                                                     
         LA    R0,MTOTLNL-2                                                     
DT29     DS    0H                                                               
         CLI   0(R4),C' '                                                       
         BNH   DT30                                                             
         CLC   MTOTLNL-1(3,R4),SPACES                                           
         BNE   DTX                                                              
DT30     DS    0H                                                               
         LA    R4,1(R4)                                                         
         BCT   R0,DT29                                                          
*                                                                               
         OC    MTOTLN1,MTOTLN2                                                  
         MVC   MTOTLN2,SPACES                                                   
*                                                                               
*                                                                               
DTX      DS    0H                                                               
         MVI   TOTSW,0                                                          
         XIT1                                                                   
         SPACE 2                                                                
MTEDT    NTR1                                                                   
         USING DOLLD,R5                                                         
         CLI   DOLLS,X'06'                                                      
         BE    MT2                                                              
         L     R0,TGRS                                                          
         CLI   DOLLS,X'01'         GROSS                                        
         BE    MT3                                                              
         CLI   DOLLS,X'04'         GROSS/GLCD                                   
         BE    MT3                                                              
         S     R0,TAC                                                           
         B     MT3                                                              
*                                                                               
MT2      DS    0H                                                               
         L     R0,TCOST                                                         
*                                                                               
MT3      DS    0H                                                               
         LTR   R0,R0                                                            
         BZ    MTEDTX                                                           
         DROP  R5                                                               
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LR    R7,R1                                                            
         MVC   X(20),SPACES                                                     
         EDIT  (R7),(11,X),FLOAT=$,COMMAS=YES                                   
         LA    R3,X+4                                                           
         C     R7,=F'1000'                                                      
         BL    MT4                                                              
         LA    R3,X+3                                                           
         C     R7,=F'10000'                                                     
         BL    MT4                                                              
         LA    R3,X+2                                                           
         C     R7,=F'1000000'                                                   
         BL    MT4                                                              
         LA    R3,X+1                                                           
MT4      DS    0H                                                               
         MVC   0(10,R4),0(R3)                                                   
MTEDTX   DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                  SET DATE LIST                                
DTL      CSECT                                                                  
         NMOD1 0,DTL                                                            
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         XC    DTLIST,DTLIST                                                    
*        GOTO1 DTCNV,DMCB,QSTART,(1,BSTART)                                     
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
*        GOTO1 (RF),(R1),QEND,(1,BEND)                                          
         GOTO1 DATCON,(R1),(0,QEND),(3,BEND)                                    
         SR    R6,R6                                                            
         LA    R4,DTLIST                                                        
         LA    R5,MAXMOS                                                        
         MVC   0(3,R4),BSTART                                                   
         CLI   WEEKSW,0                                                         
         BNE   DTL10                                                            
         MVI   2(R4),1                                                          
DTL4     DS    0H                                                               
         CLC   0(3,R4),BEND                                                     
         BH    DTL7                                                             
         CLI   1(R4),12                                                         
         BE    DTL5                                                             
         IC    R6,1(R4)                                                         
         LA    R6,1(R6)                                                         
         STC   R6,5(R4)                                                         
         MVC   4(1,R4),0(R4)                                                    
         B     DTL6                                                             
DTL5     DS    0H                                                               
         IC    R6,0(R4)                                                         
         LA    R6,1(R6)                                                         
         STC   R6,4(R4)                                                         
         MVI   5(R4),1                                                          
DTL6     DS    0H                                                               
         MVI   6(R4),1                                                          
         LA    R4,4(R4)                                                         
         BCT   R5,DTL4                                                          
DTL7     DS    0H                                                               
         MVC   0(4,R4),=4X'FF'                                                  
         B     DTL20                                                            
*                                                                               
DTL10    DS    0H                                                               
         MVC   WORK(6),QSTART                                                   
         LA    R3,1                1 DAY IF DAY REQ                             
         CLI   WEEKSW,C'D'                                                      
         BE    *+8                                                              
         LA    R3,7                7 DAYS IF WEEK REQ                           
*                                                                               
DTL11    DS    0H                                                               
         CLC   0(3,R4),BEND                                                     
         BH    DTL12                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)                                      
*                                                                               
*        GOTO1 DTCNV,DMCB,WORK+6,(1,4(R4))                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,4(R4))                                 
*                                                                               
         MVC   WORK(6),WORK+6                                                   
         LA    R4,4(R4)                                                         
         BCT   R5,DTL11                                                         
*                                                                               
DTL12    DS    0H                                                               
         MVC   0(4,R4),=4X'FF'                                                  
*                                                                               
DTL20    DS    0H                                                               
         SH    R5,=Y(MAXMOS)                                                    
         LCR   R5,R5                                                            
         ST    R5,NMOS             NO OF 'MONTHS'                               
*                                                                               
DTLX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                  SET SAVED HEADLINES                          
         SPACE 2                                                                
SETHEAD  CSECT                                                                  
         NMOD1 0,SETHEAD                                                        
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
*                                  SET RPTNAME                                  
         MVC   X,SPACES                                                         
         MVC   X(10),PAGYMED                                                    
*                                                                               
         LA    R4,PCLTREC+33                                                    
         MVI   ELCODE,X'41'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   X(10),2(R4)                                                      
*                                                                               
         LA    R7,X+10                                                          
         CLI   0(R7),C' '                                                       
         BH    *+8                                                              
         BCT   R7,*-8                                                           
         MVC   2(8,R7),=C'SCHEDULE'                                             
         LA    R0,X-1-9                                                         
         SR    R7,R0                                                            
         LA    RF,19                                                            
         SR    RF,R7                                                            
         SRL   RF,1                                                             
         LA    RF,RPTNAME(RF)                                                   
         MVC   RPTNAME(38),SPACES                                               
         MVC   0(19,RF),X                                                       
         LA    RF,19(RF)                                                        
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DASHES                                                   
         SPACE 3                                                                
         LA    R4,MYHA                                                          
         LA    R0,3                3 LINES                                      
         MVC   0(132,R4),SPACES                                                 
         LA    R4,132(R4)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         SR    R4,R4                                                            
         MVC   WORK,SPACES                                                      
         CLI   LEVSW,C'P'                                                       
         BNE   SH4                                                              
         MVC   WORK(6),=C'VENDOR'                                               
         LA    R4,6                                                             
         B     SH15                                                             
*                                                                               
SH4      DS    0H                                                               
         CLI   LEVSW,C'M'                                                       
         BNE   SH5                                                              
         MVC   WORK(6),=C'MARKET'                                               
         LA    R4,6                                                             
         B     SH15                                                             
*                                                                               
SH5      DS    0H                                                               
         CLI   DATSW,0                                                          
         BNE   *+8                                                              
         MVI   DATSW,C'S'          NO DATES                                     
         CLI   LEVSW,C'D'                                                       
         BNE   SH6                                                              
         CLI   QREGION,C' '                                                     
         BE    SH5B                                                             
         MVC   WORK(15),=C'REGION/DISTRICT'                                     
         LA    R4,15                                                            
         B     SH15                                                             
SH5B     DS    0H                                                               
         MVC   WORK(8),=C'DISTRICT'                                             
         LA    R4,8                                                             
         B     SH15                                                             
*                                                                               
SH6      DS    0H                                                               
         CLI   LEVSW,C'R'                                                       
         BNE   SH7                                                              
         MVC   WORK(6),=C'REGION'                                               
         LA    R4,6                                                             
         B     SH15                                                             
*                                                                               
SH7      DS    0H                                                               
         CLI   LEVSW,C'E'                                                       
         BNE   SH8                                                              
         MVC   WORK(8),=C'ESTIMATE'                                             
         LA    R4,8                                                             
         B     SH15                                                             
*                                                                               
SH8      DS    0H                                                               
         CLI   LEVSW,C'B'                                                       
         BNE   SH9                                                              
         MVC   WORK(7),=C'PRODUCT'                                              
         LA    R4,7                                                             
         B     SH15                                                             
*                                                                               
SH9      DS    0H                                                               
         CLI   LEVSW,C'V'                                                       
         BNE   SH10                                                             
         MVC   WORK(8),=C'DIVISION'                                             
         LA    R4,8                                                             
         B     SH15                                                             
*                                                                               
SH10     DS    0H                                                               
*                                                                               
SH15     DS    0H                                                               
         LA    R5,MYHA                                                          
         CLI   SPCOPT,0                                                         
         BE    SH16                                                             
         MVC   0(20,R5),WORK                                                    
         MVC   132(17,R5),=C'SPACE DESCRIPTION'                                 
         LA    R1,0(R5,R4)                                                      
         LTR   R4,R4                                                            
         BZ    *+8                                                              
         MVI   0(R1),C'/'                                                       
         CH    R4,=H'17'                                                        
         BNL   *+8                                                              
         LA    R4,16                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   264(0,R5),DASHES                                                 
*                                                                               
         B     SH20                                                             
SH16     DS    0H                                                               
         LTR   R4,R4                                                            
         BZ    SH20                                                             
         MVC   132(20,R5),WORK                                                  
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   264(0,R5),DASHES                                                 
*                                                                               
*                                  DATES                                        
SH20     DS    0H                                                               
         GOTO1 ADTL                SET DATE LIST                                
         LA    R4,DTLIST                                                        
         LA    R5,MYHA+28                                                       
SH21     DS    0H                                                               
         CLI   0(R4),X'FF'                                                      
         BNE   SH22                                                             
         MVC   132(5,R5),=C'TOTAL'                                              
         MVC   264(5,R5),DASHES                                                 
         B     SH30                                                             
*                                                                               
SH22     DS    0H                                                               
*        GOTO1 DTCNV,DMCB,(1,0(R4)),(4,WORK)                                    
         GOTO1 DATCON,DMCB,(3,0(R4)),(7,WORK)                                   
*                                                                               
         CLI   WEEKSW,0                                                         
         BNE   SH23                                                             
         MVC   133(3,R5),WORK                                                   
         B     SH24                                                             
SH23     DS    0H                                                               
         MVC   001(3,R5),WORK                                                   
         MVC   134(2,R5),WORK+3                                                 
*                                                                               
SH24     DS    0H                                                               
         MVC   265(3,R5),DASHES                                                 
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R5,5(R5)                                                         
         B     SH21                                                             
*                                  $ COLUMNS                                    
SH30     DS    0H                                                               
         XC    DOLLS,DOLLS                                                      
         CLI   DOLLOPT,C'S'                                                     
         BE    SH40                                                             
         LA    R4,DOLLST                                                        
SH31     DS    0H                                                               
         CLC   DOLLOPT,0(R4)                                                    
         BE    SH32                                                             
         CLI   0(R4),C' '                                                       
         BE    SH32                                                             
         LA    R4,3(R4)                                                         
         B     SH31                                                             
SH32     DS    0H                                                               
         MVC   DOLLS,1(R4)                                                      
*                                                                               
         LA    R5,6(R5)                                                         
         ST    R5,ADOLL                                                         
         LA    R4,DOLLS                                                         
         LA    R0,2                                                             
SH33     DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    SH34                                                             
         SR    RF,RF                                                            
         IC    RF,0(R4)                                                         
         MH    RF,=H'36'                                                        
         LA    RF,HEADWDS-36(RF)                                                
         MVC   000(12,R5),00(RF)                                                
         MVC   132(12,R5),12(RF)                                                
         MVC   264(12,R5),24(RF)                                                
*                                                                               
SH34     DS    0H                                                               
         LA    R4,1(R4)                                                         
         LA    R5,13(R5)                                                        
         BCT   R0,SH33                                                          
*                                                                               
         L     RF,ADOLL                                                         
         LA    R0,MYHA                                                          
         SR    RF,R0                                                            
         ST    RF,ADOLL                                                         
SH40     DS    0H                                                               
         XC    BILPROF,BILPROF                                                  
         MVC   BFORMD,SPACES                                                    
         CLI   DOLLS,X'06'                                                      
         BE    SH41                                                             
         CLI   DOLLS+1,X'06'                                                    
         BNE   SH50                                                             
SH41     DS    0H                                                               
         CLI   QPRODUCT,C' '                                                    
         BE    SH43                                                             
         CLI   QEST,C' '                                                        
         BE    SH42                                                             
         OC    BILPROF,PESTBILP                                                 
         BNZ   SH46                                                             
SH42     DS    0H                                                               
         OC    BILPROF,PPRDBILP                                                 
         BNZ   SH46                                                             
*                                  AAA                                          
SH43     DS    0H                                                               
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(7),RCSVAGY                                                   
         MVC   KEY+7(3),=C'AAA'                                                 
         CLI   QEST,C' '                                                        
         BE    SH44                                                             
         MVC   KEY+10(2),PESTKEST                                               
         MVI   KEY+3,7                                                          
         CLC   QEST,=C'ALL'                                                     
         BE    SH44B                                                            
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STH   R0,KEY+10                                                        
         B     SH44B                                                            
SH44     DS    0H                                                               
         MVI   KEY+3,6                                                          
SH44B    DS    0H                                                               
         BAS   RE,SHHIGH                                                        
         CLC   KEY(25),KEYSAVE                                                  
         BE    SH45                                                             
*                                                                               
         CLI   KEYSAVE+3,6                                                      
         BE    SH46                                                             
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+10(2),KEY+10                                                 
         B     SH44                                                             
*                                                                               
SH45     DS    0H                                                               
         L     R0,ALISREC                                                       
         ST    R0,AREC                                                          
         BAS   RE,SHGET                                                         
         L     R2,ALISREC                                                       
         LA    R2,PPRDBILP-PPRDREC(R2)                                          
         CLI   KEY+3,6                                                          
         BE    *+8                                                              
         LA    R2,PESTBILP-PESTREC(R2)                                          
         OC    BILPROF,0(R2)                                                    
         MVC   KEY(64),WORK                                                     
         CLC   QSORT,SPACES        IF SORTING                                   
         BNE   *+8                                                              
         BAS   RE,SHHIGH                                                        
SH46     DS    0H                                                               
         OC    BILBASA,BILBASA                                                  
         BNZ   *+10                                                             
         MVC   BILBASA(5),DFLTFORM                                              
         CLI   TAXOPT,C'N'                                                      
         BE    SH48                                                             
         GOTO1 AGETCOST,DMCB,(C'T',BILBASA),(1,BFORMD)                          
         B     SH50                                                             
*                                                                               
SH48     GOTO1 AGETCOST,DMCB,BILBASA,(1,BFORMD)                                 
*                                                                               
SH50     DS    0H                                                               
SHX      DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
DFLTFORM DC    X'0505000000'                                                    
         SPACE 3                                                                
*                                                                               
HEADWDS  DC    C'            '                                                  
         DC    C'  GROSS COST'                                                  
         DC    C'  ----------'                                                  
         DC    C'            '                                                  
         DC    C'    NET COST'                                                  
         DC    C'    --------'                                                  
         DC    C'      CASH  '                                                  
         DC    C'    DISCOUNT'                                                  
         DC    C'    --------'                                                  
         DC    C' GROSS COST/'                                                  
         DC    C'  LESS C.D. '                                                  
         DC    C' -----------'                                                  
         DC    C'   NET COST/'                                                  
         DC    C'   LESS C.D.'                                                  
         DC    C'   ---------'                                                  
         DC    C'            '                                                  
         DC    C'        COST'                                                  
         DC    C'        ----'                                                  
*                                                                               
         SPACE 3                                                                
*                                                                               
DOLLST   DC    C'1',X'0100'        G                                            
         DC    C'2',X'0403'        G + CD/G-CD                                  
         DC    C'3',X'0200'        N                                            
         DC    C'4',X'0503'        N + CD/N-CD                                  
         DC    C'5',X'0102'        G + N                                        
         DC    C'6',X'0405'        G + N/G-CD + N-CD                            
         DC    C'7',X'0403'        G + CD/G-CD                                  
         DC    C'8',X'0503'        N + CD/N-CD                                  
         DC    C'9',X'0105'        G + N/ - N-CD                                
         DC    C'B',X'0405'        G + N/G-CD + N-CD                            
         DC    C'C',X'0600'        COST                                         
         DC    C'D',X'0106'        G + COST                                     
         DC    C'E',X'0406'        G + COST/G-CD                                
         DC    C'F',X'0206'        N + COST                                     
         DC    C'G',X'0506'        N + COST/N-CD                                
*                                                                               
         DC    C' ',X'0403'        G + CD/G-CD (DEFAULT)                        
*                                                                               
         SPACE 2                                                                
SHHIGH   LA    R0,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
*                                                                               
SHDIR    DS    0H                                                               
         ST    R0,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,,PRTDIR,KEY,KEY                                     
         B     SHDMCK                                                           
*                                                                               
SHGET    LA    R0,GETREC                                                        
*                                                                               
SHFIL    DS    0H                                                               
         ST    R0,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,,PRTFILE,KEY+27,AREC,DMWORK                         
*                                                                               
SHDMCK   DS    0H                                                               
         LR    RE,R0                                                            
         MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,DMCB+8                                                      
         BZR   RE                                                               
         DC    H'0'                                                             
         SPACE 3                                                                
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLC   ELCODE,0(R4)                                                     
         BCR   8,RE                                                             
         CLI   0(R4),0                                                          
         BNE   *-18                                                             
         LTR   R4,R4                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                  POST TO SPACE TOTALS                         
         SPACE 2                                                                
POST     CSECT                                                                  
         NMOD1 0,POST                                                           
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         XC    X,X                                                              
         LA    R3,X                                                             
         USING STSD,R3                                                          
         LA    R5,PPBYOWRK                                                      
         USING PPBYOUTD,R5                                                      
*                                                                               
         MVC   BSPARS(24),BSPARS1  POST FIRST TO SPCTAB1                        
*                                                                               
         LA    RF,PBUYREC                                                       
         ST    RF,PBYOINPT                                                      
         MVC   PBYODTCN,DATCON                                                  
         LA    RF,GROSS                                                         
         ST    RF,PBYOVALS                                                      
         MVI   PBYOCTL,0                                                        
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
*                                                                               
         CLI   QMEDIA,C'N'                                                      
         BE    POST4                                                            
         MVC   STDSPACE(17),PBYOSPC                                             
         MVC   STDSPACE+17(17),PBYOSPC2                                         
         OC    STDSPACE(34),SPACES                                              
         B     POST8                                                            
*                                                                               
POST4    DS    0H                                                               
         CLI   PBYOSPC,C' '        TEST SPACE BUY                               
         BNH   POST5               NO                                           
         MVC   STDSPC2(8),PBYOSPC                                               
         MVI   STDSPACE,C'S'                                                    
         B     POST6                                                            
POST5    DS    0H                                                               
         ZAP   DUB,PBDUNITS                                                     
         CVB   R0,DUB                                                           
         ST    R0,STUNITS                                                       
         MVC   STUNIND,PBDUIND                                                  
POST6    DS    0H                                                               
         MVC   STPREM,PBDCL                                                     
         B     POST8                                                            
*                                                                               
POST8    DS    0H                                                               
         MVC   HOLDSPC,STDSPACE                                                 
         CLI   SPCOPT,0                                                         
         BNE   *+10                                                             
         XC    STDSPACE,STDSPACE                                                
         CLI   COSTOPT,0                                                        
         BE    POST10                                                           
         MVC   STCOST,GROSS                                                     
         CLI   DOLLS,X'02'         TEST NET $                                   
         BE    POST8B              YES                                          
         CLI   DOLLS,X'05'                                                      
         BNE   POST9               NO                                           
POST8B   DS    0H                                                               
         L     RF,GROSS                                                         
         S     RF,AGYCOM                                                        
         ST    RF,FULL                                                          
         MVC   STCOST,FULL                                                      
POST9    DS    0H                                                               
         CLI   QMEDIA,C'N'                                                      
         BNE   POST10                                                           
         MVC   STCIND,PBDCOSTY                                                  
         MVC   STPRCOS,PREMIUM                                                  
*                                                                               
POST10   DS    0H                                                               
         CLI   JOBOPT,0                                                         
         BE    POST12                                                           
         OC    PBDJOB,PBDJOB                                                    
         BZ    POST12                                                           
         MVC   STPRDJOB(3),PBUYKPRD                                             
         MVC   STPRDJOB+3(6),PBDJOB                                             
*                                                                               
POST12   DS    0H                                                               
         GOTO1 ABINSRCH,BSPARS,(1,X)                                            
*                                                                               
         OC    BSPARS+1(3),BSPARS+1                                             
         BNZ   POST13                                                           
*                                                                               
         MVC   P(27),=C'**SPACE DESC. BUFFER FULL**'                            
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 APRINTIT                                                         
         MVI   MODE,LBUYREQ                                                     
         B     POSTX                                                            
POST13   DS    0H                                                               
         L     R3,BSPARS                                                        
         MVC   FULL,STADDR                                                      
         L     R2,FULL                                                          
         CLI   BSPARS,1            TEST NEW ENTRY                               
         BNE   POST14              NO                                           
         LA    RE,ANXTTOT1                                                      
         CLC   BSPARS+4(4),BSPARS1+4     TAB1                                   
         BE    *+8                      YES                                     
         LA    RE,ANXTTOT2              NO - TAB2                               
         MVC   STADDR,0(RE)                                                     
         L     R2,0(RE)                                                         
         LA    RF,TOTL(R2)                                                      
         ST    RF,0(RE)                                                         
*                                                                               
         MVC   FULL,STADDR                                                      
         L     RE,FULL                                                          
         LA    RF,TOTL                                                          
         XCEF                                                                   
*                                                                               
POST14   DS    0H                                                               
         LA    R4,DTLIST                                                        
         LA    R5,PBUYKDAT         USE INS DATE                                 
         CLI   QBPDATE,C' '                                                     
         BE    POST15                                                           
         LA    R5,PBDBDATE         OR BILLABE DATE                              
         CLI   QBPDATE,C'B'                                                     
         BE    POST15                                                           
         LA    R5,PBDPDATE         OR PAYABLE DATE                              
         CLI   QBPDATE,C'P'                                                     
         BE    POST15                                                           
         LA    R5,PBDSDATE         OR SALE DATE                                 
         CLI   QBPDATE,C'S'                                                     
         BE    POST15                                                           
         LA    R5,PBDCDATE         OR CLOSE DATE                                
         CLI   QBPDATE,C'C'                                                     
         BE    POST15                                                           
POST15   DS    0H                                                               
         CLC   0(3,R5),4(R4)                                                    
         BL    POST16                                                           
         LA    R4,4(R4)                                                         
         B     POST15                                                           
*                                                                               
POST16   DS    0H                                                               
         MVI   BYTE,0                                                           
         CLI   PBDBFD,C'P'                                                      
         BNE   POST17                                                           
         CLI   QBPDATE,C' '                                                     
         BNE   POST17                                                           
         CLI   4(R4),X'FF'                                                      
         BE    POST17                                                           
         MVI   BYTE,X'80'          PRIOR MONTH                                  
         LA    R4,4(R4)                                                         
POST17   DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,2(R5)                                                         
         LA    R0,DTLIST                                                        
         SR    R4,R0                                                            
         ST    R4,FULL             SAVE MONTH DISP                              
         MH    R4,=H'9'           4X9=36                                        
         LA    R4,24(R2,R4)                                                     
*                                                                               
         MVC   SAVDAT,0(R5)                                                     
         CLI   DATSW,C'P'                                                       
         BE    POST17A                                                          
         CLI   DATSW,C'J'                                                       
         BE    POST17B                                                          
         AR    RF,R4                                                            
         IC    RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,0(RF)                                                         
         OC    0(1,RF),BYTE        OR ON PRIOR MONTH IND                        
*                                                                               
         B     POST17C                                                          
*                                  USE PRD CODE NOT DAY                         
POST17A  DS    0H                                                               
         LA    RF,PBUYKPRD                                                      
         CLC   PBUYKPRD,=C'ZZZ'                                                 
         BNE   P17A5                                                            
         LA    R1,PBUYREC+33                                                    
P17A2    DS    0H                                                               
         CLI   0(R1),0                                                          
         BE    POST17C             DONE                                         
         CLI   0(R1),X'21'                                                      
         BE    P17A4                                                            
P17A3    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     P17A2                                                            
*                                                                               
P17A4    DS    0H                                                               
         LA    RF,2(R1)                                                         
P17A5    DS    0H                                                               
         LA    R0,8                                                             
         LR    R7,R4                                                            
POST17A2 DS    0H                                                               
         CLI   0(R7),0                                                          
         BE    POST17A4                                                         
         CLC   0(3,R7),=C'***'                                                  
         BE    POST17A6                                                         
         CLI   DATSW2,C'D'         IF DATES ALSO                                
         BE    *+14                NO SEARCH FOR EQUAL                          
         CLC   0(3,RF),0(R7)                                                    
         BE    POST17A4                                                         
         LA    R7,4(R7)                                                         
         BCT   R0,POST17A2                                                      
*                                                                               
         SH    R7,=H'4'                                                         
         MVC   0(3,R7),=C'***'                                                  
         MVI   3(R7),1                                                          
         B     POST17A6                                                         
*                                                                               
POST17A4 DS    0H                                                               
         MVC   0(3,R7),0(RF)                                                    
         CLI   DATSW2,C'D'         IF DATES ALSO                                
         BNE   POST17A6                                                         
         MVC   3(1,R7),SAVDAT+2    HOLD DAY OF MONTH                            
         B     POST17A8                                                         
*                                                                               
POST17A6 DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,3(R7)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,3(R7)                                                         
*                                                                               
POST17A8 DS    0H                                                               
         CLC   PBUYKPRD,=C'ZZZ'                                                 
         BNE   POST17C                                                          
         B     P17A3               IF ZZZ GET NEXT PRD                          
*                                                                               
*                                  POST JOBS                                    
POST17B  DS    0H                                                               
         LA    R0,4                                                             
         LR    R7,R4                                                            
         MVC   DUB(6),PBDJOB                                                    
         CLI   PBDJOB,C' '                                                      
         BH    *+10                                                             
         MVC   DUB(6),=C'NO JOB'                                                
*                                                                               
POST17B2 DS    0H                                                               
         CLI   0(R7),0                                                          
         BE    POST17B4                                                         
         CLC   0(3,R7),=C'***'                                                  
         BE    POST17B6                                                         
         CLI   DATSW2,C'D'         IF DATES ALSO                                
         BE    *+14                NO SEARCH FOR EQUAL                          
         CLC   DUB(6),0(R7)                                                     
         BE    POST17B4                                                         
         LA    R7,7(R7)                                                         
         BCT   R0,POST17B2                                                      
*                                                                               
         SH    R7,=H'7'                                                         
         MVC   0(3,R7),=C'***'                                                  
         MVC   3(3,R7),SPACES                                                   
         MVI   6(R7),1                                                          
         B     POST17B6                                                         
POST17B4 DS    0H                                                               
         MVC   0(6,R7),DUB                                                      
         CLI   DATSW2,C'D'         IF DATES ALSO                                
         BNE   POST17B6                                                         
         MVC   6(1,R7),SAVDAT+2    HOLD DAY OF MONTH                            
         B     POST17C                                                          
POST17B6 DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,6(R7)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,6(R7)                                                         
*                                                                               
*                                                                               
POST17C  DS    0H                                                               
         SR    RF,RF                                                            
         CLI   PBDSPACE,C'*'       NO INS                                       
         BE    POST17D                                                          
*                                                                               
         LA    RF,1                SET 1 INSERTION                              
         CLI   QMEDIA,C'O'                                                      
         BNE   POST17D                                                          
*                                  OUTDOOR - COUNT NO OF DISPLAYS               
         SR    RF,RF                                                            
         CLI   HOLDSPC,C'*'                                                     
         BE    POST17D                                                          
         CLI   HOLDSPC+17,C'*'                                                  
         BE    POST17D                                                          
         ZAP   DUB,PBDREG                                                       
         AP    DUB,PBDILLUM                                                     
         CVB   RF,DUB                                                           
POST17D  DS    0H                                                               
         LH    RE,32(R4)           'APPARENT' INSERTIONS                        
         AR    RE,RF                                                            
         STH   RE,32(R4)                                                        
         CLI   RDSHRFST,1          TEST PRIMARY R/D                             
         BNE   POST18              NO                                           
         LH    RE,34(R4)           'REAL' INSERTIONS                            
         AR    RE,RF                                                            
         STH   RE,34(R4)                                                        
POST18   DS    0H                                                               
*                                                                               
*                               CALCULATE COST                                  
*                               AT INSERTION LEVEL                              
         XC    MYCOST,MYCOST                                                    
         CLI   DOLLS,X'06'       SEE IF DOING COST                              
         BE    POST19                                                           
         CLI   DOLLS+1,X'06'     SEE IF DOING COST                              
         BNE   POST19X                                                          
POST19   MVC   WORK(4),GROSS                                                    
         MVC   WORK+4(4),CSHDSC                                                 
         MVC   WORK+8(4),AGYCOM                                                 
         ST    RF,SAVERF            SAVE INSERTION COUNTER                      
         CLI   TAXOPT,C'N'                                                      
         BE    POST19C                                                          
         GOTO1 AGETCOST,DMCB,(C'T',BILBASA),WORK,PBUYREC                        
         B     POST19D                                                          
*                                                                               
POST19C  GOTO1 AGETCOST,DMCB,BILBASA,WORK,PBUYREC                               
*                                                                               
POST19D  DS    0H                                                               
         MVC   MYCOST,DMCB+4                                                    
         L     RF,SAVERF            RESTORE RF                                  
POST19X  LM    R4,R7,0(R2)                                                      
         A     R4,GROSS                                                         
         A     R5,CSHDSC                                                        
         A     R6,AGYCOM                                                        
         A     R7,MYCOST            COST                                        
         STM   R4,R7,0(R2)                                                      
         LH    RE,16(R2)           'APPARENT' INSERTIONS                        
         AR    RE,RF                                                            
         STH   RE,16(R2)                                                        
         CLI   RDSHRFST,1                                                       
         BNE   POST20                                                           
         LH    RE,18(R2)           'REAL' INSERTIONS                            
         AR    RE,RF                                                            
         STH   RE,18(R2)                                                        
*                                  POST TO MONTH TOTALS                         
POST20   DS    0H                                                               
         CLI   DOLLOPT,C'S'                                                     
         BE    POST23                                                           
         CLC   BSPARS+4(4),BSPARS1+4    TEST TAB 1                              
         BNE   POST23              NO - NO TOTALS HERE                          
         LA    R3,AVENTOTS                                                      
         LA    R0,9                                                             
         L     RE,FULL             MONTH DIST X 3                               
         SLL   RE,2                X 4 = DISP                                   
POST21   DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    POST22                                                           
         L     RF,0(R3)                                                         
         LA    RF,TOTL-MAXMOS*16(RF,RE)    THIS MONTH                           
         LM    R4,R7,0(RF)                                                      
         A     R4,GROSS                                                         
         A     R5,CSHDSC                                                        
         A     R6,AGYCOM                                                        
         A     R7,MYCOST     COST                                               
         STM   R4,R7,0(RF)                                                      
*                                                                               
POST22   DS    0H                                                               
         LA    R3,4(R3)                                                         
         BCT   R0,POST21                                                        
*                                                                               
POST23   DS    0H                                                               
         CLI   TWOTABS,C'Y'                                                     
         BNE   POST24                                                           
         CLC   BSPARS+4(4),BSPARS2+4                                            
         BE    POST23B                                                          
         MVC   BSPARS1(24),BSPARS                                               
         MVC   BSPARS(24),BSPARS2  PSOT TO 2(D SPC TAB                          
         B     POST12                                                           
POST23B  DS    0H                                                               
         MVC   BSPARS2(24),BSPARS                                               
         B     POST25                                                           
*                                                                               
POST24   DS    0H                                                               
         MVC   BSPARS1(24),BSPARS                                               
*                             POST COMMENTS                                     
POST25   DS    0H                                                               
         CLI   COMFILT,C'Y'                                                     
         BNE   POST30         NOT DOING COMMENTS                                
*                                                                               
         LA    R2,PBUYREC+33                                                    
POST26   DS    0H                                                               
         CLI   0(R2),X'66'                                                      
         BE    POST27                                                           
         CLI   0(R2),0                                                          
         BE    POST30                                                           
POST26B  DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     POST26                                                           
*                                                                               
POST27   DS    0H                                                               
         CLC   2(2,R2),=C'**'                                                   
         BNE   POST26B                                                          
*                                                                               
POST27B  DS    0H                                                               
         L     R3,BSPARS                                                        
         MVC   STCOMX,BSPARS+11    NO. OF ENTRY - USE AS INDEX                  
         L     R4,ANXTCOM                                                       
         MVC   0(1,R4),STCOMX     USE INDX AS ELEM CODE                         
         ZIC   RF,1(R2)                                                         
         LA    RF,4(RF)                                                         
         STC   RF,1(R4)                                                         
         MVC   2(3,R4),PBUYKDAT    INS DATE                                     
         MVC   5(1,R4),PBDFREQ     DATE TYPE                                    
         SH    RF,=H'7'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R4),2(R2)       COMMENT TEXT                                 
         LA    R4,7(RF,R4)                                                      
         ST    R4,ANXTCOM                                                       
         L     RF,ACOMTAB                                                       
         AH    RF,=Y(COMTABL)                                                   
         CR    R4,RF                                                            
         BL    POST26B                                                          
*                                                                               
         MVC   P(22),=C'**COMMENT TABLE FULL**'                                 
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 APRINTIT                                                         
         MVI   MODE,LBUYREQ                                                     
         B     POSTX                                                            
*                                                                               
POST30   DS    0H                                                               
POSTX    DS    0H                                                               
         XIT1                                                                   
         DROP  R3                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                  PRINTING INTERFACE                           
PRTALL   CSECT                                                                  
         NMOD1 0,PRTALL                                                         
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         MVC   HOLDPARS,0(R1)                                                   
         SPACE 3                                                                
         LM    R2,R4,HOLDPARS                                                   
         LTR   R2,R2                                                            
         BZ    PRT10                                                            
         MVC   P,SPACES                                                         
*                                                                               
PRT6     DS    0H                                                               
*                                  COUNT LINES TO PRINT                         
         LA    RF,1                                                             
PRT7     DS    0H                                                               
         MVI   BYTE,0                                                           
         CLC   0(NML,R2),SPACES                                                 
         BE    *+12                                                             
         LA    R2,NML(R2)                                                       
         MVI   BYTE,1                                                           
         CLC   0(DTLINL,R3),SPACES                                              
         BE    *+12                                                             
         LA    R3,DTLINL(R3)                                                    
         MVI   BYTE,1                                                           
         CLC   0(DOLLNL,R4),SPACES                                              
         BE    *+12                                                             
         LA    R4,DOLLNL(R4)                                                    
         MVI   BYTE,1                                                           
*                                                                               
         CLI   BYTE,0                                                           
         BE    PRT8                                                             
         LA    RF,1(RF)                                                         
         B     PRT7                                                             
PRT8     DS    0H                                                               
         SR    RE,RE                                                            
         IC    RE,LNEED                                                         
         AR    RE,RF                                                            
         STC   RE,LNEED                                                         
         SPACE 2                                                                
PRT10    DS    0H                                                               
         CLI   FORCEHED,C'Y'                                                    
         BE    PRTHEAD                                                          
         TM    LNEED,X'80'                                                      
         BZ    PRT10B                                                           
         MVI   MAXLINES,99                                                      
         B     PRT11                                                            
PRT10B   DS    0H                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LNEED                                                         
         IC    RF,LINE                                                          
         AR    RE,RF                                                            
         STC   RE,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BNL   PRTHEAD                                                          
         SPACE 2                                                                
PRT11    DS    0H                                                               
         CLI   BFSW,C'Y'                                                        
         BNE   PRT11B                                                           
         MVI   BFSW,C'N'                                                        
         CLI   BFORMD,C' '                                                      
         BE    PRT11B                                                           
         MVC   P(5),=C'COST='                                                   
         MVC   P+6(51),BFORMD                                                   
         MVI   SPACING,2                                                        
         GOTO1 APRINTIT                                                         
PRT11B   DS    0H                                                               
         LM    R2,R4,HOLDPARS                                                   
         LTR   R2,R2                                                            
         BZ    PRT14                                                            
         GOTO1 APRINTIT                                                         
PRT12    DS    0H                                                               
         MVC   P(NML),0(R2)                                                     
         MVC   P+27(DTLINL),0(R3)                                               
         OC    ADOLL,ADOLL                                                      
         BZ    PRT12B                                                           
         LA    RF,P                                                             
         A     RF,ADOLL                                                         
         MVC   0(DOLLNL,RF),0(R4)                                               
PRT12B   DS    0H                                                               
         CLC   P,SPACES                                                         
         BE    PRT14                                                            
         MVC   SVP,P                                                            
         GOTO1 APRINTIT                                                         
*                                                                               
         CLC   0(NML,R2),SPACES                                                 
         BE    *+8                                                              
         LA    R2,NML(R2)                                                       
         CLC   0(DTLINL,R3),SPACES                                              
         BE    *+8                                                              
         LA    R3,DTLINL(R3)                                                    
         CLC   0(DOLLNL,R4),SPACES                                              
         BE    *+8                                                              
         LA    R4,DOLLNL(R4)                                                    
         B     PRT12                                                            
*                                                                               
PRT14    DS    0H                                                               
         CLI   LASTMODE,LBUYPUB    FOR PUB TOTS SKIP MONTHLY $                  
         BNE   PRT14B                                                           
         CLI   MTHOPT,C'Y'         UNLESS OPT TO PRINT THEM                     
         BNE   PRTX                                                             
PRT14B   DS    0H                                                               
*                                                                               
         L     RF,NMOS                                                          
         MH    RF,=H'5'                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SVP+28(0),SPACES                                                 
*                                                                               
         BE    PRT14C              IF LAST LINE HAD DATA                        
         GOTO1 APRINTIT            IN DATE AREA - SKIP A LINE                   
PRT14C   DS    0H                                                               
         CLI   MTOTLNS,C' '                                                     
         BH    PRT15                                                            
         CLC   MTOTLNS+1(L'MTOTLNS-1),MTOTLNS                                   
         BE    PRTX                                                             
PRT15    DS    0H                                                               
         LA    RF,P+NML-2                                                       
         MVC   000(MTOTLNL,RF),MTOTLN1                                          
         MVC   132(MTOTLNL,RF),MTOTLN2                                          
         GOTO1 APRINTIT                                                         
         MVI   MTOTLNS,C' '                                                     
         MVC   MTOTLNS+1(L'MTOTLNS-1),MTOTLNS                                   
         B     PRTX                                                             
*                                                                               
PRT16    DS    0H                                                               
         GOTO1 APRINTIT                                                         
*                                                                               
*                                                                               
PRTX     DS    0H                                                               
         MVI   LNEED,0                                                          
         MVC   MAXLINES,SAVMAX                                                  
         XIT1                                                                   
         SPACE 2                                                                
PRTHEAD  DS    0H                                                               
         MVC   HEAD1+56(19),RPTNAME                                             
         MVC   HEAD2+56(19),RPTUND                                              
*                                                                               
         LA    R2,HEAD9                                                         
         MVC   000(132,R2),MYHA                                                 
         MVC   132(132,R2),MYHB                                                 
         MVC   264(132,R2),MYHC                                                 
*                                                                               
         CLI   QBPDATE,C' '                                                     
         BE    PH6                                                              
         LA    RF,=CL20'** BILLING PERIOD **'                                   
         CLI   QBPDATE,C'B'                                                     
         BE    PH4                                                              
         LA    RF,=CL20'** PAYABLE PERIOD **'                                   
         CLI   QBPDATE,C'P'                                                     
         BE    PH4                                                              
         LA    RF,=CL20'** ON-SALE DATES **'                                    
         CLI   QBPDATE,C'S'                                                     
         BE    PH4                                                              
         LA    RF,=CL20'** CLOSING DATES **'                                    
         CLI   QBPDATE,C'C'                                                     
         BE    PH4                                                              
PH4      DS    0H                                                               
         MVC   HEAD4+55(20),0(RF)                                               
PH6      DS    0H                                                               
         TM    ESTSW,X'08'                                                      
         BNZ   *+10                                                             
         XC    PESTKEY,PESTKEY     NO EST IN HEADLINES                          
         TM    PRDSW,X'08'                                                      
         BNZ   *+10                                                             
         XC    PPRDKEY(4),PPRDKEY  NO PRD IN HEADLINES                          
         TM    DIVSW,X'08'                                                      
         BNZ   *+10                                                             
         XC    PDIVKEY,PDIVKEY     NO DIV IN HEADLINES                          
         TM    CLTSW,X'08'                                                      
         BNZ   *+10                                                             
         XC    PCLTKEY,PCLTKEY                                                  
*                                                                               
         CLI   MODE,LBUYEST                                                     
         BNL   PH7                                                              
         CLI   MODE,LBUYREQ                                                     
         BL    PH7                                                              
         XC    PESTKEY,PESTKEY     NO EST IN HEADLINE                           
         CLI   MODE,LBUYPRO                                                     
         BNL   *+10                                                             
         XC    PPRDKEY(4),PPRDKEY  NO PRD IN HEADLINES                          
         CLI   MODE,LBUYDIV                                                     
         BNL   *+10                                                             
         XC    PDIVKEY,PDIVKEY     NO DIV IN HEADLINES                          
*                                                                               
PH7      DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         B     PRT11                                                            
         SPACE 3                                                                
         LTORG                                                                  
PRINTIT  CSECT                                                                  
         NMOD1 0,PRINTIT                                                        
         L     RC,PPFILEC                                                       
         CLI   QOPT7,C'Y'                                                       
         BNE   PRINTITX                                                         
         MVI   HEAD4+115,C'T'                                                   
         MVC   HEAD6+97(34),=C'*INCLUDES ANY PROPOSED INSERTIONS*'              
         CLI   QMEDIA,C'O'                                                      
         BNE   PRINTITX                                                         
         MVC   HEAD6+120(11),=C'POSTINGS*  '                                    
*                                                                               
PRINTITX GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PP60WRKD DSECT                                                                  
PP60WRK  DS    0C                                                               
*                                                                               
ADDRS    DS    0F                                                               
ADTL     DS    A                                                                
ASETHEAD DS    A                                                                
ADATOUT  DS    A                                                                
APOST    DS    A                                                                
ABINSRCH DS    A                                                                
ASETSPC  DS    A                                                                
ADOLFMT  DS    A                                                                
AGETSPC  DS    A                                                                
ANPROC   DS    A                                                                
AROLLUP  DS    A                                                                
APRTALL  DS    A                                                                
ARDSHR   DS    A                                                                
ACHOPPER DS    A                                                                
AGETCOST DS    A                                                                
APRINTIT DS    A                                                                
AJOBLST  DS    A                                                                
AJOBLSTX DS    A                                                                
*                                                                               
ADTLINS  DS    A                                                                
ACOMTAB  DS    A                                                                
ASPCTAB1 DS    A                                                                
ASPCTOT1 DS    A                                                                
ASPCTAB2 DS    A                                                                
ASPCTOT2 DS    A                                                                
*                                                                               
APUBTOTS DS    A                                                                
AVENTOTS DS    A                                                                
AMKTTOTS DS    A                                                                
ADSTTOTS DS    A                                                                
AREGTOTS DS    A                                                                
AESTTOTS DS    A                                                                
APRDTOTS DS    A                                                                
ADIVTOTS DS    A                                                                
ACLTTOTS DS    A                                                                
AREQTOTS DS    A                                                                
ADDRSX   EQU   *                                                                
*                                                                               
B2PROF   DS    XL16          B2 BILLING PROFILE                                 
*                            NEEDED FOR GETCOST                                 
*                                                                               
PROF     DS    XL32                                                             
         DS    0F                                                               
HOLDPARS DS    CL24                                                             
LASTMODE DS    X                                                                
LEVSW    DS    X                                                                
TABSW    DS    X                                                                
SPCOPT   DS    X                                                                
COSTOPT  DS    X                                                                
JOBOPT   DS    X                                                                
PAGOPT   DS    X                                                                
WEEKSW   DS    X                                                                
DATSW    DS    X                                                                
DATSW2   DS    X                                                                
ESTSW    DS    X                                                                
PRDSW    DS    X                                                                
DIVSW    DS    X                                                                
CLTSW    DS    X                                                                
DOLLOPT  DS    X                                                                
PUBOPT   DS    X                                                                
CIRCOPT  DS    X                                                                
MTHOPT   DS    X                                                                
DOLLS    DS    XL3                                                              
COMFILT  DS    CL2                                                              
TWOTABS  DS    CL1                                                              
*                                                                               
*                                                                               
TAXOPT   DS    CL1            FROM B2 PROFILE                                   
*                                                                               
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
SAVDAT   DS    XL3                                                              
DTLIST   DS    CL68                MAXMOS X 4                                   
MAXMOS   EQU   13                                                               
NMOS     DS    F                                                                
MYCOST   DS    F              CALCULATED BY GETCOST FOR EACH BUY                
SAVERF   DS    F              SAVE REGISTER F                                   
*                                                                               
ATOTS    DS    A                                                                
ADOLL    DS    A                                                                
ANXTTOT1 DS    A                                                                
ANXTTOT2 DS    F                                                                
ANXTCOM  DS    A                                                                
COMINDX  DS    X                                                                
ATOTWDS  DS    A                                                                
ADESC    DS    A                                                                
ALASTLIN DS    A                                                                
BSPARS   DS    6F                                                               
BSPARS1  DS    6F                                                               
BSPARS2  DS    6F                                                               
EDITCNT  DS    H                                                                
PBRCNT   DS    H                                                                
EDVENSW  DS    X                                                                
RISW     DS    X                                                                
ELCODE   DS    X                                                                
RDSHRFST DS    X                                                                
TOTSW    DS    X                                                                
BFSW     DS    C                                                                
NEWPAGE  DS    X                                                                
LNEED    DS    X                                                                
SAVMAX   DS    XL1                                                              
SVP      DS    CL132                                                            
HOLDSPC  DS    CL34                                                             
DASHES   DS    CL40'-'                                                          
X        DS    CL132                                                            
X2       DS    CL132                                                            
W        DS    CL200                                                            
RPTNAME  DS    CL19                                                             
RPTUND   DS    CL19                                                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
SPCN     DS    0CL405                                                           
         DS    15CL27                                                           
*                                                                               
PUBN     DS    0CL270                                                           
         DS    10CL27                                                           
*                                                                               
MKTN     DS    0CL108                                                           
MKTN1    DS    CL27                                                             
MKTN2    DS    CL27                                                             
         DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
DSTN     DS    0CL108                                                           
DSTN1    DS    CL27                                                             
DSTN2    DS    CL27                                                             
DSTN3    DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
REGN     DS    0CL108                                                           
REGN1    DS    CL27                                                             
REGN2    DS    CL27                                                             
REGN3    DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
ESTN     DS    0CL108                                                           
ESTN1    DS    CL27                                                             
ESTN2    DS    CL27                                                             
         DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
PRDN     DS    0CL108                                                           
PRDN1    DS    CL27                                                             
PRDN2    DS    CL27                                                             
         DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
DIVN     DS    0CL108                                                           
DIVN1    DS    CL27                                                             
DIVN2    DS    CL27                                                             
         DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
CLTN     DS    0CL108                                                           
CLTN1    DS    CL27                                                             
CLTN2    DS    CL27                                                             
         DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
RPTN     DS    0CL108                                                           
RPTN1    DS    CL27                                                             
RPTN2    DS    CL27                                                             
         DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
*                                                                               
*                                                                               
NML      EQU   27                                                               
*                                                                               
*                                                                               
MYHA     DS    CL132                                                            
MYHB     DS    CL132                                                            
MYHC     DS    CL132                                                            
*                                                                               
*                                                                               
DOLLNS   DS    0CL117                                                           
DOLLN1   DS    CL39                                                             
DOLLN2   DS    CL39                                                             
         DS    CL39                                                             
DOLLNL   EQU   39                                                               
*                                                                               
DTNEED   DS    F                                                                
DTLINL   EQU   85                                                               
DTLINN   EQU   75                                                               
*                                                                               
MTOTLNS  DS    0CL140                                                           
MTOTLN1  DS    CL70                                                             
MTOTLN2  DS    CL70                                                             
MTOTLNL  EQU   70                                                               
*                                                                               
*                                                                               
       ++INCLUDE PBILPROF                                                       
*                                                                               
BFORMD   DS    CL53                                                             
PPBYOWRK DS    600C                                                             
PP60WRKL EQU   *-PP60WRK                                                        
         SPACE 2                                                                
DOLLD    DSECT                                                                  
*                                                                               
TGRS     DS    F                                                                
TCD      DS    F                                                                
TAC      DS    F                                                                
TCOST    DS    F                                                                
         DS    F                                                                
         DS    F                                                                
         SPACE 2                                                                
STSD     DSECT                     DSECT TO COVER SPACE TAB                     
STDSPACE DS    0CL34                                                            
STUNITS  DS    XL4                                                              
STUNIND  DS    XL1                                                              
STCIND   DS    XL1                                                              
STUCOST  DS    XL4                                                              
STPREM   DS    XL1                                                              
STPRCOS  DS    XL4                                                              
         DS    CL2                                                              
STDSPC2  DS    CL17                EXTRA SPACE FOR OUTDOOR                      
STCOST   DS    XL4                                                              
STPRDJOB DS    CL9                 PRD/JOB NO.                                  
STKL     EQU   *-STSD                                                           
*                                                                               
STCOMX   DS    X                   COMMENT INDEX                                
STADDR   DS    AL4                 A(TOTS)                                      
*                                                                               
STL      EQU   *-STSD                                                           
TOTL     EQU   24+(MAXMOS*((32+4)+16))                                          
*                                  24 GRAND TOTALS PLUS                         
*                                  FOR EACH MONTH-                              
*                                  32 FOR DATE OR PRD LIST                      
*                                  4 FOR INS TOTAL                              
*                                  16 FOR $ TOTALS                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
*                                                                               
         PRINT ON                                                               
*                                                                               
*                                                                               
PP60ACCS CSECT                                                                  
NSPC     EQU   100                                                              
SPCTAB   DS    0F                                                               
         ORG   *+STL*NSPC                                                       
SPCTABL  EQU   *-SPCTAB                                                         
SPCTAB2  DS    0F                                                               
         ORG   *+STL*NSPC                                                       
COMTAB   DS    0F                                                               
         ORG   *+5000                                                           
COMTABL  EQU   *-COMTAB                                                         
SPCTOTS  DS    0F                                                               
         ORG   *+TOTL*NSPC                                                      
SPCTOTL  EQU   *-SPCTOTS                                                        
SPCTOTS2 DS    0F                                                               
         ORG   *+TOTL*NSPC                                                      
PUBTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
VENTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
MKTTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
DSTTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
REGTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
ESTTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
PRDTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
DIVTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
CLTTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
REQTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
ACCSL    EQU   *-PP60ACCS                                                       
*                                                                               
DTLINS   DS    75CL85                                                           
*                                                                               
JOBLST   CSECT                                                                  
         DS    1020X               10 X 102 BYTES OF JOB RECORD                 
JOBLSTX  DS    X                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'071PPREP6002 11/20/15'                                      
         END                                                                    
