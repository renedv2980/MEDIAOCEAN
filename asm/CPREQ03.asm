*          DATA SET CPREQ03    AT LEVEL 035 AS OF 05/01/02                      
*PHASE TC0403A                                                                  
*INCLUDE CPDEMTAB                                                               
         TITLE 'CPREQ03 - REQUEST - VALIDATE DATA FIELDS - PART 1'              
         PRINT NOGEN                                                            
TC0403   CSECT                                                                  
         NMOD1 040,TC0403,RR=R9                                                 
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     R9,0(R1)                                                         
         USING REQTEMP,R9          R9=A(W/S)                                    
         L     R3,ASAVE                                                         
         USING REQSAVE,R3          R3=A(TWA)                                    
         SPACE 2                                                                
         L     R1,FLDHADR          R1=A(FLD HDR TO BE VALIDATED)                
         SR    RF,RF                                                            
         IC    RF,ROUTNUM          RF=ROUTINE NUM REQUIRED                      
         SLL   RF,2                                                             
         L     RF,ROUTADRT(RF)                                                  
         A     RF,RELO             RF=A(VALIDATION ROUTINE)                     
         BASR  RE,RF                                                            
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
RNGEVAL  NTR1                      RANGE - 04=AGY 08=BNK 10=CLI 20=OFF          
         GOTO1 AINITV                    - 40=GRP                               
         CLI   FIND,1                                                           
         BL    RNGEVO              RANGE NOT INPUT                              
         GOTO1 SCANNER,DMCB,(R4),(1,(RC))                                       
         CLI   4(R1),1                                                          
         BNE   RNGEVE                                                           
         CLI   0(RC),6             CHECK FIRST LENGTH                           
         BH    RNGEVE                                                           
         SR    R1,R1                                                            
         IC    R1,0(RC)                                                         
         BCTR  R1,0                                                             
         LA    R8,RNGEVTBL                                                      
*                                                                               
RNGEV1   CLI   0(R8),0             SEARCH RANGE TABLE                           
         BE    RNGEVE                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R8),12(RC)                                                   
         BE    RNGEV2                                                           
         LA    R8,L'RNGEVTBL(R8)                                                
         B     RNGEV1                                                           
*                                                                               
RNGEV2   CLI   1(RC),0             CHECK SECOND LENGTH                          
         BNE   RNGEV3                                                           
         CLI   6(R8),0                                                          
         BNE   RNGEVE              MISSING SECOND HALF                          
         B     RNGEV4                                                           
RNGEV3   CLC   1(1,RC),6(R8)                                                    
         BH    RNGEVE                                                           
         TM    7(R8),X'01'         TEST SPECIAL CLIENT                          
         BZ    RNGEV4              NO                                           
         CLI   1(RC),3             YES CAN HAVE A9999 AS CLIENT CODE            
         BNH   RNGEV4                                                           
         CLC   22(3,RC),=C'ALL'                                                 
         BE    RNGEV4                                                           
         CLI   1(RC),5                                                          
         BNE   RNGEVE                                                           
         MVC   DUB(4),=C'0000'                                                  
         MVZ   DUB(4),23(RC)                                                    
         CLC   DUB(4),=C'0000'                                                  
         BNE   RNGEVE                                                           
         CLI   22(RC),C'A'                                                      
         BL    RNGEVE                                                           
         CLI   22(RC),C'Z'                                                      
         BH    RNGEVE                                                           
*                                                                               
RNGEV4   MVC   RRANGE,12(RC)       SET RANGE VALUE                              
         OC    FIND,7(R8)          SET RANGE FORMAT                             
         CLI   1(RC),0                                                          
         BE    RNGEVX                                                           
         CLI   RRANGE,C'C'         SET CLIENT VALUE                             
         BNE   RNGEV5                                                           
         MVC   RCLI(5),22(RC)                                                   
         CLC   RCLI(3),=C'ALL'                                                  
         BE    RNGEV4B                                                          
         CLC   RNUM(2),=C'26'      CLT MUST BE ALL FOR 26                       
         BNE   RNGEVX                                                           
         B     RNGEVE                                                           
*                                                                               
RNGEV4B  CLC   RNUM(2),=C'26'                                                   
         BE    RNGEVX              ALL OK FOR 26                                
         CLC   RNUM(2),=C'40'      OR 40                                        
         BE    RNGEVX                                                           
         B     RNGEVE                                                           
*                                                                               
RNGEV5   CLI   RRANGE,C'O'         SET OFFICE VALUE                             
         BNE   RNGEVX                                                           
         MVC   ROFF,22(RC)                                                      
         B     RNGEVX                                                           
*                                                                               
RNGEVO   CLC   RNUM(2),=C'26'                                                   
         BE    RNGEV26                                                          
         MVI   RRANGE,C'A'         DEFAULT TO AGENCY                            
*                                  EXCEPT FOR REQ 26                            
         B     RNGEVX                                                           
*                                                                               
RNGEV26  MVI   RRANGE,C'C'         FOR 26 SET TO ALL CLTS                       
         MVC   RCLI(3),=C'ALL'                                                  
         B     RNGEVX                                                           
*                                                                               
RNGEVE   MVI   FERN,2                                                           
         NI    FIND,1                                                           
RNGEVX   XIT1                                                                   
*                                                                               
RNGEVTBL DS    0CL8                                                             
         DC    C'AGENCY',X'00',X'04'                                            
         DC    C'BANK  ',X'00',X'08'                                            
*&&UK*&& DC    C'CLIENT',X'05',X'11'                                            
*&&US*&& DC    C'CLIENT',X'05',X'10'                                            
         DC    C'GROUP ',X'00',X'40'                                            
         DC    C'OFFICE',X'01',X'20'                                            
RNGEVTBX DC    X'00'                                                            
         DS    0H                                                               
         EJECT                                                                  
MKTSVAL  NTR1                      MKT SEQ - 04=ALPHA 08=RANK 10=CODE           
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    MKTSVO              MKTSEQ NOT INPUT                             
         CLI   IFLDH+5,5                                                        
         BH    MKTSVE                                                           
         BCTR  R5,0                                                             
         LA    R8,MKTSVTBL                                                      
*                                                                               
MKTSV1   CLI   0(R8),0             SEARCH MKTSEQ TABLE                          
         BE    MKTSVE                                                           
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   IFLD(0),0(R8)                                                    
         BE    MKTSV2                                                           
         LA    R8,L'MKTSVTBL(R8)                                                
         B     MKTSV1                                                           
*                                                                               
MKTSV2   MVC   RMKTSEQ,0(R8)       SET MKTSEQ VALUE                             
         OC    FIND,5(R8)          SET MKTSEQ FORMAT                            
         B     MKTSVX                                                           
*                                                                               
MKTSVO   MVI   RMKTSEQ,C'A'                                                     
         B     *+12                                                             
MKTSVE   MVI   FERN,2                                                           
         NI    FIND,1                                                           
MKTSVX   XIT1                                                                   
*                                                                               
MKTSVTBL DS    0CL6                                                             
         DC    C'ALPHA',X'04'                                                   
         DC    C'RANK ',X'08'                                                   
         DC    C'CODE ',X'10'                                                   
MKTSVTBX DC    X'00'                                                            
         DS    0H                                                               
         EJECT                                                                  
MKTVVAL  NTR1                      MKT VAL - 02=ALL 04=NNNN 08=MGNN             
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    MKTVVO              MKT  NOT INPUT                               
         BE    MKTVV1                                                           
         MVC   RMKT,IFLD           MKT =ALL                                     
         B     MKTVVX                                                           
*                                                                               
MKTVV1   GOTO1 SCANNER,DMCB,(R4),(1,(RC))                                       
         CLI   4(R1),1                                                          
         BNE   MKTVVE                                                           
         CLI   0(RC),5                                                          
         BH    MKTVVE                                                           
         CLC   12(3,RC),=C'MGR'                                                 
         BE    MKTVV4                                                           
         SR    R1,R1                                                            
         IC    R1,0(RC)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,RC),=C'GROUP'                                               
         BNE   MKTVV3                                                           
*                                                                               
MKTVV2   CLI   1(RC),0             CHECK MKT GROUP                              
         BE    MKTVVE                                                           
         CLI   1(RC),2             SECOND PART MUST BE N OR NN                  
         BH    MKTVVE                                                           
         TM    3(RC),X'80'                                                      
         BZ    MKTVVE                                                           
         MVC   RMKT(2),=C'MG'      SET MKT GROUP ID                             
         OI    FIND,X'08'                                                       
         MVC   RMKT+2(2),22(RC)    SET MKT GROUP VALUE                          
         CLI   1(RC),2                                                          
         BE    MKTVVX                                                           
         MVI   RMKT+2,C'0'                                                      
         MVC   RMKT+3(1),22(RC)                                                 
         B     MKTVVX                                                           
*                                                                               
MKTVV3   CLI   0(RC),4             CHECK MKT VALUE                              
         BH    MKTVVE                                                           
         CLI   1(RC),0             FIRST PART MUST BE N THRU NNNN               
         BNE   MKTVVE                                                           
         TM    2(RC),X'80'                                                      
         BZ    MKTVVE                                                           
         MVC   RMKT(4),=C'0000'    SET MKT VALUE                                
         OI    FIND,X'04'                                                       
         SR    R1,R1                                                            
         IC    R1,0(RC)                                                         
         LA    R8,4                                                             
         SR    R8,R1                                                            
         LA    R8,RMKT(R8)                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),12(RC)                                                   
         B     MKTVVX                                                           
*                                                                               
MKTVV4   CLI   1(RC),5             VALID GROUP                                  
         BNE   MKTVVE                                                           
         OI    FIND,X'08'                                                       
         MVC   RMGRALPH,22(RC)                                                  
         MVC   RMKT,23(RC)                                                      
         MVI   ROPT6,C'S'                                                       
         B     MKTVVX                                                           
*                                                                               
MKTVVO   MVC   RMKT(3),=C'ALL'                                                  
         B     *+12                                                             
MKTVVE   MVI   FERN,2                                                           
         NI    FIND,1                                                           
MKTVVX   XIT1                                                                   
         EJECT                                                                  
TRGTVAL  NTR1                      TARGET - 04=NNN                              
         LA    RA,RTRGT                                                         
         B     DEMOV                                                            
SLCTVAL  NTR1                      SELECT - 04=NNN                              
         LA    RA,RSLCT                                                         
         B     DEMOV                                                            
*                                                                               
DEMOV    GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    DEMOVO              DEMO NOT INPUT                               
         LA    RE,ADBLOCK                                                       
         USING DBLOCK,RE                                                        
         XC    ADBLOCK,ADBLOCK                                                  
         L     R1,APARM                                                         
         MVC   DBCOMFCS,16(R1)     PASS REAL COMFACS ADDRESS STUPID !           
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'TP '                                                   
         GOTO1 DEMOVAL,DMCB,(0,(R4)),(1,FULL),(C'S',ADBLOCK),0                  
         CLI   FULL+2,0                                                         
         BE    DEMOVE                                                           
         ZIC   R1,FULL+2                                                        
         CVD   R1,DUB                                                           
         UNPK  0(3,RA),DUB+6(2)    SET TARGET/SELECT DEMO VALUE                 
         OI    2(RA),X'F0'                                                      
         OI    FIND,X'04'                                                       
         B     DEMOVX                                                           
*                                                                               
DEMOVO   EQU   *                                                                
         B     *+12                                                             
DEMOVE   MVI   FERN,2                                                           
         NI    FIND,1                                                           
DEMOVX   XIT1                                                                   
         EJECT                                                                  
LISTVAL  NTR1                      DATA TYPE LIST - 04=N,N,N,......             
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    LISTVO              LIST NOT INPUT                               
         GOTO1 SCANNER,DMCB,(R4),(9,(RC))                                       
         SR    R0,R0                                                            
         IC    R0,4(R1)                                                         
         LTR   R0,R0               R0=NUM OF LIST ITEMS                         
         BZ    LISTVE                                                           
         LA    R5,RLIST            R5=A(LIST ITEM IN REQ REC)                   
         LR    R8,RC               R8=A(SCAN TBL ENTRY)                         
         OI    FIND,X'04'                                                       
*                                                                               
LISTV1   CLI   0(R8),1             EACH ITEM MUST BE SINGLE INTEGER             
         BNE   LISTVE                                                           
         CLI   1(R8),0                                                          
         BNE   LISTVE                                                           
         TM    2(R8),X'80'                                                      
         BZ    LISTVE                                                           
         CLI   12(R8),C'0'                                                      
         BE    LISTVE                                                           
         MVC   0(1,R5),12(R8)                                                   
LISTV2   LA    R5,1(R5)                                                         
         LA    R8,32(R8)                                                        
         BCT   R0,LISTV1                                                        
         B     LISTVX                                                           
*                                                                               
LISTVO   MVC   RLIST(2),=C'12'                                                  
         CLC   RNUM(2),=C'4T'                                                   
         BNE   *+10                                                             
         MVC   RLIST(4),=C'1234'                                                
         B     *+12                                                             
LISTVE   MVI   FERN,2                                                           
         NI    FIND,1                                                           
LISTVX   XIT1                                                                   
         EJECT                                                                  
STRDVAL  NTR1                      START DATE - 04=YYMMDD 08=YYMM               
         LA    R7,RSTRD                                                         
         B     STRDV0                                                           
DATDVAL  NTR1                                                                   
         LA    R7,RDATD                                                         
         B     STRDV0                                                           
STRDV0   GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    STRDVO              START DATE MISSING                           
         BH    STRDVE                                                           
         CLI   IFLDH+5,9                                                        
         BH    STRDVE                                                           
         GOTO1 DATVAL,PLIST,(0,IFLD),DUB                                        
         OC    PLIST(4),PLIST                                                   
         BE    STRDV1                                                           
         OI    FIND,X'04'          START DATE = YYMMDD                          
         B     STRDV2                                                           
STRDV1   GOTO1 (RF),(R1),(2,IFLD),DUB                                           
         OC    PLIST(4),PLIST                                                   
         BE    STRDVE                                                           
         MVC   DUB+4(2),=C'  '                                                  
         OI    FIND,X'08'          START DATE = YYMM                            
STRDV2   MVC   0(4,R7),DUB                                                      
         B     STRDVO                                                           
STRDVE   MVI   FERN,2              START DATE INVALID                           
STRDVO   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
ENDDVAL  NTR1                      END DATE - 04=YYMMDD 08=YYMM                 
         GOTO1 AINITV                                                           
         MVC   FIND(1),IFLDH                                                    
         CLI   FIND,1                                                           
         BL    ENDDVO              END DATE MISSING                             
         BH    ENDDVE                                                           
         CLI   IFLDH+5,9                                                        
         BH    ENDDVE                                                           
         GOTO1 DATVAL,PLIST,(0,IFLD),DUB                                        
         OC    PLIST(4),PLIST                                                   
         BE    ENDDV1                                                           
         OI    FIND,X'04'          END DATE = YYMMDD                            
         B     ENDDV2                                                           
ENDDV1   GOTO1 (RF),(R1),(2,IFLD),DUB                                           
         OC    PLIST(4),PLIST                                                   
         BE    ENDDVE                                                           
         MVC   DUB+4(2),=C'  '                                                  
         OI    FIND,X'08'          END DATE = YYMM                              
ENDDV2   MVC   RENDD,DUB                                                        
         CLI   RSTRD,C' '                                                       
         BE    ENDDVO                                                           
         CLC   RSTRD,RENDD         CHECK START LE END                           
         BNH   ENDDVO                                                           
ENDDVE   MVI   FERN,2              END DATE INVALID                             
         NI    FIND,1              SET OFF VALIDATION BITS                      
ENDDVO   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
ZONEVAL  NTR1                      TIME ZONE - 04=N                             
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    ZONEVO              ZONE NOT INPUT                               
         CLI   IFLDH+5,1                                                        
         BNE   ZONEVE                                                           
         CLI   IFLD,C'5'                                                        
         BL    ZONEVE                                                           
         CLI   IFLD,C'8'                                                        
         BH    ZONEVE                                                           
         MVC   RZONE,IFLD                                                       
         OI    FIND,X'04'          ZONE = INTEGER 5 THRU 8                      
         B     ZONEVX                                                           
*                                                                               
ZONEVO   EQU   *                                                                
         B     *+8                                                              
ZONEVE   MVI   FERN,2                                                           
ZONEVX   XIT1                                                                   
         EJECT                                                                  
SPTLVAL  NTR1                      SPOT LENGTH - 04=NNN                         
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    SPTLVO              SPTLEN NOT INPUT                             
         GOTO1 ARJN                                                             
         CLI   FERN,255                                                         
         BNE   SPTLVE                                                           
         MVC   RSPTLEN,TEMP+1                                                   
         OI    FIND,X'04'          SPTLEN = N THRU NNN  (1-255)                 
         B     SPTLVX                                                           
*                                                                               
SPTLVO   EQU   *                                                                
SPTLVE   EQU   *                                                                
SPTLVX   XIT1                                                                   
         EJECT                                                                  
DAYPVAL  NTR1                      DAY PART - 04=XXX                            
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    DAYPVO              DAYPART NOT INPUT                            
         CLI   IFLDH+5,3                                                        
         BL    DAYPVE                                                           
         CLI   IFLDH+5,4                                                        
         BH    DAYPVE                                                           
         LA    R8,DAYPVTBL                                                      
*                                                                               
DAYPV1   CLI   0(R8),0             SEARCH DAYPART TABLE                         
         BE    DAYPVE                                                           
         CLC   IFLD(3),0(R8)                                                    
         BE    DAYPV2                                                           
         LA    R8,L'DAYPVTBL(R8)                                                
         B     DAYPV1                                                           
*                                                                               
DAYPV2   MVC   RDAYPT,4(R8)                                                     
         OI    FIND,X'04'          DAYPART = XXX IN TABLE                       
         B     DAYPVX                                                           
*                                                                               
DAYPVO   EQU   *                                                                
         B     *+8                                                              
DAYPVE   MVI   FERN,2                                                           
DAYPVX   XIT1                                                                   
*                                                                               
DAYPVTBL DS    0CL6                                                             
         DC    C'EAM ',C'A '                                                    
         DC    C'DAY ',C'C '                                                    
         DC    C'WEM ',C'E '                                                    
         DC    C'WEA ',C'G '                                                    
         DC    C'ELY ',C'J '                                                    
         DC    C'PAC ',C'L '                                                    
         DC    C'PRI ',C'N '                                                    
         DC    C'LTE ',C'P '                                                    
         DC    C'LLT ',C'R '                                                    
DAYPVTBX DC    X'00'                                                            
         DS    0H                                                               
         EJECT                                                                  
AFFLVAL  NTR1                      AFFILIATION - 04=X                           
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    AFFLVO              AFFL NOT INPUT                               
         CLI   IFLDH+5,1                                                        
         BNE   AFFLVE                                                           
         CLI   IFLD,C'I'                                                        
         BE    AFFLV1                                                           
         CLI   IFLD,C'N'                                                        
         BE    AFFLV1                                                           
         B     AFFLVE                                                           
*                                                                               
AFFLV1   MVC   RAFF,IFLD                                                        
         OI    FIND,X'04'          AFFL = I OR N                                
         B     AFFLVX                                                           
*                                                                               
AFFLVO   EQU   *                                                                
         B     *+8                                                              
AFFLVE   MVI   FERN,2                                                           
AFFLVX   XIT1                                                                   
         EJECT                                                                  
RTGFVAL  NTR1                      RATING FILTER - 04=X                         
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    RTGFVO              RTGF NOT INPUT                               
         CLI   IFLDH+5,1                                                        
         BNE   RTGFVE                                                           
         CLI   IFLD,C'A'                                                        
         BE    RTGFV1                                                           
         CLI   IFLD,C'N'                                                        
         BE    RTGFV1                                                           
         B     RTGFVE                                                           
*                                                                               
RTGFV1   MVC   RRTG,IFLD                                                        
         OI    FIND,X'04'          RTGF = A OR N                                
         B     RTGFVX                                                           
*                                                                               
RTGFVO   EQU   *                                                                
         B     *+8                                                              
RTGFVE   MVI   FERN,2                                                           
RTGFVX   XIT1                                                                   
         EJECT                                                                  
PTYPEVAL NTR1                      PROGRAM TYPE FILTER - 04=ALPHA               
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    RPTVO                                                            
         CLI   IFLDH+5,1                                                        
         BNE   RPTVE                                                            
         CLI   IFLD,C'R'           CHANGE R TO O                                
         BNE   *+8                                                              
         MVI   IFLD,C'O'                                                        
         CLI   IFLD,C'N'                                                        
         BE    *+8                                                              
         CLI   IFLD,C'S'                                                        
         BE    *+8                                                              
         CLI   IFLD,C'K'                                                        
         BE    *+8                                                              
         CLI   IFLD,C'F'                                                        
         BE    *+8                                                              
         CLI   IFLD,C'O'                                                        
         BE    *+8                                                              
         B     RPTVE                                                            
         MVC   RPROGT,IFLD                                                      
         OI    FIND,X'04'                                                       
         B     RPTVX                                                            
*                                                                               
RPTVO    EQU   *                                                                
         B     *+8                                                              
*                                                                               
RPTVE    MVI   FERN,2                                                           
*                                                                               
RPTVX    XIT1                                                                   
         EJECT                                                                  
PROJVAL  NTR1                      PROJECTION FORMULA - 04=X                    
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    PROJVO              PROJ NOT INPUT                               
         CLI   IFLDH+5,1                                                        
         BNE   PROJVE                                                           
*                                                                               
PROJV1   MVC   RFORM,IFLD                                                       
         OI    FIND,X'04'          PROJ = X                                     
         B     PROJVX                                                           
*                                                                               
PROJVO   EQU   *                                                                
         B     *+8                                                              
PROJVE   MVI   FERN,2                                                           
PROJVX   XIT1                                                                   
         EJECT                                                                  
OPT1VAL  NTR1                      OPTION#N - 04=X                              
         LA    R7,OPT1VTBL                                                      
         LA    RA,ROPT1                                                         
         B     OPTNVAL                                                          
OPT2VAL  NTR1                                                                   
         LA    R7,OPT2VTBL                                                      
         LA    RA,ROPT2                                                         
         B     OPTNVAL                                                          
OPT3VAL  NTR1                                                                   
         LA    R7,OPT3VTBL                                                      
         LA    RA,ROPT3                                                         
         B     OPTNVAL                                                          
OPT4VAL  NTR1                                                                   
         LA    R7,OPT4VTBL                                                      
         LA    RA,ROPT4                                                         
         B     OPTNVAL                                                          
*                                                                               
OPT5VAL  NTR1                                                                   
         LA    R7,OPT5VTBL                                                      
         LA    RA,ROPT5                                                         
         B     OPTNVAL                                                          
*                                                                               
OPT6VAL  NTR1                                                                   
         LA    R7,OPT6VTBL                                                      
         LA    RA,ROPT6                                                         
         B     OPTNVAL                                                          
*                                                                               
OPTNVAL  GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    OPTNVO              OPTION MISSING                               
         CLI   IFLDH+5,1                                                        
         BH    OPTNVE                                                           
         SR    R8,R8                                                            
OPTNV1   IC    R8,0(R7)            FIND REQ NUM ENTRY                           
         LTR   R8,R8                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   1(1,R7),REQNUM                                                   
         BE    OPTNV2                                                           
         AR    R7,R8                                                            
         B     OPTNV1                                                           
OPTNV2   BCTR  R8,0                                                             
         BCTR  R8,0                                                             
OPTNV3   CLI   2(R7),255           ANY VALUE                                    
         BE    OPTNV4                                                           
         CLC   2(1,R7),IFLD        CHECK LIST OF VALUES FOR REQ NUM             
         BE    OPTNV4                                                           
         LA    R7,1(R7)                                                         
         BCT   R8,OPTNV3                                                        
         B     OPTNVE                                                           
OPTNV4   MVC   0(1,RA),IFLD                                                     
         OI    FIND,X'04'          OPTION = X                                   
         B     OPTNVO                                                           
OPTNVE   MVI   FERN,2              OPTION INVALID                               
OPTNVO   EQU   *                                                                
         XIT1                                                                   
*                                                                               
*        OPTION TABLE ENTRY  XL1=LENGTH , XL1=REQNUM , XLN=VALUES               
*                                                                               
OPT1VTBL DS    0C                                                               
         DC    AL1(03,20),C'Y'                                                  
         DC    AL1(03,22),C'Y'                                                  
         DC    AL1(03,26),C'Y'                                                  
         DC    AL1(03,28),C'Y'                                                  
         DC    AL1(03,30),C'Y'                                                  
         DC    AL1(03,36),C'Y'                                                  
         DC    AL1(03,38),C'Y'                                                  
         DC    AL1(03,40),X'FF'                                                 
         DC    AL1(03,41),X'FF'                                                 
         DC    AL1(03,42),C'Y'                                                  
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
OPT1VTX  DC    AL1(0)                                                           
*                                                                               
OPT2VTBL DS    0C                                                               
         DC    AL1(03,20),C'Y'                                                  
         DC    AL1(03,22),C'Y'                                                  
         DC    AL1(11,26),C'123456789'                                          
         DC    AL1(11,28),C'123456789'                                          
         DC    AL1(11,30),C'123456789'                                          
         DC    AL1(03,40),X'FF'                                                 
         DC    AL1(06,41),C'1234'                                               
         DC    AL1(03,00),X'FF'                                                 
OPT2VTX  DC    AL1(0)                                                           
*                                                                               
OPT3VTBL DS    0C                                                               
         DC    AL1(03,20),C'Y'                                                  
         DC    AL1(03,36),C'Y'                                                  
         DC    AL1(03,40),C'Y'                                                  
         DC    AL1(03,41),C'Y'                                                  
         DC    AL1(03,42),C'Y'                                                  
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
OPT3VTX  DC    AL1(0)                                                           
*                                                                               
OPT4VTBL DS    0C                                                               
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
OPT4VTX  DC    AL1(0)                                                           
*                                                                               
OPT5VTBL DS    0C                                                               
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
OPT5VTX  DC    AL1(0)                                                           
*                                                                               
OPT6VTBL DS    0C                                                               
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
OPT6VTX  DC    AL1(0)                                                           
         EJECT                                                                  
RPGMVAL  NTR1                REQ PROGRAM - 04=XXXX                              
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    RPGMVO                                                           
         CLI   IFLDH+5,4                                                        
         BH    RPGMVE                                                           
*                                                                               
RPGMV1   MVC   RMKT+4(4),IFLD                                                   
         OI    FIND,X'04'                                                       
         B     RPGMVX                                                           
*                                                                               
RPGMVO   EQU   *                                                                
         B     *+8                                                              
RPGMVE   MVI   FERN,X'02'                                                       
RPGMVX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        THIS TABLE CONTAINS THE ADDRESSES OF THE VALIDATION ROUTINES           
*        CONTAINED IN THIS PHASE. INDEXED BY ROUTNUM.                           
*                                                                               
ROUTADRT DC    A(0)                00 - N/D                                     
         DC    A(RNGEVAL)          01 - RANGE                                   
         DC    A(MKTSVAL)          02 - MARKET SEQUENCE                         
         DC    A(MKTVVAL)          03 - MARKET VALUE                            
         DC    A(TRGTVAL)          04 - TARGET                                  
         DC    A(LISTVAL)          05 - DATA TYPE LIST                          
         DC    A(STRDVAL)          06 - START DATE                              
         DC    A(ENDDVAL)          07 - END DATE                                
         DC    A(ZONEVAL)          08 - TIME ZONE                               
         DC    A(SPTLVAL)          09 - SPOT LENGTH                             
         DC    A(DAYPVAL)          10 - DAY PART                                
         DC    A(AFFLVAL)          11 - AFFILIATION                             
         DC    A(SLCTVAL)          12 - SELECT                                  
         DC    A(DATDVAL)          13 - BASE DATE                               
         DC    A(PROJVAL)          14 - PROJECTION FORMULA                      
         DC    A(RTGFVAL)          15 - RATING SERVICE FILTER                   
         DC    A(PTYPEVAL)         16 - PROGRAM TYPE FILTER                     
         DC    A(OPT1VAL)          17 - OPTION#1                                
         DC    A(OPT2VAL)          18 - OPTION#2                                
         DC    A(OPT3VAL)          18 - OPTION#3                                
         DC    A(OPT4VAL)          20 - OPTION#4                                
         DC    A(OPT5VAL)          21 - OPTION#5                                
         DC    A(OPT6VAL)          22 - OPTION#6                                
         DC    A(RPGMVAL)          23 - REQUEST PROGRAM                         
         EJECT                                                                  
*DEDBLOCK                                                                       
       ++INCLUDE DEDBLOCK                                                       
*DEEQUIVOLD                                                                     
       ++INCLUDE DEEQUIVOLD                                                     
*CPREQSAVE                                                                      
       ++INCLUDE CPREQSAVE                                                      
*CPREQTEMP                                                                      
       ++INCLUDE CPREQTEMP                                                      
       ++INCLUDE DDFLDIND                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035CPREQ03   05/01/02'                                      
         END                                                                    
