*          DATA SET SPREPMK02  AT LEVEL 026 AS OF 05/01/02                      
*PHASE SPMK02A                                                                  
         TITLE 'SPMK02 - KRAFT PRODUCT SCHEDULE'                                
         PRINT NOGEN                                                            
SPMK02   CSECT                                                                  
         NMOD1 0,**SPMK**                                                       
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         LA    R8,SPACEND                                                       
         USING MYD,R8                                                           
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         MVI   MEDEXTDM,1                                                       
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R1,RELOC                                                         
         S     R1,RELOC                                                         
         ST    R1,RELO                                                          
         CLI   MODE,RUNFRST        INITIALIZATION                               
         BNE   MK2                                                              
         MVC   MEDNUMWK,=F'56'                                                  
         MVC   MEDNUMMO,=F'12'                                                  
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'120'                                                 
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         B     XIT                                                              
         EJECT                                                                  
*              REQUEST AND ESTIMATE FIRST                                       
         SPACE 2                                                                
MK2      CLI   MODE,REQFRST                                                     
         BNE   MK10                                                             
         ZAP   MKTCNT,=P'0'                                                     
         ZAP   SLCNT,=P'0'                                                      
         CLI   QOPT1,C'2'                                                       
         BE    MK4                                                              
         BH    MK6                                                              
         MVI   RCSUBPRG,1                                                       
         MVI   FCRDBUYS,C'Y'                                                    
         MVI   FCRDGOAL,C'Y'                                                    
         B     MK8                                                              
         SPACE 2                                                                
MK4      MVI   RCSUBPRG,2                                                       
         MVI   FCRDGOAL,C'Y'                                                    
         MVI   FCRDBUYS,C'N'                                                    
         B     MK8                                                              
         SPACE 2                                                                
MK6      MVI   RCSUBPRG,3                                                       
         MVI   FCRDBUYS,C'Y'                                                    
         MVI   FCRDGOAL,C'N'                                                    
         SPACE 2                                                                
MK8      MVI   REQSW,C'Y'                                                       
         XC    REQPCT,REQPCT                                                    
         CLI   QUESTOR+3,C'.'                                                   
         BNE   XIT                                                              
         CLI   QUESTOR+6,C'%'      PERCENT SIGN HERE                            
         BNE   XIT                                                              
         MVC   WORK,QUESTOR                                                     
         OC    WORK(6),=6X'F0'                                                  
         PACK  DUB,WORK+1(2)                                                    
         CVB   R1,DUB                                                           
         PACK  DUB,WORK+4(2)                                                    
         CVB   R0,DUB                                                           
         MH    R1,=H'100'                                                       
         AR    R1,R0                                                            
         CLI   QUESTOR,C'-'                                                     
         BNE   *+6                                                              
         LCR   R1,R1                                                            
         ST    R1,REQPCT                                                        
         B     XIT                                                              
         SPACE 2                                                                
MK10     CLI   MODE,ESTFRST                                                     
         BNE   MK20                                                             
         CLI   REQSW,C'Y'                                                       
         BNE   XIT                                                              
         MVI   REQSW,C'N'                                                       
         MVC   PAGE,=H'1'                                                       
         MVC   MEDDFORM,SPOTPROF+2                                              
         GOTO1 MEDDATE,DMCB,(RA)                                                
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         BAS   RE,ESTCLEAR                                                      
         BAS   RE,MKTCLEAR                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO INITIALIZE DATES AND ACCUMULATORS                     
         SPACE 3                                                                
ESTCLEAR NTR1                                                                   
         L     R2,=A(ESTCHUNK)                                                  
         ZAP   MKTCNT,=P'0'                                                     
         B     MKTCLB                                                           
         SPACE 2                                                                
MKTCLEAR NTR1                                                                   
         L     R2,=A(MKTCHUNK)                                                  
         SPACE 1                                                                
MKTCLB   A     R2,RELO                                                          
         L     R3,NCHUNK                                                        
         LA    R3,1(R3)            PLUS 1 FOR TOTS                              
         SPACE 2                                                                
MKTCL2   BAS   RE,CLEAR                                                         
         A     R2,LCHUNK                                                        
         BCT   R3,MKTCL2                                                        
         B     XIT                                                              
         SPACE 2                                                                
CLEAR    NTR1                                                                   
         LR    R3,R2               FIRST CLEAR                                  
         LA    R4,85                                                            
         SPACE 2                                                                
CLEAR2   XC    0(48,R3),0(R3)                                                   
         LA    R3,48(R3)                                                        
         BCT   R4,CLEAR2                                                        
         SPACE 1                                                                
         LR    R3,R2                                                            
         LA    R4,12                                                            
         LA    R5,MEDMON01                                                      
         SPACE 2                                                                
CLEAR4   MVC   288(8,R3),0(R5)     NOW PUT IN MONTH START/END DATES             
         LA    R3,336(R3)                           (AND POST ADD)              
         LA    R5,12(R5)                                                        
         BCT   R4,CLEAR4                                                        
         MVC   0(8,R3),MEDPERD     (YEAR DATES AND ADDRESS)                     
         MVC   0(4,R3),=C'YEAR'                                                 
         LR    R3,R2                                                            
         LA    R4,12               FOR EACH MONTH                               
         LA    R5,MEDWEEKS                                                      
         LA    R6,56                                                            
         SPACE 2                                                                
CLEAR6   BAS   RE,CLEAR8           GO AND PUT IN WEEK DATE                      
         LA    R3,336(R3)                                                       
         BCT   R4,CLEAR6                                                        
         B     XIT                                                              
         SPACE 2                                                                
CLEAR8   NTR1                                                                   
         MVC   FULL,288(R3)        PICK OUT MONTH DATES                         
         OC    FULL,FULL                                                        
         BZ    XIT                                                              
         MVC   288(4,R3),=C'*TOT'                                               
         SPACE 2                                                                
CLEAR10  CLC   0(2,R5),FULL        DOES WEEK FIT IN MONTH S/E                   
         BL    CLEAR12                                                          
         CLC   2(2,R5),FULL+2                                                   
         BH    XIT                                                              
         MVC   0(8,R3),0(R5)       THEN MAKE A WEEK ENTRY                       
         LA    R3,48(R3)                                                        
         SPACE 2                                                                
CLEAR12  LA    R5,12(R5)                                                        
         BCT   R6,CLEAR10                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO POST GOALS                                           
         SPACE 3                                                                
MK20     CLI   MODE,PROCGOAL                                                    
         BNE   MK40                                                             
         L     R5,ADGOAL                                                        
         USING GOALREC,R5                                                       
         L     R6,MEDBUFF                                                       
         USING MEDBLOCK,R6                                                      
         MVC   MEDBRAND,GKEYPRD                                                 
         MVC   MEDSPTLN,GKEYSLN                                                 
         GOTO1 MEDGETGL,DMCB,(RA)                                               
         L     R2,=A(MKTCHUNK)                                                  
         A     R2,RELO                                                          
         LA    R3,85                                                            
         LA    R1,SLLIST                                                        
         SPACE 2                                                                
MK21     CLC   0(1,R1),MEDSPTLN                                                 
         BE    MK22                                                             
         CLI   0(R1),X'FF'                                                      
         BE    MK22                                                             
         LA    R1,1(R1)                                                         
         A     R2,LCHUNK                                                        
         B     MK21                                                             
         SPACE 2                                                                
MK22     BAS   RE,MK24                                                          
         LA    R2,48(R2)                                                        
         BCT   R3,MK22                                                          
         B     XIT                                                              
         SPACE 2                                                                
MK24     NTR1                                                                   
         OC    0(8,R2),0(R2)                                                    
         BZ    XIT                                                              
         USING LINED,R2                                                         
         USING MEDDATA,R3                                                       
         L     R3,LINADD                                                        
         SPACE 2                                                                
MK25     LA    R1,LINDAY                                                        
         LA    R4,DPLIST                                                        
         LA    R5,6                                                             
         SPACE 2                                                                
MK26     CLC   0(3,R4),MEDDPART    LOOK UP DAYPART CODE                         
         BE    MK28                                                             
         LA    R1,4(R1)                                                         
         LA    R4,3(R4)                                                         
         BCT   R5,MK26                                                          
         B     MK30                                                             
         SPACE 2                                                                
MK28     L     R0,0(R1)                 POST (DAYPART) POINTS                   
         A     R0,MEDGL1                                                        
         ST    R0,0(R1)                                                         
         SPACE 2                                                                
MK30     L     R0,LINGGRP          POST TOTAL POINTS                            
         A     R0,MEDGL1                                                        
         ST    R0,LINGGRP                                                       
         L     R1,MEDGLD           AND DOLLARS                                  
         AH    R1,=H'50'                                                        
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         A     R1,LINGDOL                                                       
         ST    R1,LINGDOL                                                       
         B     XIT                                                              
         SPACE 2                                                                
DPLIST   DC    C'DAYEFRLFRACCLNWPRI'                                            
         EJECT                                                                  
*              ROUTINES TO POST BUYS - CONTROL                                  
         SPACE 3                                                                
MK40     CLI   MODE,PROCBUY                                                     
         BNE   MK60                                                             
         XC    PSLIST,PSLIST                                                    
         GOTO1 MEDPSL,DMCB,(RA),PSLIST                                          
         LA    R2,PSLIST                                                        
         SPACE 2                                                                
MK42     OC    0(2,R2),0(R2)       LOOP ON BRAND/SPOT LENGTH                    
         BZ    XIT                                                              
         CLI   BPRD,X'FF'                                                       
         BE    MK44                                                             
         CLC   BPRD,0(R2)                                                       
         BNE   MK46                                                             
         SPACE 2                                                                
MK44     MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
         GOTO1 MEDGETBY,DMCB,(RA),2                                             
         BAS   RE,MK50                                                          
         SPACE 2                                                                
MK46     LA    R2,2(R2)                                                         
         B     MK42                                                             
         EJECT                                                                  
*              ROUTINE TO POST BUYS - POSTING                                   
         SPACE 3                                                                
MK50     NTR1                                                                   
         L     R2,=A(MKTCHUNK)                                                  
         A     R2,RELO                                                          
         LA    R3,85                                                            
         LA    R1,SLLIST                                                        
         SPACE 2                                                                
MK51     CLC   0(1,R1),MEDSPTLN                                                 
         BE    MK52                                                             
         CLI   0(R1),X'FF'                                                      
         BE    MK52                                                             
         LA    R1,1(R1)                                                         
         A     R2,LCHUNK                                                        
         B     MK51                                                             
         SPACE 2                                                                
MK52     BAS   RE,MK54                                                          
         LA    R2,48(R2)                                                        
         BCT   R3,MK52                                                          
         B     XIT                                                              
         SPACE 2                                                                
MK54     NTR1                                                                   
         OC    0(8,R2),0(R2)                                                    
         BZ    XIT                                                              
         USING LINED,R2                                                         
         L     R3,LINADD                                                        
         USING MEDDATA,R3                                                       
         CLI   QOPT1,C'3'          FOR OPTION 3 (PURCHASED)                     
         BNE   MK56                                                             
         MVC   MEDGL1,MEDBYP       MAKE BUYS LOOK LIKE GOALS                    
         MVC   MEDGLD,MEDBYD                                                    
         B     MK25                AND USE GOAL ROUTINES                        
         SPACE 2                                                                
MK56     L     R1,MEDBYP           POST POINTS                                  
         A     R1,LINPGRP                                                       
         ST    R1,LINPGRP                                                       
         L     R1,MEDBYD                AND DOLLARS                             
         M     R0,=F'2'                                                         
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         A     R1,LINPDOL                                                       
         ST    R1,LINPDOL                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO CONTROL TOTALS                                       
         SPACE 3                                                                
MK60     CLI   MODE,MKTLAST                                                     
         BNE   MK62                                                             
         L     R2,=A(MKTCHUNK)                                                  
         A     R2,RELO                                                          
         L     R3,=A(TOTCHUNK)                                                  
         A     R3,RELO                                                          
         BAS   RE,MULTADD                                                       
         BAS   RE,ADDTOEST                                                      
         MVI   ACTIVE,C'N'                                                      
         BAS   RE,MULTTOT                                                       
         CLI   ACTIVE,C'Y'                                                      
         BNE   *+10                                                             
         AP    MKTCNT,=P'1'                                                     
         BAS   RE,MKTCLEAR                                                      
         B     XIT                                                              
         SPACE 2                                                                
MK62     CLI   MODE,PRDLAST                                                     
         BNE   XIT                                                              
         L     R2,=A(ESTCHUNK)                                                  
         A     R2,RELO                                                          
         L     R3,=A(ESTTOTS)                                                   
         A     R3,RELO                                                          
         BAS   RE,MULTTOT                                                       
         BAS   RE,ESTCLEAR                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES CONTROL MUTIPLE ADDING AND TOTALLING                    
         SPACE 3                                                                
MULTADD  NTR1                                                                   
         L     R4,NCHUNK           ADD SL TO TOT                                
         SPACE 1                                                                
MADD2    BAS   RE,ADDEM                                                         
         A     R2,LCHUNK                                                        
         BCT   R4,MADD2                                                         
         B     XIT                                                              
         SPACE 2                                                                
ADDTOEST NTR1                                                                   
         L     R3,=A(ESTCHUNK)     ADD MKT TO EST                               
         A     R3,RELO                                                          
         L     R4,NCHUNK                                                        
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
ADEST2   BAS   RE,ADDEM                                                         
         A     R2,LCHUNK                                                        
         A     R3,LCHUNK                                                        
         BCT   R4,ADEST2                                                        
         B     XIT                                                              
         SPACE 2                                                                
MULTTOT  NTR1                                                                   
         L     R3,NCHUNK                                                        
         LA    R4,SLALPHA                                                       
         ZAP   SLCNT,=P'0'                                                      
         SPACE 1                                                                
MULT2    MVC   THISSL,0(R4)        IS THIS SL ACTIVE                            
         LA    R5,84                                                            
         MH    R5,=H'48'                                                        
         AR    R5,R2                                                            
         OC    8(40,R5),8(R5)                                                   
         BZ    MULT4                                                            
         MVI   ACTIVE,C'Y'                                                      
         AP    SLCNT,=P'1'                                                      
         BAS   RE,TOTALS                                                        
         SPACE 1                                                                
MULT4    A     R2,LCHUNK                                                        
         LA    R4,3(R4)                                                         
         BCT   R3,MULT2                                                         
         MVC   THISSL,=C'ALL'                                                   
         CP    SLCNT,=P'2'                                                      
         BL    XIT                                                              
         BAS   RE,TOTALS                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD CHUNKS                                            
         SPACE 3                                                                
ADDEM    NTR1                                                                   
         LA    R4,85               85 LINES                                     
         SPACE 2                                                                
ADD2     BAS   RE,ADD4                                                          
         LA    R2,48(R2)                                                        
         LA    R3,48(R3)                                                        
         BCT   R4,ADD2                                                          
         B     XIT                                                              
         SPACE 2                                                                
ADD4     NTR1                                                                   
         LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         LA    R4,10               10 ACCUMS PER LINE                           
         SPACE 2                                                                
ADD6     L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,ADD6                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT CHUNKS                                          
         SPACE 3                                                                
TOTALS   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         LA    R5,42                                                            
         MH    R5,=H'48'                                                        
         LA    R4,42                                                            
         SPACE 2                                                                
TOT2     LA    R3,P                MONTHS 1-6 ON LEFT SIDE OF PAGE              
         BAS   RE,FORMAT                                                        
         AR    R2,R5                                                            
         LA    R3,P+66                    7-12 ON RIGHT                         
         BAS   RE,FORMAT                                                        
         SR    R2,R5                                                            
         CLC   P,SPACES                                                         
         BE    TOT4                                                             
         GOTO1 REPORT                                                           
         SPACE 2                                                                
TOT4     LA    R2,48(R2)                                                        
         BCT   R4,TOT2                                                          
         AR    R2,R5                                                            
         BAS   RE,FORMAT                                                        
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT NUMBERS                                        
         SPACE 3                                                                
FORMAT   NTR1                                                                   
         MVI   SPACING,2                                                        
         USING LINED,R2                                                         
         OC    LINST(4),LINST                                                   
         BZ    XIT                                                              
         MVC   0(4,R3),LINST                                                    
         CLC   LINST(4),=C'YEAR'                                                
         BE    FORM2                                                            
         MVI   4(R3),C'*'                                                       
         CLC   LINST(4),=C'*TOT'                                                
         BE    FORM2                                                            
         MVI   SPACING,1                                                        
         GOTO1 DATCON,DMCB,(2,LINST),(4,0(R3))                                  
         SPACE 2                                                                
FORM2    LA    R4,LINDAY           EDIT GRPS                                    
         LA    R5,5(R3)                                                         
         LA    R6,8                                                             
         SPACE 2                                                                
FORM4    L     R1,0(R4)                                                         
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(5,(R5)),ZERO=BLANK                                         
         CLI   0(R5),C' '                                                       
         BE    FORM8                                                            
         MVC   0(5,R5),SPACES                                                   
         SH    R5,=H'3'                                                         
         CLC   0(3,R5),SPACES                                                   
         BNE   FORM6                                                            
         EDIT  (R1),(8,(R5))                                                    
         LA    R5,3(R5)                                                         
         B     FORM8                                                            
         SPACE 2                                                                
FORM6    EDIT  (R1),(8,132(R5))                                                 
         LA    R5,3(R5)                                                         
         SPACE 2                                                                
FORM8    LA    R4,4(R4)                                                         
         LA    R5,5(R5)                                                         
         BCT   R6,FORM4                                                         
         SPACE 2                                                                
         L     R1,LINGDOL                                                       
         LR    RF,R1               POSSIBLE PERCENT ADJUSTMENT                  
         M     R0,REQPCT                                                        
         D     R0,=F'10000'                                                     
         AR    R1,RF                                                            
         EDIT  (R1),(9,45(R3)),ZERO=BLANK                                       
         L     R1,LINPDOL                                                       
         LR    RF,R1               POSSIBLE PERCENT ADJUSTMENT                  
         M     R0,REQPCT                                                        
         D     R0,=F'10000'                                                     
         AR    R1,RF                                                            
         EDIT  (R1),(9,54(R3)),ZERO=BLANK                                       
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
         DS    0H                                                               
         USING *,RF                                                             
HOOK     NTR1                                                                   
         L     RE,4(RD)                                                         
         CLC   0(4,RE),=C'REPT'                                                 
         BE    *+12                                                             
         L     RE,4(RE)                                                         
         B     *-14                                                             
         LM    RE,RC,12(RE)                                                     
         DROP  RF                                                               
         CLI   MODE,PRDLAST                                                     
         BNE   HOOK2                                                            
         MVC   H6+54(22),=C'TOTALS FOR ALL MARKETS'                             
         SPACE 2                                                                
HOOK2    MVC   H6+99(14),=C'SPOT LENGTH - '                                     
         MVC   H6+113(3),THISSL                                                 
         B     XIT                                                              
         EJECT                                                                  
*              LTORG  ETC                                                       
         SPACE 3                                                                
THISSL   DC    C'ALL'                                                           
SLLIST   DC    AL1(10,15,20,30,40,45,60,90,120)                                 
         DC    X'FF'                                                            
SLALPHA  DC    C'10 15 20 30 40 45 60 90 120OTH'                                
         DC    C'ALL'                                                           
NCHUNK   DC    F'10'                                                            
LCHUNK   DC    F'4080'                                                          
RELOC    DC    A(*)                                                             
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LINE IN ACCUMULATORS                              
         SPACE 3                                                                
LINED    DSECT                                                                  
LINST    DS    CL2                                                              
LINEND   DS    CL2                                                              
LINADD   DS    A                                                                
LINDAY   DS    F                                                                
LINEFR   DS    F                                                                
LINLFR   DS    F                                                                
LINENW   DS    F                                                                
LINLNW   DS    F                                                                
LINPRI   DS    F                                                                
LINGGRP  DS    F                                                                
LINPGRP  DS    F                                                                
LINGDOL  DS    F                                                                
LINPDOL  DS    F                                                                
         SPACE 2                                                                
*              ACCUMULATORS ARE ARRANGED AS FOLLOWS                             
         SPACE 1                                                                
*              LINES 1-6           WEEKS FOR MONTH 1                            
*                    7             TOTAL FOR MONTH 1                            
*              ETC                                                              
*              LINE  84            TOTAL FOR MONTH 12                           
*              LINE  85            YEAR TOTALS                                  
         SPACE 3                                                                
*              PROGRAM STORAGE                                                  
         SPACE 3                                                                
MYD      DSECT                                                                  
REQSW    DS    CL1                                                              
PSLIST   DS    CL100                                                            
MKTCNT   DS    PL4                                                              
SLCNT    DS    PL4                                                              
REQPCT   DS    F                                                                
ACTIVE   DS    CL1                                                              
RELO     DS    A                                                                
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
         SPACE 3                                                                
         ENTRY MKTCHUNK                                                         
         ENTRY TOTCHUNK                                                         
         ENTRY ESTCHUNK                                                         
         ENTRY ESTTOTS                                                          
         SPACE 1                                                                
CHUNKS   CSECT                                                                  
MKTCHUNK DC    850XL48'00'                                                      
TOTCHUNK DC    085XL48'00'                                                      
ESTCHUNK DC    850XL48'00'                                                      
ESTTOTS  DC    085XL48'00'                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPREPMK02 05/01/02'                                      
         END                                                                    
