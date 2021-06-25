*          DATA SET NEPUP15    AT LEVEL 060 AS OF 05/01/02                      
*PHASE T32215A,*                                                                
         TITLE 'T32215 - PLAN CPM'                                              
T32215   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32215**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   VAL2                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
VAL2     CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE 'RECORD' FOR REPORT                                     
         SPACE 3                                                                
VKEY     NTR1                                                                   
         SPACE 1                                                                
         XC    ACCUMS(256),ACCUMS                                               
         LA    R2,PUPCLIH          CLIENT (REQUIRED)                            
         GOTO1 VVALCLT                                                          
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,PUPNETH          NETWORK                                      
         GOTO1 VVALNET                                                          
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         GOTO1 VVALDPT                                                          
         MVC   PUPDPT,DPTNAME                                                   
         OI    PUPDPTH+6,X'80'                                                  
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         LA    R2,PUPPLANH         PLAN                                         
         GOTO1 VVALPLAN                                                         
         MVC   PLANADJP,=F'1000000' O PACKAGE ADJUSTMENT                        
         XC    PLANDADJ,PLANDADJ   NO PLAN ADJUSTMENT                           
         L     R4,AIO                                                           
         USING NPLRECD,R4                                                       
         EJECT                                                                  
*              VALIDATE OPTIONAL DEMO AND FILTERS                               
         SPACE 3                                                                
VKEY1    MVI   DEMG,C'N'                                                        
         LA    R2,PUPDEMOH         OPTIONAL DEMO OVERRIDE                       
         CLI   5(R2),0                                                          
         BE    VKEY2                                                            
         MVI   DEMG,C'Y'                                                        
         MVI   MAX,1                                                            
         GOTO1 VVALDEM             (SETS DEMOS WITH OVERRIDE)                   
         LA    RE,6                CHECK DEMO IS IN PLAN                        
         LA    RF,NPLNDEMS                                                      
VKEY1A   CLC   0(3,RF),DEMOS                                                    
         BE    VKEY1B                                                           
         LA    RF,3(RF)                                                         
         BCT   RE,VKEY1A                                                        
         B     DEMOERR                                                          
VKEY1B   MVC   NPLNDEMS(3),DEMOS                                                
         GOTO1 VEXTPLAN            (SETS TARGET AND TARGNAME)                   
         SPACE 1                                                                
VKEY2    MVC   AIO,AIO1                                                         
         MVC   WORK(10),TARGNAME                                                
         GOTO1 CENTER,DMCB,WORK,10                                              
         LA    R1,WORK                                                          
         LA    R0,10                                                            
         SPACE 1                                                                
VKEY4    CLI   0(R1),C' '          FILL TARGNAME WITH DASHES                    
         BNE   *+8                                                              
         MVI   0(R1),C'-'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,VKEY4                                                         
         MVC   PUPTITR+6(10),WORK   SHOW IT IN THE HEADINGS                     
         OI    PUPTITRH+6,X'80'                                                 
         SPACE 1                                                                
VKEY6    XC    ACCUMS,ACCUMS                                                    
         BAS   RE,POSTBUDG                                                      
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              HANDLE I/O FOR REPORT                                            
         SPACE 3                                                                
PREP     NTR1                                                                   
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPUKEY,R4                                                        
         MVI   NPUKTYPE,X'22'      FILL PROGRAM KEY                             
         MVC   NPUKAM,BINAGYMD                                                  
         MVC   NPUKCLT,CLTCOMP                                                  
         MVC   NPUKNET,NETWORK                                                  
         MVC   NPUKDPT,DPTCODE                                                  
         MVC   NPUKPLAN,PLANCODE                                                
         GOTO1 HIGH                                                             
         B     PREP6                                                            
         SPACE 1                                                                
PREP4    GOTO1 SEQ                                                              
         SPACE 1                                                                
PREP6    CLC   KEY(13),KEYSAVE     MUST HAVE MATCH ON PLAN                      
         BNE   PREPEND                                                          
         MVC   PROGKEY,KEY                                                      
         GOTO1 GETREC                                                           
         GOTO1 VEXTPROG                                                         
         BAS   RE,POSTPROG                                                      
         B     PREP4                                                            
         SPACE 1                                                                
PREPEND  BAS   RE,ADDEM                                                         
         BAS   RE,CALPKGUA                                                      
         BAS   RE,SHOWVALS                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST BUDGETS FROM PLAN                                
         SPACE 3                                                                
POSTBUDG NTR1                                                                   
         MVC   LENGTH,REQLEN                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 VEXTBUDG            GET BUDGETS (MAYBE FOR LENGTH)               
         MVC   AIO,AIO1                                                         
         LA    R2,BUDGETS                                                       
         LA    R3,ACCUMS                                                        
         LA    R0,4                                                             
         SPACE 1                                                                
PBUDG2   MVC   0(4,R3),0(R2)       MOVE BUDGETS INTO ACCUMS                     
         LA    R2,20(R2)                                                        
         LA    R3,32(R3)                                                        
         BCT   R0,PBUDG2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST PROGRAM DETAILS                                  
         SPACE 3                                                                
POSTPROG NTR1                                                                   
         LA    R2,PLANPLST         SET UP FOR PERIODS                           
         ZIC   R0,PLANNPER                                                      
         SPACE 1                                                                
PPROG2   MVC   PERIOD,0(R2)                                                     
         ZIC   R3,2(R2)            PICK UP QUARTER NUMBER                       
         MH    R3,=H'32'           FOR ACCUMULATOR SET                          
         LA    R3,ACCUMS(R3)                                                    
         USING ACCUMD,R3                                                        
         SPACE 1                                                                
PPROG4   MVC   LENGTH,REQLEN                                                    
         GOTO1 VEXTUNS                                                          
         ZIC   R1,UNITS            ADD IN UNITS                                 
         A     R1,ACUNITS                                                       
         ST    R1,ACUNITS                                                       
         SPACE 1                                                                
         MVI   GDRNDOPT,C'Y'                                                    
         MVI   GDNOADJ,C'Y'                                                     
         MVC   GDDEMO,=X'000001'     GO FOR HOMES FIRST                         
***      MVI   GDDEMO+2,1            GO FOR HOMES FIRST                         
         MVC   LENGTH,REQLEN                                                    
*        GOTO1 VGETDEM                                                          
*        L     R1,GDTGRP           HOMES GRPS                                   
*        A     R1,ACHOMGRP                                                      
*        ST    R1,ACHOMGRP                                                      
*        L     R1,GDTIMP           HOMES (000)                                  
*        A     R1,ACHOMIMP                                                      
*        ST    R1,ACHOMIMP                                                      
         SPACE 1                                                                
         MVC   GDDEMO,TARGET       THEN THE TARGET                              
*        MVC   GDDEMO,TARGET+2     THEN THE TARGET                              
         MVC   LENGTH,REQLEN                                                    
         GOTO1 VGETDEM                                                          
         L     R1,GDTGRP           FOR TARGET GRP                               
         A     R1,ACTARGRP                                                      
         ST    R1,ACTARGRP                                                      
         L     R1,GDTIMP           AND (000)                                    
         A     R1,ACTARIMP                                                      
         ST    R1,ACTARIMP                                                      
         SPACE 1                                                                
         LA    R2,4(R2)                                                         
         BCT   R0,PPROG2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              NOW ADD EACH QUARTER INTO BOTTOM LINE TOTAL                      
         SPACE 3                                                                
ADDEM    NTR1                                                                   
         LA    R2,ACCUMS           ADD DETAILS INTO TOTALS                      
         LA    R3,ACCUMS+128                                                    
         LA    R0,4                                                             
         SPACE 1                                                                
ADDEM2   BAS   RE,ADDEM4                                                        
         LA    R2,32(R2)                                                        
         BCT   R0,ADDEM2                                                        
         B     XIT                                                              
         SPACE 1                                                                
ADDEM4   NTR1                                                                   
         LA    R0,6                ADD A LINE                                   
         SPACE 1                                                                
ADDEM6   L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,ADDEM6                                                        
         B     XIT                                                              
         EJECT                                                                  
*              GENERATE A PACKAGE GUARANTEE                                     
*              AND WRITE TO THE PLAN RECORD                                     
         SPACE 3                                                                
CALPKGUA NTR1                                                                   
         L     R4,AIO2             PLAN RECORD                                  
         USING NPLRECD,R4                                                       
         USING ACCUMD,R3                                                        
         L     R6,AIO2             BUDGET ELEMENT                               
         USING NPBELD,R6                                                        
*-REREAD PLAN RECORD                                                            
         MVC   AIO,AIO2                                                         
         MVC   KEY(20),NPLKEY                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
*--PROCESS BUDGET INFO                                                          
*                                                                               
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         B     CALPK070                                                         
*                                                                               
CALPK060 BAS   RE,NEXTEL                                                        
*                                                                               
*-POINT TO RIGHT TABLE ENTRY                                                    
*                                                                               
CALPK070 BNE   CALPK200                                                         
         LA    R5,BUDGTAB                                                       
CALPK080 CLC   0(1,R5),NPBPER+1                                                 
         BE    CALPK100                                                         
         LA    R5,5(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   CALPK080                                                         
         DC    H'0'                CHECK BUDGET ELEMENT IN R6                   
*                                                                               
*-POSITION ACCUM TABLE AND CALCULATE PACKAGE GUARANTEE                          
*                                                                               
CALPK100 LA    R3,ACCUMS                                                        
         A     R3,1(R5)                                                         
         CLC   ACDONIND(4),=XL4'FFFFFFFF'  WAS ENTRY PROCESSED                  
         BE    CALPK060                                                         
         MVC   ACDONIND(4),=XL4'FFFFFFFF'  SET ENTRY PROCESSED                  
         MVC   ACTARGUA,NPBPKGUA   MOVE PACKAGE GUARANTEE TO TABLE              
         OC    NPBCPGUA,NPBCPGUA   CHECK FOR GUA. CPM                           
         BZ    CALPK120            IF NONE GET NEXT ELEMENT                     
         MVC   ACTARCPM,NPBCPGUA   MOVE CPM TO ACCUM TABLE                      
         LA    R2,NPBCPGUA         GUARANTEE CPM                                
*                                                                               
         BAS   RE,GETGUA                                                        
         MVC   NPBPKGUA,PGUAHLD    UPDATE ELEMENT                               
         MVC   ACTARGUA,PGUAHLD    UPDATE THE TABLE                             
CALPK120 BAS   RE,RANYADJ                                                       
         LA    R5,ACTARIMP                                                      
         BAS   RE,ROUND                                                         
         B     CALPK060                                                         
*                                                                               
*-DO PLAN LEVEL CPM                                                             
*                                                                               
CALPK200 LA    R3,ACCUMS                                                        
         LA    R3,128(R3)          POINT TO TOTAL LINE                          
         CLI   DEMG,C'Y'           ARE WE DOING DEMO GUAR'S                     
         BE    CALPK400                                                         
*                                                                               
         MVC   ACTARGUA,NPLNADJP   UPDATE RECORD                                
         OC    NPLNGCPM,NPLNGCPM   CHECK GUARANTEE CPM                          
         BZ    CALPK300                                                         
*                                                                               
         MVC   ACTARCPM,NPLNGCPM   MOVE CPM TO ACCUM TABLE                      
         LA    R2,NPLNGCPM         GUARANTEE CPM                                
*                                                                               
         BAS   RE,GETGUA                                                        
         MVC   NPLNADJP,PGUAHLD    UPDATE RECORD                                
         MVC   ACTARGUA,PGUAHLD    UPDATE THE TABLE                             
CALPK300 BAS   RE,RANYADJ                                                       
         LA    R5,ACTARIMP                                                      
         BAS   RE,ROUND                                                         
         B     CALPK800                                                         
*--DO DEMO GUARANTEES                                                           
CALPK400 XC    ACTARGUA,ACTARGUA                                                
         L     R6,AIO2             DEMO GUAR INFO                               
         USING NPNELD,R6                                                        
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   CALPK500                                                         
*                                                                               
         MVC   ACTARGUA,NPNADJD    UPDATE RECORD                                
         OC    NPNDCPM,NPNDCPM     CHECK GUARANTEE CPM                          
         BZ    CALPK500                                                         
*                                                                               
         MVC   ACTARCPM,NPNDCPM    MOVE CPM TO ACCUM TABLE                      
         LA    R2,NPNDCPM          GUARANTEE CPM                                
*                                                                               
         BAS   RE,GETGUA                                                        
         MVC   NPNADJD,PGUAHLD     UPDATE RECORD                                
         MVC   NPNDEMO,TARGET      UPDATE WITH DEMO                             
         MVC   ACTARGUA,PGUAHLD    UPDATE THE TABLE                             
CALPK500 BAS   RE,RANYADJ                                                       
         LA    R5,ACTARIMP                                                      
         BAS   RE,ROUND                                                         
*                                                                               
CALPK800 MVC   AIO,AIO2                                                         
         GOTO1 PUTREC              WRITE BACK PLAN RECORD                       
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
*     BYTE 1 = QUARTER   BYTE 2-5 = DISPLACEMENT INTO ACCUMS TABLE              
BUDGTAB  DC    X'04',XL4'00000000'                                              
         DC    X'01',XL4'00000020'                                              
         DC    X'02',XL4'00000040'                                              
         DC    X'03',XL4'00000060'                                              
         DC    X'FF'                                                            
         EJECT                                                                  
*              SUB-ROUTINE TO CALC PACKAGE GUAANTEE                             
         SPACE 3                                                                
*                                  R3=A(ACCUMS)                                 
*                                  R3=A(IMPS)                                   
*                                  R2=A(GUARANTEE CPM)                          
         SPACE 1                                                                
         USING ACCUMD,R3                                                        
GETGUA   NTR1                                                                   
         XC    PGUAHLD,PGUAHLD                                                  
*                                                                               
         L     R1,ACBUDGET                                                      
         LA    R4,ACTARIMP                                                      
*                                                                               
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         OC    ACTARIMP,ACTARIMP                                                
         BZ    XIT                                                              
         M     R0,=F'200000'                                                    
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
*                                                                               
         M     R0,=F'10000'                                                     
         D     R0,0(R2)                                                         
         ST    R1,PGUAHLD                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ADJUST DEMOS BY GUARANTEED FACTORS                               
*                                                                               
RANYADJ  NTR1                                                                   
         MVC   FULL,ACTARGUA                                                    
         L     R2,FULL            PICK UP ALL ADJUSTMENT FACTOR                 
         MVC   FULL,=F'500000'                                                  
         SPACE 1                                                                
ANYADJ2  LTR   R2,R2                                                            
         BZ    XIT                                                              
         L     R1,ACTARGRP         ADJUST GRPS                                  
         MR    R0,R2                                                            
         D     R0,FULL                                                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,ACTARGRP                                                      
         SPACE 1                                                                
ANYADJ4  L     R1,ACTARIMP         ADJUST IMPRESSIONS                           
         MR    R0,R2                                                            
         D     R0,FULL                                                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,ACTARIMP                                                      
         B     XIT                                                              
         EJECT                                                                  
ROUND    NTR1                                                                   
         L     R1,0(R5)                                                         
         CLI   NETMAIN,0                                                        
         BE    *+8                                                              
         CLI   NETMEDIA,C'S'       SYNDICATION                                  
         BE    *+8                                                              
         CLI   NETMAIN,C'Y'        OR MAIN NETWORK                              
         BNE   ROUND2                                                           
         AH    R1,=H'50'                                                        
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         M     R0,=F'10'           (RETURN MAIN IN 000 TO NEAREST               
         ST    R1,0(R5)                                   10000)                
         B     XIT                                                              
         SPACE 1                                                                
ROUND2   AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         ST    R1,0(R5)                                   10000)                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY VALUES FOR PLAN                               
         SPACE 3                                                                
SHOWVALS NTR1                                                                   
*--JUST DO TOTAL LINE                                                           
         LA    R2,PUPPERH                                                       
*        LA    R3,ACCUMS                                                        
         LA    R3,ACCUMS+128                                                    
         USING ACCUMD,R3                                                        
*        LA    R4,5                                                             
         LA    R4,1                                                             
         B     SVAL4                                                            
         SPACE 1                                                                
SVAL2    CLI   8(R2),C'T'          SCREEN SHOWS TOTAL OR QUARTER                
         BE    SVAL4                                                            
         ZIC   R1,PLANYEAR                                                      
         CH    R4,=H'5'                                                         
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         EDIT  (R1),(2,12(R2))     SO EDIT YEAR                                 
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
SVAL4    BAS   RE,BUMP                                                          
         BAS   RE,SHOWLINE                                                      
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         LA    R3,32(R3)                                                        
         BCT   R4,SVAL2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EDIT A LINE OF NUMBERS                                
         SPACE 3                                                                
SHOWLINE NTR1                                                                   
         LA    R2,8(R2)            GET PAST HEADER                              
         MVC   0(56,R2),SPACES     AND CLEAR                                    
         USING ACCUMD,R3                                                        
*                                  SHOW BUDGET                                  
         EDIT  (4,ACBUDGET),(9,0(R2))                                           
         LA    R2,9(R2)            UNITS                                        
         EDIT  (4,ACUNITS),(5,0(R2))                                            
         LA    R2,5(R2)            COST PER UNIT                                
         L     R1,ACBUDGET                                                      
         LTR   R1,R1                                                            
         BZ    SLINE2                                                           
         SR    R0,R0                                                            
         OC    ACUNITS,ACUNITS                                                  
         BZ    SLINE2                                                           
         D     R0,ACUNITS                                                       
         EDIT  (R1),(7,0(R2)),FLOAT=$                                           
         CLI   0(R2),C'$'                                                       
         BNE   SLINE2                                                           
         MVI   0(R2),C' '                                                       
         SPACE 1                                                                
SLINE2   LA    R2,8(R2)                                                         
*        EDIT  (4,ACTARGRP),(7,0(R2)),1,ZERO=BLANK                              
*        CLI   0(R2),C' '                                                       
*        BE    SLINE8                                                           
*        L     R1,ACTARGRP                                                      
*        AH    R1,=H'5'                                                         
*        SR    R0,R0                                                            
*        D     R0,=F'10'                                                        
*        EDIT  (R1),(7,0(R2))                                                   
         SPACE 1                                                                
*        LA    R2,7(R2)                                                         
SLINE8   LA    R4,ACTARIMP         TARGET IMPS AND CPM                          
         BAS   RE,CPM                                                           
         LA    R2,22(R2)           PACKAGE GUARANTEE                            
         EDIT  (4,ACTARGUA),(11,0(R2)),4,ZERO=BLANK                             
         B     XIT                                                              
         EJECT                                                                  
*              SUB-ROUTINE TO SHOW IMPS AND CPM                                 
         SPACE 3                                                                
*              INPUTS              R2=A(OUTPUT AREA)                            
*                                  R3=A(ACCUMS)                                 
*                                  R3=A(IMPS)                                   
         SPACE 1                                                                
         USING ACCUMD,R3                                                        
CPM      NTR1                                                                   
         L     R1,0(R4)            PICK UP IMPS                                 
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         EDIT  (R1),(8,0(R2))                                                   
         SPACE 1                                                                
         LA    R2,14(R2)           COMPUTE CPM IN PENNIES                       
         MVI   7(R2),C'*'          SET GUARANTEE CPM                            
         L     R1,ACTARCPM         WAS CPM ALREADY CALCED                       
         LTR   R1,R1                                                            
         BNZ   CPM10                                                            
         MVI   7(R2),C' '          NO GUARANTEE CPM                             
         L     R1,ACBUDGET                                                      
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         M     R0,=F'200'                                                       
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
CPM10    EDIT  (R1),(7,0(R2)),2,FLOAT=$                                         
         CLI   0(R2),C' '                                                       
         BE    XIT                                                              
         SPACE 1                                                                
         L     R1,ACTARCPM         WAS CPM ALREADY CALCED                       
         LTR   R1,R1                                                            
         BZ    CPM20                                                            
         A     R1,=F'50'           ROUND TO DOLLAR                              
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         B     CPM30                                                            
CPM20    L     R1,ACBUDGET         TOO BIG SO COMPUTE TO $                      
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         M     R0,=F'2'                                                         
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
CPM30    EDIT  (R1),(7,0(R2)),FLOAT=$                                           
         B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
DEMOERR  MVC   CONHEAD(L'DEMERR),DEMERR                                         
         B     MYCURSOR                                                         
*                                                                               
DEMERR   DC    C'** ERROR ** DEMO NOT ON THE PLAN RECORD'                       
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYCURSOR MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VCURSERR            AND POSITIONING CURSOR                       
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPE6D                                                       
         SPACE 1                                                                
         DS    0D                                                               
ACCUMS   DS    CL160                                                            
REQLEN   DS    XL1                                                              
PROGKEY  DS    CL48                                                             
REVPER   DS    XL2                                                              
VPHLINK  DS    XL1                                                              
PGUAHLD  DS    XL4                                                              
DEMG     DS    XL1                 DEMO GUARANTEE FACTOR SWITCH                 
LOCALLEN EQU   *-ACCUMS                                                         
         SPACE 3                                                                
*              DSECT TO COVER ACCUMULATOR LINE                                  
         SPACE 1                                                                
ACCUMD   DSECT                                                                  
ACBUDGET DS    F                                                                
ACUNITS  DS    F                                                                
ACDONIND DS    F                                                                
ACSPARE  DS    F                                                                
ACTARGRP DS    F                                                                
ACTARIMP DS    F                                                                
ACTARCPM DS    F                                                                
ACTARGUA DS    F                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060NEPUP15   05/01/02'                                      
         END                                                                    
