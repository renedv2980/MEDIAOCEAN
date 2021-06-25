*          DATA SET NEPUP07    AT LEVEL 022 AS OF 05/01/02                      
*          DATA SET NEPUP07    AT LEVEL 023 AS OF 04/17/89                      
*PHASE T32207A,*                                                                
         TITLE 'T32207 - PLAN COMPARE'                                          
T32207   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32207**,RA                                                    
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
         BAS   RE,COMP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FOR COMPARE                                         
         SPACE 3                                                                
VKEY     NTR1                                                                   
         SPACE 1                                                                
         LA    R2,PUPCLIH          CLIENT (REQUIRED)                            
         GOTO1 VVALCLT                                                          
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,PUPQRTH          QUARTER (OPTIONAL)                           
         MVI   REQQRT,0                                                         
         CLI   5(R2),0                                                          
         BE    VKEY2                                                            
         GOTO1 VALINUM                                                          
         MVC   REQQRT,ACTUAL                                                    
         CLI   REQQRT,0            S/B 1-4                                      
         BE    BADQUART                                                         
         CLI   REQQRT,4                                                         
         BH    BADQUART                                                         
         SPACE 1                                                                
VKEY2    LA    R2,PUPLENH          LENGTH (OPTIONAL)                            
         MVI   REQLEN,0                                                         
         CLI   5(R2),0                                                          
         BE    VKEY4                                                            
         GOTO1 VALINUM                                                          
         MVC   REQLEN,ACTUAL                                                    
         SPACE 1                                                                
VKEY4    LA    R2,PUPNETH          UP TO 6 PLANS                                
         MVI   NUMPLANS,0                                                       
         GOTO1 ANY                 (AT LEAST 1 IS REQUIRED)                     
         LA    R3,6                                                             
         XC    LASTNET,LASTNET                                                  
         XC    LASTDPT,LASTDPT                                                  
         XC    LASTPLAN,LASTPLAN                                                
         SPACE 1                                                                
VKEY6    ST    R2,ATHISNET         CHECK FOR ANY INPUT                          
         CLI   5(R2),0                                                          
         BNE   VKEY7                                                            
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BNE   VKEY7                                                            
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BE    VKEY8                                                            
         SPACE 1                                                                
VKEY7    L     R2,ATHISNET                                                      
         CLI   5(R2),0             NETWORK                                      
         BNE   *+10                                                             
         MVC   8(4,R2),LASTNET                                                  
         OC    8(4,R2),SPACES                                                   
         MVI   5(R2),4                                                          
         MVC   LASTNET,8(R2)                                                    
         GOTO1 VVALNET                                                          
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
         CLI   5(R2),0             DAYPART                                      
**       BNE   *+10                                                             
         BNE   VKEY7D              PXZ CHANGE                                   
         MVC   8(1,R2),LASTDPT                                                  
         OC    8(1,R2),SPACES                                                   
         MVI   5(R2),1                                                          
         MVC   LASTDPT,8(R2)                                                    
VKEY7D   GOTO1 VVALDPT                                                          
         MVC   8(1,R2),DPTNAME                                                  
         MVC   LASTDPT,DPTNAME     PXZ CHANGE                                   
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
         CLI   5(R2),0             PLAN                                         
         BNE   *+10                                                             
         MVC   8(4,R2),LASTPLAN                                                 
         OC    8(4,R2),SPACES                                                   
         MVI   5(R2),4                                                          
         MVC   LASTPLAN,8(R2)                                                   
         GOTO1 VVALPLAN                                                         
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         AI    NUMPLANS,1                                                       
         BCT   R3,VKEY6                                                         
         B     XIT                                                              
         SPACE 1                                                                
VKEY8    CLI   8(R2),C' '          CLEAR OUT REMAINING FIELDS                   
         BNH   XIT                                                              
         MVC   8(4,R2),SPACES                                                   
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         MVC   8(4,R2),SPACES                                                   
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         MVC   8(4,R2),SPACES                                                   
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         MVC   8(59,R2),SPACES                                                  
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         BCT   R3,VKEY8                                                         
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL PLAN COMPARISON                                          
         SPACE 3                                                                
COMP     NTR1                                                                   
         MVI   GDRNDOPT,C'Y'                                                    
         LA    R2,PUPNETH          UP TO 6 PLANS                                
         ZIC   R3,NUMPLANS                                                      
         XC    TOTALS,TOTALS                                                    
         SPACE 1                                                                
COMP2    GOTO1 VVALNET             REEDIT NETWORK                               
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
         GOTO1 VVALDPT             DAYPART                                      
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
         GOTO1 VVALPLAN            PLAN                                         
         XC    ACCUMS,ACCUMS                                                    
         BAS   RE,PLANIO                                                        
         BAS   RE,BUMP                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(59,R2),SPACES                                                  
         BAS   RE,SHOWDEM                                                       
         BAS   RE,SHOWLINE                                                      
         BAS   RE,ADDEM                                                         
         BAS   RE,BUMP                                                          
         BCT   R3,COMP2                                                         
         SPACE 1                                                                
         LA    R2,PUPAVEH          SHOW AVERAGES                                
         CLI   NUMPLANS,1          IF MORE THAN 1 PLAN                          
         BE    XIT                                                              
         MVC   8(7,R2),=C'AVERAGE'                                              
         OI    6(R2),X'80'                                                      
         MVC   ACCUMS,TOTALS                                                    
         BAS   RE,AVERAGE                                                       
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(59,R2),SPACES                                                  
         BAS   RE,SHOWLINE                                                      
         SPACE 1                                                                
         LA    R2,PUPTOTH          SHOW TOTALS AS WELL                          
         MVC   8(6,R2),=C'TOTALS'                                               
         OI    6(R2),X'80'                                                      
         MVC   ACCUMS,TOTALS                                                    
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(59,R2),SPACES                                                  
         BAS   RE,SHOWLINE                                                      
         B     XIT                                                              
         EJECT                                                                  
*              HANDLE I/O FOR PLAN                                              
         SPACE 3                                                                
PLANIO   NTR1                                                                   
         BAS   RE,POSTBUDG                                                      
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
         B     PLANIO6                                                          
         SPACE 1                                                                
PLANIO4  GOTO1 SEQ                                                              
         SPACE 1                                                                
PLANIO6  CLC   KEY(13),KEYSAVE     MUST HAVE MATCH ON PLAN                      
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
         GOTO1 VEXTPROG                                                         
         BAS   RE,POSTPROG                                                      
         B     PLANIO4                                                          
         EJECT                                                                  
*              ROUTINE TO POST BUDGETS FROM PLAN                                
         SPACE 3                                                                
POSTBUDG NTR1                                                                   
         MVC   LENGTH,REQLEN                                                    
         GOTO1 VEXTBUDG            GET BUDGETS (MAYBE FOR LENGTH)               
         LA    R3,ACCUMS                                                        
         USING ACCUMD,R3                                                        
         CLI   REQQRT,0                                                         
         BNE   PBUDG4                                                           
         LA    R2,BUDGETS                                                       
         LA    R0,4                                                             
         SPACE 1                                                                
PBUDG2   L     R1,0(R2)            ADD BUDGETS INTO ACCUMS                      
         A     R1,ACBUDGET                                                      
         ST    R1,ACBUDGET                                                      
         LA    R2,20(R2)                                                        
         BCT   R0,PBUDG2                                                        
         B     XIT                                                              
         SPACE 1                                                                
PBUDG4   ZIC   R1,REQQRT           NEED BUDGET FOR 1 QUARTER                    
         CLI   REQQRT,4                                                         
         BNE   *+6                                                              
         SR    R1,R1                                                            
         MH    R1,=H'20'           DISPLACE TO BUDGET FOR QUARTER               
         LA    R1,BUDGETS(R1)                                                   
         MVC   ACBUDGET,0(R1)                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST PROGRAM DETAILS                                  
         SPACE 3                                                                
POSTPROG NTR1                                                                   
         LA    R2,PLANPLST         SET UP FOR QUARTERLY                         
         LA    R3,ACCUMS                                                        
         USING ACCUMD,R3                                                        
         ZIC   R0,PLANNPER                                                      
         SPACE 1                                                                
PPROG2   CLI   REQQRT,0            IF QUARTER WAS REQUESTED                     
         BE    PPROG4                                                           
         CLC   REQQRT,2(R2)        THEN IT MUST MATCH TABLE                     
         BE    PPROG4                                                           
         CLI   REQQRT,4            FOURTH QUARTER IS REL 0                      
         BNE   PPROG6                                                           
         CLI   2(R2),0                                                          
         BNE   PPROG6                                                           
         SPACE 1                                                                
PPROG4   MVC   PERIOD,0(R2)                                                     
         MVC   LENGTH,REQLEN                                                    
         GOTO1 VEXTUNS                                                          
         ZIC   R1,UNITS            ADD IN UNITS                                 
         A     R1,ACUNITS                                                       
         ST    R1,ACUNITS                                                       
         MVC   GDDEMO,=X'000001'      GO FOR HOMES FIRST                        
**       MVI   GDDEMO,1            GO FOR HOMES FIRST                           
         MVC   LENGTH,REQLEN                                                    
         GOTO1 VGETDEM                                                          
         L     R1,GDTGRP           HOMES GRPS                                   
         A     R1,ACHOMGRP                                                      
         ST    R1,ACHOMGRP                                                      
         L     R1,GDTIMP           HOMES (000)                                  
         A     R1,ACHOMIMP                                                      
         ST    R1,ACHOMIMP                                                      
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
PPROG6   LA    R2,4(R2)                                                         
         BCT   R0,PPROG2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              NOW ADD EACH PLAN INTO BOTTOM LINE TOTAL                         
         SPACE 3                                                                
ADDEM    NTR1                                                                   
         LA    R2,ACCUMS           ADD DETAILS INTO TOTALS                      
         LA    R3,TOTALS                                                        
         LA    R0,6                ADD A LINE                                   
         SPACE 1                                                                
ADDEM2   L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,ADDEM2                                                        
         B     XIT                                                              
         SPACE 1                                                                
AVERAGE  NTR1                                                                   
         LA    R2,ACCUMS           AVERAGE ACCUMS                               
         ZIC   R3,NUMPLANS                                                      
         LA    R4,6                                                             
         SPACE 1                                                                
AVE2     L     R0,0(R2)                                                         
         SRDA  R0,31                                                            
         DR    R0,R3                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R4,AVE2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              SHOW TARGET AND GUARANTEE                                        
         SPACE 3                                                                
SHOWDEM  NTR1                                                                   
         LA    R2,8(R2)            GET PAST HEADER                              
         LA    R2,35(R2)           AND POSITION TO TARGET                       
         MVC   1(10,R2),TARGNAME                                                
         SPACE 1                                                                
*        LA    R2,11(R2)                                                        
         LA    R2,10(R2)                                                        
         OC    GUARCPM,GUARCPM                                                  
         BZ    XIT                                                              
         EDIT  (4,GUARCPM),(6,0(R2)),2,FLOAT=$                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EDIT A LINE OF NUMBERS                                
         SPACE 3                                                                
SHOWLINE NTR1                                                                   
         LA    R2,8(R2)            GET PAST HEADER                              
         LA    R3,ACCUMS                                                        
         USING ACCUMD,R3                                                        
*                                  SHOW BUDGET                                  
         EDIT  (4,ACBUDGET),(9,0(R2))                                           
         LA    R2,9(R2)            UNITS                                        
         EDIT  (4,ACUNITS),(5,0(R2))                                            
         SPACE 1                                                                
SLINE2   LA    R2,5(R2)            HOMES POINTS                                 
         EDIT  (4,ACHOMGRP),(6,0(R2)),1,ZERO=BLANK                              
         CLI   0(R2),C' '                                                       
         BE    SLINE4                                                           
         L     R1,ACHOMGRP                                                      
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(6,0(R2))                                                   
         SPACE 1                                                                
SLINE4   LA    R2,7(R2)            HOMES CPP                                    
         L     R1,ACBUDGET                                                      
         LTR   R1,R1                                                            
         BZ    SLINE6                                                           
         M     R0,=F'20'                                                        
         OC    ACHOMGRP,ACHOMGRP                                                
         BZ    SLINE6                                                           
         D     R0,ACHOMGRP                                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(6,0(R2)),FLOAT=$                                           
         CLI   0(R2),C'$'                                                       
         BNE   SLINE6                                                           
         MVI   0(R2),C' '                                                       
         SPACE 1                                                                
SLINE6   LA    R2,7(R2)            HOMES CPM                                    
         MVC   FULL,ACHOMIMP                                                    
         LA    R4,ACHOMIMP                                                      
         BAS   RE,ROUND                                                         
         BAS   RE,CPM                                                           
         SPACE 1                                                                
SLINE8   LA    R2,24(R2)                                                        
         LA    R4,ACTARIMP                                                      
         BAS   RE,CPM                                                           
         B     XIT                                                              
         EJECT                                                                  
ROUND    NTR1                                                                   
         L     R1,FULL                                                          
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
         ST    R1,FULL                                    10000)                
         B     XIT                                                              
         SPACE 1                                                                
ROUND2   AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         ST    R1,FULL                                                          
         B     XIT                                                              
         EJECT                                                                  
*              SUB-ROUTINE TO SHOW CPM                                          
         SPACE 3                                                                
*              INPUTS              R2=A(OUTPUT AREA)                            
*                                  R3=A(ACCUMS)                                 
*                                  R4=A(IMPS)                                   
         SPACE 1                                                                
         USING ACCUMD,R3                                                        
CPM      NTR1                                                                   
         L     R1,0(R4)            PICK UP IMPS                                 
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         SPACE 1                                                                
*                                  COMPUTE CPM IN PENNIES                       
         L     R1,ACBUDGET                                                      
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         M     R0,=F'2000'                                                      
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,0(R2)),2,FLOAT=$                                         
         CLI   0(R2),C' '                                                       
         BE    XIT                                                              
         SPACE 1                                                                
         L     R1,ACBUDGET         TOO BIG SO COMPUTE TO $                      
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         M     R0,=F'20'                                                        
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,0(R2)),FLOAT=$                                           
         B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYCURSOR MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VCURSERR            AND POSITIONING CURSOR                       
         SPACE 1                                                                
BADQUART MVC   CONHEAD(L'MSGQUART),MSGQUART                                     
         B     MYEND                                                            
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
QLIST    DC    AL1(4,1,4,1,2,1,2,2,2,3,2,3)                                     
MLIST    DC    AL1(9,1,4,10,1,4,11,1,4,12,1,4)                                  
         DC    AL1(1,2,1,2,2,1,3,2,1)                                           
         DC    AL1(4,2,2,5,2,2,6,2,2)                                           
         DC    AL1(7,2,3,8,2,3,9,2,3)                                           
         SPACE 1                                                                
MSGQUART DC    C'** ERROR ** QUARTER MUST BE 1-4'                               
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPF7D                                                       
         SPACE 1                                                                
         DS    0D                                                               
ACCUMS   DS    CL24                                                             
TOTALS   DS    CL24                                                             
REQLEN   DS    XL1                                                              
REQQRT   DS    XL1                                                              
NUMPLANS DS    XL1                                                              
ATHISNET DS    F                                                                
LASTNET  DS    CL4                                                              
LASTDPT  DS    CL7                                                              
LASTPLAN DS    CL4                                                              
         SPACE 3                                                                
*              DSECT TO COVER ACCUMULATOR LINE                                  
         SPACE 1                                                                
ACCUMD   DSECT                                                                  
ACBUDGET DS    F                                                                
ACUNITS  DS    F                                                                
ACHOMGRP DS    F                                                                
ACHOMIMP DS    F                                                                
ACTARGRP DS    F                                                                
ACTARIMP DS    F                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022NEPUP07   05/01/02'                                      
         END                                                                    
