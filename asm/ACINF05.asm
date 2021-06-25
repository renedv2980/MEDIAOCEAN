*          DATA SET ACINF05    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T60505A,*,NOAUTO                                                         
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE LE'                         
T60505   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE LEDGER   IN ACCOUNTS INFO PROGRAM         
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60505)                                               
         DC    A(FILTABLE-T60505)                                               
         DC    A(PREHEADS-T60505)                                               
         DC    A(PRETABLE-T60505)                                               
         DC    A(HEADINGS-T60505)                                               
         DC    A(DATTABLE-T60505)                                               
         DC    A(KNTRYPNT-T60505)                                               
         DC    A(FNTRYPNT-T60505)                                               
         DC    A(DNTRYPNT-T60505)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'UNIT'                                                       
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS+1)                                              
*&&UK*&& DC    AL2(0)                                                           
*&&US*&& DC    X'FF00'                                                          
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'DELETED'                                                    
         DC    CL2'DE'                                                          
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(ACSTATUS-ACKEYD)                                             
         DC    AL1(L'ACSTATUS)                                                  
         DC    AL2(0)                                                           
*                                                                               
*&&UK                                                                           
         DC    CL10'DATE'                                                       
         DC    CL2'DA'                                                          
         DC    CL8'TODAY'                                                       
         DC    X'80'                                                            
         DC    AL2(ALED-GWS)                                                    
         DC    AL2(ACLTSTAT-ACLEDGD)                                            
         DC    AL1(L'ACLTSTAT)                                                  
         DC    AL2(0)                                                           
*&&                                                                             
*                                                                               
         DC    CL10'GEN.LEDGER'                                                 
         DC    CL2'GE'                                                          
         DC    CL8'YES'                                                         
         DC    X'01'                                                            
         DC    AL2(0)                                                           
         DS    5C                                                               
*                                                                               
         DC    CL10'LIKE'                                                       
         DC    CL2'LI'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ALED-GWS)                                                    
         DC    AL2(ACLTLIKE-ACLEDGD)                                            
         DC    AL1(L'ACLTLIKE)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'PRINTCNTRL'                                                 
         DC    CL2'PC'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ALED-GWS)                                                    
         DC    AL2(ACLTPRNT-ACLEDGD)                                            
         DC    AL1(L'ACLTPRNT)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'TYPE'                                                       
         DC    CL2'TY'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ALED-GWS)                                                    
         DC    AL2(ACLTTYPE-ACLEDGD)                                            
         DC    AL1(L'ACLTTYPE)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
         EJECT                                                                  
*              PRE-HEADING LINE - TO CONTAIN CONSTANT ELEMENTS OF               
*                                 EXPRESSIONS IN FORM X=Y,A=B                   
*                                                                               
*              CL39      C                      SCREEN COLS  2-40               
*              CL39      C                      SCREEN COLS 41-79               
*                                                                               
PREHEADS DC    CL39'UNIT=                                  '                    
         DC    CL39'                                       '                    
*                                                                               
*              PRE-HEADING DATA TABLE -  9 BYTE ENTRIES                         
*                                                                               
*              CONTENTS AS FOR DISPLAY DATA TABLE BELOW                         
*                                                                               
PRETABLE DC    AL2(AKEY-GWS)       UNIT CODE                                    
         DC    AL2(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL1(7)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AUNITNAM-GWS)   UNIT NAME                                    
         DC    AL2(0)                                                           
         DC    AL1(36)                                                          
         DC    AL2(0)                                                           
         DC    AL1(9)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF PRE-HEADING DATA TABLE                
*                                                                               
*              SCREEN HEADINGS - 2 LINES                                        
*                                                                               
*              CL39      C         FIRST LINE - SCREEN COLS  2-40               
*              CL39      C         2ND   LINE -                                 
*              CL39      C         FIRST LINE - SCREEN COLS 41-79               
*              CL39      C         2ND   LINE                                   
*                                                                               
HEADINGS DC    CL39'---------LEDGER CODE AND NAME--------- '                    
         DC    CL39'                                       '                    
*&&UK                                                                           
         DC    CL39'LI P TY ACCOUNT STATUS    CHECK NUMBERS'                    
         DC    CL39'KE C PE LENGTHS ------    START     END'                    
*&&                                                                             
*&&US                                                                           
         DC    CL39'LI P TY ACCOUNT           CHECK NUMBERS'                    
         DC    CL39'KE C PE LENGTHS           START     END'                    
*&&                                                                             
*                                                                               
         EJECT                                                                  
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD                      
         SPACE 1                                                                
DATTABLE DC    AL2(AKEY-GWS)       LEDGER CODE                                  
         DC    AL2(2)                                                           
         DC    AL1(1)                                                           
*&&UK*&& DC    AL2(0)                                                           
*&&US*&& DC    X'FF00'                                                          
         DC    AL1(2)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ANAM-GWS)       LEDGER NAME                                  
         DC    AL2(ACNMNAME-ACNAMED)                                            
         DC    AL1(L'ACNMNAME)                                                  
         DC    AL2(EDITNAME-GWS)                                                
*&&UK*&& DC    AL1(4)                                                           
*&&US*&& DC    AL1(5)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ALED-GWS)       LIKE                                         
         DC    AL2(ACLTLIKE-ACLEDGD)                                            
         DC    AL1(L'ACLTLIKE)                                                  
         DC    AL2(0)                                                           
         DC    AL1(41)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ALED-GWS)       PRINT CONTROL                                
         DC    AL2(ACLTPRNT-ACLEDGD)                                            
         DC    AL1(L'ACLTPRNT)                                                  
         DC    AL2(0)                                                           
         DC    AL1(44)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ALED-GWS)       TYPE                                         
         DC    AL2(ACLTTYPE-ACLEDGD)                                            
         DC    AL1(L'ACLTTYPE)                                                  
         DC    AL2(0)                                                           
         DC    AL1(46)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AHIE-GWS)       HIERARCHY LENGTHS                            
         DC    AL2(ACHRLEVA-ACHEIRD)                                            
         DC    AL1(L'ACHRLEVA)                                                  
         DC    X'FF00'                                                          
         DC    AL1(49)                                                          
         DC    AL1(0)                                                           
*                                                                               
*&&UK                                                                           
         DC    AL2(ALED-GWS)                                                    
         DC    AL2(ACLTSTAT-ACLEDGD)                                            
         DC    AL1(L'ACLTSTAT)                                                  
         DC    X'FF00'                                                          
         DC    AL1(57)                                                          
         DC    AL1(0)                                                           
*&&                                                                             
*                                                                               
         DC    AL2(ACHK-GWS)       START NUMBER                                 
         DC    AL2(ACOFBEF-ACOFFD)                                              
         DC    AL1(L'ACOFBEF)                                                   
         DC    AL2(0)                                                           
         DC    AL1(67)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ACHK-GWS)       END NUMBER                                   
         DC    AL2(ACOFAFT-ACOFFD)                                              
         DC    AL1(L'ACOFAFT)                                                   
         DC    AL2(0)                                                           
         DC    AL1(74)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AKEY-GWS)       DUMMY FOR GEN.LEDGER=YES OPTION              
         DC    AL2(0)                                                           
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
         DC    AL1(40)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
         EJECT                                                                  
*              HANDLE LEDGER KEY INPUT (US ONLY)                                
         SPACE 1                                                                
KNTRYPNT DS    0D                                                               
         NMOD1 0,**ACKI**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60505,RB                                                        
         SPACE 1                                                                
         CLI   SCANBLCK,C'T'       SPECIAL FOR UNIT T                           
         BNE   KEXIT                                                            
         GOTO1 VHEXIN,DMCB,(R2),BINTAB,2 REQUIRE VALID HEX INPUT                
         OC    DMCB+12(4),DMCB+12                                               
         BZ    KERROR                                                           
         LA    R2,BINTAB                                                        
         LA    R3,1                                                             
         B     KEXIT                                                            
         SPACE 1                                                                
KERROR   MVI   DMCB,1                                                           
         MVI   ERROR,LEDGNVAL                                                   
         B     *+8                                                              
KEXIT    MVI   DMCB,0                                                           
         XIT1  REGS=(R2,R3)        RETURN ADDRESS & LENGTH                      
         EJECT                                                                  
FNTRYPNT DS    0H                                                               
DNTRYPNT DS    0D                                                               
         NMOD1 4,**LEDG**          EDIT HIERARCHY LENGTHS OR (UK ONLY)          
*                                  STATUS OR HANDLE GE=YES OPTION               
*                                  OR (US ONLY) HANDLE HEX LEDGERS              
         USING MYWORK,RC         RC = LOCAL W/S                                 
         L     R8,0(R1)                                                         
         USING GWS,R8              R8 = GLOBAL W/S                              
         L     RB,APHASE                                                        
         USING T60505,RB                                                        
         USING T605TWA,RA                                                       
         L     R9,AHIE                                                          
         USING ACHEIRD,R9                                                       
         MVC   BINTAB(40),SPACES                                                
         LA    R0,ACHRLEVA         THIS IS HIERARCHY                            
         CR    R2,R0                                                            
         BE    T6050                                                            
         L     R9,AKEY             THIS IS LEDGER DISPLAY                       
         LA    R9,2(R9)                                                         
         CR    R2,R9                                                            
         BE    T605D                                                            
         L     R0,ADRIO            THIS IS GE=YES                               
         CR    R2,R0                                                            
         BE    T6054                                                            
         B     T6053                                                            
         SPACE 1                                                                
T6050    DS    0H                  HIERARCHY                                    
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         SR    R3,R3                                                            
         CLI   ACHRLEVA,0                                                       
         BE    T6052                                                            
         IC    R3,ACHRLEVA                                                      
         MVC   UNSCANBK(80),SPACES                                              
         LA    R5,UNSCANBK                                                      
         BAS   RE,EDITHIER                                                      
         CLI   ACHRLEVB,0                                                       
         BE    T6051                                                            
         IC    R3,ACHRLEVB                                                      
         BAS   RE,EDITHIER                                                      
         CLI   ACHRLEVC,0                                                       
         BE    T6051                                                            
         IC    R3,ACHRLEVC                                                      
         BAS   RE,EDITHIER                                                      
         CLI   ACHRLEVD,0                                                       
         BE    T6051                                                            
         IC    R3,ACHRLEVD                                                      
         BAS   RE,EDITHIER                                                      
T6051    GOTO1 VUNSCAN,DMCB,((R6),UNSCANBK),(C'C',BINTAB)                       
T6052    LA    R3,7                R3 = LENGTH                                  
         B     T605X                                                            
*                                                                               
EDITHIER NTR1                                                                   
         LR    R4,R3                                                            
         SR    R3,R7                                                            
         LR    R7,R4                                                            
         EDIT  (R3),(2,(R5)),ALIGN=LEFT,ZERO=NOBLANK,WRK=MYWORK                 
         LA    R6,1(R6)                                                         
         LA    R5,20(R5)                                                        
         XIT1  REGS=(R5,R7)                                                     
         EJECT                                                                  
T6053    TM    0(R2),X'80'         STATUS (UK ONLY)                             
         BNO   T605X                                                            
         MVC   BINTAB(9),=CL9'DTE=TODAY'                                        
         LA    R3,9                R3 = LENGTH                                  
         B     T605X                                                            
         EJECT                                                                  
*              HANDLE GE.LEDGER=YES OPTION                                      
         SPACE 1                                                                
T6054    TM    OPTIONS,1                                                        
         BNO   T605X                                                            
         LA    R3,40               R3 = DISPLAY FIELD WIDTH                     
         SR    R4,R4               R4 = NUMBER OF ADDITIONAL LINES              
         SPACE 1                                                                
T6055    DS    0H                  MODIFY HEADINGS                              
         MVC   INFDAT3+39(39),SPACES                                            
         MVC   INFDAT4+39(39),SPACES                                            
         MVC   INFDAT3+39(23),=C'GENERAL LEDGER ACCOUNTS'                       
         MVI   INFDAT4+39,C'-'                                                  
         MVC   INFDAT4+40(22),INFDAT4+39                                        
         SPACE 1                                                                
T6056    ICM   RA,15,AGEN          PREPARE FOR LOOP TO SET UP AN                
         BZ    T605X               'ACGLSUB=ACGLACC' ENTRY IN OUTPUT            
         USING ACGENLD,RA          AREA                                         
         SR    R0,R0                                                            
         LA    R7,BINTAB+1         R7 = OUTPUT AREA POINTER                     
         LA    R9,39               R9 = NO OF BYTES LEFT IN THIS LINE           
         B     T6058                                                            
         SPACE 1                                                                
T6057    IC    R0,1(RA)            BUMP ELEMENT                                 
         AR    RA,R0                                                            
         CLI   0(RA),0                                                          
         BE    T605X                                                            
         CLI   0(RA),X'15'                                                      
         BNE   T6057                                                            
         SPACE 1                                                                
T6058    MVC   MYWORK(25),SPACES   SET UP ENTRY IN MYWORK WITH ITS              
         CLC   ACGLSUB,SPACES      SIGNIFICANT LENGTH IN R1                     
         BNE   T60582                                                           
         LA    R1,7                                                             
         MVC   MYWORK(7),=C'DEFAULT'                                            
         B     T60584                                                           
T60582   LA    R1,L'ACGLSUB                                                     
         LA    R2,ACGLSUB-1(R1)                                                 
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-12                                                          
         MVC   MYWORK(L'ACGLSUB),ACGLSUB                                        
T60584   LA    R5,MYWORK(R1)                                                    
         MVI   0(R5),C'='                                                       
         LA    R1,1(R1)                                                         
         LA    R5,1(R5)                                                         
         ZIC   R6,ACGLLEN                                                       
         SH    R6,=H'13'                                                        
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ACGLACC                                                  
         LA    R6,1(R6)                                                         
         LA    R2,ACGLACC-1(R6)                                                 
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-12                                                          
         AR    R1,R6                                                            
         SPACE 1                                                                
T6059    CR    R1,R9               IF NO ROOM ON THIS LINE, MOVE TO THE         
         BL    T605A               NEXT                                         
         AR    R7,R9                                                            
         MVC   0(40,R7),SPACES                                                  
         LA    R7,1(R7)                                                         
         LA    R9,39                                                            
         LA    R4,1(R4)            INCREMENT NUMBER OF EXTRA LINES              
         SPACE 1                                                                
T605A    CH    R9,=H'39'           IF NOT START OF LINE INSERT COMMA            
         BE    T605B                                                            
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
         BCTR  R9,0                                                             
         SPACE 1                                                                
T605B    SR    R9,R1               MOVE THE ENTRY INTO THE LINE                 
         MVC   0(25,R7),MYWORK                                                  
         AR    R7,R1                                                            
         B     T6057                                                            
         EJECT                                                                  
*              HANDLE LEDGER DISPLAY (US ONLY)                                  
         SPACE 1                                                                
T605D    DS    0H                                                               
         MVC   BINTAB(1),0(R2)       SET TO DISPLAY LEDGER                      
         BCTR  R9,0                                                             
         CLI   0(R9),C'T'          SPECIAL FOR UNIT T                           
         BNE   T605X                                                            
         GOTO1 VHEXOUT,DMCB,(R2),BINTAB,1,=C'TOG' DISPLAY IN HEX FORM           
         LA    R3,2                                                             
         SPACE 1                                                                
T605X    LA    R2,BINTAB                                                        
         MVI   DMCB,0                                                           
         XIT1  REGS=(R2,R4)                                                     
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
MYWORK   DSECT                                                                  
         DS    CL32                                                             
*                                                                               
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACINF05   05/01/02'                                      
         END                                                                    
