*          DATA SET ACINF16    AT LEVEL 005 AS OF 01/24/97                      
*PHASE T60510A,*,NOAUTO                                                         
*INCLUDE CHOPPER                                                                
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE TX'                         
T60510   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE 'TX' IN ACCOUNTS INFO PROGRAM             
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60510)                                               
         DC    A(FILTABLE-T60510)                                               
         DC    A(PREHEADS-T60510)                                               
         DC    A(PRETABLE-T60510)                                               
         DC    A(HEADINGS-T60510)                                               
         DC    A(DATTABLE-T60510)                                               
         DC    A(KNTRYPNT-T60510)                                               
         DC    A(FNTRYPNT-T60510)                                               
         DC    A(DNTRYPNT-T60510)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'RECORDTYPE'                                                 
         DC    C' '                                                             
         DC    C' '                                                             
REC      DC    X'2D'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    XL2'FF00'                                                        
*                                                                               
         DC    CL10'SUBCOMPANY'                                                 
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'01'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LOCALITY'                                                   
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                 GREATER THAN A SPACE                       
         DC    AL1(3)                                                           
         DC    AL1(8)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    XL2'FF00'             DUMMY TO FORCE ENTRY INTO KNTRY            
*                                    TO REINIT FILTER START AND END             
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'CREDITACCT'                                                 
         DC    CL2'CR'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(14)                                                          
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'STARTDATE'                                                  
         DC    CL2'ST'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ENDDATE'                                                    
         DC    CL2'EN'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
*                                                                               
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
PREHEADS DC    CL39'                                       '                    
         DC    CL39'                                       '                    
*                                                                               
*              PRE-HEADING DATA TABLE -  9 BYTE ENTRIES                         
*                                                                               
*              CONTENTS AS FOR DISPLAY DATA TABLE BELOW                         
*                                                                               
PRETABLE DC    X'FF'                 END OF PRETABLE DATA                       
*                                                                               
*                                                                               
*              SCREEN HEADINGS - 2 LINES                                        
*                                                                               
*              CL39      C         FIRST LINE - SCREEN COLS  2-40               
*              CL39      C         2ND   LINE -    "     "     "                
*              CL39      C         FIRST LINE - SCREEN COLS 41-79               
*              CL39      C         2ND   LINE      "                            
*                                                                               
HEADINGS DC    CL39'------------ LOCALITY -------------    '                    
         DC    CL39'CODE         NAME                      '                    
         DC    CL39'EFFECTIVE     RATE      CREDIT         '                    
         DC    CL39'DATE          ----      ACCOUNT        '                    
*                                                                               
         EJECT                                                                  
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD                      
         SPACE 1                                                                
DATTABLE DC    AL2(AKEY-GWS)       LOCALITY CODE                                
         DC    AL2(3)              DISPLACEMENT OF CODE IN KEY                  
         DC    AL1(L'ACUTLOC)        LENGTH OF CODE                             
         DC    AL2(0)                                                           
         DC    AL1(2)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ANAM-GWS)         LOCALITY NAME                              
         DC    AL2(ACNMNAME-ACNAMED)                                            
         DC    AL1(L'ACNMNAME)                                                  
         DC    AL2(EDITNAME-GWS)                                                
         DC    AL1(15)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AKEY-GWS)         DUMMY FOR DATE, RATE, CRED ACC             
         DC    AL2(0)                                                           
         DC    AL1(39)               TOTAL LENGTH OF ABOVE                      
         DC    X'FF00'                                                          
         DC    AL1(39)               START COL                                  
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
         EJECT                                                                  
KNTRYPNT DS    0D                                                               
         NMOD1 0,*AIS10K*                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         LA    R6,BINTAB           SPACE TO BUILD SCREEN AND VARIBLES           
         USING LWSD,R6             DSECT TO COVER SCREEN AND VARIBLES           
         CLI   INITSW,C'Y'         INIT IN FILTERS                              
         BE    KEYX                YES                                          
         MVI   INITSW,C'Y'         NO, INIT HERE                                
         XC    CREDLEN,CREDLEN                                                  
         MVC   CREDACC(14),SPACES                                               
         MVC   STRTDTE(3),=XL3'000000' INIT START AND ENDDATE                   
         MVC   ENDDATE(3),=XL3'FFFFFF'                                          
KEYX     XIT1                                                                   
         SPACE 1                                                                
FNTRYPNT DS    0D                                                               
         NMOD1 0,*AIS10F*            VALIDATE FILTER DATES                      
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60510,RB                                                        
         USING T605TWA,RA                                                       
         USING  FILTERSD,R4                                                     
         LA    R6,BINTAB           SPACE TO BUILD SCREEN AND VARIBLES           
         USING LWSD,R6             DSECT TO COVER SCREEN AND VARIBLES           
         CLI   INITSW,C'Y'                                                      
         BE    F001                                                             
         MVI   INITSW,C'Y'                                                      
         XC    CREDLEN,CREDLEN                                                  
         MVC   CREDACC(14),SPACES                                               
         MVC   STRTDTE(3),=XL3'000000' INIT START AND ENDDATE                   
         MVC   ENDDATE(3),=XL3'FFFFFF'                                          
F001     SR    R5,R5                                                            
         L     R2,DMCB+4             ADDRESS OF FILTER VALUE                    
         CLI   0(R4),C'C'            IS THIS THE CREDIT ACCOUNT FILTER          
         BE    F05                                                              
         CLI   0(R4),C'S'            ADDRESS OF START DATE PASSED?              
         BE    F01                                                              
         CLI   0(R4),C'E'            ADDRESS OF END DATE PASSED?                
         BE    F02                                                              
         DC    H'0'                  WHAT WAS PASSED THEN                       
F01      LA    R5,STRTDTE                                                       
         B     F03                                                              
F02      LA    R5,ENDDATE                                                       
F03      GOTO1 VDATVAL,DMCB,(R2),WORK                                           
         OC    DMCB,DMCB                                                        
         BNZ   F04                                                              
         MVI   DMCB,1                                                           
         MVI   ERROR,DATERR                                                     
         B     FXIT                                                             
F04      GOTO1 VDATCON,(R1),WORK,(1,(R5))                                       
         B     FXIT                                                             
         SPACE 1                                                                
F05      EQU   *                   PROCESS CREDIT ACCOUNT FILTER                
         LA    R3,14               INIT COUNTER TO MAX LENGTH                   
         LA    R5,13(R2)           POINT TO END OF FILTER                       
F06      CLI   0(R5),C' '          LOOK FOR FIRST NON SPACE FROM END            
         BNE   F07                                                              
         BCTR  R5,0                                                             
         BCT   R3,F06                                                           
F07      STC   R3,CREDLEN                                                       
         MVC   CREDACC(14),0(R2)                                                
         SPACE 1                                                                
FXIT     EQU   *                                                                
         XIT1                                                                   
         DROP  R4                                                               
         SPACE 1                                                                
         EJECT                                                                  
DNTRYPNT DS    0D                    EDIT ACTAXRTE TO LOOK LIKE A %             
         NMOD1 0,*AIS10D*                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60510,RB                                                        
         USING T605TWA,RA                                                       
         LA    R6,BINTAB           SPACE TO BUILD SCREEN AND VARIBLES           
         USING LWSD,R6             DSECT TO COVER SCREEN AND VARIBLES           
         CLI   INITSW,C'Y'                                                      
         BE    D001                                                             
         MVI   INITSW,C'Y'                                                      
         XC    CREDLEN,CREDLEN                                                  
         MVC   CREDACC(14),SPACES                                               
         MVC   STRTDTE(3),=XL3'000000' INIT START AND ENDDATE                   
         MVC   ENDDATE(3),=XL3'FFFFFF'                                          
D001     LA    R2,DISPAREA         R2 = A(DISPLAY FIELD)                        
         LA    R3,DISPLEN          R3 = WIDTH OF DISPLAY FIELD                  
         SR    R4,R4               R4 = LENGTH MINUS 1 (INITIALLY '-1')         
         BCTR  R4,0                                                             
         LR    R5,R2               R5 = DISPLAY FIELD POINTER                   
         LH    R7,LINE             R7 = NUMBER OF LINES LEFT                    
         ICM   R9,15,ATAX          R9 = TAX ELEMENT POINTER                     
         USING ACTAXD,R9                                                        
         SPACE 1                                                                
         BZ    DEND1               NO TAX ELEMENTS (ICM SETS THE CC)            
         MVC   26(14,R2),SPACES                                                 
D20      MVC   0(25,R5),SPACES                                                  
         SPACE 1                                                                
         CLC   ACTAXEFF,STRTDTE   CHECK START DATE                              
         BL    D32                   DATE LOWER THAN FILTER                     
         CLC   ACTAXEFF,ENDDATE   CHECK END DATE                                
         BH    D32                                                              
         SPACE 1                                                                
         EDIT  (P4,ACTAXRTE),(8,14(R5)),4,DROP=3,ZERO=NONBLANK                  
         GOTO1 VDATCON,DMCB,(1,ACTAXEFF),(8,2(R5))                              
         SPACE 1                                                                
D30      LA    R5,DISPLEN(R5)                                                   
         LA    R4,1(R4)              NUMBER OF RATES THIS RECORD                
         SPACE 1                                                                
D32      CLI   ACTAXLEN,X'20'        IS CREDIT ACCOUNT NAME IN THIS EL          
         BNE   D33                   NO, GET NEXT EL                            
         ZIC   R1,CREDLEN            LENGTH OF CREDIT ACC FILTER PASSED         
         LTR   R1,R1                 ZERO, DON'T BOTHER WITH THIS               
         BZ    D32A                                                             
         BCTR  R1,0                  SUBTRACT 1 FOR THE MVC                     
         EX    R1,*+8                                                           
         B     *+10                  THIS ACCOUNT MATCH W/FILTER ACC            
         CLC   ACTAXACC(0),CREDACC                                              
         BE    D32A                  YES, US IT                                 
         SR    R4,R4                 NO, ZERO R4                                
         BCTR  R4,0                  THEN MAKE IT -1, THE ROOT WILL             
         B     DEND1                 IGNOR THE RECORD                           
D32A     MVC   26(14,R2),ACTAXACC                                               
         SPACE 1                                                                
D33      ZIC   R8,ACTAXLEN           GET NEXT TAX ELEMENT                       
         AR    R9,R8                                                            
         CLI   0(R9),X'5F'                                                      
         BE    D20                                                              
         CLI   0(R9),0                                                          
         BNE   D33                                                              
         SPACE 1                                                                
DEND1    EQU   *                                                                
         AR    R7,R4                 CURRENT LINE + NUMBER OF EXTRA             
         LA    R7,1(R7)              + ONE = LAST LINE NUMBER                   
         LH    R1,=H'20'             MAX LINES PER SCREEN                       
         CR    R7,R1                                                            
         BH    DEND2                                                            
         B     DXIT                                                             
         SPACE 1                                                                
DEND2    L     R1,ADRIO                                                         
         LA    R1,10(R1)           NO MORE ROOM ON PAGE, READ REC AGAIN         
         ZIC   RE,0(R1)            BY SUBTRACTING 1 FROM LAST SIGNFCNT          
         BCTR  RE,0                BYTE OF KEY (ROOT WILL REJECT REC)           
         STC   RE,0(R1)                                                         
         SPACE 1                                                                
DXIT     MVI   DMCB,0              SET FOR NO ERROR                             
*        MVI   INITSW,C'N'                                                      
         XIT1  REGS=(R2,R4)        RETURN ADDRESS,LENGTH,NO. EXTRA LNES         
         EJECT                                                                  
DISPLEN  EQU   39                  WIDTH OF ELEMENT DISPLAY                     
         LTORG                                                                  
         EJECT                                                                  
LWSD     DSECT                     LOCAL WORKING STORAGE DSECT                  
STRTDTE  DS    XL3                                                              
ENDDATE  DS    XL3                                                              
INITSW   DS    CL1                                                              
CREDACC  DS    CL14                                                             
CREDLEN  DS    CL1                 LENGTH OF FILTER REQUESTED                   
DISPAREA DS    CL195               39X5                                         
LWSLEN   EQU   *-LWSD              LENGTH OF THIS DSECT                         
         SPACE 1                                                                
*                                                                               
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACINF16   01/24/97'                                      
         END                                                                    
