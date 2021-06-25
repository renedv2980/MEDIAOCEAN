*          DATA SET ACBUD01    AT LEVEL 015 AS OF 08/05/08                      
*                                                                               
*PHASE T61001A,*                                                                
         TITLE 'ACBUD01 - BUDGET PROGRAM - SCREEN FORMATTING'                   
ACBUD01  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BUD1**,RA,RR=RE                                              
*                                                                               
         USING GWS,R9                                                           
         USING TWAD,R8                                                          
*                                                                               
         EJECT ,                                                                
         SPACE 1                                                                
FRMF02   XC    FRMINITA,FRMINITA   INITIALISE SAVE AREAS                        
         XC    BUDRULES,BUDRULES                                                
         XC    ACCNTRL,ACCNTRL                                                  
         XC    CACNTRL,CACNTRL                                                  
         XC    OFFILT,OFFILT                                                    
         NI    SAVSTAT,TOTAL                                                    
         LA    R1,BUDACCH          CLEAR VALIDATED PREVIOUSLY BITS IN           
         SR    R2,R2               THIS SECTION OF SCREEN                       
         LA    R3,BUDTYP4H                                                      
         NI    4(R1),ALL-VALPREV                                                
         IC    R2,0(,R1)                                                        
         BXLE  R1,R2,*-8                                                        
         XC    BUDACCN,BUDACCN     CLEAR NAMES                                  
         XC    BUDCACN,BUDCACN                                                  
         OI    BUDACCNH+6,TRANSMIT                                              
         OI    BUDCACNH+6,TRANSMIT                                              
         MVI   ACLEVEL,X'FF'       INITIALISE LEVELS                            
         MVI   CALEVEL,X'FF'                                                    
         MVI   NEXTMODE,FRMFIRST                                                
*                                                                               
FRMF04   GOTO1 AFVAL,BUDACCH       VALIDATE ACCOUNT                             
         BZ    ERROR               MISSING                                      
         LA    R0,SCANRHSL                                                      
         GOTO1 VSCANNER,DMCB,((R0),FLDH),(3,TEMP)                               
         MVC   FNDX,4(R1)                                                       
         MVI   FERN,INVALID                                                     
         CLI   FNDX,2              UP TO 2 ENTRIES, 2ND MUST BE NEXT            
         BH    ERROR               OR OFFICE=X                                  
         BL    FRMF06              BOTH OF WHICH IMPLY MULTIPLE A/CS            
         LLC   RE,TEMP+22+SCANRHSL                                              
         BCTR  RE,0                                                             
         EX    RE,OFFICLC                                                       
         BNE   FRMF05                                                           
         TM    COMPSTA4,X'01'      TEST NEW OFFICES                             
         BO    ERROR               YES, MUST USE OFFICE FIELD                   
                                                                                
         CLI   TEMP+23+SCANRHSL,1                                               
         BNE   ERROR                                                            
         MVC   OFFILT(1),TEMP+44+SCANRHSL                                       
         B     FRMF05A                                                          
*                                                                               
FRMF05   EX    RE,NEXTCLC                                                       
         BNE   ERROR                                                            
*                                                                               
FRMF05A  MVI   FNDX,1                                                           
         B     FRMF07                                                           
*                                                                               
OFFICLC  CLC   TEMP+34+SCANRHSL(0),=C'OFFICE'                                   
*                                                                               
NEXTCLC  CLC   TEMP+34+SCANRHSL(0),=C'NEXT'                                     
*                                                                               
FRMF06   MVI   FNDX,0              IF 1ST ENTRY HAS LHS,MUST BE 'LEDG'          
*                                                                               
FRMF07   CLI   TEMP+1,0                                                         
         BE    FRMF08                                                           
         LLC   RE,TEMP                                                          
         SH    RE,=H'1'                                                         
         BM    ERROR                                                            
         EXCLC RE,TEMP+12,=C'LEDGER'                                            
         BNE   ERROR                                                            
         CLI   TEMP+1,2                                                         
         BNE   ERROR                                                            
         MVC   TEMP(1),TEMP+1      REMOVE KEYWORD LHS FROM TAB ENTRY            
         MVC   TEMP+12(SCANRHSL),TEMP+22                                        
*                                                                               
FRMF08   DS    0H                                                               
***      CLC   TEMP+12(2),=C'SJ'   PROD LEDGER INVALID                          
***      BE    ERROR                                                            
         MVC   WORK1(1),COMPANY                                                 
         MVC   WORK1+1(15),TEMP+12                                              
         LA    R7,ACCNTRL                                                       
*                                                                               
         USING IOCBD,R7                                                         
*                                                                               
         GOTO1 ACHECKUL,DMCB,WORK1+1,(R7)                                       
         BNZ   ERROR                                                            
*                                                                               
FRMF10   CLI   TEMP,2              CHECK ACCOUNT                                
         BE    FRMF15              (LEDGER ONLY)                                
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),WORK1                                                    
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   ERROR               DOES NOT EXIST                               
         GOTO1 ASECHECK,DMCB,AIOAREA1,(R7)                                      
         BNZ   ERROR               SECURITY TOO HIGH                            
         MVC   ACLEVEL,0(R1)                                                    
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   BUDACCN,WORK                                                     
*                                                                               
FRMF15   LA    R2,IOCLOKEY         BUILD I/O CONTROL TABLE ENTRY                
         SR    R3,R3               R3 = IOCDS15                                 
         CLI   ACTION,ADD                                                       
         BE    FRMF17                                                           
         MVI   IOCLOKEY,ACBTKTEQ   IF ACTION IS NOT ADD - ACCOUNT READS         
         LA    R2,1(,R2)           ARE DRIVEN BY BUDGET RECORDS                 
         LA    R3,1                                                             
*                                                                               
FRMF17   MVC   0(16,R2),WORK1                                                   
         MVC   IOCHIKEY,IOCLOKEY                                                
         STC   R3,IOCDS15                                                       
         CLI   FNDX,0              IF ONLY LEDGER CODE GIVEN OR ',NEXT'         
         BNE   *+16                OR ',OFFICE=X'                               
         CLI   TEMP,2              SET HIGH KEY ACCOUNT TO END OF LEDGR         
         BNE   FRMF20                                                           
         MVI   3(R2),X'41'         IF LEDGER SET LOW FOR 1ST ACC                
         LA    R1,IOCHIKEY+3                                                    
         CLI   IOCHIKEY,ACBTKTEQ                                                
         BNE   *+8                                                              
         LA    R1,1(,R1)                                                        
         MVI   0(R1),X'FF'                                                      
*                                                                               
FRMF20   XC    TEMP,TEMP           VALIDATE CONTRA                              
         OI    BUDACCH+4,VALPREV                                                
         L     R5,AIOAREA1         R5 = A(RECORD)                               
*                                                                               
         USING ACKEYD,R5                                                        
*                                                                               
         XC    ACKEYD(42),ACKEYD                                                
         LA    R7,CACNTRL                                                       
         LA    R6,CATABNUL         R6 = CONTRA TAB POINTER                      
         GOTO1 AFVAL,BUDCACH                                                    
         BZ    FRMF45              MAY BE MISSING                               
*                                                                               
FRMF22   LA    R0,SCANRHSL                                                      
         GOTO1 VSCANNER,DMCB,((R0),FLDH),(3,TEMP)                               
         MVC   FNDX,4(R1)                                                       
         MVI   FERN,INVALID                                                     
         CLI   FNDX,2              UP TO 2 ENTRIES                              
         BH    ERROR                                                            
         MVI   FNDX,0                                                           
         BL    *+8                                                              
         MVI   FNDX,1                                                           
*                                                                               
FRMF24   CLI   TEMP+1,0            NO KEYWORD MEANS 'ALL' OR SINGLE CAC         
         BNE   FRMF30              MOVE VALUE TO RHS OF SCANNER ENTRY           
         CLI   TEMP,1              AND POINT TO APPROPRIATE CONTRA TAB          
         BNH   ERROR               ENTRY. MUST BE MORE THAN 1 CHARACTER         
         MVC   TEMP+1(1),TEMP                                                   
         MVC   WORK(SCANRHSL),TEMP+12                                           
         MVC   TEMP+22(SCANRHSL),WORK                                           
         LA    R6,CATABONE                                                      
         CLI   TEMP+1,3                                                         
         BNE   FRMF36                                                           
         CLC   TEMP+22(3),=C'ALL'                                               
         BNE   FRMF36                                                           
         MVC   ACKEYD(42),SPACES                                                
         MVC   IOCLCODE,SPACES                                                  
         LA    R6,CATABALL                                                      
         B     FRMF45                                                           
*                                                                               
         USING CATBD,R6                                                         
*                                                                               
FRMF30   LA    R6,CATAB            CHECK KEYWORD                                
         LLC   RE,TEMP                                                          
         SH    RE,=H'1'                                                         
         BM    ERROR                                                            
*                                                                               
FRMF32   CLI   0(R6),0                                                          
         BE    ERROR                                                            
         EXCLC RE,CATBKEYW,TEMP+12                                              
         BE    FRMF34                                                           
         LA    R6,L'CATAB(,R6)                                                  
         B     FRMF32                                                           
*                                                                               
FRMF34   TM    CATBSTAT,X'80'      IS IT COMPATIBLE WITH ACTION                 
         BZ    FRMF36                                                           
         CLI   ACTION,ADD                                                       
         BE    FRMF36                                                           
         MVI   FERN,INVNTADD                                                    
         B     ERROR                                                            
*                                                                               
FRMF36   TM    CATBSTAT,X'40'      CHECK LEDGER                                 
         BO    *+14                                                             
         XC    IOCLCODE(6),IOCLCODE                                             
         B     FRMF38                                                           
*                                                                               
         GOTO1 ACHECKUL,DMCB,TEMP+22,(R7)                                       
         BE    *+12                                                             
         CLI   CATBKEYW,0          SINGLE CONTRA NEEDNT HAVE ONE                
         BNZ   ERROR                                                            
         CLI   CATBKEYW,C'L'       LEDGER                                       
         BNE   FRMF38                                                           
         MVC   ACKEYD(42),SPACES                                                
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),TEMP+22                                            
         CLI   TEMP+1,2                                                         
         BE    FRMF45                                                           
         MVI   FERN,LEDGNVAL                                                    
         B     ERROR                                                            
*                                                                               
FRMF38   TM    CATBSTAT,X'20'      IS ACC/CAC/BUD REC READ REQUIRED             
         BNO   FRMF45                                                           
         CLI   CATBKEYW,0          UNLESS SINGLE CODE, MUST BE MORE             
         BE    FRMF38A             THAN UL                                      
         MVI   FERN,INVALID                                                     
         CLI   TEMP+1,2                                                         
         BNH   ERROR                                                            
*                                                                               
FRMF38A  XC    KEY,KEY                                                          
         LA    R1,AIOAREA1                                                      
         LA    R2,KEY                                                           
         CLI   CATBKEYW,C'B'       BUDGET                                       
         BNE   *+16                                                             
         MVI   KEY,ACBTKTEQ                                                     
         LA    R2,1(,R2)                                                        
         LA    R1,ABUDREC                                                       
         MVC   0(1,R2),COMPANY                                                  
         MVC   1(14,R2),TEMP+22                                                 
         CLI   CATBKEYW,C'C'       IF CONTRA FORCE TO CONTRA HDR                
         BNE   *+8                                                              
         MVI   KEY+17,X'41'                                                     
         GOTO1 ARDHI                                                            
*                                                                               
FRMF39   TM    DMCB+8,X'FD'        SKIP DELETES                                 
         BNZ   EXIT                                                             
         TM    DMCB+8,2                                                         
         BNO   *+14                                                             
         L     RF,ASEQ                                                          
         BASR  RE,RF                                                            
         B     FRMF39                                                           
*                                                                               
         L     R5,AIOAREA                                                       
         CLC   ACKEYACC,KEYSAVE                                                 
         BE    FRMF40              FOUND                                        
*                                                                               
         MVC   ACKEYACC,KEYSAVE                                                 
         MVC   BUDCACN(34),=C'WARNING - CONTRA ACCOUNT NOT FOUND'               
         CLI   CATBKEYW,0          RECORD MUST EXIST UNLESS A SINGLE            
         BE    FRMF45              CONTRA IS GIVEN (NO KEYWORD)                 
         MVI   FERN,NOTFOUND                                                    
         B     ERROR                                                            
*                                                                               
FRMF40   CLI   CATBKEYW,0          CHECK SECURITY & DISPLAY NAME IF             
         BNE   FRMF45              SINGLE                                       
         GOTO1 ASECHECK,DMCB,AIOAREA,(R7)                                       
         BNZ   ERROR                                                            
         MVC   CALEVEL,0(R1)                                                    
*                                                                               
FRMF42   GOTO1 AGETNAME,AIOAREA                                                 
         MVC   BUDCACN,WORK                                                     
*                                                                               
FRMF45   CLI   CATBKEYW,C'I'       NOW BUILD I/O CONTROL TAB ENTRY              
         BNE   FRMF46                                                           
         MVI   IOCLOKEY,X'01'      INPUT=YES HAS X'01' IN IOCLOKEY              
         OI    SAVSTAT,INPUTISY                                                 
         B     FRMF48                                                           
*                                                                               
FRMF46   CLI   ACTION,ADD          IF NOT ADD SET DISPLACEMENTS TO              
         BE    *+12                INDICATE CONTRA FLD OF BUDGET RECORD         
         MVI   IOCDS15,18                                                       
         B     FRMF49                                                           
         MVC   IOCLOKEY,ACKEYACC                                                
         CLI   CATBKEYW,C'C'                                                    
         BNE   *+8                                                              
         MVI   IOCLOKEY+L'IOCLOKEY-1,C' '                                       
         MVC   IOCHIKEY,IOCLOKEY                                                
         CLI   CATBKEYW,0                                                       
         BE    FRMF48                                                           
         LA    R1,IOCHIKEY+L'IOCHIKEY-1                                         
*                                                                               
         CLI   0(R1),X'41'                                                      
         BH    FRMF48                                                           
         MVI   0(R1),X'FF'                                                      
         BCT   R1,*-12                                                          
*                                                                               
FRMF48   MVC   IOCDS15,CATBS15     MOVE DISPLACEMENTS INTO IO CONTROL           
*                                                                               
FRMF49   LLC   R1,IOCDS15                                                       
         LA    R1,3(,R1)                                                        
         STC   R1,IOCDLSTA                                                      
*                                                                               
FRMF50   CLI   ACTION,ADD          CHECK FILTER                                 
         BE    *+10                                                             
         MVC   IOCFILT,ACKEYACC+1  IF ACTION ISNT ADD, CONTENT OF REC           
         CLI   FNDX,0              AREA IS CONTRA FILTER                        
         BE    FRMF55                                                           
         MVI   FNDX,2                                                           
         LLC   RE,TEMP+22+SCANRHSL                                              
         SH    RE,=H'1'                                                         
         MVI   FERN,INVALID                                                     
         BM    ERROR                                                            
*                                                                               
FRMF53   EX    RE,FRMFFILT                                                      
         BNE   ERROR                                                            
         TM    SAVSTAT,INPUTISY                                                 
         BO    ERROR                                                            
         MVC   IOCFILT,TEMP+44+SCANRHSL                                         
*                                                                               
FRMF55   DS    0H                                                               
         OI    BUDCACH+4,VALPREV                                                
         B     FRMF60                                                           
*                                                                               
FRMFFILT CLC   TEMP+34+SCANRHSL(0),=C'FILTER'                                   
*                                                                               
         DROP  R5,R6,R7                                                         
         EJECT ,                                                                
* FIRST FOR FORMAT (FORMAT TYPE)                                                
         SPACE 1                                                                
FRMF60   GOTO1 AFVAL,BUDFORMH      CHECK FOR UP TO 2 ENTRIES                    
         BZ    ERROR                                                            
         GOTO1 VSCANNER,DMCB,FLDH,(3,TEMP)                                      
         MVC   FNDX,4(R1)                                                       
         MVI   FERN,INVALID                                                     
         CLI   FNDX,2                                                           
         BH    ERROR                                                            
         MVI   FNDX,0                                                           
         BL    *+8                                                              
         MVI   FNDX,1                                                           
*                                                                               
         USING FRMTBD,RE                                                        
*                                                                               
FRMF62   LA    RE,FRMTAB           CHECK FORMAT TYPE KEYWORD V TABLE            
         CLI   TEMP+1,0                                                         
         BNE   ERROR                                                            
         LLC   RF,TEMP                                                          
         SH    RF,=H'1'                                                         
         BM    ERROR                                                            
*                                                                               
FRMF64   EXCLC RF,FRMTNAME,TEMP+12                                              
         BE    FRMF66                                                           
*                                                                               
FRMF65   LA    RE,FRMTLEN(RE)                                                   
         CLI   FRMTNAME,0                                                       
         BNE   FRMF64                                                           
         B     ERROR                                                            
*                                                                               
FRMF66   CLI   FNDX,0              NO 2ND FLD                                   
         BNE   FRMF68                                                           
         CLI   FRMTPERS,0                                                       
         BE    FRMF74              OK                                           
         MVI   FERN,NOINPUT                                                     
         MVC   XTRAMESS(7),=C',PERIOD'                                          
         B     FRMF65                                                           
*                                                                               
FRMF68   MVI   FNDX,2              CHECK 2ND FIELD                              
         LA    R1,PERTAB                                                        
         CLI   TEMP+33,0                                                        
         BNE   ERROR                                                            
         LLC   R2,TEMP+32                                                       
         SH    R2,=H'1'                                                         
         BM    ERROR                                                            
*                                                                               
FRMF70   EXCLC R2,0(R1),TEMP+32+12                                              
         BE    FRMF72                                                           
         LA    R1,L'PERTAB(,R1)                                                 
         CLI   0(R1),0                                                          
         BNE   FRMF70                                                           
         B     ERROR                                                            
*                                                                               
FRMF72   MVC   PERNUM,7(R1)        STORE PERIOD PARAM NUM (= $MONTHS            
         LA    R3,FRMTPERS         OR X'FF' FOR 'NAME')                         
         LA    R4,L'FRMTPERS                                                    
*                                                                               
FRMF73   CLC   PERNUM,0(R3)        CHECK FOR COMPATIBILITY                      
         BE    FRMF74              OK                                           
         LA    R3,1(,R3)                                                        
         BCT   R4,FRMF73                                                        
         B     FRMF65              TRY FOR ANOTHER FRMTAB ENTRY                 
*                                                                               
FRMF74   MVC   WORK(L'BUDFORM),SPACES   DISPLAY FULL KEYWORD(S)                 
         MVC   WORK(L'FRMTNAME),FRMTNAME                                        
         CLI   FNDX,0                                                           
         BE    FRMF75                                                           
         LA    R3,WORK+L'FRMTNAME-1                                             
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C','                                                       
         MVC   2(7,R3),0(R1)                                                    
*                                                                               
FRMF75   OC    BUDFORM,SPACES                                                   
         CLC   BUDFORM,WORK                                                     
         BE    FRMF76                                                           
         MVC   BUDFORM,WORK                                                     
         OI    BUDFORMH+6,X'80'                                                 
*                                                                               
FRMF76   MVI   FNDX,0              ACC FORMAT REQUIRES SOME CAC INPUT           
         MVC   XTRAMESS,SPACES                                                  
         CLI   BUDFORM,C'A'                                                     
         BNE   FRMF80                                                           
         CLI   BUDCACH+5,0                                                      
         BNE   FRMF80                                                           
         LA    R1,BUDCACH                                                       
         ST    R1,FADR                                                          
         MVI   FERN,NOINPUT                                                     
         B     ERROR                                                            
*                                                                               
FRMF80   TM    SAVSTAT,INPUTISY    CHECK FOR INPUT=YES COMPATIBILITY            
         BNO   FRMF85                                                           
         CLI   BUDFORM,C'C'                                                     
         BE    FRMF85                                                           
         LA    R1,BUDCACH                                                       
         ST    R1,FADR                                                          
         MVI   FERN,FINCMPAT                                                    
         B     ERROR                                                            
*                                                                               
FRMF85   MVC   MAXTYPES,FRMTBTNO                                                
         OI    BUDFORMH+4,VALPREV                                               
         CLI   BUDFORM,C'P'                                                     
         BNE   *+14                                                             
         MVC   MPERLINE,PERNUM                                                  
         B     FRMFOFC                                                          
*                                                                               
         CLI   PERNUM,12           ANNUAL FOR ACC/CAC FORM IS EQUIV TO          
         BNE   *+12                NO PERIOD PARA                               
         MVI   PERNUM,0                                                         
         B     FRMFOFC                                                          
*                                                                               
         CLI   PERNUM,X'FF'        NAME PARAMETER                               
         BNE   FRMFOFC                                                          
         MVI   PERNUM,0                                                         
         OI    SAVSTAT,NAME                                                     
         B     FRMFOFC                                                          
*                                                                               
         DROP  RE                                                               
                                                                                
FRMFOFC  DS    0H                  VALIDATE OFFICE FIELD                        
         MVC   BUDOFFN,SPACES                                                   
         OI    BUDOFFNH+6,X'80'                                                 
         GOTO1 AFVAL,BUDOFFCH                                                   
         BZ    FRMFOFCX            NO OFFICE INPUT                              
         MVI   FERN,INVALID                                                     
         TM    COMPSTA4,X'01'      TEST NEW OFFICES                             
         BZ    ERROR               NO, CAN'T US THIS FIELD                      
         CLI   BUDOFFCH+5,2        MUST BE 2 BYTES                              
         BNE   ERROR                                                            
         MVC   OFFILT,FLD                                                       
                                                                                
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVI   OFFAACT,OFFAVAL     VALIDATE INPUT OFFICE                        
         MVC   OFFAOFFC,OFFILT                                                  
         GOTO1 VOFFAL                                                           
         BNE   ERROR                                                            
         DROP  R1                                                               
                                                                                
         LA    R4,KEY                                                           
*&&DO                                                                           
         USING OFFRECD,R4                                                       
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COMPANY                                                  
         MVC   OFFKOFF,OFFILT                                                   
*&&                                                                             
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(2),=C'2D'                                                
         MVC   ACTKACT(2),OFFILT                                                
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   ERROR               DOES NOT EXIST                               
         L     R4,AIOAREA1                                                      
         LA    R4,ACCORFST(R4)                                                  
         USING NAMELD,R4                                                        
         SR    R1,R1                                                            
*                                                                               
FRMFOFC2 CLI   NAMEL,0                                                          
         BE    FRMF90                                                           
         CLI   NAMEL,NAMELQ                                                     
         BE    FRMFOFC3                                                         
         IC    R1,NAMLN                                                         
         AR    R4,R1                                                            
         B     FRMFOFC2                                                         
*                                                                               
FRMFOFC3 IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         MVC   WORK,SPACES                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),NAMEREC                                                  
         MVC   BUDOFFN,WORK                                                     
         OI    BUDOFFNH+6,X'80'                                                 
         DROP  R4                                                               
FRMFOFCX OI    BUDOFFCH+4,VALPREV                                               
                                                                                
         EJECT ,                                                                
* FIRST FOR FORMAT (BUDGET TYPES)                                               
         SPACE 1                                                                
         USING BUDTD,R7                                                         
         SPACE 1                                                                
FRMF90   MVI   COUNT,0             INITIALISE W/S                               
         MVC   CRD,SPACES          AREA FOR COLUMN HEADS (4X2X10)               
         XC    WORK1,WORK1         AREA FOR BUDGET VALIDATION EL                
         LA    R7,BUDTAB                                                        
         LA    R6,BUDTYP1H                                                      
*                                                                               
FRMF92   GOTO1 AFVAL,(R6)          LOOP FOR A BUDGET TYPE                       
         BNZ   FRMF94                                                           
         CLI   COUNT,0                                                          
         BE    ERROR               MUST BE ONE                                  
         SR    RE,RE               SET VALPREV ON THE UNUSED ONES               
         LA    RF,BUDTYP4H                                                      
         OI    4(R6),VALPREV                                                    
         IC    RE,0(,R6)                                                        
         BXLE  R6,RE,*-8                                                        
         B     FRMF190                                                          
*                                                                               
FRMF94   LLC   R1,COUNT            BUMP COUNT                                   
         LA    R1,1(,R1)                                                        
         STC   R1,COUNT                                                         
         CLC   COUNT,MAXTYPES      COMPARE WITH MAX FOR FORMAT                  
         BNH   FRMF96                                                           
         MVI   FERN,TOOMANY                                                     
         MVC   XTRAMESS(7),=C'B.TYPES'                                          
         B     ERROR                                                            
*                                                                               
FRMF96   LA    R0,SCANRHSL         SCAN FOR 2 OR 3 SUB-FIELDS                   
         GOTO1 VSCANNER,DMCB,((R0),FLDH),(4,TEMP)                               
         MVC   FNDX,4(R1)                                                       
         MVC   FLAG1,FNDX                                                       
         MVI   FERN,INVALID                                                     
         CLI   FNDX,3                                                           
         BH    ERROR                                                            
         CLI   FNDX,1                                                           
         BH    FRMF98                                                           
         BL    ERROR                                                            
         MVI   FERN,NOINPUT                                                     
         MVI   FNDX,2                                                           
         B     ERROR                                                            
*                                                                               
FRMF98   MVI   FNDX,1              FIRST FIELD MUST BE BUDGET TYPE CODE         
         CLI   TEMP+1,0                                                         
         BNE   ERROR                                                            
         LLC   R2,TEMP                                                          
         SH    R2,=H'1'                                                         
         BM    ERROR                                                            
         XC    KEY,KEY             BUILD KEY TO READ HIGH                       
         LA    R5,KEY                                                           
*                                                                               
         USING ACKEYD,R5                                                        
*                                                                               
         MVI   ACBTKTYP,ACBTKTEQ                                                
         MVC   ACBTKCMP,COMPANY                                                 
         EXMVC R2,ACBTKCOD,TEMP+12                                              
         GOTO1 ARDHI,AIOAREA1                                                   
         BNE   ERROR                                                            
         L     R5,AIOAREA                                                       
         LA    R2,(ACBTKCOD-ACKEYD)(R2)                                         
         EXCLC R2,ACBTKEY,KEYSAVE                                               
         MVI   FERN,BTYPNXST                                                    
         BNE   ERROR               NOT FOUND                                    
         MVC   BUDTNUM,ACBTKNO2    SAVE TYPE NUMBER IN TABLE                    
         MVC   BUDTCOL,COUNT       AND COLUMN NUMBER                            
*                                                                               
FRMF100  SR    R0,R0               HANDLE BUDGET TYPE REC ELEMENTS              
         LA    R5,ACRECORD                                                      
         B     *+10                                                             
*                                                                               
FRMF102  IC    R0,1(,R5)                                                        
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         MVI   FERN,BTYPACLE                                                    
         BE    ERROR               BUDGET TYPE INCOMPATIBLE WITH A/C            
         CLI   0(R5),ACBVELEQ                                                   
         BE    FRMF106                                                          
         CLI   0(R5),ACBCELEQ                                                   
         BNE   FRMF102                                                          
*                                                                               
         USING ACBCD,R5                                                         
         CLI   ACBCLEN,ACBCLNQ2    TEST NEW ELEMENT LENGTH                      
         BL    FRMF103             THIS BUDGET CAN'T HAVE OFFICE                
         TM    ACBCSTAT,ACBCOFFC   TEST BUDGET SHOULD HAVE OFFICE               
         BNO   FRMF103                                                          
*                                                                               
         CLI   OFFILT+1,C' '       TEST 2 BYTE OFFICE INPUT                     
         BH    FRMF104             YES, THAT'S GOOD                             
         LA    R1,BUDOFFCH                                                      
         ST    R1,FADR                                                          
         MVI   FERN,OFFREQRD       OFFICE REQUIRED                              
         B     ERROR                                                            
*                                                                               
FRMF103  CLI   OFFILT+1,C' '       TEST INPUT OFFICE                            
         BNH   FRMF104             NO,                                          
         LA    R1,BUDOFFCH                                                      
         ST    R1,FADR                                                          
         MVI   FERN,NOTALLOW       INPUT NOT ALLOWED IN THIS FIELD              
         B     ERROR                                                            
                                                                                
*                                                                               
FRMF104  XR    R1,R1                                                            
         IC    R1,COUNT            COLUMN NAME EL - SAVE NAME IN CRD            
         BCTR  R1,0                                                             
         MH    R1,=H'20'                                                        
         LA    R1,CRD(R1)                                                       
*                                                                               
*                                                                               
         MVC   0(20,R1),ACBCCOL1   COL 1 & 2                                    
         B     FRMF102                                                          
*                                                                               
         USING ACBVD,R5                                                         
*                                                                               
FRMF106  DS    0H                  VALIDATION EL                                
         LA    RF,ACCNTRL                                                       
         USING IOCBD,RF                                                         
*                                                                               
         CLC   ACBVACUL,IOCLCODE    FIND ONE FOR THIS A/C LEDGER                
         BNE   FRMF102                                                          
         LLC   R1,ACBVLEN                                                       
*                                                                               
FRMF108  LA    RF,CACNTRL                                                       
         OC    WORK1,WORK1         FIRST TIME THRU SAVE RELEVANT PARTS          
         BNZ   FRMF120             OF EL IN WORK1 WITH X'FF' TERMINATOR         
         EXMVC R1,WORK1,ACBVEL                                                  
         LA    R1,WORK1(R1)                                                     
         MVI   0(R1),X'FF'                                                      
         CLI   WORK1+5,C'+'        ADJUST FOR NONSTANDARD + SINGLE CAC          
         BNE   FRMF109                                                          
*                                                                               
         XC    IOCLCODE(6),IOCLCODE                                             
         CLC   IOCLOKEY,IOCHIKEY                                                
         BNE   FRMF109                                                          
         OC    IOCLOKEY,IOCLOKEY                                                
         BE    FRMF109                                                          
         MVC   WORK(15),IOCLOKEY+1                                              
         MVC   IOCLOKEY,SPACES                                                  
         LLC   R1,WORK1+6                                                       
         LA    R1,IOCLOKEY(R1)                                                  
         MVC   0(15,R1),WORK                                                    
         MVC   IOCHIKEY,IOCLOKEY                                                
*                                                                               
FRMF109  OC    IOCLCODE,IOCLCODE    IF CONTRA LEDGER IS DEFINED FIND            
         BZ    FRMF114             RULE FOR IT, OTHERWISE KEEP ALL              
*                                                                               
FRMF109A XC    HALF,HALF                                                        
         CLC   IOCLCODE,SPACES      CONTRA=ALL CASE - ACBVCAUN(3)=NULLS         
         BE    *+10                                                             
         MVC   HALF,IOCLCODE                                                    
         LA    R5,WORK1                                                         
         MVI   FERN,BTYPCALE                                                    
         DROP  RF                                                               
*                                                                               
FRMF110  CLC   HALF,ACBVCAUN       FIND SUBEL FOR THIS CONTRA LEDGER            
         BE    FRMF112                                                          
         MVC   ACBVCAUN(6*3),ACBVCAUN+3                                         
         CLI   ACBVCAUN,X'FF'                                                   
         BNE   FRMF110                                                          
         B     ERROR                                                            
*                                                                               
FRMF112  MVI   ACBVCAUN+3,X'FF'                                                 
*                                                                               
FRMF114  CLI   ACLEVEL,X'FF'       CHECK LEVEL                                  
         BE    FRMF116                                                          
         LA    R1,BUDACCH                                                       
         CLC   ACLEVEL,ACBVACLV                                                 
         BNE   FRMF118                                                          
*                                                                               
FRMF116  LA    R1,BUDCACH          CONTRA                                       
         CLI   ACBVCAUN,C'+'       IGNORE IRREGULAR CONTRA                      
         BE    FRMF130                                                          
         CLI   ACTION,ADD          AND NOT ADD                                  
         BNE   FRMF130                                                          
         OC    ACBVCAUN(3),ACBVCAUN ALL CASE                                    
         BNZ   FRMF117                                                          
         CLC   BUDCAC(3),=C'ALL'                                                
         BNE   FRMF118                                                          
         B     FRMF130                                                          
*                                                                               
FRMF117  CLI   CALEVEL,X'FF'       CONTRA NOT INPUT                             
         BE    FRMF130                                                          
         CLC   CALEVEL,ACBVCALV                                                 
         BE    FRMF130                                                          
*                                                                               
FRMF118  ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         MVI   FERN,WRONGLEV                                                    
         B     ERROR                                                            
*                                                                               
FRMF120  MVI   FERN,INCOMPAT       NOT FIRST TIME THRU - CHECK THAT             
         CLC   ACBVACLV,WORK1+4    RULES FOR THIS TYPE ARE COMPATIBLE           
         BE    *+14                A/C LEVEL                                    
         MVC   XTRAMESS(7),=C'A/C LEV'                                          
         B     ERROR                                                            
*                                                                               
         LA    R1,ACBVEL(R1)       CONTRA - REDUCE THE VALIDATION EL            
         MVI   0(R1),X'FF'         IN WORK1 TO THE COMMON SET                   
         LA    RF,WORK1+(ACBVCAUN-ACBVD)                                        
*                                                                               
FRMF122  LA    RE,ACBVCAUN                                                      
*                                                                               
FRMF123  CLI   0(RE),X'FF'         IF THE SAVED RULE DOESNT EXIST FOR           
         BNE   FRMF124             THIS TYPE, REMOVE IT BY SHUFFLING            
         MVC   0(20,RF),3(RF)      WORK1                                        
         CLI   0(RF),X'FF'         ANY MORE                                     
         BNE   FRMF122                                                          
         CLI   WORK1+(ACBVCAUN-ACBVD),X'FF'                                     
         BNE   FRMF130                                                          
         MVC   XTRAMESS(7),=C'C/A LEV'                                          
         B     ERROR                                                            
*                                                                               
FRMF124  CLC   0(3,RE),0(RF)                                                    
         BE    *+12                                                             
         LA    RE,3(,RE)                                                        
         B     FRMF123                                                          
*                                                                               
         LA    RF,3(,RF)                                                        
         CLI   0(RF),X'FF'                                                      
         BNE   FRMF122                                                          
*                                                                               
         USING ACKEYD,R5                                                        
*                                                                               
FRMF130  L     R5,AIOAREA          CHECK FOR 13PD AND 1000 UNIT TYPES           
         NI    ACSTATUS,THIRTEEN+THOUSAND                                       
         CLI   COUNT,1                                                          
         BH    FRMF134                                                          
         TM    ACSTATUS,THIRTEEN   IF 13 PERIOD                                 
         BNO   FRMF132                                                          
         CLI   PERNUM,3            QUARTERLY INVALID                            
         BE    FRMF135                                                          
         CLI   PERNUM,12           12 BECOMES 13                                
         BNE   FRMF132                                                          
         MVI   PERNUM,13                                                        
*                                                                               
FRMF132  OC    SAVSTAT,ACSTATUS    SAVE BITS FROM ACSTATUS IF ANY               
         B     FRMF140                                                          
*                                                                               
FRMF134  DS    0H                  IF NOT FIRST ,CHECK FOR CONSISTENT           
         MVI   WORK,THIRTEEN+THOUSAND                                           
         NC    WORK(1),SAVSTAT                                                  
         XC    WORK(1),ACSTATUS                                                 
         BZ    FRMF140                                                          
         MVC   XTRAMESS(5),=C'UNITS'                                            
         TM    WORK,THOUSAND                                                    
         BO    ERROR                                                            
*                                                                               
FRMF135  MVC   XTRAMESS(6),=C'13 PER'                                           
         B     ERROR                                                            
*                                                                               
         DROP  R5                                                               
*                                                                               
FRMF140  MVI   FNDX,2              2ND FIELD MUST BE YY(MMM-MMM)                
         MVI   FERN,INVYEAR                                                     
         LA    R5,TEMP+22+SCANRHSL                                              
         CLI   1(R5),0                                                          
         BNE   ERROR                                                            
         LA    R2,12(,R5)                                                       
         MVI   BUDTSTA+1,1                                                      
         CLI   0(R2),C'C'          CALENDAR YR                                  
         BE    FRMF141                                                          
         OI    BUDTSTAT,FINANCEY   NO PREFIX IMPLIES FINANCIAL                  
         TM    SAVSTAT,THIRTEEN                                                 
         BO    *+10                                                             
         MVC   BUDTSTA+1(1),COSTARTM                                            
         CLI   0(R2),C'F'          FINANCIAL YR                                 
         BNE   *+8                                                              
*                                                                               
FRMF141  LA    R2,1(,R2)                                                        
         GOTO1 CHKYR,DMCB,(R2)     CHECK YEAR                                   
         BNE   ERROR                                                            
         MVC   BUDTSTA(1),WORK     SAVE BINARY VALUE                            
*                                                                               
FRMF142  MVI   BUDTMNUM,12         SET MONTH/PERIOD DEFAULTS                    
         TM    SAVSTAT,THIRTEEN                                                 
         BNO   *+8                                                              
         MVI   BUDTMNUM,13                                                      
         MVC   MPERLINE,BUDTMNUM   12 PER COLUMN(IE BUDTAB ENTRY)               
         CLI   BUDFORM,C'P'        AND 12 PER LINE                              
         BE    FRMF145             PERIOD PARAM OVERRIDES COL VALUE FOR         
         CLI   PERNUM,0            NON-PERIOD FORMAT AND LINE VALUE FOR         
         BE    FRMF146             PERIOD FORMAT                                
         MVC   BUDTMNUM,PERNUM                                                  
         B     FRMF146                                                          
*                                                                               
FRMF145  MVC   MPERLINE,PERNUM                                                  
*                                                                               
FRMF146  CLI   2(R2),C'('          CHECK MONTH QUALIFIER                        
         BE    FRMF148             MUST BE PRESENT IF FORMAT IS                 
         CLI   BUDFORM,C'P'        ACC OR CONTRA,MONTH AS ONLY 4 MONTHS         
         BE    FRMF155             CAN BE SHOWN                                 
         CLI   PERNUM,1                                                         
         BNE   FRMF160                                                          
         MVI   FERN,NOINPUT                                                     
         MVC   XTRAMESS(9),=C'(MMM-MMM)'                                        
         B     ERROR                                                            
*                                                                               
FRMF148  GOTO1 CHKMON,DMCB,2(R2)                                                
         BZ    FRMF149A                                                         
*                                                                               
FRMF149  MVC   XTRAMESS(05),=C'MONTH'                                           
         B     ERROR                                                            
*                                                                               
FRMF149A MVC   BUDTSTA+1(1),WORK   SAVE 1ST MONTH FOR COL IN BINARY             
         CLI   BUDFORM,C'P'                                                     
         BE    FRMF149B                                                         
         CLI   PERNUM,0                                                         
         BNE   FRMF149C                                                         
*                                                                               
FRMF149B MVC   BUDTMNUM,WORK+2     # MONTHS PER COL IF NOT ACC/CAC,PER          
         B     FRMF149D                                                         
*                                                                               
FRMF149C MVC   MPERLINE,WORK+2     SAVE THIS VALUE FOR LATER IF IT IS           
*                                                                               
FRMF149D CLI   PERNUM,1            CHECK THAT NO MORE THAN 4 MONTHS             
         BNE   FRMF150             IF ACC/CON FORMAT WITH MONTH PARM            
         CLI   MPERLINE,4                                                       
         MVI   FERN,TOOBIG                                                      
         BH    FRMF149                                                          
*                                                                               
FRMF150  CLI   PERNUM,0            CHECK THAT # MONTHS DIVISIBLE XACTLY         
         BE    FRMF160             BY PERNUM IE CANT ALLOW QUARTERLY            
         LLC   R0,WORK+2           IF MMM-MMM IS NOT ONE OF 3,6,9,12            
         SRDL  R0,32                                                            
         LLC   R2,PERNUM                                                        
         DR    R0,R2                                                            
         MVI   FERN,INCOMPAT                                                    
         LTR   R0,R0               ANY REMAINDER                                
         BZ    FRMF156                                                          
         B     FRMF149                                                          
*                                                                               
FRMF155  MVC   BUDTSTA+1(2),BUDTAB+(BUDTSTA+1-BUDTD)                            
*                                                                               
FRMF156  CLI   BUDFORM,C'P'                                                     
         BNE   FRMF160                                                          
         CLC   BUDTSTA+1(2),BUDTAB+(BUDTSTA+1-BUDTD)                            
         BNE   FRMF149             PERIOD FORM REQUIRES SAME MONTH RNGE         
*                                                                               
FRMF160  MVI   FNDX,3              3RD FLD IS OPTIONAL - ONE OF                 
         CLI   FLAG1,3             (A) DISPLAY - MEANING PROTECT                
         BL    FRMF180             (B) YY(MMM-MMM)+OR-NN%                       
         LA    R5,22+SCANRHSL(R5)                                               
         LLC   R2,0(,R5)                                                        
         BCTR  R2,0                                                             
         EXCLC R2,12(R5),=C'DISPLAY'                                            
         BNE   FRMF161                                                          
         OI    BUDTSTAT,PROTECT                                                 
         B     FRMF180                                                          
*                                                                               
FRMF161  GOTO1 CHKYR,DMCB,12(R5)   YEAR                                         
         BNE   ERROR                                                            
         MVC   BUDTSRCE(1),WORK                                                 
         MVC   BUDTSRCE+1(1),BUDTSTA+1  DEFAULTS TO SAME MONTH                  
         LA    R4,14(,R5)                                                       
         CLI   0(R4),C'('          IS THERE A MONTH QUALIFIER                   
         BNE   FRMF165                                                          
         GOTO1 CHKMON,DMCB,(R4)    IF SO CHECK IT                               
         BNE   FRMF149             ERROR                                        
         MVC   BUDTSRCE+1(1),WORK                                               
         CLC   BUDTMNUM,WORK+2     MONTH RANGE MUST BE SAME                     
         MVI   FERN,INCOMPAT                                                    
         BNE   FRMF149                                                          
         L     R4,0(,R1)           R5 = A(NEXT BYTE AFTER MONTH QLFR)           
*                                                                               
FRMF165  CLI   0(R4),C' '          CHECK FOR +NN%                               
         BNH   FRMF180                                                          
         MVI   FERN,INVALID                                                     
         CLI   0(R4),C'+'                                                       
         BE    *+16                                                             
         OI    BUDTSTAT,NEGATIVE                                                
         CLI   0(R4),C'-'                                                       
         BNE   FRMF169                                                          
         LA    R4,1(R4)                                                         
         LA    R5,12(R2,R5)        POINT R5 LAST CHAR OF SUBFIELD               
         CLI   0(R5),C'%'          APART FROM OPTIONAL PCNT SIGN                
         BNE   *+6                                                              
         BCTR  R5,0                                                             
         SR    R5,R4               CHECK FOR NUMERIC 0-200                      
         BM    FRMF169                                                          
         MVC   WORK(4),=4C'0'                                                   
         EX    R5,F165MVZ                                                       
         CLC   WORK(4),=4C'0'                                                   
         MVI   FERN,INVNUM                                                      
         BNE   FRMF169                                                          
         EX    R5,F165PACK                                                      
         CVB   R0,DUB                                                           
         CH    R0,=H'200'                                                       
         MVI   FERN,TOOBIG                                                      
         BH    FRMF169                                                          
         STC   R0,BUDTPCNT         SAVE IN TABLE IN BINARY                      
         B     FRMF180                                                          
*                                                                               
F165MVZ  MVZ   WORK(0),0(R4)                                                    
*                                                                               
F165PACK PACK  DUB,0(0,R4)                                                      
*                                                                               
FRMF169  MVC   XTRAMESS(6),=C'+/-NN%'                                           
         B     ERROR                                                            
*                                                                               
FRMF180  CLI   BUDTSRCE,0          TIDY UP                                      
         BNE   *+10                                                             
         MVC   BUDTSRCE,BUDTSTA    SOURCE DEFAULTS TO DESTINATION YYMM          
         OI    4(R6),VALPREV                                                    
         CLI   COUNT,4                                                          
         BE    FRMF190                                                          
         LA    R7,BUDTLEN(,R7)     BUMP TABLE                                   
         SR    R0,R0               BUMP TWA TO NEXT UNP                         
*                                                                               
FRMF185  IC    R0,0(,R6)                                                        
         AR    R6,R0                                                            
         TM    1(R6),PROTECT                                                    
         BO    FRMF185                                                          
         B     FRMF92                                                           
*                                                                               
FRMF190  CLI   BUDFORM,C'P'        AT END OF BUDGET TYPES                       
         BE    FRMF200             GENERATE EXTRA TABLE ENTRIES IF              
         CLI   PERNUM,0            FORMAT=ACC/CONTRA,PERIOD                     
         BE    FRMF200                                                          
         LLC   R2,MPERLINE                                                      
         LLC   R0,PERNUM                                                        
         SRDL  R2,32                                                            
         DR    R2,R0                                                            
         BCTR  R3,0                                                             
         LTR   R3,R3                                                            
         BZ    FRMF200             ONLY THE ONE COLUMN OF FIGURES               
         LA    R7,BUDTAB                                                        
*                                                                               
FRMF192  MVC   BUDTLEN(BUDTLEN,R7),0(R7)                                        
         LA    R7,BUDTLEN(,R7)                                                  
         IC    R2,BUDTCOL          BUMP COL NUMBER                              
         LA    R2,1(,R2)                                                        
         STC   R2,BUDTCOL                                                       
         GOTO1 BUMPMON,DMCB,((R0),BUDTSTA)                                      
         GOTO1 ,DMCB,((R0),BUDTSRCE)                                            
         BASR  RE,RF                                                            
         BCT   R3,FRMF192                                                       
*                                                                               
FRMF200  LA    R7,BUDTAB           CALCULATE # FIELDS PER LINE AND PUT          
         LA    R1,1                IN TEMPFPLN FOR NOW                          
         LA    R0,4                                                             
         TM    SAVSTAT,NAME                                                     
         BNO   *+8                                                              
         LA    R1,1(,R1)                                                        
         TM    SAVSTAT,TOTAL                                                    
         BZ    FRMF202                                                          
         CLI   BUDFORM,C'P'                                                     
         BE    FRMF202                                                          
         OC    BUDTAB+BUDTLEN(2),BUDTAB+BUDTLEN                                 
         BZ    FRMF202                                                          
         LA    R1,1(,R1)                                                        
*                                                                               
FRMF202  OC    BUDTNUM,BUDTNUM                                                  
         BE    FRMF204                                                          
         LA    R1,1(,R1)                                                        
         LA    R7,BUDTLEN(,R7)                                                  
         BCT   R0,FRMF202                                                       
*                                                                               
FRMF204  STC   R1,TEMPFPLN                                                      
         MVI   0(R7),X'FF'         BUDTAB TERMINATOR                            
*                                                                               
         USING IOCBD,R7            LEVEL DATA                                   
*                                                                               
FRMF205  LA    R7,ACCNTRL          COMPLETE I/O CONTROL BLOCKS WITH             
         LLC   R1,IOCDS15          SOURCE OF 15 IS ONE PAST SOURCE OF           
         LA    R1,1(,R1)           14 BYTE                                      
         STC   R1,IOCDS14                                                       
         IC    R1,WORK1+(ACBVACLV-ACBVD)                                        
         GOTO1 ALEVSET                                                          
         MVC   BUDRULES,WORK1+(ACBVCAUN-ACBVD)    SAVE CONTRA RULES             
         LA    R7,CACNTRL          CONTRA                                       
         IC    R1,IOCDS15                                                       
         LA    R1,1(,R1)                                                        
         STC   R1,IOCDS14                                                       
         IC    R1,IOCDLSTA         PRESET LEVEL END TO START+12                 
         LA    R1,12(,R1)                                                       
         STC   R1,IOCDLEND                                                      
         CLI   BUDRULES+3,X'FF'                                                 
         BNE   FRMF210             MULTI-RULES SO DO NOTHING                    
         CLI   BUDRULES,C'+'                                                    
         BL    FRMF210                                                          
         BE    FRMF208                                                          
         IC    R1,BUDRULES+2                                                    
         GOTO1 ALEVSET                                                          
         B     FRMF210                                                          
*                                                                               
FRMF208  IC    R1,BUDRULES+1       IRREGULAR CONTRA (+NN)                       
         LLC   R2,IOCDS15                                                       
         AR    R1,R2                                                            
         STC   R1,IOCDLSTA         MODIFY START OF LEVEL                        
         STC   R1,IOCDS14          AND SOURCE OF DISPLAY                        
         CLI   BUDRULES+2,0        AND, IF 2ND N GIVEN, END OF LEVEL            
         BE    FRMF210                                                          
         IC    R2,BUDRULES+2                                                    
         AR    R1,R2                                                            
         STC   R1,IOCDLEND                                                      
*                                                                               
         DROP  R7                                                               
         EJECT ,                                                                
* BUILD THE SCREEN FOR THIS FORMAT                                              
         SPACE 1                                                                
FRMF210  L     R2,ATIA             SAVE LAST SCREEN MATRIX IN TIA               
         MVI   0(R2),0             IF INPUT=Y (FOR A/C CODES)                   
         TM    SAVSTAT,INPUTISY                                                 
         BNO   FRMF215                                                          
         OC    DFSTFLDH,DFSTFLDH                                                
         BZ    FRMF215                                                          
         LH    RE,DFSTFLDH                                                      
         AR    RE,R8                                                            
         TM    1(RE),PROTECT       ONLY IF LAST SCREEN WAS INPUT=YES            
         BO    FRMF215                                                          
         MVI   1(RE),0             2260 FUNNY                                   
         LA    RF,1500                                                          
         LR    R3,RF                                                            
         L     R2,ATIA                                                          
         MVCL  R2,RE                                                            
*                                                                               
FRMF215  LA    RE,BUDTABFH         CLEAR REST OF SCREEN IN TWA                  
         LA    RF,TWAD+2120                                                     
         SR    RF,RE                                                            
         XCEF                                                                   
         LA    R5,BUDTABFH         R5 = MAJOR TWA POINTER                       
         LA    R4,FLDHEDS          R4 = FLD HEADER TABLE POINTER                
*                                                                               
FRMF218  CLI   PERNUM,0            FORMAT=ACC/CONTRA WITHOUT PERIOD             
         BNE   FRMF225             HAS 3 HEADLINES - 1ST FOR DATES              
         MVC   0(L'FLDHEDS,R5),0(R4)                                            
         BAS   RE,SPACFILL         SPACEFILL FLD WITH HDR AT R5                 
         LA    R7,BUDTAB                                                        
*                                                                               
         USING BUDTD,R7                                                         
*                                                                               
         LA    R2,8(,R5)                                                        
         TM    SAVSTAT,NAME                                                     
         BNO   *+8                                                              
         LA    R2,39(,R2)                                                       
*                                                                               
FRMF220  CLI   BUDTD,X'FF'         USE BUDTAB TO GENERATE DATE HEADS            
         BE    FRMF222                                                          
         GOTO1 DATED,DMCB,(BUDTMNUM,BUDTSTA),(R2)                               
         LA    R2,13(,R2)                                                       
         LA    R7,BUDTLEN(,R7)                                                  
         B     FRMF220                                                          
*                                                                               
FRMF222  LLC   R0,0(,R5)           BUMP TWA                                     
         AR    R5,R0                                                            
         ST    R5,FULL1            SAVE ADDR (FOR LATER FLDADR ADJSTMT)         
*                                                                               
FRMF225  LA    R4,L'FLDHEDS(,R4)   2 FULL HEADING LINES FOR ALL FORMATS         
         MVC   0(L'FLDHEDS,R5),0(R4)                                            
         MVC   8(77,R5),SPACES                                                  
         LLC   R6,0(,R5)                                                        
         AR    R6,R5               USE R6 FOR 2ND LINE POINTER                  
         MVC   0(L'FLDHEDS,R6),4(R4)                                            
         MVC   8(77,R6),SPACES                                                  
         LA    R1,5                                                             
         CLI   BUDFORM,C'A'                                                     
         BNE   *+8                                                              
         LA    R1,6                                                             
         EX    R1,F225MVC1         MOVE IN BUDFORM NAME AS 1ST COL HEAD         
         EX    R1,F225MVC2         UNDERLINED                                   
         LA    R2,8+16(,R5)        POINT R2,R3 AT NEXT COL START                
         LA    R3,8+16(,R6)                                                     
         B     FRMF230                                                          
*                                                                               
F225MVC1 MVC   8(0,R5),BUDFORM                                                  
*                                                                               
F225MVC2 MVC   8(0,R6),DASHES                                                   
*                                                                               
FRMF230  CLI   PERNUM,0            ACC/CONTRA, PERIOD REQUIRES PERIOD           
         BE    FRMF235             HEADS UNDERLINED                             
         CLI   BUDFORM,C'P'                                                     
         BE    FRMF235                                                          
         LA    R7,BUDTAB                                                        
*                                                                               
FRMF232  CLI   BUDTD,X'FF'                                                      
         BE    FRMF234                                                          
         GOTO1 DATED,DMCB,(BUDTMNUM,BUDTSTA),(R2)                               
         LLC   RF,4(,R1)                                                        
         BCTR  RF,0                                                             
         EXMVC RF,0(R3),DASHES                                                  
         LA    R2,13(,R2)                                                       
         LA    R3,13(,R3)                                                       
         LA    R7,BUDTLEN(,R7)                                                  
         B     FRMF232                                                          
*                                                                               
FRMF234  TM    SAVSTAT,TOTAL       PLUS TOTAL COL IF REQUESTED                  
         BNO   FRMF240                                                          
         MVC   0(5,R2),TOTALMSG                                                 
         MVC   0(5,R3),DASHES                                                   
         B     FRMF240                                                          
*                                                                               
FRMF235  TM    SAVSTAT,NAME        ALL OTHER FORMATS HAVE BUDGET TYPE           
         BNO   FRMF237             COLUMN NAME AS HEADING                       
         MVC   0(4,R2),=C'NAME'    PLUS 'NAME' IF THIS OPTION IN USE            
         MVC   0(4,R3),DASHES                                                   
         LA    R2,39(,R2)                                                       
         LA    R3,39(,R3)                                                       
*                                                                               
FRMF237  LA    R1,CRD              CRD CONTAINS 4X2XCOLUMN NAMES                
         LA    R0,4                                                             
*                                                                               
FRMF238  CLI   0(R1),C' '                                                       
         BE    FRMF239                                                          
         MVC   0(10,R2),0(R1)                                                   
         MVC   0(10,R3),10(R1)                                                  
         LA    R2,13(,R2)                                                       
         LA    R3,13(,R3)                                                       
         LA    R1,20(,R1)                                                       
         BCT   R0,FRMF238                                                       
*                                                                               
FRMF239  CLI   BUDFORM,C'P'        TOTAL COLUMN IF NOT PERIOD FORMAT            
         BE    FRMF240                                                          
         CLI   BUDTAB+BUDTLEN,X'FF'    AND MULTIPLE TYPES                       
         BE    FRMF240                                                          
         TM    SAVSTAT,TOTAL       AND TOTALS REQUESTED                         
         BNO   FRMF240                                                          
         MVC   0(5,R2),TOTALMSG                                                 
         MVC   0(5,R3),DASHES                                                   
*                                                                               
FRMF240  LLC   R0,0(,R6)           BUMP TO FIRST FLD HDR IN MATRIX              
         AR    R0,R6                                                            
         LR    R5,R0                                                            
         SR    R0,R8                                                            
         STH   R0,DFSTFLDH         SAVE ITS DISP INTO TWA                       
         LA    R4,2*L'FLDHEDS(,R4) MOVE LINE TITLE FLD TO TWA                   
         MVC   0(L'FLDHEDS,R5),0(R4)                                            
         CLI   BUDFORM,C'P'                                                     
         BNE   *+8                                                              
         MVI   0(R5),8+7           SHORTEN FOR PERIOD FORMAT                    
         BAS   RE,SPACFILL                                                      
         TM    SAVSTAT,INPUTISY    UNPROTECT IF INPUT=Y                         
         BNO   *+8                                                              
         NI    1(R5),ALL-PROTECT                                                
         LLC   R0,0(,R5)           BUMP TO NEXT FLD                             
         AR    R5,R0                                                            
         LA    R4,L'FLDHEDS(,R4)                                                
         TM    SAVSTAT,NAME        MOVE IN UNP NAME FLD IF REQUIRED             
         BNO   FRMF245                                                          
         MVC   0(L'FLDHEDS,R5),FLDHNAME                                         
         IC    R0,0(,R5)                                                        
         AR    R5,R0                                                            
         LA    R4,3*L'FLDHEDS(,R4)                                              
*                                                                               
FRMF245  LA    R7,BUDTAB                                                        
*                                                                               
FRMF247  CLI   0(R7),X'FF'         BUILD A VALUE FLD                            
         BE    FRMF249                                                          
         MVC   0(L'FLDHEDS,R5),0(R4)                                            
         TM    BUDTSTAT,PROTECT    PROTECT IF ',DISPLAY'                        
         BNO   *+8                                                              
         OI    1(R5),PROTECT                                                    
         IC    R0,0(,R5)           BUMP TO NEXT                                 
         AR    R5,R0                                                            
         LA    R4,L'FLDHEDS(,R4)                                                
         LA    R7,BUDTLEN(,R7)                                                  
         B     FRMF247                                                          
*                                                                               
FRMF249  CLI   BUDFORM,C'P'        PLUS TOTAL COLUMN                            
         BE    FRMF250                                                          
         CLI   BUDTAB+BUDTLEN,X'FF'                                             
         BE    FRMF250                                                          
         TM    SAVSTAT,TOTAL                                                    
         BNO   FRMF250                                                          
         MVC   0(L'FLDHEDS,R5),0(R4)                                            
         OI    1(R5),PROTECT                                                    
         IC    R0,0(,R5)                                                        
         AR    R5,R0                                                            
*                                                                               
FRMF250  CLI   PERNUM,0            IF WE HAVE 3 HEADING LINES ALL               
         BNE   FRMF255             FLDADR'S AFTER 1ST LINE NEED UPPING          
         L     R3,FULL1            BY 80                                        
         SR    R4,R4                                                            
         BCTR  R5,0                                                             
*                                                                               
FRMF252  LH    R1,2(,R3)                                                        
         LA    R1,80(,R1)                                                       
         STH   R1,2(,R3)                                                        
         IC    R4,0(,R3)                                                        
         BXLE  R3,R4,FRMF252                                                    
         LA    R5,1(,R5)                                                        
*                                                                               
FRMF255  LR    R2,R5               PREPARE TO GENERATE N SCREEN LINES           
         LH    R5,DFSTFLDH         FROM FIRST ONE                               
         AR    R5,R8                                                            
         SR    R2,R5                                                            
         BCTR  R2,0                R2 = LENGTH OF LINE IN TWA MINUS 1           
         LA    R0,13               R0 = COUNT - 11 FOR ACC & CONTRA             
         CLI   BUDFORM,C'P'                   - #MONTHS/#PER LINE FOR P         
         BNE   FRMF258                                                          
         LA    R7,BUDTAB                                                        
         MVC   HALF,BUDTSTA        HALF = START YM FOR FORMAT-P                 
         LLC   R0,BUDTMNUM                                                      
         SRDL  R0,32                                                            
         LLC   R3,PERNUM           & R3 = #M PER LINE                           
         DR    R0,R3                                                            
         LR    R0,R1                                                            
*                                                                               
FRMF258  L     R6,ATIA             R6 = A(SAVED MATRIX) - FOR INPUT=Y           
         B     FRMF265                                                          
*                                                                               
FRMF260  LA    R1,1(R2,R5)         PROPAGATE A LINE                             
         EXMVC R2,0(R1),0(R5)                                                   
         LR    R5,R1                                                            
         SR    RF,RF                                                            
*                                                                               
FRMF262  LH    RE,2(,R1)           RESET FLDADR'S BY A LINE                     
         LA    RE,80(,RE)                                                       
         STH   RE,2(,R1)                                                        
         IC    RF,0(,R1)                                                        
         AR    R1,RF                                                            
         CLI   0(R1),0                                                          
         BNE   FRMF262                                                          
*                                                                               
FRMF265  BAS   RE,SPACFILL         PUT DATE RANGE IN LINE TITLE IF              
         CLI   BUDFORM,C'P'        PERIOD FORMAT                                
         BNE   FRMF267                                                          
         GOTO1 DATED,DMCB,((R3),HALF),8(R5)                                     
         LH    RF,HALF             BUMP YM IN HALF BY PERNUM                    
         AR    RF,R3                                                            
         STH   RF,HALF                                                          
         B     FRMF269                                                          
*                                                                               
FRMF267  CLI   0(R6),0             IF TIA CONTAINS SAVED MATRIX MOVE            
         BE    FRMF269             IN A/C OR C/AC CODE (INPUT=Y)                
         MVC   8(14,R5),8(R6)                                                   
         MVC   5(1,R5),5(R6)       AND LENGTH                                   
         LLC   RF,FPERLINE         AND BUMP TIA POINTER                         
         SR    RE,RE                                                            
*                                                                               
FRMF268  IC    RE,0(,R6)                                                        
         AR    R6,RE                                                            
         BCT   RF,FRMF268                                                       
*                                                                               
FRMF269  BCT   R0,FRMF260          BUMP TO NEXT LINE                            
         CLI   BUDFORM,C'P'        AT END ADD PERIOD FORMAT TOTAL LINE          
         BNE   FRMF280                                                          
         TM    SAVSTAT,TOTAL                                                    
         BNO   FRMF280                                                          
         CLC   MPERLINE,BUDTAB+(BUDTMNUM-BUDTD)                                 
         BE    FRMF280             NOT IF ONLY ONE LINE                         
         LA    R1,1(R2,R5)                                                      
         EXMVC R2,0(R1),0(R5)                                                   
         LR    R5,R1                                                            
         BAS   RE,SPACFILL                                                      
         MVC   8(5,R5),TOTALMSG                                                 
         SR    RF,RF                                                            
*                                                                               
FRMF271  OI    1(R1),PROTECT       PROTECT IT                                   
         LH    RE,2(,R1)                                                        
         LA    RE,80(,RE)                                                       
         STH   RE,2(,R1)                                                        
         IC    RF,0(,R1)                                                        
         AR    R1,RF                                                            
         CLI   0(R1),0                                                          
         BNE   FRMF271                                                          
*                                                                               
FRMF280  LH    R1,2(,R5)           ADD A TAB FIELD AT END                       
         LA    R1,80(,R1)          EITHER ON NEXT LINE                          
         CH    R1,=H'1920'         OR, IF THERE ISN'T ONE IN COL79              
         BL    *+8                 OF THIS LINE                                 
         SH    R1,=H'3'                                                         
         LA    R5,1(R2,R5)                                                      
         MVI   0(R5),9                                                          
         STH   R1,2(,R5)                                                        
         MVI   10(R5),1            CLEAR BEFORE                                 
*                                                                               
FRMF282  LA    R7,BUDTAB           CONVERT BINARY YM TO MM IN BUDTAB            
         LA    R2,13                                                            
         TM    SAVSTAT,THIRTEEN                                                 
         BO    FRMF283                                                          
         BCTR  R2,0                                                             
*                                                                               
FRMF283  LA    RF,BUDTSTA                                                       
         BAS   RE,YMTOMM                                                        
         LA    RF,BUDTSRCE                                                      
         LA    RE,FRMF284                                                       
*                                                                               
YMTOMM   LLC   R1,0(,RF)           CONVERT S/R                                  
         MR    R0,R2                                                            
         IC    R0,1(,RF)                                                        
         BCTR  R0,0                                                             
         AR    R1,R0                                                            
         STH   R1,0(,RF)                                                        
         BR    RE                                                               
*                                                                               
FRMF284  LA    R7,BUDTLEN(,R7)     BUMP TO NEXT                                 
         CLI   0(R7),X'FF'                                                      
         BNE   FRMF283                                                          
         LR    R0,R5                                                            
         SR    R0,R8                                                            
         STH   R0,DLSTFLDH         SAVE DISP TO TAB HDR                         
         LA    R3,VIRMSGH          RETRANSMIT ALL FIELDS                        
         SR    R4,R4                                                            
*                                                                               
FRMF284A OI    6(R3),TRANSMIT                                                   
         MVI   7(R3),1             2260 QUIRK                                   
         IC    R4,0(,R3)                                                        
         BXLE  R3,R4,FRMF284A                                                   
         MVC   FPERLINE,TEMPFPLN   RESET FLDS PER MATRIX LINE                   
*                                                                               
FRMF285  LA    R1,3                SORT BUDGET TABLE TO TYPE # SEQUENCE         
         LA    R2,BUDTAB+(4*BUDTLEN)   GET NUMBER OF ENTRIES -1 IN R1           
         LA    R3,BUDTLEN                                                       
*                                                                               
FRMF285A CLI   0(R2),X'FF'                                                      
         BE    FRMF286                                                          
         SR    R2,R3                                                            
         BCT   R1,FRMF285A                                                      
         B     FRMF290             ONE ENTRY ONLY                               
*                                                                               
FRMF286  LR    R0,R1                                                            
         LA    R2,BUDTAB                                                        
*                                                                               
FRMF287  LA    R3,BUDTLEN(,R2)                                                  
         CLC   0(2,R2),0(R3)                                                    
         BNH   FRMF289                                                          
         XC    0(BUDTLEN,R2),0(R3) SWAP 2 ENTRIES                               
         XC    0(BUDTLEN,R3),0(R2)                                              
         XC    0(BUDTLEN,R2),0(R3)                                              
*                                                                               
FRMF289  LR    R2,R3                                                            
         BCT   R0,FRMF287                                                       
         BCT   R1,FRMF286                                                       
         B     FRMF290                                                          
*                                                                               
FRMF290  TM    SAVSTAT,INPUTISY    EXIT FOR ACC/CAC INPUT IF INPUT=Y            
         BNO   SETFOR3             AND NONE PRESENT                             
         LH    R1,DFSTFLDH         OTHERWISE GO TO PRIMARY READ                 
         AR    R1,R8                                                            
         CLI   8(R1),C' '                                                       
         BH    SETFOR3                                                          
         ST    R1,FADR                                                          
         GOTO1 APROTVAL            PROTECT VALUES NEXT TIME                     
         MVI   NEXTMODE,INPFIRST                                                
         MVC   MSG(21),=C'ENTER CONTRA-ACCOUNTS'                                
         CLI   BUDFORM,C'C'                                                     
         BE    OKEND                                                            
         MVC   MSG+6(08),MSG+13                                                 
         MVC   MSG+14(7),SPACES                                                 
         B     OKEND                                                            
*                                                                               
SETFOR3  MVI   PHASE,3                                                          
         B     OKEND                                                            
*                                                                               
SPACFILL LLC   RF,0(,R5)           SPACE FILL FLD WITH HDR AT R5                
         SH    RF,=H'9'                                                         
         EXMVC RF,8(R5),SPACES                                                  
         BR    RE                                                               
*                                                                               
         DROP  R7                                                               
         EJECT ,                                                                
* EDIT A DATE OR DATE RANGE TO DISPLAY FORM                                     
*                                                                               
* P1 B0  = NUMBER OF MONTHS IN RANGE                                            
* P1 B1-3= A(1ST YM IN BINARY)                                                  
* P2     = A(DISPLAY AREA)                                                      
*                                                                               
* RETURNS LENGTH OF DISPLAY IN P2 B0                                            
         SPACE 1                                                                
DATED    NTR1                                                                   
         LR    R2,R1                                                            
         LM    R3,R4,0(R2)                                                      
         LLC   R6,0(,R2)           R6=#MONTHS                                   
         LLC   RF,1(,R3)           RF=MONTH NUMBER                              
         BAS   RE,EDMON            EDIT MONTH TO MMM OR NN                      
         CLI   PERNUM,0            YEAR REQUIRED IF PERNUM=0                    
         BNE   DATED2                                                           
         LLC   R5,0(,R3)                                                        
         BAS   RE,EDYEAR                                                        
         LA    R4,2(,R4)                                                        
*                                                                               
DATED2   CLI   0(R2),1             SINGLE MONTH ENDS HERE                       
         BE    DATED4                                                           
         AR    RF,R6                                                            
         BCTR  RF,0                                                             
         TM    SAVSTAT,THIRTEEN                                                 
         BO    DATED3                                                           
         CH    RF,=H'12'                                                        
         BNH   *+12                                                             
         SH    RF,=H'12'                                                        
         LA    R5,1(,R5)                                                        
*                                                                               
DATED3   MVI   0(R4),C'-'                                                       
         LA    R4,1(,R4)                                                        
         BAS   RE,EDMON                                                         
         CLI   PERNUM,0                                                         
         BNE   DATED4                                                           
         BAS   RE,EDYEAR                                                        
         LA    R4,2(,R4)                                                        
*                                                                               
DATED4   L     R0,4(,R2)           RETURN LENGTH                                
         SR    R4,R0                                                            
         STC   R4,4(,R2)                                                        
         B     EXIT                                                             
*                                                                               
EDMON    TM    SAVSTAT,THIRTEEN    EDIT MONTH NUMBER IN RF INTO 0(R4)           
         BNO   EDMON2              NUMERIC IF 13MONTH                           
         EDIT  (RF),(2,0(R4)),FILL=0                                            
         LA    R4,2(,R4)                                                        
         BR    RE                  AND BUMP R4                                  
*                                                                               
EDMON2   LR    R1,RF               ALPHA IF NOT 13MONTH                         
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         A     R1,AMONTHS                                                       
         MVC   0(3,R4),0(R1)                                                    
         LA    R4,3(,R4)            BUMP R4                                     
         BR    RE                                                               
*                                                                               
EDYEAR   EDIT  (R5),(2,0(R4))                                                   
         BR    RE                                                               
         EJECT ,                                                                
* CHECK MONTH INPUT                                                             
*                                                                               
* P1 = A(PERIOD)                                                                
         SPACE 1                                                                
CHKMON   NTR1                                                                   
         LR    R2,R1               R2 = ADDR OF PARM LIST                       
         L     R3,0(,R2)           R3 = ADDR OF PERIOD                          
         LR    RF,R3               CHECK FOR CLOSE PARENTHESIS                  
         LA    R0,8                                                             
*                                                                               
CHKMON1  LA    RF,1(,RF)                                                        
         CLI   0(RF),C')'                                                       
         BE    CHKMON2                                                          
         BCT   R0,CHKMON1                                                       
*                                                                               
         MVI   FERN,NOPARNTH                                                    
         B     CHKMERR                                                          
*                                                                               
CHKMON2  LA    RF,1(,RF)           RETURN A(NEXT BYTE AFTER)                    
         ST    RF,0(,R2)                                                        
         LA    R3,1(,R3)                                                        
*                                                                               
CHKMON4  TM    0(R3),X'F0'         HANDLE ALPHA MONTHS                          
         BO    CHKMON10                                                         
         MVI   FERN,INVNUM                                                      
         TM    SAVSTAT,THIRTEEN    IF 13 PERIOD TYPE MUST BE NUMERIC            
         BO    CHKMERR                                                          
         MVI   FERN,INVALID                                                     
         BAS   RE,MTOBIN           CONVERT 1ST MONTH TO BINARY                  
         BNE   CHKMERR             NG,  INVALID INPUT                           
         STC   R1,WORK                                                          
         LR    R5,R1               KEEP 1ST VALUE IN R5                         
         CLI   3(R3),C')'                                                       
         BE    CHKMON19            PROCESS ONE VALID MONTH                      
         CLI   3(R3),C'-'                                                       
         BNE   CHKMERR                                                          
         LA    R3,4(,R3)           CONVERT 2ND MONTH TO BINARY                  
         LA    RE,CHKMON6                                                       
*                                                                               
MTOBIN   LA    RF,MONTHS           CONVERT MMM AT R3 TO BINARY IN R1            
         LA    R1,1                                                             
*                                                                               
MTOBIN2  CLC   0(3,R3),0(RF)                                                    
         BER   RE                                                               
         LA    RF,3(,RF)                                                        
         LA    R1,1(,R1)                                                        
         CH    R1,=H'12'                                                        
         BNH   MTOBIN2             NOT  IN TABLE, INVALID INPUT                 
         BR    RE                                                               
*                                                                               
CHKMON6  BNE   CHKMERR             NG,  INVALID INPUT                           
         B     CHKMON20            PROCESS TWO VALID MONTHS                     
*                                                                               
CHKMON10 MVI   FERN,INVALID                                                     
         BAS   RE,NTOBIN           HANDLE NUMERIC MONTHS/PERIODS                
         STC   R1,WORK                                                          
         LR    R5,R1               KEEP 1ST VALUE IN R5                         
         CLI   0(R3),C')'                                                       
         BE    CHKMON19            PROCESS ONE VALID MONTH                      
         CLI   0(R3),C'-'                                                       
         BNE   CHKMERR                                                          
         LA    R3,1(,R3)                                                        
         MVI   FERN,INVNUM                                                      
         TM    0(R3),X'F0'                                                      
         BNO   CHKMERR                                                          
         LA    RE,CHKMON20         CONVERT 2ND MONTH TO BINARY                  
         MVI   FERN,INVALID                                                     
*                                                                               
NTOBIN   SR    RF,RF               CONVERT NN AT R3 TO BINARY IN R1             
         TM    1(R3),X'F0'         2ND  CHARACTER NUMERIC ?                     
         BNO   NTOBIN10            NO,  SKIP                                    
         LA    RF,1                YES, PACK TWO CHARACTERS                     
         TM    2(R3),X'F0'         3RD  CHARACTER ALSO NUMERIC ?                
         BO    CHKMERR             YES, INVALID INPUT                           
*                                                                               
NTOBIN10 LA    R0,1(RF,R3)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R3)                                                      
         CVB   R1,DUB                                                           
         LR    R3,R0               RETURN BUMPED ADDR IN R3                     
         BR    RE                                                               
*                                                                               
CHKMON19 LR    R1,R5               2ND  VALUE = FIRST                           
*                                                                               
CHKMON20 STC   R1,WORK+1           RETURN 2ND VALUE                             
         SR    R1,R5               CALCULATE # MONTHS                           
         MVI   FERN,STGTREND                                                    
         BNM   CHKMON22                                                         
         TM    SAVSTAT,THIRTEEN    END MONTH LESS THAN START ONLY VALID         
         BO    CHKMERR             IF 12 MONTH FINANCIAL YEAR                   
*                                                                               
         USING BUDTD,R7                                                         
*                                                                               
         TM    BUDTSTAT,FINANCEY   FINANCIAL YEAR OPTION ?                      
*                                                                               
         DROP  R7                                                               
*                                                                               
         BZ    CHKMERR             NO,  START MONTH < END MONTH INVALID         
         CLC   COSTARTM,WORK                                                    
         BH    CHKMERR                                                          
         AH    R1,=H'12'                                                        
*                                                                               
CHKMON22 LA    R1,1(,R1)                                                        
         STC   R1,WORK+2           RETURN # MONTHS                              
         MVI   FERN,INVALID        CHECK RANGE                                  
         CLI   WORK,0                                                           
         BE    CHKMERR                                                          
         CLI   WORK+1,0                                                         
         BE    CHKMERR                                                          
         MVI   FERN,TOOBIG                                                      
         CLI   WORK,13             START MONTH >= 13 ?                          
         BH    CHKMERR             HIGH, INVALID                                
         BL    CHKMON24            LOW,  CONTINUE                               
         TM    SAVSTAT,THIRTEEN    SAME, CHECK FOR 13                           
         BNO   CHKMERR                                                          
*                                                                               
CHKMON24 CLI   WORK+1,13           END   MONTH >= 13 ?                          
         BH    CHKMERR             HIGH, INVALID                                
         BL    OKXIT               LOW,  OKAY                                   
         TM    SAVSTAT,THIRTEEN    SAME, CHECK FOR 13                           
         BNO   CHKMERR                                                          
         B     OKXIT                                                            
*                                                                               
CHKMERR  LTR   R8,R8                                                            
         XIT1                                                                   
*                                                                               
         EJECT ,                                                                
* CHECK YEAR IN BUDGET TYPE FIELD AND CONVERT TO BINARY                         
*                                                                               
* P1 = A(YEAR IN 2 EBCDIC DIGITS)                                               
* RETURNS BINARY YEAR IN WORK(1)                                                
* RETURNS CC=NEQ IF ERROR AND FERN SET                                          
         SPACE 1                                                                
CHKYR    NTR1                                                                   
         L     R2,0(,R1)                                                        
         TM    0(R2),C'0'                                                       
         BNO   CHKYERR                                                          
         TM    1(R2),C'0'                                                       
         BNO   CHKYERR                                                          
         TM    2(R2),C'0'                                                       
         BO    CHKYERR                                                          
         MVC   DUB(2),0(R2)        USE  YY                                      
         MVC   DUB+2(4),=C'0101'   SET  MMDD TO 0101                            
*                                  CONVERT   TO BINARY                          
         GOTO1 VDATCON,DMCB,(0,DUB),(3,WORK)                                    
*                                                                               
         LLC   R1,WORK             GET  YEAR                                    
         MVI   HALF,0                                                           
         MVC   HALF+1(1),TODAYB    CHECK FOR THIS YEAR +/- 5 YRS                
         LA    R1,5(,R1)                                                        
         CH    R1,HALF                                                          
         BL    CHKYERR                                                          
         SH    R1,=H'10'                                                        
         CH    R1,HALF                                                          
         BNH   OKXIT                                                            
*                                                                               
CHKYERR  MVI   FERN,INVYEAR                                                     
         LTR   R8,R8               SET NON-ZERO/UNEQUAL CC                      
         XIT1  ERRXIT                                                           
         EJECT ,                                                                
* INCREMENT BINARY YM BY N MONTHS                                               
*                                                                               
* P1 B0   = N                                                                   
*    B1-3 = A(YM) - INPUT & RETURNED                                            
         SPACE 1                                                                
BUMPMON  NTR1                                                                   
         L     R3,0(,R1)                                                        
         LLC   R0,0(,R1)                                                        
         LLC   RF,1(,R3)                                                        
         AR    RF,R0                                                            
         CH    RF,=H'12'                                                        
         BNH   BUMPMEX                                                          
         LA    R7,12                                                            
         TM    SAVSTAT,THIRTEEN                                                 
         BZ    BUMPM10                                                          
         CH    RF,=H'13'                                                        
         BNH   BUMPMEX                                                          
         LA    R7,13                                                            
*                                                                               
BUMPM10  SR    RF,R7                                                            
         LLC   RE,0(,R3)                                                        
         LA    RE,1(,RE)                                                        
         STC   RE,0(,R3)                                                        
*                                                                               
BUMPMEX  STC   RF,1(,R3)                                                        
         B     EXIT                                                             
         EJECT ,                                                                
* ACCOUNT FILE I/O EXECUTIVE.                                                   
         SPACE 1                                                                
ERROR    CLI   FERN,SPECIAL                                                     
         BE    OMSG                                                             
         GOTO1 VGETMSG,DMCB1,(FERN,MSG),(FNDX,DMCB),0                           
         CLC   XTRAMESS,SPACES                                                  
         BE    OMSG                                                             
         LA    R1,XTRAMESS+L'XTRAMESS-1                                         
         LA    RE,XTRAMESS-1                                                    
         LLC   RF,DMCB1                                                         
*                                                                               
ERROR10  CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,ERROR10                                                       
         SR    R1,RE               R1=L'XTRAMESS                                
         LA    R1,3(RF,R1)         R1=TOTAL MESSAGE LENGTH                      
         LA    RE,L'MSG                                                         
         CR    R1,RE               CHECK MESSAGE FITS                           
         BH    OMSG                                                             
         LA    RF,MSG(RF)          AND IF SO TACK ON EXTRA MESSAGE              
         MVI   0(RF),C'-'                                                       
         MVC   2(L'XTRAMESS,RF),XTRAMESS                                        
*                                                                               
OMSG     MVC   VIRMSG,MSG                                                       
         OI    VIRMSGH+6,TRANSMIT                                               
         L     R1,FADR                                                          
         OI    6(R1),X'40'         SET CURSOR TO FIELD                          
         B     ERRXIT                                                           
         EJECT ,                                                                
* EXITS FROM ROUTINES AND RETURN TO ROOT (USED BY BOTH CSECTS)                  
         SPACE 1                                                                
OKEND    MVI   FERN,OK             OK COMPLETION                                
         B     EXIT                                                             
*                                                                               
OKXIT    SR    R0,R0               CC=EQU FOR OK EXITS FROM ROUTINES            
         B     EXIT                                                             
*                                                                               
ERRXIT   LTR   R8,R8                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT ,                                                                
*              LITERALS ETC                                                     
         SPACE 1                                                                
ACCOUNT  DC    CL8'ACCOUNT'                                                     
DASHES   DC    7C'-'                                                            
TOTALMSG DC    CL38'TOTALS DISPLAYED - HIT ENTER TO UPDATE'                     
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
         SPACE 1                                                                
FLDHEDS  DS    0CL4                1ST HALF OF FLD HDRS FOR SCREEN GEN.         
         DC    X'46280291'         1ST HEADLINE (ONLY USED IF PERNUM=0)         
*                                  IF USED REMAINING FLDADR'S MUST BE           
*                                  INCREMENTED BY ONE LINE (+80)                
         DC    X'55280281'         2ND HEADLINE                                 
         DC    X'552802D1'         3RD HEADLINE                                 
         DC    X'16280321'         LINE TITLE (AS FOR CONTRA FORMAT)            
         DC    X'12000331'         5 VALUE FIELDS (4+TOTAL)                     
         DC    X'1200033E'                                                      
         DC    X'1200034B'                                                      
         DC    X'12000358'                                                      
         DC    X'12000365'                                                      
*                                                                               
FLDHNAME DC    X'2D000331'         NAME FIELD FOR NAME OPTION                   
*                                                                               
MONLIMIT DC    H'36'               MAX NUMBER OF MONTHS PER BUDGET REC          
*                                                                               
CATAB    DS    0CL9                                                             
         DC    C'ACCOUNT',X'E000'                                               
         DC    C'BUDGET ',X'A012'                                               
         DC    C'CONTRA ',X'A011'                                               
         DC    C'INPUT  ',X'0011'                                               
         DC    C'LEDGER ',X'C000'                                               
CATABALL DC        7X'00',X'0000'                                               
CATABONE DC        7X'00',X'6000'                                               
CATABNUL DC        7X'00',X'0011'                                               
         SPACE 1                                                                
FRMTAB   DS    0CL12               TABLE OF FORMATS COVERED BY FRMTBD           
FRMTP    DC    C'PERIOD ',X'0401030C00'                                         
FRMTC    DC    C'CONTRA ',X'010103FF00'                                         
         DC    C'CONTRA ',X'04000C0000'                                         
FRMTA    DC    C'ACCOUNT',X'010103FF00'                                         
         DC    C'ACCOUNT',X'04000C0000'                                         
         DC    X'00'                                                            
         SPACE 1                                                                
MONTHS   DC    2CL36'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                      
         SPACE 1                                                                
PERTAB   DS    0CL8                TABLE OF PERIOD KEYWORDS + #MONTHS           
         DC    C'MONTH  ',X'01'                                                 
         DC    C'PERIOD ',X'01'                                                 
         DC    C'QUARTER',X'03'                                                 
         DC    C'YEAR   ',X'0C'                                                 
         DC    C'ANNUAL ',X'0C'                                                 
         DC    C'NAME   ',X'FF'                                                 
         DC    X'00'                                                            
         EJECT ,                                                                
*              DSECT TO COVER FORMAT TABLE FRMTAB                               
         SPACE 1                                                                
FRMTBD   DSECT                                                                  
FRMTNAME DS    CL7       C         FORMAT KEYWORD                               
FRMTBTNO DS    CL1       B         NUMBER OF BUDGET TYPES ALLOWED (1-4)         
FRMTPERS DS    CL4       X         PERIOD PARAMETER NUMBERS ALLOWED             
*                                  FORMAT PERTAB - MAX OF 1                     
*                                  INITIAL NULL = NOT REQUIRED                  
FRMTLEN  EQU   *-FRMTBD                                                         
         EJECT ,                                                                
*              DSECT TO COVER CONTRA TABLE ENTRY                                
         SPACE 1                                                                
CATBD    DSECT                                                                  
CATBKEYW DS    CL7       C         KEYWORD                                      
CATBSTAT DS    CL1       X         STATUS - X'80'= VALID ONLY IF ADD            
*                                           X'40'= HAS LEDGER DATA              
*                                           X'20'= CHECK ACC RECORD             
CATBS15  DS    CL1       B         DEFAULT DISP. TO SOURCE OF 15B KEY           
CATBLEN  EQU   *-CATBD                                                          
*&&DO                                                                           
         EJECT ,                                                                
* GET NEXT PRIMARY OR SECONDARY ACCOUNT FOR TRANSACTION SEQUENCE                
*                                                                               
* ADDRESS OF PRIMARY OR SECONDARY I/O CONTROL BLOCK IS ADDRESSED BY R1          
* SEE DSECT IOCBD FOR DETAILS                                                   
*                                                                               
* RETURN WITH ACCODE=15BYTE A/C KEY OR NULLS IF NOT FOUND                       
*             ACCTWA=14BYTE DISPLAY A/C CODE                                    
*             ACCNAM=36BYTE NAME                                                
*             AACCIO=A(RECORD) IF ACCOUNT CALL NOT CONTRA                       
*             CC=NEQ IF NOT FOUND                                               
*                                                                               
* IF CALL IS FOR INPUT=Y ACCTWA CONTAINS INPUT CODE ON INPUT AND                
* CC=NEQ IS RETURNED IF ERROR (EG LEDGER INVALID/INCOMPATIBLE-B.TYPE)           
         SPACE 1                                                                
         USING IOCBD,R7                                                         
         USING ACKEYD,R5           R5 = A(RECORD)                               
         SPACE 1                                                                
GETACC   CSECT                                                                  
         NMOD1 0,*BUDGET*                                                       
         L     RC,ASAVE                                                         
         L     R7,0(,R1)                                                        
         LA    R1,ACCNTRL                                                       
         MVI   ACCTYPE,C'A'        SET ACCTYPE TO ACC OR CAC                    
         CR    R7,R1                                                            
         BE    *+8                                                              
         MVI   ACCTYPE,C'C'                                                     
         C     R7,APRIMIOC         IF PRIMARY CALL, CLEAR CURRENT               
         BNE   GETA02              SECONDARY KEY                                
         L     R1,ASECDIOC                                                      
         XC    0(L'IOCNOWKY,R1),0(R1)                                           
         CLI   BUDFORM,C'A'        IF FORMAT IS ACCOUNT AND ITS DRIVEN          
         BNE   GETA02              BY BUDGET RECS SKIP PRIMARY IO 1ST           
         CLI   ACTION,ADD          TIME                                         
         BE    GETA02                                                           
         OC    IOCNOWKY,IOCNOWKY                                                
         BNZ   ERRXIT                                                           
         MVI   IOCNOWKY,X'FF'      FORCE IT TO FAIL 2ND TIME                    
         XC    ACCTWA,ACCTWA                                                    
         B     OKXIT                                                            
*                                                                               
GETA02   XC    ACCODE,ACCODE       INITIALIZE RETURN VALUES                     
         MVC   ACCNAM,SPACES                                                    
         XC    KEY,KEY                                                          
         CLI   IOCLOKEY,X'01'                                                   
         BE    GETA03                                                           
         XC    ACCTWA,ACCTWA                                                    
         B     GETA04                                                           
*                                                                               
GETA03   DS    0H                  HANDLE INPUT=YES CALL                        
         LLC   R1,ACCNTRL+(IOCDS15-IOCBD)                                       
         LA    R1,ACCNTRL(R1)                                                   
         MVC   KEY,SPACES          BUILD A KEY FOR CONTRA-HEADER                
         MVC   KEY(15),0(R1)       ACKEYACC FROM CURRENT A/C KEY                
         LLC   R1,IOCDS14                                                       
         LA    R1,KEY(R1)          ACKEYCON FROM INPUT CODE                     
         CLI   IOCDS14,18                                                       
         BNE   *+10                                                             
         MVC   KEY+17(1),COMPANY                                                
         MVC   0(14,R1),ACCTWA                                                  
         L     R5,AIOAREA1                                                      
         GOTO1 AREAD,AIOAREA1      READ FOR IT                                  
         BE    GETA18                                                           
         MVC   ACKEYD(42),KEY      IF NOT FOUND PUT DUMMY IN IOAREA             
         MVI   ACRECORD,0          WITH NO ELS                                  
         B     GETA18                                                           
*                                                                               
GETA04   CLC   IOCHIKEY,SPACES     HANDLE 'ALL' CASES (CONTRA=SPACES)           
         BNE   GETA06                                                           
         OC    IOCNOWKY,IOCNOWKY                                                
         BNZ   ERRXIT                                                           
         MVC   IOCNOWKY,SPACES                                                  
         MVC   ACCODE(L'ACCODE+L'ACCTWA),SPACES                                 
         MVC   ACCTWA(3),=CL4'ALL'                                              
         B     OKXIT                                                            
*                                                                               
GETA06   OC    IOCLOKEY,IOCLOKEY   IF LOW KEY IS NULLS ITS A CONTRA             
         BNZ   GETA08                                                           
         LLC   RE,IOCDLEND         THAT IS CONTROLLED BY THE ACCOUNT            
         BCTR  RE,0                                                             
         EXCLC RE,IOCNOWKY,ACCNTRL                                              
         BNL   GETA10              WERE PAST KEY OF ACCOUNT                     
         CLI   ACCNTRL,ACBTKTEQ    WERE NOT - BUDGET REC WILL DO                
         BE    GETA07                                                           
         LLC   RF,IOCDLSTA         ACCOUNT REC IS OK IF                         
         LA    R1,ACCNTRL(RF)      IT HAS A CONTRA VALUE                        
         SR    RE,RF                                                            
         EXCLC RE,0(R1),SPACES                                                  
         MVC   IOCNOWKY,ACCNTRL                                                 
         BE    GETA10              NO - SO GO TO READ HIGH FROM IT              
*                                                                               
GETA07   L     R5,AACCIO           YES - SO USE IT (IT MUST BE IN W/S)          
         B     GETA18                                                           
*                                                                               
GETA08   OC    IOCNOWKY,IOCNOWKY   CURRENT KEY IS NULLS SO WE START             
         BNZ   GETA10              FROM LOW KEY                                 
         MVC   KEY(L'IOCLOKEY),IOCLOKEY                                         
         B     GETA12                                                           
*                                                                               
GETA10   LLC   RE,IOCDLEND         OTHERWISE BUMP CURRENT KEY BY                
         LA    RE,KEY(RE)          FORCING TO NEXT AT REQUIRED LEVEL            
         MVC   KEY(L'IOCNOWKY),IOCNOWKY                                         
         MVI   0(RE),X'FF'                                                      
*                                                                               
GETA12   LA    R1,ABUDREC          READ HIGH                                    
         CLI   KEY,ACBTKTEQ                                                     
         BE    *+8                                                              
         LA    R1,AIOAREA1                                                      
         GOTO1 ARDHI                                                            
*                                                                               
GETA13   TM    DMCB+8,X'FD'        SKIP DELETES                                 
         BNZ   ERRXIT                                                           
         TM    DMCB+8,2                                                         
         BNO   *+14                                                             
         L     RF,ASEQ                                                          
         BASR  RE,RF                                                            
         B     GETA13                                                           
         CLI   ACCTYPE,C'A'                                                     
         BNE   *+10                                                             
         MVC   AACCIO,AIOAREA      SAVE ADDR IF ACC CALL                        
         L     R5,AIOAREA                                                       
*                                                                               
GETA14   OC    IOCHIKEY,IOCHIKEY   CHECK FOR CONTROL BREAKS                     
         BNZ   GETA16              IF ITS A CONTRA CONTROLLED BY ACC            
         LLC   RE,IOCDS15          A/C LEN IS DISP TO C/A CODE(NO WC)           
         BCTR  RE,0                                                             
         EXCLC RE,ACKEYACC,ACCNTRL CHECK WHETHER ACC HAS CHANGED                
         BE    GETA18                                                           
         B     ERRXIT              IF SO NOT FOUND                              
*                                                                               
GETA16   CLC   IOCHIKEY,ACKEYACC   OTHERWISE CHECK IF WERE PAST HIGHKEY         
         BNL   GETA18                                                           
         CLC   IOCLOKEY,IOCHIKEY                                                
         BNE   ERRXIT                                                           
         OC    IOCNOWKY,IOCNOWKY                                                
         BNE   ERRXIT                                                           
         MVC   ACKEYD(42),KEYSAVE  SINGLE CAC DOESNT HAVE TO EXIST              
         MVI   ACRECORD,0                                                       
*                                                                               
GETA18   MVC   IOCNOWKY,ACKEYACC   RESET CURRENT KEY                            
*                                                                               
GETA20   CLI   ACCTYPE,C'C'        CHECK FOR CHANGE OF CONTRA LEDGER            
         BNE   GETA30                                                           
         CLI   BUDRULES,C'+'       UNLESS NON-STANDARD                          
         BE    GETA35                                                           
         LLC   R2,IOCDS15                                                       
         LA    R2,ACKEYACC(R2)                                                  
         CLC   IOCLCODE,1(R2)                                                   
         BE    GETA35              SAME                                         
         LA    R3,BUDRULES         DIFFERENT - IS IT VALID FOR BUDTYPES         
         OC    BUDRULES(2),SPACES                                               
         LA    R0,5                                                             
         MVI   WORK,X'FF'          (GET NEXT HIGHER VALID UL INTO WORK)         
*                                                                               
GETA22   CLI   0(R3),X'FF'                                                      
         BE    GETA26                                                           
         CLC   0(2,R3),1(R2)                                                    
         BE    GETA28              IT IS VALID                                  
         BL    GETA24                                                           
         CLC   0(2,R3),WORK                                                     
         BH    GETA24                                                           
         MVC   WORK(3),0(R3)                                                    
*                                                                               
GETA24   LA    R3,3(,R3)                                                        
         BCT   R0,GETA22                                                        
*                                                                               
GETA26   CLI   IOCLOKEY,X'01'      NOT VALID - ERROR FOR INPUT=YES              
         BNE   *+12                                                             
         MVI   FERN,BTYPCALE                                                    
         B     ERRXIT                                                           
         CLI   WORK,X'FF'                                                       
         BE    ERRXIT              NO OTHER UL IS                               
         MVC   1(2,R2),WORK        ANOTHER IS                                   
*                                                                               
GETA27   XC    3(42,R2),3(R2)                                                   
         MVC   KEY,ACKEYD                                                       
         B     GETA12              GO AND READ HIGH FOR IT                      
*                                                                               
GETA28   DS    0H                  CHECK NEW LEDGER                             
         GOTO1 ACHECKUL,DMCB,1(R2),(R7)                                         
         BE    GETA29              OK                                           
         TM    SAVSTAT,INPUTISY                                                 
         BO    ERRXIT              LEDGER INVALID FOR INPUT=Y                   
         LLC   R1,2(,R2)           OTHERWISE READ HIGH                          
         LA    R1,1(,R1)                                                        
         STC   R1,2(,R2)                                                        
         B     GETA27                                                           
*                                                                               
GETA29   LLC   R1,2(,R3)           RESET LEVEL DATA                             
         GOTO1 ALEVSET                                                          
*                                                                               
GETA30   CLI   ACCTYPE,C'A'        CHECK SECURITY IF ACCOUNT CALL               
         BNE   GETA35                                                           
         CLI   ACBTKTYP,ACBTKTEQ   IF ITS A BUDGET REC - GET ACCOUNT            
         BNE   GETA32                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACKEYACC),ACBTKACC                                         
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   GETA10              CANT FIND A/C - SO SKIP TO NEXT              
*                                                                               
GETA32   GOTO1 AGETNAME,AIOAREA                                                 
         MVC   ACCNAM,WORK                                                      
         GOTO1 ASECHECK,DMCB,AIOAREA,(R7)                                       
         BE    GETA35                                                           
         MVC   ACCNAM,SPACES       IF LOCKOUT OPTIMAL NEXT KEY FOR              
         B     GETA12              READ HIGH RETURNED IN KEY                    
*                                                                               
GETA35   CLI   ACBTKTYP,ACBTKTEQ   CHECK LEVEL (MUSTNT BE ALL SPACES            
         BE    GETA40              BETWEEN START & END OF REQUIRED LEV)         
         LLC   RE,IOCDLSTA                                                      
         LLC   RF,IOCDLEND                                                      
         SR    RF,RE                                                            
         LA    RE,ACKEYD(RE)                                                    
         BCTR  RF,0                                                             
         EXCLC RF,0(RE),SPACES                                                  
         BNE   GETA40                                                           
         CLI   IOCLOKEY,X'01'                                                   
         BNE   GETA10                                                           
*                                                                               
GETA37   MVI   FERN,WRONGLEV       ERROR IF INPUT=YES                           
         B     ERRXIT                                                           
*                                                                               
GETA40   CLI   ACCTYPE,C'C'        CHECK FOR CONTRA LEVEL TOO LOW               
         BNE   GETA42              BY SEEING IF THERE ARE NON-SPACES            
         CLI   ACTION,ADD          BEYOND END OF REQUIRED LEVEL                 
         BNE   GETA42                                                           
         LLC   R1,IOCDLEND                                                      
         LA    RF,ACBTKBNO-ACBTKEY                                              
         SR    RF,R1                                                            
         SH    RF,=H'1'                                                         
         BM    GETA42                                                           
         LA    R1,ACKEYACC(R1)                                                  
         EXCLC RF,0(R1),SPACES     IF THERE ARE                                 
         BE    GETA42                                                           
         CLI   IOCLOKEY,X'01'      IF INPUT=YES                                 
         BE    GETA37              IT'S AN ERROR                                
         EXMVC RF,0(R1),SPACES     OTHERWISE INSERT TRAILING SPACES             
         MVC   IOCNOWKY,ACKEYACC   TO RAISE LEVEL                               
         LH    RF,DATADISP                                                      
         AR    RF,R5                                                            
         MVI   0(RF),0             HIDE ELS AS THIS NAME IS NOT RIGHT           
*                                                                               
GETA42   DS    0H                  FILTERS                                      
         LA    R4,CACNTRL                                                       
CAC      USING IOCBD,R4                                                         
         LLC   RE,CAC.IOCDS14                                                   
         LA    RE,ACKEYD(RE)                                                    
         LA    RF,L'IOCFILT-1                                                   
         CLI   ACCTYPE,C'C'                                                     
         BE    GETA44                                                           
         CLI   BUDFORM,C'A'        ACCOUNT FORMAT FILTERING ON BUDGET           
         BNE   GETA60              RECORDS - CAFILTER = SINGLE CAC              
         CLI   ACTION,ADD                                                       
         BE    GETA60                                                           
         MVC   CACNTRL(L'IOCNOWKY),IOCNOWKY  EQUATE CAC AND ACC KEYS            
         CLC   CAC.IOCFILT,0(RE)                                                
         BE    GETA50                                                           
         BL    GETA10                                                           
         B     GETA46                                                           
*                                                                               
GETA44   OC    CAC.IOCFILT,CAC.IOCFILT APPLY FILTER IF CONTRA CALL              
         BZ    GETA50                                                           
         LA    R1,CAC.IOCFILT(RF)                                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-12                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   CAC.IOCFILT(0),0(RE)                                             
         BE    GETA50              REQUIRED                                     
         BL    ERRXIT                                                           
*                                                                               
GETA46   XC    0(42,RE),0(RE)                                                   
         MVC   0(L'IOCFILT,RE),CAC.IOCFILT                                      
         MVC   KEY,ACKEYD                                                       
         B     GETA12                                                           
         DROP  CAC                                                              
*                                                                               
GETA50   TM    SAVSTAT,NAME        GET CONTRA NAME IF REQUIRED                  
         BO    *+14                                                             
         CLC   ACCTYPE,BUDFORM                                                  
         BE    GETA60                                                           
         MVC   KEYSAVE,ACKEYD                                                   
         ST    R5,AIOAREA                                                       
*                                                                               
GETA52   GOTO1 AGETNAME,AIOAREA    GET NAME INTO WORK FROM BUD/ACC/CAC          
         CLC   WORK,SPACES         RECORD                                       
         BNE   GETA59              FOUND ONE                                    
         CLI   ACCTYPE,C'A'        WE DIDNT FIND ONE -BUT WE HAVE ACC           
         BE    GETA60                                                           
*                                                                               
GETA54   MVC   KEY,SPACES          WE DIDN'T FIND ONE                           
         CLI   KEYSAVE,ACBTKTEQ    IF THIS WAS A BUDGET REC, TRY THE            
         BNE   GETA56              CONTRA HEADER                                
         MVC   KEY(32),KEYSAVE+1                                                
         B     GETA58                                                           
*                                                                               
GETA56   CLC   KEYSAVE+15(17),SPACES    IF THIS WAS A CONTRA HEADER TRY         
         BE    GETA60              THE ACCOUNT, IF AN ACCOUNT GIVE UP           
         MVC   KEY(15),KEYSAVE+17                                               
*                                                                               
GETA58   GOTO1 ARDHI,AIOAREA2                                                   
         BNE   GETA54                                                           
         L     R1,0(,R1)                                                        
         CLC   KEYSAVE,0(R1)                                                    
         BE    GETA52                                                           
         B     GETA54                                                           
*                                                                               
GETA59   MVC   ACCNAM,WORK                                                      
*                                                                               
GETA60   LLC   RE,IOCDS15          RETURN ACCODE & ACCTWA                       
         LA    R1,14                                                            
         LLC   RF,IOCDS14                                                       
         AR    R1,RE                                                            
         SR    R1,RF                                                            
         LA    RE,IOCNOWKY(RE)                                                  
         LA    RF,IOCNOWKY(RF)                                                  
         MVC   ACCODE,0(RE)                                                     
         MVC   ACCTWA,SPACES                                                    
         EXMVC R1,ACCTWA,0(RF)                                                  
         CLC   ACCTWA,SPACES                                                    
         BNE   OKXIT                                                            
         MVC   ACCTWA(3),=CL4'ALL'                                              
         B     OKXIT                                                            
         DROP  R5,R7                                                            
*&&                                                                             
         LTORG                                                                  
         EJECT ,                                                                
* ACBUDDSECT                                                                    
       ++INCLUDE ACBUDDSECT                                                     
         EJECT ,                                                                
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACBUD01   08/05/08'                                      
         END                                                                    
