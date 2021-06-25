*          DATA SET ACREPA202  AT LEVEL 071 AS OF 05/01/02                      
*PHASE ACA202A,+0                                                               
*INCLUDE SQUASHER                                                               
*INCLUDE ACSLRY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE UNDERLIN                                                               
*INCLUDE SORTER                                                                 
*INCLUDE PERVERT                                                                
         TITLE 'ACA202 -  TIME/COST ALLOCATION '                                
ACA202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACA2**                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING ACA202+4096,R9                                                   
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GENERAL W/S)                            
         LA    RC,SPACEND                                                       
         USING A202D,RC            RC=A(LOCAL W/S)                              
         EJECT                                                                  
*              FIRST FOR RUN                                                    
         SPACE 1                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   CA10                                                             
         RELOC RELO                                                             
         LA    RE,RELOTAB          RELOCATE A-TYPES                             
         LA    R1,ATYPES                                                        
RELOOP   L     RF,0(RE)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
         SPACE 1                                                                
         MVI   POSTERR,C'N'                                                     
         XC    ID,ID                                                            
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(3),=C'AA2'                                                  
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
         ZAP   POSTCASH,=P'0'                                                   
         ZAP   POSTREC,=P'0'                                                    
         ZAP   PCREDITS,=P'0'                                                   
         ZAP   PDEBITS,=P'0'                                                    
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         GOTO1 DEPGRP,DMCB,(RC)    BUILD DEPT/GROUP TABLE                       
         B     XIT                                                              
         SPACE 2                                                                
XIT      XMOD1 1                                                                
         EJECT                                                                  
*              FIRST FOR REQUEST                                                
         SPACE 1                                                                
CA10     CLI   MODE,REQFRST                                                     
         BNE   LDG00                                                            
         SPACE 1                                                                
         LA    R7,AGYCUM                                                        
         USING AGYD,R7                                                          
         LA    R2,AGBK             CLEAR AGENCY CUM TABLE                       
         LA    R3,AGBKCNT                                                       
         ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,*-10                                                          
         SPACE 1                                                                
         USING BUFD,R5                                                          
         LA    R5,BUFWRK                                                        
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         LA    R2,BUFBK            CLEAR BUFFALO BUCKETS                        
         LA    R3,BBKCNT                                                        
         ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,*-10                                                          
         SPACE 1                                                                
         L     R5,SUMBUFF          CLEAR SUMMARY TABLE                          
         USING BIND,R5                                                          
         XC    BININ,BININ         CLEAR NUMBER IN CLIENT RECORD                
         SPACE 1                                                                
         MVC   OPTIONS,STDOPTS                                                  
         OC    PROGPROF,PROGPROF                                                
         BZ    CA102                                                            
         LA    R1,PROGPROF                                                      
         LA    RE,OPTIONS                                                       
         LA    RF,L'STDOPTS                                                     
CA101    CLI   0(R1),0                                                          
         BE    *+10                                                             
         MVC   0(1,RE),0(R1)                                                    
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   RF,CA101                                                         
         SPACE 1                                                                
CA102    XC    PROFKEY,PROFKEY                                                  
         MVC   PROFKEY(4),=C'A000'                                              
         MVC   PROFKEY+2(2),=C'A3'                                              
         MVC   PROFKEY+4(1),QCOMPANY                                            
         MVC   PROFKEY+5(2),=C'1R'    UNIT/LEDGER                               
         MVC   PROFKEY+12(2),ALPHAID                                            
         GOTO1 GETPROF,PROFPARA,PROFKEY,PROGPRF2,DATAMGR                        
         OC    PROGPRF2,PROGPRF2                                                
         BZ    CA104                                                            
         LA    R1,PROGPRF2                                                      
         LA    RE,OPTA3                                                         
         LA    RF,L'STDA3OP                                                     
CA103    CLI   0(R1),0                                                          
         BE    *+10                                                             
         MVC   0(1,RE),0(R1)                                                    
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   RF,CA103                                                         
CA104    MVI   POST,C'N'                                                        
         CLI   QOPT1,C'P'                                                       
         BNE   CA10A                                                            
         MVI   POST,C'Y'                                                        
CA10A    MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         CLI   OPTYTD,C'Y'                                                      
         BE    CA11Y                                                            
         CLI   OPTYTD,C'Q'                                                      
         BE    CA11Q                                                            
CA11M    CLC   QSTART(4),QEND      MONTHLY DATES MUST BE EQUAL                  
         BE    CA11Y                                                            
         B     BADATE                                                           
CA11Q    MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         L     R6,=F'65'                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R6)                                      
         CLC   QEND(4),WORK+6                                                   
         BE    CA11Y                                                            
         B     BADATE           QTR. ALLOC MUST HAVE QUARTERLY DATES            
CA11Y    DS    0H                                                               
         MVC   QSTART+4(2),=C'01'                                               
         MVC   QEND+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(1,START)                                 
         GOTO1 (RF),(R1),(0,QEND),(1,END)                                       
         GOTO1 (RF),(R1),,(9,HEADMON)                                           
         MVC   STEND(2),START                                                   
         MVC   STEND+2(2),END                                                   
         MVC   MOS(1),QEND+1                                                    
         MVC   MOS+1(1),QEND+3                                                  
         CLI   QEND+2,C'1'                                                      
         BNE   CA12                                                             
         MVI   MOS+1,C'A'                                                       
         CLI   QEND+3,C'0'                                                      
         BE    CA12                                                             
         MVI   MOS+1,C'B'                                                       
         CLI   QEND+3,C'1'                                                      
         BE    CA12                                                             
         MVI   MOS+1,C'C'                                                       
         SPACE 1                                                                
CA12     L     R3,=F'-15'          WORK OUT LAST MONTH/QTR DATE                 
         CLI   OPTPERD,C'M'                                                     
         BE    *+8                                                              
         L     R3,=F'-75'                                                       
         GOTO1 ADDAY,DMCB,QEND,WORK,(R3)                                        
         GOTO1 DATCON,DMCB,(0,WORK),(1,LAST)                                    
         SPACE 1                                                                
         ZAP   NUMONTHS,=P'1'                                                   
         CLC   QSTART(4),QEND                                                   
         BE    CA14                                                             
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK+6(4),QEND                                                   
         MVC   WORK+10(2),=C'01'                                                
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         ZIC   R2,DMCB+15                                                       
         CVD   R2,DUB                                                           
         ZAP   NUMONTHS,DUB+6(2)                                                
         SPACE 1                                                                
CA14     GOTO1 BUILDIT,DMCB,(RC)   BUILD SKELETON POSTING RECORD                
         SPACE 1                                                                
         XC    FISCAL,FISCAL                                                    
         CLI   OPTOVHY,C'Y'        OVERHEAD ON YEAR TO DATE                     
         BNE   CA17                                                             
         CLI   OPTYTD,C'Y'                                                      
         BNE   *+6                                                              
         DC    H'0'           CAN'T HAVE OPTOVHY=Y IF OPTYTD=Y                  
*                             GET START OF FINANCIAL YEAR                       
         MVC   FISCAL,END                                                       
         MVI   FISCAL+1,1          FORCE JANUARY                                
         L     R4,ADCMPEL                                                       
         USING ACCOMPD,R4                                                       
         CLI   ACMPSTM,0           WAS A STARTING MONTH SPECIFIED               
         BE    CA15                NO LEAVE AS 01 AND CHECK START               
         MVC   WORK(1),ACMPSTM                                                  
         NI    WORK,X'0F'                                                       
         ZIC   R1,WORK             FINANCIAL YEAR                               
         TM    ACMPSTM,X'F0'       WAS IT A NUMBER?                             
         BO    *+8                                                              
         LA    R1,15(0,R1)         NO-ADD CONVERSION FACTOR                     
         STC   R1,FISCAL+1                                                      
*                                                                               
CA15     CLC   FISCAL,END                                                       
         BNH   CA17                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(1),FISCAL                                                   
         MVC   WORK+1(2),=X'0101'  SET JAN 01 AND GET PREV YEAR                 
         GOTO1 DATCON,DMCB,(X'31',WORK),(1,WORK),(5,0)                          
         MVC   FISCAL(1),WORK      YY PACKED                                    
*                                                                               
CA17     DS    0H                                                               
         CLI   OPTEXP,C'Y'                                                      
         BNE   CA17A                                                            
         GOTO1 EXPENSE,DMCB,(RC)   GET EXPENSES                                 
         SPACE 1                                                                
CA17A    CLC   ALPHAID,=C'BS'                                                   
         BNE   XIT                                                              
         USING BCLID,RF                                                         
         LA    RF,NMEWRK                                                        
         MVC   NMEWRK,SPACES                                                    
         MVC   BCLICDE(7),=C'1ZZZXXX'  ADD BSNY DUMMY CLIENT TO TABLE           
         MVC   BCLINME(9),=C'CORPORATE'                                         
         GOTO1 NMEPUT,DMCB,(RC)                                                 
         B     XIT                                                              
         DROP  RF                                                               
         SPACE 1                                                                
BADATE   MVC   P+1(33),=CL33'** ERROR ** REQUEST DATES INVALID'                 
         GOTO1 ACREPORT                                                         
         MVI   POSTERR,C'Y'                                                     
         B     CA11Y                                                            
         EJECT                                                                  
*              FIRST FOR LEDGER                                                 
         SPACE 1                                                                
LDG00    CLI   MODE,LEDGFRST                                                    
         BNE   CA20                                                             
         USING ACLELD,RF                                                        
         L     RF,ADLDGHIR                                                      
         LA    RE,ACLVALS           SAVE LENGTH OF LEVELS                       
         LA    R0,4                 ASSUME 4 LEVELS                             
         LA    R1,LLEVA                                                         
         USING ACLVALS,RE                                                       
         MVC   0(1,R1),ACLVLEN      COMBINED LENGTH                             
         LA    RE,L'ACLVALS(RE)                                                 
         LA    R1,L'LLEVA(R1)                                                   
         BCT   R0,*-14                                                          
*                                                                               
         L     R4,ADCMPEL                                                       
         USING ACCOMPD,R4                                                       
         MVC   HALF,=H'1'                                                       
         TM    ACMPSTA4,X'01'      ON TWO BYTE OFFICES                          
         BZ    *+10                                                             
         MVC   HALF,=H'2'                                                       
         SR    R1,R1                                                            
         IC    R1,LLEVA                                                         
         CH    R1,HALF             1ST LEVEL MUST BE 2 CHARS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,LLEVA                                                         
         LA    R3,LENLEVA                                                       
         SR    R4,R4                                                            
         LA    R0,4                ASSUME 4 LEVELS                              
LDG02    DS    0H                                                               
         SR    R5,R5                                                            
         IC    R5,0(R1)            PREVIOUS COMBINED LENGTH                     
         SR    R5,R4               MINUS NEW COMBINED LENGTH                    
         BP    *+6                 EQUALS INDIVIDUAL LEVEL LEN                  
         DC    H'0'                                                             
         STC   R5,0(R3)            SAVE INDIVD LENGTH OF LEVEL                  
         CLI   0(R1),12            LAST LEV HAS MAXLEN FOR ACCT                 
         BE    LDG04                                                            
         SR    R4,R4                                                            
         IC    R4,0(R1)            COMBINED LENGTH IN R4                        
         LA    R1,1(R1)            BUMP TO NEXT COMBINED LENGTH                 
         LA    R3,1(R3)            NEXT INDIVDUAL LEN SAVE AREA                 
         BCT   R0,LDG02                                                         
         DC    H'0'                                                             
         DROP  RE,RF                                                            
         SPACE 1                                                                
         USING SUMD,R8                                                          
LDG04    LA    R8,AGYTOT                                                        
         BAS   RE,CLSUM            CLEAR AGENCY TOTAL BUCKETS                   
         MVC   SOFFDEP,=12X'FF'    SET AGENCY KEY                               
         LA    R8,AGY99                                                         
         BAS   RE,CLSUM            CLEAR AGENCY 99 TOTAL                        
         SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SOFFDEP(0),=12C'9'                                               
         LA    RE,SOFFDEP                                                       
         SR    R0,R0                                                            
         IC    R0,LENLEVA                                                       
         AR    RE,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),=12X'FF'                                                 
         LA    R8,OFF99                                                         
         BAS   RE,CLSUM            CLEAR OFFICE 9/DEPT 99                       
         MVC   SOFFDEP,=12C'9'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              FIRST FOR OFFICE                                                 
         SPACE 1                                                                
CA20     CLI   MODE,LEVAFRST                                                    
         BNE   CA30                                                             
         LA    R8,OFFTOT                                                        
         BAS   RE,CLSUM            CLEAR OFFICE TOTAL BUCKETS                   
         L     R2,ADHEIRA                                                       
         MVC   SOFFDEP,3(R2)                                                    
         LA    R2,SOFFDEP                                                       
         SR    R0,R0                                                            
         IC    R0,LENLEVA                                                       
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),=12X'FF'    SET OFFC TOT KEY                             
         LA    R8,DEPT99                                                        
         BAS   RE,CLSUM            CLEAR DEPT99 BUCKETS                         
         L     R2,ADHEIRA                                                       
         MVC   SOFFDEP,3(R2)                                                    
         LA    R2,SOFFDEP                                                       
         SR    R0,R0                                                            
         IC    R0,LENLEVA                                                       
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),=12C'9'     SET DEPT 99 KEY                              
         SPACE 1                                                                
         LA    R7,OFFCUM                                                        
         USING OFFD,R7                                                          
         LA    R2,OFBK             CLEAR OFFICE CUM TABLE                       
         LA    R3,OFBKCNT                                                       
         ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
*              FIRST FOR DEPARTMENT                                             
         SPACE 1                                                                
CA30     CLI   MODE,LEVBFRST                                                    
         BNE   CA35                                                             
         USING SUMD,R8                                                          
         LA    R8,DEPTOT           INITIALIZE OFFICE /DEPT RECORD               
         BAS   RE,CLSUM                                                         
         L     R2,ADHEIRB                                                       
         MVC   SOFFDEP,3(R2)       OFFICE / DEPT                                
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   IND,C'N'                                                         
         SPACE 1                                                                
         LA    R7,DEPCUM                                                        
         USING DEPD,R7                                                          
         LA    R2,DEBK             CLEAR DEPT CUM TABLE                         
         LA    R3,DEBKCNT                                                       
         ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
*              FIRST FOR SUB DEPARTMENT                                         
         SPACE 1                                                                
CA35     CLI   MODE,LEVCFRST                                                    
         BNE   CA40                                                             
         ZAP   PCTTOT,=P'0'                                                     
         ZAP   PCTBENEF,=P'0'                                                   
         ZAP   PCTADMIN,=P'0'                                                   
         L     R2,ADHEIRA         CHECK BENE PCTS                               
         BAS   RE,EMPBEN                                                        
         L     R2,ADHEIRB                                                       
         BAS   RE,EMPBEN                                                        
         L     R2,ADHEIRC         CHECK SUB DEPT LEVEL FOR BENE PCT             
         BAS   RE,EMPBEN                                                        
         ZAP   PCTBENSV,PCTBENEF  SAVE BEN PCT                                  
         ZAP   PCTADMSV,PCTADMIN  SAVE ADMIN PCT                                
         B     XIT                                                              
         SPACE 3                                                                
EMPBEN   NTR1                       FIND EMPLOYEE BENEFIT PERCENTAGE            
         GOTO1 GETEL,DMCB,(RC),(X'23',(R2)),0                                   
         CLI   DMCB+12,0                                                        
         BNE   ADMIN                                                            
         L     R4,DMCB+12                                                       
         USING ACOTHERD,R4                                                      
         MVC   WORK(5),=5X'F0'                                                  
         MVZ   WORK(5),ACOTNUM                                                  
         CLC   WORK(5),=5X'F0'                                                  
         BNE   ADMIN                                                            
         PACK  PCTBENEF,ACOTNUM(5)                                              
         SPACE 1                                                                
ADMIN    GOTO1 GETEL,DMCB,(RC),(X'25',(R2)),0                                   
         CLI   DMCB+12,0           GET ADMINISTRATIVE PERCENT                   
         BNE   XIT                                                              
         L     R4,DMCB+12                                                       
         USING ACNOD,R4                                                         
         MVC   WORK(5),=5X'F0'                                                  
         MVZ   WORK(5),ACNO                                                     
         CLC   WORK(5),=5X'F0'                                                  
         BNE   XIT                                                              
         PACK  PCTADMIN,ACNO(5)                                                 
         B     XIT                                                              
         EJECT                                                                  
*              FIRST FOR ACCOUNT                                                
         SPACE 1                                                                
CA40     CLI   MODE,PROCACC                                                     
         BNE   CA60                                                             
         MVI   FCRDHIST,C'N'                                                    
         L     R2,ADACCSTA                                                      
         USING ACSTATD,R2                                                       
         CLI   QOPT4,C'S'                                                       
         BNE   CA41                                                             
         TM    ACSTSTAT,X'20'      SUPPRESS LOCKED                              
         BO    XIT                                                              
         SPACE 1                                                                
CA41     MVC   OVHLST(24),=10PL8'0'  CLEAR OVERHEAD AREAS                       
         MVC   OVALST(24),OVHLST                                                
         MVC   OVOLST(24),OVHLST                                                
         LA    R6,PERWRK                                                        
         USING PTOTD,R6                                                         
         LA    R2,PBK              CLEAR THE PERSON / OVERHEAD RECORD           
         LA    R3,PBKCNT                                                        
         ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,*-10                                                          
         SPACE 1                                                                
         L     R5,CLIBUFF          CLEAR CLIENT TABLE                           
         USING BIND,R5                                                          
         XC    BININ,BININ         CLEAR NUMBER IN CLIENT RECORD                
         L     R2,ADACCSTA                                                      
         USING ACSTATD,R2                                                       
         TM    ACSTSTX,X'08'       OVERHEAD ACCOUNT IS A DUMMY FOR              
         BO    CA42                AYER INDIRECT PROCEDURE                      
CA42     MVI   FCRDHIST,C'Y'                                                    
         MVC   SALPCT,SPACES                                                    
         ZAP   PCTBENEF,PCTBENSV   RESTORE BEN PCT                              
         ZAP   PCTADMIN,PCTADMSV   RESTORE ADMIN PCT                            
         L     R2,ADACC            CHECK EMPL LEVEL FOR BENE PCT                
         BAS   RE,EMPBEN                                                        
         MVI   OHDACC,C'N'         TEST FOR OVERHEAD ACCOUNT                    
***************************************************************                 
*        L     R2,ADACC                                                         
*        MVI   LEVEL,COMP          AT CORPORATE LEVEL                           
*        CLI   3(R2),C'9'                                                       
*        BE    CA44                                                             
*        MVI   LEVEL,OFFICE        OFFICE LEVEL                                 
*        CLC   4(2,R2),=C'999'                                                  
*        BE    CA44                                                             
*        MVI   LEVEL,DEPT          DEPARTMET LEVEL                              
*        CLC   8(3,R2),=C'999'                                                  
*        BNE   *+8                                                              
***************************************************************                 
         L     RF,ADACC                                                         
         LA    RF,3(RF)            POINT TO ACCOUNT                             
         LR    R2,RF                                                            
         MVI   LEVEL,COMP                                                       
         SR    R1,R1                                                            
         IC    R1,LLEVA            LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),=C'999'      CORPORATE LEVEL                             
         BE    CA44                                                             
         MVI   LEVEL,OFFICE        OFFICE LEVEL                                 
         LR    R2,RF               POINT TO ACCOUNT                             
         SR    R0,R0                                                            
         IC    R0,LLEVA                                                         
         AR    R2,R0               R2 POINTS AT DEPT                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB          LENGTH OF LEVEL B (DEPT)                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),=C'999'                                                  
         BE    CA44                                                             
         MVI   LEVEL,DEPT          DEPARTMENT LEVEL                             
         LR    R2,RF                                                            
         SR    R0,R0                                                            
         IC    R0,LLEVC            LENGTH OF LEVEL A+B+C                        
         AR    R2,R0                                                            
         CLC   0(3,R2),=C'999'     AT LEAST 3 NINES AT LOWER LEVEL              
         BNE   *+8                 NO - IT'S AN EMPLOYEE                        
CA44     MVI   OHDACC,C'Y'         OVERHEAD ACCOUNT                             
         LA    RE,FORCEHED                                                      
         CLI   OPTSKIP,C'Y'                                                     
         BE    *+8                                                              
         LA    RE,FORCEMID                                                      
         MVI   0(RE),C'Y'                                                       
         L     R2,ADACCSTA         FIND DEPT-GROUP                              
         USING ACSTATD,R2                                                       
         MVC   GROUP,ACSTCOST                                                   
         CLI   GROUP,X'40'                                                      
         BH    CA46                                                             
         L     R2,ADLVCSTA                                                      
         MVC   GROUP,ACSTCOST                                                   
         CLI   GROUP,X'40'                                                      
         BH    CA46                                                             
         L     R2,ADLVBSTA                                                      
         MVC   GROUP,ACSTCOST                                                   
         CLI   GROUP,X'40'                                                      
         BH    CA46                                                             
         GOTO1 REPORT,DMCB,(0,(RC)),(3,0)  PERSON LINE                          
         MVI   POSTERR,C'Y'                                                     
CA46     B     XIT                                                              
         EJECT                                                                  
*              FIRST FOR CONTRA-ACCOUNT                                         
         SPACE 1                                                                
CA60     CLI   MODE,SBACFRST                                                    
         BNE   CA100                                                            
         MVI   NBSW,C'N'                                                        
         LA    R7,CLIWRK           INITIALIZE CLIENT RECORD                     
         USING CLID,R7                                                          
         LA    R2,CLIBK                                                         
         LA    R3,CBKCNT                                                        
         ZAP   0(8,R2),=P'0'       CLEAR CLIENT BUCKETS                         
         LA    R2,8(R2)                                                         
         BCT   R3,*-10                                                          
         SPACE 1                                                                
         MVC   CLICLNME,SPACES                                                  
         L     R2,ADSUBAC                                                       
         USING TRSUBHD,R2                                                       
         MVC   CLILEDG(13),TRSBACNT+2  LEDGER AND ACCOUNT                       
         ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         BM    CA63                                                             
         EXMVC R1,CLICLNME,TRSBNAME    CONTRA-NAME                              
*                                                                               
CA63     MVI   WANT,C'Y'                                                        
         CLI   OHDACC,C'Y'                                                      
         BNE   CA65                                                             
         B     XIT                                                              
CA65     CLC   TRSBACNT+1(3),=C'1NL'                                            
         BE    CA66                     SKIP LEAVE OF ABSENCE                   
         CLI   OPTNO1N,C'Y'             SKIP ALL 1N                             
         BNE   XIT                                                              
         CLC   TRSBACNT+1(2),=C'1N'                                             
         BNE   XIT                                                              
CA66     MVI   WANT,C'N'                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS TRANSACTIONS                                             
         SPACE 1                                                                
CA100    CLI   MODE,PROCHIST                                                    
         BNE   CA140                                                            
         CLI   WANT,C'Y'                                                        
         BNE   XIT                                                              
         L     R4,ADTRANS                                                       
         CLI   0(R4),X'45'                                                      
         BNE   XIT                                                              
         USING TRHISTD,R4                                                       
         LA    R6,PERWRK                                                        
         USING PTOTD,R6                                                         
         LA    R7,CLIWRK                                                        
         USING CLID,R7                                                          
         CLI   BUCKTYPE,C'H'        HOURS                                       
         BE    CA130                                                            
         CLI   BUCKTYPE,C' '        OR DOLLARS                                  
         BNE   XIT                                                              
         CLI   OHDACC,C'Y'                                                      
         BNE   CA120                                                            
         SPACE 2                                                                
*              OVERHEAD ACCOUNTS                                                
CA110    CLC   TRHSYEAR(2),LAST    OVERHEAD ACCOUNTS                            
         BH    XIT                                                              
         CLI   OPTYTD,C'Y'                                                      
         BNE   CA112                                                            
         CLC   TRHSYEAR(2),START                                                
         BL    XIT                 BUCKET MUST BE WITHIN REQUEST RANGE          
         AP    OVALST,TRHSDR       OVERHEAD YTD-1                               
         AP    CCSTLST,TRHSDR                                                   
         B     XIT                                                              
CA112    OC    FISCAL,FISCAL       IS OVERHEAD YTD                              
         BZ    XIT                                                              
         CLC   TRHSYEAR(2),FISCAL                                               
         BL    XIT                                                              
         AP    OVALST,TRHSDR       OVERHEAD YTD-1                               
         AP    CCSTLST,TRHSDR                                                   
         B     XIT                                                              
         SPACE 2                                                                
*              STAFF ACCOUNTS                                                   
CA120    CLC   TRHSYEAR(2),END     SKIP IF HIGHER THAN LAST                     
         BH    XIT                                                              
         CLI   OPTYTD,C'Y'                                                      
         BNE   CA122                                                            
         CLC   TRHSYEAR(2),START                                                
         BL    XIT                 BUCKET MUST BE WITHIN REQUEST RANGE          
         CLC   TRHSYEAR(2),LAST    STAFF ACCOUNTS                               
         BH    XIT                                                              
         AP    CCSTLST,TRHSDR       YTD-1 CASH                                  
         B     XIT                                                              
CA122    OC    FISCAL,FISCAL       IS OVERHEAD YTD                              
         BZ    XIT                                                              
         CLC   TRHSYEAR(2),FISCAL                                               
         BL    XIT                                                              
         CLC   TRHSYEAR(2),LAST    STAFF ACCOUNTS                               
         BH    XIT                                                              
         ZAP   DUB,TRHSDR                                                       
         MVI   POSTSW,C'M'                                                      
         GOTO1 BUFPOST,DMCB,(RC),(R7) POST MEMO YTD-1                           
         B     XIT                                                              
         SPACE 1                                                                
*              HOURS                                                            
CA130    CLC   TRHSYEAR(2),START                                                
         BL    XIT                 BUCKET MUST BE WITHIN REQUEST RANGE          
         CLC   TRHSYEAR(2),END     STAFF ACCOUNTS                               
         BH    XIT                                                              
         AP    CYTDHRS,TRHSCR       YTD HOURS                                   
         AP    PYTDHRS,TRHSCR                                                   
         CLC   TRHSYEAR(2),LAST    ONLY WANT IF MORE RECENT THAN LAST           
         BNH   XIT                                                              
         AP    CPERHRS,TRHSCR      THIS MONTHS HOURS                            
         AP    PPERHRS,TRHSCR                                                   
         CLC   CLICDE(2),=C'P '                                                 
         BNE   XIT                                                              
         AP    PPERPHRS,TRHSCR                                                  
         B     XIT                                                              
         EJECT                                                                  
*              END OF A CONTRA-ACCOUNT                                          
         SPACE 1                                                                
CA140    CLI   MODE,SBACLAST                                                    
         BNE   CA200                                                            
         CLI   WANT,C'Y'                                                        
         BNE   XIT                                                              
         LA    R6,PERWRK                                                        
         USING PTOTD,R6                                                         
         LA    R7,CLIWRK                                                        
         USING CLID,R7                                                          
         CLI   OHDACC,C'Y'                                                      
         BE    CA150                                                            
         CLI   CLILEDG,C'N'                                                     
         BE    CA144               NON-CLIENT TIME                              
         AP    PDIRLST,CCSTLST     YTD-1 DIRECT COST                            
         B     CA150                                                            
         SPACE 1                                                                
CA144    CLC   CLICDE(2),=C'P '                                                 
         BE    CA146                                                            
*****************************************************************               
*        CLC   ALPHAID,=C'BS'      FOR BACKER ********TAKE OUT IN 93**          
*        BNE   CA144A                                                           
*        CLC   CLICDE(2),=C'C '    ALL CORP IND BECOMES OFFICE                  
*        BNE   CA144A                                                           
*        MVC   CLICDE(2),=C'O '                                                 
*****************************************************************               
CA144A   AP    PINDLST,CCSTLST     YTD-1 INDIRECT COST                          
         B     CA150                                                            
         SPACE 1                                                                
CA146    AP    PRSYTDH,CYTDHRS     YTD PERSONAL HOURS                           
         B     CA150                                                            
         SPACE 2                                                                
CA150    LA    R2,CLIBK                                                         
         LA    R3,CBKCNT                                                        
         CP    0(8,R2),=P'0'                                                    
         BNE   CA152                                                            
         LA    R2,8(R2)                                                         
         BCT   R3,*-14                                                          
         B     XIT                 DON'T ADD IF ALL BUCKETS ZERO                
         SPACE 1                                                                
CA152    GOTO1 BINADD,DMCB,(R7),CLIBUFF,(RC) ADD TO CLIENT TABLE                
         B     XIT                                                              
         EJECT                                                                  
*              ACCLAST FOR A PERSON                                             
         SPACE 1                                                                
CA200    CLI   MODE,ACCLAST                                                     
         BNE   CA400                                                            
         CLI   FCRDHIST,C'N'                                                    
         BE    XIT                                                              
         CLI   OHDACC,C'Y'                                                      
         BE    CA280               OVERHEAD ACCOUNT                             
         L     R2,ADLVBSTA                                                      
         USING ACSTATD,R2                                                       
         TM    ACSTSTX,X'08'                                                    
         BZ    CA202                                                            
         GOTO1 INDDPT,DMCB,(RC)                                                 
         B     XIT                                                              
*        BO    CA270               INDIRECT DEPARTMENT                          
         SPACE 1                                                                
         USING PTOTD,R6                                                         
CA202    LA    R6,PERWRK                                                        
         BAS   RE,ADDPCT           ADD BENEFIT PERCENT TO SALARY                
         CLI   OPTYTD,C'Y'                                                      
         BNE   CA205               NOT YTD                                      
         CP    PCOST,=P'0'         IF YTD AND NO COST REVERSE YTD-1             
         BNE   CA204                                                            
         ZAP   DUB,PINDLST                                                      
         AP    DUB,PDIRLST                                                      
         CP    DUB,=P'0'                                                        
         BE    XIT                 YTD-1 COST IS ALSO ZERO SO GET OUT           
         ZAP   RATE,=P'0'                                                       
         B     CA230                                                            
CA204    CP    PYTDHRS,=P'0'                                                    
         BNE   CA207                                                            
         CP    PPERHRS,=P'0'                                                    
         BE    CA260                NO HOURS                                    
         B     CA207A                                                           
         SPACE 1                                                                
CA205    CP    PCOST,=P'0'          IF NOT YTD AND NO COST                      
         BE    XIT                  GET OUT                                     
         CP    PYTDHRS,=P'0'                                                    
         BE    CA260                 NO HOURS                                   
CA207    CP    PYTDHRS,PRSYTDH                                                  
         BNE   CA207A         IF TOTAL EQ PERSONAL MUST GO INDIRECT             
         CP    PPERHRS,PPERPHRS                                                 
         BE    CA260                                                            
CA207A   DS    0H                                                               
         ZAP   DIVWRK,PCOST        SALARY PLUS PERCENT                          
         MP    DIVWRK,=P'10000'                                                 
         CLI   OPTHRS,0                                                         
         BE    CA208               NO PROFILE FOR STANDARD HOURS                
         ZIC   RF,OPTHRS                                                        
         MH    RF,=H'100'                                                       
         CVD   RF,DUB                                                           
         ZAP   PYTDHRS,DUB                                                      
         CLI   OPTHRS,1            IF STANDARD HOURS = 1                        
         BNE   *+10                                                             
         ZAP   PYTDHRS,=P'13333'   USE 133.33                                   
         MP    PYTDHRS,NUMONTHS                                                 
         CLI   OPTHRS,2            IF STANDARD HOURS = 2                        
         BNE   *+10                                                             
         ZAP   PYTDHRS,=P'11667'   USE 116.67                                   
         MP    PYTDHRS,NUMONTHS                                                 
         SPACE 1                                                                
CA208    ZAP   RATE,=P'0'                                                       
         CP    PYTDHRS,=P'0'                                                    
         BE    CA209                                                            
         ZAP   SVCOST,DIVWRK          SAVE THE COST                             
         DP    DIVWRK,PYTDHRS+2(6)    COST/HOURS                                
         ZAP   RATE,DIVWRK(10)        = RATE                                    
         CP    RATE,=P'0'                                                       
         BH    CA209                                                            
         ZAP   DUB,RATE                                                         
         MP    DUB,=P'-1'            IF RATE NEGATIVE, MAKE IT POSITIVE         
         ZAP   RATE,DUB                                                         
CA209    ZAP   PNONPER,PYTDHRS         TOTAL YTD HOURS                          
         SP    PNONPER,PRSYTDH         LESS PERSONAL  GIVES NON-PERS            
         SPACE 1                                                                
         USING BIND,R5                                                          
         L     R5,CLIBUFF                                                       
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BZ    XIT                 NOTHING IN TABLE                             
         ZAP   DUB,=P'0'                                                        
         SPACE 1                                                                
         USING CLID,R7                                                          
         LA    R7,BINTABLE                                                      
CA212    CLC   CLICDE(2),=C'P '    SKIP PERSONAL TIME                           
         BE    CA214               GIVE CLIENT SHARE OF PERSONAL                
         ZAP   CADJHRS,CYTDHRS       SET ADJ HRS.  - CLIENT YTD HOURS           
         ZAP   DIVWRK,CYTDHRS        CLIENT YTD HOURS                           
         MP    DIVWRK,PRSYTDH+2(6)   *  PERSONAL HOURS                          
         MP    DIVWRK,=P'1000'                                                  
         CP    PNONPER,=P'0'         TOTAL NON-PERSONAL                         
         BE    CA214                                                            
         DP    DIVWRK,PNONPER+2(6)  TOTAL CLIENT TIME                           
         SRP   DIVWRK(10),64-3,5                                                
         AP    CADJHRS,DIVWRK(10)                                               
CA214    AP    DUB,CADJHRS         ADD ALL THE ADJUSTED HOURS                   
         LA    R7,CLEN(R7)                                                      
         BCT   R3,CA212                                                         
         CLI   OPTHRS,0            IF USING STANDARD HOURS                      
         BNE   CA230               DON'T ADJUST THE RATE                        
*              RECOMPUTE RATE BASED ON ADJUSTED HOURS                           
*              TO FIX A ROUNDING PROBLEM                                        
*                                                                               
         ZAP   RATE,=P'0'                                                       
         CP    DUB+2(6),=P'0'                                                   
         BE    CA230                                                            
         ZAP   DIVWRK,SVCOST        RESTORE THE COST                            
         DP    DIVWRK,DUB+2(6)      COST/HOURS                                  
         ZAP   RATE,DIVWRK(10)        = RATE                                    
         CP    RATE,=P'0'                                                       
         BH    CA230                                                            
         ZAP   DUB,RATE                                                         
         MP    DUB,=P'-1'            IF RATE NEGATIVE, MAKE IT POSITIVE         
         ZAP   RATE,DUB                                                         
         EJECT                                                                  
*              ALLOCATE COST TO EACH CLIENT/NON-CLIENT                          
         SPACE 1                                                                
         USING BIND,R5                                                          
CA230    L     R5,CLIBUFF                                                       
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         LA    R7,BINTABLE                                                      
         MVC   TOTLINE(80),=10PL8'0'                                            
         SPACE 1                                                                
         USING CLID,R7                                                          
CA232    CLC   CLICDE(2),=C'P '                                                 
         BNE   CA235                                                            
         MVC   CADJHRS(32),=10PL8'0'  NO ADJ. HOURS OR COST                     
         B     CA247                 FOR PERSONAL TIME                          
         SPACE 1                                                                
CA235    ZAP   DIVWRK,CADJHRS       ADJUSTED HOURS                              
         MP    DIVWRK,RATE          TIMES RATE                                  
         AP    DIVWRK,=P'5000'                                                  
         CP    DIVWRK,=P'0'                                                     
         BNL   *+10                                                             
         SP    DIVWRK,=P'10000'                                                 
         DP    DIVWRK,=P'10000'                                                 
         ZAP   DUB,DIVWRK(13)                                                   
         ZAP   CCSTYTD,DUB          YTD CASH                                    
         ZAP   CCSTPST,DUB                                                      
         SP    CCSTPST,CCSTLST      POSTING IS YTD LESS YTD-1                   
         SPACE 1                                                                
         USING PSHEADD,R2          BUILD DEBIT POSTING TO PERSON                
         LA    R2,T                CONTRA IS CLIENT/NON-CLIENT                  
         L     RF,ADACC                                                         
         MVC   OFFCODE,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFCODE(0),3(RF)                                                 
         SPACE 1                                                                
         MVC   PSHDACC,0(RF)                                                    
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVC   PSHDSBAC(2),PSHDACC COMPANY/UNIT                                 
         MVC   PSHDSBAC+2(13),CLILEDG                                           
         MVC   PSHDSBNM,CLICLNME                                                
         MVI   TRNSSTAT,X'80'                                                   
         ZAP   TRNSAMNT,CCSTPST                                                 
         MVC   TRNSANAL,OFFCODE                                                 
         BAS   RE,BENEL            ADD BENEFIT ELEMENT                          
         GOTO1 POSTIT,DMCB,(RC)                                                 
         MVC   SAVE50,PSHDEL       SAVE FOR DEPT INDIRECT PROBLEM               
         SPACE 1                                                                
         CLC   PSHDSBAC+1(14),=CL14'1ND DEPT'                                   
         BNE   CA236                                                            
         MVC   CLICDE(2),=C'D '     YTD-1 WAS ADDED TO DEPT.                    
         B     CA240                                                            
         SPACE 1                                                                
CA236    CLC   PSHDSBAC+1(14),=CL14'1NO OFFICE'                                 
         BNE   CA238                                                            
         MVC   CLICDE(2),=C'O '     YTD-1 WAS ADDED TO OFFICE                   
         B     CA240                                                            
         SPACE 1                                                                
CA238    CLC   PSHDSBAC+1(14),=CL14'1NC CORP'                                   
         BNE   CA240                                                            
         MVC   CLICDE(2),=C'C '     YTD-1 WAS ADDED TO CORP                     
         SPACE 1                                                                
CA240    MVI   POSTSW,C'A'         POST ALL OTHERS                              
         MVI   NBSW,C'N'                                                        
*                                                                               
         GOTO1 BUFPOST,DMCB,(RC),(R7) POST BUFFALO RECORD                       
         LA    R8,DEPTOT                                                        
         BAS   RE,SUMPOST          POST TO SUMMARY RECORDS                      
         SPACE 1                                                                
CA247    GOTO1 REPORT,DMCB,(0,(RC)),0      PERSON LINE                          
CA248    LA    R7,CLEN(R7)         NEXT CLIENT                                  
         BCT   R3,CA232                                                         
         SPACE 1                                                                
         GOTO1 REPORT,DMCB,(0,(RC)),(C'T',0)  TOTAL LINE                        
         CP    PYTDHRS,=P'0'                                                    
         BE    CA250                                                            
         CP    PYTDHRS,PRSYTDH                                                  
         BE    CA250          IF TOTAL EQ PERSONAL MUST GO INDIRECT             
         B     XIT                                                              
CA250    ZAP   PINDLST,=P'0'                                                    
         EJECT                                                                  
*              ACCLAST FOR PEOPLE WITHOUT HOURS                                 
         SPACE 1                                                                
         USING PTOTD,R6                                                         
CA260    LA    R6,PERWRK                                                        
         BAS   RE,ADDPCT           ADD BENEFIT PERCENT TO COST                  
         CP    PCOST,=P'0'                                                      
         BNE   CA261                                                            
         CP    PINDLST,=P'0'       IND YTD-1 IS ALSO ZERO                       
         BE    XIT                 NOTHING TO DO                                
CA261    LA    R8,DEPTOT           ADD TO CORP /OFFICE/DEPT INDIRECT            
         USING SUMD,R8                                                          
         LA    R2,SCINDLST                                                      
         LA    R3,SCINDPST                                                      
         CLI   OPTNOTS,C'C'                                                     
         BE    CA262                                                            
         LA    R2,SOINDLST                                                      
         LA    R3,SOINDPST                                                      
         CLI   OPTNOTS,C'O'                                                     
         BE    CA262                                                            
         LA    R2,SDINDLST                                                      
         LA    R3,SDINDPST                                                      
CA262    AP    0(8,R2),PINDLST     FOR DISTRIBUTION AT HIGHER LEVEL             
         AP    8(8,R2),PCOST                                                    
         AP    0(8,R3),PCOST                                                    
         SP    0(8,R3),PINDLST                                                  
         SPACE 1                                                                
         USING PSHEADD,R2          BUILD DEBIT POSTING TO PERSON                
         LA    R2,T                CONTRA IS 1NC CORP                           
         L     RF,ADACC                                                         
         MVC   OFFCODE,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFCODE(0),3(RF)                                                 
         SPACE 1                                                                
         MVC   PSHDACC,0(RF)                                                    
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVC   PSHDSBAC(1),PSHDACC                COMPANY                       
         MVC   PSHDSBAC+1(14),=CL14'1NC CORP'    UNIT/LEDGER/ACCOUNT            
         MVC   PSHDSBNM,SPACES                                                  
         MVC   PSHDSBNM(13),=C'CORP INDIRECT'                                   
         CLI   OPTNOTS,C'C'                                                     
         BE    CA263                                                            
         MVC   PSHDSBAC+1(14),=CL14'1NO OFFICE'                                 
         MVC   PSHDSBNM,SPACES                                                  
         MVC   PSHDSBNM(15),=C'OFFICE INDIRECT'                                 
         CLI   OPTNOTS,C'O'                                                     
         BE    CA263                                                            
         MVC   PSHDSBAC+1(14),=CL14'1ND DEPT'                                   
         MVC   PSHDSBNM,SPACES                                                  
         MVC   PSHDSBNM(13),=C'DEPT INDIRECT'                                   
         SPACE 1                                                                
CA263    MVC   PSHDSBNM,SPACES                                                  
         MVI   TRNSSTAT,X'80'                                                   
         ZAP   TRNSAMNT,PCOST                                                   
         SP    TRNSAMNT,PINDLST                                                 
         LA    R7,CLIWRK                                                        
         ZAP   CCSTPST,PCOST       GIVE ALL BEN. AND ADM. TO CORP               
         SP    CCSTPST,PINDLST                                                  
         BAS   RE,BENEL            ADD BENEFIT ELEMENT                          
         MVC   TRNSANAL,OFFCODE                                                 
         GOTO1 POSTIT,DMCB,(RC)                                                 
         GOTO1 REPORT,DMCB,(0,(RC)),(2,0)  PRINT IT                             
         B     XIT                                                              
         EJECT                                                                  
         EJECT                                                                  
*              ACCLAST FOR OVERHEAD ACCOUNTS                                    
         SPACE 1                                                                
CA280    ZAP   DUB3,=P'0'         FIRST GET DIRECT TIME FOR THIS LEVEL          
         ZAP   DUB4,=P'0'                                                       
         ZIC   R0,LEVEL                                                         
         LA    R2,BUFNOGP          O/H NOT ALLOCATED BY GROUP                   
         USING BUFD,R5                                                          
         LA    R5,BUFWRK                                                        
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         MVI   BUFCLI,BUFTOT       TOTAL RECORD FOR THIS TYPE                   
         STC   R2,BUFTYPE                                                       
         GOTO1 BUFFALO,DMCB,=C'GET',((R2),ADBUFC),(R5),(R0)                     
         CLI   DMCB+8,0                                                         
         BNE   CA283               NO DIRECT TIME                               
         CLC   ALPHAID,=C'BS'      FOR BACKER                                   
         BNE   CA280A                                                           
         CLI   LEVEL,DEPT          CHECK PROF 1 AT OFF AND COMP LEVEL           
         BE    CA281                                                            
         B     *+12                                                             
CA280A   CLI   LEVEL,COMP          EVERYONE ELSE AT COMP LEVEL ONLY             
         BNE   CA281                                                            
         CLI   OPTEXP,C'I'         INCLUDE DEPT INDIRECT IN BASIS               
         BE    *+12                FOR ALLOCATION                               
         CLI   OPTEXP,C'Y'         INCLUDE EXPENSES/DEPT INDIRECT/              
         BNE   CA281               AND DEPT OVERHEAD                            
         AP    BDIRLST,BEXPLST    ADD EXPENSES OR IND OR OVH TO DIRECT          
         AP    BDIRYTD,BEXPYTD     EXP. ONLY PRESENT AT COMP.LEVEL              
CA281    ZAP   DUB3,BDIRLST        DIRECT TIME YTD-1                            
         ZAP   DUB4,BDIRYTD        DIRECT TIME YTD                              
         CLI   OPTOVHY,C'Y'         IS OVERHEAD YTD                             
         BNE   *+16                                                             
         AP    DUB3,BDIRLSTM    IF IT IS ADD MEMO DIRECT YTD-1                  
         AP    DUB4,BDIRLSTM                                                    
         SPACE 1                                                                
CA283    L     R2,ADACCSTA                                                      
         USING ACSTATD,R2                                                       
         TM    ACSTSTX,X'08'       OVERHEAD ACCOUNT IS A DUMMY FOR              
         BO    CA350               AYER INDIRECT PROCEDURE                      
         SPACE 1                                                                
         GOTO1 GETOVH,DMCB,(RC)    GET TOTAL OVERHEAD                           
         CP    OVHLST,=P'0'                                                     
         BNE   *+14                                                             
         CP    OVHYTD,=P'0'                                                     
         BE    XIT                 NO OVERHEAD                                  
         SPACE 1                                                                
         USING ACSTATD,R2                                                       
         L     R2,ADLVBSTA                                                      
         TM    ACSTSTX,X'08'                                                    
         BO    CA380               OVERHEAD ACCOUNT IN INDIRECT DEPT            
         SPACE 1                                                                
CA290    GOTO1 DISOVH,DMCB,(RC)    DISTRIBUTE OVERHEAD                          
         B     XIT                                                              
         EJECT                                                                  
*              MAKE NEGATIVE POSTING OF DEPT. INDIRECT (AYER)                   
         SPACE 1                                                                
CA350    L     R2,ADACC                                                         
         MVC   INDSAVE,0(R2)                                                    
         L     R4,ADACCNAM                                                      
         LA    R6,INDSAVEN                                                      
         BAS   RE,NAMOUT                                                        
         CP    DUB3,=P'0'                                                       
         BNE   *+14                                                             
         CP    DUB4,=P'0'                                                       
         BE    XIT                 NO DIRECT TIME                               
         LA    R2,T                                                             
         USING PSHEADD,R2                                                       
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVC   PSHDACC,INDSAVE                                                  
         MVC   PSHDSBAC,PSHDACC    CONTRA A/C IS ITSELF                         
         MVC   PSHDSBNM,INDSAVEN                                                
         MVI   TRNSSTAT,X'80'      DEBIT                                        
         SPACE 1                                                                
         USING SUMD,R8                                                          
         LA    R8,DEPTOT           DEPT TOTALS SHOULD BE HERE                   
         ZAP   TRNSAMNT,SDINDLST                                                
         SP    TRNSAMNT,SDINDYTD   MINUS THIS PERIOD INDIRECT                   
         CLI   LEVEL,DEPT                                                       
         BE    CA352                                                            
         LA    R8,OFFTOT                                                        
         ZAP   TRNSAMNT,SOINDLST                                                
         SP    TRNSAMNT,SOINDYTD   MINUS THIS PERIOD INDIRECT                   
         CLI   LEVEL,OFFICE                                                     
         BE    CA352                                                            
         LA    R8,AGYTOT                                                        
         ZAP   TRNSAMNT,SCINDLST                                                
         SP    TRNSAMNT,SCINDYTD   MINUS THIS PERIOD INDIRECT                   
         CLI   LEVEL,COMP                                                       
         BE    CA352                                                            
         GOTO1 REPORT,DMCB,(2,(RC)),(3,0)  NO DIRECT TIME COSTS                 
         MVI   POSTERR,C'Y'                                                     
         B     XIT                                                              
CA352    L     RF,ADACC                                                         
         MVC   OFFCODE,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFCODE(0),3(RF)                                                 
         MVC   TRNSANAL,OFFCODE                                                 
         GOTO1 POSTIT,DMCB,(RC)                                                 
         MVI   IND,C'Y'                                                         
         B     XIT                                                              
         EJECT                                                                  
*              OVERHEAD ACCOUNT IN INDIRECT DEPT.                               
         SPACE 1                                                                
         USING DEPD,R7                                                          
CA380    LA    R7,DEPCUM                                                        
         AP    DOVHCL,OVALST       ADD TO DEPT. CORP                            
         AP    DOVHCY,OVAYTD       FOR DISTRIBUTION AT CORP LEVEL               
         LA    R8,DEPTOT                                                        
         AP    SOVOLST,OVALST                                                   
         AP    SOVOYTD,OVAYTD                                                   
         AP    SOVOPST,OVAPST                                                   
         USING PSHEADD,R2          BUILD OVERHEAD ACCOUNT                       
         LA    R2,T                CONTRA IS 1NC CORP                           
         L     RF,ADACC            OR 1NO OFFICE                                
         MVC   PSHDACC,0(RF)                                                    
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVC   PSHDSBAC(1),PSHDACC                COMPANY                       
         MVC   PSHDSBNM,SPACES                                                  
         CLC   ALPHAID,=C'BS'         FOR BACKER ADD IT TO OFFICE               
         BNE   CA384                                                            
         MVC   PSHDSBAC+1(14),=CL14'1NO OFFICE'                                 
         MVC   PSHDSBNM(15),=C'OFFICE OVERHEAD'                                 
         B     *+16                                                             
CA384    MVC   PSHDSBAC+1(14),=CL14'1NC CORP'    UNIT/LEDGER/ACCOUNT            
         MVC   PSHDSBNM(13),=C'CORP OVERHEAD'                                   
         MVI   TRNSSTAT,X'80'                                                   
         ZAP   TRNSAMNT,OVAPST                                                  
         GOTO1 POSTIT,DMCB,(RC)                                                 
         GOTO1 REPORT,DMCB,(1,(RC)),(1,0)    O/H IN INDIRECT DEPT               
         B     XIT                                                              
         EJECT                                                                  
*              END OF DEPARTMENT                                                
         SPACE 1                                                                
CA400    CLI   MODE,LEVBLAST                                                    
         BNE   CA430                                                            
         MVI   LEVEL,DEPT                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 DIRPOST,DMCB,(RC)          POST DIRECT TIME                      
         GOTO1 INDPOST,DMCB,(RC)          POST THE INDIRECT                     
         SPACE 1                                                                
         USING SUMD,R8                                                          
         LA    R8,DEPTOT                                                        
         ZAP   SOINDPST,SOINDYTD   GET NEW POSTING AMOUNT                       
         SP    SOINDPST,SOINDLST                                                
         ZAP   SCINDPST,SCINDYTD   GET NEW POSTING AMOUNT                       
         SP    SCINDPST,SCINDLST                                                
         GOTO1 SUMUP,DMCB,DEPTOT,OFFTOT   ADD DEPT TOTAL TO OFFICE              
         SPACE 1                                                                
         USING OFFD,R7                                                          
         LA    R7,OFFCUM                                                        
         LA    R8,OFDACCUM                                                      
         USING DEPD,R7                                                          
         LA    R7,DEPCUM                                                        
         LA    R6,DEBK                                                          
         LA    R3,DEBKCNT                                                       
CA403    AP    0(8,R8),0(8,R6)  ADD DEPT ACCUMS TO OFFICE                       
         LA    R8,8(R8)                                                         
         LA    R6,8(R6)                                                         
         BCT   R3,CA403                                                         
         SPACE 1                                                                
         L     R2,ADHEIRA                                                       
         CLI   3(R2),C'9'                                                       
         BE    CA405               DON'T PRINT SUMMARY AT DUMMY DEPT            
         GOTO1 REPORT,DMCB,(2,(RC)),(C'S',0)  DEPT SUMMARY                      
         CLI   OPTEXP,C'I'         INCLUDE DEPT INDIRECT                        
         BE    *+12                IN BASIS FOR ALLOCATION                      
         CLI   OPTEXP,C'Y'         INCLUDE EXP/DEPT INDIRECT AND O/H            
         BNE   CA405                                                            
         GOTO1 INDOVH,DMCB,(RC)      ADD IND AND OVH TO BUFF                    
CA405    GOTO1 BUFFALO,DMCB,=C'ADD',ADBUFC,1,2,(X'80',3)  ADD 1 TO 2-3          
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,(X'80',1)   CLEAR 1                
         MVC   OVHLST(40),=10PL8'0'                                             
         MVC   INDLST(24),=10PL8'0'                                             
         LA    R8,DEPTOT           INITIALIZE OFFICE /DEPT RECORD               
         BAS   RE,CLSUM                                                         
         SPACE 1                                                                
         LA    R7,DEPCUM                                                        
         USING DEPD,R7                                                          
         LA    R2,DEBK             CLEAR DEPT CUM TABLE                         
         LA    R3,DEBKCNT                                                       
         ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
*              END OF OFFICE                                                    
         SPACE 1                                                                
CA430    CLI   MODE,LEVALAST                                                    
         BNE   CA460                                                            
         MVI   LEVEL,OFFICE                                                     
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 INDPOST,DMCB,(RC)          POST THE INDIRECT                     
         SPACE 1                                                                
         GOTO1 SUMUP,DMCB,DEPT99,OFFTOT   ADD DEPT 99 TO OFFTOT                 
         GOTO1 (RF),(R1),OFFTOT,AGYTOT   ADD OFFTOT TO AGYTOT                   
         MVC   DEPTOT(SLEN),OFFTOT                                              
         SPACE 1                                                                
         USING AGYD,R7                                                          
         LA    R7,AGYCUM                                                        
         LA    R8,AGOACCUM                                                      
         USING OFFD,R7                                                          
         LA    R7,OFFCUM                                                        
         LA    R6,OFBK                                                          
         LA    R3,OFBKCNT                                                       
CA433    AP    0(8,R8),0(8,R6)  ADD OFFICE ACCUMS TO AGENCY                     
         LA    R8,8(R8)                                                         
         LA    R6,8(R6)                                                         
         BCT   R3,CA433                                                         
         SPACE 1                                                                
         L     R2,ADHEIRA                                                       
         SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   3(0,R2),=12C'9'                                                  
         BE    CA435               DON'T PRINT SUMMARY AT DUMMY DEPT            
         SPACE 1                                                                
         GOTO1 REPORT,DMCB,(2,(RC)),(C'S',0)  OFFICE SUMMARY                    
CA435    GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,1,(X'80',2)  CLEAR 1-2             
         MVC   OVHLST(40),=10PL8'0'                                             
         MVC   INDLST(24),=10PL8'0'                                             
         SPACE 1                                                                
         LA    R7,OFFCUM                                                        
         USING OFFD,R7                                                          
         LA    R2,OFBK             CLEAR OFFICE CUM TABLE                       
         LA    R3,OFBKCNT                                                       
         ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
*              END OF AGENCY                                                    
         SPACE 1                                                                
CA460    CLI   MODE,REQLAST                                                     
         BNE   CA600                                                            
         MVI   LEVEL,COMP                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 INDPOST,DMCB,(RC)         POST THE INDIRECT                      
         GOTO1 SUMUP,DMCB,OFF99,AGY99    ADD OFFICE 9/DEPT99 AGENCY             
         GOTO1 (RF),(R1),AGY99,AGYTOT   OFFICE9 DEPT 99 TO TABLE                
         GOTO1 (RF),(R1),AGYTOT,0        AGENCY TOTAL TO TABLE                  
         MVC   DEPTOT(SLEN),AGYTOT                                              
         GOTO1 REPORT,DMCB,(2,(RC)),(C'S',0)  CORP SUMMARY                      
         BAS   RE,SUMMARY                PRINT DEPT/OFFICE SUMMARY              
         MVI   FORCEHED,C'Y'                                                    
*        GOTO1 SRTP,DMCB,(RC)            PRINT AND ADD SORTED POSTINGS          
         SPACE 1                                                                
CA465    GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,(X'80',3)   CLEAR 3                
         B     XIT                                                              
         EJECT                                                                  
*              END OF RUN                                                       
         SPACE 1                                                                
CA600    CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         MVI   FORCEHED,C'Y'       NEW PAGE FOR SUMMARY                         
         LA    R2,T                BUILD A SELF-BALANCING POSTING               
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         MVI   TRNSSTAT,0                                                       
         ZAP   TRNSAMNT,PDEBITS                                                 
         SP    TRNSAMNT,PCREDITS                                                
         CP    TRNSAMNT,=P'1000'                                                
         BNH   *+6                                                              
         DC    H'0'                ROUNDING MORE THAN 10 DOLLARS                
         CP    TRNSAMNT,=P'-1000'                                               
         BNL   *+6                                                              
         DC    H'0'                ROUNDING MORE THAN -10 DOLLARS               
         GOTO1 POSTIT,DMCB,(RC)                                                 
         SPACE 1                                                                
         CLI   POSTERR,C'Y'                                                     
         BNE   CA610                                                            
         MVC   P+1(38),=CL38'ALLOCATION INCOMPLETE-CHECK FOR ERRORS'            
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 1                                                                
CA610    CP    POSTREC,=P'0'                                                    
         BE    XIT                                                              
         XC    T(80),T                                                          
         XC    T-4(100),T-4                                                     
         MVC   T-4(2),=X'0021'                                                  
         MVC   T(2),=X'521D'                                                    
         MVC   T+2(15),=C'TIME/COST ALLOC'                                      
         ZAP   T+17(6),POSTREC                                                  
         ZAP   T+23(6),POSTCASH                                                 
         LA    R3,T-4                                                           
         L     R4,POSTBUFF                                                      
         GOTO1 WORKER,DMCB,=CL6'ADD',(R4),ID,(R3)   ADD TOTAL RECORD            
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 WORKER,DMCB,=CL6'CLOSE',(R4),ID,(R3)  AND CLOSE FILE             
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING LOGOD,R3                                                         
         L     R3,LOGOC                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 LOGO,DMCB,(R3)                                                   
         SPACE 1                                                                
         L     R6,VEXTRAS                                                       
         USING RUNXTRAD,R6                                                      
         L     R6,ADMASTD                                                       
         USING MASTD,R6                                                         
         L     R6,MCVREMOT                                                      
         USING REMOTED,R6                                                       
         OC    REMOTKEY,REMOTKEY                                                
         BZ    CA621                                                            
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         XC    REMOTKEY,REMOTKEY                                                
         SPACE 1                                                                
CA621    MVI   LOGOEND,C'X'                                                     
         MVI   LOGOTYPE,C'S'                                                    
         MVC   LOGONAME(20),=CL20'**INTERNAL CONTROL**'                         
         MVC   LOGOADD(20),=CL20'**DO NOT SEND OUT***'                          
         MVC   LOGO1,=C'CONTROL'                                                
         GOTO1 LOGO,DMCB,(R3)                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         GOTO1 REPORT,DMCB,(4,(RC)),0                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD PERCENT TO SALARY                                 
         SPACE 1                                                                
         USING PTOTD,R6                                                         
ADDPCT   NTR1                                                                   
         LA    R6,PERWRK                                                        
         L     R2,ADACC                                                         
*MN      GOTO1 ACSLRY,DMCB,(R2),STEND,SALAREA                                   
         GOTO1 ACSLRY,DMCB,(X'80',(R2)),STEND,SALAREA,ADCOMFAC                  
         LA    R2,SALAREA                                                       
         USING SLRD,R2                                                          
         ZAP   PCOST,SLRTOT        SALARY FOR PERIOD                            
         SPACE 1                                                                
         EDIT  SLRTOT,(10,SALVAL),2,MINUS=YES,ALIGN=LEFT                        
         ZAP   PCTTOT,=P'0'                                                     
         AP    PCTTOT,PCTBENEF     BENEFIT PERCENT                              
         AP    PCTTOT,PCTADMIN     PLUS ADMIN. PERCENT                          
         CP    PCTTOT,=P'0'        TOTAL PERCENT                                
         BE    ADDPCTX                                                          
         SPACE 1                                                                
         MVC   SALPCT(9),=C'+      PC'                                          
         EDIT  PCTTOT,(6,SALPCT+1),3                                            
         ZAP   DIVWRK,PCTBENEF     GET BENEFIT AMOUNT                           
         MP    DIVWRK,SLRTOT                                                    
         DP    DIVWRK,=P'100000'                                                
         ZAP   PBENEF,DIVWRK(12)                                                
         AP    PCOST,PBENEF        ADD TO TOTAL                                 
         ZAP   DIVWRK,PCTADMIN     GET ADMIN. AMOUNT                            
         MP    DIVWRK,SLRTOT                                                    
         DP    DIVWRK,=P'100000'                                                
         ZAP   PADMIN,DIVWRK(12)                                                
         AP    PCOST,PADMIN        ADD TO TOTAL                                 
         SPACE 1                                                                
ADDPCTX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD BENEBFIT ELEMENT                                  
         SPACE 1                                                                
         USING TRCASHD,R4                                                       
         USING PTOTD,R6                                                         
         USING CLID,R7                                                          
BENEL    NTR1                                                                   
         LA    R6,PERWRK                                                        
         ZIC   R3,1(R4)            R4 WAS AT TRANSACTION                        
         AR    R4,R3                                                            
         XC    0(25,R4),0(R4)                                                   
         MVC   TRCSEL(2),=X'500F'                                               
         MVI   TRCSTYPE,C'B'                                                    
         ZAP   CLITIME,CCSTPST       THIS CLIENT POSTING AMOUNT                 
         ZAP   TOTIME,PCOST          TOTAL COST                                 
         ZAP   ALLOC,PBENEF          BENEFIT TO BE ALLOCATED                    
         BAS   RE,DIV                                                           
         ZAP   TRCSAMNT,DIVWRK(8)    THIS CLIENT SHARE OF BENEFIT               
         SPACE 1                                                                
         ZAP   CLITIME,CCSTPST       THIS CLIENT POSTING AMOUNT                 
         ZAP   TOTIME,PCOST          TOTAL COST                                 
         ZAP   ALLOC,PADMIN          ADMIN. TO BE ALLOCATED                     
         BAS   RE,DIV                                                           
         ZAP   TRCSADMN,DIVWRK(8)    THIS CLIENT SHARE OF ADMIN                 
         CP    TRCSAMNT,=P'0'                                                   
         BNE   XIT                                                              
         CP    TRCSADMN,=P'0'                                                   
         BNE   XIT                                                              
         XC    TRCSEL(25),TRCSEL   DON'T ADD ZERO ELEMENTS                      
         B     XIT                                                              
         EJECT                                                                  
DIV      DS    0H                                                               
         ZAP   DIVWRK,CLITIME     THIS CLIENT TIME                              
         MP    DIVWRK,ALLOC+2(6)   AMOUNT TO BE ALLOCATED                       
         MP    DIVWRK,=P'10'                                                    
         CP    TOTIME,=P'0'                                                     
         BNE   *+12                                                             
         ZAP   DIVWRK(8),=P'0'                                                  
         BR    RE                                                               
         DP    DIVWRK,TOTIME+2(6)  TOTAL CLIENT TIME                            
         AP    DIVWRK(10),=P'5'                                                 
         CP    DIVWRK(10),=P'0'                                                 
         BNL   *+10                                                             
         SP    DIVWRK(10),=P'10'                                                
         DP    DIVWRK(10),=P'10'   ALLOCATED AMOUNT DIVWRK 8 BYTES              
         BR    RE                                                               
         SPACE 2                                                                
         USING ACNAMED,R4                                                       
NAMOUT   LTR   R4,R4                                                            
         BZR   RE                                                               
         MVC   0(36,R6),SPACES                                                  
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R6),ACNMNAME                                                 
         EJECT                                                                  
*              ROUTINE TO POST SUMMARY RECORDS                                  
         SPACE 1                                                                
         USING CLID,R7                                                          
         USING SUMD,R8                                                          
SUMPOST  NTR1                                                                   
         CLI   CLILEDG,C'C'        IS IT CLIENT TIME                            
         BNE   SUMIND              NO ADD TO INDIRECT                           
         CLI   NBSW,C'Y'                                                        
         BE    SUMIND              IS IT NEW BUSINESS                           
         AP    SDIRLST,CCSTLST     ADD TO DIRECT AT OFFICE/DEPT LEVEL           
         AP    SDIRYTD,CCSTYTD     ADD TO DIRECT AT OFFICE/DEPT LEVEL           
         AP    SDIRPST,CCSTPST                                                  
         B     SUMXIT                                                           
         SPACE 1                                                                
SUMIND   CLC   CLICDE(2),=C'T '                                                 
         BE    SUMEXEC             EXEC INDIRECT TO OFFICE O/H                  
         LA    R3,SOINDLST                                                      
         LA    R2,SOINDPST                                                      
         CLC   CLICDE(2),=C'O '                                                 
         BE    SUMIND2             OFFICE INDIRECT                              
         CLI   NBSW,C'Y'                                                        
         BNE   *+12                                                             
         CLI   OPTNBT,C'O'                                                      
         BE    SUMIND2             OR NEW BUSINESS AT OFFICE LEVEL              
         LA    R3,SCINDLST                                                      
         LA    R2,SCINDPST                                                      
         CLC   CLICDE(2),=C'C '    CORP                                         
         BE    SUMIND2                                                          
         CLI   NBSW,C'Y'                                                        
         BNE   *+12                                                             
         CLI   OPTNBT,C'C'                                                      
         BE    SUMIND2             OR NEW BUSINESS AT CORP LEVEL                
         LA    R3,SDINDLST         DEPT                                         
         LA    R2,SDINDPST                                                      
         SPACE 1                                                                
SUMIND2  AP    0(8,R3),CCSTLST    ADD AMOUNTS BY TYPE YTD-1                     
         AP    8(8,R3),CCSTYTD    AND YTD                                       
         AP    0(8,R2),CCSTPST                                                  
         B     SUMXIT                                                           
         SPACE 1                                                                
*        ADD DEPT. EXEC TIME TO ACCUMS/DISTRIBUTE WITH OFFICE O/H               
SUMEXEC  LA    RF,DEPCUM                                                        
         AP    DEXTIML-DEPD(L'DEXTIML,RF),CCSTLST                               
         AP    DEXTIMY-DEPD(L'DEXTIMY,RF),CCSTYTD                               
         ZAP   DEXTIMP-DEPD(L'DEXTIMP,RF),DEXTIMY-DEPD(L'DEXTIMY,RF)            
         SP    DEXTIMP-DEPD(L'DEXTIMP,RF),DEXTIML-DEPD(L'DEXTIML,RF)            
         AP    SOVOLST,CCSTLST     ADD TO DEPT OTHER O/H                        
         AP    SOVOYTD,CCSTYTD                                                  
         ZAP   SOVOPST,SOVOYTD                                                  
         SP    SOVOPST,SOVOLST                                                  
SUMXIT   B     XIT                                                              
         EJECT                                                                  
*              ADD UP SUMMARY RECORDS - ADD TO HIGHER LEVELS                    
         SPACE 1                                                                
         USING SUMD,R8                                                          
SUMUP    NTR1                                                                   
         L     R8,0(R1)                                                         
         L     R6,4(R1)                                                         
         LA    R2,SBK                                                           
         LA    R3,SBKCNT                                                        
         CP    0(8,R2),=P'0'                                                    
         BNE   SUMUP3                                                           
         LA    R2,8(R2)                                                         
         BCT   R3,*-14                                                          
         B     XIT                 DON'T ADD IF ALL BUCKETS ZERO                
         SPACE 1                                                                
SUMUP3   GOTO1 BINADD,DMCB,(R8),SUMBUFF,(RC)  ADD IT TO TABLE                   
         LTR   R6,R6                                                            
         BZ    XIT                 NO HIGHER LEVEL                              
         LA    R4,SBK              ADD TO HIGHER LEVEL                          
         LA    R5,SBK-SUMD(R6)                                                  
         LA    R3,SBKCNT                                                        
SUMUP4   AP    0(8,R5),0(8,R4)                                                  
         LA    R4,8(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R3,SUMUP4                                                        
         B     XIT                                                              
         SPACE 1                                                                
*              CLEAR A SUMMARY BUFFER                                           
CLSUM    LA    R2,SBK                                                           
         LA    R3,SBKCNT                                                        
         ZAP   0(8,R2),=P'0'       CLEAR THE BUCKETS                            
         LA    R2,8(R2)                                                         
         BCT   R3,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
*              PRINT DEPT./OFFICE SUMMARIES                                     
         SPACE 1                                                                
         USING BIND,R5                                                          
         USING SUMD,R8                                                          
SUMMARY  NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         L     R5,SUMBUFF                                                       
         L     R3,BININ                   NUMBER OF SUMMARY RECORDS             
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         LA    R8,BINTABLE                                                      
         SPACE 1                                                                
SUMM2    AP    STTIME,SDIRPST      GET LABOR TOTALS                             
         AP    STTIME,SDINDPST                                                  
         AP    STTIME,SOINDPST                                                  
         AP    STTIME,SCINDPST                                                  
         AP    STCOST,STTIME       GET OVERALL TOTAL                            
         AP    STCOST,SOVHPST                                                   
         AP    STCOST,SOVOPST                                                   
         GOTO1 REPORT,DMCB,(3,(RC)),0      PRINT A SUMMARY RECORD               
         LA    R8,SLEN(R8)                                                      
         BCT   R3,SUMM2                                                         
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'              NOW DO YTD                            
         L     R5,SUMBUFF                                                       
         L     R3,BININ                   NUMBER OF SUMMARY RECORDS             
         LA    R8,BINTABLE                                                      
SUMM4    AP    SDIRPST,SDIRLST            ADD YTD-1 TO POSTING                  
         AP    SDINDPST,SDINDLST          TO GET YTD                            
         AP    SOINDPST,SOINDLST                                                
         AP    SCINDPST,SCINDLST                                                
         AP    SOVHPST,SOVHLST                                                  
         AP    SOVOPST,SOVOLST                                                  
         ZAP   STTIME,SDIRPST                                                   
         AP    STTIME,SDINDPST                                                  
         AP    STTIME,SOINDPST                                                  
         AP    STTIME,SCINDPST                                                  
         ZAP   STCOST,STTIME                                                    
         AP    STCOST,SOVHPST                                                   
         AP    STCOST,SOVOPST                                                   
         GOTO1 REPORT,DMCB,(3,(RC)),(1,0)  PRINT A SUMMARY RECORD               
         LA    R8,SLEN(R8)                                                      
         BCT   R3,SUMM4                                                         
         SPACE 1                                                                
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO GET AN ENTRY IN BINSRCH TABLE                         
*              PARAM1              A(KEY)                                       
*              PARAM2              A(BINSRCH PARAMETERS)                        
BINGET   NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ                                                 
         LA    R3,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(R2),(R3)                                           
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    V(SQUASHER)                                                      
         DC    V(UNDERLIN)                                                      
         DC    V(PERVERT)                                                       
         DC    V(ACSLRY)                                                        
         DC    V(HELLO)                                                         
         DC    V(SORTER)                                                        
         DC    1F'0'               SPARE                                        
         DC    A(ACA2POST)         PUT WORKER RECORDS TO ACPOST                 
         DC    A(ACA2BLDP)         BUILD SKELETON POSTING FILE                  
         DC    A(ACA2INDP)         ALLOCATE INDIRECT COST                       
         DC    A(ACA2DOVH)         DISTRIBUTE OVERHEAD                          
         DC    A(ACA2BINS)         ADD ITEM TO BINSRCH TABLE                    
         DC    A(ACA2EXPN)         GET EXPENSES FROM '1C'                       
         DC    A(ACA2DEPG)         BUILD TABLE OF '14' AND '15' NAMES           
         DC    A(ACA2MTCH)         GET NAMES FOR DEPT. GROUPS                   
         DC    A(ACA2GETL)         GET AN ELEMENT                               
         DC    A(ACA2NMEP)         ADD NAME TO BINSRCH TABLE                    
         DC    A(ACA2NMEG)         GET NAME FROM BINSRCH TABLE                  
         DC    A(ACA2REPT)         HANDLE REPORT PRINTING                       
         DC    A(ACA2HEAD)         HEADLINE ROUTINES                            
         DC    A(ACA2BUFO)         ADD DEPT O/H AND INDIRECT TO BUFFALO         
         DC    A(ACA2SRTP)         SORT TRANSACTIONS- **NOT USED** YET          
         DC    A(ACA2GOVH)         GET OVERHEAD AMOUNT                          
         DC    A(ACA2DIRP)         POST DIRECT TIME                             
         DC    A(ABUFPOST)         POST BUFFALO RECORD                          
         DC    A(ACA2INDD)         ALLOCATE FOR INDIRECT DPTS                   
         DC    1F'0'               SPARE                                        
         DC    A(CCLSBUFF)                                                      
         DC    A(CSORTC)                                                        
         DC    A(CGRPTAB)                                                       
         DC    A(CRECORD)                                                       
         DC    A(CPSTBUFF)                                                      
         DC    A(CCLIBUFF)                                                      
         DC    A(CSUMBUFF)                                                      
         DC    A(CNMEBUFF)                                                      
         DC    A(BUFFALOC)                                                      
         DC    1F'0'                                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
STDOPTS  DS    0CL16       LIST OF STANDARD OPTIONS                             
         DC    C'N'        USE DIRECT EXPS FOR CORP OHEAD   Y,N                 
         DC    C'D'        INDIRECT LEVEL IF NO HOURS       D,O,C               
         DC    C'4'        CORP. INDIRECT CONTRA LEDGER     4,5                 
         DC    C'M'        RUN PERIOD-MONTHLY (OR QTR)      M,Q                 
         DC    C'D'        ADD INDIR TO DIRECT OR OHEAD     D,O                 
         DC    C'5'        O'HEAD CONTRA LEDGER             5,4                 
         DC    C'Y'        AGENCY IS YTD ,MONTHLY OR QTR    Y,M,Q               
         DC    C'Y'        NEW PAGE PER PERSON              Y,N                 
         DC    C'5'        CORP. OVERHEAD CONTRA  LEDGER    4,5                 
         DC    X'00'       STANDARD HOURS                   0-200               
         DC    C'N'        CORP. GETS SHARE OF DEPT IND     Y,N                 
         DC    C'O'        OVERHEAD LEVEL IF NO DIRECT TIME O,C                 
         DC    C'N'        CORP. GETS SHARE OF DEPT OVHEAD  Y,N                 
         DC    X'00'       ACCOUNT FOR INDIRECT TIME        0-9,A-Z             
         DC    C'N'        INDIRECT LEVEL FOR N.B. TIME     D,O,C               
         DC    X'00'       OFFICE O'HEAD CONTRA LEDGER                          
*  A3 PROFILES                                                                  
STDA3OP  DS    0CL16                                                            
         DC    X'00'       O'HEAD ACC FOR EXEC INDIRECT                         
         DC    X'00'       ALLOCATE OVERHEAD ON YTD BASIS                       
         DC    X'00'                                                            
         DC    X'00'                                                            
         DC    X'00'                                                            
         DC    X'00'                                                            
         DC    X'00'                                                            
         DC    X'00'                                                            
         DC    X'00'                                                            
         DC    X'00'                                                            
         DC    X'00'                                                            
         DC    X'00'                                                            
         DC    X'00'                                                            
         DC    X'00'                                                            
         DC    X'00'                                                            
         DC    X'00'                                                            
         EJECT                                                                  
*              ACCLAST FOR INDIRECT DEPARTMENTS (BATES)                         
         SPACE 1                                                                
         USING BIND,R5                                                          
ACA2INDD DS    0D                                                               
         NMOD1 0,*CA27*                                                         
         L     RC,0(R1)                                                         
         ZAP   DUB1,=P'0'          FOR YTD-1                                    
         ZAP   DUB2,=P'0'          FOR YTD                                      
         ZAP   DUB3,=P'0'          POSTING AMOUNT                               
         L     R5,CLIBUFF                                                       
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BZ    CA275               NO PRIOR ENTRIES                             
         LA    R7,BINTABLE                                                      
         SPACE 1                                                                
         USING CLID,R7                                                          
CA271    CLC   ALPHAID,=C'BS'         FOR BACKER USE OFFICE                     
         BNE   CA271A                                                           
***      CLC   CLILEDG(7),=C'NO CORP' ****CHANGE TO NO OFFICE IN 93****         
         CLC   CLILEDG(9),=C'NO OFFICE'                                         
         BE    CA274                                                            
         B     CA271B                                                           
CA271A   CLC   CLILEDG(7),=C'NC CORP'  GET YTD-1 FOR CORP INDIR                 
         BE    CA274                                                            
CA271B   LA    R7,CLEN(R7)                                                      
         BCT   R3,CA271                                                         
         B     CA275                 NO PRIOR CORP INDIRECT                     
CA274    ZAP   DUB1,CCSTLST           YTD-1                                     
         USING PTOTD,R6                                                         
CA275    LA    R6,PERWRK                                                        
         BAS   RE,ADDPCT           ADD BENEFIT PERCENT TO COST                  
         ZAP   DUB2,PCOST          YTD COST                                     
         CP    DUB1,=P'0'                                                       
         BNE   *+14                                                             
         CP    DUB2,=P'0'                                                       
         BE    CAX                 NO ACTIVITY                                  
         ZAP   DUB3,DUB2           YTD                                          
         SP    DUB3,DUB1           LESS YTD-1 GIVES POSTING                     
         LA    R7,DEPCUM           ADD TO CORP INDIRECT                         
         USING DEPD,R7                                                          
         AP    DDINDCL,DUB1        FOR DISTRIBUTION AT COMPANY LEVEL            
         AP    DDINDCY,DUB2                                                     
         LA    R8,DEPTOT                                                        
         CLC   ALPHAID,=C'BS'         FOR BACKER ADD IT TO OFFICE               
         BNE   CA276                                                            
         AP    SOINDLST,DUB1                                                    
         AP    SOINDYTD,DUB2                                                    
         B     *+16                                                             
CA276    AP    SCINDLST,DUB1                                                    
         AP    SCINDYTD,DUB2                                                    
         GOTO1 REPORT,DMCB,(0,(RC)),(1,0)  PERSON IN INDIRECT DEPT              
         SPACE 1                                                                
         USING PSHEADD,R2          BUILD DEBIT POSTING TO PERSON                
         LA    R2,T                CONTRA IS 1NC CORP                           
         L     RF,ADACC                                                         
         MVC   OFFCODE,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFCODE(0),3(RF)                                                 
         MVC   PSHDACC,0(RF)                                                    
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVC   PSHDSBAC(1),PSHDACC                COMPANY                       
         MVC   PSHDSBNM,SPACES                                                  
         CLC   ALPHAID,=C'BS'         FOR BACKER ADD IT TO OFFICE               
         BNE   CA277                                                            
         MVC   PSHDSBAC+1(14),=CL14'1NO OFFICE'  UNIT/LEDGER/ACCOUNT            
         MVC   PSHDSBNM(15),=C'OFFICE INDIRECT'                                 
         B     *+16                                                             
CA277    MVC   PSHDSBAC+1(14),=CL14'1NC CORP'    UNIT/LEDGER/ACCOUNT            
         MVC   PSHDSBNM(13),=C'CORP INDIRECT'                                   
         MVI   TRNSSTAT,X'80'                                                   
         ZAP   TRNSAMNT,DUB3                                                    
         LA    R7,CLIWRK                                                        
         USING CLID,R7                                                          
         ZAP   CCSTPST,DUB3        GIVE ALL BEN. AND ADM. TO CORP               
         BAS   RE,BENEL            ADD BENEFIT ELEMENT                          
         MVC   TRNSANAL,OFFCODE                                                 
         GOTO1 POSTIT,DMCB,(RC)                                                 
CAX      XMOD1                                                                  
         EJECT                                                                  
*              ROUTINE TO POST DIRECT TIME                                      
ACA2DIRP DS    0D                                                               
         NMOD1 0,*DIRP*                                                         
         L     RC,0(R1)                                                         
         MVC   TOTLINE(80),=10PL8'0'                                            
         LA    R3,BUFBYGP                                                       
         BAS   RE,TOTDIR           GET TOTAL DIRECT TIME                        
         CLI   DMCB+8,0                                                         
         BNE   DIRNB               NO DIRECT TIME - SEE IF NEW BUS.             
         SPACE 1                                                                
         USING BUFD,R5                                                          
DIRP1    LA    R5,BUFWRK                                                        
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         ZIC   R0,LEVEL                                                         
         STC   R3,BUFTYPE          FIRST RECORD WAS TOTAL FROM TOTDIR           
DIRP2    GOTO1 BUFFALO,DMCB,=C'SEQ',((R3),ADBUFC),(R5),(R0)                     
         TM    DMCB+8,X'80'                                                     
         BO    DIRNB               EOF  DO NEW BUSINESS DIRECT                  
         SPACE 1                                                                
         USING PSHEADD,R2                                                       
         LA    R2,T                SET FOR CREDIT TO CLIENT                     
         L     RF,ADUNIT           FOR DIRECT TIME                              
         MVC   PSHDACC(2),0(RF)                                                 
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVC   PSHDACC+2(13),BUFCLI                                             
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC(2),PSHDACC                                              
         MVI   PSHDSBAC+2,C'4'                                                  
         MVC   PSHDSBAC+3(1),BUFGRP                                             
         MVC   SVGP,PSHDSBAC+2     LEDGER/ACCOUNT                               
         GOTO1 GRPMTCH,DMCB,(RC)                                                
         MVC   PSHDSBNM,WORK                                                    
         MVI   TRNSSTAT,0                                                       
         ZAP   TRNSAMNT,BDIRPST    DIRECT                                       
         L     RF,ADHEIRB                                                       
         MVC   OFFCODE,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFCODE(0),3(RF)                                                 
         MVC   TRNSANAL,OFFCODE                                                 
         GOTO1 POSTIT,DMCB,(RC)                                                 
         B     DIRP2                                                            
         EJECT                                                                  
*              POST DIRECT TIME FOR NEW BUSINESS                                
         SPACE 1                                                                
DIRNB    LA    R3,BUFNEWB                                                       
         BAS   RE,TOTDIR           GET TOTAL DIRECT TIME                        
         CLI   DMCB+8,0                                                         
         BNE   DIRPXIT             NO DIRECT TIME                               
         SPACE 1                                                                
         USING BUFD,R5                                                          
         LA    R5,BUFWRK                                                        
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         ZIC   R0,LEVEL                                                         
         STC   R3,BUFTYPE          FIRST RECORD WAS TOTAL FROM TOTDIR           
DIRNB2   GOTO1 BUFFALO,DMCB,=C'SEQ',((R3),ADBUFC),(R5),(R0)                     
         TM    DMCB+8,X'80'                                                     
         BO    DIRPXIT             EOF                                          
         SPACE 1                                                                
         USING PSHEADD,R2                                                       
         LA    R2,T                SET FOR CREDIT TO CLIENT                     
         L     RF,ADUNIT           FOR DIRECT TIME                              
         MVC   PSHDACC(2),0(RF)                                                 
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVC   PSHDACC+2(13),BUFCLI                                             
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC(2),PSHDACC                                              
         MVI   PSHDSBAC+2,C'4'                                                  
         MVC   PSHDSBAC+3(1),BUFGRP                                             
         MVC   SVGP,PSHDSBAC+2     LEDGER/ACCOUNT                               
         GOTO1 GRPMTCH,DMCB,(RC)                                                
         MVC   PSHDSBNM,WORK                                                    
         MVI   TRNSSTAT,0                                                       
         ZAP   TRNSAMNT,BDIRPST    DIRECT                                       
         L     RF,ADHEIRB                                                       
         MVC   OFFCODE,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFCODE(0),3(RF)                                                 
         MVC   TRNSANAL,OFFCODE                                                 
         GOTO1 POSTIT,DMCB,(RC)                                                 
         B     DIRNB2                                                           
         SPACE 1                                                                
         EJECT                                                                  
*              ROUTINE TO READ TOTAL DIRECT TIME                                
         SPACE 1                                                                
         USING BUFD,R5                                                          
TOTDIR   NTR1                                                                   
         ZAP   DIRLST,=P'0'                                                     
         ZAP   DIRYTD,=P'0'                                                     
         LA    R5,BUFWRK                                                        
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         ZIC   R0,LEVEL            SHOULD BE DEPT. LEVEL ONLY                   
         STC   R3,BUFTYPE          BY GROUP OR NEW BUSINESS                     
         MVI   BUFCLI,BUFTOT       TOTAL RECORD                                 
         GOTO1 BUFFALO,DMCB,=C'GET',((R3),ADBUFC),(R5),(R0)                     
         CLI   DMCB+8,0                                                         
         BNE   DIRPXIT             NO DIRECT TIME                               
         ZAP   DIRLST,BDIRLST                                                   
         ZAP   DIRYTD,BDIRYTD                                                   
         SPACE 1                                                                
DIRPXIT  XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ALLOCATE INDIRECT AT END OF DEPT/OFFICE/REQ '         
ACA2INDP DS    0D                                                               
         NMOD1 0,*INDP*                                                         
         L     RC,0(R1)                                                         
         MVC   TOTLINE(80),=10PL8'0'                                            
         SPACE 1                                                                
         USING SUMD,R8                                                          
         CLI   LEVEL,DEPT                                                       
         BNE   INDPO                                                            
         CLI   OPTINDC,C'Y'        DOES CORP GET SHARE OF DEPT. IND             
         BNE   *+8                                                              
         BAS   RE,CORPIND          GIVE CORP ITS SHARE                          
         LA    R8,DEPTOT                                                        
         ZAP   INDLST,SDINDLST            DEPT INDIRECT YTD-1                   
         ZAP   INDYTD,SDINDYTD            DEPT INDIRECT YTD                     
         B     INDPALL                                                          
         SPACE 1                                                                
INDPO    CLI   LEVEL,OFFICE                                                     
         BNE   INDPC                                                            
         LA    R8,OFFTOT                                                        
         ZAP   INDLST,SOINDLST            OFFICE INDIRECT YTD-1                 
         ZAP   INDYTD,SOINDYTD            OFFICE INDIRECT YTD                   
         B     INDPALL                                                          
         SPACE 1                                                                
INDPC    CLI   LEVEL,COMP                                                       
         BNE   INDXIT                                                           
         LA    R8,AGYTOT                                                        
         ZAP   INDLST,SCINDLST            AGENCY INDIRECT YTD-1                 
         ZAP   INDYTD,SCINDYTD            AGENCY INDIRECT YTD                   
         SPACE 1                                                                
INDPALL  BAS   RE,TOTDIR2                 GET TOTALS FOR DIRECT                 
         USING BUFD,R5                                                          
         LA    R5,BUFWRK                                                        
         CLC   ALPHAID,=C'BS'      FOR BACKER                                   
         BNE   INDPA1                                                           
         CLI   LEVEL,DEPT          CHECK PROF 1 AT OFF AND COMP LEVEL           
         BE    INDPA2                                                           
         CLI   LEVEL,COMP                                                       
         BE    INDPA1A                                                          
         L     RF,ADHEIRB          SPECIAL FOR BACKER OFFICE X                  
         CLI   3(RF),C'X'                                                       
         BNE   INDPA1A                                                          
         ZAP   DIRLST,=P'0'                                                     
         CLC   QEND(4),QSTART      1 MONTH ONLY SKIP YTD-1                      
         BNH   *+10                                                             
         ZAP   DIRLST,=P'100'      OFFICE DIRECT TIMEYTD-1                      
         ZAP   DIRYTD,=P'100'      OFFICE DIRECT TIME YTD                       
         B     INDPA5                                                           
INDPA1   CLI   LEVEL,COMP                                                       
         BNE   INDPA2                                                           
INDPA1A  CLI   OPTEXP,C'I'         INCLUDE DEPT INDIRECT                        
         BE    *+12                IN BASIS FOR ALLOCATION                      
         CLI   OPTEXP,C'Y'         INCLUDE EXPENSES/DEPT INDIRECT               
         BNE   INDPA2              AND OVERHEAD                                 
         CLI   OPTYTD,C'Y'                                                      
         BE    INDPA1B                                                          
         ZAP   DUB,BEXPYTD                                                      
         SP    DUB,BEXPLST                                                      
         AP    DIRYTD,DUB                                                       
         B     INDPA2                                                           
INDPA1B  AP    DIRLST,BEXPLST            ADD EXPENSES/INDIRECT/OVH              
         AP    DIRYTD,BEXPYTD                                                   
         SPACE 1                                                                
INDPA2   CP    DIRLST,=P'0'        IF NO DIRECT TIME LST                        
         BNE   INDPA3                                                           
         BAS   RE,NODIRLST         MOVE INDLST UP ONE LEVEL                     
INDPA3   CP    DIRYTD,=P'0'                                                     
         BNE   INDPA5              IF YTD DIRECT,  ALLOCATE IND                 
         CP    DIRLST,=P'0'                                                     
         BE    INDPA4      IF YTD-1 WAS ZERO NO NEED TO UNDO POSTINGS           
         BAS   RE,INDP1  IF YTD-1 NOT ZERO MUST UNDO POSTINGS                   
         ZAP   INDLST,DUB1     RESTORE INDIRECT AMOUNTS                         
         ZAP   INDYTD,DUB2                                                      
INDPA4   BAS   RE,NODIRYTD         NO DIRECT DEPT COST  YTD                     
         B     INDXIT                                                           
INDPA5   BAS   RE,INDP1                                                         
         B     INDXIT                                                           
         EJECT                                                                  
         USING BUFD,R5                                                          
INDP1    NTR1                                                                   
         LA    R5,BUFWRK                                                        
         ZAP   DUB1,INDLST         SAVE INDIRECT YTD-1                          
         ZAP   DUB2,INDYTD         AND YTD                                      
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         CLC   ALPHAID,=C'BS'      FOR BACKER                                   
         BNE   INDP2                                                            
         CLI   LEVEL,OFFICE                                                     
         BNE   INDP2                                                            
         L     RF,ADHEIRB          SPECIAL FOR BACKER OFFICE X                  
         CLI   3(RF),C'X'                                                       
         BE    INDP2A              SKIP BUFFALO READ FOR OFFICE X               
INDP2    ZIC   R0,LEVEL                                                         
         LA    R3,BUFBYGP          INDIRECT IS ALLOCATED BY GROUP               
         STC   R3,BUFTYPE          FIRST RECORD WAS TOTAL FROM TOTDIR           
         GOTO1 BUFFALO,DMCB,=C'SEQ',((R3),ADBUFC),(R5),(R0)                     
         TM    DMCB+8,X'80'                                                     
         BO    INDP17              EOF                                          
INDP2A   CLC   ALPHAID,=C'BS'      FOR BACKER                                   
         BNE   INDP2B                                                           
         CLI   LEVEL,DEPT          CHECK PROF 1 AT OFF AND COMP LEVEL           
         BE    INDP3                                                            
         CLI   LEVEL,COMP                                                       
         BE    INDP2C                                                           
         L     RF,ADHEIRB          SPECIAL FOR BACKER OFFICE X                  
         CLI   3(RF),C'X'                                                       
         BNE   INDP2C                                                           
         ZAP   BDIRLST,=P'0'                                                    
         CLC   QEND(4),QSTART      1 MONTH ONLY SKIP YTD-1                      
         BNH   *+10                                                             
         ZAP   BDIRLST,=P'100'     CLIENT DIRECT TIME YTD -1                    
         ZAP   BDIRYTD,=P'100'     CLIENT DIRECT TIME YTD                       
         MVI   GROUP,C'X'                                                       
         MVI   BUFGRP,C'X'                                                      
         MVC   BUFCLI,=C'C1ZZZXXX     '                                         
         B     INDP4                                                            
INDP2B   CLI   LEVEL,COMP                                                       
         BNE   INDP3                                                            
INDP2C   CLI   OPTEXP,C'I'                                                      
         BE    *+12                                                             
         CLI   OPTEXP,C'Y'                                                      
         BNE   INDP3                                                            
         CLI   OPTYTD,C'Y'                                                      
         BE    INDP2D                                                           
         ZAP   DUB,BEXPYTD                                                      
         SP    DUB,BEXPLST                                                      
         AP    BDIRYTD,DUB                                                      
         B     INDP3                                                            
INDP2D   AP    BDIRLST,BEXPLST     ADD EXPENSES/INDIRECT/OVH                    
         AP    BDIRYTD,BEXPYTD                                                  
         SPACE 1                                                                
INDP3    CP    BDIRLST,=P'0'                                                    
         BNE   INDP4                                                            
         CP    BDIRYTD,=P'0'                                                    
         BE    INDP2               SKIP THE ZEROS                               
INDP4    ZAP   CLITIME,BDIRLST                                                  
         ZAP   ALLOC,DUB1          INDIRECT YTD-1                               
         ZAP   TOTIME,DIRLST       AGYTIME                                      
         BAS   RE,INDDIV           GET AMOUNT OF IND FOR THIS CLIENT            
         ZAP   INDLST,DIVWRK(8)                                                 
         SPACE 1                                                                
         ZAP   CLITIME,BDIRYTD                                                  
         ZAP   ALLOC,DUB2          INDIRECT YTD                                 
         ZAP   TOTIME,DIRYTD       AGYTIME                                      
         BAS   RE,INDDIV           GET AMOUNT OF IND FOR THIS CLIENT            
         ZAP   INDYTD,DIVWRK(8)                                                 
         ZAP   INDPST,INDYTD                                                    
         SP    INDPST,INDLST                                                    
         SPACE 1                                                                
         USING PSHEADD,R2                                                       
         LA    R2,T                SET FOR CREDIT TO CLIENT                     
         L     RF,ADUNIT           FOR INDIRECT                                 
         MVC   PSHDACC(2),0(RF)                                                 
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVC   PSHDACC+2(13),BUFCLI                                             
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC(2),PSHDACC                                              
         MVI   PSHDSBAC+2,C'4'                                                  
         MVC   PSHDSBAC+3(1),BUFGRP     GROUP                                   
         CLI   MODE,REQLAST                                                     
         BNE   INDP5                                                            
         MVC   PSHDSBAC+2(1),OPT45                                              
         MVC   PSHDSBAC+3(1),GROUP    CORP. INDIRECT                            
         B     INDP6                                                            
INDP5    CLC   ALPHAID,=C'BS'         FOR BACKER                                
         BNE   INDP6                                                            
         CLI   LEVEL,OFFICE           OFFICE INDIR GOES TO                      
         BNE   INDP6                                                            
         MVC   PSHDSBAC+2(1),OPT54    WITH OVERHEAD                             
         L     RF,ADHEIRB                                                       
         MVC   PSHDSBAC+3(1),3(RF)    BY 1R OFFICE CODE                         
*                                                                               
INDP6    MVC   SVGP,PSHDSBAC+2     LEDGER/ACCOUNT                               
         GOTO1 GRPMTCH,DMCB,(RC)                                                
         MVC   PSHDSBNM,WORK                                                    
         MVI   TRNSSTAT,0                                                       
         ZAP   TRNSAMNT,INDPST    DIRECT                                        
         L     RF,ADHEIRB                                                       
         MVC   OFFCODE,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFCODE(0),3(RF)                                                 
         MVC   TRNSANAL,OFFCODE                                                 
         CLI   MODE,REQLAST                                                     
         BE    INDP10                                                           
         SPACE 1                                                                
*              NOW POST THE INDIRECT                                            
         CLI   OPTIND,C'D'             POSTED WITH DIRECT                       
         BE    INDP12                                                           
         MVC   PSHDSBAC+2(1),OPT54      OR WITH OVERHEAD                        
         MVC   PSHDSBAC+3(1),GROUP      LAST KNOWN GROUP AS CONTRA ACCT         
         MVC   SVGP,PSHDSBAC+2          LEDGER/ACCOUNT                          
         GOTO1 GRPMTCH,DMCB,(RC)                                                
         MVC   PSHDSBNM,WORK                                                    
INDP10   CLI   INDACC,0                                                         
         BE    INDP12                                                           
         CLI   INDACC,C'0'                                                      
         BE    INDP12                                                           
         MVC   PSHDSBAC+3(1),INDACC INDIRECT ACCOUNT IF USED                    
INDP12   ZAP   TRNSAMNT,INDPST                                                  
         GOTO1 POSTIT,DMCB,(RC)                                                 
         BAS   RE,ABSORB           MOVE INDIRECT TO ABSORBED                    
         USING BCLID,R8                                                         
         LA    R8,NMEWRK           GET CLIENT NAME FROM NMEBUFF                 
         MVC   BCLICDE,BUFCLI+1                                                 
         GOTO1 NMEGET,DMCB,(RC)                                                 
         GOTO1 REPORT,DMCB,(2,(RC)),0   PRINT A CLIENT LINE                     
         SPACE 1                                                                
         CLI   IND,C'Y'            IS INDIRECT IN EFFECT                        
         BNE   INDP15                                                           
         MVI   TRNSSTAT,X'80'      MAKE DOUBLE ENTRY OF INDIRECT                
         MVC   PSHDSBAC,PSHDACC                                                 
         MVC   PSHDACC,INDSAVE                                                  
         MVC   PSHDSBNM,BCLINME                                                 
         GOTO1 POSTIT,DMCB,(RC)                                                 
INDP15   CLC   ALPHAID,=C'BS'      IS THIS BACKER ?                             
         BNE   INDP2               NO GET NEXT CLIENT                           
         CLI   LEVEL,OFFICE                                                     
         BNE   INDP2                                                            
         L     RF,ADHEIRB          SPECIAL FOR BACKER OFFICE X                  
         CLI   3(RF),C'X'                                                       
         BNE   INDP2               IF OFFICE X FOR BACKER YOUR FINISHED         
         SPACE 1                                                                
INDP17   CLC   TOTLINE(80),=10PL8'0'                                            
         BE    INDP18              NO ACTIVITY                                  
         GOTO1 REPORT,DMCB,(2,(RC)),(C'T',0)                                    
INDP18   BAS   RE,POSTNBT          POST NEW BUSINESS TIME                       
         B     INDXIT                                                           
         EJECT                                                                  
*              ROUTINE TO MOVE INDIRECT TO ABSORBED COLUMN                      
         SPACE 1                                                                
         USING SUMD,R8                                                          
ABSORB   NTR1                                                                   
         CLI   MODE,LEVBLAST                                                    
         BNE   ABSORB3             END OF DEPT                                  
         USING DEPD,R7                                                          
         LA    R7,DEPCUM                                                        
         AP    DDINDAL,INDLST      KEEP TRACK OF ABSORBED                       
         AP    DDINDAY,INDYTD                                                   
         AP    DDINDAP,INDPST                                                   
         CLI   OPTEXP,C'I'                                                      
         BE    *+12                                                             
         CLI   OPTEXP,C'Y'                                                      
         BNE   INDXIT                                                           
         LA    R6,CLSWRK                                                        
         USING CLSD,R6                                                          
         XC    CLSWRK,CLSWRK       ADD INDIRECT BY CLIENT                       
         MVC   CLSCDE,BUFCLI                                                    
         ZAP   DUB,INDLST                                                       
         CVB   R1,DUB                                                           
         ST    R1,CLSINLST                                                      
         ZAP   DUB,INDYTD                                                       
         CVB   R1,DUB                                                           
         ST    R1,CLSINYTD                                                      
         GOTO1 BINADD,DMCB,(R6),CLSBUFF,(RC)                                    
         B     INDXIT                                                           
         SPACE 1                                                                
         USING SUMD,R8                                                          
ABSORB3  CLI   MODE,LEVALAST                                                    
         BNE   ABSORB4                                                          
         LA    R8,DEPT99                                                        
         AP    SDINDLST,INDLST               PUT TO ABSORB AT DEP 99            
         AP    SDINDYTD,INDYTD                                                  
         AP    SDINDPST,INDPST                                                  
         SP    SOINDLST,INDLST                OUT OF OFFICE AT DEP99            
         SP    SOINDYTD,INDYTD                                                  
         SP    SOINDPST,INDPST                                                  
         USING OFFD,R7                                                          
         LA    R7,OFFCUM                                                        
         AP    OFINDAL,INDLST      KEEP TRACK OF ABSORBED                       
         AP    OFINDAY,INDYTD                                                   
         AP    OFINDAP,INDPST                                                   
         B     INDXIT                                                           
         SPACE 1                                                                
ABSORB4  CLI   MODE,REQLAST                                                     
         BE    ABSORB4A                                                         
         MVI   POSTERR,C'Y'                                                     
         GOTO1 REPORT,DMCB,(2,(RC)),(3,0)                                       
         B     INDXIT                                                           
ABSORB4A LA    R8,OFF99                                                         
         AP    SDINDLST,INDLST               PUT TO ABSORB AT OFF 99            
         AP    SDINDYTD,INDYTD                                                  
         AP    SDINDPST,INDPST                                                  
         SP    SCINDLST,INDLST               OUT OF CORP AT 9999                
         SP    SCINDYTD,INDYTD                                                  
         SP    SCINDPST,INDPST                                                  
         USING AGYD,R7                                                          
         LA    R7,AGYCUM                                                        
         AP    AGINDAL,INDLST      KEEP TRACK OF ABSORBED                       
         AP    AGINDAY,INDYTD                                                   
         AP    AGINDAP,INDPST                                                   
         B     INDXIT                                                           
         EJECT                                                                  
*               ADD INDIRECT TO HIGHER LEVEL IF NO DIRECT TIME LST              
         SPACE 1                                                                
NODIRLST NTR1                                                                   
         CP    INDLST,=P'0'                                                     
         BE    INDXIT           NO INDIRECT                                     
         CLI   MODE,LEVBLAST                                                    
         BNE   NODIRL4                                                          
         LA    R8,DEPTOT                                                        
         AP    SOINDLST,INDLST       ADD DEPT TO OFFICE                         
         ZAP   SOINDPST,SOINDYTD                                                
         SP    SOINDPST,SOINDLST                                                
         ZAP   SDINDLST,=P'0'        CLEAR DEPT TOTALS                          
         ZAP   SDINDPST,SDINDYTD     POST NOW = YTD SINCE YTD-1                 
*                                    WAS MOVED UP TO OFFICE                     
         B     INDXIT                                                           
         SPACE 1                                                                
NODIRL4  CLI   MODE,LEVALAST                                                    
         BE    NODIRL4A                                                         
         MVI   POSTERR,C'Y'                                                     
         GOTO1 REPORT,DMCB,(2,(RC)),(3,0)                                       
         B     INDXIT                                                           
NODIRL4A LA    R8,DEPT99                                                        
         AP    SCINDLST,INDLST               ADD TO CORP DEPT99                 
         ZAP   SCINDPST,SCINDYTD                                                
         SP    SCINDPST,SCINDLST                                                
*                                                                               
         SP    SOINDLST,INDLST               SUBTRACT FROM OFFICE               
         ZAP   SOINDPST,SOINDYTD                                                
         SP    SOINDPST,SOINDLST                                                
*                                                                               
*        ZAP   SOINDLST,=P'0'                SUBTRACT FROM OFFICE               
*        ZAP   SOINDPST,SOINDYTD                                                
*                                                                               
         B     INDXIT                                                           
         EJECT                                                                  
*               ADD INDIRECT TO HIGHER LEVEL IF NO DIRECT TIME YTD              
         SPACE 1                                                                
NODIRYTD NTR1                                                                   
         CLC   INDLST(24),=10PL8'0'                                             
         BE    INDXIT           NO INDIRECT                                     
         CLI   MODE,LEVBLAST                                                    
         BNE   NODIR4                                                           
         LA    R8,DEPTOT                                                        
         AP    SOINDYTD,INDYTD                                                  
         ZAP   SOINDPST,SOINDYTD                                                
         SP    SOINDPST,SOINDLST                                                
         ZAP   SDINDYTD,=P'0'                                                   
         ZAP   SDINDPST,SDINDYTD                                                
         SP    SDINDPST,SDINDLST                                                
         ZAP   INDPST,INDYTD                                                    
         SP    INDPST,INDLST                                                    
         GOTO1 REPORT,DMCB,(2,(RC)),(1,0)                                       
         B     INDXIT                                                           
         SPACE 1                                                                
NODIR4   CLI   MODE,LEVALAST                                                    
         BE    NODIR4A                                                          
         MVI   POSTERR,C'Y'                                                     
         GOTO1 REPORT,DMCB,(2,(RC)),(3,0)                                       
         B     INDXIT                                                           
NODIR4A  LA    R8,DEPT99                    ADD TO DEPT 99 - CORP               
         AP    SCINDYTD,INDYTD                                                  
         ZAP   SCINDPST,SCINDYTD                                                
         SP    SCINDPST,SCINDLST                                                
*                                                                               
         SP    SOINDYTD,INDYTD               SUBTRACT FROM OFFICE               
         ZAP   SOINDPST,SOINDYTD                                                
         SP    SOINDPST,SOINDLST                                                
*                                                                               
*        ZAP   SOINDYTD,=P'0'                SUBTRACT FROM OFFICE               
*        ZAP   SOINDPST,SOINDYTD                                                
*        SP    SOINDPST,SOINDLST                                                
*                                                                               
         ZAP   INDPST,INDYTD                                                    
         SP    INDPST,INDLST                                                    
         GOTO1 REPORT,DMCB,(2,(RC)),(1,0)                                       
INDXIT   XMOD1 1                                                                
         EJECT                                                                  
*              ROUTINE TO POST NEW BUSINESS CLIENTS TO INDIRECT                 
         SPACE 1                                                                
POSTNBT  NTR1                                                                   
         CLI   OPTNBT,C'C'         POST N.B AT CORP                             
         BNE   PSTNBTO                                                          
         CLI   LEVEL,COMP                                                       
         BNE   INDXIT           ONLY AT OFFICE                                  
         B     PSTNBT1                                                          
PSTNBTO  CLI   OPTNBT,C'O'         POST N.B AT OFFICE                           
         BNE   PSTNBTD                                                          
         CLI   LEVEL,OFFICE                                                     
         BNE   INDXIT           ONLY AT OFFICE                                  
         B     PSTNBT1                                                          
PSTNBTD  CLI   OPTNBT,C'D'         POST N.B AT DEPT                             
         BNE   INDXIT                                                           
         CLI   LEVEL,DEPT                                                       
         BNE   INDXIT                                                           
         USING BUFD,R5                                                          
PSTNBT1  ZIC   R0,LEVEL                                                         
         LA    R3,BUFNEWB                                                       
         LA    R5,BUFWRK                                                        
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         STC   R3,BUFTYPE          NEW BUSINESS CLIENTS                         
         MVI   BUFCLI,BUFTOT       TOTAL RECORD FOR THIS TYPE                   
         GOTO1 BUFFALO,DMCB,=C'GET',((R3),ADBUFC),(R5),(R0)                     
         CLI   DMCB+8,0                                                         
         BNE   INDXIT           NO NEW BUSINESS TIME                            
         SPACE 1                                                                
PSTNBT2  GOTO1 BUFFALO,DMCB,=C'SEQ',((R3),ADBUFC),(R5),(R0)                     
         TM    DMCB+8,X'80'                                                     
         BO    INDXIT           END OF NEW BUSNESS CLIENTS                      
         SPACE 1                                                                
         USING PSHEADD,R2                                                       
         LA    R2,T                POST MINUS CREDIT TO N.B. CLIENT             
         L     RF,ADUNIT                                                        
         MVC   PSHDACC(2),0(RF)    COMPANY/UNIT                                 
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVC   PSHDACC+2(13),BUFCLI    CLIENT                                   
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC(2),PSHDACC     COMPANY UNIT                             
         MVC   PSHDSBAC+2(1),OPT54     CONTRA LEDGER IS 5 OR 4                  
         MVC   PSHDSBAC+3(1),GROUP     O/H ACCOUNT  GROUP                       
         MVC   SVGP,PSHDSBAC+2         LEDGER/ACCOUNT                           
         GOTO1 GRPMTCH,DMCB,(RC)                                                
         MVC   PSHDSBNM,WORK           GROUP NAME TO CONTRA/ACCT                
         MVI   TRNSSTAT,0              CREDIT                                   
         ZAP   TRNSAMNT,BDIRPST        POSTING AMOUNT                           
         MP    TRNSAMNT,=P'-1'         NEGATIVE                                 
         L     RF,ADACC                                                         
         MVC   OFFCODE,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFCODE(0),3(RF)                                                 
         MVC   TRNSANAL,OFFCODE                                                 
         GOTO1 POSTIT,DMCB,(RC)                                                 
         B     PSTNBT2                                                          
         EJECT                                                                  
*              ROUTINE TO ALLOC. DEPT INDIRECT TO CORP.                         
*              (FOR BACKER ALLOC DPT INDIRECT TO OFFICE)                        
         SPACE 1                                                                
         USING SUMD,R8                                                          
CORPIND  NTR1                                                                   
         BAS   RE,TOTDIR2             GET TOTALS FOR DIRECT                     
         LA    R8,DEPTOT                                                        
         ZAP   DUB,SCINDLST           THIS DEPT'S CORP INDIRECT                 
         CLC   ALPHAID,=C'BS'         FOR BACKER USE OFFICE                     
         BNE   *+10                                                             
         ZAP   DUB,SOINDLST           THIS DEPT'S OFFICE INDIRECT               
         ZAP   CLITIME,DUB            THIS DEPT'S CORP INDIRECT                 
         ZAP   ALLOC,SDINDLST         DEPT INDIRECT                             
         ZAP   TOTIME,DUB             DEPT CORP                                 
         AP    TOTIME,DIRLST          PLUS CLIENT TIME = TOTLINE                
         BAS   RE,INDDIV              AMOUNT OF IND. TO CORP                    
         ZAP   INDLST,DIVWRK(8)                                                 
         SPACE 1                                                                
         ZAP   DUB,SCINDYTD           THIS DEPT'S CORP INDIRECT                 
         CLC   ALPHAID,=C'BS'         FOR BACKER USE OFFICE                     
         BNE   *+10                                                             
         ZAP   DUB,SOINDYTD           THIS DEPT'S OFFICE INDIRECT               
         ZAP   CLITIME,DUB            THIS DEPT'S CORP INDIRECT                 
         ZAP   ALLOC,SDINDYTD         DEPT INDIRECT                             
         ZAP   TOTIME,DUB             DEPT CORP                                 
         AP    TOTIME,DIRYTD          PLUS CLIENT TIME = TOTLINE                
         BAS   RE,INDDIV              AMOUNT OF IND. TO CORP                    
         ZAP   INDYTD,DIVWRK(8)                                                 
         ZAP   INDPST,INDYTD                                                    
         SP    INDPST,INDLST                                                    
         SPACE 1                                                                
         SP    SDINDLST,INDLST        TAKE IT OUT OF DEPT                       
         SP    SDINDYTD,INDYTD                                                  
         SP    SDINDPST,INDPST                                                  
         CLC   ALPHAID,=C'BS'         FOR BACKER ADD IT TO OFFICE               
         BNE   CORPIND1                                                         
         AP    SOINDLST,INDLST        ADD IT TO OFFICE                          
         AP    SOINDYTD,INDYTD                                                  
         AP    SOINDPST,INDPST                                                  
         B     CORPIND2                                                         
CORPIND1 AP    SCINDLST,INDLST        ADD IT TO CORP                            
         AP    SCINDYTD,INDYTD                                                  
         AP    SCINDPST,INDPST                                                  
         SPACE 1                                                                
CORPIND2 DS    0H                                                               
         USING ACSTATD,R2                                                       
         L     R2,ADLVBSTA                                                      
         TM    ACSTSTX,X'80'       IS THIS AN INDIRECT DEPT.                    
         BO    CORPIND4            YES SO JUST PRINT MESSAGE                    
         SPACE 1                                                                
         USING PSHEADD,R2                                                       
         LA    R2,T                BUILD MINUS DEBIT POSTING TO DUMMY           
         L     RF,ADHEIRB          ACCOUNT TO TAKE OUT DEPT                     
         MVC   PSHDACC,SPACES                                                   
         MVC   PSHDACC,0(RF)           C/U/L OFFICE/DEPT                        
         MVC   PSHDACC+6(5),=C'00ZZZ' SUB-DEPT/PERSON                           
         MVC   PSHDSBAC(1),PSHDACC                COMPANY                       
         MVC   PSHDSBAC+1(14),=CL14'1ND DEPT'    UNIT/LEDGER/ACCOUNT            
         MVC   PSHDSBNM,SPACES                                                  
         MVC   PSHDSBNM(13),=C'DEPT INDIRECT'                                   
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVI   TRNSSTAT,X'80'                                                   
         ZAP   TRNSAMNT,INDPST                                                  
         MP    TRNSAMNT,=P'-1'                                                  
         ZAP   DUB,PCTTOT         GET NET AMOUNT INTO NET                       
         AP    DUB,=P'100000'                                                   
         ZAP   NET,TRNSAMNT                                                     
         MP    NET,=P'100000'                                                   
         DP    NET,DUB+4(4)                                                     
         BAS   RE,ADDTONET         GET BENEFIT AMOUNTS                          
         LA    R7,CLIWRK                                                        
         USING CLID,R7                                                          
         ZAP   CCSTPST,NET(12)     USE NET AMOUNT TO CALC.                      
         LA    R6,PERWRK           BENEFIT AND ADMIN                            
         USING PTOTD,R6                                                         
         ZAP   PCOST,NET(12)       100 PERCENT TO THIS POSTING                  
         BAS   RE,BENEL2           ADD BENEFIT ELEMENT                          
         GOTO1 POSTIT,DMCB,(RC)                                                 
         SPACE 1                                                                
         USING PSHEADD,R2                                                       
         LA    R2,T                BUILD  DEBIT POSTING TO DUMMY                
         MVC   PSHDSBNM,SPACES                                                  
         CLC   ALPHAID,=C'BS'         FOR BACKER ADD IT TO OFFICE               
         BNE   CORPIND3                                                         
         MVC   PSHDSBAC+1(14),=CL14'1NO OFFICE'  UNIT/LEDGER/ACCOUNT            
         MVC   PSHDSBNM(15),=C'OFFICE INDIRECT'                                 
         B     *+16                                                             
CORPIND3 MVC   PSHDSBAC+1(14),=CL14'1NC CORP'    UNIT/LEDGER/ACCOUNT            
         MVC   PSHDSBNM(13),=C'CORP INDIRECT'                                   
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MP    TRNSAMNT,=P'-1'                                                  
         MP    NET(12),=P'-1'                                                   
         BAS   RE,ADDTONET                                                      
         LA    R7,CLIWRK                                                        
         USING CLID,R7                                                          
         ZAP   CCSTPST,NET(12)     USE NET AMOUNT TO CALC.                      
         LA    R6,PERWRK           BENEFIT AND ADMIN                            
         USING PTOTD,R6                                                         
         ZAP   PCOST,NET(12)                                                    
         BAS   RE,BENEL2           ADD BENEFIT ELEMENT                          
         GOTO1 POSTIT,DMCB,(RC)                                                 
         B     INDXIT                                                           
         SPACE 1                                                                
CORPIND4 GOTO1 REPORT,DMCB,(2,(RC)),(2,0)   PRINT MESSAGE AND AMOUNT            
         USING DEPD,R7                                                          
         LA    R7,DEPCUM                                                        
         AP    DDINDCL,SDINDLST    INDIRECT DEPT TO CORP                        
         AP    DDINDCY,SDINDYTD                                                 
         ZAP   SDINDLST,=P'0'      CLEAR DEPT INDIRECT                          
         ZAP   SDINDYTD,=P'0'                                                   
         ZAP   SDINDPST,=P'0'                                                   
         B     INDXIT                                                           
         EJECT                                                                  
*              ROUTINE TO GET TOTAL DIRECT TIME                                 
         SPACE 1                                                                
         USING BUFD,R5                                                          
TOTDIR2  NTR1                                                                   
         ZAP   DIRLST,=P'0'                                                     
         ZAP   DIRYTD,=P'0'                                                     
         LA    R5,BUFWRK                                                        
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         ZIC   R0,LEVEL            DEPT / OFFICE /COMP                          
         LA    R2,BUFBYGP          BY GROUP                                     
         STC   R2,BUFTYPE                                                       
         MVI   BUFCLI,BUFTOT       TOTAL RECORD                                 
         GOTO1 BUFFALO,DMCB,=C'GET',((R2),ADBUFC),(R5),(R0)                     
         CLI   DMCB+8,0                                                         
         BNE   TOTDIR2X            NO DIRECT TIME                               
         ZAP   DIRLST,BDIRLST                                                   
         ZAP   DIRYTD,BDIRYTD                                                   
TOTDIR2X B     INDXIT                                                           
         EJECT                                                                  
INDDIV   DS    0H                                                               
         ZAP   DIVWRK,CLITIME     THIS CLIENT TIME                              
         MP    DIVWRK,ALLOC+2(6)   AMOUNT TO BE ALLOCATED                       
         MP    DIVWRK,=P'10'                                                    
         CP    TOTIME,=P'0'                                                     
         BNE   *+12                                                             
         ZAP   DIVWRK(8),=P'0'                                                  
         BR    RE                                                               
         DP    DIVWRK,TOTIME+2(6)  TOTAL CLIENT TIME                            
         AP    DIVWRK(10),=P'5'                                                 
         CP    DIVWRK(10),=P'0'                                                 
         BNL   *+10                                                             
         SP    DIVWRK(10),=P'10'                                                
         DP    DIVWRK(10),=P'10'   ALLOCATED AMOUNT DIVWRK 8 BYTES              
         BR    RE                                                               
         SPACE 1                                                                
         EJECT                                                                  
*              ROUTINE TO ADD PERCENT TO SALARY                                 
         SPACE 1                                                                
         USING PTOTD,R6                                                         
ADDPCT2  NTR1                                                                   
         LA    R6,PERWRK                                                        
         L     R2,ADACC                                                         
*MN      GOTO1 ACSLRY,DMCB,(R2),STEND,SALAREA                                   
         GOTO1 ACSLRY,DMCB,(X'80',(R2)),STEND,SALAREA,ADCOMFAC                  
         LA    R2,SALAREA                                                       
         USING SLRD,R2                                                          
         ZAP   PCOST,SLRTOT        SALARY FOR PERIOD                            
         SPACE 1                                                                
         EDIT  SLRTOT,(10,SALVAL),2,MINUS=YES,ALIGN=LEFT                        
         ZAP   PCTTOT,=P'0'                                                     
         AP    PCTTOT,PCTBENEF     BENEFIT PERCENT                              
         AP    PCTTOT,PCTADMIN     PLUS ADMIN. PERCENT                          
         CP    PCTTOT,=P'0'        TOTAL PERCENT                                
         BE    ADDPCT2X                                                         
         SPACE 1                                                                
         MVC   SALPCT(9),=C'+      PC'                                          
         EDIT  PCTTOT,(6,SALPCT+1),3                                            
         ZAP   DIVWRK,PCTBENEF     GET BENEFIT AMOUNT                           
         MP    DIVWRK,SLRTOT                                                    
         DP    DIVWRK,=P'100000'                                                
         ZAP   PBENEF,DIVWRK(12)                                                
         AP    PCOST,PBENEF        ADD TO TOTAL                                 
         ZAP   DIVWRK,PCTADMIN     GET ADMIN. AMOUNT                            
         MP    DIVWRK,SLRTOT                                                    
         DP    DIVWRK,=P'100000'                                                
         ZAP   PADMIN,DIVWRK(12)                                                
         AP    PCOST,PADMIN        ADD TO TOTAL                                 
         SPACE 1                                                                
ADDPCT2X B     INDXIT                                                           
*              ROUTINE TO ADD BENEBFIT ELEMENT                                  
         SPACE 1                                                                
         USING TRCASHD,R4                                                       
         USING PTOTD,R6                                                         
         USING CLID,R7                                                          
BENEL2   NTR1                                                                   
         LA    R6,PERWRK                                                        
         ZIC   R3,1(R4)            R4 WAS AT TRANSACTION                        
         AR    R4,R3                                                            
         XC    0(25,R4),0(R4)                                                   
         MVC   TRCSEL(2),=X'500F'                                               
         MVI   TRCSTYPE,C'B'                                                    
         ZAP   CLITIME,CCSTPST       THIS CLIENT POSTING AMOUNT                 
         ZAP   TOTIME,PCOST          TOTAL COST                                 
         ZAP   ALLOC,PBENEF          BENEFIT TO BE ALLOCATED                    
         BAS   RE,DIV                                                           
         ZAP   TRCSAMNT,DIVWRK(8)    THIS CLIENT SHARE OF BENEFIT               
         SPACE 1                                                                
         ZAP   CLITIME,CCSTPST       THIS CLIENT POSTING AMOUNT                 
         ZAP   TOTIME,PCOST          TOTAL COST                                 
         ZAP   ALLOC,PADMIN          ADMIN. TO BE ALLOCATED                     
         BAS   RE,DIV                                                           
         ZAP   TRCSADMN,DIVWRK(8)    THIS CLIENT SHARE OF ADMIN                 
         CP    TRCSAMNT,=P'0'                                                   
         BNE   INDXIT                                                           
         CP    TRCSADMN,=P'0'                                                   
         BNE   INDXIT                                                           
         XC    TRCSEL(25),TRCSEL   DON'T ADD ZERO ELEMENTS                      
         B     INDXIT                                                           
ADDTONET NTR1                                                                   
         LA    R2,SALAREA                                                       
         ZAP   SLRTOT,NET(12)      NET AMOUNT OF SALARY                         
         LA    R6,PERWRK                                                        
         ZAP   PCOST,NET(12)                                                    
         B     ADDPCT2X                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO GET OVERHEAD                                          
ACA2GOVH DS    0D                                                               
         NMOD1 0,*GOVH*                                                         
         L     RC,0(R1)                                                         
         L     R2,ADACC                                                         
         MVC   WORK(4),STEND                                                    
         CLI   OPTOVHY,C'Y'                                                     
         BNE   *+10                                                             
         MVC   WORK(2),FISCAL    IF O'HEAD IS YTD USE FISCAL START              
*MN      GOTO1 ACSLRY,DMCB,(R2),WORK,SALAREA                                    
         GOTO1 ACSLRY,DMCB,(X'80',(R2)),WORK,SALAREA,ADCOMFAC                   
         SPACE 1                                                                
         USING SLRD,R2                                                          
         LA    R2,SALAREA                                                       
         ZAP   OVAYTD,SLRSAL                                                    
         TM    SLRSALST,X'20'      OVERHEAD IS PCT NOT AMOUNT                   
         BZ    GETOVH1                                                          
         ZAP   DIVWRK,SLRSAL     THIS MONTHS OVERHEAD PCT                       
         MP    DIVWRK,DUB4         TIMES YTD DIRECT                             
         DP    DIVWRK,=P'10000'                                                 
         ZAP   OVAYTD,DIVWRK(13)   GIVES YTD O'HEAD                             
         SPACE 1                                                                
GETOVH1  ZAP   OVAPST,OVAYTD                                                    
         SP    OVAPST,OVALST       GET POSTING FOR ACCOUNT                      
         ZAP   OVACC,OVAPST        SAVE ACCOUNT POSTING FOR HEADLINE            
         USING SUMD,R8                                                          
GETOVHD  CLI   LEVEL,DEPT                                                       
         BNE   GETOVHO                                                          
         CLI   OPTOHC,C'Y'         DOES CORP GET SHARE OF DEPT OVH              
         BNE   GETOVHX                                                          
         BAS   RE,CORPOVH          GIVE CORP ITS SHARE                          
         B     GETOVHX                                                          
         SPACE 1                                                                
GETOVHO  CLI   LEVEL,OFFICE                                                     
         BNE   GETOVHC                                                          
         USING DEPD,R7                                                          
         LA    R7,OFFCUM                                                        
         AP    OVOLST,DOVHOL       ADD DEPT. TO OFFICE OVERHEAD                 
         AP    OVOYTD,DOVHOY                                                    
         ZAP   DOVHOL,=P'0'        NOW CLEAR SO I DON'T  GET IT AGAIN           
         ZAP   DOVHOY,=P'0'                                                     
         CLI   OPTEXH,0                                                         
         BE    GETOVHX             NO ACCOUNT CODE FOR EXEC TIME                
         L     R2,ADACC                                                         
         LA    R2,14(R2)                                                        
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8              FIND END OF OVERHEAD ACCOUNT                 
         CLC   OPTEXH,0(R2)                                                     
         BNE   GETOVHX             NOT ACCOUNT FOR EXEC INDIRECT                
         AP    OVOLST,DEXTIML      ADD DEPT. EXEC TIME TO OVERHEAD              
         AP    OVOYTD,DEXTIMY                                                   
         ZAP   DEXTIML,=P'0'        NOW CLEAR SO I DON'T  GET IT AGAIN          
         ZAP   DEXTIMY,=P'0'                                                    
         B     GETOVHX                                                          
         SPACE 1                                                                
GETOVHC  CLI   LEVEL,COMP                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DEPD,R7                                                          
         LA    R7,AGYCUM                                                        
         AP    OVOLST,DOVHOL       ADD ANY UNABSORBED DEPT                      
         AP    OVOYTD,DOVHOY                                                    
         ZAP   DOVHOL,=P'0'                                                     
         ZAP   DOVHOY,=P'0'                                                     
         AP    OVOLST,DOVHCL       PLUS ANY DEPT. THAT WENT STRAIGHT            
         AP    OVOYTD,DOVHCY       TO CORP                                      
         ZAP   DOVHCL,=P'0'                                                     
         ZAP   DOVHCY,=P'0'                                                     
         USING OFFD,R7                                                          
         AP    OVOLST,OFOVHCL      PLUS ANY OFFICE OVERHEAD                     
         AP    OVOYTD,OFOVHCY                                                   
         ZAP   OFOVHCL,=P'0'                                                    
         ZAP   OFOVHCY,=P'0'                                                    
         SPACE 1                                                                
GETOVHX  ZAP   OVOPST,OVOYTD                                                    
         SP    OVOPST,OVOLST       GET POSTING FOR OTHER                        
         AP    OVHLST,OVALST       ACCOUNT PLUS                                 
         AP    OVHLST,OVOLST       OTHER GIVES TOTAL                            
         AP    OVHYTD,OVAYTD                                                    
         AP    OVHYTD,OVOYTD                                                    
         AP    OVHPST,OVAPST                                                    
         AP    OVHPST,OVOPST                                                    
         ZAP   OVTHR,OVOPST        SAVE OTHER FOR HEADLINE                      
         B     GOXIT                                                            
         EJECT                                                                  
*              ROUTINE TO ALLOC. DEPT OVERHEAD TO CORP.                         
         SPACE 1                                                                
         USING SUMD,R8                                                          
CORPOVH  NTR1                                                                   
         CP    DUB3,=P'0'                                                       
         BNE   *+14                                                             
         CP    DUB4,=P'0'                                                       
         BE    GOXIT               NO DIRECT TIME                               
         USING ACSTATD,R2                                                       
         L     R2,ADLVBSTA                                                      
         TM    ACSTSTX,X'08'                                                    
         BO    GOXIT               OVERHEAD ACCOUNT IN INDIRECT DEPT            
         LA    R8,DEPTOT                                                        
         ZAP   CLITIME,SCINDLST       THIS DEPT'S CORP INDIRECT                 
         ZAP   ALLOC,OVALST           DEPT OVERHEAD YTD-1                       
         ZAP   TOTIME,SCINDLST        DEPT CORP                                 
         LA    R5,BUFWRK                                                        
         AP    TOTIME,DUB3            PLUS CLIENT TIME = TOTLINE                
         BAS   RE,GODIV               AMOUNT OF OVHEAD TO CORP                  
         ZAP   INDLST,DIVWRK(8)       USING INDIRECT AREA FOR WORK              
         SPACE 1                                                                
         ZAP   CLITIME,SCINDYTD       THIS DEPT'S CORP INDIRECT                 
         ZAP   ALLOC,OVAYTD           DEPT OVERHEAD YTD                         
         ZAP   TOTIME,SCINDYTD        DEPT CORP                                 
         AP    TOTIME,DUB4            PLUS CLIENT TIME = TOTLINE                
         BAS   RE,GODIV               AMOUNT OF OVH. TO CORP                    
         ZAP   INDYTD,DIVWRK(8)                                                 
         ZAP   INDPST,INDYTD                                                    
         SP    INDPST,INDLST                                                    
         AP    SOVOLST,INDLST         MOVE IT TO OTHER COLUMN                   
         AP    SOVOYTD,INDYTD                                                   
         AP    SOVOPST,INDPST                                                   
         SPACE 1                                                                
         SP    OVALST,INDLST        TAKE IT OUT OF DEPT                         
         SP    OVAYTD,INDYTD                                                    
         USING DEPD,R7                                                          
         LA    R7,DEPCUM                                                        
         AP    DOVHCL,INDLST       ADD IT TO CORP                               
         AP    DOVHCY,INDYTD                                                    
         USING PSHEADD,R2          BUILD OVERHEAD ACCOUNT                       
         LA    R2,T                CONTRA IS 1NC CORP                           
         L     RF,ADACC            OR 1NO OFFICE                                
         MVC   PSHDACC,0(RF)                                                    
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVC   PSHDSBAC(1),PSHDACC                COMPANY                       
         MVC   PSHDSBAC+1(14),=CL14'1NC CORP'    UNIT/LEDGER/ACCOUNT            
         MVC   PSHDSBNM,SPACES                                                  
         MVC   PSHDSBNM(13),=C'CORP OVERHEAD'                                   
         MVI   TRNSSTAT,X'80'                                                   
         ZAP   TRNSAMNT,INDPST                                                  
         GOTO1 POSTIT,DMCB,(RC)                                                 
         B     GOXIT                                                            
         EJECT                                                                  
GODIV    DS    0H                                                               
         ZAP   DIVWRK,CLITIME     THIS CLIENT TIME                              
         MP    DIVWRK,ALLOC+2(6)   AMOUNT TO BE ALLOCATED                       
         MP    DIVWRK,=P'10'                                                    
         CP    TOTIME,=P'0'                                                     
         BNE   *+12                                                             
         ZAP   DIVWRK(8),=P'0'                                                  
         BR    RE                                                               
         DP    DIVWRK,TOTIME+2(6)  TOTAL CLIENT TIME                            
         AP    DIVWRK(10),=P'5'                                                 
         CP    DIVWRK(10),=P'0'                                                 
         BNL   *+10                                                             
         SP    DIVWRK(10),=P'10'                                                
         DP    DIVWRK(10),=P'10'   ALLOCATED AMOUNT DIVWRK 8 BYTES              
         BR    RE                                                               
         SPACE 1                                                                
GOXIT    XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DISTRIBUTE OVERHEAD                                   
ACA2DOVH DS    0D                                                               
         NMOD1 0,*DISOVH*                                                       
         L     RC,0(R1)                                                         
         CP    DUB3,=P'0'                                                       
         BNE   DISOVH2                                                          
         CP    DUB4,=P'0'                                                       
         BNE   DISOVH2                                                          
         BAS   RE,OVUP        NO DIRECT TIME CARRY OVERHEAD TO HIGH             
         B     OVHXIT                                                           
         SPACE 1                                                                
DISOVH2  ZAP   DUB1,OVHLST         TOTAL OVERHEADS                              
         ZAP   DUB2,OVHYTD                                                      
         ZAP   DUB5,OVALST         ACCOUNT OVERHEADS                            
         ZAP   DUB6,OVAYTD                                                      
         MVC   TOTLINE(80),=10PL8'0'                                            
         LA    R5,BUFWRK                                                        
         USING BUFD,R5                                                          
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         SPACE 1                                                                
DISOVH3  LA    R7,CLIWRK           INITIALIZE CLIENT RECORD                     
         USING CLID,R7                                                          
         LA    R2,CLIBK                                                         
         LA    R3,CBKCNT                                                        
         ZAP   0(8,R2),=P'0'       CLEAR CLIENT BUCKETS                         
         LA    R2,8(R2)                                                         
         BCT   R3,*-10                                                          
         SPACE 1                                                                
         ZIC   R0,LEVEL                                                         
         LA    R2,BUFNOGP          O/H NOT ALLOCATED BY GROUP                   
         STC   R2,BUFTYPE                                                       
         GOTO1 BUFFALO,DMCB,=C'SEQ',((R2),ADBUFC),(R5),(R0)                     
         TM    DMCB+8,X'80'                                                     
         BO    DISOVH7             EOF                                          
         MVC   CLILEDG(13),BUFCLI                                               
         ZAP   CDIRLST,BDIRLST     DIRECT TIME TO CLIENT TABLE                  
         ZAP   CDIRYTD,BDIRYTD                                                  
         CLI   OPTOVHY,C'Y'         IS OVERHEAD YTD                             
         BNE   *+16                                                             
         AP    CDIRLST,BDIRLSTM    IF IT IS ADD MEMO DIRECT YTD-1               
         AP    CDIRYTD,BDIRLSTM                                                 
         CLC   ALPHAID,=C'BS'      FOR BACKER                                   
         BNE   DISOVH4                                                          
         CLI   LEVEL,DEPT          CHECK PROF 1 AT OFF AND COMP LEVEL           
         BE    DISOVH5                                                          
         B     *+12                                                             
DISOVH4  CLI   LEVEL,COMP                                                       
         BNE   DISOVH5                                                          
         CLI   OPTEXP,C'I'                                                      
         BE    *+12                                                             
         CLI   OPTEXP,C'Y'                                                      
         BNE   DISOVH5                                                          
         AP    CDIRLST,BEXPLST     ADD EXPENSES/INDIRECT/OVH TO DIRECT          
         AP    CDIRYTD,BEXPYTD     EXP. ONLY PRESENT AT COMP.LEVEL              
DISOVH5  CP    CDIRLST,=P'0'                                                    
         BNE   DISOVH6                                                          
         CP    CDIRYTD,=P'0'                                                    
         BE    DISOVH3                                                          
         SPACE 1                                                                
DISOVH6  ZAP   CLITIME,CDIRYTD     CLIENT TIME YTD                              
         ZAP   ALLOC,DUB2          TOTAL OVERHEAD YTD                           
         ZAP   TOTIME,DUB4         AGYTIME YTD                                  
         BAS   RE,DODIV            GET AMOUNT OF O/H FOR THIS CLIENT            
         ZAP   CCSTYTD,DIVWRK(8)   YTD OVERHEAD TO CLIENT TABLE                 
         SPACE 1                                                                
         ZAP   CLITIME,CDIRLST     CLIENT TIME YTD-1                            
         ZAP   ALLOC,OVHLST        TOTAL OVERHEAD YTD-1                         
         SP    ALLOC,OVALST        LESS ACCOUNT OVERHEAD YTD-1                  
*                                  GIVES OTHER OVERHEAD YTD-1                   
         ZAP   TOTIME,DUB3         AGYTIME YTD-1                                
         BAS   RE,DODIV            GET AMOUNT OF OTHER FOR YTD-1                
         ZAP   CCSTLST,DIVWRK(8)   THIS WILL ADD IT TO ACCT POSTINGS            
         GOTO1 BINADD,DMCB,(R7),CLIBUFF,(RC) ADD TO CLIENT TABLE                
         B     DISOVH3                                                          
         SPACE 1                                                                
         USING BIND,R5                                                          
DISOVH7  L     R5,CLIBUFF                                                       
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BZ    OVHXIT              NO CLIENTS IN TABLE                          
         LA    R7,BINTABLE                                                      
DISOVH8  ZAP   OVHLST,CCSTLST      CLIENT YTD-1                                 
         ZAP   OVHYTD,CCSTYTD      CLIENT YTD                                   
         ZAP   OVHPST,CCSTYTD                                                   
         SP    OVHPST,CCSTLST      GET POSTING AMOUNT                           
         SPACE 1                                                                
         ZAP   CLITIME,CDIRLST     CLIENT TIME YTD-1                            
         ZAP   ALLOC,DUB5          ACCOUNT OVERHEAD YTD-1                       
         ZAP   TOTIME,DUB3         AGYTIME YTD-1                                
         BAS   RE,DODIV            GET AMOUNT OF O/H FOR THIS CLIENT            
         ZAP   OVALST,DIVWRK(8)    ACCOUNT  YTD-1                               
         SPACE 1                                                                
         ZAP   CLITIME,CDIRYTD     CLIENT TIME YTD                              
         ZAP   ALLOC,DUB6          ACCOUNT OVERHEAD YTD                         
         ZAP   TOTIME,DUB4         AGYTIME YTD                                  
         BAS   RE,DODIV            GET AMOUNT OF O/H FOR THIS CLIENT            
         ZAP   OVAYTD,DIVWRK(8)    ACCOUNT OVERHEAD YTD                         
         ZAP   OVAPST,OVAYTD                                                    
         SP    OVAPST,OVALST       GET POSTING AMOUNT                           
         SPACE 1                                                                
         ZAP   OVOLST,OVHLST       TOTAL LESS ACCOUNT GIVES OTHER               
         SP    OVOLST,OVALST                                                    
         ZAP   OVOYTD,OVHYTD                                                    
         SP    OVOYTD,OVAYTD                                                    
         ZAP   OVOPST,OVHPST                                                    
         SP    OVOPST,OVAPST                                                    
         SPACE 1                                                                
         USING PSHEADD,R2                                                       
         LA    R2,T                NOW POST-CREDIT CLIENT,DEBIT O'HEAD          
         L     RF,ADUNIT                                                        
         MVC   PSHDACC(2),0(RF)                                                 
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVC   PSHDACC+2(13),CLILEDG                                            
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC(2),PSHDACC                                              
         MVC   PSHDSBAC+2(1),OPT54 CONTRA LEDGER IS 5 OR 4                      
         CLI   LEVEL,COMP                                                       
         BNE   *+10                                                             
         MVC   PSHDSBAC+2(1),OPTC54                                             
         MVC   PSHDSBAC+3(1),GROUP O'HEAD A/C HAS ITS OWN POINTER               
         CLI   LEVEL,OFFICE                                                     
         BNE   DISOVH8A                                                         
         CLI   OPTOFH,0                                                         
         BE    DISOVH8A                                                         
         MVC   PSHDSBAC+2(1),OPTOFH   SPECIAL LEDGER FOR OFFICE O/H             
DISOVH8A MVC   SVGP,PSHDSBAC+2     LEDGER/ACCOUNT                               
         GOTO1 GRPMTCH,DMCB,(RC)                                                
         MVC   PSHDSBNM,WORK                                                    
         MVI   TRNSSTAT,0                                                       
         ZAP   TRNSAMNT,OVHPST                                                  
         SPACE 1                                                                
         L     RF,ADACC                                                         
         MVC   OFFCODE,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFCODE(0),3(RF)                                                 
         MVC   TRNSANAL,OFFCODE                                                 
         CLC   CLILEDG(7),=C'NC CORP' REVERSE PREVIOUS OFFICE OVERHEAD          
         BNE   DISOVH8B                                                         
         ZAP   OVAPST,OVHPST                                                    
         ZAP   OVALST,OVHLST                                                    
         ZAP   OVAYTD,OVHYTD                                                    
         BAS   RE,OVUP             TAKE IT OUT OF CORP                          
         B     DISOVH8C                                                         
DISOVH8B GOTO1 POSTIT,DMCB,(RC)                                                 
         L     RF,ADACC                                                         
         MVC   PSHDSBAC,PSHDACC    NOW DEBIT OVERHEAD                           
         MVC   PSHDACC,0(RF)                                                    
         SPACE 1                                                                
         USING BCLID,R8                                                         
         LA    R8,NMEWRK           GET CLIENT NAME FROM NMEBUFF                 
         MVC   BCLICDE,CLICDE                                                   
         GOTO1 NMEGET,DMCB,(RC)                                                 
         MVC   PSHDSBNM,BCLINME                                                 
         MVI   TRNSSTAT,X'80'                                                   
         ZAP   TRNSAMNT,OVAPST                                                  
         GOTO1 POSTIT,DMCB,(RC)                                                 
         GOTO1 REPORT,DMCB,(1,(RC)),0      PRINT ALLOCATION TO CLIENT           
         BAS   RE,ABOVH                                                         
DISOVH8C LA    R7,CLEN(R7)                                                      
         BCT   R3,DISOVH8                                                       
         SPACE 1                                                                
DISOVH9  GOTO1 REPORT,DMCB,(1,(RC)),(C'T',0)                                    
         B     OVHXIT                                                           
         EJECT                                                                  
*              ROUTINE TO PUT OVERHEAD TO ABSORBED COLUMN                       
         SPACE 1                                                                
         USING SUMD,R8                                                          
ABOVH    NTR1                                                                   
         CLI   LEVEL,DEPT                                                       
         BNE   ABOVH3              END OF DEPT                                  
         LA    R8,DEPTOT                                                        
         AP    SOVHLST,OVHLST                                                   
         AP    SOVHYTD,OVHYTD                                                   
         AP    SOVHPST,OVHPST      ADD TO ABSORBED                              
         LR    R3,R7               SAVE ADDRESS OF CLID RECORD                  
         USING DEPD,R7                                                          
         LA    R7,DEPCUM                                                        
         AP    DOVHAL,OVHLST                                                    
         AP    DOVHAY,OVHYTD                                                    
         AP    DOVHAP,OVHPST                                                    
         CLI   OPTEXP,C'Y'                                                      
         BNE   OVHXIT                                                           
         LA    R6,CLSWRK                                                        
         USING CLSD,R6                                                          
         XC    CLSWRK,CLSWRK       ADD OVERHEAD BY CLIENT                       
         LR    R7,R3               ADDRESS OF CLID RECORD                       
         USING CLID,R7                                                          
         MVC   CLSCDE,CLILEDG                                                   
         ZAP   DUB,OVHLST                                                       
         CVB   R1,DUB                                                           
         ST    R1,CLSOVLST                                                      
         ZAP   DUB,OVHYTD                                                       
         CVB   R1,DUB                                                           
         ST    R1,CLSOVYTD                                                      
         GOTO1 BINADD,DMCB,(R6),CLSBUFF,(RC)                                    
         B     OVHXIT                                                           
         SPACE 1                                                                
ABOVH3   CLI   LEVEL,OFFICE                                                     
         BNE   ABOVH4                                                           
         LA    R8,DEPTOT                                                        
         AP    SOVHLST,OVHLST    OVERHEAD ABSORBED                              
         AP    SOVHYTD,OVHYTD                                                   
         AP    SOVHPST,OVHPST                                                   
         SP    SOVOLST,OVOLST    TAKE OTHER OUT AT OTHER COLUMN                 
         SP    SOVOYTD,OVOYTD                                                   
         SP    SOVOPST,OVOPST                                                   
         USING OFFD,R7                                                          
         LA    R7,OFFCUM                                                        
         AP    OFOVHAL,OVHLST                                                   
         AP    OFOVHAY,OVHYTD                                                   
         AP    OFOVHAP,OVHPST                                                   
         B     OVHXIT                                                           
         SPACE 1                                                                
ABOVH4   CLI   LEVEL,COMP                                                       
         BE    ABOVH4A                                                          
         MVI   POSTERR,C'Y'                                                     
         GOTO1 REPORT,DMCB,(2,(RC)),(3,0)                                       
         B     OVHXIT                                                           
ABOVH4A  LA    R8,DEPTOT                                                        
         AP    SOVHLST,OVHLST    OVERHEAD ABSORBED                              
         AP    SOVHYTD,OVHYTD                                                   
         AP    SOVHPST,OVHPST                                                   
         SP    SOVOLST,OVOLST    OUT AT OTHER COLUMN                            
         SP    SOVOYTD,OVOYTD                                                   
         SP    SOVOPST,OVOPST                                                   
         USING AGYD,R7                                                          
         LA    R7,AGYCUM                                                        
         AP    AGOVHAL,OVHLST                                                   
         AP    AGOVHAY,OVHYTD                                                   
         AP    AGOVHAP,OVHPST                                                   
         B     OVHXIT                                                           
         EJECT                                                                  
*              ROUTINE TO ADD UNABSORBED OVERHEAD TO HIGHER LEVEL               
OVUP     NTR1                                                                   
         USING PSHEADD,R2          BUILD OVERHEAD ACCOUNT                       
         LA    R2,T                CONTRA IS 1NC CORP                           
         L     RF,ADACC            OR 1NO OFFICE                                
         MVC   PSHDACC,0(RF)                                                    
         ZIC   R4,PSHDLEN                                                       
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVC   PSHDSBAC(1),PSHDACC                COMPANY                       
         MVI   TRNSSTAT,X'80'                                                   
         ZAP   TRNSAMNT,OVAPST     POST ACCOUNT ACCOUNT                         
         CLI   LEVEL,DEPT                                                       
         BE    OVUP3                                                            
         CLI   LEVEL,OFFICE                                                     
         BE    OVUP5                                                            
         GOTO1 REPORT,DMCB,(1,(RC)),(3,0)  ERROR NO DIRECT TIME COST            
         MVI   POSTERR,C'Y'                                                     
         B     OVHXIT                                                           
         SPACE 1                                                                
OVUP3    CLI   OPTOVH,C'C'         OPTION TP POST TO CORP                       
         BE    OVUP5                                                            
         MVC   PSHDSBAC+1(14),=CL14'1NO OFFICE'                                 
         MVC   PSHDSBNM,SPACES                                                  
         MVC   PSHDSBNM(15),=C'OFFICE OVERHEAD'                                 
         USING DEPD,R7                                                          
         LA    R7,DEPCUM                                                        
         AP    DOVHOL,OVHLST       ADD ALL TO OFFICE OVERHEAD                   
         AP    DOVHOY,OVHYTD                                                    
         B     OVUP7                                                            
         SPACE 1                                                                
OVUP5    MVC   PSHDSBAC+1(14),=CL14'1NC CORP'                                   
         MVC   PSHDSBNM,SPACES                                                  
         MVC   PSHDSBNM(13),=C'CORP OVERHEAD'                                   
         USING OFFD,R7                                                          
         LA    R7,OFFCUM                                                        
         AP    OFOVHCL,OVHLST      ADD ALL TO CORP. OVERHEAD                    
         AP    OFOVHCY,OVHYTD                                                   
         SPACE 1                                                                
         USING SUMD,R8                                                          
OVUP7    GOTO1 POSTIT,DMCB,(RC)                                                 
         GOTO1 REPORT,DMCB,(1,(RC)),(2,0)                                       
         LA    R8,DEPTOT                                                        
         AP    SOVOLST,OVALST      ONLY ACCOUNT TOTALS TO SUMMARY               
         AP    SOVOYTD,OVAYTD                                                   
         AP    SOVOPST,OVAPST                                                   
         SPACE 1                                                                
OVHXIT   XMOD1 1                                                                
         EJECT                                                                  
DODIV    DS    0H                                                               
         ZAP   DIVWRK,CLITIME     THIS CLIENT TIME                              
         MP    DIVWRK,ALLOC+2(6)   AMOUNT TO BE ALLOCATED                       
         MP    DIVWRK,=P'10'                                                    
         CP    TOTIME,=P'0'                                                     
         BNE   *+12                                                             
         ZAP   DIVWRK(8),=P'0'                                                  
         BR    RE                                                               
         DP    DIVWRK,TOTIME+2(6)  TOTAL CLIENT TIME                            
         AP    DIVWRK(10),=P'5'                                                 
         CP    DIVWRK(10),=P'0'                                                 
         BNL   *+10                                                             
         SP    DIVWRK(10),=P'10'                                                
         DP    DIVWRK(10),=P'10'   ALLOCATED AMOUNT DIVWRK 8 BYTES              
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ADD DEPT OVERHEAD AND INDIRECT TO BUFFALO '                      
ACA2BUFO DS    0D                                                               
         NMOD1 0,*INDOVH*                                                       
         L     RC,0(R1)                                                         
         L     R5,CLSBUFF                                                       
         USING BIND,R5                                                          
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BZ    INDOVHX                                                          
         LA    R6,BINTABLE                                                      
         USING CLSD,R6                                                          
         LA    R5,BUFWRK                                                        
         USING BUFD,R5                                                          
         LA    R1,BUFBK                                                         
         LA    R2,BBKCNT                                                        
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R2,*-10                                                          
         SPACE 1                                                                
INDOVH1  MVI   BUFTYPE,BUFBYGP     ADD INDIRECT BY GROUP                        
         MVI   BUFGRP,BUFIND                                                    
         MVC   BUFCLI,CLSCDE       CLIENT CODE                                  
         L     R4,CLSINLST         CLIENT DEPT INDIRECT YTD-1                   
         CVD   R4,DUB                                                           
         ZAP   BINDLST,DUB                                                      
         L     R4,CLSINYTD                              YTD                     
         CVD   R4,DUB                                                           
         ZAP   BINDYTD,DUB                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,(R5)                                 
         MVI   BUFTYPE,BUFNOGP                                                  
         MVI   BUFGRP,0            AND NOT BY GROUP                             
         BASR  RE,RF                                                            
         XC    BUFCLI,BUFCLI                                                    
         MVI   BUFCLI,BUFTOT       TOTAL NOT BY GROUP                           
         BASR  RE,RF                                                            
         MVI   BUFTYPE,BUFBYGP     TOTAL BY GROUP                               
         BASR  RE,RF                                                            
         CLI   OPTEXP,C'I'         OPTION TO ADD ONLY DEPT INDIRECT             
         BE    INDOVH4                                                          
         SPACE 1                                                                
         MVI   BUFTYPE,BUFBYGP     ADD OVERHEAD BY GROUP                        
         MVI   BUFGRP,BUFOVH                                                    
         MVC   BUFCLI,CLSCDE       CLIENT CODE                                  
         L     R4,CLSOVLST         CLIENT DEPT OVERHEAD YTD-1                   
         CVD   R4,DUB                                                           
         ZAP   BOVHLST,DUB                                                      
         L     R4,CLSOVYTD                              YTD                     
         CVD   R4,DUB                                                           
         ZAP   BOVHYTD,DUB                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,(R5)                                 
         MVI   BUFTYPE,BUFNOGP                                                  
         MVI   BUFGRP,0            AND NOT BY GROUP                             
         BASR  RE,RF                                                            
         XC    BUFCLI,BUFCLI                                                    
         MVI   BUFCLI,BUFTOT       TOTAL NOT BY GROUP                           
         BASR  RE,RF                                                            
         MVI   BUFTYPE,BUFBYGP     TOTAL BY GROUP                               
         BASR  RE,RF                                                            
         SPACE 1                                                                
INDOVH4  LA    R6,CLSLEN(R6)                                                    
         BCT   R3,INDOVH1                                                       
         SPACE 1                                                                
INDOVHX  L     R5,CLSBUFF                                                       
         USING BIND,R5                                                          
         XC    BININ,BININ                                                      
         XMOD1                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD CLIENT CODE/NAME TO BINSRCH TABLE                 
         SPACE 1                                                                
         USING BIND,R5                                                          
ACA2NMEP DS    0D                                                               
         NMOD1 0,*NMEPUT*                                                       
         L     RC,0(R1)                                                         
         L     R5,NMEBUFF                                                       
         MVC   DMCB+8(16),BININ                                                 
         LA    R8,NMEWRK                                                        
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R8)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         XMOD1                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO GET CLIENT NAME FROM NMEBUFF                          
         SPACE 1                                                                
         USING BIND,R5                                                          
         USING BCLID,R8                                                         
ACA2NMEG DS    0D                                                               
         NMOD1 0,*NMEGET*                                                       
         L     RC,0(R1)                                                         
         L     R5,NMEBUFF                                                       
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         LA    R8,NMEWRK                                                        
         MVC   BCLINME,SPACES                                                   
         MVC   BCLINME(7),=C'MISSING'                                           
         GOTO1 BINSRCH,DMCB,(R8),(R2)                                           
         CLI   DMCB,1                                                           
         BE    NMEGTX              NOT FOUND                                    
         L     R3,DMCB                                                          
         MVC   BCLINME,BCLINME-BCLICDE(R3)                                      
NMEGTX   XMOD1                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD TO A BINSRCH TABLE                                
*              PARAM1              A(RECORD TO BE ADDED)                        
*              PARAM2              A(BINSRCH PARAMS)                            
ACA2BINS DS    0D                                                               
         NMOD1 0,*BINADD*                                                       
         L     RC,8(R1)                                                         
         USING BIND,R5                                                          
         L     R5,4(R1)            BINSRCH PARAMETERS                           
         L     R3,0(R1)            A(RECORD)                                    
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R3)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         BE    BINXIT              NOT FOUND - ADDED                            
         L     R4,DMCB             A(RECORD FOUND)                              
         ZIC   R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         ZIC   R7,BINNUMB          NUMBER OF BUCKETS                            
         TM    BINSTAT,X'80'                                                    
         BO    BINBIN              DATA IS BINARY                               
         AP    0(8,R4),0(8,R3)     ADD NEW TO OLD                               
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R7,*-14                                                          
         B     BINXIT                                                           
         SPACE 1                                                                
BINBIN   L     RE,0(R3)                                                         
         L     RF,0(R4)                                                         
         AR    RF,RE                                                            
         ST    RF,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R7,BINBIN                                                        
BINXIT   XMOD1 1                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BUILD LIST OF DEPARTMENT GROUP NAMES                             
ACA2DEPG DS    0D                                                               
         NMOD1 0,*DEPGRP                                                        
         L     RC,0(R1)                                                         
         L     R5,GRPTAB                                                        
         MVI   0(R5),X'FF'                                                      
         USING ACKEYD,R4                                                        
         L     R4,RECORD                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'14'                                             
         MVC   SAVEKEY,ACKEYACC                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R4),(R4)                        
         CLC   SAVEKEY(3),ACKEYACC                                              
         BE    DEPGRP2                                                          
         DC    H'0'                NO 14 LEDGER                                 
         SPACE 1                                                                
DEPGRP2  CLC   ACKEYACC(1),SAVEKEY                                              
         BNE   DEPGRP10            END OF COMPANY                               
         CLC   ACKEYACC+1(2),=C'17'                                             
         BH    DEPGRP10            END OF INDIRECT ACCOUNTS                     
         CLC   ACKEYWRK(ACLENGTH-ACKEYWRK),SPACES                               
         BE    DEPGRP4             ACCOUNT RECORD                               
DEPGRP3  MVI   ACKEYWRK,X'FF'      FORCE FOR NEXT ACCOUNT                       
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R4),(R4)                        
         B     DEPGRP2                                                          
         SPACE 1                                                                
DEPGRP4  GOTO1 GETEL,DMCB,(RC),(X'20',(R4)),0                                   
         CLI   DMCB+12,0                                                        
         BNE   DEPGRP3             NO NAME ELEMENT - READ HIGH TO NEXT          
         MVC   0(38,R5),SPACES                                                  
         MVC   0(2,R5),ACKEYACC+2  LEDGER/GROUP CODE                            
         USING ACNAMED,R4                                                       
         L     R4,DMCB+12                                                       
         SPACE 1                                                                
         IC    R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R5),ACNMNAME                                                 
         SPACE 1                                                                
         LA    R5,38(R5)                                                        
         L     R4,RECORD                                                        
         B     DEPGRP3                                                          
         SPACE 1                                                                
DEPGRP10 MVI   0(R5),X'FF'         END OF TABLE                                 
         XMOD1 1                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*             READ 1C LEDGER FOR EXPENSES                                       
ACA2EXPN DS    0D                                                               
         NMOD1 0,*EXPEN*                                                        
         L     RC,0(R1)                                                         
         L     R4,RECORD                                                        
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'1C'                                             
         MVI   ACKEYACC+3,X'41'    SET FOR FIRST ACCOUNT                        
         MVC   SAVEKEY,ACKEYACC                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R4),(R4)                        
         CLC   SAVEKEY(3),ACKEYACC                                              
         BE    EXP2                                                             
         DC    H'0'                NO 1C ACCOUNTS                               
         SPACE 1                                                                
EXP2     CLC   SAVEKEY(3),ACKEYACC                                              
         BNE   EXP40               END OF LEDGER                                
         GOTO1 GETEL,DMCB,(RC),(X'20',(R4)),0                                   
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                NO 20 ELEMENT ON ACCOUNT                     
         SPACE 1                                                                
         USING BUFD,R5                                                          
         USING BCLID,R8                                                         
         LA    R5,BUFWRK                                                        
         LA    R8,NMEWRK                                                        
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         LA    R2,BUFBK            CLEAR BUFFALO BUCKETS                        
         LA    R3,BBKCNT                                                        
         ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,*-10                                                          
         MVC   BCLINME,SPACES                                                   
         USING ACNAMED,R4                                                       
         L     R4,DMCB+12                                                       
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   BCLINME(0),ACNMNAME    ACCOUNT NAME                              
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
         L     R4,RECORD                                                        
         MVC   BCLICDE,ACKEYACC+3  CLIENT CODE                                  
         MVC   BUFCLI,ACKEYACC+2   LEDGER AND ACCOUNT CODE                      
         MVC   ACKEYCON(1),RCCOMPFL                                             
         MVC   ACKEYCON+1(2),=C'13'     CLIENT DIRECT EXPENSES                  
         MVC   SAVEKEY,ACKEYACC        SAVE ACCOUNT                             
         SPACE 1                                                                
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R4),(R4)                        
EXP5     CLC   SAVEKEY(3),ACKEYACC                                              
         BNE   EXP40               END OF LEDGER                                
         CLC   SAVEKEY(15),ACKEYACC                                             
         BNE   EXP30               END OF ACCOUNT                               
         CLC   ACKEYCON+1(2),=C'13'                                             
         BNE   EXP30               END OF CONTRA - 13                           
         GOTO1 GETEL,DMCB,(RC),(X'45',(R4)),0                                   
         CLI   DMCB+12,0                                                        
         BNE   EXP15               NO HISTORY FOR CONTRA                        
         L     R4,DMCB+12                                                       
         USING TRHISTD,R4                                                       
EXP8     MVC   WORK(L'START),START                                              
         OC    FISCAL,FISCAL                                                    
         BZ    *+10                                                             
         MVC   WORK(L'FISCAL),FISCAL                                            
         CLC   TRHSYEAR(2),WORK                                                 
         BL    EXP9                                                             
         CLC   TRHSYEAR(2),END                                                  
         BH    EXP9                                                             
         B     EXP10                                                            
EXP9     ZIC   R3,1(R4)                                                         
         AR    R4,R3                                                            
         CLI   0(R4),0                                                          
         BE    EXP15                                                            
         CLI   0(R4),X'45'                                                      
         BE    EXP8                                                             
         B     EXP9                                                             
         SPACE 1                                                                
EXP10    AP    BEXPYTD,TRHSCR       YTD EXPENSE                                 
         CLC   TRHSYEAR(2),LAST                                                 
         BH    EXP9                                                             
         AP    BEXPLST,TRHSCR       YTD-1 EXPENSE                               
         B     EXP9                                                             
         SPACE 1                                                                
EXP15    L     R4,RECORD           READ FOR NEXT CONTRA                         
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCOUNT',(R4),(R4)                        
         B     EXP5                                                             
         SPACE 1                                                                
EXP30    CP    BEXPLST,=P'0'       END OF ACCOUNT - CHECK BUCKETS               
         BNE   *+14                                                             
         CP    BEXPYTD,=P'0'                                                    
         BE    EXP33               NO ACTIVITY                                  
         MVI   BUFTYPE,BUFBYGP     BY CLIENT GROUP                              
         MVI   BUFGRP,BUFEXPN                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,(R5)                                 
         MVI   BUFTYPE,BUFNOGP     AND NOT BY GROUP                             
         MVI   BUFGRP,0                                                         
         BASR  RE,RF                                                            
         XC    BUFCLI,BUFCLI                                                    
         MVI   BUFCLI,BUFTOT      TOTAL RECORD NOT BY GROUP                     
         BASR  RE,RF                                                            
         MVI   BUFTYPE,BUFBYGP                                                  
         BASR  RE,RF               TOTAL BY GROUP                               
         GOTO1 NMEPUT,DMCB,(RC)    ADD CLIENT CODE/NAME TO TABLE                
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
EXP33    L     R4,RECORD           READ HIGH FOR NEXT ACCOUNT                   
         MVC   ACKEYACC,SAVEKEY    RESTORE LAST ACCOUNT KEY                     
         MVC   ACKEYWRK(ACLENGTH-ACKEYWRK),SPACES                               
         MVI   ACKEYWRK,X'FF'      SET FOR READ HIGH TO NEXT                    
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R4),(R4)                        
         B     EXP2                                                             
         SPACE 1                                                                
EXP40    GOTO1 BUFFALO,DMCB,=C'ADD',ADBUFC,1,2,(X'80',3)   ADD 1 T0 3           
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,1,(X'80',2)   CLEAR 1-2            
         XMOD1 1                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              FIND A NAME FOR A DEPARTMENT GROUP                               
         SPACE 1                                                                
         ENTRY ACA2MTCH                                                         
ACA2MTCH DS    0D                                                               
         NMOD1 0,*GRPM*                                                         
         L     RC,0(R1)                                                         
         MVC   WORK,SPACES                                                      
         L     R5,GRPTAB                                                        
         MVC   WORK(7),=C'MISSING'                                              
GRP2     CLI   0(R5),X'FF'                                                      
         BE    CGXIT                                                            
         CLC   0(2,R5),SVGP                                                     
         BE    GRP4                                                             
         LA    R5,38(R5)                                                        
         B     GRP2                                                             
GRP4     MVC   WORK(36),2(R5)                                                   
CGXIT    XMOD1 1                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BUILD HEADLINES AND PRINT DATA                                   
         SPACE 1                                                                
*              PARAM1              BYTE 0 = RCSUBPRG                            
*                                  BYTE 1-3 = (RC)                              
*              PARAM2              BYTE 0 = SPECIAL MODE , C'T' = TOTAL         
*                                  BYTE 1-3 NOTUSED                             
         SPACE 1                                                                
ACA2REPT DS    0D                                                               
         NMOD1 0,**REPRT*                                                       
         L     RC,0(R1)                                                         
         USING CLID,R7                                                          
         MVC   RCSUBPRG,0(R1)                                                   
         MVC   MYMODE,4(R1)                                                     
         CLI   RCSUBPRG,0                                                       
         BNE   REOVH               NOT APERSON MIGHT BR OVERHEAD                
         CLI   QOPT2,C'S'                                                       
         BE    REPORTX             NO PERSON PAGES                              
         CLI   MYMODE,0                                                         
         BNE   REPER2                                                           
         LA    R3,CBKCNT           SET UP PRINT LINE FOR PERSON                 
         LA    R4,CLIBK                                                         
         LA    R8,TOTLINE                                                       
         BAS   RE,ADDTOT           ADD CLIENT TOTALS TO TOTLINE                 
         MVC   P+1(48),CLICDE      CLIENT CODE AND NAME TO PRINT                
         GOTO1 SQUASHER,DMCB,P+1,48                                             
         LA    R2,P+53                                                          
         LA    R8,CPERHRS                                                       
         LA    R3,3                                                             
         BAS   RE,FRMTH            FORMAT HOURS                                 
         LA    R2,P+77                                                          
         LA    R8,CCSTLST                                                       
         LA    R3,3                                                             
         BAS   RE,FRMTD            FORMAT DOLLARS                               
         B     REPORTP                                                          
         SPACE 1                                                                
REPER2   CLI   MYMODE,C'T'                                                      
         BNE   REPER4                                                           
         GOTO1 HEADUP,DMCB,(RC)     SET-UP HEADLINES                            
         GOTO1 ACREPORT            SKIP LINE BEFORE TOTAL                       
         MVC   P+1(7),=C'*TOTAL*'                                               
         LA    R2,P+53                                                          
         LA    R8,TOTLINE                                                       
         LA    R3,3                                                             
         BAS   RE,FRMTH            FORMAT HOURS                                 
         LA    R2,P+77                                                          
         LA    R8,TOTLINE+24                                                    
         LA    R3,3                                                             
         BAS   RE,FRMTD            FORMAT DOLLARS                               
         CLI   OPTSKIP,C'Y'                                                     
         BE    REPER3                                                           
         GOTO1 ACREPORT            SKIP LINE AFTER TOTAL                        
REPER3   EQU   *                                                                
         B     REPORTP                                                          
         SPACE 1                                                                
REPER4   CLI   MYMODE,1                                                         
         BNE   REPER5                                                           
         MVC   P+1(30),=CL30'DEPT. INDIRECT TO CORP. INDIR'                     
         CLC   ALPHAID,=C'BS'         FOR BACKER USE OFFICE                     
         BNE   *+10                                                             
         MVC   P+1(30),=CL30'DEPT. INDIRECT TO OFFICE INDIR'                    
         USING PTOTD,R6                                                         
         LA    R6,PERWRK                                                        
         LA    R3,3                                                             
         LA    R2,P+77                                                          
         LA    R8,DUB1             EDIT PERSON COST TO YTD-1,YTD,POST           
         BAS   RE,FRMTD                                                         
         B     REPORTP                                                          
         SPACE 1                                                                
REPER5   CLI   MYMODE,2            FOR PERSON WITHOUT TIME SHEET                
         BNE   REPER6                                                           
         USING PTOTD,R6                                                         
         LA    R6,PERWRK                                                        
         LA    R3,1                                                             
         LA    R2,P+77                                                          
         LA    R8,PINDLST          EDIT PERSON COST TO YTD/COST COLUMN          
         BAS   RE,FRMTD                                                         
         LA    R3,1                                                             
         LA    R8,PCOST            EDIT PERSON COST TO YTD/COST COLUMN          
         BAS   RE,FRMTD                                                         
         ZAP   DUB2,PCOST                                                       
         SP    DUB2,PINDLST                                                     
         LA    R3,1                                                             
         LA    R8,DUB2             EDIT PERSON COST TO YTD/COST COLUMN          
         BAS   RE,FRMTD                                                         
         MVC   P+1(40),=CL40'NO HOURS - COST TO CORP. INDIRECT'                 
         CLI   OPTNOTS,C'C'                                                     
         BE    REPORTP                                                          
         MVC   P+1(40),=CL40'NO HOURS - COST TO OFFICE INDIRECT'                
         CLI   OPTNOTS,C'O'                                                     
         BE    REPORTP                                                          
         MVC   P+1(40),=CL40'NO HOURS - COST TO DEPT. INDIRECT'                 
         B     REPORTP                                                          
         SPACE 1                                                                
REPER6   CLI   MYMODE,3                                                         
         BNE   REPER7                                                           
         MVC   P+1(40),=CL40'** ERROR ** MISSING GROUP CODES'                   
         B     REPORTP                                                          
REPER7   DC    H'0'                MYMODE UNKNOW                                
         EJECT                                                                  
*              PRINT OVERHEAD ACCOUNT                                           
         SPACE 1                                                                
         USING CLID,R7                                                          
         USING BUFD,R5                                                          
REOVH    CLI   RCSUBPRG,1                                                       
         BNE   REIND             NOT OVERHEAD MIGHT BE DIRECT/INDIRECT          
         CLI   QOPT3,C'S'                                                       
         BE    REPORTX             NO OVERHEAD PAGES                            
         CLI   MYMODE,0                                                         
         BNE   REOVH2                                                           
         LA    R3,2                                                             
         LA    R4,CDIRLST                                                       
         LA    R8,TOTLINE                                                       
         BAS   RE,ADDTOT           ADD DIRECT TO TOTAL                          
         LA    R4,OVHLST                                                        
         LA    R3,3                                                             
         BAS   RE,ADDTOT           ADD OVERHEAD YTD-1,YTD,POST TO TOTAL         
         MVC   P+1(12),CLICDE      CLIENT CODE                                  
         USING BCLID,R8                                                         
         LA    R8,NMEWRK                                                        
         MVC   P+13(36),BCLINME    AND NAME                                     
         GOTO1 SQUASHER,DMCB,P+1,50                                             
         LA    R3,2                                                             
         LA    R8,CDIRLST                                                       
         LA    R2,P+55                                                          
         BAS   RE,FRMTD            FORMAT DIRECT DOLLARS                        
         LA    R3,3                                                             
         LA    R8,OVHLST                                                        
         BAS   RE,FRMTD            FORMAT OVERHEAD DOLLARS                      
         B     REPORTP                                                          
         SPACE 1                                                                
REOVH2   CLI   MYMODE,C'T'                                                      
         BNE   REOVH4                                                           
         GOTO1 HEADUP,DMCB,(RC)    SETUP HEADLINES AGAIN                        
         GOTO1 ACREPORT            SKIP LINE BEFORE TOTAL                       
         MVC   P+1(7),=C'*TOTAL*'                                               
         LA    R3,5                                                             
         LA    R8,TOTLINE                                                       
         LA    R2,P+55                                                          
         BAS   RE,FRMTD            FORMAT DIRECT DOLLARS                        
         B     REPORTP                                                          
         SPACE 1                                                                
REOVH4   CLI   MYMODE,1                                                         
         BNE   REOVH5                                                           
         MVC   P+1(38),=C'1ND. DEPT. OVERHEAD ADDED TO CORP. O/H'               
         LA    R2,P+77                                                          
         LA    R3,2                                                             
         LA    R8,OVHLST                                                        
         BAS   RE,FRMTD                                                         
         B     REPORTP                                                          
         SPACE 1                                                                
REOVH5   CLI   MYMODE,2                                                         
         BNE   REOVH6                                                           
         LA    R2,P+77                                                          
         LA    R3,2                                                             
         LA    R8,OVHLST                                                        
         BAS   RE,FRMTD                                                         
         MVC   P+1(40),=CL40'NO OFFICE DIRECT TIME - COST TO CORP'              
         CLI   LEVEL,OFFICE                                                     
         BE    REPORTP                                                          
         MVC   P+1(40),=CL40'NO DIRECT TIME - COST TO CORP. OVERHEAD'           
         CLI   OPTOVH,C'C'                                                      
         BE    REPORTP                                                          
         MVC   P+1(40),=CL40'NO DIRECT TIME - COST TO OFFICE OVERHEAD'          
         B     REPORTP                                                          
         SPACE 1                                                                
REOVH6   CLI   MYMODE,3                                                         
         BNE   REOVH7                                                           
         MVC   P+1(40),=CL40'** ERROR ** NO DIRECT TIME COSTS'                  
         B     REPORTP                                                          
REOVH7   DC    H'0'                MYMODE UNKNOW                                
         EJECT                                                                  
*              PRINT DIRECT/INDIRECT PAGE                                       
         SPACE 1                                                                
         USING BUFD,R5                                                          
REIND    CLI   RCSUBPRG,2                                                       
         BNE   RESUM                                                            
         CLI   MYMODE,0                                                         
         BNE   REIND2                                                           
         CLI   BUFGRP,BUFEXPN                                                   
         BE    *+16                                                             
         ZAP   BDIRPST,BDIRYTD                                                  
         SP    BDIRPST,BDIRLST                                                  
         LA    R3,3                                                             
         LA    R4,BDIRLST                                                       
         LA    R8,TOTLINE                                                       
         BAS   RE,ADDTOT           ADD DIRECT TO TOTAL                          
         LA    R4,INDLST                                                        
         LA    R3,3                                                             
         BAS   RE,ADDTOT           ADD INDIRECT YTD-1,YTD,POST TO TOTAL         
         MVC   P+1(12),BUFCLI+1                                                 
         LA    R4,P+14                                                          
         LA    R6,17                                                            
REIND1A  CLI   0(R4),C' '          FIGURE OUT HOW MUCH ROOM FOR NAME            
         BNE   REIND1B                                                          
         BCTR  R4,0                                                             
         AH    R6,=H'1'                                                         
         B     REIND1A                                                          
         USING BCLID,R8                                                         
REIND1B  LA    R8,NMEWRK                                                        
         GOTO1 CHOPPER,DMCB,(36,BCLINME),((R6),2(R4)),(C'P',2)                  
         MVC   P+35(2),=C'4-'                                                   
         MVC   P+37(1),BUFGRP      GROUP FOR DIRECT                             
         CLI   BUFGRP,BUFEXPN                                                   
         BNE   *+10                                                             
         MVC   P+35(3),=C'EXP'                                                  
         CLI   BUFGRP,BUFIND                                                    
         BNE   *+10                                                             
         MVC   P+35(3),=C'IND'                                                  
         CLI   BUFGRP,BUFOVH                                                    
         BNE   *+10                                                             
         MVC   P+35(3),=C'OVH'                                                  
         LA    R3,3                                                             
         LA    R8,BDIRLST                                                       
         LA    R2,P+39                                                          
         BAS   RE,FRMTD            FORMAT DIRECT DOLLARS                        
         SPACE 1                                                                
         USING PSHEADD,R2                                                       
         LA    R2,T                                                             
         MVC   P+73(1),PSHDSBAC+2  LEDGER                                       
         MVI   P+74,C'-'                                                        
         MVC   P+75(1),PSHDSBAC+3  ACCOUNT FOR INDIRECT                         
         LA    R2,P+77                                                          
         LA    R3,3                                                             
         LA    R8,INDLST                                                        
         BAS   RE,FRMTD            FORMAT INDIRECT DOLLARS                      
         CLC   P+77(33),SPACES                                                  
         BNE   *+10                                                             
         MVC   P+73(3),SPACES      IF NO INDIRECT DON'T PRINT GROUP             
         B     REPORTP                                                          
         SPACE 1                                                                
REIND2   CLI   MYMODE,C'T'                                                      
         BNE   REIND4                                                           
         GOTO1 HEADUP,DMCB,(RC)    SETUP HEADLINES AGAIN                        
         GOTO1 ACREPORT            SKIP LINE BEFORE TOTAL                       
         MVC   P+1(11),=C'*SUB-TOTAL*'                                          
         LA    R3,3                                                             
         LA    R8,TOTLINE                                                       
         LA    R2,P+39                                                          
         BAS   RE,FRMTD            FORMAT DIRECT DOLLARS                        
         LA    R2,P+77                                                          
         LA    R3,3                                                             
         BAS   RE,FRMTD            FORMAT INDIRECT DOLLARS                      
         B     REPORTP                                                          
         SPACE 1                                                                
REIND4   CLI   MYMODE,1                                                         
         BNE   REIND5                                                           
         LA    R3,3                                                             
         LA    R2,P+77                                                          
         LA    R8,INDLST                                                        
         BAS   RE,FRMTD                                                         
         MVC   P+1(50),=CL50'NO DEPT. DIRECT TIME - COST TO OFFICE INDIX        
               RECT'                                                            
         CLI   MODE,LEVBLAST                                                    
         BE    REPORTP                                                          
         SPACE 1                                                                
         MVC   P+1(50),=CL50'NO OFFICE DIRECT TIME - COST TO CORP. INDIX        
               RECT'                                                            
         B     REPORTP                                                          
         SPACE 1                                                                
REIND5   CLI   MYMODE,2                                                         
         BNE   REIND5A                                                          
         MVC   P+1(38),=C'INDIRECT DEPT. COST ADDED TO CORP. O/H'               
         LA    R3,3                                                             
         LA    R2,P+77                                                          
         USING SUMD,R8                                                          
         LA    R8,DEPTOT                                                        
         LA    R8,SDINDLST                                                      
         LA    RE,FRMTD                                                         
         B     REPORTP                                                          
REIND5A  CLI   MYMODE,3                                                         
         BNE   REIND6                                                           
         MVC   P+1(38),=C'** ERROR NO DIRECT TIME COSTS'                        
         B     REPORTP                                                          
         SPACE 1                                                                
REIND6   CLI   MYMODE,C'S'           DEPT. SUMMARIES                            
         BNE   REIND7                                                           
         LA    R7,DEPTAB           DEPT. LABOR SUMMARY                          
         CLI   MODE,LEVBLAST                                                    
         BE    REIND6A                                                          
         LA    R7,OFFTAB                                                        
         CLI   MODE,LEVALAST                                                    
         BE    REIND6A                                                          
         LA    R7,CORTAB                                                        
REIND6A  BAS   RE,DEPLOOK          'ANYTHING IN SUMMARY                         
         CLC   TOTLINE(24),=10PL8'0'                                            
         BE    REIND6B             NOTHING IN TABLE FOR LABOR                   
         GOTO1 HEADUP,DMCB,(RC)                                                 
         GOTO1 ACREPORT            SKIP 2 LINES                                 
REIND6B  LA    R8,DEPTOT                                                        
         AP    TOTLINE(8),SDIRLST      ADD DIRECT FOR TOTAL                     
         AP    TOTLINE+8(8),SDIRYTD                                             
         AP    TOTLINE+16(8),SDIRPST                                            
         CLC   TOTLINE(24),=10PL8'0'                                            
         BE    REIND6C             NOTHING IN TABLE FOR LABOR                   
         GOTO1 HEADUP,DMCB,(RC)                                                 
         GOTO1 ACREPORT                                                         
         BAS   RE,DEPSUM           PRINT THE SUMMARY                            
         MVC   P+1(12),=C'*TOTAL TIME*'                                         
         LA    R2,P+39                                                          
         LA    R8,TOTLINE                                                       
         LA    R3,3                                                             
         BAS   RE,FRMTD            NOW PRINT THE TOTAL                          
         GOTO1 HEADUP,DMCB,(RC)                                                 
         GOTO1 ACREPORT                                                         
REIND6C  LA    R7,DEPOTAB                                                       
         CLI   MODE,LEVBLAST                                                    
         BE    REIND6D                                                          
         LA    R7,OFFOTAB                                                       
         CLI   MODE,LEVALAST                                                    
         BE    REIND6D                                                          
         LA    R7,COROTAB                                                       
REIND6D  BAS   RE,DEPLOOK          ANYTHING FOR OVERHEAD                        
         CLC   TOTLINE(24),=10PL8'0'                                            
         BE    REPORTX             NOTHING SO GET OUT                           
         GOTO1 HEADUP,DMCB,(RC)                                                 
         GOTO1 ACREPORT            SKIP A LINE                                  
         BAS   RE,DEPSUM           PRINT THE OVERHEAD SUMMARY                   
         MVC   P+1(16),=C'*TOTAL OVERHEAD*'                                     
         LA    R2,P+39                                                          
         LA    R8,TOTLINE                                                       
         LA    R3,3                                                             
         BAS   RE,FRMTD            NOW PRINT THE TOTAL                          
         GOTO1 HEADUP,DMCB,(RC)                                                 
         GOTO1 ACREPORT                                                         
         B     REPORTX                                                          
         SPACE 1                                                                
REIND7   DC    H'0'                MYMODE UNKNOWN                               
         EJECT                                                                  
*              SEE IF ANYTHING TO PRINT                                         
         USING TABD,R7                                                          
DEPLOOK  NTR1                                                                   
         MVC   TOTLINE(80),=10PL8'0'                                            
DEPLOOKA LA    R6,TABBKS           FIRST BUCKET DISP.                           
         LA    R5,TOTLINE                                                       
         LA    R4,3                                                             
DEPLOOKB CLI   0(R6),0                                                          
         BE    DEPLOOKC            NO BUCKET                                    
         L     R1,TABADD           DISP TO TABLE                                
         AR    R1,RC               IN MY WORK AREA                              
         ZIC   R9,0(R6)            DISP. TO BUCKET IN SUMMARY RECORD            
         LA    R8,0(R9,R1)                                                      
         AP    0(8,R5),0(8,R8)     ADD TO TOTLINE                               
DEPLOOKC LA    R6,1(R6)            NEXT BUCKET DISP.                            
         LA    R5,8(R5)            NEXT TOTLINE                                 
         BCT   R4,DEPLOOKB                                                      
         LA    R7,TABLEN(R7)       NEXT CATEGORY                                
         CLI   0(R7),X'FF'         END OF TABLE                                 
         BE    REPORTX                                                          
         B     DEPLOOKA                                                         
         EJECT                                                                  
*              PRINT THE DEPT. SUMMARY                                          
DEPSUM   NTR1                                                                   
DEPSUMA  LA    R6,TABBKS           FIRST BUCKET DISP                            
         LA    R4,3                                                             
         LA    R2,P+39                                                          
DEPSUMB  CLI   0(R6),0                                                          
         BE    DEPSUMC                                                          
         L     R1,TABADD                                                        
         AR    R1,RC                                                            
         ZIC   R9,0(R6)                                                         
         LA    R8,0(R9,R1)         R8 TO BUCKET                                 
         CP    0(8,R8),=P'0'                                                    
         BE    DEPSUMC                                                          
         MVC   P+1(L'TABNME),TABNME      PRINT CATEGORY NAME                    
         LA    R3,1                                                             
         BAS   RE,FRMTD            PRINT THE AMOUNT                             
         SH    R2,=H'11'                                                        
DEPSUMC  LA    R6,1(R6)            NEXT BUCKET DISP.                            
         LA    R2,11(R2)           NEXT PRINT AREA                              
         BCT   R4,DEPSUMB                                                       
         CLC   P,SPACES                                                         
         BE    DEPSUMD             NOTHING TO PRINT                             
         GOTO1 HEADUP,DMCB,(RC)                                                 
         GOTO1 ACREPORT            PRINT A LINE OF SUMMARY                      
DEPSUMD  LA    R7,TABLEN(R7)                                                    
         CLI   0(R7),X'FF'         END OF TABLE                                 
         BNE   DEPSUMA                                                          
         B     REPORTX             GO AWAY                                      
         EJECT                                                                  
DEPTAB   DS    0F                                                               
         DC    CL36'D-DEPT. INDIRECT (ABSORBED)'                                
         DC    AL4(DEPTOT-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(SDINDLST-SUMD)                                               
         DC    AL1(SDINDYTD-SUMD)                                               
         DC    AL1(SDINDPST-SUMD)                                               
         SPACE 1                                                                
         DC    CL36'O-OFFICE INDIRECT'                                          
         DC    AL4(DEPTOT-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(SOINDLST-SUMD)                                               
         DC    AL1(SOINDYTD-SUMD)                                               
         DC    AL1(SOINDPST-SUMD)                                               
         SPACE 1                                                                
         DC    CL36'C-CORPORATE INDIRECT'                                       
         DC    AL4(DEPTOT-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(SCINDLST-SUMD)                                               
         DC    AL1(SCINDYTD-SUMD)                                               
         DC    AL1(SCINDPST-SUMD)                                               
         SPACE 1                                                                
         DC    CL36'T-EXEC TIME TO OFFICE OVERHEAD'                             
         DC    AL4(DEPCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(DEXTIML-DEPD)                                                
         DC    AL1(DEXTIMY-DEPD)                                                
         DC    AL1(DEXTIMP-DEPD)                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
DEPOTAB  DS    0F                                                               
         DC    CL36'OVERHEAD (ABSORBED)'                                        
         DC    AL4(DEPTOT-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(SOVHLST-SUMD)                                                
         DC    AL1(SOVHYTD-SUMD)                                                
         DC    AL1(SOVHPST-SUMD)                                                
         SPACE 1                                                                
         DC    CL36'OVERHEAD TO OFFICE'                                         
         DC    AL4(DEPCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(DOVHOL-DEPD)                                                 
         DC    AL1(DOVHOY-DEPD)                                                 
         DC    AL1(0)                                                           
         SPACE 1                                                                
         DC    CL36'OVERHEAD TO CORPORATE'                                      
         DC    AL4(DEPCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(DOVHCL-DEPD)                                                 
         DC    AL1(DOVHCY-DEPD)                                                 
         DC    AL1(0)                                                           
         DC    X'FF'                                                            
         EJECT                                                                  
OFFTAB   DS    0F                                                               
         DC    CL36'D-DEPT. INDIRECT (ABSORBED)'                                
         DC    AL4(OFFCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(DDINDAL-DEPD)                                                
         DC    AL1(DDINDAY-DEPD)                                                
         DC    AL1(DDINDAP-DEPD)                                                
         SPACE 1                                                                
         DC    CL36'O-OFFICE INDIRECT (ABSORBED)'                               
         DC    AL4(OFFCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(OFINDAL-OFFD)                                                
         DC    AL1(OFINDAY-OFFD)                                                
         DC    AL1(OFINDAP-OFFD)                                                
         SPACE 1                                                                
         DC    CL36'C-CORPORATE INDIRECT'                                       
         DC    AL4(DEPTOT-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(SCINDLST-SUMD)                                               
         DC    AL1(SCINDYTD-SUMD)                                               
         DC    AL1(0)                                                           
         DC    X'FF'                                                            
         SPACE 1                                                                
OFFOTAB  DS    0F                                                               
         DC    CL36'DEPT. OVERHEAD (ABSORBED)'                                  
         DC    AL4(OFFCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(DOVHAL-DEPD)                                                 
         DC    AL1(DOVHAY-DEPD)                                                 
         DC    AL1(DOVHAP-DEPD)                                                 
         SPACE 1                                                                
         DC    CL36'OFFICE OVERHEAD (ABSORBED)'                                 
         DC    AL4(OFFCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(OFOVHAL-OFFD)                                                
         DC    AL1(OFOVHAY-OFFD)                                                
         DC    AL1(OFOVHAP-OFFD)                                                
         SPACE 1                                                                
         DC    CL36'DEPT. OVERHEAD TO CORP.'                                    
         DC    AL4(OFFCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(DOVHCL-DEPD)                                                 
         DC    AL1(DOVHCY-DEPD)                                                 
         DC    AL1(0)                                                           
         SPACE 1                                                                
         DC    CL36'OFFICE OVERHEAD TO CORP.'                                   
         DC    AL4(OFFCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(OFOVHCL-OFFD)                                                
         DC    AL1(OFOVHCY-OFFD)                                                
         DC    AL1(0)                                                           
         DC    X'FF'                                                            
         EJECT                                                                  
CORTAB   DS    0F                                                               
         DC    CL36'D-DEPT. INDIRECT (ABSORBED)'                                
         DC    AL4(AGYCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(DDINDAL-DEPD)                                                
         DC    AL1(DDINDAY-DEPD)                                                
         DC    AL1(DDINDAP-DEPD)                                                
         SPACE 1                                                                
         DC    CL36'O-OFFICE INDIRECT (ABSORBED)'                               
         DC    AL4(AGYCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(OFINDAL-OFFD)                                                
         DC    AL1(OFINDAY-OFFD)                                                
         DC    AL1(OFINDAP-OFFD)                                                
         SPACE 1                                                                
         DC    CL36'C-CORPORATE INDIRECT (ABSORBED)'                            
         DC    AL4(AGYCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(AGINDAL-AGYD)                                                
         DC    AL1(AGINDAY-AGYD)                                                
         DC    AL1(AGINDAP-AGYD)                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
COROTAB  DS    0F                                                               
         DC    CL36'DEPT. OVERHEAD (ABSORBED)'                                  
         DC    AL4(AGYCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(DOVHAL-DEPD)                                                 
         DC    AL1(DOVHAY-DEPD)                                                 
         DC    AL1(DOVHAP-DEPD)                                                 
         SPACE 1                                                                
         DC    CL36'OFFICE OVERHEAD (ABSORBED)'                                 
         DC    AL4(AGYCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(OFOVHAL-OFFD)                                                
         DC    AL1(OFOVHAY-OFFD)                                                
         DC    AL1(OFOVHAP-OFFD)                                                
         SPACE 1                                                                
         DC    CL36'CORP. OVERHEAD (ABSORBED)'                                  
         DC    AL4(AGYCUM-A202D)                                                
         DC    XL1'00'                                                          
         DC    AL1(AGOVHAL-AGYD)                                                
         DC    AL1(AGOVHAY-AGYD)                                                
         DC    AL1(AGOVHAP-AGYD)                                                
         SPACE 1                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              PRINT OFFICE /DEPT SUMMARY                                       
         SPACE 1                                                                
         USING SUMD,R8                                                          
RESUM    CLI   RCSUBPRG,3                                                       
         BNE   REDDS                                                            
         CLC   SDIRPST(64),=10PL8'0'                                            
         BE    REPORTX             NOHING GOOD TO PRINT                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SOFFDEP(0),=12X'FF' FOXES IN OFFICE MEANS AGY TOTALS             
         BNE   RESUM3                                                           
*                                                                               
         GOTO1 HEADUP,DMCB,(RC)                                                 
         GOTO1 ACREPORT            SKIP LINE BEFORE TOTAL                       
         MVC   P+1(15),=C'*AGENCY TOTALS*'                                      
         B     RESUM7                                                           
         SPACE 1                                                                
*****************************************                                       
RESUM3   LA    R2,SOFFDEP                                                       
         SR    R0,R0                                                            
         IC    R0,LENLEVA                                                       
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB          LENGTH OF LEVEL B                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),=12X'FF'    OFFICE TOTAL                                 
         BNE   RESUM4                                                           
         MVI   SPACING,2                                                        
         MVC   P+1(15),=C'*OFFICE TOTALS*'                                      
         B     RESUM7                                                           
         SPACE 1                                                                
RESUM4   SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   P+5(0),SOFFDEP      OFFICE CODE                                  
         LA    R2,SOFFDEP                                                       
         SR    R0,R0                                                            
         IC    R0,LENLEVA                                                       
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB          LENGTH OF LEVEL B                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   P+12(0),0(R2)       DEPT CODE                                    
         SPACE 1                                                                
RESUM7   LA    R3,8                                                             
         LA    R8,SDIRPST                                                       
         LA    R2,P+22                                                          
         BAS   RE,FRMTD            FORMAT SUMMARY DOLLARS                       
         B     REPORTP                                                          
         SPACE 1                                                                
RESUM8   DC    0H'0'                                                            
         B     REPORTP                                                          
         EJECT                                                                  
*              PRINT DDS CONTROL TOTALS                                         
REDDS    CLI   RCSUBPRG,4                                                       
         BNE   REPOST                                                           
         MVC   P+25(13),=C'TOTAL POSTING'                                       
         EDIT  POSTCASH,(12,P+40),2,MINUS=YES                                   
         GOTO1 LOGIO,DMCB,1,(40,P+25)                                           
         B     REPORTP                                                          
         EJECT                                                                  
*              PRINT POSTING SUMMARY                                            
REPOST   CLI   RCSUBPRG,5                                                       
         BE    *+6                                                              
         DC    H'0'                BAD RCSUBPRG                                 
         CLI   MYMODE,0                                                         
         BNE   REPOST4                                                          
         L     R8,RECORD           PRINT DETAILS OF RECORD FROM SORT            
         USING SRTD,R8                                                          
         LA    R2,SRTREC                                                        
         LA    R2,4(R2)                                                         
         USING PSHEADD,R2                                                       
         LA    R7,P+44                                                          
         LA    R5,TOTLINE                                                       
         CLC   PSHDSBAC+1(2),=C'14' IS CONTRA GROUP 4                           
         BE    REPOST3                                                          
         LA    R7,P+55                                                          
         LA    R5,TOTLINE+8                                                     
         CLC   PSHDSBAC+1(2),=C'15' OR GROUP                                    
         BNE   REPORTX             IF NEITHER GET OUT                           
REPOST3  MVC   P+1(12),PSHDACC+3   ACCOUNT CODE                                 
         ZIC   R4,1(R2)                                                         
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         LA    R8,TRNSAMNT                                                      
         LA    R3,1                                                             
         LR    R2,R7               PRINT AREA                                   
         AP    0(8,R5),TRNSAMNT    ADD AMOUNT TO TOTAL LINE                     
         ZAP   DOUBLE,TRNSAMNT                                                  
         BAS   RE,FRMTD            FORMAT DOLLARS                               
         B     REPORTP             PRINT THE LINE AND GET OUT                   
         SPACE 1                                                                
REPOST4  CLI   MYMODE,C'T'                                                      
         BNE   REPOST6                                                          
         LA    R2,P+44             NOW PRINT TOTAL                              
         LA    R3,2                                                             
         LA    R8,TOTLINE                                                       
         MVC   P+1(7),=C'*TOTAL*'                                               
         BAS   RE,FRMTD                                                         
         B     REPORTP                                                          
         SPACE 1                                                                
REPOST6  DC    H'0'                MYMODE UNKNOW                                
         SPACE 2                                                                
REPORTP  GOTO1 HEADUP,DMCB,(RC)    SET UP HEADLINES                             
         GOTO1 ACREPORT                                                         
REPORTX  XMOD1                                                                  
         EJECT                                                                  
ADDTOT   AP    0(8,R8),0(8,R4)     ADD ACCUMULATORS TO TOTLINE                  
         LA    R8,8(R8)                                                         
         LA    R4,8(R4)                                                         
         BCT   R3,*-14                                                          
         BR    RE                                                               
         SPACE 2                                                                
FRMTH    ZAP   DOUBLE,0(8,R8)      FORMAT HOURS                                 
         EDIT  (P8,DOUBLE),(8,0(R2)),2,MINUS=YES                                
         LA    R8,8(R8)                                                         
         LA    R2,8(R2)                                                         
         BCT   R3,FRMTH                                                         
         BR    RE                                                               
         SPACE 2                                                                
FRMTD    ZAP   DOUBLE,0(8,R8)      FORMAT DOLLARS                               
         AP    DOUBLE,=P'50'       ROUNDING                                     
         CP    DOUBLE,=P'0'                                                     
         BNL   *+10                                                             
         SP    DOUBLE,=P'100'                                                   
         DP    DOUBLE,=P'100'      DROP THE PENNIES                             
         EDIT  (P6,DOUBLE),(11,0(R2)),MINUS=YES                                 
FRMTD3   LA    R8,8(R8)                                                         
         LA    R2,11(R2)                                                        
         BCT   R3,FRMTD                                                         
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINES TO HEADUP PAGE                                          
         SPACE 1                                                                
ACA2HEAD DS    0D                                                               
         NMOD1 0,*HEAD*                                                         
         L     RC,0(R1)                                                         
         MVC   HEAD3+58(6),HEADMON                                              
         MVC   HEAD3+85(4),=C'LIVE'                                             
         CLI   QOPT1,C'P'                                                       
         BE    *+10                                                             
         MVC   HEAD3+85(5),=C'DRAFT'                                            
         SPACE 1                                                                
         CLI   RCSUBPRG,0                                                       
         BNE   HEADOVH             HEADUP A PERSON PAGE                         
         BAS   R8,HEADOFF          OFFICE HEADING                               
         BAS   R8,HEADDEP          DEPT HEADING                                 
         BAS   R8,HEADPER          PERSON HEADING                               
         B     HEADX                                                            
         SPACE 1                                                                
HEADOVH  CLI   RCSUBPRG,1                                                       
         BNE   HEADIND             HEADUP AN OVERHEAD PAGE                      
         LA    R7,HEAD5                                                         
         CLI   LEVEL,COMP                                                       
         BE    HEADOV4             CORPORATE OVERHEAD                           
         BAS   R8,HEADOFF                                                       
         LA    R7,132(R7)                                                       
         CLI   LEVEL,OFFICE                                                     
         BE    HEADOV4             OFFICE OVERHEAD                              
         BAS   R8,HEADDEP                                                       
         LA    R7,132(R7)                                                       
HEADOV4  MVC   1(8,R7),=C'OVERHEAD'                                             
         L     R2,ADACC                                                         
         LA    R2,3(R2)                                                         
         SR    R0,R0                                                            
         IC    R0,LLEVB                                                         
         AR    R2,R0               BUMP PAST OF DEPT                            
         SR    R1,R1                                                            
         IC    R1,LENLEVC                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R7),0(R2)      SUB DEP                                      
         LA    R1,1(R1)                                                         
         LA    R3,12(R7)                                                        
         AR    R3,R1               LEAVE SPACE BETWEEN SUB AND EMPL             
         L     R2,ADACC                                                         
         LA    R2,3(R2)                                                         
         SR    R0,R0                                                            
         IC    R0,LLEVC                                                         
         AR    R2,R0               BUMP PAST OFF/DPT/SUB                        
         SR    R1,R1                                                            
         IC    R1,LENLEVD                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)       ACCOUNT                                      
         L     R4,ADACCNAM                                                      
         LA    R6,23(R7)                                                        
         BAS   RE,HNAMOUT          NAME                                         
         MVC   60(11,R7),=C'(GROUP= - )'                                        
         MVC   67(1,R7),SVGP        LEDGER                                      
         MVC   69(1,R7),SVGP+1      ACCOUNT FOR OVERHEAD                        
         GOTO1 SQUASHER,DMCB,(0,12(R7)),60                                      
         CP    OVACC,=P'0'                                                      
         BE    HEADOV5                                                          
         MVC   HEAD5+85(8),=C'ACCOUNT='                                         
         EDIT  (P8,OVACC),(11,HEAD5+93),2,MINUS=YES,ALIGN=LEFT                  
HEADOV5  CP    OVTHR,=P'0'                                                      
         BE    HEADOV6                                                          
         MVC   HEAD6+85(6),=C'OTHER='                                           
         EDIT  (P8,OVTHR),(11,HEAD6+91),2,MINUS=YES,ALIGN=LEFT                  
HEADOV6  B     HEADX                                                            
         EJECT                                                                  
HEADIND  CLI   RCSUBPRG,2                                                       
         BNE   HEADSUM             HEADUP DIRECT/INDIRECT PAGE                  
         LA    R7,HEAD5                                                         
         CLI   LEVEL,COMP                                                       
         BE    HEADIND4            CORPORATE OVERHEAD                           
         BAS   R8,HEADOFF                                                       
         LA    R7,132(R7)                                                       
         CLI   LEVEL,OFFICE                                                     
         BE    HEADIND4            OFFICE OVERHEAD                              
         BAS   R8,HEADDEP                                                       
         LA    R7,132(R7)                                                       
HEADIND4 MVC   1(33,R7),=C'** DIRECT/INDIRECT TIME TOTALS **'                   
         MVC   HEAD10+65(6),=C'AMOUNT'                                          
         CLI   MODE,LEVBLAST                                                    
         BE    *+10                                                             
         MVC   HEAD10+65(6),=C'TOTALS'                                          
         B     HEADX                                                            
         SPACE 1                                                                
HEADSUM  CLI   RCSUBPRG,3                                                       
         BNE   HEADDDS             HEADUP AN OFFICE-DEPT SUMMARY PAGE           
         MVC   HEAD5+1(19),=C'OFFICE-DEPT SUMMARY'                              
         CLI   MYMODE,1                                                         
         BNE   *+10                                                             
         MVC   HEAD5+21(5),=C'(YTD)'                                            
         B     HEADX                                                            
         SPACE 1                                                                
HEADDDS  CLI   RCSUBPRG,4          DDS CONTROL PAGE                             
         BNE   HEADPOST                                                         
         B     HEADX                                                            
         SPACE 1                                                                
HEADPOST CLI   RCSUBPRG,5                                                       
         BNE   HEADALT                                                          
         MVC   HEAD5+1(15),=C'POSTING SUMMARY'                                  
         B     HEADX                                                            
HEADALT  CLI   RCSUBPRG,6          ALTERNATE HEADING                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     HEADX                                                            
         EJECT                                                                  
HEADOFF  L     R2,ADHEIRA                                                       
         SR    R1,R1                                                            
         IC    R1,LLEVA                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   3(0,R2),=12C'9'                                                  
         BNE   HEADOFF2            OFFICE 9 IS NOT AN OFFICE                    
         SH    R7,=H'132'                                                       
         BR    R8                                                               
HEADOFF2 MVC   HEAD5+1(6),=C'OFFICE'                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEAD5+12(0),3(R2)   OFFICE CODE(R1 R2 SET ABOVE)                 
         L     R4,ADLVANAM                                                      
         LA    R6,HEAD5+15                                                      
         BAS   RE,HNAMOUT           AND NAME                                    
         BR    R8                                                               
         SPACE 1                                                                
HEADDEP  L     R2,ADHEIRB                                                       
         LA    R2,3(R2)                                                         
         SR    R0,R0                                                            
         IC    R0,LLEVA                                                         
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),=12C'9'                                                  
         BNE   HEADDEP2            DEPT 99 IS NOT A DEPT                        
         SH    R7,=H'132'                                                       
         BR    R8                                                               
HEADDEP2 MVC   HEAD6+1(4),=C'DEPT'                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEAD6+12(0),0(R2)   DEPARTMENT CODE(R1 R2 SET ABOVE)             
         L     R4,ADLVBNAM                                                      
         LA    R6,HEAD6+15                                                      
         BAS   RE,HNAMOUT           AND NAME                                    
         BR    R8                                                               
         SPACE 1                                                                
HEADPER  L     R2,ADACC                                                         
         LA    R3,HEAD7                                                         
         CLI   OPTSKIP,C'Y'                                                     
         BE    *+8                                                              
         LA    R3,MID1                                                          
         MVC   1(6,R3),=C'PERSON'                                               
         LA    R2,3(R2)                                                         
         SR    R0,R0                                                            
         IC    R0,LLEVB                                                         
         AR    R2,R0               BUMP PAST OFF DEPT                           
         SR    R1,R1                                                            
         IC    R1,LENLEVC          LEN OF SUBDEPT                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),0(R2)      SUB DEPT                                     
         SR    R1,R1                                                            
         IC    R1,LENLEVC                                                       
         LA    R1,1(R1)                                                         
         LA    R4,12(R3)                                                        
         AR    R4,R1               LEAVE A SPACE BETWEEN SUB AND PERSON         
         L     R2,ADACC                                                         
         LA    R2,3(R2)                                                         
         SR    R0,R0                                                            
         IC    R0,LLEVC                                                         
         AR    R2,R0               POINTS TO PERSON                             
         SR    R1,R1                                                            
         IC    R1,LENLEVD                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)       PERSON                                       
         L     R4,ADACCNAM                                                      
         LA    R6,24(R3)                                                        
         BAS   RE,HNAMOUT                                                       
         MVC   62(11,R3),=C'(GROUP=4- )'                                        
         MVC   71(1,R3),GROUP                                                   
         LA    R2,12(R3)                                                        
         CLI   OPTHNT,C'B'          DEFAULT IS N                                
         BE    *+8                                                              
         CLI   OPTHNT,C'T'                                                      
         BE    *+8                                                              
         CLI   OPTHNT,C'H'                                                      
         BNE   *+8                                                              
         BAS   RE,HNTDATE                                                       
         SPACE 1                                                                
         GOTO1 SQUASHER,DMCB,(R2),70                                            
         L     R2,DMCB+4                                                        
         LA    R3,HEAD5+85                                                      
         CLI   OPTSKIP,C'Y'                                                     
         BE    *+8                                                              
         LA    R3,MID1+13(R2)                                                   
         MVC   0(L'SALVAL,R3),SALVAL                                            
         MVC   13(L'SALPCT,R3),SALPCT                                           
         GOTO1 SQUASHER,DMCB,(R3),30                                            
         LA    R3,132(R3)                                                       
         MVC   0(6,R3),=C'TOTAL='                                               
         USING PTOTD,R6                                                         
         LA    R6,PERWRK                                                        
         EDIT  PCOST,(11,6(R3)),2,MINUS=YES,ALIGN=LEFT                          
         BR    R8                                                               
         EJECT                                                                  
         USING ACNAMED,R4                                                       
HNAMOUT  LTR   R4,R4                                                            
         BZR   RE                                                               
         MVC   0(36,R6),SPACES                                                  
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R6),ACNMNAME                                                 
         SPACE 1                                                                
HEADX    XMOD1 1                                                                
         EJECT                                                                  
         USING ACEMPD,R4                                                        
HNTDATE  NTR1                                                                   
         L     R4,ADACC                                                         
         GOTO1 GETEL,DMCB,(RC),(X'56',(R4)),0                                   
         CLI   DMCB+12,0                                                        
         BNE   HNT9                                                             
         L     R4,DMCB+12                                                       
         LA    R3,HEAD8+12                                                      
         CLI   OPTSKIP,C'Y'                                                     
         BE    *+8                                                              
         LA    R3,MID2+12                                                       
         LR    R6,R3                                                            
         OC    ACEMPHIR,ACEMPHIR                                                
         BZ    HNT1                                                             
         CLI   OPTHNT,C'T'                                                      
         BE    HNT1                                                             
         MVI   0(R3),C'*'                                                       
         LA    R3,1(R3)                                                         
         MVC   0(2,R3),=C'H-'                                                   
         LA    R3,2(R3)                                                         
         GOTO1 DATCON,DMCB,(1,ACEMPHIR),(5,(R3))                                
         LA    R3,8(R3)                                                         
         CLI   OPTHNT,C'H'                                                      
         BE    HNT8                                                             
HNT1     EQU   *                                                                
         OC    ACEMPTRM,ACEMPTRM                                                
         BZ    HNT8                                                             
         CLI   OPTHNT,C'T'                                                      
         BE    HNT3                                                             
         SPACE 1                                                                
         OC    ACEMPHIR,ACEMPHIR                                                
         BZ    HNT3                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         B     HNT4                                                             
         SPACE 1                                                                
HNT3     EQU   *                                                                
         MVI   0(R3),C'*'                                                       
         LA    R3,1(R3)                                                         
HNT4     EQU   *                                                                
         MVC   0(2,R3),=C'T-'                                                   
         LA    R3,2(R3)                                                         
         GOTO1 DATCON,DMCB,(1,ACEMPTRM),(5,(R3))                                
         LA    R3,8(R3)                                                         
HNT8     EQU   *                                                                
         CLI   0(R6),C'*'                                                       
         BNE   HNT9                                                             
         MVI   RCSUBPRG,6                                                       
         MVI   0(R3),C'*'                                                       
HNT9     EQU   *                                                                
         B     HEADX                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              BUILD SKELETON POSTING FILE RECORD                               
         SPACE 1                                                                
ACA2BLDP DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,*BUILD*                                                        
         L     RC,0(R1)                                                         
         LA    R2,T                                                             
         XC    T-4(4),T-4                                                       
         USING PSHEADD,R2                                                       
         MVC   PSHDEL(2),=X'5046'                                               
         MVC   PSHDANAL,SPACES                                                  
         ZIC   R4,1(R2)                                                         
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVC   TRNSEL(2),=X'441D'                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TRNSDATE)                              
         MVI   TRNSSBRF,0                                                       
         MVI   TRNSTYPE,X'91'                                                   
         MVC   TRNSBTCH,SPACES                                                  
         MVC   TRNSBTCH(2),MOS                                                  
         MVI   TRNSNARR,X'40'                                                   
         MVI   TRNSNARR+1,0                                                     
         XMOD1 1                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO BUILD POSTING RECORDS                                 
*              PUT WORKER RECORD TO ACPOST                                      
         SPACE 1                                                                
ACA2POST DS    0D                                                               
         NMOD1 0,**POST**                                                       
         L     RC,0(R1)                                                         
         CLI   MODE,RUNLAST                                                     
         BNE   PUTA                                                             
         CP    POSTREC,=P'0'    IF RUNLAST POST BALANCING ITEM                  
         BE    CPXIT           ONLY IF I HAVE POSTED SOMETHING ELSE             
         B     PUTB                                                             
PUTA     CLI   POST,C'Y'                                                        
         BNE   CPXIT                                                            
         SPACE 1                                                                
PUTB     LA    R2,T                                                             
         ZIC   RF,1(R2)                                                         
         LA    R4,0(RF,R2)                                                      
         USING TRANSD,R4                                                        
         CP    TRNSAMNT,=P'0'      DON'T MAKE ZERO POSTINGS                     
         BE    CPXIT                                                            
         SPACE 1                                                                
         AP    POSTREC,=P'1'                                                    
         TM    TRNSSTAT,X'80'                                                   
         BZ    PUTCR               POST CREDIT                                  
         AP    POSTCASH,TRNSAMNT                                                
         AP    PDEBITS,TRNSAMNT    ADD DEBITS                                   
         B     PUT1                                                             
PUTCR    AP    PCREDITS,TRNSAMNT   ADD CREDITS                                  
PUT1     EDIT  POSTREC,(6,TRNSREF),FILL=0                                       
         ZIC   R3,1(R2)                                                         
PUT2     AR    R2,R3                                                            
         CLI   0(R2),0                                                          
         BE    PUT4                                                             
         IC    R3,1(R2)                                                         
         LTR   R3,R3                                                            
         BNZ   PUT2                                                             
         MVI   0(R2),0                                                          
PUT4     LA    R2,1(R2)                                                         
         LA    R3,T-4                                                           
         SR    R2,R3                                                            
         STH   R2,T-4                                                           
         SPACE 1                                                                
         LA    R3,T-4                                                           
         L     R4,POSTBUFF                                                      
         GOTO1 WORKER,DMCB,=CL6'ADD',(R4),ID,(R3)   ADD TOTAL RECORD            
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     CPXIT                                                            
         SPACE 1                                                                
         USING SRTD,R8                                                          
         L     R8,RECORD                                                        
         XC    0(50,R8),0(R8)                                                   
         LA    R6,SRTREC                                                        
         LA    R7,600                                                           
         LA    R2,T-4                                                           
         XR    R3,R3                                                            
         ICM   R3,3,0(R2)                                                       
         MVCL  R6,R2               MOVE POSTING RECORD TO SRTREC                
         SPACE 1                                                                
         LA    R2,T                                                             
         USING PSHEADD,R2                                                       
         ZIC   R4,1(R2)                                                         
         LA    R4,0(R4,R2)                                                      
         USING TRANSD,R4                                                        
         MVC   SRTKACC,PSHDACC     BUILD SORT KEY                               
         MVC   SRTKCON,PSHDSBAC                                                 
         MVI   SRTKTYP,1           DEBIT                                        
         TM    TRNSSTAT,X'80'                                                   
         BO    PUT5                                                             
         MVI   SRTKTYP,2           CREDIT                                       
PUT5     LA    R2,T-4                                                           
         XR    R3,R3               SET SORT RECORD LENGTH                       
         ICM   R3,3,0(R2)                                                       
         AH    R3,=H'40'                                                        
         STCM  R3,3,SRTLEN                                                      
*        GOTO1 SORTER,DMCB,=C'PUT',(R8)                                         
         B     CPXIT                                                            
         SPACE 1                                                                
CPXIT    LA    R2,T                                                             
         ZIC   RF,1(R2)                                                         
         LA    R4,0(RF,R2)                                                      
CPXIT1   CLI   0(R4),0             CLEAR 50 ELEMENT                             
         BE    CPOUT                                                            
         CLI   0(R4),X'50'                                                      
         BE    CPOUT                                                            
         ZIC   R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     CPXIT1                                                           
CPOUT    XC    0(2,R4),0(R4)                                                    
         XMOD1 1                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO POST DIRECT TIME TO BUFFALO RECORDS                   
         SPACE 1                                                                
         USING CLID,R7                                                          
ABUFPOST DS    0D                                                               
         NMOD1 0,**BUFP**                                                       
         L     RC,0(R1)                                                         
         L     R7,4(R1)                                                         
         CLI   CLILEDG,C'C'                                                     
         BNE   BUFPX               DON'T POST NON-CLIENT TIME                   
         USING BCLID,R8                                                         
         LA    R8,NMEWRK                                                        
         MVC   BCLICDE,CLICDE      ADD CLIENT CODE AND NAME TO TABLE            
         MVC   BCLINME,CLICLNME                                                 
         GOTO1 NMEPUT,DMCB,(RC)                                                 
         USING BUFD,R5                                                          
         LA    R5,BUFWRK                                                        
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         LA    R2,BUFBK            CLEAR BUFFALO BUCKETS                        
         LA    R3,BBKCNT                                                        
         ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,*-10                                                          
         SPACE 1                                                                
         MVC   BUFCLI,CLILEDG           CLIENT CODE                             
         MVC   BUFGRP,GROUP             GROUP CODE                              
         MVI   BUFTYPE,BUFBYGP          CLIENT BY GROUP                         
         BAS   RE,NBTEST                                                        
         CLI   NBSW,C'N'                                                        
         BE    *+8                                                              
         MVI   BUFTYPE,BUFNEWB          NEW BUSINESS CLIENT TYPE 3              
         CLI   POSTSW,C'M'                                                      
         BNE   BUFP3                                                            
         ZAP   BDIRLSTM,DUB             YTD-1 AS MEMO                           
         B     BUFP4                                                            
BUFP3    ZAP   BDIRLST,CCSTLST          YTD-1                                   
         ZAP   BDIRYTD,CCSTYTD          YTD                                     
         ZAP   BDIRPST,CCSTPST          POSTING                                 
         SPACE 1                                                                
BUFP4    GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFWRK                               
         XC    BUFCLI,BUFCLI          TOTAL RECORD BY GROUP                     
         MVI   BUFGRP,0                                                         
         BASR  RE,RF                                                            
         SPACE 1                                                                
         CLI   NBSW,C'Y'                                                        
         BE    BUFPX                                                            
         MVC   BUFCLI,CLILEDG                                                   
         MVI   BUFGRP,0                                                         
         MVI   BUFTYPE,BUFNOGP         CLIENT NO GROUP                          
         BASR  RE,RF                                                            
         XC    BUFCLI,BUFCLI       TOTAL FOR NO GROUP                           
         BASR  RE,RF                                                            
BUFPX    XMOD1 1                                                                
         EJECT                                                                  
*              DETERMINE IF THIS BUFFALO RECORD IS NEW BUSINESS                 
         SPACE 2                                                                
         USING BUFD,R5                                                          
NBTEST   DS    0H                                                               
         MVI   NBSW,C'N'                                                        
         CLI   OPTNBT,C'N'                                                      
         BER   RE                  NO NEW BUSINESS OPTION                       
         CLI   BUFCLI,C'C'                                                      
         BNER  RE                                                               
*                                                                               
         CLC   ALPHAID,=C'T6'      FOR DDST6                                    
         BE    NBTESTT6                                                         
         CLC   ALPHAID,=C'KP'      FOR KALLIR,PHILIPS AND ROSS                  
         BE    NBTESTKP                                                         
*        CLC   ALPHAID,=C'BS'      FOR BACKER (PER SREN 5/24/95)                
*        BE    NBTESTBS                                                         
         CLC   ALPHAID,=C'DM'      AND DOREMUS                                  
         BE    NBTESTDM            SPECIAL TEST FOR NEW BUSINESS                
         CLC   ALPHAID,=C'GV'      AND GAVIN                                    
         BE    NBTESTDM            SPECIAL TEST FOR NEW BUSINESS                
         CLC   ALPHAID,=C'PN'      AND PORTER                                   
         BE    NBTESTDM            SPECIAL TEST FOR NEW BUSINESS                
         CLC   ALPHAID,=C'CE'      AND CEMN                                     
         BE    NBTESTCE            SPECIAL TEST FOR NEW BUSINESS                
         CLC   ALPHAID,=C'WS'      AND WSTO                                     
         BE    NBTESTWS            SPECIAL TEST FOR NEW BUSINESS                
*                                                                               
         CLI   BUFCLI+2,C'9'       CODES OF 9A TO 98 ARE NEW BUSINESS           
         BNER  RE                                                               
         CLI   BUFCLI+3,C'9'                                                    
         BER   RE                                                               
         B     NBTEST99                                                         
*                                                                               
NBTESTT6 CLC   BUFCLI+2(2),=C'DO'  CLIENTS WITH FIRST 2 OF CLIENT               
         BNER  RE                  = DO ARE NEW BUSINESS                        
         B     NBTEST99                                                         
*                                                                               
NBTESTDM CLC   BUFCLI+3(2),=C'DO'  CLIENTS WITH FIRST 2 OF CLIENT               
         BNER  RE                  = DO ARE NEW BUSINESS                        
         B     NBTEST99                                                         
*                                                                               
NBTESTBS CLI   BUFCLI+1,C'2'       CLIENTS WITH FIRST POSITION                  
         BNER  RE                  = 2 ARE NEW BUSINESS                         
         B     NBTEST99                                                         
*                                                                               
NBTESTWS CLI   BUFCLI+1,C'9'       CLIENTS WITH FIRST POSITION                  
         BNER  RE                  = 9 ARE NEW BUSINESS                         
         B     NBTEST99                                                         
*                                                                               
NBTESTCE CLI   BUFCLI+3,C'9'       CODES OF 9A TO 98 ARE NEW BUSINESS           
         BNER  RE                                                               
         CLI   BUFCLI+4,C'9'                                                    
         BER   RE                                                               
         B     NBTEST99                                                         
*                                                                               
NBTESTKP CLI   BUFCLI+4,C'9'       CODES OF 9A TO 98 ARE NEW BUSINESS           
         BNER  RE                                                               
         CLI   BUFCLI+5,C'9'                                                    
         BER   RE                                                               
NBTEST99 MVI   NBSW,C'Y'           THIS IS A NB CLIENT                          
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 1-2  (RC)                                              
*              P2   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P3   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
ACA2GETL DS    0D                                                               
         NMOD1 0,**GETEL**                                                      
         L     RC,0(R1)                                                         
         LM    R2,R3,4(R1)                                                      
         ZIC   R4,4(R1)                                                         
         ZIC   R5,8(R1)                                                         
         GOTO1 HELLO,DMCB,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))           
         XMOD1                                                                  
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO GET AND PRINT SORTED POSTINGS                         
         SPACE 1                                                                
ACA2SRTP DS    0D                                                               
         NMOD1 0,**SRTP**                                                       
         L     RC,0(R1)                                                         
         MVC   TOTLINE(80),=10PL8'0'                                            
         L     R8,RECORD                                                        
         XC    0(50,R8),0(R8)                                                   
SRTP2    GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         ST    R6,INREC                                                         
         LTR   R6,R6                                                            
         BZ    SRTEND                                                           
         OC    0(4,R8),0(R8)                                                    
         BNZ   SRTP10              NOT FIRST RECORD                             
SRTP5    LR    R4,R8                                                            
         LA    R5,500                                                           
         XR    R7,R7                                                            
         ICM   R7,3,0(R6)                                                       
         MVCL  R4,R6               SAVE FIRST INPUT IN RECORD                   
         B     SRTP2               GET NEXT                                     
         SPACE 1                                                                
         USING SRTD,R8                                                          
SRTP10   CLC   SRTLEN(40),0(R6)    SAME LENGTH AND KEY                          
         BNE   SRTEND              NOT SAME - SO PRINT AND POST LAST            
         LA    R2,SRTREC                                                        
         LA    R2,4(R2)                                                         
         ZIC   R4,1(R2)                                                         
         LA    R4,0(R4,R2)         TRANSACTION IN LAST RECORD                   
         USING TRANSD,R4                                                        
         LA    R3,SRTREC-SRTD(R6)                                               
         LA    R3,4(R3)                                                         
         ZIC   R5,1(R3)                                                         
         LA    R5,0(R5,R3)         R5 TO TRANSACTION IN NEW RECORD              
         CLC   TRNSANAL,TRNSANAL-TRANSD(R5)                                     
         BNE   SRTEND              ANALYSIS MUST BE SAME                        
         AP    TRNSAMNT,TRNSAMNT-TRANSD(L'TRNSAMNT,R5)                          
         SPACE 1                                                                
SRTP15   ZIC   R1,1(R4)                                                         
         AR    R4,R1               LOOK FOR A 50 ELEMENT                        
         ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         CLC   0(1,R4),0(R5)       BOTH S/B 0 OR 50                             
         BE    *+6                                                              
         DC    H'0'                NOT THE SAME ELEMENT                         
         CLI   0(R4),0                                                          
         BE    SRTP2               BOTH AT END OF RECORD - GET NEXT             
         CLI   0(R4),X'50'                                                      
         BE    *+6                                                              
         DC    H'0'                UNKNOWN ELEMENT                              
         USING TRCASHD,R4                                                       
         AP    TRCSAMNT,TRCSAMNT-TRCASHD(L'TRCSAMNT,R5)                         
         CLI   TRCSLEN,9                                                        
         BE    SRTP15                                                           
         LA    R6,TRCSAMNT-TRCASHD(R5)                                          
         LA    R6,6(R6)                                                         
         AP    TRCSAMNT+6(6),0(L'TRCSAMNT,R6)                                   
         B     SRTP15                                                           
         SPACE 1                                                                
SRTEND   L     R8,RECORD                                                        
         CLI   SRTKTYP,0                                                        
         BE    SRTX                NO INPUT RECORDS                             
         CLI   SRTKTYP,1                                                        
         BE    SRTPST              DEBITS DON'T PRINT                           
         GOTO1 REPORT,DMCB,(5,(RC)),0                                           
         SPACE 1                                                                
SRTPST   LA    R3,SRTREC                                                        
         L     R4,POSTBUFF                                                      
         CLI   POST,C'Y'                                                        
         BNE   SRTPST2             DON'T POST                                   
         GOTO1 WORKER,DMCB,=CL6'ADD',(R4),ID,(R3)                               
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
SRTPST2  L     R6,INREC                                                         
         LTR   R6,R6                                                            
         BZ    SRTOT               NO MORE INPUT  DO TOTAL                      
         L     R8,RECORD                                                        
         B     SRTP5               MOVE LAST TO RECORD                          
         SPACE 1                                                                
SRTOT    GOTO1 REPORT,DMCB,(5,(RC)),(C'T',0)                                    
SRTX     XMOD1 1                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         BUFF  LINES=500,ROWS=3,COLUMNS=06,FLAVOR=PACKED,              X        
               KEYLIST=(15,A)                                                   
         SPACE 1                                                                
CRECORD  DS    0D                                                               
         DS    1008C                                                            
         SPACE 1                                                                
CGRPTAB  DS    0D                                                               
         DS    6000C                                                            
         SPACE 1                                                                
CPSTBUFF DS    0D                                                               
         DC    4500X'00'                                                        
         SPACE 1                                                                
CCLIBUFF DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(CLEN)           RECORD LENGTH                                
         DC    AL4(CLIKLEN)        DISP. TO KEY / KEY LENGTH                    
         DC    F'2000'             MAX. NUMBER OF RECORDS                       
         DC    AL1(CBKCNT)         NUMBER OF BUCKETS                            
         DC    AL1(CLIBK-CLID)     DISP TO BUCKETS                              
         DC    AL2(0)                                                           
         DS    (2000*CLEN)C        TABLE                                        
         SPACE 1                                                                
CSUMBUFF DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(SLEN)           RECORD LENGTH                                
         DC    AL4(SKLEN)          DISP. TO KEY / KEY LENGTH                    
         DC    F'300'              MAX. NUMBER OF RECORDS                       
         DC    AL1(SBKCNT)         NUMBER OF BUCKETS                            
         DC    AL1(SBK-SUMD)       DISP TO BUCKETS                              
         DC    AL2(0)                                                           
         DS    (300*SLEN)C         TABLE                                        
         SPACE 1                                                                
CNMEBUFF DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(BCLEN)          RECORD LENGTH                                
         DC    AL4(BCKLEN)         DISP. TO KEY/ KEY LENGTH                     
         DC    F'2500'             MAX. IN TABLE                                
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP. TO BUCKETS                             
         DC    AL2(0)                                                           
         DS    (2600*BCLEN)C       TABLE                                        
         EJECT                                                                  
CCLSBUFF DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(CLSLEN)         RECORD LENGTH                                
         DC    AL4(CLSKLEN)        DISP. TO KEY/ KEY LENGTH                     
         DC    F'1000'             MAX. IN TABLE                                
         DC    AL1(CLSBKCNT)       NUMBER OF BUCKETS                            
         DC    AL1(CLSBK-CLSD)     DISP. TO BUCKETS                             
         DC    X'80'               BINARY                                       
         DC    AL1(0)                                                           
         DS    (1000*CLSLEN)C      TABLE                                        
         SPACE 1                                                                
CSORTC   DS    0D                                                               
         DS    41000C                                                           
         EJECT                                                                  
*              DSECTS FOR ACREPA202                                             
         SPACE 1                                                                
A202D    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
SQUASHER DS    V                                                                
UNDERLIN DS    V                                                                
PERVERT  DS    V                                                                
ACSLRY   DS    V                                                                
HELLO    DS    V                                                                
SORTER   DS    V                                                                
         DS    1F                  SPARE                                        
POSTIT   DS    V                   PUT WORKER RECORDS TO ACPOST                 
BUILDIT  DS    V                   BUILD SKELETON POSTING FILE                  
INDPOST  DS    V                   ALLOCATE INDIRECT COST                       
DISOVH   DS    V                   DISTRIBUTE OVERHEAD                          
BINADD   DS    V                   ADD ITEM TO BINSRCH TABLE                    
EXPENSE  DS    V                   GET EXPENSES FROM '1C'                       
DEPGRP   DS    V                   BUILD TABLE OF '14' AND '15' NAMES           
GRPMTCH  DS    V                   GET NAMES FOR DEPT. GROUPS                   
GETEL    DS    V                   GET AN ELEMENT                               
NMEPUT   DS    V                   ADD NAME TO BINSRCH TABLE                    
NMEGET   DS    V                   GET NAME FROM BINSRCH TABLE                  
REPORT   DS    V                   HANDLE REPORT PRINTING                       
HEADUP   DS    V                   HEADLINE ROUTINES                            
INDOVH   DS    V                   ADD DEPT O/H AND INDIRECT TO BUFFALO         
SRTP     DS    V                   SORT TRANSACTIONS                            
GETOVH   DS    V                   GET OVERHEAD AMOUNT                          
DIRPOST  DS    V                   POST DIRECT TIME                             
BUFPOST  DS    V                   POST BUFFALO RECORD                          
INDDPT   DS    V                   ALLOCATE FOR INDIRECT DPTS                   
         DS    1F                                                               
CLSBUFF  DS    A                                                                
SORTC    DS    A                                                                
GRPTAB   DS    A                                                                
RECORD   DS    A                                                                
POSTBUFF DS    A                                                                
CLIBUFF  DS    A                                                                
SUMBUFF  DS    A                                                                
NMEBUFF  DS    A                                                                
ADBUFC   DS    A                                                                
         DS    1F                  SPARE                                        
         DS    CL1                 END OF TABLE                                 
         SPACE 1                                                                
PROFKEY  DS    CL14                                                             
PROFPARA DS    3F                                                               
PROGPRF2 DS    CL16                                                             
         SPACE 1                                                                
OPTIONS  DS    0CL32               PROFILE OPTIONS                              
OPTEXP   DS    CL1        USE DIRECT EXPS FOR CORP OHEAD   Y,N                  
OPTNOTS  DS    CL1        INDIRECT LEVEL IF NO HOURS       D,O,C                
OPT45    DS    CL1        CORP. INDIRECT CONTRA LEDGER     4,5                  
OPTPERD  DS    CL1        RUN PERIOD-MONTHLY (OR QTR)      M,Q                  
OPTIND   DS    CL1        ADD INDIR TO DIRECT OR OHEAD     D,O                  
OPT54    DS    CL1        DEPT. O'HEAD CONTRA LEDGER       5,4                  
OPTYTD   DS    CL1        AGENCY IS YTD, MONTHLY OR QTR    Y,M,Q                
OPTSKIP  DS    CL1        NEW PAGE PER PERSON              Y,N                  
OPTC54   DS    CL1        CORP. OVERHEAD CONTRA LEDGER     4,5                  
OPTHRS   DS    CL1        STANDARD HOURS                   0-200                
OPTINDC  DS    CL1        CORP. GETS SHARE OF DEPT IND     Y,N                  
OPTOVH   DS    CL1        OVERHEAD LEVEL IF NO DIRECT TIME O,C                  
OPTOHC   DS    CL1        CORP. GETS SHARE OF DEPT OVHEAD  Y,N                  
INDACC   DS    CL1        ACCOUNT FOR INDIRECT TIME      0-9,A-Z                
OPTNBT   DS    CL1        INDIRECT LEVEL FOR  N.B. TIME    D,O,C                
OPTOFH   DS    CL1        OFFICE O'HEAD CONTRA LEDGER     4,5,7,D               
OPTA3    DS    0CL16      OPTIONS FROM A3 PROFILE                               
OPTEXH   DS    CL1        O'HEAD ACCOUNT FOR EXEC INDIRECT 0-9,A-Z              
OPTOVHY  DS    CL1        ALLOCATE OVERHEAD ON YTD BASIS                        
OPTNO1N  DS    CL1        IGNORE ALL 1N TIME                                    
OPTHNT   DS    CL1        PRINT HIRE AND/OR TERM DATES                          
         DS    CL12       SPARE                                                 
         SPACE 1                                                                
BUFWRK   DS    CL200                                                            
CLIWRK   DS    CL200                                                            
PERWRK   DS    CL200                                                            
TOTLINE  DS    CL200                                                            
CLSWRK   DS    CL50                                                             
         SPACE 1                                                                
DEPT99   DS    CL(SLEN)                                                         
DEPTOT   DS    CL(SLEN)                                                         
OFF99    DS    CL(SLEN)                                                         
OFFTOT   DS    CL(SLEN)                                                         
AGY99    DS    CL(SLEN)                                                         
AGYTOT   DS    CL(SLEN)                                                         
         SPACE 1                                                                
DEPCUM   DS    CL(DEPLEN)                                                       
OFFCUM   DS    CL(OFFLEN)                                                       
AGYCUM   DS    CL(AGYLEN)                                                       
         SPACE 1                                                                
LEVEL    DS    CL1                                                              
DEPT     EQU   1                                                                
OFFICE   EQU   2                                                                
COMP     EQU   3                                                                
LLEVA    DS    CL1                 COMBINED LEVEL LENGTHS                       
LLEVB    DS    CL1                                                              
LLEVC    DS    CL1                                                              
LLEVD    DS    CL1                                                              
LENLEVA  DS    CL1                                                              
LENLEVB  DS    CL1                                                              
LENLEVC  DS    CL1                                                              
LENLEVD  DS    CL1                                                              
         SPACE 1                                                                
OFFCODE  DS    CL2                 OFFICE CODE                                  
         SPACE 1                                                                
OVHLST   DS    PL8                 WORK AREAS FOR TOTAL OVERHEAD                
OVHYTD   DS    PL8                                                              
OVHPST   DS    PL8                                                              
OVALST   DS    PL8                 ACCOUNT OVERHEAD                             
OVAYTD   DS    PL8                                                              
OVAPST   DS    PL8                                                              
OVOLST   DS    PL8                 OTHER OVERHEAD                               
OVOYTD   DS    PL8                                                              
OVOPST   DS    PL8                                                              
OVACC    DS    PL8                                                              
OVTHR    DS    PL8                                                              
         SPACE 1                                                                
INDLST   DS    PL8                 WORK AREAS FOR INDIRECT                      
INDYTD   DS    PL8                                                              
INDPST   DS    PL8                                                              
         SPACE 1                                                                
DIRLST   DS    PL8                 WORK AREAS FOR DIRECT TIME                   
DIRYTD   DS    PL8                                                              
DIRPST   DS    PL8                                                              
         SPACE 1                                                                
NMEWRK   DS    CL60             WORK AREA FOR CLIENT CODE/NAME TABLE            
         SPACE 1                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
DUB3     DS    D                                                                
DUB4     DS    D                                                                
DUB5     DS    D                                                                
DUB6     DS    D                                                                
DIVWRK   DS    PL16                                                             
NET      DS    PL16                                                             
CLITIME  DS    PL8                                                              
ALLOC    DS    PL8                                                              
TOTIME   DS    PL8                                                              
POSTSW   DS    CL1                                                              
SVCOST   DS    PL8                                                              
         SPACE 1                                                                
ID       DS    CL16                                                             
MOS      DS    CL2                                                              
START    DS    CL3                                                              
END      DS    CL3                                                              
FISCAL   DS    CL3                                                              
LAST     DS    CL3                                                              
NUMONTHS DS    PL2                                                              
HEADMON  DS    CL6                                                              
STEND    DS    CL4                                                              
SALAREA  DS    CL100                                                            
         SPACE 1                                                                
POST     DS    CL1                                                              
POSTCASH DS    PL6                                                              
POSTREC  DS    PL6                                                              
PDEBITS  DS    PL6                                                              
PCREDITS DS    PL6                                                              
         SPACE 1                                                                
OHDACC   DS    CL1                                                              
IND      DS    CL1                                                              
INDSAVE  DS    CL15                                                             
INDSAVEN DS    CL36                                                             
GROUP    DS    CL1                                                              
SVGP     DS    CL2                                                              
MYMODE   DS    CL1                                                              
NBSW     DS    CL1                 NEW BUSINESS SWITCH                          
POSTERR  DS    CL1                                                              
         DS    CL28                SPARE                                        
         SPACE 1                                                                
SAVEKEY  DS    CL30                                                             
         SPACE 1                                                                
SALVAL   DS    CL10                                                             
SALPCT   DS    CL9                                                              
PCTBENEF DS    PL3                                                              
PCTADMIN DS    PL3                                                              
PCTTOT   DS    PL3                                                              
RATE     DS    PL6                                                              
PCTBENSV DS    PL3                 SAVED BENE PCT TO RESTORE                    
PCTADMSV DS    PL3                 SAVED ADMIN PCT TO RESTORE                   
         DS    CL30                SPARE                                        
         SPACE 1                                                                
INREC    DS    F                                                                
WANT     DS    CL1                                                              
         SPACE 1                                                                
SAVE50   DS    CL100                                                            
         DS    F                                                                
T        DS    CL600                                                            
         EJECT                                                                  
*              DSECT FOR PERSON/ACCOUNT  TOTALS                                 
PTOTD    DSECT                                                                  
PBK      EQU   *                                                                
PPERHRS  DS    PL8                 PERIOD HOURS                                 
PPERPHRS DS    PL8                 PERIOD PERSONAL HOURS                        
PYTDHRS  DS    PL8                 YTD HOURS                                    
PRSYTDH  DS    PL8                 YTD PERSONAL HOURS                           
PNONPER  DS    PL8                 YTD NON-PERS                                 
PCOST    DS    PL8                 COST  (SALARY + PERCENT)                     
PDIRLST  DS    PL8                 YTD-1 DIRECT COST                            
PINDLST  DS    PL8                 YTD-1 INDIRECT COST                          
PBENEF   DS    PL8                 BENEFIT AMOUNT                               
PADMIN   DS    PL8                 ADMINISTRATIVE AMOUNT                        
PBKCNT   EQU   (*-PBK)/8           NUMBER OF BUCKETS                            
         SPACE 2                                                                
*              DSECT FOR CLIENT CODE/NAME LIST                                  
         SPACE 1                                                                
BCLID    DSECT                                                                  
BCLICDE  DS    CL12                CLIENT CODE                                  
BCKLEN   EQU   *-BCLID                                                          
BCLINME  DS    CL36                CLIENT NAME                                  
BCLEN    EQU   *-BCLID                                                          
         SPACE 2                                                                
*              DSECT FOR A CLIENT DETAILS - WITHIN AN ACCOUNT                   
CLID     DSECT                                                                  
CLILEDG  DS    CL1                 LEDGER                                       
*                                  C=CLIENT TIME                                
*                                  N=NON CLIENT TIME                            
*                                  X'FF'=TOTAL                                  
CLICDE   DS    CL12                CLIENT CODE                                  
CLIKLEN  EQU   *-CLID              KEY LENGTH                                   
CLICLNME DS    CL36                CLIENT NAME                                  
CLIBK    EQU   *                                                                
CPERHRS  DS    PL8                 PERIOD HOURS                                 
CYTDHRS  DS    PL8                 YTD HOURS                                    
CADJHRS  DS    PL8                 ADJUSTED HOURS                               
         ORG   CPERHRS                                                          
CDIRLST  DS    PL8                 DIRECT TIME YTD-1 FOR O/H ACCOUNTS           
CDIRYTD  DS    PL8                 DIRECT TIME YTD                              
         DS    PL8                                                              
CCSTLST  DS    PL8                 COST YTD-1                                   
CCSTYTD  DS    PL8                 COST YTD                                     
CCSTPST  DS    PL8                 COST POSTING                                 
CCSTLSTM DS    PL8                 COST YTD-1 MEMO                              
CBKCNT   EQU   (*-CLIBK)/8         NUMBER OF BUCKETS                            
CLEN     EQU   *-CLID              RECORD LENGTH                                
         EJECT                                                                  
*              DSECT FOR DEPARTMENT ACCUMULATORS                                
DEPD     DSECT                                                                  
DEBK     EQU   *                                                                
DDINDCL  DS    PL8                 INDIRECT DEPT TO CORP.   YTD-1               
DDINDCY  DS    PL8                                          YTD                 
DDINDAL  DS    PL8                 DEPT. INDIRECT ABSORBED  YTD-1               
DDINDAY  DS    PL8                                          YTD                 
DDINDAP  DS    PL8                                          POST                
DOVHCL   DS    PL8                 DEPT. OVERHEAD TO CORP   YTD-1               
DOVHCY   DS    PL8                                          YTD                 
DOVHAL   DS    PL8                 DEPT. OVERHEAD ABSORBED  YTD-1               
DOVHAY   DS    PL8                                          YTD                 
DOVHAP   DS    PL8                                          POST                
DOVHOL   DS    PL8                 DEPT. OVERHEAD TO OFFICE YTD-1               
DOVHOY   DS    PL8                                          YTD                 
DEXTIML  DS    PL8                 DEPT EXEC TIME TO OFFICE YTD-1               
DEXTIMY  DS    PL8                                          YTD                 
DEXTIMP  DS    PL8                                          YTD                 
         DS    3PL8               SPARE                                         
DEBKCNT  EQU   (*-DEBK)/8                                                       
DEPLEN   EQU   *-DEPD                                                           
         SPACE 2                                                                
*              DSECT FOR OFFICE ACCUMULATORS                                    
OFFD     DSECT                                                                  
OFBK     EQU   *                                                                
OFDACCUM DS    (DEBKCNT)PL8        DEPT TOTALS (DEPD)                           
OFINDAL  DS    PL8                 OFFICE INDIRECT (ABSORBED) YTD-1             
OFINDAY  DS    PL8                                            YTD               
OFINDAP  DS    PL8                                            POST              
OFOVHCL  DS    PL8                 OFFCIE OVERHEAD TO CORP    YTD-1             
OFOVHCY  DS    PL8                                            YTD               
OFOVHAL  DS    PL8                 OFFICE OVERHEAD (ABSORBED) YTD-1             
OFOVHAY  DS    PL8                                            YTD               
OFOVHAP  DS    PL8                                            POST              
         DS    3PL8               SPARE                                         
OFBKCNT  EQU   (*-OFBK)/8                                                       
OFFLEN   EQU   *-OFFD                                                           
         SPACE 2                                                                
*              DSECT FOR AGENCY ACCUMULATORS                                    
AGYD     DSECT                                                                  
AGBK     EQU   *                                                                
AGOACCUM DS    (OFBKCNT)PL8        OFFICE TOTALS (OFFD)                         
AGINDAL  DS    PL8                 CORP. INDIRECT ABSORBED YTD-1                
AGINDAY  DS    PL8                                         YTD                  
AGINDAP  DS    PL8                                         POST                 
AGOVHAL  DS    PL8                 CORP OVERHEAD ABSORBED  YTD-1                
AGOVHAY  DS    PL8                                         YTD                  
AGOVHAP  DS    PL8                                         POST                 
         DS    3PL8                 SPARE                                       
AGBKCNT  EQU   (*-AGBK)/8                                                       
AGYLEN   EQU   *-AGYD                                                           
         EJECT                                                                  
*              DSECT FOR OFFICE/DEPT SUMMARY RECORDS                            
SUMD     DSECT                                                                  
SOFFDEP  DS    CL12                OFFICE CODE X'FF' = AGENCY TOTALS            
*                                  DEPT. CODE X'FFFF' = OFFICE TOTALS           
SKLEN    EQU   *-SUMD                                                           
SBK      EQU   *                                                                
SDIRPST  DS    PL8                 DIRECT POSTING                               
SDINDPST DS    PL8                 DEPT. INDIRECT POSTING                       
SOINDPST DS    PL8                 OFFICE INDIRECT POSTING                      
SCINDPST DS    PL8                 CORP. INDIRECT POSTING                       
STTIME   DS    PL8                 TOTAL TIME                                   
SOVHPST  DS    PL8                 OVERHEAD POSTING                             
SOVOPST  DS    PL8                 OTHER OVERHEAD                               
STCOST   DS    PL8                 TOTAL COST                                   
SDIRLST  DS    PL8                 DIRECT YTD-1                                 
SDIRYTD  DS    PL8                 DIRECT YTD                                   
SDINDLST DS    PL8                 DEPT. INDIRECT YTD-1                         
SDINDYTD DS    PL8                 DEPT. INDIRECT YTD                           
SOINDLST DS    PL8                 OFFICE INDIRECT YTD-1                        
SOINDYTD DS    PL8                 OFFICE INDIRECT YTD                          
SCINDLST DS    PL8                 CORP. INDIRECT YTD-1                         
SCINDYTD DS    PL8                 CORP. INDIRECT YTD                           
SOVHLST  DS    PL8                 OVERHEAD YTD-1                               
SOVHYTD  DS    PL8                 OVERHEAD YTD                                 
SOVOLST  DS    PL8                 OTHER OVERHEAD YTD-1                         
SOVOYTD  DS    PL8                 OTHER OVERHEAD YTD                           
         DS    3PL8                SPARE                                        
SBKCNT   EQU   (*-SBK)/8           NUMBER OF BUCKETS                            
SLEN     EQU   *-SUMD              LENGTH                                       
         EJECT                                                                  
*              DSECT FOR BUFFALO RECORDS                                        
*                                  LEVEL 1=DEPT, 2=OFFICE, 3=COMPANY            
BUFD     DSECT                                                                  
BUFKEY   DS    0C                                                               
BUFTYPE  DS    CL1                                                              
BUFBYGP  EQU   1                   CLIENTS BY GROUP                             
BUFNOGP  EQU   2                   CLIENTS NO GROUP                             
BUFNEWB  EQU   3                   NEW BUSINESS CLIENTS                         
BUFCLI   DS    CL13                CLIENT                                       
BUFTOT   EQU   0                   TOTAL RECORD =X'00'                          
BUFGRP   DS    CL1                 GROUP FOR DIRECT  DEPT/CORP =X'00'           
BUFEXPN  EQU   X'5C'                                                            
BUFIND   EQU   X'5D'                                                            
BUFOVH   EQU   X'5E'                                                            
BUFKLEN  EQU   *-BUFKEY            KEY LENGTH                                   
BUFBK    EQU   *                                                                
BDIRLST  DS    PL8                 DIRECT COST YTD-1                            
BDIRYTD  DS    PL8                 DIRECT COST YTD                              
BDIRPST  DS    PL8                 DIRECT COST POSTING                          
BDIRLSTM DS    PL8                 DIRECT COST YTD-1 MEMO                       
BEXPLST  DS    PL8                 EXPENSES YTD-1                               
BEXPYTD  DS    PL8                 EXPENSES YTD                                 
         ORG   BEXPLST                                                          
BINDLST  DS    PL8                 INDIRECT YTD-1                               
BINDYTD  DS    PL8                 INDIRECT YTD                                 
         ORG   BEXPLST                                                          
BOVHLST  DS    PL8                 OVERHEAD YTD-1                               
BOVHYTD  DS    PL8                 OVERHEAD YTD                                 
BBKCNT   EQU   (*-BUFBK)/8         NUMBER OF BUCKETS                            
BLEN     EQU   *-BUFD                                                           
         EJECT                                                                  
*              DSECT FOR CLIENT SUMMARY RECORDS                                 
         SPACE 1                                                                
CLSD     DSECT                                                                  
CLSCDE   DS    CL13                CLIENT CODE                                  
CLSKLEN  EQU   *-CLSD                                                           
         DS    CL3                 SPARE                                        
CLSBK    EQU   *                                                                
CLSINLST DS    F                   INDIRECT YTD-1                               
CLSINYTD DS    F                            YTD                                 
CLSOVLST DS    F                   OVERHEAD YTD-1                               
CLSOVYTD DS    F                            YTD                                 
CLSBKCNT EQU   (*-CLSBK)/4         NUMBER OF BUCKETS                            
CLSLEN   EQU   *-CLSD                                                           
         EJECT                                                                  
*              DSECT FOR THE BINSRCH LIST                                       
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISP. TO FIRST BUCKET                        
BINSTAT  DS    CL1                 X'80' BINARY DATA                            
         DS    CL1                 SPARE                                        
BINTABLE DS    0CL1                                                             
         SPACE 2                                                                
*              DSECT FOR SORTED POSTING FILE                                    
SRTD     DSECT                                                                  
SRTLEN   DS    F                                                                
SRTKEY   DS    0CL36                                                            
SRTKTYP  DS    CL1                 1=DR, 2=CR, 3=TOTAL RECORD                   
SRTKACC  DS    CL15                ACCOUNT                                      
SRTKCON  DS    CL15                CONTRA                                       
         DS    CL5                 SPACE                                        
SRTREC   DS    0C                  THE POSTING RECORD                           
         SPACE 2                                                                
*              DSECT FOR SUMMARY TABLES                                         
TABD     DSECT                                                                  
TABNME   DS    CL36                CATEGORY NAME                                
TABADD   DS    F                   DISP TO TABLE                                
TABSTAT  DS    CL1                 STATUS                                       
TABBKS   DS    CL3                                                              
TABLEN   EQU   *-TABNME                                                         
         EJECT                                                                  
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        ACGENPOST                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        DDLOGOD                                                                
*        DDREPXTRAD                                                             
*        DDREPMASTD                                                             
*        DDREMOTED                                                              
*        DDSLRD                                                                 
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDSLRD                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'071ACREPA202 05/01/02'                                      
         END                                                                    
