*          DATA SET ACREP3D02  AT LEVEL 019 AS OF 06/03/15                      
*PHASE AC3D02A,*                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE SORTER                                                                 
*INCLUDE PROLLER                                                                
*INCLUDE CHOPPER                                                                
         TITLE 'SALES ANALYSIS REPORTS'                                         
*--------------------------------------------------------------------*          
*--------------------------------------------------------------------*          
*   POSSIBLE SORT OPTIONS ARE:                                       *          
*      SJ/SI - SORTOPT=1, (QOPT1=C,QOPT2=I)                          *          
*      SI/SJ - SORTOPT=2, (QOPT1=I,QOPT2=C)                          *          
*      1C/SI - SORTOPT=3, (QOPT1=P,QOPT2=I)                          *          
*      SI/1C - SORTOPT=4, (QOPT1=I,QOPT2=P)                          *          
*                                                                    *          
*    PRIMARY SORTKEY ALWAYS 12 CHARACTERS, SECONDARY SORTKEY         *          
*        ALWAYS 12 CHARACTERS.                                       *          
*    FOR SI AND 1C SORTKEY IS ACCOUNT CODE.                          *          
*    FOR SJ SORTKEY IS CLIENT (6 CHARS LEFT JUSTIFIED), THEN         *          
*        PRODUCT (6 CHARS LEFT JUSTIFIED).                           *          
*    ONE SORTREC MARKING EVERY LEVEL BREAK GOES OUT BEFORE ALL       *          
*    TRANSACTIONS (FROM HIGHEST LEVEL TO LOWEST LEVEL).              *          
*    ONE MARKING EVERY LEVEL BREAK GOES OUT AFTER TRANSACTIONS       *          
*    (FROM LOWEST LEVEL TO HIGHEST LEVEL).                           *          
*    TRAILER SORTREC KEYS ARE PADDED WITH X'FF'S TO FORCE            *          
*    CORRECT ORDER FROM LOWEST LEVEL TO HIGHEST LEVEL.               *          
*                                                                    *          
*    BEWARE OF USING R6, MOST OF THE PROGRAM ASSUMES IT IS           *          
*    POINTING TO THE SORTREC YOU ARE CURRENTLY PROCESSING.           *          
*    R5 IS USED FOR WIDE PRINTING                                    *          
*--------------------------------------------------------------------*          
*--------------------------------------------------------------------*          
*                                                                               
AC3D02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC3D**,R9,R8,R7                                              
         L     RA,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RA                                                       
*                                                                               
         LA    RC,SPACEND                                                       
         L     R5,VBIGPRNT                                                      
*                                                                               
         USING AC3D02D,RC                                                       
         USING BIGPRNTD,R5                                                      
*                                                                               
         EJECT ,                                                                
*                                                                               
*              RUNFRST                                                          
         CLI   MODE,RUNFRST                                                     
         BNE   REQSTFR                                                          
         L     RF,=A(BUFFALOC)                                                  
         ST    RF,ABUFF                                                         
         L     RF,=V(PROLLER)                                                   
         ST    RF,PROLLER                                                       
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(,R2)                                                        
         L     R2,VEXTRAS                                                       
*                                                                               
         USING RUNXTRAD,R2                                                      
*                                                                               
         L     R2,ADMASTD                                                       
*                                                                               
         USING MASTD,R2                                                         
*                                                                               
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         EJECT ,                                                                
*                                                                               
*              REQFRST                                                          
REQSTFR  CLI   MODE,REQFRST                                                     
         BNE   LEDGF                                                            
         BAS   RE,SETSORT                                                       
         GOTO1 PROLLER,DMCB,0,ACCUMS,11,10                                      
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         NI    QOPT4,X'0F'              CHANGE CHAR NUMS TO HEX                 
         NI    QOPT5,X'0F'              DEFAULT TO 1-1 SUMMARY                  
         CLI   QOPT4,0                                                          
         BNE   *+8                                                              
         MVI   QOPT4,1                                                          
         CLI   QOPT5,0                                                          
         BNE   *+8                                                              
         MVI   QOPT5,1                                                          
REQF03   CLC   PROGPROF(9),=9XL1'00'                                            
         BNE   REQF04                                                           
         MVC   PROGPROF(9),=X'010203000000000400'                               
         MVC   PROGPROF+9(4),=C'ABNN'                                           
REQF04   MVC   PCTCOL,PROGPROF+7        NEED TO FLAG PCT COL (NOT               
*                                        REPORTED AS A TOTAL)                   
         MVI   SORTOPT,1                POSSIBLE COMBS OF OPT 1,2 ARE:          
         CLI   QOPT1,C'C'                CI - IC - PI - IP                      
         BE    REQF05                   IN ONE BYTE MAKE CODES:                 
         MVI   SORTOPT,2                 1 -  2 -  3 -  4                       
         CLI   QOPT2,C'C'                                                       
         BE    REQF05                                                           
         MVI   SORTOPT,3                                                        
         CLI   QOPT1,C'P'                                                       
         BE    REQF05                                                           
         MVI   SORTOPT,4                                                        
*                                                                               
*              CLEAR ALL SORTREC SPACES                                         
REQF05   LA    R1,SORTREC1                                                      
         LA    R2,9                                                             
         MVC   0(L'SORTREC1,R1),SPACES                                          
         LA    R1,L'SORTREC1(,R1)                                               
         BCT   R2,*-10                                                          
*                                                                               
         MVI   SORTREC1+(SRTTYP-SORTRECD),C'A'    MOVE TYPE INDICATORS          
         MVI   SORTREC2+(SRTTYP-SORTRECD),C'B'    INTO SORTRECS                 
         MVI   SORTREC3+(SRTTYP-SORTRECD),C'C'                                  
         MVI   SORTREC4+(SRTTYP-SORTRECD),C'D'                                  
         MVI   SORTLEVA+(SRTTYP-SORTRECD),C'1'                                  
         MVI   SORTLEVB+(SRTTYP-SORTRECD),C'2'                                  
         MVI   SORTLEVC+(SRTTYP-SORTRECD),C'3'                                  
         MVI   SORTLEVD+(SRTTYP-SORTRECD),C'4'                                  
         MVI   SORTTRAN+(SRTTYP-SORTRECD),C'T'                                  
*                                                                               
*              SET APPROPRIATE SPROG                                            
         LA    R3,0                                                             
         CLI   SORTOPT,1                    SJ/SI                               
         BE    REQF10                                                           
         LA    R3,2(,R3)                                                        
         CLI   SORTOPT,2                    SI/SJ                               
         BE    REQF10                                                           
         LA    R3,2(,R3)                                                        
         CLI   SORTOPT,3                    1C/SI                               
         BE    REQF10                                                           
*                                                                               
         LA    R3,2(,R3)                    SI/1C                               
         MVI   SORTREC1+(SRTTYP-SORTRECD),C'1'                                  
         MVI   SORTREC2+(SRTTYP-SORTRECD),C'2'                                  
         MVI   SORTREC3+(SRTTYP-SORTRECD),C'3'                                  
         MVI   SORTREC4+(SRTTYP-SORTRECD),C'4'                                  
         MVI   SORTLEVA+(SRTTYP-SORTRECD),C'A'                                  
         MVI   SORTLEVB+(SRTTYP-SORTRECD),C'B'                                  
         MVI   SORTLEVC+(SRTTYP-SORTRECD),C'C'                                  
         MVI   SORTLEVD+(SRTTYP-SORTRECD),C'D'                                  
REQF10   STC   R3,RCSUBPRG                                                      
*                                                                               
*              SET UP COLUMN HEADINGS ACCORDING TO PROFILE                      
         BAS   RE,COLHEADS                                                      
*              READ SJ FOR CLIENT AND PRODUCT LEVEL LENGTHS                     
REQF30   DS    0H                                                               
         MVC   SAVEKEY,SPACES                                                   
         MVC   SAVEKEY(15),KEY                                                  
         L     R4,=A(ACCBUFF)                                                   
         MVC   0(42,R4),SPACES                                                  
         MVC   0(1,R4),QCOMPANY                                                 
         MVC   1(2,R4),=C'SJ'                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         MVI   BYTE,X'14'                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACLEDGD,R4                                                       
*                                                                               
         MVC   COSTCLIL,ACLTCLI            DISPLACEMENT OF CLIENT               
*                                          IF ZERO IS NOT SET UP                
         MVI   BYTE,X'16'                                                       
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACHEIRD,R4                                                       
*                                                                               
         ZIC   R1,ACHRLEVA                 CLI AND PRD LENGTHS FROM             
         STC   R1,CLILEN                   LEVEL ELEMS                          
         ZIC   R2,ACHRLEVB                                                      
         SR    R2,R1                                                            
         STC   R2,PRODLEN                                                       
         MVC   SVCLILEN,CLILEN                                                  
*                                                                               
REQF70   CLI   SORTOPT,3                   ONLY NEED TO READ 1C IF              
         BE    REQF70A                     SORTING BY COSTING ACCT              
         CLI   SORTOPT,4                   ONLY NEED TO READ 1C IF              
         BNE   REQF175                     SORTING BY COSTING ACCT              
REQF70A  MVC   0(42,R4),SPACES                                                  
         MVC   0(1,R4),QCOMPANY                                                 
         MVC   1(2,R4),=C'1C'                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         MVI   BYTE,X'16'                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACHEIRD,R4                    LEVELS AND LEVEL NAMES             
*                                                                               
         MVC   CSTTOTA,ACHRLEVA              FOR UNIT/LEDG 1C                   
         MVC   CSTTOTB,ACHRLEVB                                                 
         MVC   CSTTOTC,ACHRLEVC                                                 
         MVC   CSTTOTD,ACHRLEVD                                                 
         MVC   ACSTNAM,ACHRDESA                                                 
         MVC   BCSTNAM,ACHRDESB                                                 
         MVC   CCSTNAM,ACHRDESC                                                 
         MVC   DCSTNAM,ACHRDESD                                                 
         MVC   SVHEAD4(15),ACSTNAM                                              
*                                                                               
         CLI   ACHRLEVB,0                   CALC INDIVIDUAL LENGTH OF           
         BE    REQF90                       EACH LEVEL                          
         ZIC   R2,ACHRLEVB                                                      
         ZIC   R3,ACHRLEVA                                                      
         SR    R2,R3                                                            
         STC   R2,CSTLEVB                                                       
*                                                                               
         CLI   ACHRLEVC,0                                                       
         BE    REQF90                                                           
         ZIC   R2,ACHRLEVC                                                      
         ZIC   R3,ACHRLEVB                                                      
         SR    R2,R3                                                            
         STC   R2,CSTLEVC                                                       
*                                                                               
         CLI   ACHRLEVD,0                                                       
         BE    REQF90                                                           
         ZIC   R2,ACHRLEVD                                                      
         ZIC   R3,ACHRLEVC                                                      
         SR    R2,R3                                                            
         STC   R2,CSTLEVD                                                       
*                                                                               
REQF90   ZIC   R1,COSTCLIL                1C CLIENT DISPLACEMENT                
         LTR   R1,R1                      IF ZERO NO CLIENT DISPL               
         BZ    REQF175                    SPECIFIED                             
         BCTR  R1,0                                                             
         LTR   R1,R1                      IF ZERO MEANS FIRST LEVEL             
         BNZ   REQF100                                                          
         MVI   COSTCLIL,1                                                       
         B     REQF175                                                          
REQF100  DS    0H                                                               
         ZIC   R2,CSTTOTA                 FROM CLIENT DISPLACEMENT              
         CR    R2,R1                      COMPARE WITH CLIENT LEVELS            
         BNE   REQF120                    AND GET LEVEL NUMBER OF               
         MVI   COSTCLIL,2                 CLIENT INTO 1C LEDGER SETUP           
         B     REQF175                                                          
*                                                                               
REQF120  ZIC   R2,CSTTOTB                                                       
         CR    R2,R1                                                            
         BNE   REQF130                                                          
         MVI   COSTCLIL,3                                                       
         B     REQF175                                                          
*                                                                               
REQF130  ZIC   R2,CSTTOTC                                                       
         CR    R2,R1                                                            
         BNE   REQF175                                                          
         MVI   COSTCLIL,4                                                       
         B     REQF175                                                          
*                                                                               
REQF175  L     R4,=A(ACCBUFF)                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',SAVEKEY,(R4)                 
         B     CIAXIT                                                           
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*              TOTAL AND INDIVIDUAL LENGTHS OF SI                    *          
*--------------------------------------------------------------------*          
LEDGF    CLI   MODE,LEDGFRST                                                    
         BNE   PRLEVA                                                           
         L     R4,ADLDGHIR                                                      
*                                                                               
         USING ACHEIRD,R4                                                       
*                                                                               
         MVC   ALEVLEN,ACHRLEVA             STORE TOTAL LENGTHS                 
         MVC   BLEVTOT,ACHRLEVB                                                 
         MVC   CLEVTOT,ACHRLEVC                                                 
         MVC   DLEVTOT,ACHRLEVD                                                 
*                                                                               
         CLI   ACHRLEVB,0                   CALC INDIVIDUAL LENGTH OF           
         BE    CIAXIT                       EACH LEVEL                          
         ZIC   R2,ACHRLEVB                                                      
         ZIC   R3,ACHRLEVA                                                      
         SR    R2,R3                                                            
         STC   R2,BLEVLEN                                                       
*                                                                               
         CLI   ACHRLEVC,0                                                       
         BE    CIAXIT                                                           
         ZIC   R2,ACHRLEVC                                                      
         ZIC   R3,ACHRLEVB                                                      
         SR    R2,R3                                                            
         STC   R2,CLEVLEN                                                       
*                                                                               
         CLI   ACHRLEVD,0                                                       
         BE    CIAXIT                                                           
         ZIC   R2,ACHRLEVD                                                      
         ZIC   R3,ACHRLEVC                                                      
         SR    R2,R3                                                            
         STC   R2,DLEVLEN                                                       
*                                                                               
         B     CIAXIT                                                           
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*   PROPOGATE LEVEL A OF SI DOWN APPROPRIATE SRTRCS FOR ALL SORTS    *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
         USING SORTRECD,R6                                                      
         SPACE 1                                                                
PRLEVA   CLI   MODE,PROCLEVA                                                    
         BNE   PRLEVB                                                           
         L     R4,ADHEIRA                                                       
*                                                                               
         CLI   SORTOPT,4                      FOR SI/1C SORT                    
         BNE   PRLA50                         SI IS PRIMARY SORT                
         LA    R6,SORTREC1                    PROPOGATE SI ACC DOWN             
         LA    R2,9                           ALL SORTRECS                      
*                                                                               
PRLA10   MVC   0(L'SRTKEY1,R6),3(R4)                                            
         LA    R6,L'SORTREC(,R6)                                                
         BCT   R2,PRLA10                                                        
         LA    R6,SORTREC1                                                      
         B     PRLA80                                                           
*                                                                               
PRLA50   LA    R6,SORTLEVA                    SI IS SECNDARY SORT               
         LA    R3,SRTKEY2-SORTKEY(,R6)        POINT TO SECONDARY KEY            
         CLI   SORTOPT,2                                                        
         BNE   *+8                            PRIMARY SORT FOR SI/SJ            
         LA    R3,SRTKEY1-SORTKEY(,R6)        POINT TO PRIMARY KEY              
         MVC   0(L'SRTKEY2,R3),3(R4)          ACCT KEY TO SORT REC              
         BAS   RE,REKEY                       PROPOGATE KEY                     
*                                                                               
         USING ACNAMED,R4                                                       
*                                                                               
PRLA80   L     R4,ADLVANAM                                                      
         MVC   SRTDATA,SPACES                                                   
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'03'                                                        
         EXMVC R1,SRTDATA,ACNMNAME            LEV A NAME TO SORTREC             
         MVC   SAVACCT,0(R3)                  LEV AND LOWEST ACC CODE           
         MVI   LOWSI,1                        KEEP TRACK OF LOWEST SI           
         B     CIAXIT                                                           
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*   PROPOGATE LEVEL B OF SI DOWN APPROPRIATE SRTRCS FOR ALL SORTS    *          
*--------------------------------------------------------------------*          
PRLEVB   CLI   MODE,PROCLEVB                                                    
         BNE   PRLEVC                                                           
         L     R4,ADHEIRB                                                       
*                                                                               
         CLI   SORTOPT,4                      FOR SI/1C SORT                    
         BNE   PRLB50                         SI IS PRIMARY SORT                
         LA    R6,SORTREC2                    PROPOGATE SI ACC DOWN             
         LA    R2,8                           ALL SORTRECS                      
*                                                                               
PRLB10   MVC   0(L'SRTKEY1,R6),3(R4)                                            
         LA    R6,L'SORTREC(,R6)                                                
         BCT   R2,PRLB10                                                        
         LA    R6,SORTREC2                                                      
         B     PRLB80                                                           
*                                                                               
PRLB50   LA    R6,SORTLEVB                    SI IS SECONDARY SORT              
         LA    R3,SRTKEY2-SORTKEY(,R6)        POINT TO SECONDARY KEY            
         CLI   SORTOPT,2                                                        
         BNE   *+8                            PRIMARY SORT FOR SI/SJ            
         LA    R3,SRTKEY1-SORTKEY(,R6)        POINT TO PRIMARY KEY              
         MVC   0(L'SRTKEY2,R3),3(R4)          ACCT KEY TO SORT REC              
         BAS   RE,REKEY                       PROPOGATE KEY                     
*                                                                               
         USING ACNAMED,R4                                                       
*                                                                               
PRLB80   L     R4,ADLVBNAM                                                      
         MVC   SRTDATA,SPACES                                                   
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'03'                                                        
         EXMVC R1,SRTDATA,ACNMNAME            LEV B NAME TO SRTDATA             
         MVC   SAVACCT,0(R3)                  KEEP LOWEST LEV ACC CODE          
         MVI   LOWSI,2                        KEEP TRACK OF LOWEST SI           
         B     CIAXIT                                                           
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*   PROPOGATE LEVEL C OF SI DOWN APPROPRIATE SRTRCS FOR ALL SORTS    *          
*--------------------------------------------------------------------*          
PRLEVC   CLI   MODE,PROCLEVC                                                    
         BNE   PRLEVD                                                           
         L     R4,ADHEIRC                                                       
*                                                                               
         CLI   SORTOPT,4                      FOR SI/1C SORT                    
         BNE   PRLC50                         SI IS PRIMARY SORT                
         LA    R6,SORTREC3                    PROPOGATE SI ACC DOWN ALL         
         LA    R2,7                           SORTRECS                          
*                                                                               
PRLC10   MVC   0(L'SRTKEY1,R6),3(R4)                                            
         LA    R6,L'SORTREC(,R6)                                                
         BCT   R2,PRLC10                                                        
         LA    R6,SORTREC3                                                      
         B     PRLC80                                                           
*                                                                               
PRLC50   LA    R6,SORTLEVC                    SI IS SECONDARY SORT              
         LA    R3,SRTKEY2-SORTKEY(,R6)        POINT TO SECONDARY KEY            
         CLI   SORTOPT,2                                                        
         BNE   *+8                            PRIMARY SORT FOR SI/SJ            
         LA    R3,SRTKEY1-SORTKEY(,R6)        POINT TO PRIMARY KEY              
         MVC   0(L'SRTKEY2,R3),3(R4)          ACCT KEY TO SORT REC              
         BAS   RE,REKEY                       PROPOGATE KEY                     
*                                                                               
         USING ACNAMED,R4                                                       
*                                                                               
PRLC80   L     R4,ADLVCNAM                                                      
         MVC   SRTDATA,SPACES                CLEAR ACCUMULATORS                 
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'03'                                                        
         EXMVC R1,SRTDATA,ACNMNAME            LEV C NAME TO SRTDATA             
         MVI   LOWSI,3                                                          
         MVC   SAVACCT,0(R3)                                                    
         B     CIAXIT                                                           
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*   PROPOGATE LEVEL D OF SI DOWN APPROPRIATE SRTRCS FOR ALL SORTS    *          
*--------------------------------------------------------------------*          
PRLEVD   CLI   MODE,PROCLEVD                                                    
         BNE   SBACF100                                                         
         L     R4,ADHEIRD                                                       
*                                                                               
         CLI   SORTOPT,4                      FOR SI/1C SORT                    
         BNE   PRLD50                         SI IS PRIMARY SORT                
         LA    R6,SORTREC4                    PROPOGATE SI ACC DOWN             
         LA    R2,6                           ALL SORTRECS                      
*                                                                               
PRLD10   MVC   0(L'SRTKEY1,R6),3(R4)                                            
         LA    R6,L'SORTREC(,R6)                                                
         BCT   R2,PRLD10                                                        
         LA    R6,SORTREC4                                                      
         B     PRLD80                                                           
*                                                                               
PRLD50   LA    R6,SORTLEVD                    SI IS SECONDARY SORT              
         LA    R3,SRTKEY2-SORTKEY(,R6)        POINT TO SECONDARY KEY            
         CLI   SORTOPT,2                                                        
         BNE   *+8                            PRIMARY SORT FOR SI/SJ            
         LA    R3,SRTKEY1-SORTKEY(,R6)        POINT TO PRIMARY KEY              
         MVC   0(L'SRTKEY2,R3),3(R4)          ACCT KEY TO SORT REC              
         BAS   RE,REKEY                       PROPOGATE KEY                     
*                                                                               
         USING ACNAMED,R4                                                       
*                                                                               
PRLD80   L     R4,ADLVDNAM                                                      
         MVC   SRTDATA,SPACES                CLEAR DATA SPACE                   
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'03'                                                        
         EXMVC R1,SRTDATA,ACNMNAME            LEV D NAME TO SRTDATA             
         MVI   LOWSI,4                                                          
         MVC   SAVACCT,0(R3)                                                    
         B     CIAXIT                                                           
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*   PROCESS NEW CLIENT AND PRODUCT                                   *          
*--------------------------------------------------------------------*          
SBACF100 CLI   MODE,SBACFRST                                                    
         BNE   PRCT100                                                          
         MVI   READTRNS,C'Y'                                                    
*                                                                               
         L     R4,ADSUBAC                                                       
         LA    R4,TRSBACNT-TRSUBHD(,R4)   POINT TO SUB ACCT NUM                 
**T                                                                             
         CLC   1(2,R4),=C'SR'             CHECK UNIT AND LEDGER                 
         BE    SBF200                     CAN'T SEE SR'S WHEN                   
**T                                                                             
SBF100   CLC   1(2,R4),=C'SJ'             CHECK UNIT AND LEDGER                 
         BE    SBF200                                                           
         CLC   1(2,R4),=C'SM'                                                   
         BNE   SBF230                                                           
*                                                                               
SBF200   DS    0H                                                               
         LA    R6,SORTREC1                                                      
         LA    R3,SCLIKEY-SORTKEY(,R6)    CLIENT IS PRIMARY SORT EXCEPT         
         CLI   SORTOPT,2                  SI/SJ(2) IS SECONDARY                 
         BNE   *+8                                                              
         LA    R3,SCLIKEY2-SORTKEY(,R6)                                         
*                                                                               
         CLC   QCUL,=C'SJ'                WAS CLIENT FILTER REQUESTED           
         BNE   SBF240                                                           
         CLC   QCACCT(3),3(R4)            CHECK CLIENT FILTER                   
         BNE   SBF230                                                           
         CLC   QCACCT+3(3),SPACES         WAS PROD FILTER ALSO REQSTD           
         BE    SBF240                                                           
         CLC   QCACCT+3(3),6(R4)          CHECK PRODUCT FILTER                  
         BE    SBF240                                                           
*                                                                               
SBF230   MVI   READTRNS,C'N'              IF IT FAILED ALL TESTS MARK           
         B     CIAXIT                     DO NOT USE AND EXIT                   
*                                                                               
SBF240   DS    0H                                                               
         CLI   SORTOPT,3                  FOR 1C/SI(3), SI/1C(4) NEED           
         BE    COSTPRC                    TO GO READ 1C INSTEAD OF              
         CLI   SORTOPT,4                  PROCESSING CLI,PRD                    
         BE    COSTPRC                                                          
*                                                                               
         ZIC   R1,CLILEN                  SEE IF STILL ON SAME CLIENT           
         BCTR  R1,0                                                             
         MVI   BYTE,0                                                           
         EXCLC R1,0(R3),3(R4)                                                   
         BE    SBF300                     SAME CLIENT                           
*                                                                               
SBF270   MVC   0(L'SCLIKEY,R3),SPACES     ELSE MOVE CLIENT TO SORTREC           
         EXMVC R1,0(R3),3(R4)                                                   
         MVI   LEV1OK,X'80'               MARK SORTREC TO BE SENT               
*                                                                               
         CLI   SORTOPT,2                                                        
         BNE   SBF290                     IF SI/SJ SORT CLIENT IS SECN          
         LA    R6,SORTREC2                SORT,FINISH PROPOGATION OF            
         MVC   SCLIKEY2,0(R3)             CLIENT CODE TO PROD AND TRANS         
         LA    R6,SORTTRAN                SORTRECS.                             
         MVC   SCLIKEY2,0(R3)                                                   
         B     *+8                                                              
*                                                                               
SBF290   BAS   RE,REKEY                                                         
         MVI   BYTE,1                                                           
*                                                                               
SBF300   LA    R6,SORTREC2                 PRODUCT RECORD                       
         LA    R3,L'SORTREC+L'SCLIKEY(,R3) BUMP ONE SORTREC+L'CLIKEY            
         ZIC   R2,PRODLEN                                                       
         BCTR  R2,0                                                             
         LA    R4,4(R1,R4)                 POINT TO START OF PROD KEY           
         EXCLC R2,SPACES,0(R4)                                                  
         BNE   SBF350                                                           
         MVC   0(L'SPRDKEY,R3),=CL6'ALL'                                        
         B     SBF420                                                           
*                                                                               
SBF350   OC    BYTE,BYTE                                                        
         BNZ   SBF400                      NEW CLI, MUST BE NEW PRD             
         EXCLC R2,0(R3),0(R4)              SPRDKEY                              
         BE    SBF500                      SAME PRODUCT                         
*                                                                               
SBF400   MVC   0(L'SPRDKEY,R3),SPACES                                           
         EXMVC R2,0(R3),0(R4)              SPRDKEY                              
*                                                                               
SBF420   DS    0H                                                               
         CLI   SORTOPT,2              FOR SISJ PROCESSING DON'T REKEY           
         BNE   SBF450                 PROD CODE WILL ONLY BE IN                 
         LA    R6,SORTTRAN            PROD AND TRAN SORTREC                     
         EXMVC R2,SPRDKEY2,0(R4)                                                
         LA    R6,SORTREC2            RESET POINTING TO PROD SORTREC            
         B     *+8                                                              
*                                                                               
SBF450   BAS   RE,REKEY                                                         
         L     R4,ADSUBAC                                                       
*                                                                               
         USING TRSUBHD,R4                                                       
*                                                                               
         MVC   SRTDATA,SPACES                                                   
         ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         LTR   R1,R1                                                            
         BM    SBF500                                                           
         EXMVC R1,SRTDATA,TRSBNAME            SUBACCOUNT NAME                   
*                                                                               
SBF500   DS    0H                                                               
         MVI   LEV2OK,X'80'                                                     
         MVI   LEVAOK,X'80'        INCOME AC RECORD'S KEY HAS CHANGED           
         MVI   LEVBOK,X'80'                                                     
         MVI   LEVCOK,X'80'                                                     
         MVI   LEVDOK,X'80'                                                     
         B     CIAXIT                                                           
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*        IF SORTING USING 1C AS EITHER PRIMARY OR SECONDARY SORT                
*        NEED TO READ SJ PROD/CLT RECORDS TO FIND COSTING CODE                  
*        THIS CLIENT/PROD IS LINKED TO                                          
*-------------------------------------------------------------------*           
COSTPRC  DS    0H                                                               
**T                                                                             
         CLC   1(2,R4),=C'SR'                                                   
         BNE   CP50                                                             
         MVI   READTRNS,C'N'                                                    
         B     CIAXIT                                                           
**T                                                                             
CP50     MVC   SAVEKEY,SPACES           SAVE CURRENT KEY TO LATER               
         MVC   SAVEKEY(42),KEY          RESTORE READ                            
         L     R2,ADSUBAC               READ PROD FIRST, IF NO X'24' EL         
         L     R4,=A(ACCBUFF)           THEN READ FOR X'24' AT CLI LEV          
         MVC   0(42,R4),SPACES                                                  
         MVC   0(1,R4),QCOMPANY                                                 
         MVC   1(2,R4),=C'SJ'                                                   
         MVC   3(12,R4),5(R2)                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         MVI   BYTE,X'24'                                                       
         BAS   RE,GETEL                                                         
         BNE   CP100                                                            
*                                                                               
         USING ACPROFD,R4                                                       
*                                                                               
         CLC   ACPRCOST+3(12),SPACES       COSTING CODE                         
         BH    CP200                                                            
*                                                                               
CP100    L     R4,=A(ACCBUFF)           THEN READ FOR X'24' AT CLI LEV          
         L     R2,ADSUBAC               READ PROD FIRST, IF NO X'24' EL         
         MVC   0(42,R4),SPACES             NOT FOUND AT PRD LEVEL               
         MVC   0(1,R4),QCOMPANY            READ CLIENT                          
         MVC   1(2,R4),=C'SJ'                                                   
         ZIC   R3,SVCLILEN                                                      
         BCTR  R3,0                                                             
         EXMVC R3,3(R4),5(R2)                                                   
*                                                                               
CP150    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         MVI   BYTE,X'24'                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        THE FOLLOWING CODE IS FOR THE SOLE PURPOSE OF AGENCIES THAT            
*        PAD ACCOUNTS WITH BLANKS BUT STILL CONSIDER THAT BLANK A               
*        CHARACTER TO BE FILTERED ON                                            
CP200    MVC   CSTACCT,ACPRCOST+3                                               
         CLC   QCUL,=C'1C'                WAS ACCOUNT FILTER ON 1C              
         BNE   CP210                      REQUESTED?                            
         LA    R1,CSTACCT                 COSTING ACCOUNT                       
         LA    R2,QCACCT                  COSTING ACCOUNT FILTER                
         ZIC   R3,CSTTOTA                 COMPARE FOR LEVEL A LENGTH            
         BCTR  R3,0                                                             
         EXCLC R3,0(R2),0(R1)                                                   
         BNE   CP205                                                            
*                                                                               
         CLI   CSTLEVB,0                  IF LEVEL NOT SET UP DO NOT            
         BE    CP210                      NEED TO COMPARE                       
         LA    R3,1(,R3)                                                        
         AR    R1,R3                      BUMP CSTACCT AND                      
         AR    R2,R3                      QCACCT                                
         ZIC   R3,CSTLEVB                 IS LEVEL B INCLUDED IN FILTER         
         BCTR  R3,0                                                             
         EXCLC R3,0(R2),SPACES                                                  
         BE    CP210                                                            
         EXCLC R3,0(R2),0(R1)                                                   
         BNE   CP205                                                            
*                                                                               
         CLI   CSTLEVC,0                  IF LEVEL NOT SET UP DO NOT            
         BE    CP210                      NEED TO COMPARE                       
         LA    R3,1(,R3)                                                        
         AR    R1,R3                      BUMP CSTACCT AND                      
         AR    R2,R3                      QCACCT                                
         ZIC   R3,CSTLEVC                 IS LEVEL C INCLUDED IN FILTER         
         BCTR  R3,0                                                             
         EXCLC R3,0(R2),SPACES                                                  
         BE    CP210                                                            
         EXCLC R3,0(R2),0(R1)                                                   
         BNE   CP205                                                            
*                                                                               
         CLI   CSTLEVD,0                  IF LEVEL NOT SET UP DO NOT            
         BE    CP210                      NEED TO COMPARE                       
         LA    R3,1(,R3)                                                        
         AR    R1,R3                      BUMP CSTACCT AND                      
         AR    R2,R3                      QCACCT                                
         ZIC   R3,CSTLEVD                 IS LEVEL D INCLUDED IN FILTER         
         BCTR  R3,0                                                             
         EXCLC R3,0(R2),SPACES                                                  
         BE    CP210                                                            
         EXCLC R3,0(R2),0(R1)                                                   
         BNE   CP205                                                            
*                                                                               
         B     CP210                                                            
*                                                                               
CP205    MVI   READTRNS,C'N'                                                    
*                                                                               
         USING ACMD,R2                                                          
*                                                                               
CP210    L     R2,AMONACC                                                       
         L     R2,ACMALTN                                                       
         MVC   SAVEKEY,0(R2)                                                    
         L     R4,=A(ACCBUFF)             RESET DATAMGR READ                    
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',SAVEKEY,(R4)                 
         CLI   READTRNS,C'N'                                                    
         BE    CIAXIT                                                           
*                                                                               
*              PROPOGATE COSTING ACCT CODE DOWN SORTRECS                        
*              BEGIN WITH LEVEL A                                               
CP220    MVI   LOW1C,1                   KEEP TRACK OF LOWEST LEV 1C            
         LA    R6,SORTREC1               IF 1C/SI PROPOGATE 1C LEV A            
         LA    R3,SRTKEY1                DOWN ENTIRE ARRAY OF SRTRECS           
         LA    R2,9                                                             
         CLI   SORTOPT,4                 IF SI/1C PROPOGATE DOWN                
         BNE   CP300                     SECONDARY SORTRECS                     
         LA    R6,SORTLEVA                                                      
         LA    R3,SRTKEY2                                                       
         LA    R2,5                                                             
CP300    ZIC   R1,CSTTOTA                                                       
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),CSTACCT                                                 
         LA    R3,L'SORTREC(,R3)                                                
         BCT   R2,CP300                                                         
*                                                                               
*              LEVEL B OF 1C                                                    
         CLI   CSTTOTB,0                                                        
         BE    CPXIT                                                            
         MVI   LOW1C,2                   KEEP TRACK OF LOWEST LEV 1C            
         LA    R6,SORTREC2               IF 1C/SI PROPOGATE 1C LEV B            
         LA    R3,SRTKEY1                DOWN ITSELF AND ALL BELOW              
         LA    R2,8                                                             
         CLI   SORTOPT,4                 IF SI/1C PROPOGATE 1C LEV C            
         BNE   CP400                     DOWN SECONDARY SORTRECS                
         LA    R6,SORTLEVB                                                      
         LA    R3,SRTKEY2                                                       
         LA    R2,4                                                             
*                                                                               
CP400    ZIC   R1,CSTTOTB                                                       
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),CSTACCT                                                 
         LA    R3,L'SORTREC(,R3)                                                
         BCT   R2,CP400                                                         
*                                                                               
         CLI   CSTTOTC,0                                                        
         BE    CPXIT                                                            
         MVI   LOW1C,3                   KEEP TRACK OF LOWEST LEV 1C            
         LA    R6,SORTREC3               IF 1C/SI PROPOGATE 1C LEV C            
         LA    R3,SRTKEY1                DOWN ITSELF AND ALL BELOW              
         LA    R2,7                                                             
         CLI   SORTOPT,4                 IF SI/1C PROPOGATE 1C LEV C            
         BNE   CP500                     DOWN SECONDARY SORTRECS                
         LA    R6,SORTLEVC                                                      
         LA    R3,SRTKEY2                                                       
         LA    R2,3                                                             
*                                                                               
CP500    ZIC   R1,CSTTOTC                                                       
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),CSTACCT                                                 
         LA    R3,L'SORTREC(,R3)                                                
         BCT   R2,CP500                                                         
*                                                                               
         CLI   CSTTOTD,0                                                        
         BE    CPXIT                                                            
         MVI   LOW1C,4                   KEEP TRACK OF LOWEST LEV 1C            
         LA    R6,SORTREC4               IF 1C/SI PROPOGATE 1C LEV D            
         LA    R3,SRTKEY1                DOWN ITSELF AND ALL BELOW              
         LA    R2,6                                                             
         CLI   SORTOPT,4                 IF SI/1C PROPOGATE 1C LEV D            
         BNE   CP600                     DOWN SECONDARY SORTRECS                
         LA    R6,SORTLEVD                                                      
         LA    R3,SRTKEY2                                                       
         LA    R2,2                                                             
*                                                                               
CP600    ZIC   R1,CSTTOTD                                                       
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),CSTACCT                                                 
         LA    R3,L'SORTREC(,R3)                                                
         BCT   R2,CP600                                                         
*                                                                               
CPXIT    DS    0H                                                               
         MVI   LEVAOK,X'80'                                                     
         MVI   LEVBOK,X'80'                                                     
         MVI   LEVCOK,X'80'                                                     
         MVI   LEVDOK,X'80'                                                     
         MVI   LEV1OK,X'80'                                                     
         MVI   LEV2OK,X'80'                                                     
         MVI   LEV3OK,X'80'                                                     
         MVI   LEV4OK,X'80'                                                     
         B     CIAXIT                                                           
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*   ALL TRANSACTION INFORMATION (TEXT AND DOLLAR AMTS) IS PUT TO    *           
*   SORTREC AND RELEASED TO SORTER                                  *           
*-------------------------------------------------------------------*           
PRCT100  CLI   MODE,PROCTRNS                                                    
         BNE   CIA600                                                           
*                                                                               
         CLI   READTRNS,C'Y'                                                    
         BNE   CIAXIT                                                           
         L     R4,ADTRANS                                                       
*                                                                               
         USING TRANSD,R4                                                        
*                                                                               
         CLI   TRNSEL,X'44'                                                     
         BNE   CIAXIT                                                           
*                                                                               
         LA    R6,SORTTRAN                                                      
         MVC   SINVNUM,TRNSREF             INV NUM TO SORTREC                   
         MVC   SINVDAT,TRNSDATE            INV DATE TO SORTREC                  
         ZAP   TGROSS,TRNSAMNT                                                  
         ZAP   TCOMM,TRNSAMNT                                                   
         ZAP   TNET,=P'0'                                                       
         MVC   SINVMOS,SPACES              CLEAR UNUSED FIELD                   
*        MVC   SINVMOS,WORK                SAVE MOS AS PWOS YYMM                
*                                                                               
         MVI   BYTE,X'50'                                                       
*                                                                               
PRCT200  BAS   RE,NEXTEL                                                        
         BNE   PRCT230                                                          
*                                                                               
         USING TRCASHD,R4                                                       
*                                                                               
         CLI   TRCSTYPE,C'G'                                                    
         BNE   PRCT200                     NOT GROSS ELEMENT,TRY AGAIN          
         ZAP   TGROSS,TRCSAMNT                                                  
         B     PRCT280                                                          
*                                                                               
         USING TRANSD,R4                                                        
*                                                                               
PRCT230  L     R4,ADTRANS                                                       
         CLI   TRNSTYPE,8                                                       
         BE    PRCT250                                                          
         CLI   TRNSTYPE,21                                                      
         BE    PRCT250                                                          
*                                                                               
         CLI   TRNSTYPE,0                  BUG IN BILLING                       
         BE    PRCT250                                                          
         CLI   TRNSTYPE,6                                                       
         BE    PRCT250                                                          
         CLI   TRNSTYPE,7                                                       
         BE    PRCT250                                                          
         CLI   TRNSTYPE,9                                                       
         BE    PRCT250                                                          
         B     PRCT280                                                          
PRCT250  ZAP   TGROSS,=P'0'                                                     
*                                                                               
PRCT280  DS    0H                                                               
         BAS   RE,SRTAMTS               PUT $ AMTS AND TEXT TO SORTREC          
*                                                                               
         OI    LEVAOK,1                 O.K. RELEASE OF ALL SORTRECS            
         OI    LEVBOK,1                                                         
         OI    LEVCOK,1                                                         
         OI    LEVDOK,1                                                         
         OI    LEV1OK,1                 O.K. RELEASE OF ALL SORTRECS            
         OI    LEV2OK,1                                                         
         OI    LEV3OK,1                                                         
         OI    LEV4OK,1                                                         
         BAS   RE,PUTSORT                                                       
         B     CIAXIT                                                           
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*              SBACLAST  --  SEND OUT ALL SORTRECS OTHER THAN        *          
*              TRANSACTIONS(SENT OUT IN PROCTRNS)                    *          
*--------------------------------------------------------------------*          
CIA600   CLI   MODE,SBACLAST                                                    
         BNE   CIA700                                                           
         MVI   READTRNS,C'Y'                                                    
*                                                                               
         CLI   SORTOPT,2              SI/SJ SORT                                
         BE    CIA6F                                                            
         CLI   SORTOPT,3              1C/SI SORT                                
         BE    CIA6M                                                            
*                                                                               
*                                                                               
*              SET UP SJ/SI SORT            CLI SORTREC.                        
*              ALSO FOR SI/1C SORT                                              
         TM    LEV1OK,X'81'                HAS CLI BEEN SENT?                   
         BNO   CIA6A                       YES.                                 
         LA    R6,SORTREC1                                                      
         BAS   RE,PUTSORT                  SEND LEAD CLI SORTREC.               
         MVC   LASTSORT,SORTREC            TEMP SAVE.                           
         CLI   SORTOPT,4                                                        
         BNE   CIA608                                                           
         LR    R1,R6                                                            
         ZIC   R0,ALEVLEN                                                       
         AR    R1,R0                                                            
         MVC   0(12,R1),=12X'FF'                                                
         B     *+10                                                             
CIA608   MVC   SPRDKEY,=12X'FF'            SET UP TRAILER CLI SORTREC.          
         MVC   SRTKEY2,=12X'FF'                                                 
         MVC   SINVNUM,=12X'FF'                                                 
         BAS   RE,PUTSORT                  SEND TRAILER CLI SORTREC.            
         MVC   SORTREC,LASTSORT            RESTORE.                             
         XC    LEV1OK,LEV1OK               SET CLI SENT.                        
*                                                                               
*                                                                               
CIA6A    LA    R6,SORTREC2                 PROD SORTREC                         
         TM    LEV2OK,X'81'                HAS PROD BEEN SENT?                  
         BNO   CIA6B                       YES.                                 
         BAS   RE,PUTSORT                  SEND LEAD PROD SORTREC.              
         MVC   LASTSORT,SORTREC            TEMP SAVE.                           
         CLI   SORTOPT,4                                                        
         BNE   CIA6A5                                                           
         LR    R1,R6                                                            
         ZIC   R0,BLEVTOT                                                       
         AR    R1,R0                                                            
         MVC   0(12,R1),=12X'FF'                                                
CIA6A5   MVC   SRTKEY2,=12X'FF'            SET UP TRAILER PROD                  
         MVC   SINVNUM,=12X'FF'            SORTREC.                             
         BAS   RE,PUTSORT                  SEND TRAILER PROD SORTREC.           
         MVC   SORTREC,LASTSORT            RESTORE.                             
         XC    LEV2OK,LEV2OK               SET PROD SENT.                       
         CLI   SORTOPT,4                                                        
         BE    CIA6M4                                                           
*                                                                               
*                                                                               
CIA6B    LA    R6,SORTLEVA                 LEVEL A SORTREC.                     
         TM    LEVAOK,X'81'                HAS LEVEL A BEEN SENT?               
         BNO   CIA6C                       YES.                                 
         BAS   RE,PUTSORT                  SENT LEAD LEVA SORTREC.              
         MVC   LASTSORT,SORTREC            TEMP SAVE.                           
         MVC   SINVNUM,=12X'FF'            SET UP TRAILER LEVA SORTREC.         
*                                                                               
         LA    R4,SRTKEY2                                                       
         CLI   SORTOPT,2                                                        
         BNE   CIA6B05                                                          
         MVC   SRTKEY2,=12X'FF'                                                 
         LA    R4,SRTKEY1                                                       
*                                                                               
CIA6B05  CLI   BLEVLEN,0                   PAD LEV B,C,D ACCOUNT IN KEY         
         BE    CIA6B10                     WITH HEX FF'S.                       
         ZIC   R0,ALEVLEN                                                       
         AR    R4,R0                                                            
         LA    R1,L'SRTKEY2                                                     
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EXMVC R1,0(R4),=12X'FF'                                                
*                                                                               
CIA6B10  BAS   RE,PUTSORT                  SEND TRAILER LEVA SORTREC.           
         MVC   SORTREC,LASTSORT            RESTORE.                             
         XC    LEVAOK,LEVAOK               SET LEVA SENT.                       
*                                                                               
*                                                                               
CIA6C    CLI   BLEVLEN,0                                                        
         BE    CIA6X                                                            
         LA    R6,SORTLEVB                 LEVEL B SORTREC.                     
         TM    LEVBOK,X'81'                HAS LEVB BEEN SENT?                  
         BNO   CIA6D                       YES.                                 
         BAS   RE,PUTSORT                  SEND LEAD LEVB SORTREC.              
         MVC   LASTSORT,SORTREC            TEMP SAVE.                           
         MVC   SINVNUM,=12X'FF'            SET UP TRAILER SORTREC.              
         ZIC   R0,BLEVTOT                  BUMP PAST LEVEL A AND                
*                                                                               
         LA    R4,SRTKEY2                                                       
         CLI   SORTOPT,2                                                        
         BNE   CIA6C05                                                          
         MVC   SRTKEY2,=12X'FF'                                                 
         LA    R4,SRTKEY1                                                       
*                                                                               
CIA6C05  CLI   CLEVLEN,0                                                        
         BE    CIA6C10                                                          
         AR    R4,R0                                                            
         LA    R1,12                                                            
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EXMVC R1,0(R4),=12X'FF'                                                
*                                                                               
CIA6C10  BAS   RE,PUTSORT                  SEND TRAILER LEVB REC                
         MVC   SORTREC,LASTSORT            RESTORE.                             
         XC    LEVBOK,LEVBOK               SET LEVB SENT.                       
*                                                                               
*                                                                               
CIA6D    CLI   CLEVLEN,0                                                        
         BE    CIA6X                                                            
         LA    R6,SORTLEVC                 LEVEL C SORTREC.                     
         TM    LEVCOK,X'81'                HAS LEVC BEEN SENT?                  
         BNO   CIA6E                       YES.                                 
         BAS   RE,PUTSORT                  SENT LEAD LEVC SORTREC.              
         MVC   LASTSORT,SORTREC            TEMP SAVE.                           
         MVC   SINVNUM,=12X'FF'            SET UP TRAILER LEVC BY               
         ZIC   R0,CLEVTOT                                                       
*                                                                               
         LA    R4,SRTKEY2                                                       
         CLI   SORTOPT,2                                                        
         BNE   CIA6D05                                                          
         MVC   SRTKEY2,=12X'FF'                                                 
         LA    R4,SRTKEY1                                                       
*                                                                               
CIA6D05  CLI   DLEVLEN,0                   PADDING LEVEL D WITH                 
         BE    CIA6D10                     HEX FF'S.                            
         AR    R4,R0                                                            
         LA    R1,12                                                            
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EXMVC R1,0(R4),=12X'FF'                                                
*                                                                               
CIA6D10  BAS   RE,PUTSORT                  SEND TRAILER LEVC SORTREC.           
         MVC   SORTREC,LASTSORT            RESTORE.                             
         XC    LEVCOK,LEVCOK               SET LEVC SENT.                       
*                                                                               
*                                                                               
CIA6E    CLI   DLEVLEN,0                                                        
         BE    CIA6X                                                            
         LA    R6,SORTLEVD                 LEVEL D SORTREC.                     
         TM    LEVDOK,X'81'                HAS LEVD BEEN SENT?                  
         BNO   CIA6X                       YES.                                 
         BAS   RE,PUTSORT                  SEND LEAD LEVC SORTREC.              
         MVC   LASTSORT,SORTREC            TEMP SAVE.                           
*                                                                               
         CLI   SORTOPT,2                                                        
         BNE   *+10                                                             
         MVC   SCLIKEY2,=12X'FF'                                                
*                                                                               
         MVC   SINVNUM,=12X'FF'            SET UP TRAILER SORTREC.              
         BAS   RE,PUTSORT                  SEND.                                
         MVC   SORTREC,LASTSORT            RESTORE.                             
         XC    LEVDOK,LEVDOK               SET LEVD SENT.                       
         B     CIA6X                                                            
         EJECT ,                                                                
*                                                                               
CIA6F    DS    0H                                                               
         TM    LEV1OK,X'81'                                                     
         BNO   CIA6G                                                            
         LA    R6,SORTREC1                                                      
         MVC   SRTKEY1,SAVACCT                                                  
         BAS   RE,PUTSORT                                                       
         MVC   LASTSORT,SORTREC                                                 
         MVC   SPRDKEY2,=12X'FF'                                                
         MVC   SINVNUM,=12X'FF'                                                 
         BAS   RE,PUTSORT                                                       
         MVC   SORTREC,LASTSORT                                                 
         XC    LEV1OK,LEV1OK                                                    
*                                                                               
CIA6G    DS    0H                                                               
         TM    LEV2OK,X'81'                                                     
         BNO   CIA6H                                                            
         LA    R6,SORTREC2                                                      
         MVC   SRTKEY1,SAVACCT                                                  
         BAS   RE,PUTSORT                                                       
         MVC   LASTSORT,SORTREC                                                 
         MVC   SINVNUM,=12X'FF'                                                 
         BAS   RE,PUTSORT                                                       
         MVC   SORTREC,LASTSORT                                                 
         XC    LEV2OK,LEV2OK                                                    
CIA6H    B     CIA6B                       CONTINUE AS BEFORE                   
*                                                                               
*              SET UP 1C/SI SORT                                                
CIA6M    DS    0H                                                               
         TM    LEV1OK,X'81'                                                     
         BNO   CIA6M2                                                           
         LA    R6,SORTREC1                                                      
         BAS   RE,PUTSORT                                                       
         MVC   LASTSORT,SORTREC                                                 
         LR    R3,R6                                                            
         ZIC   R2,CSTTOTA                                                       
         AR    R3,R2                                                            
         MVC   0(12,R3),=12X'FF'                                                
         MVC   SINVNUM,=12X'FF'                                                 
         BAS   RE,PUTSORT                                                       
         MVC   SORTREC,LASTSORT                                                 
         XC    LEV1OK,LEV1OK                                                    
*                                                                               
CIA6M2   CLI   CSTTOTB,0                                                        
         BE    CIA6M8                                                           
         TM    LEV2OK,X'81'                                                     
         BNO   CIA6M4                                                           
         LA    R6,SORTREC2                                                      
         BAS   RE,PUTSORT                                                       
         MVC   LASTSORT,SORTREC                                                 
         LR    R3,R6                                                            
         ZIC   R2,CSTTOTB                                                       
         AR    R3,R2                                                            
         MVC   0(12,R3),=12X'FF'                                                
         MVC   SINVNUM,=12X'FF'                                                 
         BAS   RE,PUTSORT                                                       
         MVC   SORTREC,LASTSORT                                                 
         XC    LEV2OK,LEV2OK                                                    
*                                                                               
CIA6M4   ZIC   R2,CSTTOTC                                                       
         CLI   SORTOPT,4                                                        
         BNE   *+10                                                             
         ZIC   R2,CLEVTOT                                                       
         CH    R2,=H'0'                                                         
         BE    CIA6M7                                                           
         TM    LEV3OK,X'81'                                                     
         BNO   CIA6M6                                                           
         LA    R6,SORTREC3                                                      
         BAS   RE,PUTSORT                                                       
         MVC   LASTSORT,SORTREC                                                 
         LR    R3,R6                                                            
         AR    R3,R2                                                            
         MVC   0(12,R3),=12X'FF'                                                
         MVC   SINVNUM,=12X'FF'                                                 
         BAS   RE,PUTSORT                                                       
         MVC   SORTREC,LASTSORT                                                 
         XC    LEV3OK,LEV3OK                                                    
*                                                                               
CIA6M6   ZIC   R2,CSTTOTD                                                       
         CLI   SORTOPT,4                                                        
         BNE   *+10                                                             
         ZIC   R2,DLEVTOT                                                       
         CH    R2,=H'0'                                                         
         BE    CIA6M7                                                           
         TM    LEV4OK,X'81'                                                     
         BNO   CIA6M7                                                           
         LA    R6,SORTREC4                                                      
         BAS   RE,PUTSORT                                                       
         MVC   LASTSORT,SORTREC                                                 
         LR    R3,R6                                                            
         AR    R3,R2                                                            
         MVC   0(12,R3),=12X'FF'                                                
         MVC   SINVNUM,=12X'FF'                                                 
         BAS   RE,PUTSORT                                                       
         MVC   SORTREC,LASTSORT                                                 
         XC    LEV4OK,LEV4OK                                                    
CIA6M7   CLI   SORTOPT,4                                                        
         BE    CIA6P                                                            
CIA6M8   B     CIA6B                                                            
*                                                                               
*        SI/1C SORT                                                             
CIA6P    LA    R6,SORTLEVA                 LEVEL A SORTREC.                     
         TM    LEVAOK,X'81'                HAS LEVEL A BEEN SENT?               
         BNO   CIA6R                       YES.                                 
         BAS   RE,PUTSORT                  SENT LEAD LEVA SORTREC.              
         MVC   LASTSORT,SORTREC            TEMP SAVE.                           
         MVC   SINVNUM,=12X'FF'            SET UP TRAILER LEVA SORTREC.         
         CLI   CSTTOTB,0                   PAD LEV B,C,D ACCOUNT IN KEY         
         BE    CIA6P10                     WITH HEX FF'S.                       
         ZIC   R0,CSTTOTA                                                       
         LA    R4,SRTKEY2                                                       
         AR    R4,R0                                                            
         LA    R1,L'SRTKEY2                                                     
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EXMVC R1,0(R4),=12X'FF'                                                
*                                                                               
CIA6P10  BAS   RE,PUTSORT                  SEND TRAILER LEVA SORTREC.           
         MVC   SORTREC,LASTSORT            RESTORE.                             
         XC    LEVAOK,LEVAOK               SET LEVA SENT.                       
*                                                                               
*                                                                               
CIA6R    CLI   CSTTOTB,0                                                        
         BE    CIA6X                                                            
         LA    R6,SORTLEVB                 LEVEL B SORTREC.                     
         TM    LEVBOK,X'81'                HAS LEVB BEEN SENT?                  
         BNO   CIA6S                       YES.                                 
         BAS   RE,PUTSORT                  SEND LEAD LEVB SORTREC.              
         MVC   LASTSORT,SORTREC            TEMP SAVE.                           
         MVC   SINVNUM,=12X'FF'            SET UP TRAILER SORTREC.              
         CLI   CSTTOTC,0                                                        
         BE    CIA6R10                                                          
         ZIC   R0,CSTTOTB                  BUMP PAST LEVEL A AND                
         LA    R4,SRTKEY2                                                       
         AR    R4,R0                                                            
         LA    R1,12                                                            
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EXMVC R1,0(R4),=12X'FF'                                                
*                                                                               
CIA6R10  BAS   RE,PUTSORT                  SEND TRAILER LEVB REC                
         MVC   SORTREC,LASTSORT            RESTORE.                             
         XC    LEVBOK,LEVBOK               SET LEVB SENT.                       
*                                                                               
*                                                                               
CIA6S    CLI   CSTTOTC,0                                                        
         BE    CIA6X                                                            
         LA    R6,SORTLEVC                 LEVEL C SORTREC.                     
         TM    LEVCOK,X'81'                HAS LEVC BEEN SENT?                  
         BNO   CIA6T                       YES.                                 
         BAS   RE,PUTSORT                  SENT LEAD LEVC SORTREC.              
         MVC   LASTSORT,SORTREC            TEMP SAVE.                           
         MVC   SINVNUM,=12X'FF'            SET UP TRAILER LEVC BY               
         CLI   CSTTOTD,0                   PADDING LEVEL D WITH                 
         BE    CIA6S10                     HEX FF'S.                            
         ZIC   R0,CSTTOTC                                                       
         LA    R4,SRTKEY2                                                       
         AR    R4,R0                                                            
         LA    R1,12                                                            
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EXMVC R1,0(R4),=12X'FF'                                                
*                                                                               
CIA6S10  BAS   RE,PUTSORT                  SEND TRAILER LEVC SORTREC.           
         MVC   SORTREC,LASTSORT            RESTORE.                             
         XC    LEVCOK,LEVCOK               SET LEVC SENT.                       
*                                                                               
*                                                                               
CIA6T    CLI   CSTTOTD,0                                                        
         BE    CIA6X                                                            
         LA    R6,SORTLEVD                 LEVEL D SORTREC.                     
         TM    LEVDOK,X'81'                HAS LEVD BEEN SENT?                  
         BNO   CIA6X                       YES.                                 
         BAS   RE,PUTSORT                  SEND LEAD LEVC SORTREC.              
         MVC   LASTSORT,SORTREC            TEMP SAVE.                           
         MVC   SINVNUM,=12X'FF'            SET UP TRAILER SORTREC.              
         BAS   RE,PUTSORT                  SEND.                                
         MVC   SORTREC,LASTSORT            RESTORE.                             
         XC    LEVDOK,LEVDOK               SET LEVD SENT.                       
*                                                                               
*                                                                               
CIA6X    B     CIAXIT                                                           
         EJECT ,                                                                
*              PROCESS SI/SJ SORT                                               
*                                                                               
*--------------------------------------------------------------------*          
*  FOR ALL SORTS THERE IS A PRIMARY SORT AND SECONDARY SORT -        *          
*  WITH IN EACH THERE IS A MAX POSSIBILITY OF FOUR LEVELS            *          
*  (EIGHT TOTAL) BEFORE YOU GET TO TRANSACTION LEVEL                 *          
*                                                                    *          
*  AT EVEY LEVEL BREAK YOU WILL CHECK TO SEE IF THIS LEVEL HAS BEEN  *          
*  REQUESTED TO BE INCLUDED IN THE SUMMARY, IF SO YOU WILL BUILD A   *          
*  BUFFALO RECORD FROM THE SORTRECORDS AND RELEASE IT -              *          
*  A HEADER AND TRAILER RECORD FOR EACH LEVEL OF THE PRIMARY AND     *          
*  SECONDARY SORT REQUESTED (QOPT4,5) WILL BE RELEASED               *          
*                                                                    *          
*--------------------------------------------------------------------*          
*--------------------------------------------------------------------*          
*  ***R IN COL ONE SIGNIFIES CODE THAT WAS TAKEN OUT TO UNSUPPRESS   *          
*  REDUNDANT TOTALS                                     --           *          
*--------------------------------------------------------------------*          
CIA700   CLI   MODE,REQLAST                                                     
         BNE   CIA900                                                           
*                                                                               
         MVC   PAGE,=H'1'                  INITIALIZE                           
         MVI   FORCEHED,C'Y'                                                    
         XC    LASTSORT,LASTSORT                                                
         MVC   SAVACNM,SPACES                                                   
         MVI   NUMOFINV,0                                                       
         MVC   SAVINVN,SPACES                                                   
*                                                                               
         CLI   SORTOPT,1                   NEED TO SEE IF SELECTED              
         BNE   CIA705                      SUMMARY LEVELS DO NOT                
         CLI   QOPT4,2                     EXCEED THE LEVELS THEY               
         BNH   *+8                         HAVE SET UP ON THE                   
         MVI   QOPT4,2                     CORRESPONDING LEDGERS -              
CIA705   CLI   SORTOPT,2                   WOULD CAUSE INACCURATE               
         BE    CIA705A                     LEVEL MARKERS TO BE SENT             
         CLI   SORTOPT,4                   OUT ON BUFFALO RECS                  
         BNE   CIA707                                                           
CIA705A  CLC   QOPT4,LOWSI                                                      
         BNH   *+10                                                             
         MVC   QOPT4,LOWSI                                                      
*                                                                               
CIA707   CLI   SORTOPT,3                                                        
         BNE   CIA709                                                           
         CLC   QOPT4,LOW1C                                                      
         BNH   *+10                                                             
         MVC   QOPT4,LOW1C                                                      
*                                                                               
CIA709   DS    0H                                                               
***R     LA    R4,9                        COUNTERS FOR EACH LEVEL              
***R     LA    R6,RECCNTRS                 OF SORTRECS TO PREVENT               
***R10   ZAP   0(L'REC1CONT,R6),=P'0'      PRINTING OF REDUNDANT                
***R     LA    R6,L'REC1CONT(,R6)          TOTALS                               
***R     BCT   R4,CIA710                                                        
*                                                                               
         USING PLINED,R4                                                        
*                                                                               
         LA    R4,XP                       USING R4 FOR PRINT LINE              
         LA    R6,SORTREC1                 SORTREC1                             
*                                                                               
CIA750   BAS   RE,GETSORT                                                       
         CLC   SORTREC,SPACES              EOF                                  
         BE    PRNTREQ                     PRINT REQUEST TOTALS                 
*                                                                               
         CLI   SORTOPT,2                   PROCESSING SI/SJ SORT                
         BE    SISJ100                                                          
         CLI   SORTOPT,4                   PROCESSING SI/1C SORT                
         BE    SISJ100                                                          
         CLI   SRTTYP,C'T'                 TRANSACTIONS                         
         BE    TRNA100                                                          
         CLC   SORTKEY,LASTSORT            TEST AGAINST LAST SORTREC            
         BE    CIA750                      DUPLICATE RECS WILL OCCUR            
         MVC   LASTSORT,SORTREC            SAVE FOR NEXT PASS                   
         MVC   WORK,SPACES                                                      
         MVC   XP,XSPACES                                                       
         B     LVA100                                                           
*                                                                               
*              ACCUMULATE AND PROPOGATE ALL $ AMTS                              
PUTROLL  GOTO1 PROLLER,DMCB,2,ACCUMS,(R2)                                       
         B     CIA750                                                           
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*  PROCESS SJ/SI AND 1C/SI SORTS                                     *          
*        PROCESS PRIMARY SORTRECS                                    *          
*--------------------------------------------------------------------*          
*--------------------------------------------------------------------*          
*              PRIMARY SORT          (EITHER SJ OR 1C)               *          
*              FIRST FOR LEVEL 1                                     *          
*--------------------------------------------------------------------*          
LVA100   CLI   SRTTYP,C'A'                                                      
         BNE   LVB100                                                           
         CLC   SINVNUM,=12X'FF'         LAST FOR CLI(OR LEV 1 OF 1C)            
         BE    LVA400                                                           
*                                                                               
         MVI   FORCEHED,C'Y'            FIRST FOR CLI(OR LEV 1 OF 1C)           
         ZIC   R2,CLILEN                GET CLIENT (OR 1C LEV 1) NAME           
         BAS   RE,GETNAME                                                       
         MVC   SAVANAM,SRTDATA          SAVE NAME FOR TOTAL LINE                
         MVC   MYHEAD4(68),SPACES                                               
         MVC   MYHEAD4(L'SVHEAD4),SVHEAD4                                       
         ZIC   R2,CSTTOTA                                                       
         BCTR  R2,0                                                             
         EXMVC R2,MYHEAD4+16,SRTKEY1                                            
         MVC   MYHEAD4+29(L'SRTDATA),SRTDATA                                    
*H       MVC   WORK(L'SVHEAD4),SVHEAD4                                          
*H       ZIC   R2,CSTTOTA                                                       
*H       BCTR  R2,0                                                             
*H       EXMVC R2,WORK+L'SVHEAD4,SRTKEY1   CODE AND NAME TO SQUASHER            
*H       MVC   WORK+L'SVHEAD4+L'SRTKEY1+1(L'SRTDATA),SRTDATA                    
*H       BAS   RE,SQUASHIT                                                      
*H       EXMVC R2,MYHEAD4+1,WORK                                                
***R     AP    REC1CONT,=P'1'              INCREMENT REC1 CNTR                  
***R     ZAP   REC2CONT,=P'0'              CLEAR REC2 CNTR                      
*              SEND SUMMARY RECORD TO BUFFALO                                   
         ZIC   R2,ROLPRIA                  ROLLER POSITION                      
         CLI   QOPT4,1                     IF LOWER THAN ONE NO                 
         BL    PUTROLL                     SUMMARY REQUESTED                    
         MVI   BUFLW1,C'L'                 MARK AS LOWEST LEVEL                 
         CLI   QOPT4,1                     IF LOWER THAN ONE NO                 
         BE    *+8                                                              
         MVI   BUFLW1,C'H'                                                      
         XC    BUFFADD,BUFFADD             SEND OUT REC FOR SUMMARIES           
         MVI   BUFMRK1,X'11'               HEADER PRIMARY SORT                  
         MVI   BUFMRK2,1                   LEVEL ONE                            
         BAS   RE,PUTBUFF                                                       
         B     PUTROLL                                                          
*--------------------------------------------------------------------*          
*              PRIMARY SORT   (EITHER SJ OR 1C)                      *          
*              LAST FOR LEVEL 1                                      *          
*--------------------------------------------------------------------*          
LVA400   DS    0H                          LAST FOR CLI(OR 1C LEV 1)            
         MVI   FORCEHED,C'N'                                                    
         BAS   RE,CIAREPRT                                                      
         MVC   WORK(L'TOTAL),TOTAL                                              
         ZIC   R2,CSTTOTA                  CLI (OR 1C LV1) CODE                 
         BCTR  R2,0                                                             
         EXMVC R2,WORK+L'TOTAL,SRTKEY1                                          
         MVC   WORK+L'TOTAL+L'SRTKEY1+1(L'SRTDATA),SAVANAM                      
         BAS   RE,SQUASHIT                                                      
         BAS   RE,CHOPIT1                                                       
         MVC   PNAME1(L'WORK1A),WORK1A                                          
         MVC   PNAME1+L'XP+3(L'WORK1B),WORK1B                                   
         ZIC   R2,ROLPRIA                  ROLLER POSITION                      
         BAS   RE,FORMAT                                                        
         BAS   RE,CIAREPRT                                                      
         MVI   FORCEHED,C'Y'                                                    
*              SEND SUMMARY RECORD TO BUFFALO                                   
         CLI   QOPT4,1                     IF LOWER THAN ONE NO                 
         BL    LVA450                      SUMMARY REQUESTED                    
         MVI   BUFMRK1,X'22'               TRAILER PRIMARY SORT                 
         MVI   BUFMRK2,1                   LEVEL ONE                            
         CLI   QOPT4,1                     IS THIS LOWEST LEVEL                 
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                 MARK AS LOWEST LEVEL                 
         BAS   RE,PUTBUFF                  SEND REC OUT FOR SUMMARIES           
*                                                                               
LVA450   DS    0H                                                               
         B     CIA750                                                           
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*              PRIMARY SORT        (EITHER SJ OR 1C)                 *          
*              FIRST FOR LEVEL 2                                     *          
*--------------------------------------------------------------------*          
LVB100   CLI   SRTTYP,C'B'                                                      
         BNE   LVC100                                                           
         CLC   SINVNUM,=12X'FF'            LAST FOR LEV 2                       
         BE    LVB400                                                           
*                                                                               
         CLI   SORTOPT,3                   SQUASHER, PRINT LINE                 
         BNE   LVB200                                                           
         MVC   MYHEAD5,XSPACES                                                  
         MVC   MYHEAD5(15),BCSTNAM                                              
         ZIC   R2,CSTTOTB                  READ COST LEV 2 ACCT NAME            
         BAS   RE,GETNAME                                                       
         ZIC   R2,CSTTOTB                                                       
         BCTR  R2,0                                                             
         EXMVC R2,MYHEAD5+16,SRTKEY1                                            
         MVC   MYHEAD5+29(L'SRTDATA),SRTDATA                                    
*H       MVC   WORK(L'BCSTNAM),BCSTNAM                                          
*H       ZIC   R2,CSTTOTB                  READ COST LEV 2 ACCT NAME            
*H       BAS   RE,GETNAME                                                       
*H       ZIC   R2,CSTTOTB                                                       
*H       BCTR  R2,0                                                             
*H       EXMVC R2,WORK+L'BCSTNAM+1,SRTKEY1                                      
*H       MVC   WORK+L'SRTKEY1+L'BCSTNAM+2(L'SRTDATA),SRTDATA                    
*H       BAS   RE,SQUASHIT                                                      
*H       EXMVC R2,MYHEAD5+1,WORK           SQUASHED DATA TO PRINT LINE          
         B     LVB260                                                           
*                                                                               
LVB200   BAS   RE,CIAREPRT                            BLANK LINE                
         MVC   WORK(L'SPRDKEY),SPRDKEY                PROD TO WORK AREA         
         MVC   WORK+L'SPRDKEY+1(L'SRTDATA),SRTDATA    NAME TO WORK AREA         
         BAS   RE,SQUASHIT                                                      
         EXMVC R2,PNAME1,WORK              SQUASHED DATA TO PRINT LINE          
         BAS   RE,CIAREPRT                                                      
*                                                                               
LVB260   DS    0H                                                               
***R     AP    REC2CONT,=P'1'              INCREMENT PROD COUNTER AND           
***R     ZAP   REC3CONT,=P'0'              RESET LOWER LEVEL COUNTERS           
***R     ZAP   LEVACONT,=P'0'              RESET LOWER LEVEL COUNTERS           
         MVC   SAVBNAM,SRTDATA                                                  
         ZIC   R2,ROLPRIB                  ROLLER POSITION                      
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
         CLI   QOPT4,2                     DO I NEED THIS LEVEL FOR             
         BL    PUTROLL                     SUMMARY                              
         XC    BUFFADD,BUFFADD                                                  
         MVI   BUFMRK1,X'11'               HEADER PRIMARY SORT                  
         MVI   BUFMRK2,2                   LEVEL TWO                            
         MVI   BUFLW1,C'L'                 MARK AS LOWEST LEVEL                 
         CLI   QOPT4,2                     IS THIS LOWEST LEVEL                 
         BE    *+8                                                              
         MVI   BUFLW1,C'H'                 MARK AS LOWEST LEVEL                 
         BAS   RE,PUTBUFF                  PUT OUT REC FOR SUMMARY              
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              PRIMARY SORT   (EITHER SJ OR 1C)                      *          
*              LAST FOR LEVEL 2                                      *          
*--------------------------------------------------------------------*          
LVB400   DS    0H                                                               
***R     ZAP   FULL,LEVACONT               CHECK LEVEL COUNTERS                 
***R     CLI   SORTOPT,3                                                        
***R     BNE   LVB450                                                           
***R     CLI   CSTTOTC,0                                                        
***R     BE    LVB450                                                           
***R     ZAP   FULL,REC3CONT                                                    
***R50   CP    FULL,=P'1'   (LVB450)       DO I NEED TO PRINT TOTAL             
***R     BNH   LVB650                      LINE                                 
*                                                                               
         MVI   FORCEHED,C'N'                                                    
LVB500   BAS   RE,CIAREPRT                                                      
         MVC   WORK(L'TOTAL),TOTAL                                              
         MVC   WORK+L'TOTAL(L'SPRDKEY),SPRDKEY                                  
         CLI   SORTOPT,3                                                        
         BNE   LVB550                                                           
         MVC   SRTDATA,SAVBNAM                                                  
         ZIC   R2,CSTTOTB                                                       
         BCTR  R2,0                                                             
         EXMVC R2,WORK+L'TOTAL,SRTKEY1     KEY AND NAME TO SQUASHER             
*                                                                               
LVB550   MVC   WORK+L'TOTAL+L'SRTKEY1+1(L'SRTDATA),SRTDATA                      
         BAS   RE,SQUASHIT                                                      
         BAS   RE,CHOPIT1                                                       
         MVC   PNAME1(L'WORK1A),WORK1A                                          
         MVC   PNAME1+L'XP+3(L'WORK1B),WORK1B                                   
         ZIC   R2,ROLPRIB                  LEVEL POSITION IN ROLLER             
         BAS   RE,FORMAT                   TOTALS TO PRINT LINE                 
         BAS   RE,CIAREPRT                                                      
         CLI   SORTOPT,1                   ONLY PRINT LINE BREAK ON             
         BNE   LVB600                      C/I SORT                             
         MVC   XP(L'TOTUNDL),TOTUNDL                                            
         BAS   RE,CIAREPRT                                                      
*                                                                               
LVB600   CLI   SORTOPT,1                   IF SJ/SI SORT NO PAGE                
         BE    *+8                         BREAK ON PRODUCT BREAK               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
LVB650   DS    0H                                                               
         CLI   QOPT4,2                     DO I NEED THIS LEVEL FOR             
         BL    CIA750                      SUMMARIES                            
         ZIC   R2,ROLPRIB                                                       
         BAS   RE,FORMAT                                                        
         MVI   BUFMRK1,X'22'               TRAILER PRIMARY SORT                 
         MVI   BUFMRK2,2                   LEVEL TWO                            
         CLI   QOPT4,2                     IS THIS LOWEST LEVEL                 
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                 MARK AS LOWEST LEVEL                 
         BAS   RE,PUTBUFF                  SEND REC TO BUFFALO                  
         MVC   XP,XSPACES                   CLEAR UNPRINTED DATA FROM           
         B     CIA750                      PRINT LINE                           
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*              PRIMARY SORT   (MUST BE 1C)                           *          
*              FIRST FOR LEVEL 3                                     *          
*--------------------------------------------------------------------*          
LVC100   CLI   SRTTYP,C'C'                                                      
         BNE   LVD100                                                           
         CLC   SINVNUM,=12X'FF'            LAST FOR LEVEL C                     
         BE    LVC400                                                           
*                                                                               
         MVC   MYHEAD6,XSPACES                                                  
         ZIC   R2,CSTTOTC                  GET 1C LEVEL C ACC NAME              
         BAS   RE,GETNAME                                                       
         MVC   SAVCNAM,SRTDATA                                                  
         MVC   MYHEAD6(15),CCSTNAM                                              
         ZIC   R2,CSTTOTC                                                       
         BCTR  R2,0                                                             
         EXMVC R2,MYHEAD6+16,SRTKEY1                                            
         MVC   MYHEAD6+29(L'SRTDATA),SRTDATA                                    
*H       ZIC   R2,CSTTOTC                                                       
*H       BCTR  R2,0                        KEY AND ACCT NAME TO                 
*H       EXMVC R2,WORK+L'CCSTNAM+1,SRTKEY1 SQUASHER, THEN PRINT LINE            
*H       MVC   WORK(L'CCSTNAM),CCSTNAM                                          
*H       MVC   WORK+L'CCSTNAM+L'SRTKEY1+2(L'SRTDATA),SRTDATA                    
*H       BAS   RE,SQUASHIT                                                      
*H       EXMVC R2,MYHEAD6+1,WORK                                                
***R     AP    REC3CONT,=P'1'              INCREMENT PROD COUNTER AND           
***R     ZAP   REC4CONT,=P'0'              RESET LOWER LEVEL COUNTERS           
         ZIC   R2,ROLPRIC                  POSITIONING FOR ROLLER               
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
         CLI   QOPT4,3                     DO I NEED THIS LEVEL FOR             
         BL    PUTROLL                     SUMMARY                              
         XC    BUFFADD,BUFFADD                                                  
         MVI   BUFMRK1,X'11'               HEADER PIRMARY SORT                  
         MVI   BUFMRK2,3                   LEVEL THREE                          
         MVI   BUFLW1,C'L'                 MARK AS LOWEST LEVEL                 
         CLI   QOPT4,3                     IS THIS LOWEST LEVEL                 
         BE    *+8                                                              
         MVI   BUFLW1,C'H'                 MARK AS LOWEST LEVEL                 
         BAS   RE,PUTBUFF                  PUT REC OUT FOR SUMMARY              
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              PRIMARY SORT   (MUST BE 1C)                           *          
*              LAST FOR LEVEL 3                                      *          
*--------------------------------------------------------------------*          
LVC400   CLI   CSTTOTD,0                   CHECK LEVEL COUNTERS TO SEE          
         BE    LVC410                      IF I NEED TO PRINT THIS              
***R     CP    REC4CONT,=P'1'              LEVEL TOTAL                          
***R     BNH   LVC460                                                           
         B     LVC420                                                           
LVC410   CP    TRNSCONT,=P'1'                                                   
         BNH   LVC460                                                           
LVC420   MVI   FORCEHED,C'N'                                                    
         BAS   RE,CIAREPRT                                                      
         MVC   WORK(L'TOTAL),TOTAL                                              
         MVC   WORK+L'TOTAL(L'SRTKEY1),SRTKEY1                                  
         MVC   WORK+L'TOTAL+L'SRTKEY1+1(L'SRTDATA),SAVCNAM                      
         BAS   RE,SQUASHIT                                                      
         BAS   RE,CHOPIT1                                                       
         MVC   PNAME1(L'WORK1A),WORK1A                                          
         MVC   PNAME1+L'XP+3(L'WORK1B),WORK1B                                   
         ZIC   R2,ROLPRIC                  ROLLER POSITION                      
         BAS   RE,FORMAT                                                        
         BAS   RE,CIAREPRT                                                      
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
LVC460   DS    0H                                                               
         CLI   QOPT4,3                     DO I NEED THIS LEVEL FOR             
         BL    CIA750                      SUMMARY                              
         ZIC   R2,ROLPRIC                                                       
         BAS   RE,FORMAT                                                        
         MVI   BUFMRK1,X'22'               TRAILER PRIMARY SORT                 
         MVI   BUFMRK2,3                   LEVEL THREE                          
         CLI   QOPT4,3                     IS THIS LOWEST LEVEL                 
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                 MARK AS LOWEST LEVEL                 
         BAS   RE,PUTBUFF                  PUT REC OUT FOR SUMMARY              
         MVC   XP,XSPACES                  CLEAR UNPRINTED DATA FROM            
         B     CIA750                      PRINT LINE                           
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*              PRIMARY SORT   (MUST BE 1C)                           *          
*              FIRST FOR LEVEL 4                                     *          
*--------------------------------------------------------------------*          
LVD100   CLI   SRTTYP,C'D'                  1C LEVEL D                          
         BNE   SUBA100                                                          
         CLC   SINVNUM,=12X'FF'             LAST FOR THIS LEVEL                 
         BE    LVD400                                                           
         MVC   MYHEAD7,XSPACES                                                  
         ZIC   R2,CSTTOTD                   GET LEVEL NAME                      
         BAS   RE,GETNAME                                                       
         MVC   SAVDNAM,SRTDATA                                                  
         MVC   MYHEAD7(15),DCSTNAM                                              
         ZIC   R2,CSTTOTD                                                       
         BCTR  R2,0                                                             
         EXMVC R2,MYHEAD7+16,SRTKEY1                                            
         MVC   MYHEAD7+29(L'SRTDATA),SRTDATA                                    
*H       MVC   WORK(L'DCSTNAM),DCSTNAM                                          
*H       MVC   WORK+L'DCSTNAM+1(L'SRTKEY1),SRTKEY1                              
*H       MVC   WORK+L'SRTKEY1+L'DCSTNAM+2(L'SRTDATA),SRTDATA                    
*H       BAS   RE,SQUASHIT                                                      
*H       EXMVC R2,MYHEAD7+1,WORK           SQUASHED DATA TO PRINT LINE          
***R     AP    REC4CONT,=P'1'              INCREMENT PROD COUNTER AND           
***R     ZAP   LEVACONT,=P'0'              RESET LOWER LEVEL COUNTERS           
         ZIC   R2,ROLPRID                  POSITIONING FOR ROLLER               
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
         CLI   QOPT4,4                     DO I NEED THIS LEVEL FOR             
         BL    PUTROLL                     SUMMRAY                              
         XC    BUFFADD,BUFFADD                                                  
         MVI   BUFMRK1,X'11'               HEADER PRIMARY SORT                  
         MVI   BUFMRK2,4                   LEVEL FOUR                           
         MVI   BUFLW1,C'L'                 MUST BE LOWEST LEVEL                 
         BAS   RE,PUTBUFF                  PUT REC OUT FOR SUMMARY              
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              PRIMARY SORT   (MUST BE 1C)                           *          
*              LAST FOR LEVEL 4                                      *          
*--------------------------------------------------------------------*          
LVD400   DS    0H                          CHECK COUNTERS TO SEE IF I           
***R     CP    LEVACONT,=P'1'              NEED TO PRINT THIS TOTAL             
***R     BNH   LVD460                                                           
LVD420   MVI   FORCEHED,C'N'               ACCT AND NAME TO PRINT LINE          
         BAS   RE,CIAREPRT                 ACCT AND NAME TO PRINT LINE          
         MVC   WORK(L'TOTAL),TOTAL                                              
         MVC   WORK+L'TOTAL(L'SRTKEY1),SRTKEY1                                  
         MVC   WORK+L'TOTAL+L'SRTKEY1+1(L'SRTDATA),SAVDNAM                      
         BAS   RE,SQUASHIT                                                      
         BAS   RE,CHOPIT1                                                       
         MVC   PNAME1(L'WORK1A),WORK1A                                          
         MVC   PNAME1+L'XP+3(L'WORK1B),WORK1B                                   
         ZIC   R2,ROLPRID                  ROLLER POSITION                      
         BAS   RE,FORMAT                                                        
         BAS   RE,CIAREPRT                                                      
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
LVD460   DS    0H                                                               
         CLI   QOPT4,4                     DO I NEED THIS LEVEL FOR             
         BL    CIA750                      SUMMARY                              
         ZIC   R2,ROLPRID                                                       
         BAS   RE,FORMAT                                                        
         MVI   BUFMRK1,X'22'               TRAILER PRIMARY SORT                 
         MVI   BUFMRK2,4                   LEVEL FOUR                           
         MVI   BUFLW1,C'L'                 MUST BE LOWEST LEVEL                 
         BAS   RE,PUTBUFF                  PUT REC OUT TO BUFFALO FOR           
         MVC   XP,XSPACES                  SUMMARY                              
         B     CIA750                                                           
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        PROCESSING SJ/SI AND 1C/SI SORTS                            *          
*              PROCESS SECONDARY SORTRECS                            *          
*--------------------------------------------------------------------*          
*--------------------------------------------------------------------*          
*              SECONDARY SORTRECS LEVEL 1                            *          
*--------------------------------------------------------------------*          
SUBA100  CLI   SRTTYP,C'1'                 LEVEL A SORTREC                      
         BNE   SUBB100                     ONTO CHECK FOR NEW LEVELS            
         CLC   SINVNUM,=12X'FF'            LAST FOR LEVEL A                     
         BE    SUBA400                                                          
         MVC   SAVINVN,SPACES                                                   
*                                                                               
         MVC   WORK(L'SRTKEY2),SRTKEY2                                          
         MVC   WORK+L'SRTKEY2+1(L'SRTDATA),SRTDATA                              
         BAS   RE,SQUASHIT                                                      
         EXMVC R2,SAVACNM,WORK                                                  
***R     AP    LEVACONT,=P'1'              ADD ONE TO LEVA CNTR                 
***R     ZAP   LEVBCONT,=P'0'              CLEAR ALL LOWER LEVEL                
         ZAP   TRNSCONT,=P'0'                                                   
         ZIC   R2,ROLSECA                  POSITIONING FOR ROLLER               
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
         XC    BUFFADD,BUFFADD             NO AMTS FOR HEADER                   
         MVI   BUFMRK1,X'12'               HEADER SECONDARY SORT                
         MVI   BUFMRK2,1                   LEVEL ONE                            
         CLI   QOPT5,1                     IS THIS LOWEST LEVEL                 
         BL    PUTROLL                                                          
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                 MARK AS LOWEST LEVEL                 
         BAS   RE,PUTBUFF                  SUMM REC TO BUFFALO                  
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              LAST FOR SECONDARY LEVEL 1                            *          
*--------------------------------------------------------------------*          
SUBA400  CLI   BLEVLEN,0                      IF NO B LEVEL, CHECK NUM          
         BE    SUBA500                        OF TRANSACTIONS                   
***R     CP    LEVBCONT,=P'1'                 IF ONLY ONE LEVB ACCT             
***R     BNH   SUBA590                        WITHIN THIS LEVA ACCT             
         B     SUBA530                        TOTAL IS REDUNDANT                
SUBA500  BAS   RE,INVTOT                                                        
         CP    TRNSCONT,=P'1'                 IF ONLY ONE TRNS, TOTAL           
         BNH   SUBA590                        IS REDUNDANT                      
SUBA530  DS    0H                                                               
         BAS   RE,CIAREPRT                                                      
         MVC   WORK(L'TOTAL),TOTAL                                              
         MVC   WORK+L'TOTAL(L'SRTKEY2),SRTKEY2                                  
         MVC   WORK+L'TOTAL+L'SRTKEY2+1(L'SRTDATA),SRTDATA                      
         BAS   RE,SQUASHIT                                                      
         BAS   RE,CHOPIT2                                                       
         MVC   PNAME2(L'WORK2A),WORK2A                                          
         MVC   PNAME2+L'XP+3(L'WORK2B),WORK2B                                   
         ZIC   R2,ROLSECA                ROLLER POSITION                        
         BAS   RE,FORMAT                 TOTALS TO PRINT LINE                   
         BAS   RE,CIAREPRT                                                      
         CLI   SORTOPT,1                                                        
         BE    SUBA590                                                          
         MVC   XP(L'TOTUNDL),TOTUNDL                                            
         BAS   RE,CIAREPRT                                                      
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
SUBA590  EQU   *                                                                
         ZIC   R2,ROLSECA                GET AMTS NEEDED FOR SUMMARY            
         BAS   RE,FORMAT                                                        
         MVC   XP,XSPACES                CLEAR UNWANTED DATA FROM               
         CLI   QOPT5,1                   DO I NEED THIS LEVEL FOR               
         BL    CIA750                    SUMARY                                 
         MVI   BUFMRK1,X'21'             TRAILER SECONDARY SORT                 
         MVI   BUFMRK2,1                 LEVEL ONE                              
         CLI   QOPT5,1                   IS THIS LOWEST LEVEL                   
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'               MARK AS LOWEST LEVEL                   
         BAS   RE,PUTBUFF                                                       
         B     CIA750                    SUMM REC TO BUFFALO                    
         EJECT ,                                                                
*                                                                               
*                                                                               
*--------------------------------------------------------------------*          
*              PROCESS SECONDARY SORTRECS LEVEL 2                    *          
*--------------------------------------------------------------------*          
SUBB100  DS    0H                                                               
         CLI   SRTTYP,C'2'                 SECONDARY SORT, LEVEL 2              
         BNE   SUBC100                                                          
         CLC   SINVNUM,=12X'FF'            LAST FOR THIS LEVEL                  
         BE    SUBB400                     PRINT LAST FOR LEVEL B               
         MVC   SAVINVN,SPACES                                                   
*                                                                               
         MVC   SAVACNM,SPACES                                                   
         MVC   WORK(L'SRTKEY2),SRTKEY2     TO SQUASHER                          
         MVC   WORK+L'SRTKEY2+1(L'SRTDATA),SRTDATA                              
         BAS   RE,SQUASHIT                                                      
         EXMVC R2,SAVACNM,WORK                                                  
***R     AP    LEVBCONT,=P'1'            INCREMENT COUNTER                      
***R     ZAP   LEVCCONT,=P'0'            CLEAR LOWER LEVEL COUNTERS             
         ZAP   TRNSCONT,=P'0'                                                   
         ZIC   R2,ROLSECB                POSITION IN ROLLER                     
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
         CLI   QOPT5,2                   DO I NEED THIS LEVEL FOR               
         BL    PUTROLL                   SUMMARY                                
         XC    BUFFADD,BUFFADD           NO AMTS NEEDED                         
         MVI   BUFMRK1,X'12'             HEADER SECONDARY SORT                  
         MVI   BUFMRK2,2                 LEVEL TWO                              
         CLI   QOPT5,2                   IS THIS LOWEST LEVEL                   
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'               MARK AS LOWEST LEVEL                   
         BAS   RE,PUTBUFF                SUMM REC TO BUFF                       
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              LAST FOR SECONDARY LEVEL 2                            *          
*--------------------------------------------------------------------*          
SUBB400  CLI   CLEVLEN,0                    IF NO LOWER LEVELS CHECK            
         BE    SUBB480                      TRANSACTION COUNTER                 
***R     CP    LEVCCONT,=P'1'                                                   
***R     BNH   SUBB600                                                          
         B     SUBB500                                                          
SUBB480  BAS   RE,INVTOT                                                        
         CP    TRNSCONT,=P'1'                                                   
         BNH   SUBB600                                                          
SUBB500  DS    0H                           PRINT TOTAL                         
         BAS   RE,CIAREPRT                                                      
         MVC   WORK(L'TOTAL),TOTAL                                              
         MVC   WORK+L'TOTAL(L'SRTKEY2),SRTKEY2                                  
         MVC   WORK+L'TOTAL+L'SRTKEY2+1(L'SRTDATA),SRTDATA                      
         BAS   RE,SQUASHIT                                                      
         BAS   RE,CHOPIT2                                                       
         MVC   PNAME2(L'WORK2A),WORK2A                                          
         MVC   PNAME2+L'XP+3(L'WORK2B),WORK2B                                   
         ZIC   R2,ROLSECB                                                       
         BAS   RE,FORMAT                                                        
         BAS   RE,CIAREPRT                                                      
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
SUBB600  DS    0H                           RECS TO BUFFALO FOR                 
         ZIC   R2,ROLSECB                   SUMMARIES                           
         BAS   RE,FORMAT                    RETREIVE THIS LEVEL TOTAL           
         MVC   XP,XSPACES                                                       
         CLI   QOPT5,2                      DO I NEED THIS LEVEL FOR            
         BL    CIA750                       SUMMARY                             
         MVI   BUFMRK1,X'21'                TRAILER SECONDARY SORT              
         MVI   BUFMRK2,2                    LEVEL TWO                           
         CLI   QOPT5,2                      IS THIS LOWEST LEVEL                
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                  MARK AS LOWEST LEVEL                
         BAS   RE,PUTBUFF                   SUMMARY REC TO BUFFALO              
         B     CIA750                                                           
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*              PROCESS SECONDARY SORTRECS LEVEL 3                    *          
*--------------------------------------------------------------------*          
SUBC100  DS    0H                                                               
         CLI   SRTTYP,C'3'                 SECONDARY SORT, LEVEL 2              
         BNE   SUBD100                                                          
         CLC   SINVNUM,=12X'FF'            LAST FOR THIS LEVEL                  
         BE    SUBC400                     PRINT LAST FOR LEVEL C               
         MVC   SAVINVN,SPACES                                                   
*                                                                               
         MVC   SAVACNM,SPACES                                                   
         MVC   WORK(L'SRTKEY2),SRTKEY2     ACCOUNT AND NAME TO SQUASHER         
         MVC   WORK+L'SRTKEY2+1(L'SRTDATA),SRTDATA                              
         BAS   RE,SQUASHIT                                                      
         EXMVC R2,SAVACNM,WORK                                                  
***R     AP    LEVCCONT,=P'1'              INCREMENT COUNTER                    
***R     ZAP   LEVDCONT,=P'0'              CLEAR LOWER LEVEL COUNTERS           
         ZAP   TRNSCONT,=P'0'                                                   
         ZIC   R2,ROLSECC                  POSITION IN ROLLER                   
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
         CLI   QOPT5,3                   DO I NEED THIS LEVEL FOR               
         BL    PUTROLL                   SUMMARY                                
         XC    BUFFADD,BUFFADD           NO AMTS NEEDED                         
         MVI   BUFMRK1,X'12'             HEADER SECONDARY SORT                  
         MVI   BUFMRK2,3                 LEVEL THREE                            
         CLI   QOPT5,3                   IS THIS LOWEST LEVEL                   
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'               MARK AS LOWEST LEVEL                   
         BAS   RE,PUTBUFF                SUMM REC TO BUFF                       
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              LAST FOR SECONDARY LEVEL 3                            *          
*--------------------------------------------------------------------*          
SUBC400  CLI   DLEVLEN,0                  DO I NEED TO PRINT THIS               
         BE    SUBC450                    LEVEL                                 
***R     CP    LEVDCONT,=P'1'                                                   
***R     BNH   SUBC500                                                          
         B     SUBC480                                                          
SUBC450  BAS   RE,INVTOT                                                        
         CP    TRNSCONT,=P'1'                                                   
         BNH   SUBC500                                                          
SUBC480  DS    0H                                                               
         BAS   RE,CIAREPRT                                                      
         MVC   WORK(L'TOTAL),TOTAL           ACCT AND NAME TO PRINT             
         MVC   WORK+L'TOTAL(L'SRTKEY2),SRTKEY2                                  
         MVC   WORK+L'TOTAL+L'SRTKEY2+1(L'SRTDATA),SRTDATA                      
         BAS   RE,SQUASHIT                                                      
         BAS   RE,CHOPIT2                                                       
         MVC   PNAME2(L'WORK2A),WORK2A                                          
         MVC   PNAME2+L'XP+3(L'WORK2B),WORK2B                                   
         ZIC   R2,ROLSECC                 ROLLER POSITION                       
         BAS   RE,FORMAT                                                        
         BAS   RE,CIAREPRT                                                      
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
SUBC500  DS    0H                                                               
         ZIC   R2,ROLSECC                 TOTALS FOR SUMMARY REC                
         BAS   RE,FORMAT                                                        
         MVC   XP,XSPACES                                                       
         CLI   QOPT5,3                    DO I NEED THIS LEVEL FOR              
         BL    CIA750                     SUMMARY                               
         MVI   BUFMRK1,X'21'              TRAILER SECONDARY SORT                
         MVI   BUFMRK2,3                  LEVEL THREE                           
         CLI   QOPT5,3                    IS THIS LOWEST LEVEL                  
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                MARK AS LOWEST LEVEL                  
         BAS   RE,PUTBUFF                 SUMMARY REC TO BUFFALO                
         B     CIA750                                                           
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*              PROCESS SECONDARY SORTRECS LEVEL 4                    *          
*--------------------------------------------------------------------*          
SUBD100  DS    0H                                                               
         CLI   SRTTYP,C'4'                                                      
         BNE   TRNA100                                                          
         CLC   SINVNUM,=12X'FF'            LAST FOR LEVEL D                     
         BE    SUBD400                     PRINT LAST FOR LEVEL D               
         MVC   SAVINVN,SPACES                                                   
*                                                                               
         MVC   SAVACNM,SPACES                                                   
         MVC   WORK(L'SRTKEY2),SRTKEY2                                          
         MVC   WORK+L'SRTKEY2+1(L'SRTDATA),SRTDATA                              
         BAS   RE,SQUASHIT                                                      
         EXMVC R2,SAVACNM,WORK                                                  
***R     AP    LEVDCONT,=P'1'                                                   
         ZAP   TRNSCONT,=P'0'                                                   
         ZIC   R2,ROLSECD                  POSITION IN ROLLER                   
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
         CLI   QOPT5,4                   DO I NEED THIS LEVEL FOR               
         BL    PUTROLL                   SUMMARY                                
         XC    BUFFADD,BUFFADD           NO AMTS NEEDED                         
         MVI   BUFMRK1,X'12'             HEADER SECONDARY SORT                  
         MVI   BUFMRK2,4                 LEVEL FOUR                             
         MVI   BUFLW1,C'L'               MUST BE LOWEST LEVEL                   
         BAS   RE,PUTBUFF                SUMM REC TO BUFF                       
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              LAST FOR SECONDARY LEVEL 4                            *          
*--------------------------------------------------------------------*          
SUBD400  DS    0H                          CHECK COUNTERS                       
         BAS   RE,INVTOT                                                        
         CP    TRNSCONT,=P'1'              DO I NEED TO PRINT THIS TTL          
         BNH   SUBD500                                                          
SUBD450  DS    0H                                                               
         BAS   RE,CIAREPRT                                                      
         MVC   WORK(L'TOTAL),TOTAL          ACCT AND NAME TO PRNT               
         MVC   WORK+L'TOTAL(L'SRTKEY2),SRTKEY2                                  
         MVC   WORK+L'TOTAL+L'SRTKEY2+1(L'SRTDATA),SRTDATA                      
         BAS   RE,SQUASHIT                                                      
         BAS   RE,CHOPIT2                                                       
         MVC   PNAME2(L'WORK2A),WORK2A                                          
         MVC   PNAME2+L'XP+3(L'WORK2B),WORK2B                                   
         ZIC   R2,ROLSECD                  POSITION IN ROLLER                   
         BAS   RE,FORMAT                                                        
         BAS   RE,CIAREPRT                                                      
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
SUBD500  DS    0H                                                               
         ZIC   R2,ROLSECD                  GET TOTALS FOR SUMMARY REC           
         BAS   RE,FORMAT                                                        
         MVC   XP,XSPACES                                                       
         CLI   QOPT5,4                     DO I NEED THIS LEVEL FOR             
         BL    CIA750                      SUMMARY                              
         MVI   BUFMRK1,X'21'               TRAILER SECONDARY SORT               
         MVI   BUFMRK2,4                   LEVEL FOUR                           
         MVI   BUFLW1,C'L'                 MUST BE LOWEST LEVEL                 
         BAS   RE,PUTBUFF                  SUMM REC TO BUFFALO                  
         B     CIA750                                                           
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*              PROCESS TRANACTION SORTRECS FOR SJ/SI , 1C/SI         *          
*--------------------------------------------------------------------*          
TRNA100  EQU   *                                                                
         BAS   RE,INVTOT                   CHECK INVOICE TOTALS                 
*                                                                               
TRAN300  DS    0H                                                               
         CLC   SAVACNM,SPACES                                                   
         BE    TRNA400                                                          
         BAS   RE,CIAREPRT                                                      
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SAVACNM),SAVACNM                                          
         BAS   RE,CHOPIT2                                                       
         MVC   PNAME2(L'WORK2A),WORK2A                                          
         MVC   PNAME2+L'XP(L'WORK2B),WORK2B                                     
         MVC   SAVACNM,SPACES                                                   
*                                                                               
TRNA400  DS    0H                                                               
         MVC   SAVINVN,SINVNUM                                                  
         ZIC   R2,NUMOFINV                                                      
         LA    R2,1(,R2)                                                        
         STC   R2,NUMOFINV                                                      
         BAS   RE,TRANPRNT                    ALL TRANSACTION DATA TO           
         B     CIA750                         PRINT LINE                        
*                                                                               
*--------------------------------------------------------------------*          
*        AT EOF PRINT REQUEST TOTAL                                  *          
*--------------------------------------------------------------------*          
PRNTREQ  CLI   QOPT3,C'Y'                                                       
         BE    PREQ300                                                          
         MVI   PRNTBF,C'Y'                WILL GIVE ALTERNATE BOXES             
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   XP,XSPACES                                                       
*                                                                               
         USING PLINED,R4                                                        
*                                                                               
         LA    R4,XP                                                            
         MVC   PNAME1(13),=C'REQUEST TOTAL'                                     
         ZIC   R2,ROLREPT                                                       
         BAS   RE,FORMAT                                                        
         MVC   SAVEREQT,XP                                                      
         BAS   RE,CIAREPRT                                                      
*                                                                               
PREQ300  BAS   RE,PRTBUFF                 PRINT SUMMARY                         
         B     CIAXIT                                                           
*                                                                               
         EJECT ,                                                                
*--------------------------------------------------------------------*          
* PROCESS SI/SJ AND SI/1C SORTS                                      *          
*  FOR ALL SORTS THERE IS A PRIMARY SORT AND SECONDARY SORT -        *          
*  WITH IN EACH THERE IS A MAX POSSIBILITY OF FOUR LEVELS            *          
*  (EIGHT TOTAL) BEFORE YOU GET TO THE TRANSACTION LEVEL             *          
*--------------------------------------------------------------------*          
*--------------------------------------------------------------------*          
*              PROCESS PRIMARY SORTRECS                              *          
*--------------------------------------------------------------------*          
*                                                                               
SISJ100  DS    0H                                                               
         MVC   WORK,SPACES                                                      
         CLI   SRTTYP,C'T'                 TRANSACTIONS                         
         BE    SISJ700                                                          
         CLC   SORTKEY,LASTSORT            TEST AGAINST LAST SORTREC            
         BE    CIA750                      DUPLICATE RECS WILL OCCUR            
         MVC   LASTSORT,SORTREC            SAVE FOR NEXT PASS                   
*                                                                               
*--------------------------------------------------------------------*          
*              SI LEVEL A SORTRECS                                   *          
*              FIRST FOR LEVEL A OF SI                               *          
*--------------------------------------------------------------------*          
SISJ105  CLI   SRTTYP,C'1'                                                      
         BNE   SISJ200                                                          
         CLC   SINVNUM,=12X'FF'           LAST FOR LEVEL A                      
         BE    SISJ150                                                          
*                                                                               
SISJ110  DS    0H                                                               
         CLI   LOWSI,1                    IS THIS LOWEST LEV OF SI              
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   MYHEAD4+9(12),SRTKEY1      ACCT AND NAME TO HEADLINE             
         MVC   MYHEAD4+22(36),SRTDATA                                           
***R     AP    LEVACONT,=P'1'             INCREMENT COUNTER                     
***R     ZAP   LEVBCONT,=P'0'             CLEAR LOWER LEVEL COUNTERS            
***R     ZAP   REC1CONT,=P'0'                                                   
         MVC   SAVECLT,SPACES             NEW SI ACCT/NEW CLI                   
         ZIC   R2,ROLPRIA                 ROLLER POSITION                       
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
         CLI   QOPT4,1                    DO I NEED THIS LEVEL FOR              
         BL    PUTROLL                    SUMMARY                               
         XC    BUFFADD,BUFFADD            NO AMTS ON HEADER REC                 
         MVI   BUFMRK1,X'11'              HEADER PRIMARY SORT                   
         MVI   BUFMRK2,1                  LEVEL ONE                             
         MVI   BUFLW1,C'L'                IS THIS LOW OR NOT LOW                
         CLI   QOPT4,1                                                          
         BE    *+8                                                              
         MVI   BUFLW1,C'H'                                                      
         BAS   RE,PUTBUFF                 PUT REC TO BUFFALO                    
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              LAST FOR LEVEL A OF SI                                *          
*--------------------------------------------------------------------*          
SISJ150  DS    0H                          CHECK COUNTERS                       
         MVI   FORCEHED,C'N'                                                    
***R     CLI   BLEVLEN,0                   DO I NEED TO PRINT THIS              
***R     BE    SISJ160                     LEVEL TOTAL                          
***R     CP    LEVBCONT,=P'1'                                                   
***R     BNH   SISJ190                                                          
***R     B     SISJ170                                                          
SISJ160  DS    0H                                                               
***R     CP    REC1CONT,=P'1'                                                   
***R     BNH   SISJ190                                                          
SISJ170  BAS   RE,CIAREPRT                 PRINT BLANK LINE                     
         MVC   WORK(L'TOTAL),TOTAL                                              
         MVC   WORK+L'TOTAL(L'SRTKEY1),SRTKEY1    ACC AND NAME TO PRNT          
         MVC   WORK+L'TOTAL+L'SRTKEY1(L'SRTDATA),SRTDATA                        
         BAS   RE,SQUASHIT                 SQUASH ACCT AND NAME                 
         BAS   RE,CHOPIT1                                                       
         MVC   PNAME1(L'WORK1A),WORK1A                                          
         MVC   PNAME1+L'XP+3(L'WORK1B),WORK1B                                   
         ZIC   R2,ROLPRIA                  POSITION IN ROLLER                   
         BAS   RE,FORMAT                                                        
         BAS   RE,CIAREPRT                                                      
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
SISJ190  DS    0H                                                               
         ZIC   R2,ROLPRIA                  NEED TOTALS FOR SUMMARY              
         BAS   RE,FORMAT                                                        
         MVC   XP,XSPACES                                                       
         CLI   QOPT4,1                     DO I NEED THIS LEVEL FOR             
         BL    CIA750                      SUMMARY                              
         MVI   BUFMRK1,X'22'               TRAILER PRIMARY SORT                 
         MVI   BUFMRK2,1                                                        
         CLI   QOPT4,1                                                          
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                                                      
         BAS   RE,PUTBUFF                  SUM REC TO BUFF                      
         B     CIA750                                                           
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*              SI LEVEL B SORTRECS                                   *          
*              FIRST FOR LEVEL B OF SI                               *          
*--------------------------------------------------------------------*          
SISJ200  DS    0H                                                               
         CLI   SRTTYP,C'2'                                                      
         BNE   SISJ300                                                          
         CLC   SINVNUM,=12X'FF'               LAST FOR LEVEL B                  
         BE    SISJ250                                                          
*                                                                               
         CLI   LOWSI,2                        IS THIS LOWEST LEV OF SI          
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   MYHEAD5+9(12),SRTKEY1          KEY AND DATA TO HEADLINE          
         MVC   MYHEAD5+22(36),SRTDATA                                           
***R     AP    LEVBCONT,=P'1'                 INCREMENT LEVEL COUNTER           
***R     ZAP   LEVCCONT,=P'0'                                                   
***R     ZAP   REC1CONT,=P'0'                                                   
         MVC   SAVECLT,SPACES                                                   
         ZIC   R2,ROLPRIB                     POSITION IN ROLLER                
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
         CLI   QOPT4,2                        DO I NEED THIS LEVEL FOR          
         BL    PUTROLL                        SUMMARY                           
         XC    BUFFADD,BUFFADD                NO AMTS ON HEADER REC             
         MVI   BUFMRK1,X'11'                  HEADER PRIMARY SORT               
         MVI   BUFMRK2,2                                                        
         MVI   BUFLW1,C'L'                                                      
         CLI   QOPT4,2                                                          
         BE    *+8                                                              
         MVI   BUFLW1,C'H'                                                      
         BAS   RE,PUTBUFF                     SUM REC TO BUFFALO                
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              LAST FOR LEVEL B OF SI                                *          
*--------------------------------------------------------------------*          
SISJ250  DS    0H                             CHECK COUNTERS                    
         MVI   FORCEHED,C'N'                                                    
***R     CLI   CLEVLEN,0                      DO I NEED TO PRINT THIS           
***R     BE    SISJ260                        LEVEL TOTAL                       
***R     CP    LEVCCONT,=P'1'                                                   
***R     BNH   SISJ290                                                          
***R     B     SISJ270                                                          
SISJ260  DS    0H                                                               
***R     CP    REC1CONT,=P'1'                                                   
***R     BNH   SISJ290                                                          
SISJ270  BAS   RE,CIAREPRT                                                      
         MVC   WORK(L'TOTAL),TOTAL                                              
         MVC   WORK+L'TOTAL(L'SRTKEY1),SRTKEY1                                  
         MVC   WORK+L'TOTAL+L'SRTKEY1+1(L'SRTDATA),SRTDATA                      
         BAS   RE,SQUASHIT                                                      
         BAS   RE,CHOPIT1                                                       
         MVC   PNAME1(L'WORK1A),WORK1A                                          
         MVC   PNAME1+L'XP+3(L'WORK1B),WORK1B                                   
         ZIC   R2,ROLPRIB                     ROLLER POSITION                   
         BAS   RE,FORMAT                                                        
         BAS   RE,CIAREPRT                                                      
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
SISJ290  DS    0H                                                               
         ZIC   R2,ROLPRIB                     GET AMTS FOR SUMMARY REC          
         BAS   RE,FORMAT                                                        
         MVC   XP,XSPACES                                                       
         CLI   QOPT4,2                        DO I NEED THIS LEVEL FOR          
         BL    CIA750                         SUMMARY                           
         MVI   BUFMRK1,X'22'                  TRAILER PRIMARY SORT              
         MVI   BUFMRK2,2                                                        
         CLI   QOPT4,2                                                          
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                                                      
         BAS   RE,PUTBUFF                     SUM REC TO BUFFALO                
         B     CIA750                                                           
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*              SI LEVEL C SORTRECS                                   *          
*              FIRST FOR LEVEL C OF SI                               *          
*--------------------------------------------------------------------*          
SISJ300  DS    0H                                                               
         CLI   SRTTYP,C'3'                                                      
         BNE   SISJ400                                                          
         CLC   SINVNUM,=12X'FF'              LAST FOR LEVEL C                   
         BE    SISJ350                                                          
*                                                                               
         CLI   LOWSI,3                       IS THIS LOWEST LEVEL OF SI         
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   MYHEAD6+9(12),SRTKEY1         ACCT AND NAME TO HEADLINE          
         MVC   MYHEAD6+22(36),SRTDATA                                           
***R     AP    LEVCCONT,=P'1'                INCREMENT LEVEL COUNTER            
***R     ZAP   LEVDCONT,=P'0'                                                   
***R     ZAP   REC1CONT,=P'0'                                                   
         MVC   SAVECLT,SPACES                                                   
         ZIC   R2,ROLPRIC                    ROLLER POSITION                    
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
         CLI   QOPT4,3                       DO I NEED THIS LEVEL FOR           
         BL    PUTROLL                       SUMMARY                            
         XC    BUFFADD,BUFFADD               NO AMTS ON HEADER REC              
         MVI   BUFMRK1,X'11'                 HEADER PRIMARY SORT                
         MVI   BUFMRK2,3                                                        
         MVI   BUFLW1,C'L'                                                      
         CLI   QOPT4,3                                                          
         BE    *+8                                                              
         MVI   BUFLW1,C'H'                                                      
         BAS   RE,PUTBUFF                    SUM REC TO BUFFALO                 
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              LAST FOR LEVEL C OF SI                                *          
*--------------------------------------------------------------------*          
SISJ350  DS    0H                            CHECK COUNTERS                     
         MVI   FORCEHED,C'N'                                                    
***R     CLI   DLEVLEN,0                     DO I NEED TO PRINT THIS            
***R     BE    SISJ360                       LEVEL TOTAL                        
***R     CP    LEVDCONT,=P'1'                                                   
***R     BNH   SISJ390                                                          
***R     B     SISJ370                                                          
SISJ360  DS    0H                                                               
***R     CP    REC1CONT,=P'1'                                                   
***R     BNH   SISJ390                                                          
SISJ370  BAS   RE,CIAREPRT                                                      
         MVC   WORK(L'TOTAL),TOTAL         ACCT AND NAME TO PRNT                
         MVC   WORK+L'TOTAL(L'SRTKEY1),SRTKEY1                                  
         MVC   WORK+L'TOTAL+L'SRTKEY1+1(L'SRTDATA),SRTDATA                      
         BAS   RE,SQUASHIT                                                      
         BAS   RE,CHOPIT1                                                       
         MVC   PNAME1(L'WORK1A),WORK1A                                          
         MVC   PNAME1+L'XP+3(L'WORK1B),WORK1B                                   
         ZIC   R2,ROLPRIC                    ROLLER POSITION                    
         BAS   RE,FORMAT                     TOTALS TO PRNT LINE                
         BAS   RE,CIAREPRT                                                      
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
SISJ390  DS    0H                                                               
         ZIC   R2,ROLPRIC                    GET AMTS FOR SUMMARY REC           
         BAS   RE,FORMAT                                                        
         MVC   XP,XSPACES                                                       
         CLI   QOPT4,3                       DO I NEED THIS LEVEL FOR           
         BL    CIA750                        SUMMARY                            
         MVI   BUFMRK1,X'22'                 TRAILER PRIMARY SORT               
         MVI   BUFMRK2,3                                                        
         CLI   QOPT4,3                                                          
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                                                      
         BAS   RE,PUTBUFF                    SUM REC TO BUFFALO                 
         B     CIA750                                                           
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*              SI LEVEL D SORTRECS                                   *          
*              FIRST FOR LEVEL D OF SI                               *          
*--------------------------------------------------------------------*          
SISJ400  DS    0H                                                               
         CLI   SRTTYP,C'4'                                                      
         BNE   SISJ500                                                          
         CLC   SINVNUM,=12X'FF'            LAST FOR LEVEL D                     
         BE    SISJ450                                                          
*                                                                               
         MVI   FORCEHED,C'Y'               MUST BE LOWEST LEVEL                 
         MVC   MYHEAD7+9(12),SRTKEY1       ACCT AND NAME TO HEADLINES           
         MVC   MYHEAD7+22(36),SRTDATA                                           
***R     AP    LEVDCONT,=P'1'              INCREMENT LEVEL COUNTER              
***R     ZAP   REC1CONT,=P'0'                                                   
         MVC   SAVECLT,SPACES                                                   
         ZIC   R2,ROLPRID                  ROLLER POSITION                      
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
         CLI   QOPT4,4                       DO I NEED THIS LEVEL FOR           
         BL    PUTROLL                       SUMMARY                            
         XC    BUFFADD,BUFFADD               NO AMTS ON HEADER REC              
         MVI   BUFMRK1,X'11'                 HEADER PRIMARY SORT                
         MVI   BUFMRK2,4                                                        
         MVI   BUFLW1,C'L'                                                      
         BAS   RE,PUTBUFF                    SUM REC TO BUFFALO                 
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              LAST FOR LEVEL D OF SI                                *          
*--------------------------------------------------------------------*          
SISJ450  DS    0H                           CHECK COUNTERS                      
         MVI   FORCEHED,C'N'                                                    
***R     CP    REC1CONT,=P'1'               DO I NEED TO PRINT THIS             
***R     BNH   SISJ490                      LEVEL TOTAL                         
         BAS   RE,CIAREPRT                                                      
         MVC   WORK(L'TOTAL),TOTAL         ACCT AND NAME TO PRNT                
         MVC   WORK+L'TOTAL(L'SRTKEY1),SRTKEY1                                  
         MVC   WORK+L'TOTAL+L'SRTKEY1+1(L'SRTDATA),SRTDATA                      
         BAS   RE,SQUASHIT                                                      
         BAS   RE,CHOPIT1                                                       
         MVC   PNAME1(L'WORK1A),WORK1A                                          
         MVC   PNAME1+L'XP+3(L'WORK1B),WORK1B                                   
         ZIC   R2,ROLPRID                   ROLLER POSITION                     
         BAS   RE,FORMAT                    $ AMTS TO PRINT LINE                
         BAS   RE,CIAREPRT                                                      
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
SISJ490  DS    0H                                                               
         ZIC   R2,ROLPRID                   GET AMTS FOR SUMMARY REC            
         BAS   RE,FORMAT                                                        
         MVC   XP,XSPACES                                                       
         CLI   QOPT4,4                      DO I NEED THIS LEVEL FOR            
         BL    CIA750                       SUMMARY                             
         MVI   BUFMRK1,X'22'                TRAILER PRIMARY SORT                
         MVI   BUFMRK2,4                                                        
         MVI   BUFLW1,C'L'                                                      
         BAS   RE,PUTBUFF                   SUMMARY REC TO BUFFALO              
         B     CIA750                                                           
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*        PROCESS SI/SJ AND SI/1C SORTS                               *          
*              PROCESS SECONDARY SORTRECS                            *          
*--------------------------------------------------------------------*          
*--------------------------------------------------------------------*          
*              LEVEL 1 FOR SECONDARY SORT (EITHER SJ OR 1C)          *          
*--------------------------------------------------------------------*          
SISJ500  DS    0H                                                               
         CLI   SRTTYP,C'A'                                                      
         BNE   SISJ600                                                          
         CLC   SINVNUM,=12X'FF'           LAST FOR THIS LEVEL                   
         BE    SISJ550                                                          
         MVC   SAVINVN,SPACES                                                   
*                                                                               
         CLI   SORTOPT,4                  IS SORT SI/1C                         
         BE    SISJ520                                                          
         CLC   SAVECLT,SCLI2              ONLY PROCESS IF CLIENT                
         BE    CIA750                     HAS CHANGED                           
         MVC   SAVECLT,SCLI2                                                    
*                                                                               
SISJ520  DS    0H                                                               
         BAS   RE,CIAREPRT                PRINT BLANK LINE                      
         MVC   SAVACNM,SPACES                                                   
         ZIC   R2,CLILEN                  GET LEVEL 1 NAME                      
         BAS   RE,GETNAME                                                       
         ZIC   R2,CLILEN                                                        
         BCTR  R2,0                                                             
         EXMVC R2,WORK,SRTKEY2             ACCT AND NAME TO PRNT                
         MVC   WORK+L'SRTKEY2+1(L'SRTDATA),SRTDATA                              
         BAS   RE,SQUASHIT                                                      
         EXMVC R2,SAVACNM,WORK                                                  
         CLI   SORTOPT,4                                                        
         BE    SISJ530                                                          
         MVC   PNAME1(L'SAVACNM),SAVACNM                                        
         BAS   RE,CIAREPRT                                                      
*                                                                               
SISJ530  DS    0H                                                               
***R     AP    REC1CONT,=P'1'             INCREMENT COUNTER                     
***R     ZAP   REC2CONT,=P'0'                                                   
         ZAP   TRNSCONT,=P'0'                                                   
         MVC   SAVANAM,SRTDATA                                                  
         ZIC   R2,ROLSECA                 ROLLER POSITION                       
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
         CLI   QOPT5,1                    DO I NEED THIS LEVEL FOR              
         BL    PUTROLL                    SUMMARY                               
         XC    BUFFADD,BUFFADD            NO AMTS ON HEADER REC                 
         MVI   BUFMRK1,X'12'              HEADER SECONDARY SORT                 
         MVI   BUFMRK2,1                                                        
         CLI   QOPT5,1                                                          
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                                                      
         BAS   RE,PUTBUFF                 SUM REC TO BUFFALO                    
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              LAST FOR LEVEL 1  (EITHER SJ OR 1C)                   *          
*--------------------------------------------------------------------*          
SISJ550  DS    0H                         CHECK COUNTERS                        
         CLI   CSTTOTB,0                                                        
         BNE   SISJ555                                                          
         BAS   RE,INVTOT                                                        
***R     CP    REC2CONT,=P'1'             DO I NEED TO PRINT THIS LEVEL         
***R     BNH   SISJ590                    TOTAL                                 
*                                                                               
SISJ555  DS    0H                                                               
         BAS   RE,CIAREPRT                                                      
         MVC   WORK(L'TOTAL),TOTAL                                              
         ZIC   R2,CSTTOTA                 ACCT AND NAME TO PRNT LINE            
         BCTR  R2,0                                                             
         EXMVC R2,WORK+L'TOTAL,SRTKEY2                                          
         MVC   WORK+L'TOTAL+L'SRTKEY1+1(L'SRTDATA),SAVANAM                      
         BAS   RE,SQUASHIT                                                      
         BAS   RE,CHOPIT2                                                       
         MVC   PNAME2(L'WORK2A),WORK2A                                          
         MVC   PNAME2+L'XP+3(L'WORK2B),WORK2B                                   
         ZIC   R2,ROLSECA                 ROLLER POSITION                       
         BAS   RE,FORMAT                                                        
         BAS   RE,CIAREPRT                                                      
         CLI   SORTOPT,2                                                        
         BE    SISJ580                                                          
         CLI   COSTCLIL,1                                                       
         BNE   SISJ585                                                          
*                                                                               
SISJ580  MVC   XP(L'TOTUNDL),TOTUNDL                                            
*                                                                               
SISJ585  BAS   RE,CIAREPRT                                                      
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
SISJ590  DS    0H                                                               
         CLI   QOPT5,1                    DO I NEED THIS LEVEL FOR              
         BL    CIA750                     SUMMARY                               
         ZIC   R2,ROLSECA                 GET AMTS FOR SUMMARY REC              
         BAS   RE,FORMAT                                                        
         MVI   BUFMRK1,X'21'              TRAILER SECONDARY SORT                
         MVI   BUFMRK2,1                                                        
         CLI   QOPT5,1                                                          
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                                                      
         BAS   RE,PUTBUFF                 SUM REC TO BUFFALO                    
         MVC   XP,XSPACES                 CLEAR UNNEEDED DATA FROM P            
         B     CIA750                                                           
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*              SECONDARY SORTRECS                                    *          
*              FIRST FOR LEVEL 2  (EITHER SJ OR 1C)                  *          
*--------------------------------------------------------------------*          
SISJ600  DS    0H                                                               
         CLI   SRTTYP,C'B'                                                      
         BNE   SISJ700                                                          
         CLC   SINVNUM,=12X'FF'           LAST FOR THIS LEVEL                   
         BE    SISJ650                                                          
         MVC   SAVINVN,SPACES                                                   
*                                                                               
         MVC   SAVACNM,SPACES                                                   
         CLI   SORTOPT,4                                                        
         BNE   SISJ610                                                          
         ZIC   R2,CSTTOTB                 IF 1C IN SORT GET LEV NAME            
         BAS   RE,GETNAME                                                       
         ZIC   R2,CSTTOTB                                                       
         BCTR  R2,0                                                             
         EXMVC R2,WORK,SRTKEY2             ACCT AND NAME TO PRNT LINE           
         MVC   WORK+L'SRTKEY2+1(L'SRTDATA),SRTDATA                              
         B     SISJ620                                                          
*                                                                               
SISJ610  MVC   WORK(L'SPRDKEY2),SPRDKEY2                                        
         MVC   WORK+L'SPRDKEY2+1(L'SRTDATA),SRTDATA                             
*                                                                               
SISJ620  BAS   RE,SQUASHIT                                                      
         EXMVC R2,SAVACNM,WORK                                                  
***R     AP    REC2CONT,=P'1'             INCREMENT LEVEL COUNTER               
***R     ZAP   REC3CONT,=P'0'                                                   
         ZAP   TRNSCONT,=P'0'                                                   
         MVC   SAVBNAM,SRTDATA                                                  
         ZIC   R2,ROLSECB                 ROLLER POSITION                       
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
         CLI   QOPT5,2                    DO I NEED THIS LEVEL FOR              
         BL    PUTROLL                    SUMMARY                               
         XC    BUFFADD,BUFFADD            NO AMTS ON HEADER REC                 
         MVI   BUFMRK1,X'12'              HEADER SECONDARY SORT                 
         MVI   BUFMRK2,2                                                        
         CLI   QOPT5,2                                                          
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                                                      
         BAS   RE,PUTBUFF                 SUM REC TO BUFFALO                    
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              LAST FOR LEVEL 2  (EITHER SJ OR 1C)                   *          
*--------------------------------------------------------------------*          
SISJ650  DS    0H                         CHECK COUNTERS                        
         CLI   SORTOPT,4                  DO I NEED TO PRINT THIS               
         BNE   SISJ660                    LEVEL TOTAL                           
         CLI   CSTTOTC,0                                                        
         BE    SISJ660                                                          
***R     CP    REC3CONT,=P'1'                                                   
***R     BNH   SISJ690                                                          
         B     SISJ670                                                          
SISJ660  BAS   RE,INVTOT                                                        
         CP    TRNSCONT,=P'1'                                                   
         BNH   SISJ686                                                          
SISJ670  BAS   RE,CIAREPRT                                                      
         MVC   WORK(L'TOTAL),TOTAL               PROD AND NAME TO PRNT          
         MVC   WORK+L'TOTAL(L'SPRDKEY),SPRDKEY2                                 
         CLI   SORTOPT,4                                                        
         BNE   *+10                              ELSE                           
         MVC   WORK+L'TOTAL(L'SRTKEY2),SRTKEY2   1C ACCT AND NAME TO            
         MVC   WORK+L'TOTAL+L'SRTKEY2+1(L'SRTDATA),SAVBNAM                      
         BAS   RE,SQUASHIT                                                      
         CLI   SORTOPT,4                                                        
         BE    SISJ683                                                          
         BAS   RE,CHOPIT2                                                       
         MVC   PNAME2(L'WORK2A),WORK2A           NEED INDENTING ON              
         MVC   PNAME2+L'XP+3(L'WORK2B),WORK2B    SI/SJ SIRT                     
         B     SISJ685                                                          
SISJ683  DS    0H                                                               
         BAS   RE,CHOPIT2                                                       
         MVC   PNAME2(L'WORK2A),WORK2A                                          
         MVC   PNAME2+L'XP+3(L'WORK2B),WORK2B                                   
SISJ685  ZIC   R2,ROLSECB                 ROLLER POSITION                       
         BAS   RE,FORMAT                  TOTALS TO PRNT LINE                   
         BAS   RE,CIAREPRT                                                      
SISJ686  CLI   SORTOPT,4                                                        
         BNE   SISJ690                                                          
         CLI   COSTCLIL,2                                                       
         BNE   SISJ690                                                          
         MVC   XP(L'TOTUNDL),TOTUNDL                                            
         BAS   RE,CIAREPRT                                                      
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
SISJ690  DS    0H                                                               
         CLI   QOPT5,2                    DO I NEED THIS LEVEL FOR              
         BL    CIA750                     SUMMARY                               
         ZIC   R2,ROLSECB                 GET SUMMARY AMTS                      
         BAS   RE,FORMAT                                                        
         MVI   BUFMRK1,X'21'              TRAILER SECONDARY SORT                
         MVI   BUFMRK2,2                                                        
         CLI   QOPT5,2                                                          
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                                                      
         BAS   RE,PUTBUFF                 SUMM REC TO BUFFALO                   
         MVC   XP,XSPACES                 FORMT WILL PUT AMTS TO P              
         B     CIA750                     THAT I DON'T WANT                     
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*              SECONDARY SORTRECS                                    *          
*             FIRST FOR LEVEL 3 (MUST BE 1C)                         *          
*--------------------------------------------------------------------*          
SISJ700  DS    0H                                                               
         CLI   SRTTYP,C'C'                  IS THIS LEVEL 3                     
         BNE   SISJ800                                                          
         CLC   SINVNUM,=12X'FF'             LAST FOR LEVEL 3(1C)                
         BE    SISJ750                                                          
         MVC   SAVINVN,SPACES                                                   
*                                                                               
         ZIC   R2,CSTTOTC                   LEVEL 3 LENGTH                      
         BAS   RE,GETNAME                   READ NAME FOR THIS LEVEL            
         MVC   SAVCNAM,SRTDATA              SAVE NAME FOR TOTAL LINE            
         MVC   SAVACNM,SPACES                                                   
         ZIC   R2,CSTTOTC                   MOVE ACCOUNT TO BE                  
         BCTR  R2,0                         SQUASHED                            
         EXMVC R2,WORK,SRTKEY2                                                  
         MVC   WORK+L'SRTKEY2+1(L'SRTDATA),SRTDATA     NAME TO SQUASH           
         BAS   RE,SQUASHIT                                                      
         EXMVC R2,SAVACNM,WORK              SAVE SQUASHED ACCT AND              
***R     AP    REC3CONT,=P'1'               NAME TO KEEP LOWEST LEVEL           
***R     ZAP   REC4CONT,=P'0'                                                   
         ZAP   TRNSCONT,=P'0'                                                   
         ZIC   R2,ROLSECC                                                       
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
         CLI   QOPT5,3                    DO I NEED THIS LEVEL FOR              
         BL    PUTROLL                    SUMMARY                               
         XC    BUFFADD,BUFFADD            NO AMTS ON HEADER REC                 
         MVI   BUFMRK1,X'12'              HEADER SECONDARY SORT                 
         MVI   BUFMRK2,3                                                        
         CLI   QOPT5,3                                                          
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                                                      
         BAS   RE,PUTBUFF                 SUM REC TO BUFFALO                    
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              LAST FOR LEVEL 3  (MUST BE 1C)                        *          
*--------------------------------------------------------------------*          
SISJ750  DS    0H                         CHECK COUNTERS                        
         CLI   CSTTOTD,0                  DO I NEED TO PRINT THIS               
         BE    SISJ760                    LEVEL TOTAL                           
***R     CP    REC4CONT,=P'1'                                                   
***R     BNH   SISJ790                                                          
         B     SISJ770                                                          
SISJ760  BAS   RE,INVTOT                                                        
         CP    TRNSCONT,=P'1'                                                   
         BNH   SISJ780                                                          
SISJ770  BAS   RE,CIAREPRT                                                      
         MVC   WORK(L'TOTAL),TOTAL        ACCT TO PRNT LINE                     
         MVC   WORK+L'TOTAL(L'SRTKEY2),SRTKEY2                                  
         MVC   WORK+L'TOTAL+L'SRTKEY2+1(L'SRTDATA),SAVCNAM                      
         BAS   RE,SQUASHIT                                                      
         BAS   RE,CHOPIT2                                                       
         MVC   PNAME2(L'WORK2A),WORK2A                                          
         MVC   PNAME2+L'XP+3(L'WORK2B),WORK2B                                   
         ZIC   R2,ROLSECC                 ROLLER POSITION                       
         BAS   RE,FORMAT                  LEVEL AMTS TO PRNT LINE               
         BAS   RE,CIAREPRT                                                      
SISJ780  CLI   SORTOPT,4                                                        
         BNE   SISJ790                                                          
         CLI   COSTCLIL,3                                                       
         BNE   SISJ790                                                          
         MVC   XP(L'TOTUNDL),TOTUNDL                                            
         BAS   RE,CIAREPRT                                                      
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
SISJ790  DS    0H                                                               
         CLI   QOPT5,3                    DO I NEED THIS LEVEL FOR              
         BL    CIA750                     SUMMARY                               
         ZIC   R2,ROLSECC                 GET SUMMARY AMTS                      
         BAS   RE,FORMAT                                                        
         MVI   BUFMRK1,X'21'              TRAILER SECONDARY SORT                
         MVI   BUFMRK2,3                                                        
         CLI   QOPT5,3                                                          
         BNE   *+8                                                              
         MVI   BUFLW1,C'L'                                                      
         BAS   RE,PUTBUFF                 SUMM REC TO BUFFALO                   
         MVC   XP,XSPACES                                                       
         B     CIA750                                                           
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*              SECONDARY SORTRECS                                    *          
*              FIRST FOR LEVEL 4 (MUST BE 1C)                        *          
*--------------------------------------------------------------------*          
SISJ800  DS    0H                                                               
         CLI   SRTTYP,C'D'                                                      
         BNE   SISJ900                                                          
         CLC   SINVNUM,=12X'FF'           LAST FOR THIS LEVEL                   
         BE    SISJ850                                                          
         MVC   SAVINVN,SPACES                                                   
*                                                                               
         ZIC   R2,CSTTOTD                 GET THIS LEVEL NAME                   
         BAS   RE,GETNAME                                                       
         MVC   SAVDNAM,SRTDATA            SAVE NAME FOR TOTAL LINE              
         MVC   SAVACNM,SPACES                                                   
         ZIC   R2,CSTTOTD                                                       
         BCTR  R2,0                                                             
         EXMVC R2,WORK,SRTKEY2             NAME AND ACCT TO PRNT LINE           
         MVC   WORK+L'SRTKEY2+1(L'SRTDATA),SRTDATA                              
         BAS   RE,SQUASHIT                                                      
         EXMVC R2,SAVACNM,WORK                                                  
*                                                                               
SISJ820  DS    0H                                                               
***R     AP    REC4CONT,=P'1'             INCREMENT LEVEL COUNTER               
         ZAP   TRNSCONT,=P'0'                                                   
         ZIC   R2,ROLSECD                 ROLLER POSITION                       
*        SEND SUMMARY RECORD TO BUFFALO                                         
         CLI   QOPT5,4                    DO I NEED THIS LEVEL FOR              
         BL    PUTROLL                    SUMMARY                               
         XC    BUFFADD,BUFFADD            NO AMTS ON HEADER REC                 
         MVI   BUFMRK1,X'12'              HEADER SECONDARY SORT                 
         MVI   BUFMRK2,4                                                        
         MVI   BUFLW1,C'L'                MUST BE LOW LEVEL                     
         BAS   RE,PUTBUFF                 SUM REC TO BUFFALO                    
         B     PUTROLL                                                          
*                                                                               
*--------------------------------------------------------------------*          
*              LAST FOR LEVEL 4  (MUST BE 1C)                        *          
*--------------------------------------------------------------------*          
SISJ850  DS    0H                         CHECK COUNTERS                        
         BAS   RE,INVTOT                                                        
         CP    TRNSCONT,=P'1'             DO I NEED TO PRINT THIS               
         BNH   SISJ880                    LEVEL TOTAL                           
         BAS   RE,CIAREPRT                PRINT BLANK LINE                      
         MVC   WORK(L'TOTAL),TOTAL                                              
         MVC   WORK+L'TOTAL(L'SRTKEY2),SRTKEY2          ACCT AND                
         ZIC   R2,CSTTOTC                            NAME TO PRNT LINE          
         BAS   RE,GETNAME                                                       
         MVC   WORK+L'TOTAL+L'SRTKEY2+1(L'SRTDATA),SAVDNAM                      
         BAS   RE,SQUASHIT                                                      
         BAS   RE,CHOPIT2                                                       
         MVC   PNAME2(L'WORK2A),WORK2A                                          
         MVC   PNAME2+L'XP+3(L'WORK2B),WORK2B                                   
         ZIC   R2,ROLSECD                 GET LEVEL TOTALS                      
         BAS   RE,FORMAT                                                        
         BAS   RE,CIAREPRT                                                      
*                                                                               
SISJ880  CLI   SORTOPT,4                                                        
         BNE   SISJ890                                                          
         CLI   COSTCLIL,4                                                       
         BNE   SISJ890                                                          
         MVC   XP(L'TOTUNDL),TOTUNDL                                            
         BAS   RE,CIAREPRT                                                      
*                                                                               
*        SEND SUMMARY RECORD TO BUFFALO                                         
SISJ890  DS    0H                                                               
         CLI   QOPT5,4                    DO I NEED THIS LEVEL FOR              
         BL    CIA750                     SUMMARY                               
         ZIC   R2,ROLSECD                 GET SUMMARY AMTS                      
         BAS   RE,FORMAT                                                        
         MVI   BUFMRK1,X'21'              TRAILER SECONDARY SORT                
         MVI   BUFMRK2,4                                                        
         MVI   BUFLW1,C'L'                MUST BE LOW LEVEL                     
         BAS   RE,PUTBUFF                 SUMM REC TO BUFFALO                   
         MVC   XP,XSPACES                                                       
         B     CIA750                                                           
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*              TRANSACTIONS FOR SI/SJ AND SI/1C SORT                 *          
*--------------------------------------------------------------------*          
SISJ900  DS    0H                                                               
         CLI   SRTTYP,C'T'                                                      
         BNE   SISJ1000                                                         
         BAS   RE,INVTOT                                                        
*                                                                               
SISJ920  CLC   SAVACNM,SPACES         IF FIRST TRAN WILL HAVE ACCT              
         BE    SISJ930                SO                                        
         BAS   RE,CIAREPRT            FIRST PRINT BLANK LINE                    
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SAVACNM),SAVACNM                                          
         BAS   RE,CHOPIT2                                                       
         MVC   PNAME2(L'WORK2A),WORK2A                                          
         MVC   PNAME2+L'XP(L'WORK2B),WORK2B                                     
         MVC   SAVACNM,SPACES         CLEAR ACCT                                
*                                                                               
SISJ930  MVC   SAVINVN,SINVNUM                                                  
         ZIC   R2,NUMOFINV                                                      
         LA    R2,1(,R2)                                                        
         STC   R2,NUMOFINV                                                      
         BAS   RE,TRANPRNT            ALL TRANSACTION INFO TO P LINE            
         MVC   WORK,SPACES            CLEAR WORK SPACE                          
         B     CIA750                                                           
*                                                                               
*              GO GET NEXT SORTREC                                              
SISJ1000 DS    0H                                                               
         B     CIA750                                                           
         EJECT ,                                                                
*                                                                               
*              RUNLAST                                                          
CIA900   CLI   MODE,RUNLAST                                                     
         BNE   CIAXIT                                                           
         BAS   RE,ENDSORT                 CLOSE SORT                            
         B     CIAXIT                                                           
*                                                                               
CIAXIT   XMOD1 1                                                                
*                                                                               
XIT      XIT1                                                                   
         GETEL R4,DATADISP,BYTE                                                 
         EJECT ,                                                                
*                                                                               
*                                                                               
*--------------------------------------------------------------------*          
*        PROCESS TRANSACTION RECORDS RECEIVED FROM SORTER            *          
*        FOR ALL SORTS                                               *          
*        ALL TEXT AND DOLLAR AMOUNTS TO PRINT LINE                   *          
*--------------------------------------------------------------------*          
TRANPRNT NTR1                                                                   
         XC    HALF,HALF                                                        
         MVI   HALF+1,L'TEXT1+1           LENGTH ON PRINT LINE OF ONE           
*                                         TEXT ENTRY                            
         CLI   SVTXPROF+0,C'N'            WAS INVOICE NUM REQUESTED             
         BE    TRN100                                                           
         LA    R2,XP+L'ACCTCOL+3          BEGINNING OF TEXT PORTION OF          
         ZIC   R3,SVTXPROF                PRINT LINE                            
         BCTR  R3,0                                                             
         MH    R3,HALF                    POINT TO COL THEY REQUESTED           
         AR    R2,R3                      INV NUM TO PRINT IN                   
         MVC   2(6,R2),SINVNUM                                                  
*                                                                               
TRN100   CLI   SVTXPROF+1,C'N'            WAS INVOICE DATE REQUESTED            
         BE    TRN150                                                           
         LA    R2,XP+L'ACCTCOL+3          BEGINNING OF TEXT PORTION OF          
         ZIC   R3,SVTXPROF+1              PRINT LINE                            
         BCTR  R3,0                                                             
         MH    R3,HALF                    POINT TO COL THEY REQUESTED           
         AR    R2,R3                      INV DATE TO PRINT IN                  
         GOTO1 DATCON,DMCB,(1,SINVDAT),(5,1(R2))                                
*                                                                               
TRN150   CLI   SVTXPROF+2,C'N'            WAS JOB/EST NUM REQUESTED             
         BE    TRN200                                                           
         LA    R2,XP+L'ACCTCOL+3          BEGINNING OF TEXT PORTION OF          
         ZIC   R3,SVTXPROF+2              PRINT LINE                            
         BCTR  R3,0                                                             
         MH    R3,HALF                    POINT TO COL THEY REQUESTED           
         AR    R2,R3                      NUM TO PRINT IN                       
         MVC   2(6,R2),SJOBEST                                                  
*                                                                               
TRN200   AP    TRNSCONT,=P'1'                                                   
         GOTO1 PROLLER,DMCB,2,ACCUMS,1                                          
         GOTO1 (RF),(R1),1                                                      
         L     R2,DMCB                                                          
         ZAP   0(6,R2),SRTAMT1                                                  
         ZAP   6(6,R2),SRTAMT2                                                  
         ZAP   12(6,R2),SRTAMT3                                                 
         ZAP   18(6,R2),SRTAMT4                                                 
         ZAP   24(6,R2),SRTAMT5                                                 
         ZAP   30(6,R2),SRTAMT6                                                 
         ZAP   36(6,R2),SRTAMT7                                                 
         ZAP   42(6,R2),SRTAMT8                                                 
         ZAP   48(6,R2),SRTAMT9                                                 
*                                                                               
         GOTO1 (RF),(R1),6                                                      
         ZIC   R2,ROLTRAN                                                       
         BAS   RE,FORMAT                                                        
         BAS   RE,CIAREPRT                                                      
         B     XIT                                                              
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        PROCESS INVOICE TOTALS                                      *          
*--------------------------------------------------------------------*          
INVTOT   NTR1                                                                   
         CLI   SVTXPROF+4,C'Y'             DOES PROFILE REQUEST INVOICE         
         BNE   XIT                         TOTALS                               
         CLI   SVTXPROF,C'N'               WAS INVOICE NUM REQUESTED ON         
         BE    XIT                         REPORT                               
         CLC   SAVINVN,SPACES              IS IT FIRST TIME THROUGH?            
         BE    INVT250                                                          
         CLC   SAVINVN,SINVNUM             HAS INVOICE NUM CHANGED              
         BE    INVT300                                                          
         CLI   NUMOFINV,1                  ONE INVOICE NEEDS NO TOTAL           
         BNH   INVT250                                                          
*                                                                               
         XC    HALF,HALF                                                        
         MVI   HALF+1,L'TEXT1+1                                                 
         LA    R1,XP+L'ACCTCOL+3          BEGINNING OF TEXT PORTION OF          
         ZIC   R2,SVTXPROF                PRINT LINE                            
         BCTR  R2,0                                                             
         MH    R2,HALF                    POINT TO COL THEY REQUESTED           
         AR    R1,R2                      INV NUM TO PRINT IN                   
         MVC   2(6,R1),SAVINVN                                                  
         MVI   1(R1),C'*'                                                       
         MVI   8(R1),C'*'                                                       
*                                                                               
         ZIC   R2,ROLINVN                                                       
         BAS   RE,FORMAT                                                        
         CLI   SRTTYP,C'T'                 ONLY WANT TO SPACE WHEN              
         BNE   *+8                         PRINTING TRANSACTIONS                
         MVI   SPACING,2                                                        
         BAS   RE,CIAREPRT                                                      
*                                                                               
INVT250  ZIC   R2,ROLINVN                                                       
         GOTO1 PROLLER,DMCB,2,ACCUMS,(R2)                                       
         MVI   NUMOFINV,0                                                       
*                                                                               
INVT300  DS    0H                                                               
         B     CIAXIT                                                           
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*              ROUTINE PROPAGATES A NEW KEY DOWN ARRAY OF SORT RECS  *          
*--------------------------------------------------------------------*          
REKEY    NTR1                                                                   
         LA    RF,SORTLEVD+L'SORTKEY         ADDRESSED IN RF.                   
         LA    RE,L'SORTREC                  LENGTH FOR BXLE.                   
*                                                                               
         ZIC   R1,CLILEN                                                        
         BCTR  R1,0                                                             
         LA    R2,SCLIKEY-SORTRECD                                              
         CLI   SORTOPT,2                                                        
         BNE   *+8                                                              
         LA    R2,SCLIKEY2-SORTRECD                                             
         CLI   SRTTYP,C'A'                   CLIENT                             
         BE    REK100                                                           
*                                                                               
         ZIC   R1,PRODLEN                                                       
         BCTR  R1,0                                                             
         LA    R2,SPRDKEY-SORTRECD                                              
         CLI   SORTOPT,2                                                        
         BNE   *+8                                                              
         LA    R2,SPRDKEY2-SORTRECD                                             
         CLI   SRTTYP,C'B'                   PRODUCT                            
         BE    REK100                                                           
*                                                                               
         ZIC   R1,ALEVLEN                    EXMVC LENGTH                       
         BCTR  R1,0                                                             
         LA    R2,SRTKEY2-SORTRECD           R2 IS THE SAME FOR ALL             
         CLI   SORTOPT,2                     REMAINING TYPES                    
         BNE   *+8                                                              
         LA    R2,SRTKEY1-SORTRECD                                              
         CLI   SRTTYP,C'1'                                                      
         BE    REK100                                                           
*                                                                               
         ZIC   R1,BLEVLEN                    EXMVC LENGTH                       
         BCTR  R1,0                                                             
         LA    R2,SRTKEY2-SORTRECD           R2 IS THE SAME FOR ALL             
         CLI   SORTOPT,2                     REMAINING TYPES                    
         BNE   *+8                                                              
         LA    R2,SRTKEY1-SORTRECD                                              
         ZIC   R0,ALEVLEN                                                       
         AR    R2,R0                                                            
         CLI   SRTTYP,C'2'                                                      
         BE    REK100                                                           
*                                                                               
         ZIC   R1,CLEVLEN                                                       
         BCTR  R1,0                                                             
         LA    R2,SRTKEY2-SORTRECD           R2 IS THE SAME FOR ALL             
         CLI   SORTOPT,2                     REMAINING TYPES                    
         BNE   *+8                                                              
         LA    R2,SRTKEY1-SORTRECD                                              
         ZIC   R0,BLEVTOT                                                       
         AR    R2,R0                                                            
         CLI   SRTTYP,C'3'                                                      
         BE    REK100                                                           
*                                                                               
         ZIC   R1,DLEVLEN                                                       
         BCTR  R1,0                                                             
         LA    R2,SRTKEY2-SORTRECD           R2 IS THE SAME FOR ALL             
         CLI   SORTOPT,2                     REMAINING TYPES                    
         BNE   *+8                                                              
         LA    R2,SRTKEY1-SORTRECD                                              
         ZIC   R0,CLEVTOT                                                       
         AR    R2,R0                                                            
         CLI   SRTTYP,C'4'                                                      
         BE    REK100                        HAS TO BE ONE OF THESE             
*                                                                               
REK100   LA    R2,0(R2,R6)                                                      
*                                                                               
REK200   LA    R3,L'SORTREC(,R2)                                                
         PRINT GEN                                                              
         EXMVC R1,0(R3),0(R2)                                                   
         BXLE  R2,RE,REK200                                                     
         PRINT NOGEN                                                            
         B     XIT                                                              
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*              PUT REQUESTED LEVELS TO BUFFALO FOR SUMMARY           *          
*--------------------------------------------------------------------*          
PUTBUFF  NTR1                                                                   
         MVI   BUFTYP,X'01'              X'01' - SUMMARY RECS                   
         MVC   BUFAC1,SRTKEY1            PRIMARY SORT KEY                       
         MVC   BUFAC2,SRTKEY2            SECONDARY SORTKEY                      
         MVC   BUFNAME,SRTDATA           ACCT NAME                              
         OC    BUFFADD,BUFFADD           ANY AMTS FOR THIS REC                  
         BNZ   PUTB50                                                           
*                                                                               
         LA    R3,BUFAMTS                IF HEADER REC INITIALIZE               
         LA    R4,BUFCOLS                TEN $COLS FIELDS                       
*                                                                               
PUTB25   ZAP   0(L'BUFAMT1,R3),=P'0'     6 IS HARD LENGTH FROM ROLLER           
         LA    R3,L'BUFAMT1(,R3)                                                
         BCT   R4,PUTB25                                                        
         B     PUTB200                                                          
*                                                                               
PUTB50   L     R2,BUFFADD                ADD OF $AMTS FROM ROLLER               
         LA    R3,BUFAMTS                ADD OF $AMTS IN BUFFREC                
         LA    R4,BUFCOLS-1              NINE AMT FIELDS                        
*                                                                               
PUTB100  ZAP   0(L'BUFAMT1,R3),0(6,R2)   6 IS HARD LENGTH OF ROLLER             
         LA    R3,L'BUFAMT1(,R3)                                                
         LA    R2,6(,R2)                                                        
         BCT   R4,PUTB100                                                       
*                                                                               
PUTB200  DS    0H                                                               
         CLI   SORTOPT,1                                                        
         BNE   PUTB210                                                          
         CLI   QOPT4,1                                                          
         BNE   PUTB800                                                          
         MVC   BUFAC1+6(6),SPACES                                               
         B     PUTB800                                                          
*                                                                               
PUTB210  CLI   SORTOPT,2                                                        
         BNE   PUTB220                                                          
         CLI   QOPT5,1                                                          
         BH    *+10                                                             
         MVC   BUFAC2+6(6),SPACES                                               
         MVC   BUFAC1,SPACES                                                    
*                                                                               
         CLC   QOPT4,LOWSI              HAVE THEY REQUESTED A LOWER             
         BNH   *+10                     LEVEL FOR THEIR SUMMARY THAN            
         MVC   QOPT4,LOWSI              THEY HAVE SET UP                        
*                                                                               
         ZIC   R1,ALEVLEN                                                       
         CLI   QOPT4,1                                                          
         BE    PUTB215                                                          
         ZIC   R1,BLEVTOT                                                       
         CLI   QOPT4,2                                                          
         BE    PUTB215                                                          
         ZIC   R1,CLEVTOT                                                       
         CLI   QOPT4,3                                                          
         BE    PUTB215                                                          
         ZIC   R1,DLEVTOT                                                       
*                                                                               
PUTB215  BCTR  R1,0                                                             
         EXMVC R1,BUFAC1,SRTKEY1                                                
         B     PUTB800                                                          
*                                                                               
PUTB220  CLI   SORTOPT,3                                                        
         BNE   PUTB230                                                          
         MVC   BUFAC1,SPACES                                                    
*                                                                               
         CLC   QOPT4,LOW1C              HAVE THEY REQUESTED A LOWER             
         BNH   *+10                     LEVEL FOR THEIR SUMMARY THAN            
         MVC   QOPT4,LOW1C              THEY HAVE SET UP                        
*                                                                               
         ZIC   R1,CSTTOTA                                                       
         CLI   QOPT4,1                                                          
         BE    PUTB225                                                          
         ZIC   R1,CSTTOTB                                                       
         CLI   QOPT4,2                                                          
         BE    PUTB225                                                          
         ZIC   R1,CSTTOTC                                                       
         CLI   QOPT4,3                                                          
         BE    PUTB225                                                          
         ZIC   R1,CSTTOTD                                                       
*                                                                               
PUTB225  BCTR  R1,0                                                             
         EXMVC R1,BUFAC1,SRTKEY1                                                
         B     PUTB800                                                          
*                                                                               
PUTB230  CLI   SORTOPT,4                                                        
         BNE   PUTB800                                                          
         MVC   BUFAC1,SPACES                                                    
*                                                                               
         CLC   QOPT4,LOWSI              HAVE THEY REQUESTED A LOWER             
         BNH   *+10                     LEVEL FOR THEIR SUMMARY THAN            
         MVC   QOPT4,LOWSI              THEY HAVE SET UP                        
*                                                                               
         ZIC   R1,ALEVLEN                                                       
         CLI   QOPT4,1                                                          
         BE    PUTB235                                                          
         ZIC   R1,BLEVTOT                                                       
         CLI   QOPT4,2                                                          
         BE    PUTB235                                                          
         ZIC   R1,CLEVTOT                                                       
         CLI   QOPT4,3                                                          
         BE    PUTB235                                                          
         ZIC   R1,DLEVTOT                                                       
*                                                                               
PUTB235  BCTR  R1,0                                                             
         EXMVC R1,BUFAC1,SRTKEY1                                                
         B     PUTB800                                                          
*                                                                               
PUTB800  ZAP   BUFAMT10,=P'1'            INSURE RETURN OF ALL BUFFRECS          
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC                               
*----------------------------------------------------------------*              
         CLI   QOPT6,C'B'                                        *              
         BNE   PUTB810                                           *              
         MVC   XP(L'BUFFREC),BUFFREC                             *              
         GOTO1 ACREPORT                                          *              
*                                                                *              
PUTB810  DS    0H                                                *              
*----------------------------------------------------------------*              
         XC    BUFFREC,BUFFREC                                                  
         B     XIT                                                              
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*        PROCESS AND PRINT ALL BUFFRECS FOR SUMMARY                  *          
*--------------------------------------------------------------------*          
PRTBUFF  NTR1                                                                   
         MVC   XP,XSPACES             CLEAR ALL SUMMARY PRINT LINES             
*                                                                               
         MVI   FORCEHED,C'Y'          BEGIN SUMMARY ON NEW PAGE                 
         MVI   PRNTBF,C'Y'            MARK BEGINNING OF SUM PRNT                
         MVI   TOPBOX,C'N'            SET TOP BOX TO NONE                       
         MVC   TOPBOXT+1(L'TOPBOXT-1),XSPACES                                   
         MVC   TOPBOXM+1(L'TOPBOXM-1),XSPACES                                   
         MVC   TOPBOXB+1(L'TOPBOXB-1),XSPACES                                   
         MVI   RCSUBPRG,8             SET SUMMARY SPROG                         
         CLI   SORTOPT,2                                                        
         BNE   *+8                                                              
         MVI   RCSUBPRG,9                                                       
         CLI   SORTOPT,3                                                        
         BNE   *+8                                                              
         MVI   RCSUBPRG,10                                                      
         CLI   SORTOPT,4                                                        
         BNE   *+8                                                              
         MVI   RCSUBPRG,11                                                      
*                                                                               
         MVC   SVBFAMTS,SPACES        FOR SUPPR REDUNDANT TOTALS                
         XC    BUFFREC,BUFFREC        GET FIRST REC FROM BUFFALO                
         MVI   BUFTYP,X'01'                X'01' - SUMMARY RECS                 
         GOTO1 BUFFALO,DMCB,=C'HIGH',(X'01',ABUFF),BUFFREC,1                    
         TM    DMCB+8,X'80'                                                     
         BO    PRTBXIT2                    EOF                                  
         B     PRTB80                                                           
*                                                                               
PRTB50   GOTO1 BUFFALO,DMCB,=C'SEQ',(X'01',ABUFF),BUFFREC,1                     
*                                                                               
PRTB70   TM    DMCB+8,X'80'                                                     
         BO    PRTBXIT                     EOF                                  
*                                                                               
PRTB80   DS    0H                                                               
*----------------------------------------------------------------*              
         CLI   QOPT7,C'B'           TEMPORARILY PRINT BUFFRECS   *              
         BNE   PRTB90               WHEN RETURNED TO ME          *              
         MVC   XP(L'BUFFREC),BUFFREC                             *              
         GOTO1 ACREPORT                                          *              
*                                                                *              
PRTB90   DS    0H                                                *              
*----------------------------------------------------------------*              
         BAS   RE,BUFFORM                  FORMAT OUTPUT                        
         B     PRTB50                                                           
*                                                                               
PRTBXIT  DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   XP,SAVEREQT                                                      
         BAS   RE,BUFREPRT                                                      
*                                                                               
PRTBXIT2 MVI   PRNTBF,C'N'                 MARK END OF SUMMARY PRINT            
         B     XIT                                                              
         EJECT ,                                                                
*                                                                               
*--------------------------------------------------------------------*          
*        FORMAT BUFFRECS FOR SUMMARY - THEY WILL COME BACK FROM      *          
*        BUFFALO IN THIS ORDER :                                     *          
*            HEADER - PRIMARY SORT                                   *          
*              HEADER - SECONDARY SORT                               *          
*              TRAILER - SECONDARY SORT                              *          
*            TRAILER - PRIMARY SORT                                  *          
*        LOWEST LEVEL IS TAGGED - TRAILER REC OF SECONDARY SORT      *          
*        IS WHERE $ AMTS ARE PRINTED                                 *          
*--------------------------------------------------------------------*          
BUFFORM  NTR1                                                                   
*        HEADER BUFFRECS/PRIMARY SORT                                           
BFP100   DS    0H                                                               
         MVC   WORK,SPACES                                                      
         LA    R4,WORK                                                          
*                                                                               
         USING PRBUFFD,R4                                                       
*                                                                               
         TM    BUFMRK1,X'11'              IS THIS HEADER REC                    
         BNO   BFP150                     IF NOT MUST BE SECONDARY              
*                                                                               
         CLI   SORTOPT,1                  IF CLI/INC SORT CHECK IF ONE          
         BNE   BFP105                     CLIENT/PAGE REQUESTED                 
         CLI   SVTXPROF+3,C'Y'                                                  
         BE    BFP105                                                           
         MVI   TOPBOX,C'N'                IF NOT NO PAGE BREAK AND              
         B     BFP125                     NO HEADLINE BOX                       
*                                                                               
BFP105   DS    0H                                                               
         CLI   BUFLW1,C'H'           PUT ALL LEVELS OF PRIMARY SORT             
         BNE   BFP120                EXCEPT LOWEST IN TOPBOX                    
         MVI   FORCEHED,C'Y'         NEW PAGE                                   
         MVI   TOPBOX,C'Y'           SET BOX BEING PRINTED                      
*                                                                               
BFP110   CLI   BUFMRK2,1                 PRIMARY SORT LEVEL 1                   
         BNE   BFP112                                                           
         MVC   BOXAC1(L'BUFAC1),BUFAC1                  ACCT AND NAME           
         MVC   BOXAC1+L'BUFAC1+1(L'BUFNAME),BUFNAME     TO TOP BOX              
         MVC   BTLIN1,BXLIN              BUILD FIRST BOX                        
         MVC   BBLIN1,BXLIN                                                     
         MVC   BMDIV1,MMIDDIV                                                   
         MVC   BTDIV1,MTOPRIG                                                   
         MVC   BBDIV1,MBOTRIG                                                   
         B     BFP500                                                           
*                                                                               
BFP112   CLI   BUFMRK2,2                 PRIMARY SORT LEVEL 2                   
         BNE   BFP114                                                           
         MVC   BOXAC2(L'BUFAC1),BUFAC1                                          
         MVC   BOXAC2+L'BUFAC1+1(L'BUFNAME),BUFNAME                             
         MVC   BTDIV1,MTOPDIV                                                   
         MVC   BBDIV1,MBOTDIV                                                   
         MVC   BTLIN2,BXLIN                                                     
         MVC   BBLIN2,BXLIN                                                     
         MVC   BTDIV2,MTOPRIG                                                   
         MVC   BMDIV2,MMIDDIV                                                   
         MVC   BBDIV2,MBOTRIG                                                   
         B     BFP500                                                           
*                                                                               
BFP114   CLI   BUFMRK2,3                 PRIMARY SORT LEVEL 3                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BOXAC3(L'BUFAC1),BUFAC1                                          
         MVC   BOXAC3+L'BUFAC1+1(L'BUFNAME),BUFNAME                             
         MVC   BTDIV2,MTOPDIV                                                   
         MVC   BBDIV2,MBOTDIV                                                   
         MVC   BTLIN3,BXLIN                                                     
         MVC   BBLIN3,BXLIN                                                     
         MVC   BTDIV3,MTOPRIG                                                   
         MVC   BMDIV3,MMIDDIV                                                   
         MVC   BBDIV3,MBOTRIG                                                   
         B     BFP500                                                           
*                                                                               
BFP120   DS    0H                                                               
         CLI   TOPBOX,C'Y'           IF ANY HEADLINES PRINT FIRST               
         BNE   BFP125                                                           
         MVC   XHEAD5(L'TOPBOXT),TOPBOXT        BOX TOP                         
         MVC   XHEAD6(L'TOPBOXM),TOPBOXM        BOX MID                         
         MVC   XHEAD7(L'TOPBOXB),TOPBOXB        BOX BOTTOM                      
*                                                                               
BFP125   DS    0H                                                               
         MVC   PBUFAC1,BUFAC1             PRIMARY SORT KEY                      
         MVC   PBNAME1,BUFNAME                                                  
         CLI   SORTOPT,1                  SJ IS NOT A PROGRESSIVE KEY           
         BNE   BFP130                                                           
         CLI   QOPT4,1                    WAS NO PRODUCT LEVEL REQSTD           
         BE    BFP130                                                           
         CLI   BUFLW1,C'L'                IS THIS LOWEST LEVEL                  
         BNE   BFP130                                                           
         MVC   PBUFAC1(6),BUFAC1+6                                              
         MVC   PBUFAC1+6(6),SPACES        CLEAR OUT CLIENT FIELD                
*                                                                               
BFP130   DS    0H                                                               
         BAS   RE,SQUASHIT                                                      
         LA    R4,XP                                                            
         EXMVC R2,PBUFAC1,WORK                                                  
         MVI   SPACING,2                                                        
         BAS   RE,BUFREPRT                                                      
         B     BFP500                     EXIT ROUTINE                          
*                                                                               
*                                                                               
*        HEADER BUFFRECS/SECONDARY SORT                                         
BFP150   DS    0H                                                               
         TM    BUFMRK1,X'12'                                                    
         BNO   BFP200                                                           
         CLI   BUFLW1,C'L'                                                      
         BE    BFP500                                                           
         MVC   PBUFAC2,BUFAC2             SECONDARY SORT KEY                    
         MVC   PBNAME2,BUFNAME                                                  
*                                                                               
         CLI   SORTOPT,2                  NEED TO TREAT SJ DIFFERENTLY          
         BNE   BFP175                                                           
         CLI   BUFMRK2,1                  CLIENT LEVEL                          
         BE    BFP175                                                           
         MVC   PBUFAC2(6),BUFAC2+6                                              
         MVC   PBUFAC2+6(6),SPACES        CLEAR OUT CLIENT FIELD                
*                                                                               
BFP175   DS    0H                                                               
         BAS   RE,SQUASHIT                                                      
         LA    R4,XP                                                            
         EXMVC R2,PBUFAC2,WORK                                                  
         BAS   RE,BUFREPRT                PRINT                                 
         B     BFP500                     EXIT ROUTINE                          
*                                                                               
*        TRAILER BUFFRECS/SECONDARY SORT                                        
BFP200   DS    0H                                                               
         TM    BUFMRK1,X'21'              IS THIS TRAILER REC                   
         BNO   BFP250                     IF NOT MUST BE PRIMARY                
*                                                                               
BFP215   CLI   SORTOPT,2                  TREAT SJ DIFFERENTLY                  
         BNE   BFP225                                                           
         CLI   QOPT5,1                    WAS PROD LEVEL REQUESTED              
         BE    BFP225                                                           
         CLI   BUFLW1,C'L'                IS THIS LOWEST LEVEL                  
         BNE   BFP225                                                           
         MVC   BUFAC2(6),SPACES           CLEAR OUT CLIENT                      
*                                                                               
BFP225   DS    0H                                                               
         CLI   BUFLW1,C'L'                IS THIS LOWEST LEVEL IN               
         BNE   BFP240                     SUMMARY                               
         MVC   PBUFAC3,BUFAC2                                                   
         MVC   PBNAME3,BUFNAME                                                  
         B     BFP245                                                           
*                                                                               
BFP240   MVC   PTOT2(L'TOTSUM),TOTSUM                                           
         MVC   PTOTAC2(L'BUFAC2),BUFAC2   ACCOUNT                               
         MVC   PTOTNM2,BUFNAME           ACCOUNT NAME                           
*                                                                               
BFP245   BAS   RE,SQUASHIT                                                      
         LA    R4,XP                                                            
         EXMVC R2,PBUFAC2,WORK                                                  
         B     BFP300                                                           
*                                                                               
*        TRAILER BUFFRECS/PRIMARY SORT                                          
BFP250   DS    0H                                                               
         TM    BUFMRK1,X'22'                                                    
         BNO   BFP500                                                           
         MVC   PTOT1(L'TOTSUM),TOTSUM                                           
         MVC   PTOTAC1,BUFAC1                   ACCOUNT                         
         MVC   PTOTNM1(L'BUFNAME),BUFNAME       ACCOUNT NAME                    
*                                                                               
         CLI   SORTOPT,1                  NEED TO TREAT SJ DIFFERENTLY          
         BNE   BFP275                                                           
         CLI   QOPT4,1                    WAS PROD LEVEL REQUESTED              
         BE    BFP275                                                           
         CLI   BUFLW1,C'L'                IS THIS LOWEST LEVEL                  
         BNE   BFP275                                                           
         MVC   PTOTAC1(6),SPACES                                                
*                                                                               
BFP275   DS    0H                                                               
         BAS   RE,SQUASHIT                                                      
         LA    R4,XP                                                            
         EXMVC R2,PBUFAC1,WORK                                                  
         B     BFP300                                                           
*                                                                               
*              PRINT ALL AMOUNTS FROM BUFFALO                                   
BFP300   LA    R6,XP+L'ACCTCOL         CURRENT PSTN IN PRNT LINE                
         L     R0,TEXTDISP                                                      
         AR    R6,R0                                                            
         LA    R2,BUFAMT1                                                       
         ZIC   R1,MAXCOLS              NUM OF DOLLAR COLS.                      
         LA    R3,1                    COUNT OF CURRENT COL                     
         XC    FULL,FULL               POSITION OF PCT COL                      
         MVC   FULL+3(1),PCTCOL                                                 
*                                                                               
BFP350   C     R3,FULL                 IF PROCESSING PCT COL                    
         BNE   BFP400                  WILL BE EQUAL                            
         CP    BUFAMT8,=P'0'               PERCENT(COMM/GROSS)                  
         BE    BFP450                                                           
         CP    BUFAMT9,=P'0'                                                    
         BE    BFP450                                                           
         ZAP   DUB(16),BUFAMT9(8)      COMPUTE PERCENT                          
         MP    DUB(16),=P'100000'                                               
         DP    DUB(16),BUFAMT8(8)                                               
         SRP   DUB(8),63,5                                                      
         ZAP   0(8,R2),DUB(8)                                                   
         BAS   RE,BUFEDIT                                                       
         B     BFP450                                                           
*                                                                               
BFP400   BAS   RE,BUFEDIT                                                       
*                                                                               
BFP450   LA    R2,L'BUFAMT1(,R2)       POSITION IN ACCUMS(ROLLER)               
         LA    R6,18(,R6)              POSITION IN PRINT LINE                   
         LA    R3,1(,R3)               CURRENT COL COUNTER                      
         BCT   R1,BFP350                                                        
*                                                                               
         CLC   SVBFAMTS,BUFAMT1        IF AMTS SAME MUST BE REDUNDANT           
         BE    BFP480                  TOTAL SO SKIP PRINTING                   
         MVI   SPACING,2                                                        
         BAS   RE,BUFREPRT                                                      
*                                                                               
BFP480   MVC   SVBFAMTS,BUFAMT1                                                 
*                                                                               
BFP500   MVC   XP,XSPACES                                                       
         B     XIT                                                              
*                                                                               
BUFEDIT  EDIT  (P8,(R2)),(17,(R6)),2,COMMAS=YES,MINUS=YES                       
         BR    RE                                                               
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        PUT REQUESTED DOLLAR AMOUNTS IN REQUESTED ORDER IN SORTREC  *          
*--------------------------------------------------------------------*          
SRTAMTS  NTR1                                                                   
         ZAP   SRTAMT8,TGROSS            NEED FOR PERCENT CALC                  
         ZAP   SRTAMT9,TCOMM             EVEN IF NOT SHOWN ON RPT               
*                                                                               
         LA    R2,8                      BEFORE CALCULATIONS                    
         LA    R3,TNET                   CLEAR REMAINING AMTS                   
SRTM10   ZAP   0(6,R3),=P'0'                                                    
         LA    R3,L'TGROSS(,R3)                                                 
         BCT   R2,SRTM10                                                        
         MVC   SJOBEST,SPACES                                                   
*                                                                               
         LA    R2,7                      CLEAR AMT FIELDS IN SORTREC            
         LA    R3,SRTAMT1-SORTREC(,R6)                                          
*                                                                               
SRTM12   ZAP   0(L'SRTAMT1,R3),=P'0'                                            
         LA    R3,L'SRTAMT1(,R3)                                                
         BCT   R2,SRTM12                                                        
*                                                                               
         CP    TCOMM,=P'0'               PERCENT(COMM/GROSS)                    
         BE    SRTMS20                                                          
         CP    TGROSS,=P'0'                                                     
         BE    SRTMS20                                                          
         ZAP   DUB(16),TCOMM                                                    
         MP    DUB(16),=P'100000'                                               
         DP    DUB(16),TGROSS                                                   
         SRP   DUB(10),63,5                                                     
         AP    TPCT,DUB+4(6)                                                    
*                                                                               
SRTMS20  DS    0H                                                               
*                                                                               
*&&UK                                                                           
         L     R4,ADTRANS                                                       
*                                                                               
         USING TRANSD,R4                                                        
*                                                                               
         ZAP   TNET,TGROSS                                                      
         SP    TNET,TRNSAMNT                                                    
*&&                                                                             
*                                                                               
         USING ACMTD,R4                                                         
*                                                                               
         L     R4,ADTRANS                                                       
         MVI   BYTE,X'1A'               FROM 1A ELEM GET:                       
         BAS   RE,NEXTEL                                                        
         BNE   SRTM80                                                           
*                                                                               
*&&US                                                                           
SRTMS30  ICM   R0,15,ACMTNET             NET FROM 1A ELEM                       
         CVD   R0,WORK                                                          
         ZAP   TNET,WORK+2(6)            CALC NET                               
*&&                                                                             
*                                                                               
         ICM   R0,15,ACMTRECV            RECEIVABLE FROM 1A ELEM                
         CVD   R0,WORK                   (BILL AMT)                             
         ZAP   TBILAMT,WORK+2(6)                                                
*                                                                               
         ICM   R0,15,ACMTCD              CASH DISCOUNT                          
         CVD   R0,WORK                                                          
         ZAP   TCD,WORK+2(6)                                                    
*                                                                               
         ICM   R0,15,ACMTINTL            INTERNAL                               
         CVD   R0,WORK                                                          
         ZAP   TINTRN,WORK+2(6)                                                 
*                                                                               
         ICM   R0,15,ACMTRECV            RECEIVABLE-CD                          
         CVD   R0,WORK                                                          
         ZAP   TARMCD,WORK+2(6)                                                 
         SP    TARMCD,TCD                                                       
*                                                                               
         ICM   R0,15,ACMTVAT             GST AMOUNT                             
         CVD   R0,WORK                                                          
         ZAP   TGSTAMT,WORK+2(6)                                                
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   SJOBEST,ACMTJOB           JOB NUM OR ESTIMATE NUM                
         CLC   ACMTJOB,SPACES                                                   
         BH    *+10                                                             
         MVC   SJOBEST+3(3),ACMTEST                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
SRTM80   ZAP   TGRMCD,TGROSS             GROSS-CD                               
         SP    TGRMCD,TCD                                                       
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),MAXCOLS       NUM $COLS SPEC IN PROFILE                
         ST    R6,SAVEH8                                                        
*                                                                               
         USING TABINDCD,R3                                                      
*                                                                               
         LA    R2,SRTAMT1              POSTN IN SORTREC BEING PROCSSD           
         LA    R6,1                                                             
*                                                                               
SRTM100  LA    R4,TABAMTS              TABLE OF AMOUNTS                         
         LA    R3,TABINDC              POSITION INDICATORS                      
         LA    R5,TABENTRY             NUMBER OF $ AMTS AND INDCS               
*                                                                               
SRTM120  CLI   TABSEL,X'00'            ZERO MEANS NOT BEING USED - SKIP         
         BE    SRTM125                 TO NEXT ENTRY IN AMTS AND INDCS          
         ZIC   R0,TABSEL               INDICATOR INTO REG FOR COMPARE           
         CR    R0,R6                                                            
         BE    SRTM130                                                          
*                                                                               
SRTM125  LA    R3,L'TABKEY(,R3)        NEXT ENTRY IN INDICATOR TABLE            
         LA    R4,6(,R4)               NEXT AMOUNT IN TABLE                     
         BCT   R5,SRTM120                                                       
         B     SRTM140                 MEANS THEY SKIPPED A SPOT WHEN           
*                                      SETTING UP PROFILE                       
SRTM130  ZAP   0(6,R2),0(6,R4)         INSERT INTO SORTREC                      
*                                                                               
SRTM140  LA    R2,6(,R2)               NEXT AMT FIELD IN SORTREC                
         LA    R6,1(,R6)                                                        
         CH    R6,HALF                                                          
         BH    SRTMXIT                                                          
         B     SRTM100                                                          
*                                                                               
SRTMXIT  DS    0H                                                               
         L     R6,SAVEH8                                                        
         B     XIT                                                              
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*              FORMAT MASSAGES & EDITS FIGURES ONTO PRINT LINE       *          
*              LEVEL IS PASSED TO THIS ROUTINE IN R2                 *          
*--------------------------------------------------------------------*          
FORMAT   NTR1                                                                   
         GOTO1 PROLLER,DMCB,1,ACCUMS,(R2)     RETREIVE LEVEL OF ACCUMS          
         MVC   FULL,DMCB                                                        
         MVC   BUFFADD,FULL            SAVE ADDR FOR USE WHEN ADDING            
         BAS   RE,FORMCTRS             RECS TO BUFFALO FOR SUMMARY              
         B     CIAXIT                                                           
*                                                                               
FORMCTRS NTR1                                                                   
         CLI   SRTTYP,C'T'                                                      
         BNE   FRM50                                                            
*                                                                               
FRM50    LA    R4,XP+L'ACCTCOL+3       CURRENT POSITION IN PRINT LINE.          
         L     R0,TEXTDISP                                                      
         AR    R4,R0                                                            
         L     R2,FULL                                                          
         ZAP   TOTGROS,42(6,R2)        SAVE TOTAL GROSS                         
         ZAP   TOTCOMM,48(6,R2)        SAVE TOTAL COMM                          
         ZIC   R1,MAXCOLS              NUM OF DOLLAR COLS.                      
         LA    R3,1                    COUNT OF CURRENT COL                     
         XC    FULL,FULL               POSITION OF PCT COL                      
         MVC   FULL+3(1),PCTCOL                                                 
*                                                                               
FRM100   C     R3,FULL                 IF PROCESSING PCT COL                    
         BNE   FRM180                  WILL BE EQUAL                            
         CLI   SRTTYP,C'T'             IF TRAN SRTREC PERCENT HAS               
         BE    FRM180                  ALREADY BEEN CALCULATED                  
         CP    TOTGROS,=P'0'           PERCENT=(COMM/GROSS)                     
         BE    FRM190                                                           
         CP    TOTCOMM,=P'0'                                                    
         BE    FRM190                                                           
         ZAP   DUB(16),TOTCOMM(6)                                               
         MP    DUB(16),=P'100000'                                               
         DP    DUB(16),TOTGROS(6)                                               
         SRP   DUB(10),63,5                                                     
         ZAP   0(6,R2),DUB+4(6)                                                 
         BAS   RE,EDIT1                                                         
         B     FRM190                                                           
*                                                                               
FRM180   DS    0H                                                               
         BAS   RE,EDIT1                                                         
*                                                                               
FRM190   LA    R2,6(,R2)               POSITION IN ACCUMS(ROLLER)               
         LA    R4,18(,R4)              POSITION IN PRINT LINE                   
         LA    R3,1(,R3)               CURRENT COL COUNTER                      
         BCT   R1,FRM100                                                        
*                                                                               
FRM200   B     XIT                                                              
*                                                                               
EDIT1    EDIT  (P6,(R2)),(17,(R4)),2,COMMAS=YES,MINUS=YES                       
         BR    RE                                                               
*                                                                               
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*              SORTER INTERFACE                                      *          
*--------------------------------------------------------------------*          
SETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECDCARD,RR=RB                          
         B     XIT                                                              
*                                                                               
PUTSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC,RR=RB                            
*--------------------------------------------------------------------*          
         CLI   QOPT6,C'S'                                            *          
         BNE   XIT                                                   *          
         MVC   XP(L'SORTREC),SORTREC                                 *          
         GOTO1 ACREPORT                                              *          
*--------------------------------------------------------------------*          
         B     XIT                                                              
*                                                                               
GETSORT  NTR1                                                                   
         MVC   SORTREC,SPACES                                                   
         GOTO1 =V(SORTER),DMCB,=C'GET',0,RR=RB                                  
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         MVC   SORTREC,0(R1)                                                    
*--------------------------------------------------------------------*          
         CLI   QOPT7,C'S'                                            *          
         BNE   XIT                                                   *          
         MVC   XP(L'SORTREC),SORTREC                                 *          
         GOTO1 ACREPORT                                              *          
*--------------------------------------------------------------------*          
         B     XIT                                                              
*                                                                               
ENDSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'END',RR=RB                                    
         B     XIT                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(01,30,A),FORMAT=BI,WORK=1'                     
RECDCARD DC    CL80'RECORD TYPE=F,LENGTH=96'                                    
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*              PRINT ROUTINE FOR BODY OF REPORT                      *          
*--------------------------------------------------------------------*          
CIAREPRT NTR1                                                                   
         CLI   QOPT3,C'S'                AM I SUPPRESSING BODY OF RPT           
         BE    XIT                                                              
*                                                                               
CREP05   ZIC   R1,LINE                                                          
         LA    R1,1(,R1)                                                        
         ZIC   R0,MAXLINES                                                      
         CR    R1,R0                                                            
         BL    CREP10                                                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
CREP10   DS    0H                                                               
         MVC   XHEAD4(L'MYHEAD4),MYHEAD4                                        
         MVC   XHEAD5(L'MYHEAD5),MYHEAD5                                        
         MVC   XHEAD6(L'MYHEAD6),MYHEAD6                                        
         MVC   XHEAD7(L'MYHEAD7),MYHEAD7                                        
         MVC   XHEAD9(L'MYHEAD9),MYHEAD9                                        
*        MVC   XHEAD10(L'MYHEAD10),MYHEAD10                                     
         CLC   XP+1(13),=C'REQUEST TOTAL'                                       
         BNE   CREP60                                                           
         MVC   XHEAD4(L'MYHEAD4),XSPACES  CLEAR ACCOUNT FROM HEADLINES          
         MVC   XHEAD5(L'MYHEAD5),XSPACES                                        
         MVC   XHEAD6(L'MYHEAD6),XSPACES                                        
         MVC   XHEAD7(L'MYHEAD7),XSPACES                                        
         MVC   XHEAD9(L'BFHEAD9),BFHEAD9                                        
*        MVC   XHEAD10(L'BFHEAD10),BFHEAD10  FROM COLUMN HEADINGS               
*                                                                               
CREP60   GOTO1 ACREPORT                                                         
         B     XIT                                                              
*--------------------------------------------------------------------*          
*              PRINT ROUTINE FOR PRINTING BUFFRECS                   *          
*--------------------------------------------------------------------*          
BUFREPRT NTR1                                                                   
         ZIC   R1,LINE                                                          
         LA    R1,1(,R1)                                                        
         ZIC   R0,MAXLINES                                                      
         CR    R1,R0                                                            
         BL    BUFP10                                                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
BUFP10   DS    0H                                                               
         CLI   TOPBOX,C'Y'           IF ANY HEADLINES PRINT FIRST               
         BNE   BUFP20                                                           
         MVC   XHEAD5(L'TOPBOXT),TOPBOXT        BOX TOP                         
         MVC   XHEAD6(L'TOPBOXM),TOPBOXM        BOX MID                         
         MVC   XHEAD7(L'TOPBOXB),TOPBOXB        BOX BOTTOM                      
*                                                                               
BUFP20   MVC   XHEAD9(REPWID),BFHEAD9                                           
*        MVC   XHEAD10(REPWID),BFHEAD10                                         
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        CREATING REPORT COLUMN HEADINGS FROM PROFILE                *          
*        A POSSIBLE THREE TEXT COLUMN HEADINGS WILL BE FIRST         *          
*        FOLLOWED BY DOLLAR COLUMNS                                  *          
*--------------------------------------------------------------------*          
COLHEADS NTR1                                                                   
*        INITIALLY CLEAR ALL PROFILE SELECTION INDICATORS TO ZERO               
         LA    R2,TABENTRY                                                      
         LA    R3,TABINDC                                                       
*                                                                               
         USING TABINDCD,R3                                                      
*                                                                               
COLH100  MVI   TABSEL,X'00'                                                     
         LA    R3,L'TABKEY(,R3)                                                 
         BCT   R2,COLH100                                                       
*                                                                               
***    TEST PURPOSES ONLY                                                       
***      MVC   PROGPROF(9),=X'010203000000000000'                               
***      MVC   PROGPROF+9(4),=C'ABNN'                                           
***      MVI   PROGPROF+14,4                                                    
***      MVI   PCTCOL,0                                                         
***    TEST PURPOSES ONLY                                                       
         MVC   SVTXPROF,PROGPROF+9  NEED PROFILE AT REQUEST LAST                
         MVC   SVNMPROF,PROGPROF+0                                              
         MVC   SVGSTPF,PROGPROF+14                                              
*                                                                               
         MVC   MYHEAD4,XSPACES                                                  
         MVC   MYHEAD5,XSPACES                                                  
         MVC   MYHEAD6,XSPACES                                                  
         MVC   MYHEAD7,XSPACES                                                  
         MVC   MYHEAD9,XSPACES                                                  
         MVC   MYHEAD10,XSPACES                                                 
         MVC   BFHEAD9,XSPACES             BUFFALO HEADLINES                    
         MVC   BFHEAD10,XSPACES                                                 
         MVC   SVHEAD4,XSPACES                                                  
         MVC   SVHEAD4(6),=C'CLIENT'                                            
*                                                                               
         XC    HALF,HALF                                                        
         MVI   TEXTCOLS,0           NUM OF COLS OF TEXT REQUESTED               
*                                                                               
         USING TEXTTABD,R5                                                      
*                                                                               
         LA    R5,TEXTTAB                                                       
         LA    R2,SVTXPROF                                                      
         LA    R3,TEXENTRY          MAX OF 3 TEXT COLS                          
*                                                                               
COLH130  CLI   0(R2),C'N'           NOT SELECTED                                
         BE    COLH150                                                          
         NI    0(R2),X'0F'          CONVERT TO HEX NUM                          
         MVC   BYTE,0(R2)                                                       
         ZIC   R4,BYTE              CALC DISPLACEMENT INTO                      
         BCTR  R4,0                 TEXTTAB TO POINT TO SELECTED                
         MVI   HALF+1,L'TEXT1+1            COLUMN HEADER                        
         MH    R4,HALF                     CALULATE DISPLACEMENT                
         LA    R1,TEXT1             STARTING POSITION IN PRINT LINE             
         AR    R1,R4                       ADD DISP TO TABLE POINTER            
         MVC   0(L'TXTCOL,R1),TXTCOL         MOVE IN COLUMN HEADING             
         MVC   REPWID(L'TXTUNDL,R1),TXTUNDL  MOVE IN UNDERSCORING               
*                                                                               
         ZIC   R1,TEXTCOLS          COUNT NUM OF TEXT COLS REQUESTED            
         LA    R1,1(,R1)                                                        
         STC   R1,TEXTCOLS                                                      
*                                                                               
COLH150  LA    R2,1(,R2)                                                        
         LA    R5,L'TXTNTRY(,R5)                                                
         BCT   R3,COLH130                                                       
*                                                                               
         LA    R1,TEXT1             TEXT INTO PRINT LINE                        
         CLI   TEXTCOLS,0                                                       
         BE    COLH190                                                          
         ZIC   R2,TEXTCOLS          CALCULATE DISPLACEMENT OF END OF            
*                                                                               
COLH180  LA    R1,L'TEXT1+1(,R1)                                                
         BCT   R2,COLH180                                                       
*                                                                               
COLH190  ST    R1,SAVEH8            USE AT REQLAST                              
         LA    R2,REPWID(,R1)                                                   
         LA    R2,REPWID(,R2)       SAME DISPL INTO SUMMARY HEADLINES           
         ST    R2,SAVEH9            STORE HEADLINE DISPL FOR CONTINUE           
         LA    R4,TEXT1                                                         
         SR    R1,R4                                                            
         ST    R1,TEXTDISP          DISPL OF TEXT BEING PRNTD FOR               
*                                   OF HEADLINE BUILDING                        
         MVI   MAXCOLS,0                 STORE NUM OF $ COLS ON REPORT          
         LA    R1,SVNMPROF               FOR SORTREC AND EDITING LOOPS          
         LA    R3,TABENTRY                                                      
*                                                                               
COLH200  NI    0(R1),X'0F'                                                      
         CLC   MAXCOLS,0(R1)                                                    
         BH    *+10                                                             
         MVC   MAXCOLS,0(R1)                                                    
         LA    R1,1(,R1)                                                        
         BCT   R3,COLH200                                                       
*                                                                               
         CLI   TEXTCOLS,X'00'            CORRECT NUM OF $ COLS IF THEY          
         BNE   COLH220                   HAVE OVERRAN SPACE LIMIT IN            
         CLI   MAXCOLS,X'07'             PROFILE - IF SO END $ COLS             
         BNH   *+8                       WILL BE TRUNCATED.                     
         MVI   MAXCOLS,X'07'             IF NO TEXT - MAX OF 7 $ COLS           
         B     COLH280                                                          
*                                                                               
COLH220  CLI   TEXTCOLS,X'01'                                                   
         BNE   COLH230                                                          
         CLI   MAXCOLS,X'06'                                                    
         BNH   *+8                                                              
         MVI   MAXCOLS,X'06'             MAX OF 6 $ COLS                        
         B     COLH280                                                          
*                                                                               
COLH230  CLI   TEXTCOLS,X'02'                                                   
         BNE   COLH250                                                          
         CLI   MAXCOLS,X'06'                                                    
         BNH   *+8                                                              
         MVI   MAXCOLS,X'06'             MAX OF 6 $ COLS                        
         B     COLH280                                                          
*                                                                               
COLH250  CLI   MAXCOLS,X'05'             MUST BE 3 TEXT COLS                    
         BNH   *+8                                                              
         MVI   MAXCOLS,X'05'             CAN BE MAX OF 5 $ COLS                 
*                                                                               
COLH280  DS    0H                                                               
         LA    R4,TABENTRY               SET ON TABLE OF COLUMN                 
         LA    R1,SVNMPROF               INDICATORS                             
         LA    R3,TABINDC                                                       
*                                                                               
COLH290  MVC   TABSEL,0(R1)                                                     
         LA    R3,L'TABKEY(,R3)                                                 
         LA    R1,1(,R1)                                                        
         BCT   R4,COLH290                                                       
*                                                                               
         XC    HALF,HALF                 CONTINUE SET UP COLMN HEADINGS         
         MVC   HALF+1(1),MAXCOLS         NOW DO DOLLAR COLS                     
         L     R1,SAVEH8                 R1, R2 FROM PREV LOOP - DISPL          
         L     R2,SAVEH9                 INTO HEAD8, HEAD9                      
         LA    R4,1                      CNT IN R3, CAN'T EXCEED MAXCLS         
*                                                                               
COLH300  LA    R3,TABINDC                                                       
*                                                                               
COLH320  ZIC   R0,TABSEL                                                        
         CR    R0,R4                                                            
         BE    COLH350                                                          
         CLI   0(R3),X'FF'                                                      
         BE    COLH370                                                          
         LA    R3,L'TABKEY(,R3)                                                 
         B     COLH320                                                          
*                                                                               
COLH350  MVC   0(L'TABCOLH,R1),TABCOLH   COLUMN HEADING TO HEAD LINE            
         MVC   REPWID(L'TABUNDL,R1),TABUNDL                                     
         MVC   0(L'TABCOLH,R2),TABCOLH   COLUMN UNDERLINING TO HD LINE          
         MVC   REPWID(L'TABUNDL,R2),TABUNDL                                     
         LA    R1,18(,R1)                INCREMENT HEADLINES                    
         LA    R2,18(,R2)                                                       
*                                                                               
COLH370  LA    R4,1(,R4)                                                        
         CH    R4,HALF                                                          
         BNH   COLH300                                                          
         B     XIT                                                              
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*              GETNAME FINDS CLIENT IN SJ LEDGER                     *          
*                   OR FINDS ACCOUNT IN 1C                           *          
*--------------------------------------------------------------------*          
GETNAME  NTR1                                                                   
         L     R4,=A(ACCBUFF)                                                   
         MVC   0(42,R4),SPACES                                                  
*                                                                               
         MVC   0(1,R4),QCOMPANY                                                 
         CLI   SORTOPT,3                  USING 1C IN SORT                      
         BE    GETN150                                                          
         CLI   SORTOPT,4                  USING 1C IN SORT                      
         BE    GETN150                                                          
*                                                                               
         MVC   1(2,R4),=C'SJ'                                                   
         MVC   3(3,R4),SCLIKEY2                                                 
         CLI   SORTOPT,2                  IF SI/SJ SORT CLIENT IS IN            
         BE    GETN200                    SECONDARY SORT KEY                    
         MVC   3(3,R4),SCLIKEY                                                  
         B     GETN200                                                          
*                                                                               
GETN150  MVC   1(2,R4),=C'1C'                                                   
         LA    R1,SRTKEY1                 1C IS PRIMARY SORT                    
         CLI   SORTOPT,4                  OR                                    
         BNE   *+8                                                              
         LA    R1,SRTKEY2                 1C IS SECONDARY SORT                  
         BCTR  R2,0                                                             
         EXMVC R2,3(R4),0(R1)                                                   
*                                                                               
GETN200  GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
*                                                                               
         MVI   BYTE,X'20'                 GET NAME ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SRTDATA,SPACES                                                   
*                                                                               
         USING ACNAMED,R4                                                       
*                                                                               
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'03'                                                        
         LTR   R1,R1                                                            
         BM    XIT                                                              
         EXMVC R1,SRTDATA,ACNMNAME                                              
         B     XIT                                                              
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*              SQUASH DATA PASSED IN WORK                                       
*--------------------------------------------------------------------*          
SQUASHIT NTR1                                                                   
         OC    WORK,SPACES                                                      
         LA    R3,WORK                  WILL GET RID OF ALL PADDED              
         LA    R2,L'WORK                FOXES BEFORE SQUASHING                  
*                                                                               
SQ100    CLI   0(R3),X'FF'                                                      
         BNE   *+8                                                              
         MVI   0(R3),C' '                                                       
         LA    R3,1(,R3)                                                        
         BCT   R2,SQ100                                                         
*                                                                               
         LA    R2,64                             MAX POSSIBLE LENGTH            
         GOTO1 =V(SQUASHER),DMCB,WORK,(R2),RR=RB     OF DATA                    
         L     R2,DMCB+4                                                        
         BCTR  R2,0                                                             
         XIT1  REGS=(R2)                                                        
*                                                                               
*--------------------------------------------------------------------*          
*        CHOP SQUASHED DATA PASSED IN WORK                                      
*        PASSED BACK IN WORK2, BLOCKS OF LENGTH 32                              
*--------------------------------------------------------------------*          
CHOPIT1  NTR1                                                                   
         MVC   WORK1,SPACES                                                     
         GOTO1 =V(CHOPPER),DMCB,(L'WORK,WORK),(L'WORK1A,WORK1),        X        
               (L'WORK1A,2)                                                     
         B     XIT                                                              
*                                                                               
*--------------------------------------------------------------------*          
*        CHOP SQUASHED DATA PASSED IN WORK                                      
*        PASSED BACK IN WORK2, BLOCKS OF LENGTH 32                              
*--------------------------------------------------------------------*          
CHOPIT2  NTR1                                                                   
         MVC   WORK2,SPACES                                                     
         GOTO1 =V(CHOPPER),DMCB,(L'WORK,WORK),(L'WORK2A,WORK2),        X        
               (L'WORK2A,2)                                                     
         B     XIT                                                              
         EJECT ,                                                                
*                                                                               
TABINDC  DS    0H                                                               
TINGRS   DC    XL2'0100',CL16'      GROSS     ',CL16'      _____     '          
TINCOMM  DC    XL2'0200',CL16'     INCOME     ',CL16'     ______     '          
TINNET   DC    XL2'0300',CL16'       NET      ',CL16'       ___      '          
TINRECVB DC    XL2'0400',CL16'   BILL AMOUNT  ',CL16'   ___________  '          
TINCD    DC    XL2'0500',CL16'  CASH DISCOUNT ',CL16'  _____________ '          
TINARMCD DC    XL2'0600',CL16' BILL AMOUNT-CD ',CL16'  _____________ '          
TINGRMCD DC    XL2'0700',CL16'    GROSS-CD    ',CL16'    ________    '          
TINPCT   DC    XL2'0800',CL16'     PERCENT    ',CL16'     _______    '          
TININTRN DC    XL2'0900',CL16'    INTERNAL    ',CL16'    ________    '          
TINGST   DC    XL2'0F00',CL16'       GST      ',CL16'       ___      '          
TABINEND EQU   *                                                                
TABENTRY EQU   (TABINEND-TABINDC)/L'TABKEY                                      
*                                                                               
TEXTTAB  DS    0H                                                               
         DC    CL9' INVOICE ',CL9' ------- '                                    
         DC    CL9'   DATE  ',CL9'   ----  '                                    
         DC    CL9' JOB/EST ',CL9' ------- '                                    
TEXTEND  EQU   *                                                                
TEXENTRY EQU   (TEXTEND-TEXTTAB)/L'TXTNTRY                                      
*                                                                               
TOTAL    DC    CL13'** TOTAL FOR '                                              
TOTSUM   DC    CL12'**TOTAL FOR '                                               
*                                                                               
*                                                                               
BXLIN    DS    0XL53                                                            
         DC    53XL1'BF'                                                        
MTOPDIV  DC    XL1'CC'                                                          
MMIDDIV  DC    XL1'FA'                                                          
MBOTDIV  DC    XL1'CB'                                                          
MTOPRIG  DC    XL1'BC'                                                          
MBOTRIG  DC    XL1'BB'                                                          
*                                                                               
TOPBOXT  DS    0CL163                                                           
         DC    XL1'AC'                                                          
BTLIN1   DS    XL53                                                             
BTDIV1   DS    XL1                                                              
BTLIN2   DS    XL53                                                             
BTDIV2   DS    XL1                                                              
BTLIN3   DS    XL53                                                             
BTDIV3   DS    XL1                                                              
*                                                                               
TOPBOXM  DS    0CL163                                                           
         DC    XL1'FA'                                                          
BOXAC1   DS    CL53                                                             
BMDIV1   DS    XL1                                                              
BOXAC2   DS    CL53                                                             
BMDIV2   DS    XL1                                                              
BOXAC3   DS    CL53                                                             
BMDIV3   DS    XL1                                                              
*                                                                               
TOPBOXB  DS    0CL163                                                           
         DC    XL1'AB'                                                          
BBLIN1   DS    XL53                                                             
BBDIV1   DS    XL1                                                              
BBLIN2   DS    XL53                                                             
BBDIV2   DS    XL1                                                              
BBLIN3   DS    XL53                                                             
BBDIV3   DS    XL1                                                              
*                                                                               
ROLTRAN  DC    AL1(1)                                                           
ROLINVN  DC    AL1(2)                                                           
ROLSECD  DC    AL1(3)                                                           
ROLSECC  DC    AL1(4)                                                           
ROLSECB  DC    AL1(5)                                                           
ROLSECA  DC    AL1(6)                                                           
ROLPRID  DC    AL1(7)                                                           
ROLPRIC  DC    AL1(8)                                                           
ROLPRIB  DC    AL1(9)                                                           
ROLPRIA  DC    AL1(10)                                                          
ROLREPT  DC    AL1(11)                                                          
*                                                                               
TOTUNDL  DS    0CL42                                                            
         DC    2CL1' '                                                          
         DC    40CL1'-'                                                         
*                                                                               
COSTUL   DC    CL2'1C'                                                          
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        SET UP BOXES                                                *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
         USING BOXD,R4                                                          
         USING BIGPRNTD,R5                                                      
         SPACE 1                                                                
BXHOOK   NMOD1 0,*BXHOOK*                                                       
         L     RC,BOXRC                                                         
         L     R4,ADBOX                                                         
         L     R5,VBIGPRNT                                                      
         MVC   BOXROWS,XSPACES                                                  
         MVC   BOXCOLS,XSPACES                                                  
*                                                                               
BXH50    MVC   BOXCOLS(198),XSPACES                                             
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+7,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'                                                     
         CLI   PRNTBF,C'Y'                                                      
         BE    *+8                                                              
         MVI   BOXCOLS+L'ACCTCOL+2,C'C'                                         
*                                                                               
         LA    R1,BOXCOLS+L'ACCTCOL+2   TEXT COLUMN BORDERS                     
         LA    R2,SVTXPROF                                                      
         LA    R3,3                     THREE POSSIBLE CHOICES                  
*                                                                               
BXH100   CLI   0(R2),C'N'                                                       
         BE    BXH150                                                           
         CLI   PRNTBF,C'Y'                                                      
         BE    *+8                                                              
         MVI   10(R1),C'C'                                                      
         LA    R1,10(,R1)                                                       
*                                                                               
BXH150   LA    R2,1(,R2)                                                        
         BCT   R3,BXH100                                                        
         MVI   0(R1),C'C'                                                       
*                                                                               
         ZIC   R3,MAXCOLS              NUMBER OF DOLLAR COLS                    
         LTR   R3,R3                   IF NONE SKIP                             
         BZ    BXH500                                                           
*                                                                               
BXH200   MVI   18(R1),C'C'                                                      
         LA    R1,18(,R1)                                                       
         BCT   R3,BXH200                                                        
         MVI   0(R1),C'R'                                                       
*                                                                               
BXH500   DS    0H                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*                                                                               
ACCBUFF  CSECT                                                                  
         DS    CL1024              1K FOR DATAMGR READ                          
         SPACE 3                                                                
         BUFF  LINES=1000,ROWS=1,COLUMNS=10,COMMENT=36,FLAVOR=PACKED,  X        
               KEYLIST=(28,A)                                                   
         EJECT ,                                                                
*                                                                               
TABINDCD DSECT                                                                  
TABKEY   DS    0CL34                 DSECT TO COVER TABINDC TABLE               
TABIND   DS    CL1                   PROFILE NUM                                
TABSEL   DS    CL1                   COLUMN SELECTED                            
TABCOLH  DS    CL16                  COLUMN HEADER                              
TABUNDL  DS    CL16                  COLUMN UNDERSCORING                        
         EJECT ,                                                                
*                                                                               
TEXTTABD DSECT                                                                  
TXTNTRY  DS    0CL18                 LENGTH OF ONE ENTRY                        
TXTCOL   DS    CL9                   LENGTH OF ONE ENTRY                        
TXTUNDL  DS    CL9                   LENGTH OF ONE ENTRY                        
         EJECT ,                                                                
*                                                                               
SORTRECD DSECT                                                                  
SORTREC  DS    0CL96                                                            
SORTKEY  DS    0CL30                                                            
SRTKEY1  DS    0CL12                 PRIMARY SORT KEY                           
SCLIKEY  DS    0CL6                                                             
SCLI     DS    CL3                                                              
         DS    CL3                                                              
SPRDKEY  DS    CL6                                                              
*                                                                               
SRTKEY2  DS    0CL12                 SECONDARY SORT KEY                         
SCLIKEY2 DS    0CL6                                                             
SCLI2    DS    CL3                                                              
         DS    CL3                                                              
SPRDKEY2 DS    CL6                                                              
*                                                                               
SINVNUM  DS    CL6                   INVOICE NUMBER                             
SRTTYP   DS    CL1                   RECORD IDENTIFIER                          
SRTDATA  DS    0CL36                                                            
SRTAMT1  DS    PL6                   SELECTED DOLLAR AMOUNTS                    
SRTAMT2  DS    PL6                                                              
SRTAMT3  DS    PL6                                                              
SRTAMT4  DS    PL6                                                              
SRTAMT5  DS    PL6                                                              
SRTAMT6  DS    PL6                                                              
SRTAMT7  DS    PL6                                                              
SRTAMT8  DS    PL6                   NEED EXTRA AMTS (8 AND 9) TO               
SRTAMT9  DS    PL6                    CALC PCTS                                 
SINVDAT  DS    CL3                                                              
SINVMOS  DS    XL2                   NOT  USED                                  
SJOBEST  DS    CL6                                                              
         EJECT ,                                                                
*                                                                               
PLINED   DSECT                  PRINT LINE DSECT                                
PLINE    DS    0CL165                                                           
         DS    CL1                                                              
PNAME1   DS    CL59                                                             
         ORG   PNAME1+3                                                         
PNAME2   DS    CL56                                                             
         DS    CL106                                                            
         EJECT ,                                                                
*                                                                               
PRBUFFD  DSECT                  PRINT LINE DSECT FOR SUMMARIES                  
PBLINE   DS    0CL165                                                           
         DS    CL1                                                              
PBUFAC1  DS    CL12             PRIMARY SORT ACCOUNT                            
         DS    CL1                                                              
PBNAME1  DS    CL36                                                             
         ORG   PBUFAC1          TOTAL LINE FOR PRIMARY SORT                     
PTOT1    DS    CL12                                                             
         DS    CL1                                                              
PTOTAC1  DS    CL12                                                             
         DS    CL1                                                              
PTOTNM1  DS    CL36                                                             
         ORG   PBUFAC1+2        SECONDARY SORT ACCOUNT                          
PBUFAC2  DS    CL12                                                             
         DS    CL1                                                              
PBNAME2  DS    CL36                                                             
         ORG   PBUFAC2          TOTAL LINE FOR SECONDARY SORT                   
PTOT2    DS    CL12                                                             
         DS    CL1                                                              
PTOTAC2  DS    CL12                                                             
         DS    CL1                                                              
PTOTNM2  DS    CL36                                                             
         ORG   PBUFAC2+2        LOWEST LEVEL ACCOUNT LINE                       
PBUFAC3  DS    CL12                                                             
         DS    CL1                                                              
PBNAME3  DS    CL36                                                             
         DS    CL112                                                            
         EJECT                                                                  
*                                                                               
*              AC3D02D,DSECT FOR LOCAL WORKING STORAGE                          
AC3D02D  DSECT                                                                  
         DS    0D                                                               
ABUFF    DS    A                                                                
ADBOX    DS    A                                                                
SAVEKEY  DS    CL42                                                             
READTRNS DS    CL1                                                              
REPWID   EQU   165                  REPORT WIDTH                                
*                                                                               
LEV1OK   DS    CL1                  INDICATORS OF WHETHER OR NOT THIS           
LEV2OK   DS    CL1                  SORTREC WILL BE INCLUDED IN SORT            
LEV3OK   DS    CL1                  X'80' SET ON WHEN FIRST SET UP IN           
LEV4OK   DS    CL1                  SBACFRST                                    
LEVAOK   DS    CL1                  X'01' SET ON WHEN TRANSACTIONS              
LEVBOK   DS    CL1                  FOUND FOR THIS ACCOUNT                      
LEVCOK   DS    CL1                  WILL NOT BE SENT TO SORT UNLESS             
LEVDOK   DS    CL1                  BOTH BITS ON                                
*                                                                               
ALEVLEN  DS    CL1                  INDIVIDUAL LEVEL                            
BLEVLEN  DS    CL1                  LENGTHS OF SI                               
CLEVLEN  DS    CL1                                                              
DLEVLEN  DS    CL1                                                              
BLEVTOT  DS    CL1                  CUMULATIVE LENGTHS OF SI                    
CLEVTOT  DS    CL1                                                              
DLEVTOT  DS    CL1                                                              
*                                                                               
CLILEN   DS    CL1                  SJ CLIENT LENGTH                            
PRODLEN  DS    CL1                  SJ PRODUCT LENGTH                           
         ORG   CLILEN                                                           
CSTTOTA  DS    CL1                  CUMULATIVE LEVEL LENGTHS OF 1C              
CSTTOTB  DS    CL1                                                              
CSTTOTC  DS    CL1                                                              
CSTTOTD  DS    CL1                                                              
CSTLEVB  DS    CL1                                                              
CSTLEVC  DS    CL1                                                              
CSTLEVD  DS    CL1                                                              
*                                                                               
LOWSI    DS    CL1                  LOWEST LEVEL OF SI                          
LOW1C    DS    CL1                  LOWEST LEVEL OF 1C                          
LOWSJ    EQU   2                    ALWAYS CONSTANT                             
SVCLILEN DS    CL1                  SAVE LENGTH OF CLI FIELD                    
COSTCLIL DS    CL1                  DISPLACEMENT OF CLI IN 1C LEDGER            
*                                                                               
MAXCOLS  DS    CL1                  NUM OF $ COLS ON THEIR RPT                  
TEXTCOLS DS    XL1                  NUM OF TEXT COLS ON THEIR RPT               
PCTCOL   DS    XL1                  $ COL PERCENT IS IN (NOT A TOTAL)           
PRNTBF   DS    CL1                  HAVE THEY REQUESTED A SUMMARY?              
TOPBOX   DS    CL1                  PRINTING A HEADER(TOP) BOX?                 
SORTOPT  DS    XL1                  REQUESTED SORT OPTION                       
SAVINVN  DS    CL6                  INVOICE NUM FOR INVOICE TOTALS              
NUMOFINV DS    CL1                                                              
*                                                                               
SVPROF   DS    0CL15                                                            
SVNUMPF  DS    0CL10                                                            
SVNMPROF DS    CL9                  $ PORTION OF PROGPROF                       
SVGSTPF  DS    CL1                                                              
SVTXPROF DS    CL5                  TEXT PORTION OF PROGPROF                    
*                                                                               
SAVACNM  DS    0CL48                                                            
SAVACCT  DS    CL12                                                             
         DS    CL36                                                             
*                                                                               
TOTGROS  DS    PL6                   GROSS FROM TRANSACTION                     
TOTCOMM  DS    PL6                   COMMISSION FROM TRANSACTION                
SAVECLT  DS    CL3                                                              
TEXTDISP DS    F                     DISPLACEMENT OF TEXT INTO P LINE           
SAVEH8   DS    F                                                                
SAVEH9   DS    F                                                                
BUFFADD  DS    F                     ADD IN ROLLER OF TTLS FOR BUFFALO          
CSTACCT  DS    CL12                  COSTING CODE OFF SJ                        
*                                                                               
TABAMTS  DS    0H                    WHEN PROCESSING TRNSACTION                 
TGROSS   DS    PL6                   FILL THIS TABLE THEN PUT AMTS              
TCOMM    DS    PL6                   IN APPROPRIATE ORDER IN SORTREC            
TNET     DS    PL6                                                              
TBILAMT  DS    PL6                                                              
TCD      DS    PL6                                                              
TARMCD   DS    PL6                                                              
TGRMCD   DS    PL6                                                              
TPCT     DS    PL6                                                              
TINTRN   DS    PL6                                                              
TGSTAMT  DS    PL6                                                              
TAMTLNG  EQU   *-TABAMTS                                                        
*                                                                               
*              POSSIBILITY OF EIGHT TOTAL LEVEL BREAKS SO NEED                  
*              NINE SORTREC SPACES                                              
*        PRIMARY SORTRECS                                                       
SORTREC1 DS    CL(L'SORTREC)                                                    
SORTREC2 DS    CL(L'SORTREC)                                                    
SORTREC3 DS    CL(L'SORTREC)                                                    
SORTREC4 DS    CL(L'SORTREC)                                                    
*        SECONDARY SORTRECS                                                     
SORTLEVA DS    CL(L'SORTREC)                                                    
SORTLEVB DS    CL(L'SORTREC)                                                    
SORTLEVC DS    CL(L'SORTREC)                                                    
SORTLEVD DS    CL(L'SORTREC)                                                    
SORTTRAN DS    CL(L'SORTREC)             TRANSACTION SORTREC                    
LASTSORT DS    CL(L'SORTREC)             SAVE LAST SORTREC FROM                 
*                                        SORTER                                 
SAVEREQT DS    CL165                     SAVE REQ TOTAL LINE FOR                
*                                        SUMMARY TOTAL                          
MYHEAD9  DS    0CL165                                                           
         DS    CL2                                                              
ACCTCOL  DS    CL40                                                             
         DS    CL1                                                              
TEXT1    DS    CL9                                                              
         DS    CL1                                                              
TEXT2    DS    CL9                                                              
         DS    CL1                                                              
TEXT3    DS    CL9                                                              
SPARE    DS    CL94                                                             
*                                                                               
MYHEAD10 DS    CL165                                                            
BFHEAD9  DS    CL165                COL HEADINGS FOR SUMMARIES                  
BFHEAD10 DS    CL165                COL UNDERSCORING FOR SUMMARIES              
*                                                                               
*                                   SECONDARY TOTALS                            
MYHEAD4  DS    CL65                                                             
MYHEAD5  DS    CL65                                                             
MYHEAD6  DS    CL65                                                             
MYHEAD7  DS    CL65                                                             
SVHEAD4  DS    CL15                                                             
*                                                                               
***R RECCNTRS DS    0H              KEEP COUNTS OF RECORDS PROCESSED            
***R REC1CONT DS    PL4             TO AVOID PRINTING REDUNDANT                 
***R REC2CONT DS    PL4             TOTALS                                      
***R REC3CONT DS    PL4                                                         
***R REC4CONT DS    PL4                                                         
***R LEVACONT DS    PL4                                                         
***R LEVBCONT DS    PL4                                                         
***R LEVCCONT DS    PL4                                                         
***R LEVDCONT DS    PL4                                                         
TRNSCONT DS    PL4                                                              
*                                                                               
ACSTNAM  DS    CL15                LEDGER 1C LEVEL NAMES                        
BCSTNAM  DS    CL15                                                             
CCSTNAM  DS    CL15                                                             
DCSTNAM  DS    CL15                                                             
*                                                                               
SAVANAM  DS    CL36                SAVING 1C NAMES WILL AVOID DOING             
SAVBNAM  DS    CL36                DUPLICATE READS                              
SAVCNAM  DS    CL36                                                             
SAVDNAM  DS    CL36                                                             
*                                                                               
ACCUMS   DS    CL8,(11*10)PL6      ACCUMULATOR SPACE FOR ROLLER                 
*                                                                               
SVBFAMTS DS    0CL72           TEMP SAVE FOR AMTS TO ELIMINATE                  
         DS    9PL8            TEMP SAVE FOR AMTS TO ELIMINATE                  
*                              REDUNDANT TOTALS                                 
BUFFREC  DS    0CL136                                                           
BUFFKEY  DS    0CL28                                                            
BUFTYP   DS    XL1             X'01' - SUMMARY RECORDS                          
BUFAC1   DS    XL12            LEVEL 1 SORTKEY                                  
BUFAC2   DS    XL12            LEVEL 2 SORTKEY                                  
BUFMRK1  DS    XL1                                                              
BUFMRK2  DS    XL1                                                              
BUFLW1   DS    CL1             LOW LEVEL                                        
*                                                                               
BUFFDATA DS    0CL36                                                            
BUFNAME  DS    CL36            ACCT  NAME                                       
BUFAMTS  DS    0CL72                                                            
BUFAMT1  DS    PL8             AMOUNTS                                          
BUFAMT2  DS    PL8                                                              
BUFAMT3  DS    PL8                                                              
BUFAMT4  DS    PL8                                                              
BUFAMT5  DS    PL8                                                              
BUFAMT6  DS    PL8                                                              
BUFAMT7  DS    PL8                                                              
BUFAMT8  DS    PL8                                                              
BUFAMT9  DS    PL8                                                              
BUFAMT10 DS    PL8             DUMMY ACCUMULATOR TO INSURE RETURN OF            
*                              ALL RECS                                         
BUFENDRC EQU   *                                                                
BUFCOLS  EQU   (BUFENDRC-BUFAMTS)/L'BUFAMT1                                     
*                                                                               
WORK1    DS    0CL82                                                            
WORK1A   DS    CL41                                                             
WORK1B   DS    CL41                                                             
*                                                                               
         ORG   WORK1                                                            
WORK2    DS    0CL76                                                            
WORK2A   DS    CL38                                                             
WORK2B   DS    CL38                                                             
         EJECT ,                                                                
*              STANDARD DSECTS                                                  
*                                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACBIGPRNTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDREPMASTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019ACREP3D02 06/03/15'                                      
         END                                                                    
