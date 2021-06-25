*          DATA SET PRREQ03    AT LEVEL 057 AS OF 06/20/19                      
*PHASE T41203A                                                                  
*INCLUDE SRCHCALL                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PRREQ03  NEW REQUEST  VALIDATE FIELDS PART - 1'                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BPLA 05/14      CHANGE FOR AT&T INTERFACE                                     
*                                                                               
* BPLA 01/14      ADD SOME INTERFACE REPORTS TO RTABLE                          
*                 THE LIST OF REPORTS TO IGNORE ESTIMATE                        
*                 DATES VS. START/END DATES CHECKING                            
*                                                                               
* BPLA 03/13      SAVE PO# BILLING CONTROLS FOR THE CLIENT                      
*                                                                               
* BPLA 06/07      LIMIT PRD GROUP BILLING REQUESTS TO AGY TH CLT TCF            
*                 (FOX) OR  SJR+DDSB CLT FOX                                    
*                                                                               
* BPLA 06/07      ACCEPT ADIDXXXXXXXXXXXX IN PUB FIELD                          
*                 FOR BILLING FOR ONE ADID                                      
*                                                                               
* BPLA 10/06      IF RDIV = ALL AND ONE PRODUCT ENTERED,                        
*                 SET RDIV TO ITS DIVISION (IF PRESENT)                         
*                                                                               
* BOBY 11/05      HANDLE 2 CH MEDIA OFFICE CODES                                
*                                                                               
* YKAP 04/11/2002 USE CORE-RESIDENT PUBVAL                                      
*                                                                               
* KWAN 09/06/01 ALLOW PRD GRP INPUTS ON PRODUCT CODE FIELD                      
*                                                                               
* KWAN 08/03/01 CLIENT TRAFFIC OFFICE LIMIT ACCESS (1)                          
*                                                                               
* BPLA 06/01    FIX OFFICE LIST SECURITY VALIDATION                             
*                                                                               
* KWAN 04/10/01 VALIDATIONS FOR CLT/PRD/PUB GRP RECORD PURGES                   
*                                                                               
* KWAN 04/00    DISALLOW PRD=SPACES WHEN "ES" IS USED IN STR END DATES          
*               (ONLY IF DATES ARE DIFFERENT FOR ALL PRD OF THAT EST)           
*                                                                               
* KWAN 03/00    BUG FIX IN JOBVAL FOR P01 AND P02                               
*                                                                               
* SMYE 11/99    IN CLIVAL ALLOW ONLY ONE SPECIFIC CLIENT FOR B1, D1,            
*               R1, & RD REQUESTS IF MEDIA * (ALL MEDIA)                        
*                                                                               
* KWAN 09/99    ADD NEW REQ ID: RT01                                            
*                                                                               
* SMYE 5/98     IN CLIVAL ACCEPT *N (OFFICE) FOR 48 FOR LIST-TYPE 1-4           
*                                                                               
* SMYE 9/97     IN PROVAL DO NOT READ FOR PRODUCT IF MASTER CLIENT AND          
*               REPORT REQUEST FOR 12, 14, 16 OR 18                             
*                                                                               
* SMYE 6/97     IN ESTVAL ACCEPT "L" FOR LAST AND "H" FOR HIGH AND              
*               DISPLAY ESTIMATE NUMBER SELECTED (REPLACE LAST & HIGH)          
*                                                                               
* SMYE 1/97     IN CLIVAL INCORPORATE CLIENT GROUP SECURITY                     
*                                                                               
* SMYE 1/97     IN ESTVAL ACCEPT "LAST" TO LOOK FOR "LATEST" START DATE         
*               AND "HIGH" TO LOOK FOR THE HIGHEST ESTIMATE NUMBER              
*                                                                               
* BPLA 8/26/94  IN CLIVAL SAVE DRD OVERRIDE CLT (FROM PCLTDRO X'30'             
*               ELEMENT) IN CLIPROF+20(3). IT WILL BE CHECKED IN                
*               PRREQ02 (BILLING POST VALIDATION)                               
*                                                                               
* BPLA 1/18/94  IN CLIVAL IF P48 AND OPTION 1 = V                               
*               THEY SHOULD ENTER THE ADV SO DON'T TRY TO READ AS A             
*               CLIENT.                                                         
*                                                                               
* BPLA 3/19/93  CHANGES FOR RFP                                                 
*                                                                               
* BPLA 1/29/93  CHANGES IN CLIVAL FOR AR                                        
*                                                                               
* BPLA 1/25/93  CODE TO CHECK LIMIT ACCESS FOR CLIENT GROUP REQS                
*                                                                               
* BPLA 3/5/92   CHANGES IN CLIVAL FOR AC AND AU                                 
*                                                                               
* BPLA 10/22/91 ADD 60,77,L1,LB,S2 TO LIST OF REPORTS FOR WHICH                 
*               START AND END ARE NOT REQUIRED TO BE IN ESTIMATE                
*               PERIOD (MAY BE USING BILLABLE,PAYABLE ETC.)                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT NOGEN                                                            
T41203   CSECT                                                                  
         NMOD1 000,T41203,R2,RR=R9          R2 NEW 2ND BASE REG 10/6/87         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*  ADDING ONE TO FIELD VALIDATION CODES CHANGES THEM FROM REQUIRED              
*  FIELDS TO OPTIONAL FIELDS                                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     R9,0(R1)                                                         
         USING REQTEMP,R9          R9=A(W/S)                                    
         L     R3,ASAVE                                                         
         USING TWAD,R3             R3=A(TWA)                                    
         EJECT                                                                  
         L     R1,FLDHADR          R1=A(FLD HDR TO BE VALED)                    
         SR    RF,RF                                                            
         IC    RF,ROUTNUM          RF=ROUTINE NUM REQD                          
         SLL   RF,2                                                             
         L     RF,ROUTADRT(RF)                                                  
         A     RF,RELO             RF=A(ROUTINE REQ)                            
         BASR  RE,RF               PASS CONTROL TO ROUTINE                      
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
EXITVAL  XIT1                                                                   
*                                                                               
*******************************************************                         
*                                                                               
         EJECT                                                                  
*        VALIDATE DIVISION AND SET FIND FORMAT BITS                             
*        X'02' =ALL                                                             
*        X'04' =XXX                                                             
*                                                                               
DIVVAL   NTR1                                                                   
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         CLI   FIND,1                                                           
         BL    DIVVO                                                            
         CLI   FIND,1                                                           
         BE    DIVVA5                                                           
*                                                                               
         CLI   RMED,C'*'            SEE IF ALL MEDIA REQ                        
         BE    DIVVO                YES THEN NO CHK                             
         CLI   RCLI,C'*'            IF NOT EQUAL BUT OFFICE SPECIFIED           
         BE    DIVVO                GO ON TO FURTHER VALIDATION                 
         CLI   RCLI,C'&&'           IF CLIENT GROUP REQUEST                     
         BE    DIVVO                GO ON TO FURTHER VALIDATION                 
         CLC   RCLI,=C'ALL'         CLIENT ALL REQUEST                          
         BE    DIVVO                                                            
*                                                                               
         XC    KRT1+7(18),KRT1+7    CLEAR END OF KEY                            
         MVI   KRT1+3,X'03'         MOVE IN DIV CODE                            
**                                                                              
         CLC   RPUB+1(3),=C'RD='                                                
         BNE   DIVVA3                                                           
         MVC   KRT1+4(3),RPUB+4                                                 
**                                                                              
DIVVA3   GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KRT1,KEYSAVE                  
**                                                                              
         CLC   RPUB+1(3),=C'RD='                                                
         BNE   DIVVA4                                                           
         CLC   KEYSAVE+7(3),=C'000'                                             
         BNE   DIVINV                                                           
**                                                                              
DIVVA4   CLC   KEYSAVE(7),KRT1        IF EQUAL, DIVISION FOUND                  
         BE    DIVVO                                                            
         B     DIVINV                 ELSE DIVISION NOT FOUND                   
*                                                                               
DIVVA5   CLI   RCLI,C'*'              IF ALL OFFICE CODE SPECIFIED              
         BNE   DIVVA8                                                           
         MVC   FERN,=AL2(FF)                                                    
         B     DIVVO                                                            
*                                                                               
DIVVA8   GOTO1 ARJN                   RIGHT JUSTIFY ROUTINE                     
*                                                                               
         CLC   TEMP+2(3),=C'000'                                                
         BNE   DIVV10                                                           
         MVC   FERN,=AL2(FF)                                                    
         B     DIVV10                                                           
*                                                                               
DIVV10   CLC   FERN,=AL2(FF)                                                    
         BL    DIVINV                                                           
         MVC   IFLD(3),TEMP+2                                                   
         MVC   KRT1+7(3),TEMP+2                                                 
         UNPK  RDIV(3),DUB+6(2)                                                 
         MVC   KRT1+7(3),RDIV                DIVISION TO KEY1                   
**                                                                              
         CLC   RPUB+1(3),=C'RD='                                                
         BNE   DIVV20                                                           
         CLC   RDIV,=C'000'                                                     
         BNE   DIVV20                        NO LONGER USE DRD CLT              
         MVC   KRT1+4(3),RPUB+4              HERE (UNLESS DIV =000)             
*                                            MAYBE NOT EVEN THEN?               
**                                                                              
DIVV20   OC    KRT1+4(3),KRT1+4              WAS CLIENT SPECIFIC                
         BZ    DIVINV                        NO                                 
         MVI   KRT1+3,03                     READ DIVISION                      
         GOTO1 AREAD,PLIST,C'PRT1'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    DIVVO                         DISK ERROR                         
         BH    DIVV30                                                           
         MVC   FERN,=AL2(DIVNOF)             DIVISION NOT ON FILE               
         B     DIVVO                                                            
DIVV30   OI    FIND,X'04'                                                       
DIVV40   MVC   NAME(20),PRTREC+35                                               
         B     DIVVO                                                            
*                                                                               
DIVINV   MVC   FERN,=AL2(DIVERR)             DIVISION INVALID                   
DIVVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         FOUT  (R6),NAME                                                        
DIVVX    B     EXITVAL                                                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE PRODUCT AND SET FIND FORMAT BITS                                     
* X'02' =ALL                                                                    
* X'04' =XXX                                                                    
* X'08' =X9999, X999*, X99*, X9*, X* (PRODUCT GROUP)                            
* X'10' =*XX OTHER AGY PRDS                                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROVAL   NTR1                                                                   
         GOTO1 AINITV              SET R4=A(HDR) & R5=L'DATA                    
*                                                                               
         BRAS  RE,CKPGRP           CHECK FOR PRD GRP                            
         BE    PROVXX              IT IS PRD GRP, DONE                          
         CHI   R5,3                                                             
         BNH   *+14                IF NOT PRD GRP, MAX INPUT IS 3 CHARS         
         MVC   FERN,=AL2(FLDINV)                                                
         B     PROVXX                                                           
*                                                                               
         CLC   RNUM,=C'01'         REQUEST 01?                                  
         BE    PROV05D                                                          
         CLC   RNUM,=C'02'         REQUEST 02?                                  
         BNE   PROV05X                                                          
*                                                                               
PROV05D  CLI   RO1,C'J'            RECORD TYPE J=JOBS                           
         BE    PROV05H                                                          
*                                                                               
* ONLY J RECORD TYPE IS VALID                                                   
*                                                                               
         MVC   FERN,=AL2(RTYPINV1)                                              
         B     PROVXX                                                           
*                                                                               
PROV05H  CLC   =C'ALL ',IFLD                                                    
         BNE   PROV05X                                                          
         OI    FIND,X'02'          PRODUCT=ALL BIT ON                           
*                                                                               
PROV05X  DS    0H                                                               
         CLI   FIND,1                                                           
         BNE   PROVO               PRODUCT =ALL OR MISSING                      
PROV1    CLC   RNUM,=C'04'                                                      
         BE    CKZZZ                                                            
         CLC   RNUM,=C'06'                                                      
         BE    CKZZZ                                                            
         CLC   RNUM,=C'07'                                                      
         BE    CKZZZ                                                            
         CLC   RNUM,=C'B1'                                                      
         BE    CKZZZ                                                            
         CLC   RNUM,=C'D1'                                                      
         BE    CKZZZ                                                            
         CLC   RNUM,=C'E1'                                                      
         BE    CKZZZ                                                            
         CLC   RNUM,=C'R1'                                                      
         BE    CKZZZ                                                            
         CLC   RNUM,=C'RD'         REBATE DRAFT                                 
         BE    CKZZZ                                                            
         CLC   RNUM,=C'66'         DUPONT REPORT                                
         BE    DUPON1                                                           
         CLC   RNUM,=C'L1'         OR L1 FOR DUPONT                             
         BE    DUPON0                                                           
         CLC   RNUM,=C'LB'         OR LB FOR DUPONT                             
         BNE   PROV1A                                                           
DUPON0   DS    0H                                                               
         B     PROV1A                                                           
DUPON1   MVC   KRT2(2),KRT2+4      MOVE CLT (AGY) TO AGY                        
         MVC   KRT2+4(3),=C'DP '   AGY DP NOW INACTIVE                          
*                                                                               
PROV1A   MVC   KRT2+7(3),IFLD                                                   
         OC    KRT2+4(3),KRT2+4    CLIENT SPECIFIC?                             
         BZ    PROINV              NO                                           
*                                                                               
PROV1D   CLI   CLIPROF+5,C'1'      MASTER CLIENT ?                              
         BNE   PROV2               NO                                           
         CLC   RNUM,=C'12'         CONTRACTS ?                                  
         BE    PROV1G              YES                                          
         CLC   RNUM,=C'14'         CONTRACTS LISTING ?                          
         BE    PROV1G              YES                                          
         CLC   RNUM,=C'16'         AUTOMATIC RATE CHG ?                         
         BE    PROV1G              YES                                          
         CLC   RNUM,=C'18'         CONTRACT ANALYSIS ?                          
         BNE   PROV2               NO - NOT 12, 14, 16, OR 18                   
*                                                                               
PROV1G   DS    0H                  DO NOT READ PRODUCT - MASTER CLIENT          
         OI    FIND,X'04'          JUST SET PRODUCT VALID =XXX                  
         XC    NAME(20),NAME       CLEAR NAME                                   
         B     PROVO               AND FINISH PRODUCT                           
*                                                                               
PROV2    MVI   KRT2+3,06                     READ PRODUCT                       
         GOTO1 AREAD,PLIST,C'PRT2'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    PROVO                         DISK ERROR                         
         BH    *+14                                                             
         MVC   FERN,=AL2(PRDNOF)             PRODUCT NOT ON FILE                
         B     PROVO                                                            
         OI    FIND,X'04'                    PRODUCT VALID =XXX                 
         MVC   NAME(20),PRTREC+35                                               
         CLI   IFLD,C'*'           CHK FOR OTHER AGY PRD                        
         BNE   PROV4                                                            
         MVI   FIND,X'11'          SET VALID *XX PRD                            
         B     PROV4                                                            
*                                                                               
PROV4    CLC   RNUM,=C'B1'                   NEW BILLING                        
         BE    PROV5                                                            
         CLC   RNUM,=C'D1'                   NEW DRAFT BILLING                  
         BE    PROV5                                                            
         CLC   RNUM,=C'R1'                   NEW REBATE BILLING                 
         BE    PROV5                                                            
         CLC   RNUM,=C'RD'                   NEW REBATE BILLING DRAFT           
         BE    PROV5                                                            
         B     PROVO                                                            
*                                                                               
PROV5    CLC   PRTREC+159(3),=C'   '         CHK FOR DIVISION                   
         BNH   PROV6                         IN THIS PRODUCT                    
         CLC   RDIV,=C'ALL'                  SEE IF ALL IN DIVISION             
         BNE   PROVO                                                            
         MVC   RDIV,PRTREC+159               SET TO PROPER DIVISION             
         B     PROVO                                                            
*                                                                               
PROV6    CLC   RPUB+1(3),=C'RD='   USING DRD CLT?                               
         BNE   PROVO                                                            
         MVC   RDIV,SPACES         CLEAR IF PRODUCT NOT IN A DIVISION           
         B     PROVO                                                            
*                                                                               
PROINV   MVC   FERN,=AL2(PRDERR)   PRODUCT INVALID                              
PROVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         TM    FIND,X'02'          PRD=ALL                                      
         BZ    PROVO5                                                           
         CLC   RNUM(2),=C'27'      PAYER'S LIST                                 
         BNE   PROVO5                                                           
         MVC   NAME(21),=C'** PRDS SEPARATELY **'                               
*                                                                               
PROVO5   DS    0H                                                               
         FOUT  (R6),NAME                                                        
PROVX    OC    PROSAVE,FIND                                                     
*                                                                               
PROVXX   B     EXITVAL                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKZZZ    CLC   IFLD(3),=C'ZZZ'         NO ZZZ BILLING ALLOWED                   
         BNE   PROV1A                                                           
         MVC   FERN,=AL2(NOZZZ)                                                 
         B     PROVO                                                            
         EJECT                                                                  
*        VALIDATE REGION AND SET FIND FORMAT BITS                               
*        X'02' =ALL                                                             
*        X'04' = XXX                                                            
*                                                                               
REGVAL   NTR1                                                                   
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         CLI   FIND,1                                                           
         BL    REGVO               REG=ALL OR MISSING                           
         CLI   FIND,1                                                           
         BE    REGVA20             REG=ALL OR MISSING                           
*                                                                               
         CLI   RMED,C'*'            SEE IF ALL MEDIA REQ                        
         BE    REGVO                YES THEN NO CHK                             
         CLI   RCLI,C'*'            SEE IF OFFICE REQUEST                       
         BE    REGVO                YES THEN NO CHK                             
         CLI   RCLI,C'&&'           SEE IF CLIENT GROUP REQUEST                 
         BE    REGVO                YES THEN NO CHK                             
         CLC   RCLI,=C'ALL'                                                     
         BE    REGVO                                                            
*                                                                               
         XC    KRT1+10(15),KRT1+10                                              
         MVI   KRT1+3,X'04'                                                     
**                                                                              
         CLC   RPUB+1(3),=C'RD='                                                
         BNE   REGVA5                                                           
         MVC   KRT1+4(3),RPUB+4                                                 
         MVC   KRT1+7(3),=C'000'     USE DIVISION 000                           
**                                                                              
REGVA5   GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KRT1,KEYSAVE                  
         OC    KRT1+7(3),KRT1+7                                                 
         BZ    REGVA10                                                          
         CLC   KRT1(10),KEYSAVE                                                 
         BNE   REGINV                                                           
         B     REGVO                                                            
*                                                                               
REGVA10  CLC   KRT1(7),KEYSAVE                                                  
         BNE   REGINV                                                           
         B     REGVO                                                            
*                                                                               
REGVA20  GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BL    REGINV                                                           
         MVC   IFLD(3),TEMP+2                                                   
         MVC   KRT1+10(3),TEMP+2             REGION TO KEY1                     
         CLC   RPUB+1(3),=C'RD='    SEE IF USING CLT R/D OVERRIDE               
         BNE   REGV1                                                            
         MVC   KRT1+4(3),RPUB+4                                                 
         MVC   KRT1+7(3),=C'000'       MUST BE DIV 000                          
*                                                                               
REGV1    OC    KRT1+7(3),KRT1+7              WAS DIVISION SPECIFIC              
         BZ    REGINV                        NO                                 
         CLC   TEMP+2(3),=C'999'                                                
         BNE   REGV2                                                            
         OI    FIND,X'04'                                                       
         B     REGVO         OMIT FILE READ FOR REG 999                         
REGV2    MVI   KRT1+3,04                     READ REGION                        
         GOTO1 AREAD,PLIST,C'PRT1'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    REGVO                         DISK ERROR                         
         BH    *+14                                                             
         MVC   FERN,=AL2(REGNOF)             REGION NOT ON FILE                 
         B     REGVO                                                            
         OI    FIND,X'04'                    REGION VALID =XXX                  
         MVC   NAME(20),PRTREC+35                                               
         B     REGVO                                                            
*                                                                               
REGINV   MVC   FERN,=AL2(REGERR)             REGION INVALID                     
REGVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         FOUT  (R6),NAME                                                        
REGVX    B     EXITVAL                                                          
         EJECT                                                                  
*        VALIDATE DISTRICT AND SET FIND FORMAT BITS                             
*        X'02' =ALL                                                             
*        X'04' =XXX                                                             
*                                                                               
DISVAL   NTR1                                                                   
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         CLI   FIND,1                                                           
         BL    DISVO                                                            
         CLI   FIND,1                                                           
         BE    DISVA5                                                           
*                                                                               
         CLI   RMED,C'*'            SEE IF ALL MEDIA REQ                        
         BE    DISVO                YES THEN NO CHK                             
         CLI   RCLI,C'*'            SEE IF OFFICE REQUEST                       
         BE    DISVO                YES THEN NO CHK                             
         CLI   RCLI,C'&&'           SEE IF CLIENT GROUP REQUEST                 
         BE    DISVO                YES THEN NO CHK                             
         CLC   RCLI,=C'ALL'                                                     
         BE    DISVO                                                            
*                                                                               
*                                                                               
         XC    KRT1+13(12),KRT1+13                                              
         MVI   KRT1+3,X'05'                                                     
**                                                                              
         CLC   RPUB+1(3),=C'RD='                                                
         BNE   DISVAA                                                           
         MVC   KRT1+4(3),RPUB+4                                                 
**                                                                              
DISVAA   GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KRT1,KEYSAVE                  
         OC    KRT1+10(3),KRT1+10                                               
         BZ    DISVA1                                                           
         CLC   KRT1(13),KEYSAVE                                                 
         BNE   DISINV                                                           
         B     DISVO                                                            
*                                                                               
DISVA1   OC    KRT1+7(3),KRT1+7                                                 
         BZ    DISVA3                                                           
         CLC   KRT1(10),KEYSAVE                                                 
         BNE   DISINV                                                           
         B     DISVO                                                            
*                                                                               
DISVA3   CLC   KRT1(7),KEYSAVE                                                  
         BNE   DISINV                                                           
         B     DISVO                                                            
*                                                                               
DISVA5   GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BL    DISINV                                                           
         MVC   IFLD(3),TEMP+2                                                   
         MVC   KRT1+13(3),TEMP+2                                                
         OC    KRT1+10(3),KRT1+10            WAS REGION SPECIFIC                
         BZ    DISINV                        NO                                 
         CLC   TEMP+2(3),=C'999'                                                
         BNE   DISV2                                                            
         OI    FIND,X'04'                                                       
         B     DISVO        OMIT FILE READ FOR DIS 999                          
DISV2    MVI   KRT1+3,05                     READ DISTRICT                      
         GOTO1 AREAD,PLIST,C'PRT1'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    DISVO                         DISK ERROR                         
         BH    *+14                                                             
         MVC   FERN,=AL2(DISERR)             DISTRICT NOT ON FILE               
         B     DISVO                                                            
         OI    FIND,X'04'         DIS=XXX                                       
         MVC   NAME(20),PRTREC+35                                               
         B     DISVO                                                            
*                                                                               
DISINV   MVC   FERN,=AL2(DISERR)             DISTRICT INVALID                   
DISVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         FOUT  (R6),NAME                                                        
DISVX    B     EXITVAL                                                          
         EJECT                                                                  
*        VALIDATE ESTIMATE(S) AND SET FIND FORMAT BITS                          
*        X'02' = ALL                                                            
*        X'04' =NNN                                                             
*        X'08' =NNN-NNN                                                         
*        X'10' = ALL,XXX   FILTERS                                              
*        X'20' =NNN  + NON SPECIFIC PROD                                        
*        X'40' = NNN-NNN  + NON SPECIFIC PROD                                   
*        X'80' =NO,XXX             FILTERS                                      
*                                                                               
ESTVAL   NTR1                                                                   
         MVI   ROUTSUB,1           TEST FIRST FOR "HIGH" AND "LAST"             
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         XC    ESTDATES,ESTDATES                                                
         CLC   IFLD(4),=C'HIGH'    SEARCH FOR HIGHEST NUMBER USED               
         BE    EVAL2                                                            
         CLC   IFLD(4),=C'LAST'  SEARCH FOR EST WITH HIGHEST START DATE         
         BE    EVAL2                                                            
         CLI   IFLDH+5,1                                                        
         BNE   EVALX               EST NOT HIGH OR LAST OR H OR L               
         CLI   IFLD,C'H'                                                        
         BE    EVAL2                                                            
         CLI   IFLD,C'L'                                                        
         BNE   EVALX               EST NOT HIGH OR LAST OR H OR L               
EVAL2    DS    0H                                                               
         TM    PROSAVE,X'0C'       IS PRODUCT SPECIFIC ?                        
         BZ    ESTVE1              NO - HIGH AND LAST ARE INVALID               
         XC    SVESDT,SVESDT       CLEAR SEARCH EST START DATE                  
         XC    SVLEST,SVLEST       CLEAR ESTIMATE NUMBER                        
         XC    KRT2+10(15),KRT2+10 CLEAR KEY FOLLOWING PROD                     
         MVI   KRT2+3,X'07'        READ ESTIMATE                                
         XC    KEYSAVE,KEYSAVE                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KRT2,KEYSAVE                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EVAL4B                                                           
*                                                                               
EVAL4    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'PRTDIR',KEYSAVE,KEYSAVE               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EVAL4B   CLC   KRT2(10),KEYSAVE    CHECK THRU PRODUCT                           
         BNE   EVAL4T                                                           
         MVC   FULL,KEYSAVE+27     SAVE REC ADDRESS                             
         CLI   IFLD,C'H'           SEE IF LOOKING FOR HIGHEST EST               
         BNE   EVAL4D                                                           
         MVC   SVLEST,KEYSAVE+10   SAVE NUMBER AND KEEP SEARCHING               
         B     EVAL4                                                            
*                                                                               
EVAL4D   DS    0H                  READ THE RECORD                              
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'PRTFIL',FULL,PRTREC,DMWORK            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*****    CLC   IFLD(4),=C'HIGH'    FINISHING "HIGH" PROCESSING ?                
         CLI   IFLD,C'H'           FINISHING "HIGH" PROCESSING ?                
         BE    EVAL4TD             YES - GO DO IT                               
         CLC   SVESDT,PRTREC+55    EST DATE IN RECORD HIGH ?                    
         BH    EVAL4               NO - KEEP SEARCHING                          
         MVC   SVESDT,PRTREC+55    YES - SAVE DATES                             
         MVC   ESTDATES(12),PRTREC+55                                           
         MVC   SVLEST,PRTREC+10          AND EST NUM                            
         MVC   NAME(20),PRTREC+35        AND NAME                               
         B     EVAL4               LOOK FOR HIGHER DATE                         
*                                                                               
EVAL4T   DS    0H                  LAST ESTIMATE READ                           
         OC    SVLEST,SVLEST       SEE IF ANY FOUND                             
         BZ    ESTVE1              NO - INVALID EST ERROR EXIT                  
         OI    FIND,X'04'          EST=NNN                                      
*****    CLC   IFLD(4),=C'LAST'    LOOKING FOR HIGHEST START DATE ?             
         CLI   IFLD,C'L'           LOOKING FOR HIGHEST START DATE ?             
         BE    EVAL4TX             YES - FINISH UP                              
         B     EVAL4D            FOR "HIGH" GO READ REC FOR DATA BELOW          
EVAL4TD  MVC   ESTDATES(12),PRTREC+55    SAVE DATES                             
         MVC   NAME(20),PRTREC+35        AND NAME                               
EVAL4TX  LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         EDIT  (B2,SVLEST),(3,0(R7)),0,ALIGN=LEFT,FILL=0,ZERO=NOBLANK           
         XC    8(4,R4),8(R4)                                                    
         FOUT  (R4),0(R7),3                                                     
         FOUT  (R6),NAME                                                        
         B     ESTVX               DONE WITH "LAST" AND "HIGH"                  
*                                                                               
EVALX    DS    0H                  EST NOT "LAST" OR "HIGH"                     
         CLI   FIND,1                                                           
         BNE   ESTVO               EST = ALL OR MISSING                         
         CLI   IFLDH+5,3                                                        
         BH    ESTVE                                                            
         CLC   IFLD(3),=C'NO '                                                  
         BNE   ESTV1                                                            
         OI    FIND,X'80'                                                       
         CLC   5(1,R4),IFLDH+5                                                  
         BE    ESTVE1              'NO' MUST HAVE FILTERS                       
         B     ESTVO                                                            
*                                                                               
ESTV1    GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BL    ESTVE                                                            
         MVC   IFLD(3),TEMP+2                                                   
         TM    PROSAVE,X'0C'                                                    
         BNZ   ESTV1A                                                           
         OI    FIND,X'20'      EST=NNN + NON SPECIFIC PROD                      
         B     ESTVO                                                            
*                                                                               
ESTV1A   MVC   KRT2+10(2),TEMP                                                  
         MVI   KRT2+3,X'07'                                                     
         GOTO1 AREAD,PLIST,C'PRT2'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    ESTVO               DISK ERROR                                   
         BH    *+14                                                             
         MVC   FERN,=AL2(ESTNOF)   NOT ON FILE                                  
         B     ESTVX                                                            
         MVC   ESTDATES(12),PRTREC+55                                           
         MVC   NAME(20),PRTREC+35                                               
         OI    FIND,X'04'          EST=NNN                                      
         B     ESTVO                                                            
*                                                                               
ESTVE    MVC   FERN,=AL2(ESTINV)                                                
ESTVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         FOUT  (R6),NAME                                                        
*                                                                               
ESTV3    CLC   5(1,R4),IFLDH+5     ONLY ONE FIELD                               
         BE    ESTVX                                                            
ESTV3A   TM    FIND,X'A6'     WAS FIRST FLD NNN,NO,ALL                          
         BZ    ESTVE1              NO - CAN'T HAVE SECOND EST                   
         NI    FIND,X'59'                                                       
*                                                                               
*              CHK FOR NO,XXX   FILTERS                                         
*                                  OR ALL,XXX                                   
*                                                                               
         CLC   0(2,R7),=C'NO'                                                   
         BE    ESTV3B                                                           
         CLC   0(3,R7),=C'ALL'                                                  
         BE    ESTV3B                                                           
         B     ESTV3K                                                           
*                                                                               
ESTV3B   DS    0H                                                               
         MVI   ROUTSUB,0           SET TO REEDIT FIELD                          
         GOTO1 AINITV                                                           
*                                  CHECK FILTERS                                
         CLC   0(2,R7),=C'NO'                                                   
         BE    ESTV3B4                                                          
         CLI   IFLDH+5,7                                                        
         BL    ESTVE1                                                           
         CLI   IFLDH+5,10                                                       
         BH    ESTVE1                                                           
         ZIC   R1,IFLDH+5                                                       
         AHI   R1,-4          ADJUST FOR ALL,                                   
         LA    R4,IFLD+4                                                        
         B     ESTV3B6                                                          
*                                                                               
ESTV3B4  DS    0H                                                               
         CLI   IFLDH+5,6                                                        
         BL    ESTVE1                                                           
         CLI   IFLDH+5,9                                                        
         BH    ESTVE1              MAX IS -X-X-X                                
         ZIC   R1,IFLDH+5          SAVE INPUT LENGHT                            
         AHI   R1,-3               ADJUST FOR NO,                               
         LA    R4,IFLD+3                                                        
ESTV3B6  LA    R5,3                FOR BCT                                      
         XC    TEMP(5),TEMP                                                     
         LA    R6,TEMP+2                                                        
ESTV3C   EQU   *                                                                
         CLI   0(R4),C'-'          SEE IF NEGATIVE FILTER                       
         BNE   ESTV3E              NO                                           
         MVI   TEMP,1                                                           
         LA    R4,1(R4)            PAST -                                       
         BCTR  R1,0                DECREMENT COUNTER                            
ESTV3E   MVC   0(1,R6),0(R4)                                                    
         CLI   0(R6),C'*'                                                       
         BNE   ESTV3F                                                           
         CLI   TEMP,0                                                           
         BNE   ESTVE1              -* IS INVALID                                
         B     ESTV3G                                                           
*                                                                               
ESTV3F   CLI   0(R6),C'A'                                                       
         BL    ESTVE1                                                           
         CLI   0(R6),C'9'                                                       
         BH    ESTVE1                                                           
         CLI   TEMP,1            SEE IF NEGATIVE FILETR                         
         BNE   *+8                                                              
         NI    0(R6),X'BF'         SET OFF X'40'                                
ESTV3G   LA    R6,1(R6)                                                         
         MVI   TEMP,0            ZERO NEGATIVE INDICATOR                        
         LA    R4,1(R4)                                                         
         BCTR  R1,0                                                             
         BCT   R5,ESTV3C                                                        
         LTR   R1,R1                                                            
         BNZ   ESTVE1              SHOULD HAVE NO MORE INPUT                    
         CLC   0(3,R7),=C'ALL'                                                  
         BNE   ESTV3H                                                           
         MVI   FIND,X'11'          ALL,XXX                                      
         B     ESTV5                                                            
*                                                                               
ESTV3H   DS    0H                                                               
         MVI   FIND,X'81'          NO,XXX                                       
         B     ESTV5                                                            
*                                                                               
ESTV3K   EQU   *                                                                
         MVI   ROUTSUB,2                                                        
         GOTO1 AINITV                                                           
         CLI   IFLDH,1                                                          
         BNE   ESTVE1                                                           
         GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BL    ESTVE1                                                           
         TM    PROSAVE,X'0C'                                                    
         BNZ   *+18                                                             
         OI    FIND,X'40'          EST=NNN-NNN + NON SPECIFIC PRD               
         XC    ESTDATES,ESTDATES                                                
         B     *+8                                                              
         OI    FIND,X'08'          EST=NNN-NNN                                  
ESTV5    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   3(3,R7),TEMP+2                                                   
         CLC   0(2,R7),=C'NO'                                                   
         BNE   ESTV6                                                            
         MVC   0(3,R7),=C'   '     SET TO BLANKS                                
         B     ESTVX                                                            
*                                                                               
ESTV6    DS    0H                                                               
         CLC   0(3,R7),=C'ALL'                                                  
         BE    ESTVX                                                            
         CLC   0(3,R7),3(R7)                                                    
         BL    ESTVX                                                            
         MVC   FERN,=AL2(EST1G2)                                                
         B     ESTVX                                                            
*                                                                               
ESTVE1   MVC   FERN,=AL2(ESTINV)                                                
ESTVX    MVC   ESTSAVE,FIND                                                     
         B     EXITVAL                                                          
         EJECT                                                                  
*        VALIDATE PUBLICATION,ZONE,EDITION AND SET FIND FORMAT BITS             
*        X'02' =ALL   SET TO BLANKS IN REQ REC                                  
*        X'04' NNNNNNNN                                                         
*        X'08' NNNNNNNN,XX,X                                                    
*        X'10' JNNNNNN           JOB NUMBER                                     
*        X'20' NUM,Z,ALL                                                        
*        X'40' NUM,ALL                                                          
*        X'10' ADIDXXXXXXXXXXXX      ADID  - SAME BIT AS JOB                    
*                                                                               
PZEVAL   NTR1                                                                   
*                                                                               
*        PUB NAME SEARCHING                                                     
*                                                                               
*        DATA SET PPMBC00N   AT LEVEL 113 AS OF 08/23/91                        
*                                                                               
*        NAME SEARCH CALL                                                       
*                                                                               
         L     R4,FLDHADR                                                       
         SR    R4,R3               GET DISPLACEMENT INTO TWA OF PUB             
         LA    R5,TEMP                                                          
         USING DSPARM,R5                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,RMED                                                    
*                                                                               
         GOTO1 =V(SRCHCALL),PLIST,(3,(R4)),(X'80',(R3)),ACOMFACS,      X        
               ('DSPARML',TEMP),(1,=CL8'PUB'),0,RR=RELO                         
         DROP  R5                                                               
*                                                                               
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
*                                                                               
         CLC   RNUM,=C'01'         REQUEST 01?                                  
         BE    PZEV0FA                                                          
         CLC   RNUM,=C'02'         REQUEST 02?                                  
         BNE   PZEV001                                                          
PZEV0FA  LR    R8,R3                                                            
         USING T412FFD,R8                                                       
         XC    BVRHDR,BVRHDR                                                    
*                                                                               
         CLI   IFLDH+5,0           ANY INPUTS?                                  
         BE    PZEVXXX                                                          
         CLI   RO1,C'P'            RECORD TYPE MUST BE PUBS                     
         BE    PZEV001                                                          
         MVC   BVRHDR(L'RN01ERR2),RN01ERR2                                      
         MVC   FERN,=AL2(FE)                                                    
         B     PZEVXXX                                                          
         DROP  R8                                                               
*                                                                               
PZEV001  DS    0H                                                               
*                                                                               
         TM    FIND,X'02'     CHK FOR PUB=ALL                                   
         BZ    PZEV1      MOVE ALL TO REQ REC UNLESS RPUB USED                  
         CLC   RPUB(4),=4C' '                                                   
         BE    PZEV0                                                            
         B     PZEVX         RPUB USED ALREADY SO LEAVE ALONE                   
PZEV0    MVC   RPUB(3),=C'ALL'                                                  
         B     PZEVX                                                            
*                                                                               
PZEV1    CLI   FIND,1              PUB= MISSING - EXIT                          
         BNE   PZEVX                                                            
         CLC   RPUB(4),=4C' '                                                   
         BE    EDTJOB                                                           
         MVC   FERN,=AL2(NOPUB)  RPUB USED ALREADY - ERROR                      
         B     PZEVX                                                            
*                                                                               
EDTJOB   CLI   IFLD,C'J'                                                        
         BNE   EDTADID                                                          
         CLI   IFLDH+5,7                                                        
         BH    EJOBINV                                                          
*                                                                               
         MVC   KRT2+10(6),IFLD+1                                                
         OC    KRT2+7(3),KRT2+7                                                 
         BZ    EJOBINV         PRODUCT MUST BE SPECIFIED                        
         MVI   KRT2+3,X'15'                                                     
         GOTO1 AREAD,PLIST,C'PRT2'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    EJOBVO                                                           
         BH    *+14                                                             
         MVC   FERN,=AL2(JOBNOF)                                                
         B     EJOBVO                                                           
         OI    FIND,X'10'            VALID JOB INPUT                            
         B     EJOBVO                                                           
*                                                                               
EJOBINV  MVC   FERN,=AL2(JOBERR)                                                
EJOBVO   MVI   RPUB+1,C'J'                                                      
         MVC   RPUB+2(6),KRT2+10                                                
         B     PZEVXX                                                           
*                                                                               
EDTADID  CLC   IFLD(4),=C'ADID'      ADID ENTRY                                 
         BNE   EDTPUB                                                           
         CLI   IFLDH+5,16            4+12                                       
         BNE   EADIINV                                                          
*                                                                               
         MVC   KRT2+10(12),IFLD+4                                               
         OC    KRT2+7(3),KRT2+7                                                 
         BZ    EADIINV         PRODUCT MUST BE SPECIFIED                        
         MVI   KRT2+3,X'C1'                                                     
         GOTO1 AREAD,PLIST,C'PRT2'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    EADIVO                                                           
         BH    *+14                                                             
         MVC   FERN,=AL2(RECNOF)                                                
         B     EADIVO                                                           
         OI    FIND,X'10'      VALID ADI INPUT (SAME AS JOB)                    
         B     EADIVO                                                           
*                                                                               
EADIINV  MVC   FERN,=AL2(FLDINV)                                                
         B     PZEVXX                                                           
*                                                                               
EADIVO   MVI   RCONT,C'*'            SET 2 CARD REQ                             
         MVC   RCARD2+8(12),KRT2+10     COL 9                                   
         B     PZEVXX                                                           
*                                                                               
*                                                                               
*                                                                               
EDTPUB   MVC   TEMP(20),IFLD           SAVE ALL OF INPUT                        
         LHI   R7,4                                                             
         MVI   ROUTSUB,1                                                        
         GOTO1 AINITV                                                           
         CLC   IFLD(3),=C'ALL'                                                  
         BE    PZEINV                                                           
         CLI   IFLDH+5,0                                                        
         BE    PZEINV                                                           
         LR    R8,R5                                                            
EDTP1    MVI   ROUTSUB,2                                                        
         GOTO1 AINITV                                                           
         CLC   IFLD(3),=C'ALL'                                                  
         BE    EDTP1E                                                           
         CLI   IFLDH+5,0                                                        
         BE    EDTP1D                                                           
         AR    R8,R5                                                            
         LA    R8,1(R8)                                                         
         LHI   R7,5                                                             
         CLI   IFLD,C'0'       NOT NUMERIC SO NO 3RD FLD ALLOWED                
         BL    EDTP1D                                                           
         MVI   ROUTSUB,3                                                        
         GOTO1 AINITV                                                           
         CLI   IFLDH+5,0                                                        
         BNE   EDTP1A                                                           
         LHI   R7,6                                                             
         B     EDTP2                                                            
*                                                                               
EDTP1A   CLC   IFLD(3),=C'ALL'                                                  
         BNE   EDTP1B                                                           
         B     EDTP2                                                            
*                                                                               
EDTP1B   AR    R8,R5                                                            
         LA    R8,1(R8)                                                         
         LHI   R7,6                                                             
         B     EDTP2                                                            
*                                                                               
EDTP1D   LHI   R7,6                                                             
EDTP1E   MVI   ROUTSUB,3                                                        
         GOTO1 AINITV                                                           
         CLI   IFLDH+5,0                                                        
         BNE   PZEINV                                                           
*                                                                               
EDTP2    GOTO1 PUBVAL,PLIST,((R8),TEMP),(1,RPUB)                                
*                                                                               
         CLI   0(R1),X'FF'                   PUBLICATION TO REQ REC             
         BE    PZEINV                                                           
         GOTO1 (RF),(R1),,(0,KUB1+1)         PUBLICATION TO KEY#1               
*                                                                               
*        READ AGYHEADER FOR DEFAULT PROFILE BYTE                                
*                                                                               
         MVC   TEMP(25),KRT2       SAVE KRT2 KEY                                
         XC    KRT2+4(21),KRT2+4                                                
         MVI   KRT2+3,X'01'                                                     
         GOTO1 AREAD,PLIST,C'PRT2'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    PZEVX         DISK ERROR                                         
         BH    *+6                                                              
         DC    H'0'         AGY HEADER NOT FOUND                                
         MVC   DFSW,PRTREC+117            SRDS DEFAULT PROFILE BYTE             
*                                                                               
         MVI   KUB1+9,X'81'                                                     
         MVC   KRT2(25),TEMP       RESTORE KRT2 KEY                             
         B     PZEV2                                                            
*                                                                               
*                                                                               
*                                                                               
HIGH     LA    R4,=C'DMRDHI'                                                    
         B     READ                                                             
SEQ      LA    R4,=C'DMRSEQ'                                                    
READ     LA    R5,=C'PUBDIR'                                                    
*                                                                               
         ST    R6,FULL                                                          
         LA    R6,KUB1                                                          
         B     FILE                                                             
GET      LA    R4,=C'GETREC'                                                    
         LA    R5,=C'PUBFILE'                                                   
         LA    R6,PRTREC+27                                                     
FILE     GOTO1 DATAMGR,DMCB,(R4),(R5),(R6),PRTREC                               
*                                                                               
         L     R6,FULL                                                          
         CLI   DMCB+8,0        DISK ERROR                                       
         BE    *+14                                                             
         MVC   FERN,=AL2(0)                                                     
         B     PZEVX                                                            
         BR    R8                                                               
*                                                                               
*                                                                               
*                                                                               
PZEV2    BAS   R8,HIGH                                                          
         B     *+8                                                              
PZEV2A   BAS   R8,SEQ                                                           
         EX    R7,*+8            R7 WAS SET FOR LENGHT TO COMPARE               
         B     *+10                                                             
         CLC   PRTREC(0),KUB1                                                   
         BNE   NOTFND                                                           
         CLC   PRTREC+7(3),KUB1+7                                               
         BE    PZEV5              FOUND                                         
         CLI   DFSW,C'0'                                                        
         BE    PZEV2A                                                           
         CLC   PRTREC+7(2),=C'ZZ'                                               
         BE    PZEV5        ZZ FOUND                                            
         B     PZEV2A            GO READ SEQ                                    
*                                                                               
*                                                                               
PZEV5    BAS   R8,GET                                                           
         MVC   NAME(20),PRTREC+35                                               
         CHI   R7,6                                                             
         BNE   PZEV6                                                            
         OC    KUB1+5(2),KUB1+5                                                 
         BNZ   PZEV5A                                                           
         OI    FIND,X'04'          NUM                                          
         B     PZEVX                                                            
PZEV5A   OI    FIND,X'08'          NUM,Z,E                                      
         B     PZEVX                                                            
*                                                                               
PZEV6    CHI   R7,4                                                             
         BNE   PZEV6A                                                           
         OI    FIND,X'40'          NUM,ALL,ALL                                  
         MVC   RPUB+8(3),=C'ZZZ'                                                
         B     PZEVX                                                            
PZEV6A   OI    FIND,X'20'          NUM,Z,ALL                                    
         MVI   RPUB+10,C'Z'                                                     
         B     PZEVX                                                            
*                                                                               
DFSW     DC    X'00'            PAGYPROF+16     DEFAULT PUB READ BYTE           
*                                                                               
NOTFND   MVC   FERN,=AL2(PUBNOF)                                                
         B     PZEVX                                                            
*                                                                               
*                                                                               
*                                                                               
PZEINV   MVC   FERN,=AL2(PUBERR)             PUBLICATION INVALID                
PZEVX    FOUT  (R6),NAME                                                        
PZEVXX   MVC   PUBSAVE,FIND                                                     
PZEVXXX  DS    0H                                                               
         B     EXITVAL                                                          
         EJECT                                                                  
*        VALIDATE START DATE AND SET FIND FORMAT BITS                           
*        X'04' =YYMM                                                            
*        X'08' =YYMMDD                                                          
*        X'10' = ES                                                             
*        X'20' = ES  +EST =ALL OR ALL+FILTERS (FOR 52/EC WITH 'BILL'            
*                    IN CONTROL DATE)                                           
*                                                                               
SEDVAL   NTR1                                START,END DATES -FIND BITS         
         MVI   ROUTSUB,1                     04=YYMM 08=YYMMDD 10=ES            
         GOTO1 AINITV                                                           
         CLI   5(R4),0                                                          
         BE    SEDVO                         START,END NOT INPUT                
         CLI   IFLDH,1                                                          
         BNE   SEDVE                                                            
         CLI   IFLDH+5,2                                                        
         BNE   SEDV1                                                            
         CLC   IFLD(2),=C'ES'                                                   
         BNE   SEDVE                                                            
*                                                                               
         CLC   RPRO(3),=C'   '     BLANK PRDS NEED MORE VALIDATIONS             
         BNE   SEDV0                                                            
         CLC   REST(3),=C'   '     DON'T NEED TO CHECK FOR BLANK EST            
         BE    SEDV0                                                            
         BRAS  RE,CKESDAT                                                       
         BE    SEDV0               BLANK PRDS ARE ACCEPTABLE                    
*                                                                               
         LR    R8,R3               BLANK PRDS ARE NOT ALLOWED FOR "ES"          
         USING T412FFD,R8                                                       
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(L'RNXXERR0),RNXXERR0                                      
         MVC   FERN,=AL2(FE)                                                    
         B     SEDVXX                                                           
         DROP  R8                                                               
*                                                                               
SEDV0    DS    0H                                                               
*                                                                               
         OI    FIND,X'10'                    START = ES                         
         MVC   RSTRD(2),IFLD                                                    
         B     SEDV2                                                            
SEDV1    GOTO1 DATVAL,PLIST,(0,IFLD),RSTRD                                      
         OC    PLIST(4),PLIST                                                   
         BE    *+12                                                             
         OI    FIND,X'08'                    START = YYMMDD                     
         B     SEDV2                                                            
         GOTO1 (RF),(R1),(2,IFLD)                                               
         OC    PLIST(4),PLIST                                                   
         BE    SEDVE                                                            
         OI    FIND,X'04'                    START = YYMM                       
SEDV2    MVI   ROUTSUB,2                                                        
         GOTO1 AINITV                                                           
         CLI   IFLDH,1                                                          
         BL    SEDV3                         END NOT INPUT                      
         BH    SEDVE                                                            
         CLI   IFLDH+5,2                                                        
         BNE   SEDV4                                                            
         CLC   IFLD(2),=C'ES'                ALLOW ES OR ES,ES                  
         BNE   SEDVE                                                            
SEDV3    TM    FIND,X'10'                                                       
         BZ    SEDVE                         END DATE MISSING                   
         TM    ESTSAVE,X'24'                                                    
         BNZ   SEDVO                         ES ONLY FOR SPECIFIC EST           
         TM    ESTSAVE,X'12'      THEN MUST BE 'ALL' OR ALL+FILTERS             
         BZ    SEDVE              ERROR                                         
         NI    FIND,X'EF'         SET OFF X'10'                                 
         OI    FIND,X'20'         ES AND EST = 'ALL' OR ALL+FILTERS             
         B     SEDVO                                                            
*                                                                               
SEDV4    TM    FIND,X'0C'                                                       
         BZ    SEDVE                                                            
         GOTO1 DATVAL,PLIST,(0,IFLD),RENDD                                      
         OC    PLIST(4),PLIST                                                   
         BE    *+16                                                             
         TM    FIND,X'08'                                                       
         BZ    SEDVE                                                            
         B     SEDV5                         STR = END = YYMMDD                 
         GOTO1 (RF),(R1),(2,IFLD)                                               
         OC    PLIST(4),PLIST                                                   
         BE    SEDVE                                                            
         TM    FIND,X'04'                                                       
         BZ    SEDVE                         STR = END = YYMM                   
SEDV5    CLC   RSTRD,RENDD                                                      
         BNH   SEDV6                                                            
         MVC   FERN,=AL2(SEDSGE)             START GT END                       
         B     SEDVO                                                            
SEDV6    CLI   ESTDATES,0                                                       
         BE    SEDVO                                                            
*                     BILLING AND PAYING DATES NEED NOT FALL IN EST PD          
*                     RTABLE IS A LIST OF REPORTS FOR WHICH                     
*                     BILLING OR PAYING DATES MAY BE USED                       
*****                                                                           
         ST    R6,FULL                                                          
         LA    R6,RTABLE                                                        
SEDV7    CLC   RNUM(2),0(R6)                                                    
         BE    SEDVO                                                            
         CLI   0(R6),X'FF'           END OF TABLE                               
         BE    SEDV7X                                                           
         LA    R6,2(R6)                                                         
         B     SEDV7                                                            
*                                                                               
SEDV7X   CLC   ESTDATES(6),RENDD                                                
         BH    *+14                                                             
         CLC   ESTDATES+6(6),RSTRD                                              
         BNL   SEDVO                                                            
         MVC   FERN,=AL2(SEDNIE)             ERROR DATES NOT WITHIN EST         
         B     SEDVO                                                            
SEDVE    MVC   FERN,=AL2(SEDINV)             DATES INVALID                      
SEDVO    EQU   *                                                                
*****                                                                           
         L     R6,FULL                                                          
         MVC   SEDSAVE,FIND                                                     
SEDVX    DS    0H                                                               
         CLI   FIND,0              IF NO INPUT                                  
         BE    SEDVXX              EXIT                                         
         CLC   FERN,=AL2(FF)       IF NO ERROR                                  
         BE    SEDVXX              EXIT                                         
         TM    RFPSTAT,RFPINUSE          SEE IF $RFP IN USE                     
         BZ    SEDVXX                                                           
*                                                                               
*              $RFP - VALIDATE SYMBOLIC EQUATE                                  
*                                                                               
         MVI   FIND,X'01'                                                       
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,IFLD                                                    
         OC    QRFPWORK,SPACES                                                  
         MVC   FERN,=AL2(ISYMBEQU)      INVALID SYMBOLIC EQUATE                 
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   SEDVXX                                                           
*                                                                               
         CLC   QRFPDICT,=Y(PP#RFPST)    START DATE (YYMMDD) + NO END            
         BNE   *+12                                                             
         OI    FIND,X'80'                                                       
         B     START400                                                         
*                                                                               
         CLC   QRFPDICT,=Y(PP#RFPSM)    START DATE (YYMM) + NO END              
         BNE   *+12                                                             
         OI    FIND,X'40'                                                       
         B     START400                                                         
*                                                                               
         CLC   QRFPDICT,=Y(PP#RFPRD)    START-END (YYMMDD-YYMMDD)               
         BNE   *+12                                                             
         OI    FIND,X'08'                                                       
         B     START400                                                         
*                                                                               
         CLC   QRFPDICT,=Y(PP#RFPRM)    START-END (YYMM-YYMM)                   
         BNE   *+12                                                             
         OI    FIND,X'04'                                                       
         B     START400                                                         
*                                                                               
         B     SEDVXX                                                           
*                                                                               
START400 MVC   RSTRD,SPACES             STORE ESCAPE SEQ IN REQCARD             
         MVC   RSTRD(L'QRFPESC),QRFPESC                                         
         MVI   RSTRD+3,12                                                       
         MVC   FERN,=AL2(FF)                                                    
*                                                                               
SEDVXX   DS    0H                                                               
         B     EXITVAL                                                          
*                                                                               
*                                                                               
RTABLE   DC    C'102728363752607792AICHECGMGTINJWLBLOLTL1S2WBXTVL'              
         DC    X'FFFF'      END OF LIST                                         
*                                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE JOD CODE AND SET FIND BITS                                    
*        X'02'=ALL                                                              
*        X'04'=XXXXX                                                            
*                                                                               
JOBVAL   NTR1                                                                   
*                                                                               
         GOTO1 AINITV                                                           
*                                                                               
         CLC   RNUM,=C'01'         REQUEST 01?                                  
         BE    JOBV0FA                                                          
         CLC   RNUM,=C'02'         REQUEST 02?                                  
         BNE   JOBV001                                                          
JOBV0FA  LR    R8,R3                                                            
         USING T412FFD,R8                                                       
         XC    BVRHDR,BVRHDR                                                    
*                                                                               
         CLI   IFLDH+5,0           NO AD CODE IS OK                             
         BE    JOBVX                                                            
*                                                                               
         CLI   RO1,C'J'            RECORD TYPE MUST BE JOBS (AD CODES)          
         BNE   JOBV0AD                                                          
         CLC   RCLI(3),=C'   '     CLIENT PRESENT?                              
         BE    JOBV0AB                                                          
         CLC   RPRO(3),=C'   '     PRODUCT PRESENT?                             
         BNE   JOBV001                                                          
         MVC   BVRHDR(L'RN01ERR4),RN01ERR4                                      
         MVI   ROUTNUM,X'06'                                                    
         B     JOBV0AF                                                          
JOBV0AB  MVC   BVRHDR(L'RN01ERR4),RN01ERR4                                      
         MVI   ROUTNUM,X'02'                                                    
         B     JOBV0AF                                                          
JOBV0AD  MVC   BVRHDR(L'RN01ERR3),RN01ERR3                                      
         B     JOBV0AM                                                          
*                                                                               
JOBV0AF  DS    0H                                                               
*                                                                               
         BAS   RE,PCURSOR          POINT CURSOR TO TARGETED FLD                 
*                                                                               
JOBV0AM  MVC   FERN,=AL2(FE)                                                    
         B     JOBVX                                                            
         DROP  R8                                                               
*                                                                               
JOBV001  DS    0H                                                               
*                                                                               
         CLI   FIND,1                                                           
         BNE   JOBVO            JOB=ALL OR MISSING                              
         MVC   KRT2+10(6),IFLD                                                  
         OC    KRT2+7(3),KRT2+7                                                 
         BZ    JOBINV            PRODUCT NOT SPECIFIED                          
         MVI   KRT2+3,X'15'                                                     
         GOTO1 AREAD,PLIST,C'PRT2'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    JOBVO         DISK ERROR                                         
         BH    *+14                                                             
         MVC   FERN,=AL2(JOBNOF)       NOT FOUND                                
         B     JOBVO                                                            
         OI    FIND,X'04'         JOB=XXXXX                                     
         B     JOBVO                                                            
*                                                                               
JOBINV   MVC   FERN,=AL2(JOBERR)                                                
*                                                                               
JOBVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(6,R7),IFLD                                                     
*                                                                               
         CLC   RNUM,=C'01'         REQUEST 01?                                  
         BE    JOBVO60                                                          
         CLC   RNUM,=C'02'         REQUEST 02?                                  
         BNE   *+8                                                              
JOBVO60  MVI   RPUB+1,C'J'         FOR JOBS                                     
*                                                                               
JOBVX    B     EXITVAL                                                          
         EJECT                                                                  
*                                                                               
*              VALIDATE ASPO                                                    
*              X'80' = ASPO=ALL OR XXXXXXXXX                                    
*                                                                               
ASPOVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   ASPOVO                                                           
         CLC   IFLD(5),=C'ASPO='                                                
         BNE   EDTPUB              GO USE PUBEDIT IN PZEVAL                     
         CLI   IFLDH+5,5                                                        
         BNH   ASPOVE                                                           
         CLI   IFLDH+5,14                                                       
         BH    ASPOVE              TOO LONG                                     
         CLC   IFLD+5(3),=C'ALL'                                                
         BE    ASPOV2                                                           
         TM    CLISAVE,X'02'       CLT CAN'T BE ALL FOR ONE ASPO                
         BNZ   ASPOVE                                                           
*                                                                               
ASPOV2   DS    0H                                                               
         OI    FIND,X'80'                                                       
         B     ASPOVO                                                           
*                                                                               
ASPOVE   MVC   FERN,=AL2(FLDINV)                                                
         B     ASPOVX                                                           
*                                                                               
ASPOVO   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(9,R7),IFLD+5                                                   
ASPOVX   B     EXITVAL                                                          
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
*                                                                               
KEY      DS    CL32                                                             
KEYSAVE  DS    CL32                                                             
SVESDT   DS    CL6                 ESTIMATE DATE (YYMMDD)                       
SVLEST   DS    H                   ESTIMATE NUMBER (BINARY)                     
DATADISP DC    H'33'                                                            
ELCODE   DS    X                                                                
DMWORK   DS    12D                                                              
WORK     DS    CL48                                                             
TRAFID   DS    CL1                 C'T' - THIS IS A TRAFFIC SIGN ON             
*                                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* THIS TABLE CONTAINS THE ADDRESSES OF THE VALIDATION ROUTINES                  
* CONTAINED IN THIS PHASE. INDEXED BY ROUTNUM                                   
*                                                                               
ROUTADRT DC    F'0'                00                                           
         DC    A(CLIVAL)           01 - CLIENT                                  
         DC    A(DIVVAL)           02 - DIVISION                                
         DC    A(PROVAL)           03 - PRODUCT                                 
         DC    A(REGVAL)           04 - REGION                                  
         DC    A(DISVAL)           05 - DISTRICT                                
         DC    A(ESTVAL)           06 - ESTIMATE(S)                             
         DC    A(PZEVAL)           07 - PUBLICATION,ZONE,EDN                    
         DC    A(SEDVAL)           08 - START,END DATES                         
         DC    A(JOBVAL)           09 - JOB (AD CODE)                           
         DC    A(ASPOVAL)          10 - PUB OR ASPO                             
*                                                                               
FLDMIS   EQU   001                                                              
FLDINV   EQU   002                                                              
JOBERR   EQU   002                                                              
PRDERR   EQU   015                                                              
ESTINV   EQU   016                                                              
EST1G2   EQU   016                                                              
PUBERR   EQU   018                                                              
SEDINV   EQU   020                                                              
DIVERR   EQU   022                                                              
REGERR   EQU   023                                                              
DISERR   EQU   024                                                              
CLINOF   EQU   040                                                              
PRDNOF   EQU   041                                                              
ESTNOF   EQU   042                                                              
PUBNOF   EQU   044                                                              
DIVNOF   EQU   045                                                              
REGNOF   EQU   046                                                              
DISNOF   EQU   047                                                              
JOBNOF   EQU   053                                                              
RECNOF   EQU   053                                                              
ACCERR   EQU   055                 LIMIT ACCESS ERROR                           
RTYPINV1 EQU   058                                                              
SEDSGE   EQU   080                                                              
SEDNIE   EQU   081                                                              
NUMINV   EQU   090                 NO DETAIL BILLING FOR THIS CLT               
NOZZZ    EQU   178                                                              
NOSLAV   EQU   189                                                              
NOPUB    EQU   249                 CAN'T USE PUB FLD WITH CLT OVERRIDE          
CAERROR  EQU   207                 CLIENT LIMIT ACCESS ERROR                    
*                                                                               
* OTHER DESCRIPTIVE ERROR MSGS (RN=REQUEST NUMBER)                              
*                                                                               
RN01ERR2 DC    C'** ERROR ** - RECORD TYPE MUST BE "P"'                         
RN01ERR3 DC    C'** ERROR ** - RECORD TYPE MUST BE "J"'                         
RN01ERR4 DC    C'** ERROR ** - MISSING INPUT FIELD'                             
*                                                                               
RNXXERR0 DC    C'** ERROR ** - EST DATES CONFLICT, MUST ENTER DATES'            
*                                                                               
*                                                                               
*                                                                               
PCURSOR  DS    0H                  POSITION CURSOR TO TARGET FIELD              
*                                                                               
         LA    R0,24               SEARCH REQ MAP TABLE                         
         LA    R1,LREQMAP                                                       
CHKREQ1  CLI   0(R1),127                                                        
         JE    CHKREQ2                                                          
         CLC   ROUTNUM,0(R1)                                                    
         JE    CHKREQ3                                                          
         LA    R1,3(R1)                                                         
         BCT   R0,CHKREQ1                                                       
CHKREQ2  LA    R1,LREQMAP          NOT IN TBL POSN TO 1ST FLD                   
CHKREQ3  MVC   HALF,1(R1)                                                       
         ST    RE,FULL                                                          
         LR    RE,R3                                                            
         AH    RE,HALF                                                          
CHKREQ4  ST    RE,FADR             POSN CURSOR TO ROUTNUM FLD                   
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRREQ03 NEW REQUEST VAL FIELDS PART - 1 - CHKGRP'               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*        CHECK CLIENT GROUP SECURITY                                  *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
CHKGRP   NTR1  BASE=*,LABEL=*        * TEST CLIENT GROUP SECURITY *             
*                                                                               
         XC    PRTREC(10),PRTREC   ESTABLISH CLIENT RECORD                      
         LA    R7,PRTREC                                                        
         USING PCLTRECD,R7                                                      
*                                                                               
*        READ IN CLIENT HEADERS                                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),KRT1         SET AGENCY AND MEDIA IN KEY                  
         MVI   KEY+3,X'02'         WANT CLIENT RECORDS                          
*                                                                               
         BAS   R8,PRTHIGH          READ FIRST CLIENT HEADER                     
*                                                                               
CKGPLP   CLC   KEY(4),KEYSAVE      DONE ON CHANGE IN MEDIA                      
         BNE   CKGPDN                                                           
*                                                                               
         BAS   R8,PRTGET           READ IN RECORD                               
*                                                                               
         CLC   PCLTBLGP,IFLD+1     IGNORE IF NOT RIGHT GROUP                    
         BNE   CKGPCN                                                           
*                                                                               
*        USE OFFICER TO CHECK IF SIGN ON HAS ACCESS TO CLIENT                   
*                                                                               
         XC    WORK,WORK           ESTABLISH OFFICE CONTROL BLOCK               
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'         SYSTEM ID                                    
         MVC   OFCAUTH,6(R3)       ID AUTH VALUE                                
         MVC   OFCLMT,6(R3)        SECURITY LIMITS                              
*                                                                               
         MVC   OFCAGY,RAGY         SET AGENCY                                   
         MVC   OFCCLT,PCLTKCLT     SET CLIENT                                   
         MVC   OFCOFC,PCLTOFF      SET CLIENT OFFICE                            
         MVC   OFCSECD,ASECBLK     A(FASECRET BLOCK)                            
*                                                                               
         CLI   TRAFID,C'T'         IF TRAFFIC ID                                
         BNE   CKGP10                                                           
*                                                                               
*        FIND TRAFFIC OFFICE                                                    
*                                                                               
         LA    R6,PRTREC           POINT TO CLIENT REC                          
         USING PCLTTOEL,R6         ESTABLISH AS TRAFFIC OFFICE ELEM             
         MVI   ELCODE,X'50'        CLIENT TRAFFIC OFFICE ELEM CODE              
*                                                                               
         BRAS  RE,GETEL            FIND TRAFFIC OFFICE ELEMENT                  
         BNE   *+10                SKIP IF NO TRAFFIC OFFICE FOUND              
         MVC   OFCOFC,PCLTTOFC     REPLACE PCLTOFF WITH TRAFFIC OFFICE          
*                                                                               
CKGP10   DS    0H                                                               
*                                                                               
         DROP  R1                                                               
*                                                                               
         GOTO1 OFFICER,PLIST,(C'2',WORK),ACOMFACS                               
         CLI   0(R1),0                                                          
         BNE   CKGPERR             FAILS SECURITY CHECK                         
*                                                                               
CKGPCN   BAS   R8,PRTSEQ           READ NEXT CLIENT HEADER                      
*                                                                               
CKGPDN   DS    0H                                                               
         CR    RB,RB               PASSES SECURITY - CC SET TO EQ               
*                                                                               
         B     CHKGRPX                                                          
*                                                                               
CKGPERR  DS    0H                                                               
         LTR   RB,RB               SET CC TO NE                                 
*                                                                               
CHKGRPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
*                                                                               
*        DATAMGR CALLS                                                          
*                                                                               
PRTHIGH  LA    R4,=C'DMRDHI'                                                    
         MVC   KEYSAVE(25),KEY                                                  
         B     PRTREAD                                                          
*                                                                               
PRTSEQ   LA    R4,=C'DMRSEQ'                                                    
PRTREAD  LA    R5,=C'PRTDIR'                                                    
*                                                                               
         ST    R6,FULL                                                          
         LA    R6,KEY                                                           
         LA    R3,KEY                                                           
         B     PRTFILE                                                          
*                                                                               
PRTGET   LA    R4,=C'GETREC'                                                    
         LA    R5,=C'PRTFILE'                                                   
         LA    R6,KEY+27         DISK ADDR                                      
         LA    R3,PRTREC                                                        
*                                                                               
*                                                                               
PRTFILE  GOTO1 DATAMGR,DMCB,(R4),(R5),(R6),(R3)                                 
         L     R3,ASAVE       MUST RESTORE R3                                   
         L     R6,FULL                                                          
         CLI   DMCB+8,0        CHK FOR DISK ERROR                               
         BE    *+14                                                             
         MVC   FERN,=AL2(0)                                                     
         J     CLIVOKX                                                          
         BR    R8                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R7                                                               
*                                                                               
         TITLE 'PRREQ03 NEW REQUEST VAL FIELDS PART - 1 - CKESDAT'              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*        CHECKING PRD WITH 'ES' AS EST DATES                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
CKESDAT  NTR1  BASE=*,LABEL=*      CHECKING PRD WITH "ES" AS EST DATES          
*                                                                               
         XC    WORK,WORK           PREPARE WORK FOR ST AND END DATES            
         PACK  DUB,REST                                                         
         CVB   R8,DUB                                                           
         STH   R8,HALF                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PESTKEY,R3                                                       
         MVC   PESTKAGY,RAGY       AGENCY                                       
         MVC   PESTKMED,RMED       MEDIA                                        
         MVI   PESTKRCD,X'07'      EST RECORD CODE                              
         MVC   PESTKCLT,RCLI       CLIENT                                       
         DROP  R3                                                               
*                                                                               
         BAS   R8,PRTHIG01                                                      
         B     CKESD60                                                          
*                                                                               
CKESD50  DS    0H                                                               
         BAS   R8,PRTSEQ01                                                      
*                                                                               
CKESD60  DS    0H                                                               
         CLC   KEY(7),KEYSAVE                                                   
         BNE   CKESD99             HAVE CHECKED ALL PRD UNDER GIVEN EST         
*                                                                               
         CLC   HALF,KEY+10         SAME ESTIMATE?                               
         BNE   CKESD50             NO, GET NEXT ESTIMATE RECORD                 
*                                                                               
         BAS   R8,PRTGET01                                                      
         LA    R3,PRTREC+33                                                     
         USING PESTELEM,R3                                                      
         CLI   PESTELEM,X'07'      FIRST ESTIMATE ELEM?                         
         BE    *+6                                                              
         DC    H'0'                ELEM HAS TO BE THERE                         
         OC    WORK(12),WORK                                                    
         BNZ   *+10                                                             
         MVC   WORK(12),PESTST     SHOULD ONLY COPY DATES ONCE                  
         CLC   WORK(12),PESTST                                                  
         BNE   CKESDERR            IF DATES ARE DIFFERENT, ERROR!               
         B     CKESD50             GET NEXT ESTIMATE RECORD                     
*                                                                               
PRTHIG01 LA    R4,=C'DMRDHI'                                                    
         MVC   KEYSAVE(25),KEY                                                  
         B     PRTREA01                                                         
*                                                                               
PRTSEQ01 LA    R4,=C'DMRSEQ'                                                    
PRTREA01 LA    R5,=C'PRTDIR'                                                    
*                                                                               
         ST    R6,FULL                                                          
         LA    R6,KEY                                                           
         LA    R3,KEY                                                           
         B     PRTFIL01                                                         
*                                                                               
PRTGET01 LA    R4,=C'GETREC'                                                    
         LA    R5,=C'PRTFILE'                                                   
         LA    R6,KEY+27         DISK ADDR                                      
         LA    R3,PRTREC                                                        
*                                                                               
PRTFIL01 GOTO1 DATAMGR,DMCB,(R4),(R5),(R6),(R3)                                 
         L     R3,ASAVE       MUST RESTORE R3                                   
         L     R6,FULL                                                          
         CLI   DMCB+8,0        CHK FOR DISK ERROR                               
         BE    *+14                                                             
         MVC   FERN,=AL2(0)                                                     
         J     CLIVOKX                                                          
         BR    R8                                                               
*                                                                               
*                                                                               
CKESD99  SR    R8,R8                                                            
CKESDERR LTR   R8,R8               R8 HAS SOMETHING IN IT                       
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRREQ03 NEW REQUEST VAL FIELDS PART - 1 - CLIVSUB'              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*        SUBROUTINE FOR MORE CLT VALIDATIONS                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
CLIVSUB  NTR1  BASE=*,LABEL=*      SUBROUTINE FOR MORE CLT VALIDATIONS          
*                                                                               
         L     R3,ASAVE                                                         
         USING TWAD,R3             R3=A(TWA)                                    
*                                                                               
         CLC   RNUM,=C'PM'         REQUEST PM?                                  
         BNE   CVSUB20                                                          
         CLC   RAGY,=C'BS'                                                      
         BNE   CVSUB10                                                          
         CLC   =C'PM',IFLD                                                      
         BE    CVSUBX              CONTINUE VALIDATION                          
         B     CVSUBINV                                                         
*                                                                               
CVSUB10  CLC   RAGY,=C'H9'                                                      
         BNE   CVSUB10E                                                         
         CLC   =C'PMT',IFLD                                                     
         BE    CVSUBX              CONTINUE VALIDATION                          
         CLC   =C'PCR',IFLD                                                     
         BE    CVSUBX              CONTINUE VALIDATION                          
         CLC   =C'YSP',IFLD                                                     
         BE    CVSUBX              CONTINUE VALIDATION                          
         B     CVSUBINV                                                         
*                                                                               
CVSUB10E CLC   RAGY,=C'YN'                                                      
         BNE   CVSUBINV                                                         
         CLC   =C'PMU',IFLD                                                     
         BE    CVSUBX              CONTINUE VALIDATION                          
         B     CVSUBINV                                                         
*                                                                               
CVSUB20  CLC   RNUM,=C'01'         REQUEST 01?                                  
         BE    CVSUB20E                                                         
         CLC   RNUM,=C'02'         REQUEST 02?                                  
         BNE   CVSUB30                                                          
*                                                                               
CVSUB20E CLI   RO1,C'H'            RECORD TYPE H=CLIENT                         
         BE    CVSUB20M                                                         
         CLI   RO1,C'J'            RECORD TYPE J=JOBS                           
         BE    CVSUB20M                                                         
         CLI   RO1,C'L'            RECORD TYPE L=PUBLIST                        
         BE    CVSUB20M                                                         
*                                                                               
         CLI   RO1,C'E'            RECORD TYPE E=PRD GRP                        
         BNE   CVSUBRTY                                                         
         CLC   =C'ALL ',IFLD       CLT=ALL IS NOT ALLOWED FOR PRD GRP           
         BE    CVSUBINV                                                         
         CLI   IFLDH+5,0           CLT IS REQUIRED FOR PRD GRP PURGES           
         BNE   CVSUB20M                                                         
         OI    FIND,X'01'          SO THAT MISSING MSG CAN BE SHOWN             
         B     CVSUBMIS                                                         
*                                                                               
* ONLY H,J,L,E RECORD TYPES ARE VALID                                           
*                                                                               
CVSUB20M CLC   =C'ALL ',IFLD                                                    
         BE    CVSUB20R                                                         
         CLC   =C'ZZZ ',IFLD                                                    
         BNE   CVSUB30                                                          
*                                                                               
CVSUB20R OI    FIND,X'04'          NOTE: ZZZ WORK SAME AS ALL                   
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         FOUT  (R6),NAME                                                        
         OC    CLISAVE,FIND                                                     
         B     CVSUBERR            NOT ERROR, BUT TO EXIT                       
*                                                                               
CVSUB30  DS    0H                  FOR FUTURE USES                              
         B     CVSUBX                                                           
*                                                                               
CVSUBINV MVC   FERN,=AL2(FLDINV)                                                
         B     CVSUBERR                                                         
CVSUBMIS MVC   FERN,=AL2(FLDMIS)                                                
         B     CVSUBERR                                                         
CVSUBRTY MVC   FERN,=AL2(RTYPINV1)                                              
         B     CVSUBERR                                                         
*                                                                               
CVSUBX   CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CVSUBERR LTR   RB,RB               NOT EQUAL (ERROR)                            
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
         TITLE 'PRREQ03 NEW REQUEST VAL FIELDS PART - 1 - CHKPGRP'              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* CHECK IF PRODUCT GROUP INPUT HAS BEEN ENTERED ON PRD CODE FIELD     *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
CKPGRP   NTR1  BASE=*,LABEL=*      NOTE: PRTREC MUST HAVE CLIENT REC!           
*                                                                               
         CLC   IFLD(4),=C'PGR='    PRD GRP FILTER?                              
         BNE   CKPGRPER                                                         
*                                                                               
         CLC   RNUM,=C'B1'         SEE IF BILLING REQ                           
         BE    CKPG5                                                            
         CLC   RNUM,=C'D1'         OR DRAFT BILLING                             
         BNE   CKPG5X                                                           
*                                                                               
CKPG5    CLC   RAGY,=C'TH'         MUST BE ZENITH AND CLT TCF                   
         BNE   CKPG5C                                                           
         CLC   RCLI,=C'TCF'                                                     
         BNE   CKPGRPER                                                         
         B     CKPG5X                                                           
*                                                                               
CKPG5C   CLC   RAGY,=C'SJ'         OR SJR AND CLT FOX                           
         BNE   CKPG5D                                                           
         CLC   RCLI,=C'FOX'                                                     
         BNE   CKPGRPER                                                         
         B     CKPG5X                                                           
*                                                                               
CKPG5D   CLC   RAGY,=C'*B'         DDSB AND CLT FOX                             
         BNE   CKPGRPER                                                         
         CLC   RCLI,=C'FOX'                                                     
         BNE   CKPGRPER                                                         
*                                                                               
CKPG5X   CLI   IFLD+4,C'A'         FIRST CHAR IS ALPHA?                         
         BL    CKPGRPER                                                         
         CLI   IFLD+4,C'I'                                                      
         BNH   CKPG30              A,B,C,D,E,F,G,H,I                            
         CLI   IFLD+4,C'J'                                                      
         BL    CKPGRPER                                                         
         CLI   IFLD+4,C'R'                                                      
         BNH   CKPG30              J,K,L,M,N,O,P,Q,R                            
         CLI   IFLD+4,C'S'                                                      
         BL    CKPGRPER                                                         
         CLI   IFLD+4,C'Z'                                                      
         BH    CKPGRPER            S,T,U,V,W,X,Y,Z                              
*                                                                               
CKPG30   LA    RE,IFLD+5           POINT TO INPUT FIELD                         
         LA    RF,1                INPUT COUNTER                                
*                                                                               
         LR    R8,R5               GET LENGTH                                   
         AHI   R8,-4               MINUS 4 BYTES "PGR=" OVERHEAD                
*                                                                               
         CHI   R8,1                ALPHA PORTION OF SCHEME CODE ONLY?           
         BE    CKPG70                                                           
*                                                                               
CKPG40   CLI   0(RE),C'0'          NUMERIC?                                     
         BL    CKPGRPER                                                         
         CLI   0(RE),C'9'          NUMERIC?                                     
         BH    CKPGRPER                                                         
         LA    RE,1(RE)            POINT TO NEXT CHAR IN INPUT                  
         AHI   RF,1                INCREMENT FOR CHAR COUNTER                   
         CR    R8,RF                                                            
         BE    CKPG50              VALID SCHEME CODE ENTERED                    
         B     CKPG40                                                           
*                                                                               
CKPG50   AHI   RF,-2               MINUS 1 FOR ALPHA AND 1 FOR EX               
         CHI   RF,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD LENGTH!                                  
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   REXTRA+25(0),IFLD+5 NUMERIC PORTION OF SCHEME CODE               
*                                                                               
CKPG70   MVC   REXTRA+24(1),IFLD+4  ALPHA PART OF SCHEME CODE                   
         MVI   RCONT,C'*'          INDICATOR FOR SECOND REQUEST CARD            
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),=C'ALL'     PRD GRP NEEDS "ALL" IN PRD CODE              
*                                                                               
         MVC   NAME(19),=C'** PRODUCT GROUP **'                                 
         FOUT  (R6),NAME                                                        
         OI    FIND,X'08'          SET PRODUCT GROUP BIT ON                     
*                                                                               
CKPGRPX  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKPGRPER LTR   RB,RB               NOT EQUAL (NOT PRD GRP INPUT)                
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
         TITLE 'PRREQ03 NEW REQUEST VAL FIELDS PART - 1 - CLIVAL'               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE CLIENT AND SET FIND FORMAT BITS                                      
*                                                                               
* X'02' = ALL                                                                   
* X'04' = XXX                                                                   
* X'08' = *XX     OFFICE CODE                                                   
* X'10' = XXX-XXX SECOND XXX IS REG/DST OVERRIDE CLT                            
* X'20' = XXX-XXX SECOND XXX IS SLAVE FOR FIRST                                 
* X'40' = $N      FOR CONTRACTS OFFICE LISTS OR ALL*=ALL OFFICES                
* X'80' = &N      CLIENT GROUP                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLIVAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,ASAVE            ESTABLISH TWA                                
         USING TWAD,R3             R3=A(TWA)                                    
*                                                                               
         XC    CLTPOOPT(20),CLTPOOPT  CLEAR PO BILLING OPTIONS                  
*                                                                               
         BRAS  RE,CKTRAFID         CHECK IF WE HAVE TRAFFIC SIGN ON             
*                                                                               
         MVI   ROUTSUB,0           READ IN COMPLETE FIELD                       
         GOTO1 AINITV              SET R4=A(HDR) & R5=L'DATA                    
*                                                                               
         BRAS  RE,CLIVSUB          DO SPECIAL VALIDATIONS                       
         BNE   CLIVXX              ERROR - JUST EXIT                            
*                                                                               
         CLC   RNUM,=C'L1'         IF REQUEST L1                                
         BE    CLV0                                                             
         CLC   RNUM,=C'LB'         OR LB                                        
         BNE   CLV                                                              
*                                                                               
CLV0     DS    0H                                                               
*                                                                               
CLV      CLC   =C'ALL*',IFLD       IF ALL OFFICES                               
         BNE   CLVB                                                             
*                                                                               
         OC    6(2,R3),6(R3)       AND NO LIMIT ACCESS                          
         BNZ   CACCERR                                                          
*                                                                               
         OI    FIND,X'40'             INDICATOR                                 
*                                                                               
         MVC   RCLI(2),=C'$*'                                                   
         MVC   NAME(15),=C'* ALL OFFICES *'                                     
*                                                                               
         B     CLIVOKX                                                          
*                                                                               
CLVB     CLC   RNUM(2),=C'81'                                                   
         BNE   CLVC                                                             
         CLC   8(2,R4),=C'RI'      ONLY CLT RI FOR REQ 81                       
         BNE   CLIVE                                                            
*                                                                               
* MUST BE ONE ONLY CLIENT FOR MEDIA * BILLING REQ'S                             
*                                                                               
CLVC     DS    0H                                                               
*                                                                               
         CLI   RMED,C'*'           "ALL" MEDIA ?                                
         BNE   CLVD                NO                                           
*                                                                               
         CLC   =C'B1',RNUM                                                      
         BE    CLVCC               GO TEST CLIENT                               
         CLC   =C'D1',RNUM                                                      
         BE    CLVCC               GO TEST CLIENT                               
         CLC   =C'R1',RNUM                                                      
         BE    CLVCC               GO TEST CLIENT                               
         CLC   =C'RD',RNUM                                                      
         BNE   CLVD                NOT "BILLING" REQUEST                        
*                                                                               
CLVCC    DS    0H                  TEST FOR "SPECIFIC" CLIENT                   
*                                                                               
         CLI   IFLDH+5,3           MORE THAN 3 CHARACTERS ?                     
         BH    CLIVE               YES - ERROR                                  
*                                                                               
         CLC   =C'ALL',IFLD        ALL CLIENTS ?                                
         BE    CLIVE               YES - ERROR                                  
*                                                                               
         CLI   IFLD,C'*'           OFFICE ?                                     
         BE    CLIVE               YES - ERROR                                  
         CLI   IFLD,C'$'           OFFICE LIST ?                                
         BE    CLIVE               YES - ERROR                                  
         CLI   IFLD,C'&&'          GROUP ?                                      
         BE    CLIVE               YES - ERROR                                  
*                                                                               
CLVD     XC    CLIPROF,CLIPROF                                                  
         XC    CLISAVE(6),CLISAVE  CLEARS CLT,PRD,EST,PUB,S-E                   
*                                                                               
         CLC   RNUM,=C'48'         SEE IF PUB LISTING - AOR ADV                 
         BNE   CLVD5                                                            
*                                                                               
         CLI   RO1,C'V'            AOR ADV LISTING?                             
         BNE   CLVD5                                                            
*                                                                               
         OI    FIND,X'04'                                                       
*                                                                               
         B     CLIVOK              DON'T VAL AS A CLT, IT'S AOR ADV             
*                                                                               
CLVD5    DS    0H                                                               
*                                                                               
         TM    FIND,X'02'          IF REQUESTING ALL CLIENTS                    
         BZ    CLIV                                                             
*                                                                               
         CLC   RNUM,=C'47'            BYPASS CLT SECURITY FOR 47                
         BE    CLIVOK                                                           
*                                                                               
         MVI   CLIPROF+9,C'0'      FOR BILLING - NEED EST=ALL,NNN               
*                                                                               
         OC    6(2,R3),6(R3)       CHK LIMIT ACCESS                             
         BZ    CLIVOK              NONE                                         
*                                                                               
         B     CACCERR                                                          
*                                                                               
CLIV     CLI   FIND,1              IF CLIENT FIELD HAS AN ENTRY                 
         BNE   CLIVOK                 NO - 'ALL' OR NOTHING                     
                                                                                
*----------------------------------------------------------------------         
* VALIDATE OFFICE LIST                                                          
*----------------------------------------------------------------------         
CLIVOFL  DS    0H                                                               
*                                                                               
         CLI   IFLD,C'$'          TEST FOR OFFICE LIST                          
         BNE   CLIVBGP                                                          
*                                                                               
         XR    R1,R1              1-1 ASSUME 1 CHAR OFFICE LIST FOR EX          
*                                                                               
         CLI   IFLDH+5,2          LIST CODE ONE CHARACTER?                      
         BL    CLIVE              YES                                           
         CLI   IFLDH+5,3          LIST CODE IS TWO CHARACTERS                   
         BH    CLIVE                                                            
*                                                                               
         XC    WORK,WORK          WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAGY,RAGY                                                      
         MVC   OFCMOL2,IFLD+1     EXTERNAL OFFICE LIST                          
         OI    OFCINDS,OFCIMOLC   CONVERT OFFICE LIST                           
         DROP  R1                                                               
         GOTO1 OFFICER,DMCB,(C'2',WORK),ACOMFACS,0,0                            
         BNE   CLIVE                                                            
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
         MVI   RCLI,C'$'                                                        
         MVC   RCLI+1(1),OFCMOL   SINGLE CHARACTER OFFICE LIST                  
         MVI   RCLI+2,0                                                         
         DROP  R1                                                               
*                                                                               
         MVC   NAME(14),=C'OFFICE LIST X '                                      
         MVC   NAME+12(2),IFLD+1                                                
*                                                                               
         OI    FIND,X'40'          OFFICE LIST                                  
*                                                                               
         OC    6(2,R3),6(R3)       CHK FOR LIMIT ACCESS                         
         BZ    CLIVOFLX            NONE                                         
*                                                                               
         LHI   R1,0                                                             
         CLI   IFLDH+5,2          LIST CODE ONE CHARACTER?                      
         BNE   *+8                YES                                           
         LHI   R1,1                                                             
*                                                                               
         CLI   6(R3),C'$'          OFFICE LIST LOCKOUT                          
         BNE   CACCERR             NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   7(0,R3),IFLD+1      OFFICE LIST MUST MATCH                       
         BNE   CACCERR                                                          
CLIVOFLX B     CLIVOKX                                                          
                                                                                
*----------------------------------------------------------------------         
* VALIDATE BILLING GROUP                                                        
*----------------------------------------------------------------------         
CLIVBGP  DS    0H                                                               
*                                                                               
         CLI   IFLD,C'&&'          CLIENT BILLING GROUP                         
         BNE   CLIVBGPN                                                         
*                                                                               
         CLI   IFLDH+5,2           MAX 1 CHARACTER GROUP ID                     
         BH    CLIVE                                                            
*                                                                               
         CLI   RMED,C'*'           DISALLOW FOR MEDIA *                         
         BE    CLIVE                                                            
*                                                                               
         MVC   NAME(7),=C'GROUP  '                                              
         MVC   NAME+6(1),IFLD+1                                                 
*                                                                               
         OI    FIND,X'80'          CLIENT GROUP                                 
*                                                                               
         OC    6(2,R3),6(R3)       CHK FOR LIMIT ACCESS TERMINAL                
         BZ    CLIVBGPX            NO                                           
*                                                                               
         CLI   6(R3),C'*'          OFFICE LOCKOUT                               
         BE    CLIVBGP5                                                         
         CLI   6(R3),C'$'          OR OFFICE LIST LOCKOUT                       
         BE    CLIVBGP5                                                         
*                                                                               
         B     CACCERR             MUST BE SINGLE CLIENT - NO GROUPS            
*                                                                               
CLIVBGP5 DS    0H                                                               
*                                                                               
         BRAS  RE,CHKGRP                                                        
         BNE   CACCERR             DONE ON SECURITY ERROR                       
*                                                                               
CLIVBGPX DS    0H                                                               
         B     CLIVOK                                                           
*                                                                               
CLIVBGPN DS    0H                                                               
*                                                                               
*        VALIDATE OFFICE OR SINGLE CLIENT                                       
*                                                                               
CLIVOFC  DS    0H                                                               
*                                                                               
         CLC   IFLD(2),=C'*-'      BYPASS DELIMITER                             
         BNE   CLIVOFC0            ALL BUT THIS OFFICE                          
*                                                                               
         OC    6(2,R3),6(R3)       MUST HAVE NO SECURITY RESTRICTIONS           
         BZ    *+14                                                             
         MVC   FERN,=AL2(CAERROR)  NO                                           
         B     CLIVXX                                                           
*                                                                               
         CLI   IFLDH+5,4           MAX 4 CH'S ALLOWWED *-XX                     
         BH    CLIVE                                                            
*                                                                               
         B     CLIVOFC2                                                         
*                                                                               
CLIVOFC0 DS    0H                                                               
*                                                                               
         CLI   IFLDH+5,3           SKIP IF SINGLE CLT OR OFC                    
         BNH   CLIVOFC1                                                         
*                                  TO ALLOW FOR *XX-XX                          
         MVI   ROUTSUB,1           SCAN FOR 1ST FIELD                           
         GOTO1 AINITV              SET R4=A(HDR) & R5=L'DATA                    
*                                                                               
CLIVOFC1 BCTR  R5,0                DECREMENT FOR EXECUTE                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   RCLI(0),IFLD        CLIENT TO REQ REC                            
*                                                                               
         CLI   IFLDH+5,2           FIELD MUST BE 2 OR 3 CHS                     
         BL    CLIVE               INCLUDING * FOR OFFICE REQUEST               
         CLI   IFLDH+5,3                                                        
         BH    CLIVE                                                            
*                                                                               
         CLI   IFLD,C'*'           SKIP IF NOT OFFICE REQUEST                   
         BNE   CLIVOFCN                                                         
*                                                                               
CLIVOFC2 DS    0H                                                               
*                                                                               
         CLC   RNUM,=C'48'         PUB LISTING ?                                
         BNE   CLIVOFC3            NO                                           
*                                                                               
         CLI   RO1,C'1'            LIST TYPE 1 - 4 ?                            
         BL    CLIVE               NO - ERROR                                   
*                                                                               
         CLI   RO1,C'4'                                                         
         BH    CLIVE                                                            
*                                                                               
CLIVOFC3 DS    0H                  VALIDATE OFFICE AND SECURITY                 
*                                                                               
         XC    RCLI,RCLI           INIT REQUEST CLIENT                          
*                                                                               
         MVI   RCLI,C'*'                                                        
*                                                                               
         XC    WORK,WORK          WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(R3)                                                    
         MVC   OFCAGY,RAGY                                                      
         MVC   OFCOFC2,IFLD+1     EXTERNAL OFFICE CODE                          
*                                                                               
         CLC   =C'*-',IFLD        IF ALL BUT REQUEST                            
         BNE   *+14                                                             
         MVC   OFCOFC2,IFLD+2                                                   
         MVI   RCLI+1,C'-'                                                      
*                                                                               
         MVC   OFCLMT,6(R3)                                                     
         MVC   OFCSECD,ASECBLK    A("SECRET BLOCK")                             
*                                                                               
         DROP  R1                                                               
*                                                                               
*        VALIDATE OFFICE CODE VIA OFFICER AND GET OFFICE NAME                   
*                                                                               
         GOTO1 OFFICER,DMCB,(C'2',WORK),(1,ACOMFACS),                  X        
               (C'L',NAME)                                                      
*                                                                               
         CLI   0(R1),0             DID IT PASS SECURITY?                        
         BE    *+14                YES                                          
         MVC   FERN,=AL2(CAERROR)  NO                                           
         B     CLIVXX                                                           
*                                                                               
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         LA    RF,RCLI+1           POINT TO OFFICE CODE SAVEAREA                
         CLC   =C'*-',IFLD         IF ALL BUT OFFICE                            
         BNE   *+8                                                              
         LA    RF,RCLI+2                                                        
*                                                                               
         MVC   0(1,RF),OFCOFC      SAVE INTERNAL OFFICE CODE                    
*                                                                               
         OI    FIND,X'08'          OFFICE INPUT                                 
         MVI   CLIPROF+9,C'0'      FOR BILLING - TO REQUIRE EST=ALL,NNN         
*                                                                               
         CLC   IFLD(2),=C'*-'      DONE IF ALL BUT OFFICE REQUEST               
         BE    CLIVOKX                                                          
*                                                                               
         B     CLIV10                                                           
*                                                                               
*        SINGLE CLIENT REQUEST                                                  
*                                                                               
CLIVOFCN MVC   KRT1+4(3),RCLI      CLIENT TO KEY1                               
         MVC   KRT2+4(3),RCLI      CLIENT TO KEY2                               
*                                                                               
         MVI   KRT1+3,X'02'        READ CLIENT RECORD                           
*                                                                               
         GOTO1 AREAD,PLIST,C'PRT1'                                              
*                                                                               
         LA    R5,PRTREC                                                        
         USING PCLTRECD,R5                                                      
*                                                                               
         CLC   FERN,=AL2(FE)                                                    
         BL    CLIVOK              DISK ERROR                                   
         BH    CLIV2A                                                           
*                                                                               
         MVC   FERN,=AL2(CLINOF)                                                
         B     CLIVOK                                                           
*                                                                               
CLIV2A   OC    6(2,R3),6(R3)       CHK LIMIT ACCESS                             
         BZ    CLIV2G              NO RESTRICTIONS                              
*                                                                               
         BAS   RE,CLIV100          CHECK SECURITY                               
*                                                                               
         CLC   FERN,=AL2(CAERROR)  EXIT ON ACCESS ERROR                         
BUG3     BE    CLIVXX              WAS CLIVOK                                   
         B     CLIV2G              ACCESS OK                                    
*                                                                               
CLIV2G   DS    0H                                                               
         CLC   RNUM(2),=C'AC'      ADV CONTRACT                                 
         BE    CLIV2I                                                           
         CLC   RNUM(2),=C'AR'      AOR CON RPT                                  
         BE    CLIV2I                                                           
         CLC   RNUM(2),=C'AU'      ADV UTILIZATION                              
         BNE   CLIV2X                                                           
*                                                                               
CLIV2I   LA    RE,PCLTREC+33                                                    
*                                                                               
         USING PCLTADVE,RE                                                      
*                                                                               
CLIV2K   CLI   0(RE),X'15'                                                      
         BE    CLCV8                                                            
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BE    CLIVE                                                            
         B     CLIV2K                                                           
*                                                                               
CLCV8    CLC   RNUM,=C'AR'                                                      
         BNE   CLCV12                                                           
         TM    PCLTACON,X'20'      MUST BE SET FOR NO BUYING AGY CONS           
         BNO   CLIVE                                                            
         CLC   RAGY,PCLTAOR        ALSO I CAN'T BE THE AOR                      
         BE    CLIVE                                                            
         B     CLIV2X                                                           
*                                                                               
CLCV12   CLC   PCLTKCLT,PCLTADV    MUST BE ADVERTISER CLIENT                    
         BNE   CLIVE                                                            
*                                                                               
         DROP  RE                                                               
*                                                                               
CLIV2X   DS    0H                                                               
         CLC   RNUM,=C'12'         NO SLAVE CLTS FOR CONTRACTS                  
         BE    CLIV3                                                            
         CLC   RNUM,=C'14'                                                      
         BNE   CLIV4                                                            
CLIV3    CLI   PCLTPROF+5,C'2'     SLAVE                                        
         BNE   CLIV8                                                            
         MVC   FERN,=AL2(NOSLAV)                                                
         B     CLIVX                                                            
*                                                                               
CLIV4    CLC   RNUM,=C'06'                                                      
         BNE   CLIV8                                                            
         CLI   PCLTPROF+13,C'N'                                                 
         BNE   CLIV8                                                            
         MVC   FERN,=AL2(NUMINV)   NO DETAIL BILLING FOR THIS CLT               
         B     CLIVX                                                            
CLIV8    OI    FIND,X'04'                                                       
         MVC   NAME(20),PCLTNAME                                                
         MVC   CLIPROF,PCLTPROF                                                 
*                                                                               
* SEARCH FOR PCLTDRO (X'30') ELEMENT AND SAVE DRD OVERRIDE                      
* CLIENT IN CLIPROF+20(3)                                                       
*                                                                               
         XC    CLIPROF+20(3),CLIPROF+20                                         
*                                                                               
         LA    RE,PCLTREC+33                                                    
CLIV8C   CLI   0(RE),X'30'                                                      
         BE    CLIV8F                                                           
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BE    CLIV8X                                                           
         B     CLIV8C                                                           
*                                                                               
CLIV8F   MVC   CLIPROF+20(3),2(RE)                                              
         OC    CLIPROF+20(3),SPACES                                             
*                                                                               
* OVERLAY LAST 3 BYTES WITH OFFICE CODE                                         
*                                                                               
CLIV8X   MVC   CLIPROF+27(3),PCLTOFF                                            
*                                                                               
         LA    RE,PCLTREC+33                                                    
CLIV9    CLI   0(RE),X'52'      LOOK FOR PO BILLING ELEMENT                     
         BE    CLIV9F                                                           
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BE    CLIV9X                                                           
         B     CLIV9                                                            
*                                                                               
CLIV9F   MVC   CLTPOOPT,2(RE)      PCLTPOF1                                     
         MVC   CLTPOTYP,3(RE)      PCLTPOLV                                     
         OC    4(3,RE),4(RE)       EFFECTIVE DATE PRESENT                       
         BZ    CLIV9H                                                           
         GOTO1 DATCON,PLIST,(3,4(RE)),(0,CLTPOEFF)                              
CLIV9H   TM    CLTPOOPT,X'40'      P_POLOVQ  (X'40')                            
         BZ    *+10                                                             
         MVC   CLTPOANN,7(RE)      PCLTPONM                                     
*                                                                               
CLIV9X   DS    0H                                                               
CLIV10   FOUT  (R6),NAME                                                        
*                                                                               
         CLI   ROUTSUB,0           ONE FIELD INPUT                              
         BE    CLIVOKX                                                          
         DROP  R5                                                               
*                                                                               
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         MVI   ROUTSUB,2           SCAN FOR A SECOND CLIENT                     
         GOTO1 AINITV                                                           
         CLI   IFLDH+5,0           SKIP IF NONE                                 
         BE    CLIVX                                                            
         TM    FIND,X'04'                                                       
         BZ    CLIVE               FIRST CLT CAN'T BE ALL OR *NN                
         CLC   IFLD(3),=C'ALL'     SECOND CLT CAN'T BE ALL                      
         BE    CLIVE                                                            
         CLI   IFLDH+5,2                                                        
         BL    CLIVE                                                            
         CLI   IFLDH+5,3                                                        
         BH    CLIVE                                                            
         MVC   KRT1+4(3),IFLD      READ SECOND CLT                              
         GOTO1 AREAD,PLIST,C'PRT1'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    CLIVOKX             DISK ERROR                                   
         BH    *+14                                                             
         MVC   FERN,=AL2(CLINOF)   NOT FOUND                                    
         B     CLIVOKX                                                          
*                                                                               
* CONTRACTS - SECOND CLT MUST BE SLAVE FOR FIRST CLIENT                         
*                                                                               
         CLC   RNUM,=C'12'                                                      
         BNE   CLIV12                                                           
*                                                                               
         LA    R5,PRTREC                                                        
         USING PCLTRECD,R5                                                      
         CLI   PCLTPROF+5,C'2'     MUST BE SLAVE                                
         BNE   CLIVE                                                            
         CLC   PCLTPROF+6(3),RCLI  MUST BE FOR REQUESTED MASTER                 
         BNE   CLIVE                                                            
         MVI   FIND,X'21'          SET FOR MASTER/SLAVE                         
*                                                                               
         MVC   KRT1+4(3),RCLI      NEEDED FOR CONTRACT READ                     
         MVC   KRT2+4(3),IFLD      SLAVE-FOR PRODUCT READ                       
         MVC   RDIV(3),IFLD                                                     
         B     CLIVX                                                            
*                                                                               
CLIV12   MVI   FIND,X'11'          CLI=XXX-XXX                                  
         MVC   KRT1+4(3),RCLI      RESET OT FIRST CLT                           
         MVC   RPUB+1(3),=C'RD='                                                
         MVC   RPUB+4(3),IFLD                                                   
         B     CLIVX                                                            
*                                                                               
CACCERR  MVC   FERN,=AL2(CAERROR)                                               
         B     CLIVOK                                                           
*                                                                               
CLIVE    MVC   FERN,=AL2(FLDINV)                                                
*                                                                               
CLIVOK   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
*                                                                               
CLIVOKX  FOUT  (R6),NAME                                                        
*                                                                               
CLIVX    OC    CLISAVE,FIND                                                     
*                                                                               
CLIVXX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
*********************************************************************           
*                                                                  **           
*  NEW CHK LIMIT ACCESS                                            **           
*                                                                  **           
*********************************************************************           
         DS    0D                                                               
CLIV100  NTR1                   * TEST OFFICE LIST SECURITY *                   
*                                                                               
         L     RF,ASAVE                                                         
*                                                                               
         OC    4(2,RF),4(RF)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RF),6(RF)       TEST ANY LIMIT ACCESS                        
         BZ    CLIV100X                                                         
*                                                                               
         LA    R6,PRTREC          POINT TO CLIENT REC                           
         USING PCLTREC,R6                                                       
*                                                                               
         MVC   BYTE,PCLTOFF       SAVE PCLTOFF FOR LIMIT ACCESS TESTING         
*                                                                               
         CLI   TRAFID,C'T'        TRAFFIC ID ?                                  
         BNE   VCLT20             NO                                            
*                                 SEE IF TRAFFIC OFFICE EXISTS                  
         LA    R6,PRTREC          POINT TO CLIENT REC                           
         MVI   ELCODE,X'50'       CLIENT TRAFFIC OFFICE ELEM CODE               
         BAS   RE,GETEL                                                         
         BNE   VCLT20             NO TRAFFIC OFFICE FOUND                       
         MVC   BYTE,2(R6)         REPLACE PCLTOFF SAVED IN BYTE                 
*                                 WITH CLIENT TRAFFIC OFFICE CODE               
         DROP  R6                                                               
*                                                                               
VCLT20   DS    0H               *****  LIMIT ACCESS TESTING   *****             
         XC    WORK,WORK          WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         L     RF,ASAVE                                                         
         LA    R7,PRTREC          POINT TO CLIENT REC                           
         USING PCLTREC,R7                                                       
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RF)                                                    
         MVC   OFCAGY,RAGY                                                      
         MVC   OFCOFC,BYTE        CLT OR CLT TRAFFIC OFFICE CODE                
*                                                                               
         CLI   IFLD,C'*'           SEE IF OFFICE REQ                            
         BNE   *+10                                                             
         MVC   OFCOFC2,IFLD+1     AND MOVE IN REQ OFFICE                        
*                                                                               
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RF)                                                  
         MVC   OFCSECD,ASECBLK    A("SECRET BLOCK")                             
*                                                                               
         DROP  R1                                                               
*                                                                               
         GOTO1 OFFICER,DMCB,(C'2',WORK),ACOMFACS                                
*                                                                               
         CLI   0(R1),0                                                          
         BE    CLIV100X                                                         
*                                                                               
         MVC   FERN,=AL2(CAERROR)                                               
*                                                                               
CLIV100X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R7                                                               
*                                                                               
         TITLE 'PRREQ03 NEW REQUEST VAL FIELDS PART - 1 - VALGROUP'             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*        CHECKING CLIENT GROUP SECURITY                               *         
*                                                                     *         
*        R3 ==> GROUP ID                                              *         
*                                                                     *         
*        DOESN'T APPEAR TO BE USED                                    *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
VALGROUP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R7,PRTREC          STILL SHOULD POINT TO CLIENT                  
         USING PCLTRECD,R7         ESTABLISH CLIENT HEADER RECORD               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPRECD,R4          CLIENT GROUP PASSIVE POINTER                 
*                                                                               
         MVI   GRPPTYP,GRPPCGQ     RECORD TYPE                                  
         MVC   GRPPAGY,PCLTKAGY       AGENCY                                    
         MVC   GRPPMED,PCLTKMED       MEDIA                                     
         MVC   GRPPVAL(3),PCLTKCLT    CLIENT                                    
         OC    GRPPVAL,=6C' '         SPACE PADDED                              
         MVC   GRPPID(1),7(R3)        GROUP ID                                  
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEYSAVE,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(14),KEYSAVE     CHECK THROUGH ID                             
         BNE   VALGR10                                                          
*                                                                               
         MVC   FULL,=6C' '         (6 TO SAVE USING ANOTHER LITERAL)            
         MVC   FULL(2),8(R3)       GROUP CODE                                   
         OC    FULL,=C'0000'       REPLACE BLANKS WITH X'F0'                    
*                                                                               
         PACK  DUB,FULL                                                         
*                                                                               
         L     R0,DUB+4                                                         
         SRL   R0,4                GET RID OF SIGN NIBBLE                       
         STCM  R0,3,HALF           LEFT-JUSTIFIED, PWOS                         
*                                                                               
         CLC   HALF,GRPPCODE       GROUP CODE MATCH?                            
         BE    VALGR20             YES                                          
*                                                                               
VALGR10  LTR   RE,RE               ACCESS DENIED EXIT (NOT EQUAL)               
         B     VALGXIT                                                          
*                                                                               
VALGR20  CR    RE,RE               ACCESS OK EXIT (EQUAL)                       
*                                                                               
VALGXIT  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R7,RB                                                         
*                                                                               
         TITLE 'PRREQ03 NEW REQUEST VAL FIELDS PART - 1 - CKTRAFID'             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*        CHECKING FOR TRAFFIC ID SIGN-ON                              *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
CKTRAFID NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   TRAFID,0            INIT TRAFFIC SIGN ON INDICATOR               
*                                                                               
         L     R0,AIOCTF                                                        
         LHI   R1,1600                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,AIOCTF                                                        
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         L     RF,ASAVE                                                         
         MVC   CTIKNUM,10(RF)      ID NUMBER                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),(R4)                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
CKTRA10  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
         CLI   0(RE),CTAGYELQ      AGENCY ALPHA ID ELEMENT (X'06')              
         BE    CKTRA20                                                          
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CKTRA10                                                          
*                                                                               
CKTRA20  DS    0H                                                               
         USING CTAGYD,RE                                                        
         CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   CKTRIDER                                                         
         DROP  R4,RE                                                            
*                                                                               
CKTRIDX  DS    0H                                                               
         MVI   TRAFID,C'T'         INDICATE THIS A TRAFFIC SIGN ON              
*                                                                               
         CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKTRIDER LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE CTGENFILE                                                      
*                                                                               
       ++INCLUDE PRREQSAVE                                                      
       ++INCLUDE PRREQTEMP                                                      
*                                                                               
       ++INCLUDE FLDIND                                                         
*                                                                               
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
*                                                                               
PESTRECD DSECT                                                                  
       ++INCLUDE PESTREC                                                        
*                                                                               
       ++INCLUDE PGENGRP                                                        
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE PPSRCHPARM                                                     
*                                                                               
       ++INCLUDE PPDDEQUS                                                       
       ++INCLUDE PRREQFFD                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057PRREQ03   06/20/19'                                      
         END                                                                    
