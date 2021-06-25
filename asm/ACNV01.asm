*          DATA SET ACNV01     AT LEVEL 022 AS OF 08/16/00                      
*PHASE ACNV01A                                                                  
         TITLE 'ACCPAK CONVERSION - ELEMENT ROUTINE'                            
ACNV01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*ELMC*,RA                                                      
         USING ACNVD,R9                                                         
*                                                                               
         MVC   AELMT(12),TABADD   SET TABLE ADDRESSES                           
         L     R2,AOUT                                                          
         USING ACTRECD,R2                                                       
         SR    R0,R0                                                            
         SR    R4,R4                                                            
         LA    R2,ACTRFST                                                       
ACELM    CLI   0(R2),0             TEST EOR                                     
         BE    XIT                                                              
         SR    R3,R3                                                            
         IC    R3,0(R2)            ELEMENT CODE                                 
         SLL   R3,2                X 4                                          
         A     R3,AELMT            ADD START OF TABLE                           
         CLC   0(1,R3),0(R2)       TEST ENTRY FOR ELEMENT                       
         BNE   ACELM3                                                           
         ICM   R4,7,1(R3)          RF=DISPLACEMENT TO TABLE                     
         BAS   RE,PRCL             PROCESS ELEMENT DATA                         
*                                                                               
ACELM3   IC    R0,1(R2)                                                         
         LTR   R0,R0                                                            
         BZ    XIT                                                              
         AR    R2,R0                                                            
         B     ACELM                                                            
*                                                                               
XIT      XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ELEMENT DATA                                                *         
***********************************************************************         
                                                                                
         USING EDD,R4                                                           
PRCL     NTR1  ,                                                                
         SR    R0,R0                                                            
         ICM   R0,1,EDNUM          NUMBER OF ENTRIES                            
         BZ    PRCL11                                                           
         LA    R5,EDMIN            R5 = MINI ELEMENNT                           
*                                                                               
PRCL3    CLC   1(1,R2),EDDSP-EDMIN(R5) TEST ELEMENT LENGTH                      
         BNH   PRCL9                                                            
         CLI   MODE,CHNGCMP        TEST CHANGE COMPANY CODE                     
         BNE   PRCL5                                                            
         TM    EDSTA-EDMIN(R5),EDSCM  ONLY CHANGE COMPANY CODES                 
         BNO   PRCL9                                                            
*                                                                               
PRCL5    SR    R3,R3                                                            
         ICM   R3,1,EDDSP-EDMIN(R5)                                             
         AR    R3,R2               R3 = ELEMENT DATA                            
         BAS   RE,HOOKER           LINK TO HOOK                                 
         TM    HKSTA,HKSENO        SHIP CHANGE ROUTINE                          
         BO    PRCL9                                                            
*                                                                               
PRCL7    SR    RE,RE                                                            
         ICM   RE,1,EDSTA-EDMIN(R5)  ROUTINE NUMBER(STANDARD ROUTINES)          
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    RE,PRCLTAB(RE)                                                   
         ICM   RF,15,0(RE)                                                      
         BASR  RE,RF               REPLACE DATA                                 
*                                                                               
PRCL9    LA    R5,L'EDMIN(R5)      PROCESS NEXT MINI                            
         BCT   R0,PRCL3                                                         
*                                                                               
PRCL11   SR    R3,R3               TELL HOOK IT'S SPECIAL ROUTINE               
         SR    R5,R5                                                            
         BAS   RE,HOOKER           LINK TO HOOK                                 
         TM    HKSTA,HKSENO        SHIP CHANGE ROUTINE                          
         BO    XIT                                                              
         SR    RE,RE                                                            
         ICM   RE,1,EDSPL          NUMBER OF SPECIAL ROUTINE                    
         BZ    XIT                                                              
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    RE,SPCLTAB(RE)                                                   
         ICM   RF,15,0(RE)                                                      
         BASR  RE,RF                                                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* STANDARD ROUTINES                                                   *         
***********************************************************************         
                                                                                
         DS    0F                                                               
PRCLTAB  DC    A(ACRPL)            REPLACE ACCOUNT                              
         DC    A(OFRPL)                    OFFICE                               
         DC    A(ANRPL)                    ANALYSIS                             
         DC    A(SJRPL)                    SJ                                   
         DC    A(MCRPL)                    MEDIA CODE                           
         DC    A(WCRPL)                    WORKCODE                             
         DC    A(CCRPL)                    CLIENT CODE                          
         DC    A(MGRPL)                    MEDIA GROUP                          
         DC    A(WGRPL)                    WORKCODE GROUP                       
         DC    A(BSRPL)                    BILLING SOURCE                       
         DC    A(CPRPL)                    COMPANY CODE                         
         EJECT                                                                  
***********************************************************************         
* CALL THE HOOK                                                       *         
* PASS FOLLOWING TO HOOK ROUTINE                                      *         
*  R2 = A(ELEMENT)                                                    *         
*  R3 = A(DATA WITHIN ELEMENT)  IF R3 = 0 IT'S THE SPECIAL ROUTINE    *         
*  R4 = A(START ELEMENT TABLE FOR THIS ELEMENT)                       *         
*  R5 = A(TABLE ENTRY FOR THIS ITEM) R5 = 0 IF SPECIAL ROUTINE        *         
***********************************************************************         
                                                                                
HOOKER   TM    HKSTA,HKSELM        TEST HOOK NEEDS ELEMENT ROUTINE              
         BZR   RE                                                               
         ST    RE,SVRE                                                          
         MVC   SVMODE,MODE                                                      
         MVI   MODE,CHNGELM                                                     
         GOTO1 ACNVHOOK                                                         
         MVC   MODE,SVMODE                                                      
         L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ACCOUNT REPLACE ROUTINE                                             *         
***********************************************************************         
                                                                                
ACRPL    TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNOR  RE                                                               
         CLI   0(R3),C' '                                                       
         BNHR  RE                                                               
*                                                                               
ACRPL1   NTR1  ,                                                                
         MVC   SRCHARG(L'ACEO),0(R3) OLD ACCOUNT                                
         GOTO1 ASRCH,SRCHEA        SEARCH FOR NEW                               
         BNE   *+10                                                             
         MVC   0(14,R3),ACEN       REPLACE WITH NEW                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* OFFICE REPLACE ROUTINE                                              *         
***********************************************************************         
                                                                                
OFRPL    TM    CNVSW,CNVSOF        TEST OFFICE CONVERSION                       
         BNOR  RE                                                               
         CLI   0(R3),C' '          TEST FOR ANY OFFICE                          
         BNHR  RE                                                               
         CLI   1(R3),C' '          TEST FOR NEW OFFICE                          
         BHR   RE                                                               
*                                                                               
OFRPL1   NTR1  ,                                                                
         MVC   SRCHARG(L'OFEO),0(R3) OFFICE                                     
         GOTO1 ASRCH,SRCHEO        SEARCH FOR NEW                               
         BNE   *+10                                                             
         MVC   0(2,R3),OFEN        REPLACE WITH NEW                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ANALYSIS REPLACE ROUTINE                                            *         
***********************************************************************         
                                                                                
ANRPL    TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNOR  RE                                                               
         CLI   0(R3),C' '                                                       
         BNHR  RE                                                               
*                                                                               
ANRPL1   NTR1  ,                                                                
         MVC   ANALACC,0(R3)       COST ANALYSIS                                
         LA    R3,ANALUL                                                        
         BAS   RE,ACRPL                                                         
         MVC   0(14,R3),ANALACC                                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRODUCTION ACCOUNT                                                  *         
***********************************************************************         
                                                                                
SJRPL    TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNOR  RE                                                               
         CLI   0(R3),C' '                                                       
         BNHR  RE                                                               
*                                                                               
SJRPL1   NTR1  ,                                                                
         MVC   PRDACC,0(R3)        PRODUCTION ACCOUNT                           
         LA    R3,PRDUL                                                         
         BAS   RE,ACRPL                                                         
         BNE   XIT                                                              
         CLC   PRDUL(2),=C'SJ'                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE SJ                                   
         MVC   0(14,R3),PRDACC                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* EXPENSE ACCOUNT                                                     *         
***********************************************************************         
                                                                                
EXRPL    TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNOR  RE                                                               
         CLI   0(R3),C' '                                                       
         BNHR  RE                                                               
*                                                                               
EXRPL1   NTR1  ,                                                                
         MVC   EXPNACC,0(R3)       EXPENSE ACCOUNT                              
         LA    R3,EXPNUL                                                        
         BAS   RE,ACRPL                                                         
         MVC   0(14,R3),EXPNACC                                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MEDIA CODE                                                          *         
***********************************************************************         
                                                                                
MGRPL    DS    0H                                                               
         BR    RE                  NO MEDIA GROUP CONVERSION                    
*                                                                               
MCRPL    TM    CNVSW,CNVSMC        TEST MEDIA CODE CONVERSION                   
         BNOR  RE                                                               
         CLI   0(R3),C' '                                                       
         BNHR  RE                                                               
*                                                                               
MCRPL1   NTR1  ,                                                                
         MVC   SRCHARG(L'MEDO),0(R3)     OLD MEDIA                              
         GOTO1 ASRCH,SRCHMC        SEARCH FOR NEW                               
         BNE   *+10                                                             
         MVC   0(1,R3),MEDN        REPLACE WITH NEW                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* WORKCODE                                                            *         
***********************************************************************         
                                                                                
WGRPL    DS    0H                                                               
         BR    RE                  NO W/CODE GROUP CONVERSION                   
*                                                                               
WCRPL    TM    CNVSW,CNVSWC        TEST W/CODE CONVERSION                       
         BNOR  RE                                                               
         CLI   0(R3),C' '                                                       
         BNHR  RE                                                               
         CLI   RECTYP,ACRTWCO      TEST WORKCODE RECORD                         
         BNE   WCRPL1                                                           
         L     RF,AINP                                                          
         CLC   WCOKUNT-WCORECD(2,RF),=C'SJ'                                     
         BNER  RE                                                               
*                                                                               
WCRPL1   NTR1  ,                                                                
         MVC   SRCHARG(L'WRKO),0(R3) OLD W/CODE                                 
         GOTO1 ASRCH,SRCHWC        SEARCH FOR NEW                               
         BNE   *+10                                                             
         MVC   0(2,R3),WRKN        REPLACE WITH NEW                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CLIENT CODE                                                         *         
***********************************************************************         
                                                                                
CCRPL    TM    CNVSW,CNVSCC        TEST CLIENT CODE CONVERSION                  
         BNOR  RE                                                               
         CLI   0(R3),C' '                                                       
         BNHR  RE                                                               
*                                                                               
CCRPL1   NTR1  ,                                                                
         MVC   SRCHARG(L'CLIO),0(R3) OLD ACCOUNT                                
         GOTO1 ASRCH,SRCHCC        SEARCH FOR NEW                               
         BNE   *+10                                                             
         MVC   0(3,R3),CLIN        REPLACE WITH NEW                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BILLING SOURCE                                                      *         
***********************************************************************         
                                                                                
BSRPL    DC    H'0'                BILLING SOURCE IS NOT IN ELEMENT             
         EJECT                                                                  
***********************************************************************         
* COMPANY CODE                                                        *         
***********************************************************************         
                                                                                
*                                                                               
CPRPL    CLI   XCOMPANY,0          TEST CHANGING COMPANY CODE                   
         BER   RE                                                               
         CLC   0(1,R3),XCOMPANY                                                 
         BNER  RE                                                               
         MVC   0(1,R3),COMPANY     REPLACE IT                                   
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SPECIAL  ROUTINES                                                   *         
***********************************************************************         
                                                                                
         DS    0F                                                               
SPCLTAB  DC    A(CPYR)         1   COMPANY                                      
         DC    A(SPAR)         2   SPECIAL POSTING                              
         DC    A(ICPR)         3   INTERCOMPANY                                 
         DC    A(OTHR)         4   OTHERS                                       
         DC    A(SPDR)         5   SUBSIDIARY POSTING                           
         DC    A(CPJR)         6   CLIENT/PRODUCT/JOB                           
         DC    A(APER)         7   ANALYSIS POINTER                             
         DC    A(RALR)         8   RECEIVABLE ALLOCATION                        
         DC    A(ASKR)         9   ACCOUNT SYSTEM KEY                           
         DC    A(SFSR)        10   SCREEN FIELD                                 
         DC    A(GLRR)        11   GENERAL LEDGER OFFICE RULES                  
         DC    A(ORDR)        12   PRODUCTION ORDER                             
         DC    A(PPRR)        13   PRODUCTIION PROFILE                          
         DC    A(OPDR)        14   PRODUCTIION OPTIONS                          
         DC    A(OCNR)        15   OFFICE CHECK                                 
         DC    A(MTPR)        16   MEDIA TRANSFER PROFILE                       
         DC    A(TRNR)        17   TRANSACTION NARRATIVE                        
         DC    A(PTRR)        18   PASSIVE POINTER ELEMENTS                     
         DC    A(STUR)        19   STUDIO ELEMENT                               
         DC    A(LOCR)        20   LOCATTION ELEMENT                            
         DC    A(TIMR)        21   TIME ELEMENT                                 
         DC    A(FFTR)        22   FREE FORM TEXT                               
         DC    A(RFLR)        23   R.L. FILTER                                  
         EJECT                                                                  
***********************************************************************         
* COMPANY                                                             *         
***********************************************************************         
                                                                                
         USING CPYELD,R2                                                        
CPYR     NTR1  ,                                                                
         CLI   ACTNSW,ACTNCON      TEST ACTION IS CONVERSION                    
         BNE   XIT                                                              
*&&US                                                                           
         OI    CPYSTAT4,CPYSOFF2   SET 2 CHARACTER OFFICE                       
*&&                                                                             
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SPECIAL POSTING A/C                                                 *         
***********************************************************************         
                                                                                
         USING SPAELD,R2                                                        
SPAR     TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNOR  RE                                                               
*                                                                               
SPAR1    NTR1  ,                                                                
         LA    R3,SPAAANAL                                                      
         CLI   SPATYPE,SPATANAL    ANALYSIS ACCOUNT                             
         BNE   *+12                                                             
         BAS   RE,ANRPL                                                         
         B     XIT                                                              
*                                                                               
         BAS   RE,ACRPL            'OTHER' ACCOUNT                              
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INTERCOMPANY                                                        *         
***********************************************************************         
                                                                                
         USING ICPELD,R2                                                        
ICPR     NTR1  ,                                                                
         DC    H'0'         I NEED TO LOOK AT ONE, BEFORE I CODE                
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* OTHERS                                                              *         
***********************************************************************         
                                                                                
         USING OTHELD,R2                                                        
OTHR     NTR1  ,                                                                
         TM    LGRTYP,LTPPAY+LTXPAY   TEST PRODUCTION PAYABLE                   
         BZ    OTHR3                                                            
         CLI   OTHNUM+3,C' '                                                    
         BH    XIT                                                              
         LA    R3,OTHNUM+6                                                      
         BAS   RE,MCRPL            REPLACE PRODUCTION MEDIA CODE                
         B     XIT                                                              
*                                                                               
OTHR3    CLI   OTHPROF,C'J'        TEST JOB                                     
         BNE   XIT                                                              
         LA    R3,OTHNUM+3                                                      
         BAS   RE,MCRPL            REPLACE PRODUCTION MEDIA CODE                
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SUBSIDIARY POSTING                                                  *         
***********************************************************************         
                                                                                
         USING SPDELD,R2                                                        
SPDR     TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNOR  RE                                                               
*                                                                               
SPDR1    NTR1  ,                                                                
N        USING SPDELD,R5                                                        
         LA    R5,ELEMENT                                                       
         LA    R3,SPDACCS-SPDELD(R5)                                            
         MVC   0(15,R3),SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,SPDLN                                                         
         SH    R1,=H'3'                                                         
         CH    R1,=H'13'                                                        
         BNH   *+8                                                              
         LA    R1,13                                                            
         LA    R4,SPDACCS                                                       
         CLI   XCOMPANY,0          TEST CHANGING COMPANY CODE                   
         BE    SPDR2                                                            
         CLC   0(1,R4),XCOMPANY                                                 
         BNE   SPDR2                                                            
         MVC   0(1,R4),COMPANY     REPLACE IT                                   
         B     SPDR3                                                            
*                                                                               
SPDR2    CLC   0(1,R4),COMPANY                                                  
         BNE   SPDR4                                                            
*                                                                               
SPDR3    MVC   0(1,R3),0(R4)       KEEP COMPANY CODE IN ACCOUNT                 
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         BCTR  R1,0                                                             
SPDR4    EX    R1,*+4                                                           
         MVC   0(0,R3),0(R4)                                                    
         BAS   RE,ACRPL            REPLACE ACCOUNT                              
         BNE   XIT                 TEST ANY REPLACEMENT                         
         LA    R5,ELEMENT                                                       
         MVI   N.SPDEL,SPDELQ      SET ELEMENT CODE                             
         LA    R1,N.SPDACCS+13                                                  
         LA    RF,14                                                            
         CLI   0(R1),C' '          FIND ACCOUNT LENGTH                          
         BH    *+12                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
         DC    H'0'                                                             
         AH    RF,=H'2'                                                         
         STC   RF,N.SPDLN                                                       
         BAS   RE,FITL             FIT ELEMENT INTO THE RECORD                  
         B     XIT                                                              
         DROP  R2,N                                                             
         EJECT                                                                  
***********************************************************************         
* CLI/PROD/JOB / EXPENSE ACCOUNT / OTHER ACCOUNT                      *         
***********************************************************************         
                                                                                
         USING CPJELD,R2                                                        
CPJR     TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNOR  RE                                                               
*                                                                               
CPJR1    NTR1  ,                                                                
         LA    R3,CPJEXP                                                        
         CLI   CPJDATA,CPJTEXP     TEST EXPENSE LEDGER                          
         BNE   *+12                                                             
         BAS   RE,EXRPL                                                         
         B     XIT                                                              
*                                                                               
         LA    R3,CPJOUNT                                                       
         CLI   CPJDATA,CPJTOTH     TEST OTHER LEDGER                            
         BNE   *+8                                                              
         BAS   RE,ACRPL            REPLACE OTHER ACCOUNT                        
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ANALYSIS POINTER                                                    *         
***********************************************************************         
                                                                                
         USING APEELD,R2                                                        
APER     TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNOR  RE                                                               
*                                                                               
APER1    NTR1  ,                                                                
         LA    R5,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   APELN-APEELD(R5),APELN1Q SET ELEMENT LENGTH                      
         MVC   APENUM-APEELD(L'APENUM,R5),APENUM SET NUMBER OF ITEMS            
         LA    R4,APENTRY                                                       
         USING APENTRY,R4                                                       
         LA    R6,APENTRY-APEELD(R5)                                            
         SR    R0,R0                                                            
         IC    R0,APENUM           NUMBER OF SUB ELEMENTS                       
*                                                                               
APER3    SR    R1,R1                                                            
         IC    R1,APENLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R6),APENTRY                                                  
         OC    APENACT-APENTRY(L'APENACT,R6),SPACES                             
         LA    R3,APENACT-APENTRY(R6)                                           
         BAS   RE,ACRPL            REPLACE ACCOUNT                              
         BNE   *+8                 TEST ANY REPLACEMENT                         
         MVI   APEEL-APEELD(R5),APEELQ  SET ELEMENT CODE                        
         LA    R1,APENACT-APENTRY+(L'APENACT-1)(R6)                             
         LA    RF,L'APENACT                                                     
         CLI   0(R1),C' '          FIND ACCOUNT LENGTH                          
         BNE   *+14                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
         B     XIT                                                              
*                                                                               
         LA    RF,2(RF)                                                         
         STC   RF,APENLEN-APENTRY(R6)                                           
         SR    R1,R1                                                            
         IC    R1,APELN-APEELD(R5)                                              
         AR    R1,RF                                                            
         STC   R1,APELN-APEELD(R5)                                              
*                                                                               
         LA    R6,0(RF,R6)         R6=NEXT 'NEW' SUB ELEMENT                    
         IC    RF,APENLEN                                                       
         LA    R4,0(RF,R4)         R4=NEXT 'OLD' SUB ELEMENT                    
         BCT   R0,APER3                                                         
         CLI   APEEL-APEELD(R5),APEELQ  TEST ANY REPLACEMENTS                   
         BNE   *+8                                                              
         BAS   RE,FITL             FIT ELEMENT INTO THE RECORD                  
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* RECEIVABLE ALLOCATION                                               *         
***********************************************************************         
                                                                                
         USING RALELD,R2                                                        
RALR     TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNOR  RE                                                               
*                                                                               
RALR1    NTR1  ,                                                                
         LA    R3,RALWULA          WRITE-OFF ACCOUNT                            
         CLI   RALTYPE,RALTWOF                                                  
         BE    RALR3                                                            
         LA    R3,RALTULA          TRANSFER TO/FROM                             
         CLI   RALTYPE,RALTTTO                                                  
         BE    RALR3                                                            
         CLI   RALTYPE,RALTTFR                                                  
         BNE   XIT                                                              
RALR3    BAS   RE,ACRPL                                                         
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ACCOUNT SCREEN FIELD                                                *         
***********************************************************************         
                                                                                
         USING ASKELD,R2                                                        
ASKR     CLI   ACTNSW,ACTNCHG      TEST ACTION IS CHANGE                        
         BNER  RE                                                               
         TM    CNVSW,CNVSAC        TEST ACCOUNT CHANGES                         
         BNOR  RE                                                               
*                                                                               
ASKR1    NTR1  ,                                                                
         LA    R6,ASKKEY                                                        
         USING TRNRECD,R6                                                       
         LA    R3,TRNKUNT                                                       
         BAS   RE,ACRPL            REPLACE ACCOUNT                              
         LA    R3,TRNKCUNT                                                      
         BAS   RE,ACRPL            REPLACE CONTRA                               
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* SCREEN FIELD                                                        *         
***********************************************************************         
*                                                                               
         USING SFSELD,R2                                                        
SFSR     NTR1  ,                                                                
         L     R3,AOUT             R2=BATCH RECORD                              
         USING TBARECD,R3                                                       
         L     R4,ABATT            R4=BATCH SCREEN FIELD TABLE                  
         USING BATTABD,R4                                                       
         XC    ELEMENT,ELEMENT     CLEAR FOR NEW ELEMENT                        
         LA    R5,ELEMENT                                                       
         MVI   SFSLN-SFSELD(R5),SFSLN1Q   SET MINIMUM LENGTH                    
         MVC   SFSFLDN-SFSELD(L'SFSFLDN,R5),SFSFLDN                             
*                                                                               
SFSR3    CLC   TBAKBTYP,BATTYP     TEST TYPE                                    
         BNE   *+14                                                             
         CLC   SFSFLDN,BATFLD      TEST FIELD NUMBER                            
         BE    SFSR5                                                            
         LA    R4,BATTLNQ(R4)                                                   
         CLI   0(R4),EOT           TEST EOT                                     
         BNE   SFSR3                                                            
         B     XIT                                                              
*                                                                               
SFSR5    CLI   BATCIND,BATOFF      TEST OFFICE FIELD                            
         BNE   SFSR7                                                            
         CLI   SFSLN,SFSLN1Q+1     TEST LENGTH                                  
         BH    XIT                                                              
         MVC   SFSFIELD-SFSELD(1,R5),SFSFIELD                                   
         LA    R3,SFSFIELD-SFSELD(R5)                                           
         TM    CNVSW,CNVSOF        TEST OFFICE CONVERSION                       
         BNO   XIT                                                              
         BAS   RE,OFRPL            REPLACE OFFICE                               
         BNE   XIT                                                              
         MVI   SFSLN-SFSELD(R5),SFSLN1Q+2 SET NEW LENGTH                        
         MVI   SFSEL-SFSELD(R5),SFSELQ    SET ELEMENT CODE                      
         BAS   RE,FITL             REPLACE ELEMENT                              
         B     XIT                                                              
*                                                                               
SFSR7    MVC   WORK,SPACES                                                      
         SR    R1,R1                                                            
         IC    R1,SFSLN                                                         
         SH    R1,=Y(SFSFIELD-SFSELD+1) R1=ACCOUNT LENGTH                       
         LA    RF,SFSFIELD                                                      
         CLI   SFSFIELD,C'*'       *ULACCOUNT                                   
         BNE   SFSR9                                                            
         LA    RF,1(RF)                                                         
         BCTR  R1,0                                                             
         B     SFSR11                                                           
*                                                                               
SFSR9    CLI   BATUL,C' '          ULACCOUNT                                    
         BNE   SFSR13                                                           
SFSR11   CH    R1,=H'13'           TEST MAX U/L ACCOUNT LENGTH                  
         BH    XIT                                                              
         LTR   R1,R1                                                            
         BNP   XIT                                                              
         EX    R1,*+4                                                           
         MVC   WORK(0),0(RF)                                                    
         B     SFSR15                                                           
*                                                                               
SFSR13   MVC   WORK(2),BATUL       DEFAULT UL                                   
         CH    R1,=H'11'           TEST MAX ACCOUNT LENGTH                      
         BH    XIT                                                              
         EX    R1,*+4                                                           
         MVC   WORK+2(0),SFSFIELD                                               
*                                                                               
SFSR15   LA    R3,WORK             R3=OLD ACCOUNT                               
         TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNO   XIT                                                              
         BAS   RE,ACRPL            GET NEW ACCOUNT                              
         BNE   XIT                                                              
         LA    R1,WORK+L'ACEN-1                                                 
         LA    RF,L'ACEN                                                        
         CLI   0(R1),C' '          FIND ACCOUNT LENGTH                          
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
         DC    H'0'                                                             
         AH    RF,=Y(SFSFIELD-SFSELD+1) R1=ELEMENT LENGTH                       
         CLI   SFSFIELD,C'*'       *ULACCOUNT                                   
         BNE   SFSR17                                                           
         LA    RF,1(RF)            FIX LENGTH                                   
         MVI   SFSFIELD-SFSELD(R5),C'*'                                         
         MVC   SFSFIELD-SFSELD+1(L'ACEN,R5),ACEN    NEW *ULACCOUNT              
         B     SFSR21                                                           
*                                                                               
SFSR17   CLI   BATUL,C' '          ULACCOUNT                                    
****     BE    SFSR19                                                           
         BNE   SFSR19                                                           
         MVC   SFSFIELD-SFSELD(L'ACEN,R5),ACEN      NEW ULACCOUNT               
         B     SFSR21                                                           
*                                                                               
SFSR19   SH    RF,=H'2'            ACCOUNT                                      
         MVC   SFSFIELD-SFSELD(L'ACEN-2,R5),ACEN+2  NEW ACCOUNT                 
*                                                                               
SFSR21   STC   RF,SFSLN-SFSELD(R5)           SET NEW LENGTH                     
         MVI   SFSEL-SFSELD(R5),SFSELQ       SET ELEMENT CODE                   
         BAS   RE,FITL                       REPLACE ELEMENT                    
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* GENERAL LEDGER OFFICE RULES                                         *         
***********************************************************************         
                                                                                
GLRR     TM    CNVSW,CNVSOF        TEST OFFICE CONVERSION                       
         BNOR  RE                                                               
                                                                                
         USING GLRELD,R2                                                        
GLRR1    NTR1  ,                                                                
         ZIC   R3,GLRPRS                                                        
         MH    R3,=H'2'                                                         
         LA    R4,GLROFFP                                                       
GLRR5    MVC   SRCHARG(2),0(R4)                                                 
         GOTO1 ASRCH,SRCHEO                                                     
         BNE   *+10                                                             
         MVC   0(2,R4),OFEN                                                     
         LA    R4,2(R4)                                                         
         BCT   R3,GLRR5                                                         
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOCATION ELEMENT                                                    *         
***********************************************************************         
                                                                                
         USING LOCELD,R2                                                        
LOCR     NTR1  ,                                                                
         TM    CNVSW,CNVSOF        TEST OFFICE CONVERSION                       
         BNO   LOCR3                                                            
         MVC   SRCHARG(2),LOCOFF                                                
         GOTO1 ASRCH,SRCHEO                                                     
         BNE   *+10                                                             
         MVC   LOCOFF,OFEN                                                      
         B     XIT                                                              
*                                                                               
LOCR3    TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNO   XIT                                                              
         CLI   RECTYP,ACRTPER      TEST PERSON RECORD                           
         BNE   XIT                                                              
         MVC   PERIN,SPACE                                                      
         MVC   PEROUT,SPACE                                                     
         MVC   PERIOFF(L'LOCOFF),LOCOFF    OFFICE                               
         MVC   PERIDPT(L'LOCDEPT),LOCDEPT  DEPARTMENT                           
         MVC   PERISUB(L'LOCSUB),LOCSUB    SUB-DEPARTMENT                       
         L     R3,AINP                                                          
         USING PERRECD,R3                                                       
         MVC   PERIPER(L'PERKCODE),PERKCODE PERSON                              
*                                                                               
         LA    RE,PEREL                                                         
         ST    RE,PARCTAB          A(PARSE CONTROL TABLE)                       
         LA    RE,PERIN                                                         
         ST    RE,PARINP           A(INPUT PERSON CODE)                         
         LA    RE,PEROUT                                                        
         ST    RE,PAROUT           A(OUTPUT PERSON CODE)                        
         GOTO1 APAR                PARSE                                        
         CLC   PEROUT,SPACE        NO ENTRY                                     
         BE    XIT                                                              
*                                                                               
         MVC   LOCOFF,PEROOFF      OFFICE                                       
         MVC   LOCDEPT,PERODPT     DEPARTMENT                                   
         MVC   LOCSUB,PEROSUB      SUB-DEPARTMENT                               
         L     R3,AOUT                                                          
         MVC   PERKCODE,PEROPER    PERSON                                       
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* TIME ELEMENT                                                        *         
***********************************************************************         
                                                                                
         USING TIMELD,R2                                                        
TIMR     NTR1  ,                                                                
         CLI   TIMETYP,TIMEINP     TEST INPUT ELEMENT                           
         BNE   TIMR3                                                            
         LA    R3,TIMACC           ACCOUNT(SJ/1N)                               
         BAS   RE,ACRPL                                                         
         LA    R3,TIMTSK           WORKCODE                                     
         BAS   RE,WCRPL                                                         
         LA    R3,TIMOFF           OFFICE                                       
         BAS   RE,OFRPL                                                         
         LA    R3,TIMINC           INCOME ACCOUNT                               
         BAS   RE,ACRPL                                                         
         B     XIT                                                              
*                                                                               
TIMR3    CLI   TIMETYP,TIMETAX     TEST TAX ELEMENT                             
         BNE   TIMR5                                                            
         LA    R3,TIMTACC          TAX CREDIT ACCOUNT                           
         BAS   RE,ACRPL                                                         
         B     XIT                 TEST OFFICE CONVERSION                       
*                                                                               
TIMR5    CLI   TIMETYP,TIMEXREF    TEMPO XREF ELEMENT                           
         BNE   XIT                                                              
         CLI   RECTYP,ACRTTPOX     TEMPO XREF RECORD                            
         BNE   XIT                                                              
         MVC   PERIN,SPACE                                                      
         MVC   PEROUT,SPACE                                                     
         MVC   PERIOFF(L'TIMXOFFC),TIMXOFFC  OFFICE                             
         MVC   PERIDPT(L'TIMXDEPT),TIMXDEPT  DEPARTMENT                         
         MVC   PERISUB(L'TIMXSDPT),TIMXSDPT  SUB-DEPARTMENT                     
         L     R3,AINP                                                          
         USING TSXRECD,R3                                                       
         MVC   PERIPER(L'TSXKPER),TSXKPER    PERSON                             
*                                                                               
         LA    RE,PEREL                                                         
         ST    RE,PARCTAB          A(PARSE CONTROL TABLE)                       
         LA    RE,PERIN                                                         
         ST    RE,PARINP           A(INPUT PERSON CODE)                         
         LA    RE,PEROUT                                                        
         ST    RE,PAROUT           A(OUTPUT PERSON CODE)                        
         GOTO1 APAR                PARSE                                        
         CLC   PEROUT,SPACE        NO ENTRY                                     
         BE    XIT                                                              
*                                                                               
         MVC   TIMXOFFC,PEROOFF    OFFICE                                       
         MVC   TIMXDEPT,PERODPT    DEPARTMENT                                   
         MVC   TIMXSDPT,PEROSUB    SUB-DEPARTMENT                               
         L     R3,AOUT                                                          
         MVC   TSXKPER,PEROPER     PERSON                                       
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* FREE FORM TEXT ELEMENT                                              *         
***********************************************************************         
                                                                                
         USING FFTELD,R2                                                        
FFTR     NTR1  ,                                                                
         LA    R5,FFTTAB                                                        
FFTR1    CLI   0(R5),EOT                                                        
         BE    XIT                                                              
         CLC   FFTTYPE,0(R5)       MATCH TYPE                                   
         BE    FFTR3                                                            
         LA    R5,1(R5)                                                         
         B     FFTR1                                                            
*                                                                               
FFTR3    DC    H'0'                NEED TO CODE THIS ONE                        
         B     XIT                                                              
*                                                                               
FFTTAB   DC    AL1(FFTTBACC)                                                    
         DC    AL1(FFTTATRB)                                                    
         DC    AL1(EOT)                                                         
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ORDER                                                               *         
***********************************************************************         
                                                                                
         USING ORDELD,R2                                                        
ORDR     NTR1  ,                                                                
         CLC   ORDJOB+1(2),=C'SJ'                                               
         BNE   XIT                                                              
         TM    CNVSW,CNVSCC            TEST CLIENT CODE CONVERSION              
         BNO   ORDR3                                                            
         MVC   SRCHARG(L'CLIO),ORDACCA                                          
         GOTO1 ASRCH,SRCHCC            SEARCH FOR CLIENT CODE                   
         BNE   ORDR3                                                            
         MVC   ORDACCA(L'CLIN),CLIN    NEW CLIENT CODE                          
*                                                                               
ORDR3    TM    CNVSW,CNVSMC            TEST MEDIA CODE CONVERSION               
         BNO   XIT                                                              
         CLI   ORDACCA+6,C' '          TEST LOW LEVEL ACCOUNT                   
         BNH   XIT                                                              
         MVC   SRCHARG(L'MEDO),ORDACCA+6                                        
         GOTO1 ASRCH,SRCHMC            SEARCH FOR MEDIA CODE                    
         BNE   XIT                                                              
         MVC   ORDACCA+6(L'MEDN),MEDN  NEW MEDIA CODE                           
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PRODUCTION PROFILE                                                  *         
***********************************************************************         
                                                                                
         USING PPRELD,R2                                                        
PPRR     NTR1  ,                                                                
         LA    R0,PPRUWRKN             UNBILLABLE W/C                           
         LA    R3,PPRUWRK                                                       
         BAS   RE,WCRPL                                                         
         LA    R3,2(R3)                                                         
         BCT   R0,*-8                                                           
*                                                                               
         TM    CNVSW,CNVSCO        TEST CLIENT/OFFICE CONVERSION                
         BNO   XIT                                                              
         CLI   RECTYP,ACRTACTH     TEST ACCOUNT HIGH                            
         BNE   XIT                                                              
*                                                                               
PPRR3    CLC   PPRGAOFF,SPACE      TEST ANY OFFICE NEEDED                       
         BNH   XIT                                                              
         MVC   SRCHARG(L'CLOC),OLDCC                                            
         GOTO1 ASRCH,SRCHCO        SEARCH FOR CLIENT CODE                       
         BNE   XIT                                                              
         MVC   PPRGAOFF,CLOO       SET NEW OFFICE CODE                          
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PRODUCTION OPTIONS                                                  *         
***********************************************************************         
                                                                                
         USING OPDELD,R2                                                        
OPDR     NTR1  ,                                                                
         L     R4,APOPT            R4=OPTIONS TABLE                             
         USING OPTTABD,R4                                                       
*                                                                               
OPDR3    CLC   OPNUM,OPDNUM        TEST OPTION NUMBER                           
         BE    OPDR5                                                            
         LA    R4,OPTTLNQ(R4)                                                   
         CLI   0(R4),EOT           TEST EOT                                     
         BNE   OPDR3                                                            
         B     XIT                 NO CONVERSION ON THIS OPTION                 
*                                                                               
OPDR5    LA    R6,OPENTRY                                                       
         USING OPENTRY,R6                                                       
         SR    R3,R3               R3 = ELEMENT DATA                            
         ICM   R3,1,OPTDISP                                                     
         AR    R3,R2                                                            
*                                                                               
         CLI   OPTST,EDSWC                                                      
         BNE   OPDR9                                                            
         SR    R1,R1                                                            
         IC    R1,OPDLN            ASSUME WC FIELD LEN WILL BE EVEN             
         SH    R1,=Y(OPDLN1Q)      DIVIDE DATA LEN BY 2 = # OF WC'S             
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         SRL   R1,1                                                             
         BAS   RE,WCRPL            WORKCODE                                     
         LA    R3,2(R3)                                                         
         BCT   R1,*-8                                                           
         B     XIT                                                              
*                                                                               
OPDR9    CLI   OPTST,EDSOF         OFFICE                                       
         BNE   OPDR11                                                           
         XC    ELEMENT,ELEMENT     PROFILE EL MAY GET BIGGER                    
         SR    R1,R1                                                            
         IC    R1,OPDLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   ELEMENT(0),OPDELD                                                
         LA    R5,ELEMENT                                                       
         MVI   OPDLN-OPDELD(R5),OPDLN1Q+2   SET LEN FOR 2 BYTE CODE             
         LA    R3,OPDDATA-OPDELD(R5)                                            
         BAS   RE,OFRPL                                                         
         BNE   XIT                                                              
         BAS   RE,FITL             FIT ELEMENT INTO THE RECORD                  
         B     XIT                                                              
*                                                                               
OPDR11   CLC   OPDLN,OPELN         TEST EL LEN INSURE PROPER DATA FIELD         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   OPTST,EDSCM                                                      
         BNE   OPDR13                                                           
         BAS   RE,CPRPL            COMPANY CODE                                 
         LA    R6,OPENLN(R6)       SECONDARY ENTRY                              
         CLI   OPTDISP,0                                                        
         BE    XIT                 ACCOUNT ALSO?                                
         SR    R3,R3               R3 = ELEMENT DATA                            
         ICM   R3,1,OPTDISP                                                     
         AR    R3,R2                                                            
*                                                                               
OPDR13   CLI   OPTST,EDSAC                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ACRPL            ACCOUNT                                      
*                                                                               
OPDR15   B     XIT                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* OFFICE CHECK NUMBER                                                 *         
***********************************************************************         
                                                                                
         USING OCNELD,R2                                                        
OCNR     TM    CNVSW,CNVSOF        TEST OFFICE CONVERSION                       
         BNOR  RE                                                               
OCNR1    NTR1  ,                                                                
         CLI   OCNFILT,C'*'        OFFICE FILTER                                
         BNE   *+10                                                             
         MVC   OCNFILT,SPACES                                                   
         LA    R3,OCNPOFF                                                       
         BAS   RE,OFRPL                                                         
         CLI   OCNPOFF+1,C' '      TEST 2 BYTE OFFICE                           
         BH    XIT                                                              
         MVC   OCNPOFF,DFLTOFF     USE DEFAULT                                  
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* MEDIA TRANSFER PROFILE                                              *         
***********************************************************************         
                                                                                
         USING MTPELD,R2                                                        
MTPR     NTR1  ,                                                                
         XC    ELEMENT,ELEMENT                                                  
         SR    R1,R1                                                            
         IC    R1,MTPLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   ELEMENT(0),MTPELD                                                
         LA    R3,ELEMENT+(MTPFDATA-MTPELD)                                     
*                                  IGNORE MEDIA OFFICE (FOR NOW)                
*        LA    RF,OFRPL            TRY OFFICE                                   
*        CLI   MTPFNUM,MTPFIOFC                                                 
*        BE    MTPR5                                                            
*                                                                               
         LA    RF,ACRPL            OR ACCOUNT                                   
         LA    RE,MTPACTAB                                                      
*                                                                               
MTPR3    CLI   0(RE),X'FF'         TEST END OF TABLE                            
         BE    XIT                                                              
         CLC   MTPFNUM,0(RE)       MATCH TYPE CODE                              
         BE    MTPR5                                                            
         LA    RE,1(RE)                                                         
         B     MTPR3                                                            
*                                                                               
MTPR5    BASR  RE,RF               REPLACE ROUTINE                              
         BNE   XIT                                                              
         LA    R1,ELEMENT+13+(MTPFDATA-MTPELD)                                  
         LA    RF,14                                                            
         CLI   0(R1),C' '          FIND ACCOUNT LENGTH                          
         BH    *+12                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
         DC    H'0'                                                             
         AH    RF,=Y(MTPLN1Q)                                                   
         STC   RF,ELEMENT+1                                                     
         BAS   RE,FITL            GET ELEMENT INTO RECORD                       
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
MTPACTAB DC    AL1(MTPFSJ1C)                                                    
         DC    AL1(MTPFRCV)                                                     
         DC    AL1(MTPFRCSJ)                                                    
         DC    AL1(MTPFCLRC)                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* TRANSACTION NARRATIVE                                               *         
***********************************************************************         
                                                                                
         USING TRNELD,R2                                                        
TRNR     NTR1  ,                                                                
         CLI   TRNLN,TRNLN1Q                                                    
         BNH   XIT                                                              
         CLC   TRNNARR(6),=C'CHECK#'                                            
         BNE   TRNR10                                                           
         CLC   TRNNARR+14(5),=C'DATED'                                          
         BNE   XIT                                                              
         CLC   TRNNARR+29(6),=C'AMOUNT'                                         
         BNE   XIT                                                              
         CLC   TRNNARR+43(8),=C'BANK A/C'                                       
         BNE   XIT                                                              
         MVC   ELEMENT,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,TRNLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   ELEMENT(0),TRNELD                                                
         LA    R5,ELEMENT                                                       
         LA    R3,TRNNARR+52-TRNELD(R5)                                         
         OC    0(14,R3),SPACE                                                   
         BAS   RE,ACRPL                                                         
         BNE   XIT                                                              
         LA    R1,L'ACEN+(TRNNARR+52-TRNELD)                                    
         LA    R3,L'ACEN-1+TRNNARR+52-TRNELD(R5)                                
TRNR2    CLI   0(R3),C' '                                                       
         BH    *+12                                                             
         BCTR  R1,0                                                             
         BCT   R3,TRNR2                                                         
         DC    H'0'                                                             
         STC   R1,1(R5)                                                         
         BAS   RE,FITL                                                          
         B     XIT                                                              
*                                                                               
TRNR10   CLI   TRNTYPE,X'81'                                                    
         BNE   XIT                                                              
         CLI   XCOMPANY,0          TEST CHANGING COMPANY CODE                   
         BE    TRNR13                                                           
         CLC   TRNNARR+26(1),XCOMPANY                                           
         BNE   *+10                                                             
         MVC   TRNNARR+26(1),COMPANY                                            
TRNR13   MVI   BYTE,C' '                                                        
         LA    R3,TRNNARR+27                                                    
         CLI   13(R3),C'V'                                                      
         BNE   TRNR15                                                           
         MVC   BYTE,13(R3)                                                      
         MVI   13(R3),C' '                                                      
*                                                                               
TRNR15   BAS   RE,ACRPL                                                         
         CLI   BYTE,C'V'                                                        
         BNE   XIT                                                              
         MVC   13(1,R3),BYTE                                                    
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE RECORD ACTIVITY POINTERS                                     *         
***********************************************************************         
                                                                                
         USING PTRELD,R2                                                        
PTRR     NTR1  ,                                                                
         CLI   PTRTYPE,PTRTRAP                                                  
         BNE   XIT                                                              
         MVI   PTREL,X'FF'                                                      
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',AOUT),0,0                        
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* STUDIO ELEMENT                                                      *         
***********************************************************************         
                                                                                
         USING STUELD,R2                                                        
STUR     NTR1  ,                                                                
         LA    R3,STUMED           MEDIA CODE                                   
         BAS   RE,MCRPL                                                         
         LA    R3,STUVEND          COMPANY                                      
         BAS   RE,CPRPL                                                         
         LA    R3,STUVEND+1        ACCOUNT                                      
         BAS   RE,ACRPL                                                         
         SR    R1,R1                                                            
         IC    R1,STULN                                                         
         SH    R1,=H'18'           NUMBER OF OFFICE/CLIENT PAIRS                
         BZ    XIT                                                              
         SR    R0,R0                                                            
         D     R0,=F'5'                                                         
         LR    R0,R1                                                            
         LA    R1,STUOCLN                                                       
*                                                                               
STUR3    LA    R3,STUOFF-STUOCLN(R1) OFFCIE                                     
         BAS   RE,OFRPL                                                         
         LA    R3,STUCLN-STUOCLN(R1) CLIENT                                     
         BAS   RE,CCRPL                                                         
         LA    R1,L'STUOCLN(R1)                                                 
         BCT   R0,STUR3                                                         
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* R.L. FILTER                                                         *         
***********************************************************************         
                                                                                
         USING RFLELD,R2                                                        
RFLR     TM    CNVSW,CNVSOF        TEST OFFICE CONVERSION                       
         BNOR  RE                                                               
RFLR1    NTR1  ,                                                                
         CLI   RFLTYPE,RFLOFF      OFFICE                                       
         BE    *+12                                                             
         CLI   RFLTYPE,RFLAOFF     ANALYSIS OFFICE                              
         BNE   XIT                                                              
*                                                                               
         LA    R5,RFLDATA-RFLELD                                                
         XC    ELEMENT,ELEMENT                                                  
         LR    RF,R5                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ELEMENT(0),RFLELD   MOVE FIRST PART OF ELEMENT                   
         SR    R0,R0                                                            
         IC    R0,RFLLN                                                         
         SR    R0,R5               R0=LENGTH OF DATA                            
         AH    R0,=H'1'                                                         
         SRL   R0,1                R0=NUMBER OF OFFICES                         
         LR    R1,R0                                                            
         MH    R1,=H'3'                                                         
         BCTR  R1,0                                                             
         AR    R1,R5                                                            
         STC   R1,ELEMENT+1        SET NEW ELEMENT LENGTH                       
         LA    R6,RFLDATA                                                       
         LA    R4,ELEMENT(R5)                                                   
*                                                                               
RFLR3    MVC   OFFC,SPACE          CLEAR 2 BYTE OFFICE                          
         MVC   OFFC(1),0(R6)                                                    
         LA    R3,OFFC                                                          
         BAS   RE,OFRPL            SEARCH/REPLACE FROM OFFICE TABLE             
         MVC   0(2,R4),OFFC                                                     
         MVC   2(1,R4),1(R6)       SOFT DELIMITER                               
         LA    R4,3(R4)                                                         
         LA    R6,2(R6)                                                         
         BCT   R0,RFLR3                                                         
         BAS   RE,FITL                                                          
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FIT NEW ELEMENT INTO THE RECORD                                     *         
***********************************************************************         
                                                                                
FITL     NTR1  ,                                                                
         CLC   ELEMENT+1(1),1(R2)  TEST SAME LENGTH                             
         BE    FITL5                                                            
*                                                                               
         L     RF,AOUT                                                          
         SR    R3,R3               FIND LENGTH OF DATA TO MOVE                  
         ICM   R3,3,ACCRLEN-ACCRECD(RF)  R3=RECORD LENGTH                       
         LR    RF,R2                     RF=CURRENT                             
         S     RF,AOUT                   RF=CURRENT-START                       
         SR    R3,RF                     R3=REMAINDER                           
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         SR    R3,R1                     R3=LESS CURRENT ELEMENT                
         BP    *+6                                                              
         DC    H'0'            BAD RECORD(MUST INCLUDE AN ENDING 0)             
         LA    RE,0(R1,R2)         RE=START OF REMAINING DATA                   
         LR    R1,R3               SET LENGTH FOR MOVE                          
         LR    RF,R3                                                            
         L     R0,ALIO                                                          
         MVCL  R0,RE               SAVE REMAINING DATA                          
         SR    R1,R1                                                            
         IC    R1,ELEMENT+1                                                     
         LA    RE,0(R1,R2)         RE=NEW START OF REMAINING DATA               
         LR    R1,R3               SET LENGTH FOR MOVE                          
         LR    RF,R3                                                            
         L     R0,ALIO                                                          
         MVCL  RE,R0               RESTORE REST OF RECORD                       
*                                                                               
         L     RF,AOUT             FIX RECORD LENGTH                            
         SR    RE,RE                                                            
         ICM   RE,3,ACCRLEN-ACCRECD(RF)                                         
         SR    R1,R1                                                            
         IC    R1,1(R2)            SUBTRACT OLD LENGTH                          
         SR    RE,R1                                                            
         IC    R1,ELEMENT+1        ADD NEW LENGTH                               
         AR    RE,R1                                                            
         STCM  RE,3,ACCRLEN-ACCRECD(RF)                                         
         BCTR  RE,0                                                             
         AR    RF,RE               RF=EOR                                       
         MVI   0(RF),0             NEW EOR                                      
*                                                                               
FITL5    SR    R1,R1               MOVE IN NEW ELEMENT                          
         IC    R1,ELEMENT+1                                                     
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R2),ELEMENT     REPLACE OLD ELEMENT                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONSTANTS & LOACAL STORAGE                                          *         
***********************************************************************         
                                                                                
TABADD   DS    0F                                                               
         DC    A(ELMT)                                                          
         DC    A(BATT)                                                          
         DC    A(POPT)                                                          
*                                                                               
ALIO     DC    A(LIO)              A(LOCAL IO AREA)                             
*                                                                               
OFFC     DS    CL2                                                              
*                                                                               
ANALULA  DS    0CL14               ANALSIS ACCOUNT                              
ANALUL   DC    CL2'11'                                                          
ANALACC  DC    CL12' '                                                          
*                                                                               
PRDULA   DS    0CL14               PRODUCTION ACCOUNT                           
PRDUL    DC    CL2'SJ'                                                          
PRDACC   DC    CL12' '                                                          
*                                                                               
EXPNULA  DS    0CL14               EXPENSE ACCOUNT                              
EXPNUL   DC    CL2'SJ'                                                          
EXPNACC  DC    CL12' '                                                          
*                                                                               
SPACES   DC    CL132' '                                                         
SVMODE   DC    X'00'               SAVE CURRENT MODE                            
SVRE     DC    F'0'                SAVE RE                                      
*                                                                               
PERIN    DS    0CL22               INPUT                                        
PERIOFF  DS    CL2                 OFFICE                                       
PERIDPT  DS    CL6                 DEPARTMENT                                   
PERISUB  DS    CL6                 SUB DEPARTMENT                               
PERIPER  DS    CL8                 PERSON CODE                                  
*                                                                               
PEROUT   DS    0CL22               OUTPUT                                       
PEROOFF  DS    CL2                 OFFICE                                       
PERODPT  DS    CL6                 DEPARTMENT                                   
PEROSUB  DS    CL6                 SUB DEPARTMENT                               
PEROPER  DS    CL8                 PERSON CODE                                  
*                                                                               
*                                  PARSE CONTROL TABLE                          
PEREL    DC    AL1(PERIOFF-PERIN,L'PERIOFF)                                     
         DC    AL1(PERIDPT-PERIN,L'PERIDPT)                                     
         DC    AL1(PERISUB-PERIN,L'PERISUB)                                     
         DC    AL1(PERIPER-PERIN,L'PERIPER)                                     
*                                                                               
LIO      DS    XL2000                                                           
                                                                                
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ELEMENT POINTER TABLE                                               *         
***********************************************************************         
                                                                                
ELMT     DC    255F'0'                                                          
         ORG   ELMT+(CPYELQ*4)                                                  
         DC    AL1(CPYELQ),AL3(CPYT)  COMPANY                                   
         ORG   ELMT+(PMDELQ*4)                                                  
         DC    AL1(PMDELQ),AL3(PMDT)  PRODUCTION MEDIA                          
         ORG   ELMT+(WCOELQ*4)                                                  
         DC    AL1(WCOELQ),AL3(WCOT)  WORKCODE                                  
         ORG   ELMT+(LDGELQ*4)                                                  
         DC    AL1(LDGELQ),AL3(LDGT)  LEDGER                                    
         ORG   ELMT+(GLPELQ*4)                                                  
         DC    AL1(GLPELQ),AL3(GLPT)  GENERAL LEDGER POSTING                    
         ORG   ELMT+(MDIELQ*4)                                                  
         DC    AL1(MDIELQ),AL3(MDIT)  MEDIA INTERFACE                           
         ORG   ELMT+(MDTELQ*4)                                                  
         DC    AL1(MDTELQ),AL3(MDTT)  MEDIA TRANSFER                            
         ORG   ELMT+(OTHELQ*4)                                                  
         DC    AL1(OTHELQ),AL3(OTHT)  OTHERS                                    
         ORG   ELMT+(PPRELQ*4)                                                  
         DC    AL1(PPRELQ),AL3(PPRT)  PRODUCTION PROFILE                        
         ORG   ELMT+(RBRELQ*4)                                                  
         DC    AL1(RBRELQ),AL3(RBRT)  RETAIL BILL RECEIVABLE                    
         ORG   ELMT+(SPAELQ*4)                                                  
         DC    AL1(SPAELQ),AL3(SPAT)  SPECIAL POSTING ACCOUNT                   
         ORG   ELMT+(ICPELQ*4)                                                  
         DC    AL1(ICPELQ),AL3(ICPT)  INTERCOMPANY                              
         ORG   ELMT+(MBTELQ*4)                                                  
         DC    AL1(MBTELQ),AL3(MBTT)  MEDIA BILLING TRANSFER                    
         ORG   ELMT+(MTPELQ*4)                                                  
         DC    AL1(MTPELQ),AL3(MTPT)  MEDIA TRANSFER PROFILE                    
         ORG   ELMT+(RSTELQ*4)                                                  
         DC    AL1(RSTELQ),AL3(RSTT)  RECORD STATUS                             
         ORG   ELMT+(BUDELQ*4)                                                  
         DC    AL1(BUDELQ),AL3(BUDT)  BUDGET                                    
         ORG   ELMT+(ESTELQ*4)                                                  
         DC    AL1(ESTELQ),AL3(ESTT)  JOB ESTIMATE                              
         ORG   ELMT+(CEXELQ*4)                                                  
         DC    AL1(CEXELQ),AL3(CEXT)  COKE EXPENDITURE                          
         ORG   ELMT+(PBAELQ*4)                                                  
         DC    AL1(PBAELQ),AL3(PBAT)  BRAND POOL                                
         ORG   ELMT+(WPBELQ*4)                                                  
         DC    AL1(WPBELQ),AL3(WPBT)  CREATIVE BILLING                          
         ORG   ELMT+(SANELQ*4)                                                  
         DC    AL1(SANELQ),AL3(SANT)  SALES ANALYSIS                            
         ORG   ELMT+(PRUELQ*4)                                                  
         DC    AL1(PRUELQ),AL3(PRUT)  PRODUCTION RULES                          
         ORG   ELMT+(CACELQ*4)                                                  
         DC    AL1(CACELQ),AL3(CACT)  CONTRA ELEMENT                            
         ORG   ELMT+(TRNELQ*4)                                                  
         DC    AL1(TRNELQ),AL3(TRNT)  TRANSACTION ELEMENT                       
         ORG   ELMT+(VBIELQ*4)                                                  
         DC    AL1(VBIELQ),AL3(VBIT)  VAT BILLED                                
         ORG   ELMT+(SPDELQ*4)                                                  
         DC    AL1(SPDELQ),AL3(SPDT)  SUBSIDIARY POSTING                        
         ORG   ELMT+(PXDELQ*4)                                                  
         DC    AL1(PXDELQ),AL3(PXDT)  POSTING XREF                              
         ORG   ELMT+(CPJELQ*4)                                                  
         DC    AL1(CPJELQ),AL3(CPJT)  CLIENT/PROD/JOB                           
         ORG   ELMT+(PCIELQ*4)                                                  
         DC    AL1(PCIELQ),AL3(PCIT)  PROJECT CONTROL                           
         ORG   ELMT+(OCNELQ*4)                                                  
         DC    AL1(OCNELQ),AL3(OCNT)  OFFICE CHECK NUMBER                       
         ORG   ELMT+(APTELQ*4)                                                  
         DC    AL1(APTELQ),AL3(APTT)  ACCOUNT POINTER                           
         ORG   ELMT+(SUTELQ*4)                                                  
         DC    AL1(SUTELQ),AL3(SUTT)  SALES/USE TAX                             
         ORG   ELMT+(MPYELQ*4)                                                  
         DC    AL1(MPYELQ),AL3(MPYT)  MANUAL PAYMENT                            
         ORG   ELMT+(ANOELQ*4)                                                  
         DC    AL1(ANOELQ),AL3(ANOT)  ANALYZED OFFICE                           
         ORG   ELMT+(ORDELQ*4)                                                  
         DC    AL1(ORDELQ),AL3(ORDT)  PRODUCTION ORDER                          
         ORG   ELMT+(OAMELQ*4)                                                  
         DC    AL1(OAMELQ),AL3(OAMT)  ORDER AMOUNT                              
         ORG   ELMT+(ADSELQ*4)                                                  
         DC    AL1(ADSELQ),AL3(ADST)  ASSET DESCRIPTION                         
         ORG   ELMT+(MRXELQ*4)                                                  
         DC    AL1(MRXELQ),AL3(MRXT)  MEDIA RECONCILE                           
         ORG   ELMT+(PTAELQ*4)                                                  
         DC    AL1(PTAELQ),AL3(PTAT)  PROD TRANSACTION ACTIVITY                 
         ORG   ELMT+(SORELQ*4)                                                  
         DC    AL1(SORELQ),AL3(SORT)  SOURCE                                    
         ORG   ELMT+(PBIELQ*4)                                                  
         DC    AL1(PBIELQ),AL3(PBIT)  PST BILLED                                
         ORG   ELMT+(LOCELQ*4)                                                  
         DC    AL1(LOCELQ),AL3(LOCT)  STAFF LOCATION ELEMENT                    
         ORG   ELMT+(TIMELQ*4)                                                  
         DC    AL1(TIMELQ),AL3(TIMT)  TIME DETAIL                               
         ORG   ELMT+(VPDELQ*4)                                                  
         DC    AL1(VPDELQ),AL3(VPDT)  VENDOR PAYMENT DETAIL                     
         ORG   ELMT+(TSDELQ*4)                                                  
         DC    AL1(TSDELQ),AL3(TSDT)  TALENT  DETAIL                            
         ORG   ELMT+(OPDELQ*4)                                                  
         DC    AL1(OPDELQ),AL3(OPDT)  OPTIONS DATA                              
         ORG   ELMT+(CWKELQ*4)                                                  
         DC    AL1(CWKELQ),AL3(CWKT)  CATEGORY WORK CODE                        
         ORG   ELMT+(EDAELQ*4)                                                  
         DC    AL1(EDAELQ),AL3(EDAT)  ESTIMATE DATA                             
         ORG   ELMT+(BESELQ*4)                                                  
         DC    AL1(BESELQ),AL3(BEST)  BILLING ESTIMATE                          
         ORG   ELMT+(MNAELQ*4)                                                  
         DC    AL1(MNAELQ),AL3(MNAT)  MEDIA NAME                                
         ORG   ELMT+(BDAELQ*4)                                                  
         DC    AL1(BDAELQ),AL3(BDAT)  BILLING DATA                              
*&&US                                                                           
         ORG   ELMT+(INCELQ*4)                                                  
         DC    AL1(INCELQ),AL3(INCT)  INTERNAL INCOME                           
*&&                                                                             
         ORG   ELMT+(STUELQ*4)                                                  
         DC    AL1(STUELQ),AL3(STUT)  STUDIO DATA                               
         ORG   ELMT+(LNKELQ*4)                                                  
         DC    AL1(LNKELQ),AL3(LNKT)  LINK DATA                                 
         ORG   ELMT+(EPTELQ*4)                                                  
         DC    AL1(EPTELQ),AL3(EPTT)  EST. PERSON TIME                          
         ORG   ELMT+(APEELQ*4)                                                  
         DC    AL1(APEELQ),AL3(APET)  ANALYSIS POINTER                          
         ORG   ELMT+(RFLELQ*4)                                                  
         DC    AL1(RFLELQ),AL3(RFLT)  R.L FILTER                                
         ORG   ELMT+(RALELQ*4)                                                  
         DC    AL1(RALELQ),AL3(RALT)  RECEIVABLE ALLOCATION                     
         ORG   ELMT+(FFTELQ*4)                                                  
         DC    AL1(FFTELQ),AL3(FFTT)  FREE FORM TEXT                            
         ORG   ELMT+(TAXIELQ*4)                                                 
         DC    AL1(TAXIELQ),AL3(TAXT) TAX RULES                                 
         ORG   ELMT+(TAXOELQ*4)                                                 
         DC    AL1(TAXOELQ),AL3(TAXT) TAX RULES                                 
         ORG   ELMT+(SFSELQ*4)                                                  
         DC    AL1(SFSELQ),AL3(SFST)  SCREEN FIELD                              
         ORG   ELMT+(ASKELQ*4)                                                  
         DC    AL1(ASKELQ),AL3(ASKT)  ACCOUNT SYSTEM KEY                        
         ORG   ELMT+(GLRELQ*4)                                                  
         DC    AL1(GLRELQ),AL3(GLRT)  G/L OFFICE RULES                          
         ORG   ELMT+(BICELQ*4)                                                  
         DC    AL1(BICELQ),AL3(BICT)  BATCH ITEM CHECK                          
         ORG   ELMT+(BIOELQ*4)                                                  
         DC    AL1(BIOELQ),AL3(BIOT)  BATCH ORDER                               
         ORG   ELMT+(PTRELQ*4)                                                  
         DC    AL1(PTRELQ),AL3(PTRT)  BATCH ORDER                               
         ORG   ELMT+(255*4)                                                     
         EJECT                                                                  
***********************************************************************         
* ELEMENT DETAIL TABLE                                                *         
***********************************************************************         
                                                                                
*                                                                               
CPYT     DC    AL1(CPYELQ),AL1(CPYX-CPYT),AL1(CPYRQ)  COMPANY                   
         DC    AL1((CPYX-*)/2)                                                  
CPYX     EQU   *                                                                
*                                                                               
*                                                                               
PMDT     DC    AL1(PMDELQ),AL1(PMDX-PMDT),AL1(0)      PRODUCTION MEDIA          
         DC    AL1((PMDX-*)/2)                                                  
         DC    AL1(PMDCODE-PMDELD),AL1(EDSMC)                                   
         DC    AL1(PMDCOMC1-PMDELD),AL1(EDSCM)                                  
         DC    AL1(PMDCOMU1-PMDELD),AL1(EDSAC)                                  
*&&US                                                                           
         DC    AL1(PMDCSHDC-PMDELD),AL1(EDSCM)                                  
         DC    AL1(PMDCSHDU-PMDELD),AL1(EDSAC)                                  
*&&                                                                             
*&&UK                                                                           
         DC    AL1(PMDVATC1-PMDELD),AL1(EDSCM)                                  
         DC    AL1(PMDVATU1-PMDELD),AL1(EDSAC)                                  
*&&                                                                             
         DC    AL1(PMDGRP-PMDELD),AL1(EDSMG)                                    
*&&US                                                                           
         DC    AL1(PMDPRCD-PMDELD),AL1(EDSCM)                                   
         DC    AL1(PMDPRCD+1-PMDELD),AL1(EDSAC)                                 
         DC    AL1(PMDTWO-PMDELD),AL1(EDSCM)                                    
         DC    AL1(PMDTWO+1-PMDELD),AL1(EDSAC)                                  
         DC    AL1(PMDCOST-PMDELD),AL1(EDSAN)                                   
         DC    AL1(PMDTIN-PMDELD),AL1(EDSCM)                                    
         DC    AL1(PMDTIN+1-PMDELD),AL1(EDSAC)                                  
*&&                                                                             
*&&UK                                                                           
         DC    AL1(PMDVATC2-PMDELD),AL1(EDSCM)                                  
         DC    AL1(PMDVATU2-PMDELD),AL1(EDSAC)                                  
         DC    AL1(PMDCOMC2-PMDELD),AL1(EDSCM)                                  
         DC    AL1(PMDCOMU2-PMDELD),AL1(EDSAC)                                  
         DC    AL1(PMDFLTC1-PMDELD),AL1(EDSCM)                                  
         DC    AL1(PMDFLTU1-PMDELD),AL1(EDSAC)                                  
         DC    AL1(PMDFLTC2-PMDELD),AL1(EDSCM)                                  
         DC    AL1(PMDFLTU2-PMDELD),AL1(EDSAC)                                  
*&&                                                                             
PMDX     EQU   *                                                                
*                                                                               
WCOT     DC    AL1(WCOELQ),AL1(WCOX-WCOT),AL1(0)      WORK CODE                 
         DC    AL1((WCOX-*)/2)                                                  
         DC    AL1(WCOCODE-WCOELD),AL1(EDSWC)                                   
         DC    AL1(WCORMED-WCOELD),AL1(EDSMC)                                   
         DC    AL1(WCOGRP-WCOELD),AL1(EDSWG)                                    
WCOX     EQU   *                                                                
*                                                                               
LDGT     DC    AL1(LDGELQ),AL1(LDGX-LDGT),AL1(0)      LEDGER                    
         DC    AL1((LDGX-*)/2)                                                  
*&&UK*&& DC    AL1(LDGCDSCC-LDGELD),AL1(EDSCM)                                  
         DC    AL1(LDGCDSC-LDGELD),AL1(EDSAC)                                   
         DC    AL1(LDGOFFC-LDGELD),AL1(EDSOF)                                   
LDGX     EQU   *                                                                
*                                                                               
GLPT     DC    AL1(GLPELQ),AL1(GLPX-GLPT),AL1(0)      G/L POSTING               
         DC    AL1((GLPX-*)/2)                                                  
         DC    AL1(GLPACC1-GLPELD),AL1(EDSAC)                                   
         DC    AL1(GLPACC2-GLPELD),AL1(EDSAC)                                   
         DC    AL1(GLPCON1-GLPELD),AL1(EDSAC)                                   
         DC    AL1(GLPCON2-GLPELD),AL1(EDSAC)                                   
GLPX     EQU   *                                                                
*                                                                               
MDIT     DC    AL1(MDIELQ),AL1(MDIX-MDIT),AL1(0)      MEDIA INTERFACE           
         DC    AL1((MDIX-*)/2)                                                  
         DC    AL1(MDICOMM-MDIELD),AL1(EDSAC)                                   
         DC    AL1(MDICSHD-MDIELD),AL1(EDSAC)                                   
         DC    AL1(MDICSHR-MDIELD),AL1(EDSAC)                                   
         DC    AL1(MDICCSR-MDIELD),AL1(EDSAC)                                   
         DC    AL1(MDICNTL-MDIELD),AL1(EDSAC)                                   
         DC    AL1(MDICOST-MDIELD),AL1(EDSAN)                                   
         DC    AL1(MDILOST-MDIELD),AL1(EDSAC)                                   
         DC    AL1(MDICONL-MDIELD),AL1(EDSAC)                                   
MDIX     EQU   *                                                                
*                                                                               
MDTT     DC    AL1(MDTELQ),AL1(MDTX-MDTT),AL1(0)     MEDIA TRANSFER             
         DC    AL1((MDTX-*)/2)                                                  
         DC    AL1(MDTMED-MDTELD),AL1(EDSMC)                                    
         DC    AL1(MDTCLI-MDTELD),AL1(EDSCC)                                    
         DC    AL1(MDTJOB-MDTELD),AL1(EDSMC)                                    
MDTX     EQU   *                                                                
*                                                                               
OTHT     DC    AL1(OTHELQ),AL1(OTHX-OTHT),AL1(OTHRQ) OTHERS                     
         DC    AL1((OTHX-*)/2)                                                  
OTHX     EQU   *                                                                
*                                                                               
PPRT     DC    AL1(PPRELQ),AL1(PPRX-PPRT),AL1(PPRRQ) PROD PROFILE               
         DC    AL1((PPRX-*)/2)                                                  
         DC    AL1(PPRRECVC-PPRELD),AL1(EDSCM)                                  
         DC    AL1(PPRRECVU-PPRELD),AL1(EDSAC)                                  
         DC    AL1(PPRCOSTC-PPRELD),AL1(EDSCM)                                  
         DC    AL1(PPRCOSTU-PPRELD),AL1(EDSAC)                                  
         DC    AL1(PPRGAOFF-PPRELD),AL1(EDSOF)                                  
PPRX     EQU   *                                                                
*                                                                               
RBRT     DC    AL1(RBRELQ),AL1(RBRX-RBRT),AL1(0)     RETAIL BILL RECV           
         DC    AL1((RBRX-*)/2)                                                  
         DC    AL1(RBRRECB-RBRELD),AL1(EDSAC)                                   
         DC    AL1(RBRCOST-RBRELD),AL1(EDSAC)                                   
RBRX     EQU   *                                                                
*                                                                               
SPAT     DC    AL1(SPAELQ),AL1(SPAX-SPAT),AL1(SPARQ) SPECIAL POSTING            
         DC    AL1((SPAX-*)/2)                                                  
SPAX     EQU   *                                                                
*                                                                               
ICPT     DC    AL1(ICPELQ),AL1(ICPX-ICPT),AL1(ICPRQ) INTERCOMPANY               
         DC    AL1((ICPX-*)/2)                                                  
         DC    AL1(ICPSOFF-ICPELD),AL1(EDSOF)                                   
ICPX     EQU   *                                                                
*                                                                               
MBTT     DC    AL1(MBTELQ),AL1(MBTX-MBTT),AL1(0)     MEDIA BILLING              
         DC    AL1((MBTX-*)/2)                                                  
         DC    AL1(MBTULA-MBTELD),AL1(EDSAC)                                    
         DC    AL1(MBTCNTRA-MBTELD),AL1(EDSAC)                                  
         DC    AL1(MBTOFFC-MBTELD),AL1(EDSOF)                                   
MBTX     EQU   *                                                                
*                                                                               
MTPT     DC    AL1(MTPELQ),AL1(MTPX-MTPT),AL1(MTPRQ) MEDIA TRANSFER             
         DC    AL1((MTPX-*)/2)                                                  
MTPX     EQU   *                                                                
*                                                                               
RSTT     DC    AL1(RSTELQ),AL1(RSTX-RSTT),AL1(0)     RECORD STATUS              
         DC    AL1((RSTX-*)/2)                                                  
         DC    AL1(RSTOFFC-RSTELD),AL1(EDSOF)                                   
         DC    AL1(RSTDFTSK-RSTELD),AL1(EDSWC)                                  
RSTX     EQU   *                                                                
*                                                                               
BUDT     DC    AL1(BUDELQ),AL1(BUDX-BUDT),AL1(0)     BUDGET                     
         DC    AL1((BUDX-*)/2)                                                  
         DC    AL1(BUDSBAC+1-BUDELD),AL1(EDSAC)                                 
BUDX     EQU   *                                                                
*                                                                               
ESTT     DC    AL1(ESTELQ),AL1(ESTX-ESTT),AL1(0)     JOB ESTIMATE               
         DC    AL1((ESTX-*)/2)                                                  
         DC    AL1(ESTWORK-ESTELD),AL1(EDSWC)                                   
ESTX     EQU   *                                                                
*                                                                               
CEXT     DC    AL1(CEXELQ),AL1(CEXX-CEXT),AL1(0)     COKE EXPENDITURE           
         DC    AL1((CEXX-*)/2)                                                  
         DC    AL1(CEXACCU-CEXELD),AL1(EDSAC)                                   
CEXX     EQU   *                                                                
*                                                                               
PBAT     DC    AL1(PBAELQ),AL1(PBAX-PBAT),AL1(0)     BRAND POOL                 
         DC    AL1((PBAX-*)/2)                                                  
         DC    AL1(PBARECVC-PBAELD),AL1(EDSCM)                                  
         DC    AL1(PBARECVU-PBAELD),AL1(EDSAC)                                  
         DC    AL1(PBACOSTC-PBAELD),AL1(EDSCM)                                  
         DC    AL1(PBACOSTU-PBAELD),AL1(EDSAC)                                  
PBAX     EQU   *                                                                
*                                                                               
WPBT     DC    AL1(WPBELQ),AL1(WPBX-WPBT),AL1(0)     CREATIVE BILL              
         DC    AL1((WPBX-*)/2)                                                  
         DC    AL1(WPBWORK-WPBELD),AL1(EDSWC)                                   
WPBX     EQU   *                                                                
*                                                                               
SANT     DC    AL1(SANELQ),AL1(SANX-SANT),AL1(0)     SALES ANALYSIS             
         DC    AL1((SANX-*)/2)                                                  
         DC    AL1(SANCODE-SANELD),AL1(EDSCM)                                   
         DC    AL1(SANCODE+1-SANELD),AL1(EDSAC)                                 
SANX     EQU   *                                                                
*                                                                               
PRUT     DC    AL1(PRUELQ),AL1(PRUX-PRUT),AL1(0)     PRODUCTION RULES           
         DC    AL1((PRUX-*)/2)                                                  
         DC    AL1(PRUMED-PRUELD),AL1(EDSMC)                                    
         DC    AL1(PRUWORK-PRUELD),AL1(EDSWC)                                   
PRUX     EQU   *                                                                
*                                                                               
CACT     DC    AL1(CACELQ),AL1(CACX-CACT),AL1(0)     CONTRA ELEMENT             
         DC    AL1((CACX-*)/2)                                                  
         DC    AL1(CACCNTC-CACELD),AL1(EDSCM)                                   
CACX     EQU   *                                                                
*                                                                               
TRNT     DC    AL1(TRNELQ),AL1(TRNX-TRNT),AL1(TRNRQ) TRANSACTION                
         DC    AL1((TRNX-*)/2)                                                  
TRNX     EQU   *                                                                
*                                                                               
VBIT     DC    AL1(VBIELQ),AL1(VBIX-VBIT),AL1(0)     VAT BILLED                 
         DC    AL1((VBIX-*)/2)                                                  
         DC    AL1(VBIACCT-VBIELD),AL1(EDSAC)                                   
VBIX     EQU   *                                                                
*                                                                               
SPDT     DC    AL1(SPDELQ),AL1(SPDX-SPDT),AL1(SPDRQ) SUBSIDIARY POSTING         
         DC    AL1((SPDX-*)/2)                                                  
SPDX     EQU   *                                                                
*                                                                               
PXDT     DC    AL1(PXDELQ),AL1(PXDX-PXDT),AL1(0)     POSTING XREF               
         DC    AL1((PXDX-*)/2)                                                  
         DC    AL1(PXDFRTOC-PXDELD),AL1(EDSCM)                                  
         DC    AL1(PXDFRTOU-PXDELD),AL1(EDSAC)                                  
         DC    AL1(PXDFRTOA-PXDELD),AL1(EDSCC)                                  
         DC    AL1(PXDFRTOA+6-PXDELD),AL1(EDSMC)                                
PXDX     EQU   *                                                                
*                                                                               
CPJT     DC    AL1(CPJELQ),AL1(CPJX-CPJT),AL1(CPJRQ) CLI/PROD/JOB               
         DC    AL1((CPJX-*)/2)                                                  
         DC    AL1(CPJCLI-CPJELD),AL1(EDSCC)                                    
         DC    AL1(CPJJOB-CPJELD),AL1(EDSMC)                                    
         DC    AL1(CPJWRK-CPJELD),AL1(EDSWC)                                    
CPJX     EQU   *                                                                
*                                                                               
PCIT     DC    AL1(PCIELQ),AL1(PCIX-PCIT),AL1(0)     PROJECT CONTROL            
         DC    AL1((PCIX-*)/2)                                                  
         DC    AL1(PCICLI-PCIELD),AL1(EDSCM)                                    
         DC    AL1(PCICLI+3-PCIELD),AL1(EDSCC)                                  
         DC    AL1(PCICLI+9-PCIELD),AL1(EDSMC)                                  
         DC    AL1(PCIPRJT-PCIELD),AL1(EDSCM)                                   
         DC    AL1(PCIPRJT+3-PCIELD),AL1(EDSCC)                                 
         DC    AL1(PCIPRJT+9-PCIELD),AL1(EDSMC)                                 
         DC    AL1(PCITSK-PCIELD),AL1(EDSWC)                                    
PCIX     EQU   *                                                                
*                                                                               
OCNT     DC    AL1(OCNELQ),AL1(OCNX-OCNT),AL1(OCNRQ) OFFICE CHECK               
         DC    AL1((OCNX-*)/2)                                                  
         DC    AL1(OCNBANKC-OCNELD),AL1(EDSCM)                                  
         DC    AL1(OCNBANKU-OCNELD),AL1(EDSAC)                                  
         DC    AL1(OCNDISCU-OCNELD),AL1(EDSAC)                                  
OCNX     EQU   *                                                                
*                                                                               
APTT     DC    AL1(APTELQ),AL1(APTX-APTT),AL1(0)     ACCOUNT POINTER            
         DC    AL1((APTX-*)/2)                                                  
         DC    AL1(APTACCC-APTELD),AL1(EDSCM)                                   
         DC    AL1(APTACCU-APTELD),AL1(EDSAC)                                   
APTX     EQU   *                                                                
*                                                                               
SUTT     DC    AL1(SUTELQ),AL1(SUTX-SUTT),AL1(0)     SALES/USE TAX              
         DC    AL1((SUTX-*)/2)                                                  
         DC    AL1(SUTACCU-SUTELD),AL1(EDSAC)                                   
SUTX     EQU   *                                                                
*                                                                               
MPYT     DC    AL1(MPYELQ),AL1(MPYX-MPYT),AL1(0)     MANUAL PAYMENT             
         DC    AL1((MPYX-*)/2)                                                  
         DC    AL1(MPYBNK-MPYELD),AL1(EDSAC)                                    
MPYX     EQU   *                                                                
*                                                                               
ANOT     DC    AL1(ANOELQ),AL1(ANOX-ANOT),AL1(0)     ANALYZED OFFICE            
         DC    AL1((ANOX-*)/2)                                                  
         DC    AL1(ANOOFFC-ANOELD),AL1(EDSOF)                                   
ANOX     EQU   *                                                                
*                                                                               
ORDT     DC    AL1(ORDELQ),AL1(ORDX-ORDT),AL1(ORDRQ) PRODUCTION ORDER           
         DC    AL1((ORDX-*)/2)                                                  
         DC    AL1(ORDACCC-ORDELD),AL1(EDSCM)                                   
         DC    AL1(ORDACCU-ORDELD),AL1(EDSAC)                                   
         DC    AL1(ORDSUPC-ORDELD),AL1(EDSCM)                                   
         DC    AL1(ORDSUPU-ORDELD),AL1(EDSAC)                                   
ORDX     EQU   *                                                                
*                                                                               
OAMT     DC    AL1(OAMELQ),AL1(OAMX-OAMT),AL1(0)     ORDER AMOUNT               
         DC    AL1((OAMX-*)/2)                                                  
         DC    AL1(OAMWORK-OAMELD),AL1(EDSWC)                                   
         DC    AL1(OAMCONU-OAMELD),AL1(EDSAC)                                   
OAMX     EQU   *                                                                
*                                                                               
ADST     DC    AL1(ADSELQ),AL1(ADSX-ADST),AL1(0)     ASSET DESCRIPTION          
         DC    AL1((ADSX-*)/2)                                                  
         DC    AL1(ADSSUPU-ADSELD),AL1(EDSAC)                                   
ADSX     EQU   *                                                                
*                                                                               
MRXT     DC    AL1(MRXELQ),AL1(MRXX-MRXT),AL1(0)     MEDIA RECONCILE            
         DC    AL1((MRXX-*)/2)                                                  
         DC    AL1(MRXUNT-MRXELD),AL1(EDSAC)                                    
MRXX     EQU   *                                                                
*                                                                               
PTAT     DC    AL1(PTAELQ),AL1(PTAX-PTAT),AL1(0)     PROD TRANSACTION           
         DC    AL1((PTAX-*)/2)                                                  
         DC    AL1(PTAWEUNT-PTAELD),AL1(EDSAC)                                  
         DC    AL1(PTAFWRK-PTAELD),AL1(EDSWC)                                   
         DC    AL1(PTAWWUNT-PTAELD),AL1(EDSAC)                                  
PTAX     EQU   *                                                                
*                                                                               
SORT     DC    AL1(SORELQ),AL1(SORX-SORT),AL1(0)     SOURCE                     
         DC    AL1((SORX-*)/2)                                                  
         DC    AL1(SORAUNT-SORELD),AL1(EDSAC)                                   
SORX     EQU   *                                                                
*                                                                               
PBIT     DC    AL1(PBIELQ),AL1(PBIX-PBIT),AL1(0)     PST BILLED                 
         DC    AL1((PBIX-*)/2)                                                  
         DC    AL1(PBIACCT-PBIELD),AL1(EDSCM)                                   
         DC    AL1(PBIACCT+1-PBIELD),AL1(EDSAC)                                 
PBIX     EQU   *                                                                
*                                                                               
LOCT     DC    AL1(LOCELQ),AL1(LOCX-LOCT),AL1(LOCRQ) STAFF LOCATION             
         DC    AL1((LOCX-*)/2)                                                  
LOCX     EQU   *                                                                
*                                                                               
TIMT     DC    AL1(TIMELQ),AL1(TIMX-TIMT),AL1(TIMRQ) TIME DETAIL                
         DC    AL1((TIMX-*)/2)                                                  
TIMX     EQU   *                                                                
*                                                                               
VPDT     DC    AL1(VPDELQ),AL1(VPDX-VPDT),AL1(0)     VENDOR PAYMENT             
         DC    AL1((VPDX-*)/2)                                                  
         DC    AL1(VPDBNK-VPDELD),AL1(EDSAC)                                    
VPDX     EQU   *                                                                
*                                                                               
TSDT     DC    AL1(TSDELQ),AL1(TSDX-VPDT),AL1(0)     TALENT DETAILS             
         DC    AL1((TSDX-*)/2)                                                  
         DC    AL1(TSDCLI-TSDELD),AL1(EDSCC)                                    
TSDX     EQU   *                                                                
*                                                                               
OPDT     DC    AL1(OPDELQ),AL1(OPDX-OPDT),AL1(OPDRQ) OPTIONS DATA               
         DC    AL1((OPDX-*)/2)                                                  
OPDX     EQU   *                                                                
*                                                                               
CWKT     DC    AL1(CWKELQ),AL1(CWKX-CWKT),AL1(0)     CATEGORY W/C               
         DC    AL1((CWKX-*)/2)                                                  
         DC    AL1(CWKWORK-CWKELD),AL1(EDSWC)                                   
CWKX     EQU   *                                                                
*                                                                               
EDAT     DC    AL1(EDAELQ),AL1(EDAX-EDAT),AL1(0)     ESTIMATE DATA              
         DC    AL1((EDAX-*)/2)                                                  
         DC    AL1(EDAWORK-EDAELD),AL1(EDSWC)                                   
EDAX     EQU   *                                                                
*                                                                               
BEST     DC    AL1(BESELQ),AL1(BESX-BESX),AL1(0)     BILLING ESTIMATE           
         DC    AL1((BESX-*)/2)                                                  
         DC    AL1(BESWC-BESELD),AL1(EDSWC)          3 BYTE WORKCODE            
BESX     EQU   *                                                                
*                                                                               
MNAT     DC    AL1(MNAELQ),AL1(MNAX-MNAT),AL1(0)     MEDIA NAME                 
         DC    AL1((MNAX-*)/2)                                                  
         DC    AL1(MNACODE-MNAELD),AL1(EDSMC)                                   
MNAX     EQU   *                                                                
*                                                                               
BDAT     DC    AL1(BDAELQ),AL1(BDAX-BDAT),AL1(0)     BILLING DATA               
         DC    AL1((BDAX-*)/2)                                                  
         DC    AL1(BDARCVA-BDAELD),AL1(EDSCM)                                   
         DC    AL1(BDARCVA+1-BDAELD),AL1(EDSAC)                                 
         DC    AL1(BDARCVO-BDAELD),AL1(EDSOF)                                   
BDAX     EQU   *                                                                
*                                                                               
*&&US                                                                           
INCT     DC    AL1(INCELQ),AL1(INCX-INCT),AL1(0)     INTERNAL INCOME            
         DC    AL1((INCX-*)/2)                                                  
         DC    AL1(INCWRKC-INCELD),AL1(EDSWC)                                   
         DC    AL1(INCCACC-INCELD),AL1(EDSAC)                                   
         DC    AL1(INCDACC-INCELD),AL1(EDSAC)                                   
INCX     EQU   *                                                                
*&&                                                                             
*                                                                               
STUT     DC    AL1(STUELQ),AL1(STUX-STUT),AL1(STURQ) STUDIO DATA                
         DC    AL1((STUX-*)/2)                                                  
STUX     EQU   *                                                                
*                                                                               
LNKT     DC    AL1(LNKELQ),AL1(LNKX-LNKT),AL1(0)     LINK DATA                  
         DC    AL1((LNKX-*)/2)                                                  
         DC    AL1(LNKSTJB-LNKELD),AL1(EDSSJ)                                   
         DC    AL1(LNKSTJB+6-LNKELD),AL1(EDSMC)                                 
         DC    AL1(LNKAGJB-LNKELD),AL1(EDSSJ)                                   
         DC    AL1(LNKAGJB+6-LNKELD),AL1(EDSMC)                                 
LNKX     EQU   *                                                                
*                                                                               
EPTT     DC    AL1(EPTELQ),AL1(EPTX-EPTT),AL1(0)    EST. PERSON TIME            
         DC    AL1((EPTX-*)/2)                                                  
         DC    AL1(EPTULA-EPTELD),AL1(EDSAC)                                    
EPTX     EQU   *                                                                
*                                                                               
APET     DC    AL1(APEELQ),AL1(APEX-APET),AL1(APERQ) ANALYSIS POINTER           
         DC    AL1((APEX-*)/2)                                                  
APEX     EQU   *                                                                
*                                                                               
RFLT     DC    AL1(RFLELQ),AL1(RFLX-RFLT),AL1(RFLRQ) R.L. FILTER                
         DC    AL1((RFLX-*)/2)                                                  
RFLX     EQU   *                                                                
*                                                                               
RALT     DC    AL1(RALELQ),AL1(RALX-RALT),AL1(RALRQ) RECV. ALLOCATION           
         DC    AL1((RALX-*)/2)                                                  
RALX     EQU   *                                                                
*                                                                               
FFTT     DC    AL1(FFTELQ),AL1(FFTX-FFTT),AL1(FFTRQ) FREE FORM TEXT             
         DC    AL1((FFTX-*)/2)                                                  
FFTX     EQU   *                                                                
*                                                                               
TAXT     DC    AL1(TAXIELQ),AL1(TAXX-TAXT),AL1(0)  TAX RULES                    
         DC    AL1((TAXX-*)/2)                                                  
         DC    AL1(TAXACTU-TAXELD),AL1(EDSAC)                                   
TAXX     EQU   *                                                                
*                                                                               
SFST     DC    AL1(SFSELQ),AL1(SFSX-SFST),AL1(SFSRQ) SCREEN FIELD               
         DC    AL1((SFSX-*)/2)                                                  
SFSX     EQU   *                                                                
*                                                                               
ASKT     DC    AL1(ASKELQ),AL1(ASKX-ASKT),AL1(ASKRQ) ACC SYS KEY                
         DC    AL1((ASKX-*)/2)                                                  
         DC    AL1((ASKKEY-ASKELD)+(TRNKCPY-TRNRECD)),AL1(EDSCM)                
         DC    AL1((ASKKEY-ASKELD)+(TRNKCCPY-TRNRECD)),AL1(EDSCM)               
ASKX     EQU   *                                                                
*                                                                               
GLRT     DC    AL1(GLRELQ),AL1(GLRX-GLRT),AL1(GLRRQ) G/L OFFICE RULES           
         DC    AL1((GLRX-*)/2)                                                  
GLRX     EQU   *                                                                
*                                                                               
BICT     DC    AL1(BICELQ),AL1(BICX-BICT),AL1(0)     BATCH ITEM                 
         DC    AL1((BICX-*)/2)                                                  
         DC    AL1(BICACT-BICELD),AL1(EDSCM)                                    
         DC    AL1(BICACTU-BICELD),AL1(EDSAC)                                   
         DC    AL1(BICCAC-BICELD),AL1(EDSCM)                                    
         DC    AL1(BICCACU-BICELD),AL1(EDSAC)                                   
         DC    AL1(BICOFF-BICELD),AL1(EDSOF)                                    
BICX     EQU   *                                                                
*                                                                               
BIOT     DC    AL1(BIOELQ),AL1(BIOX-BIOT),AL1(0)     BATCH ORDER                
         DC    AL1((BIOX-*)/2)                                                  
         DC    AL1(BIOWORK-BIOELD),AL1(EDSWC)                                   
BIOX     EQU   *                                                                
*                                                                               
PTRT     DC    AL1(PTRELQ),AL1(PTRX-PTRT),AL1(PTRRQ)  PASSIVE POINTER           
         DC    AL1((PTRX-*)/2)                                                  
PTRX     EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* TABLE OF BATCH SCREEN DISPLAY ELEMENTS                              *         
***********************************************************************         
                                                                                
*&&UK                                                                           
       ++INCLUDE ACNVBATGER                                                     
         DC    X'FF'                                                            
*&&                                                                             
                                                                                
*&&US                                                                           
BATT     DS    0D                                                               
         DC    AL1(1),AL1(21),AL1(BATACC),CL2'SV',AL2(0)                        
         DC    AL1(1),AL1(40),AL1(BATACC),CL2'SE',AL2(0)                        
         DC    AL1(1),AL1(45),AL1(BATOFF),CL2'  ',AL2(0)                        
         DC    AL1(1),AL1(47),AL1(BATOFF),CL2'  ',AL2(0)                        
         DC    AL1(1),AL1(49),AL1(BATOFF),CL2'  ',AL2(0)                        
*                                                                               
         DC    AL1(3),AL1(21),AL1(BATACC),CL2'SC',AL2(0)                        
         DC    AL1(3),AL1(35),AL1(BATACC),CL2'SV',AL2(0)                        
         DC    AL1(3),AL1(33),AL1(BATACC),CL2'SE',AL2(0)                        
         DC    AL1(3),AL1(41),AL1(BATOFF),CL2'  ',AL2(0)                        
         DC    AL1(3),AL1(43),AL1(BATOFF),CL2'  ',AL2(0)                        
         DC    AL1(3),AL1(45),AL1(BATOFF),CL2'  ',AL2(0)                        
*                                                                               
         DC    AL1(6),AL1(20),AL1(BATACC),CL2'SI',AL2(0)                        
*                                                                               
         DC    AL1(8),AL1(21),AL1(BATACC),CL2'SI',AL2(0)                        
         DC    AL1(8),AL1(35),AL1(BATACC),CL2'SE',AL2(0)                        
         DC    AL1(8),AL1(31),AL1(BATOFF),CL2'  ',AL2(0)                        
         DC    AL1(8),AL1(42),AL1(BATOFF),CL2'  ',AL2(0)                        
*                                                                               
         DC    AL1(14),AL1(21),AL1(BATACC),CL2'SE',AL2(0)                       
         DC    AL1(14),AL1(23),AL1(BATACC),CL2'SE',AL2(0)                       
         DC    AL1(14),AL1(25),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(14),AL1(30),AL1(BATOFF),CL2'  ',AL2(0)                       
         DC    AL1(14),AL1(35),AL1(BATOFF),CL2'  ',AL2(0)                       
*                                                                               
         DC    AL1(15),AL1(21),AL1(BATACC),CL2'SE',AL2(0)                       
         DC    AL1(15),AL1(23),AL1(BATACC),CL2'SE',AL2(0)                       
         DC    AL1(15),AL1(25),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(15),AL1(30),AL1(BATOFF),CL2'  ',AL2(0)                       
         DC    AL1(15),AL1(35),AL1(BATOFF),CL2'  ',AL2(0)                       
*                                                                               
         DC    AL1(19),AL1(13),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(19),AL1(15),AL1(BATACC),CL2'  ',AL2(0)                       
*                                                                               
         DC    AL1(20),AL1(13),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(20),AL1(15),AL1(BATOFF),CL2'  ',AL2(0)                       
         DC    AL1(20),AL1(17),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(20),AL1(19),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(20),AL1(21),AL1(BATOFF),CL2'  ',AL2(0)                       
         DC    AL1(20),AL1(23),AL1(BATACC),CL2'  ',AL2(0)                       
*                                                                               
         DC    AL1(21),AL1(15),AL1(BATACC),CL2'SE',AL2(0)                       
         DC    AL1(21),AL1(17),AL1(BATACC),CL2'SX',AL2(0)                       
         DC    AL1(21),AL1(24),AL1(BATOFF),CL2'  ',AL2(0)                       
         DC    AL1(21),AL1(25),AL1(BATOFF),CL2'  ',AL2(0)                       
         DC    AL1(21),AL1(26),AL1(BATOFF),CL2'  ',AL2(0)                       
         DC    AL1(21),AL1(58),AL1(BATACC),CL2'2D',AL2(0)                       
*                                                                               
         DC    AL1(22),AL1(15),AL1(BATACC),CL2'SE',AL2(0)                       
         DC    AL1(22),AL1(17),AL1(BATACC),CL2'SC',AL2(0)                       
         DC    AL1(22),AL1(21),AL1(BATOFF),CL2'  ',AL2(0)                       
         DC    AL1(22),AL1(23),AL1(BATOFF),CL2'  ',AL2(0)                       
         DC    AL1(22),AL1(25),AL1(BATOFF),CL2'  ',AL2(0)                       
         DC    AL1(22),AL1(35),AL1(BATACC),CL2'SX',AL2(0)                       
*                                                                               
         DC    AL1(26),AL1(29),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(26),AL1(31),AL1(BATACC),CL2'SE',AL2(0)                       
         DC    AL1(26),AL1(27),AL1(BATOFF),CL2'  ',AL2(0)                       
         DC    AL1(26),AL1(28),AL1(BATOFF),CL2'  ',AL2(0)                       
*                                                                               
         DC    AL1(41),AL1(11),AL1(BATACC),CL2'1R',AL2(0)                       
*                                                                               
         DC    AL1(45),AL1(16),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(45),AL1(17),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(45),AL1(23),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(45),AL1(24),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(45),AL1(30),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(45),AL1(31),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(45),AL1(37),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(45),AL1(38),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(45),AL1(44),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(45),AL1(45),AL1(BATACC),CL2'  ',AL2(0)                       
*                                                                               
         DC    AL1(46),AL1(14),AL1(BATACC),CL2'SC',AL2(0)                       
         DC    AL1(46),AL1(16),AL1(BATACC),CL2'SV',AL2(0)                       
         DC    AL1(46),AL1(20),AL1(BATACC),CL2'SX',AL2(0)                       
*                                                                               
         DC    AL1(47),AL1(20),AL1(BATACC),CL2'SC',AL2(0)                       
         DC    AL1(47),AL1(35),AL1(BATACC),CL2'SV',AL2(0)                       
*                                                                               
         DC    AL1(49),AL1(11),AL1(BATACC),CL2'1R',AL2(0)                       
         DC    AL1(49),AL1(27),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(49),AL1(40),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(49),AL1(53),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(49),AL1(66),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(49),AL1(79),AL1(BATACC),CL2'  ',AL2(0)                       
*                                                                               
         DC    AL1(55),AL1(13),AL1(BATACC),CL2'SI',AL2(0)                       
         DC    AL1(55),AL1(29),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(55),AL1(31),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(55),AL1(27),AL1(BATOFF),CL2'  ',AL2(0)                       
         DC    AL1(55),AL1(28),AL1(BATOFF),CL2'  ',AL2(0)                       
*                                                                               
         DC    AL1(62),AL1(26),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(62),AL1(33),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(62),AL1(40),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(62),AL1(47),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(62),AL1(54),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(62),AL1(61),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(62),AL1(68),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(62),AL1(75),AL1(BATACC),CL2'  ',AL2(0)                       
         DC    AL1(62),AL1(82),AL1(BATACC),CL2'  ',AL2(0)                       
*                                                                               
         DC    X'FF'                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* PRODUCTION OPTIONS (OPTION MAINT)                                   *         
***********************************************************************         
                                                                                
POPT     DS    0D                                                               
         DC    AL1(OPNUNBWC)                                                    
         DC    AL1(L'GOUWLIST+OPDLN1Q)                                          
         DC    AL1(OPDDATA-OPDELD),AL1(EDSWC)                                   
         DC    AL1(0),AL1(0)                                                    
*                                                                               
         DC    AL1(OPNTBC)                                                      
         DC    AL1(L'GOTBC+OPDLN1Q)                                             
         DC    AL1(OPDDATA-OPDELD),AL1(EDSCM)                                   
         DC    AL1(OPDDATA+1-OPDELD),AL1(EDSAC)                                 
*                                                                               
*&&US                                                                           
         DC    AL1(OPNAWA)                                                      
         DC    AL1(L'GOAWOA+OPDLN1Q)                                            
         DC    AL1(OPDDATA-OPDELD),AL1(EDSAC)                                   
         DC    AL1(0),AL1(0)                                                    
*                                                                               
         DC    AL1(OPNAWFOF)                                                    
         DC    AL1(L'GOAWOFOF+OPDLN1Q)                                          
         DC    AL1(OPDDATA-OPDELD),AL1(EDSOF)                                   
         DC    AL1(0),AL1(0)                                                    
*                                                                               
         DC    AL1(OPNAWAOF)                                                    
         DC    AL1(L'GOAWOAOF+OPDLN1Q)                                          
         DC    AL1(OPDDATA-OPDELD),AL1(EDSOF)                                   
         DC    AL1(0),AL1(0)                                                    
*&&                                                                             
*                                                                               
         DC    AL1(OPNTXWC)                                                     
         DC    AL1(L'GOTXWC+OPDLN1Q)                                            
         DC    AL1(OPDDATA-OPDELD),AL1(EDSWC)                                   
         DC    AL1(0),AL1(0)                                                    
*                                                                               
         DC    AL1(OPNICA)                                                      
         DC    AL1(L'GOICA+OPDLN1Q)                                             
         DC    AL1(OPDDATA-OPDELD),AL1(EDSCM)                                   
         DC    AL1(OPDDATA+1-OPDELD),AL1(EDSAC)                                 
*                                                                               
*&&US                                                                           
         DC    AL1(OPNBDB)                                                      
         DC    AL1(L'GOBDB+OPDLN1Q)                                             
         DC    AL1(OPDDATA-OPDELD),AL1(EDSCM)                                   
         DC    AL1(OPDDATA+1-OPDELD),AL1(EDSAC)                                 
*                                                                               
         DC    AL1(OPNICR)                                                      
         DC    AL1(L'GOICR+OPDLN1Q)                                             
         DC    AL1(OPDDATA-OPDELD),AL1(EDSCM)                                   
         DC    AL1(OPDDATA+1-OPDELD),AL1(EDSAC)                                 
*                                                                               
         DC    AL1(OPNAGWC)                                                     
         DC    AL1(L'GOAGWC+OPDLN1Q)                                            
         DC    AL1(OPDDATA-OPDELD),AL1(EDSWC)                                   
         DC    AL1(0),AL1(0)                                                    
*&&                                                                             
*                                                                               
         DC    AL1(OPNINCAC)                                                    
         DC    AL1(L'GOINCAC+OPDLN1Q)                                           
         DC    AL1(OPDDATA-OPDELD),AL1(EDSCM)                                   
         DC    AL1(OPDDATA+1-OPDELD),AL1(EDSAC)                                 
*                                                                               
         DC    AL1(OPNEXTAC)                                                    
         DC    AL1(L'GOEXPTAC+OPDLN1Q)                                          
         DC    AL1(OPDDATA-OPDELD),AL1(EDSCM)                                   
         DC    AL1(OPDDATA+1-OPDELD),AL1(EDSAC)                                 
*                                                                               
         DC    AL1(OPNEXOAC)                                                    
         DC    AL1(L'GOEXPOAC+OPDLN1Q)                                          
         DC    AL1(OPDDATA-OPDELD),AL1(EDSCM)                                   
         DC    AL1(OPDDATA+1-OPDELD),AL1(EDSAC)                                 
*                                                                               
         DC    AL1(OPNWOTAC)                                                    
         DC    AL1(L'GOWOFTAC+OPDLN1Q)                                          
         DC    AL1(OPDDATA-OPDELD),AL1(EDSCM)                                   
         DC    AL1(OPDDATA+1-OPDELD),AL1(EDSAC)                                 
*                                                                               
         DC    AL1(OPNWOOAC)                                                    
         DC    AL1(L'GOWOFOAC+OPDLN1Q)                                          
         DC    AL1(OPDDATA-OPDELD),AL1(EDSCM)                                   
         DC    AL1(OPDDATA+1-OPDELD),AL1(EDSAC)                                 
*                                                                               
         DC    AL1(OPNSRGAC)                                                    
         DC    AL1(L'GOSRGAC+OPDLN1Q)                                           
         DC    AL1(OPDDATA-OPDELD),AL1(EDSCM)                                   
         DC    AL1(OPDDATA+1-OPDELD),AL1(EDSAC)                                 
*                                                                               
         DC    AL1(OPNDSCAC)                                                    
         DC    AL1(L'GODSCAC+OPDLN1Q)                                           
         DC    AL1(OPDDATA-OPDELD),AL1(EDSCM)                                   
         DC    AL1(OPDDATA+1-OPDELD),AL1(EDSAC)                                 
*                                                                               
*&&US                                                                           
         DC    AL1(OPNASACC)                                                    
         DC    AL1(L'GOASACC+OPDLN1Q)                                           
         DC    AL1(OPDDATA-OPDELD),AL1(EDSCM)                                   
         DC    AL1(OPDDATA+1-OPDELD),AL1(EDSAC)                                 
*                                                                               
         DC    AL1(OPNASWRK)                                                    
         DC    AL1(L'GOASWRK+OPDLN1Q)                                           
         DC    AL1(OPDDATA-OPDELD),AL1(EDSWC)                                   
         DC    AL1(0),AL1(0)                                                    
*&&                                                                             
*                                                                               
         DC    AL1(OPNINCCR)                                                    
         DC    AL1(L'GOINCCR+OPDLN1Q)                                           
         DC    AL1(OPDDATA-OPDELD),AL1(EDSCM)                                   
         DC    AL1(OPDDATA+1-OPDELD),AL1(EDSAC)                                 
*                                                                               
         DC    AL1(OPNINCDR)                                                    
         DC    AL1(L'GOINCDR+OPDLN1Q)                                           
         DC    AL1(OPDDATA-OPDELD),AL1(EDSCM)                                   
         DC    AL1(OPDDATA+1-OPDELD),AL1(EDSAC)                                 
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
ACNVD    DSECT                                                                  
       ++INCLUDE ACNVWORK                                                       
         EJECT                                                                  
       ++INCLUDE ACNVDSECT                                                      
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* ACGOBLOCK                                                                     
         PRINT OFF                                                              
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
* ACGOXBLOCK                                                                    
         PRINT OFF                                                              
GOXBLOCD DSECT                                                                  
       ++INCLUDE ACGOXBLOCK                                                     
         PRINT ON                                                               
* ACGOBBLOCK                                                                    
         PRINT OFF                                                              
GOBBLOCD DSECT                                                                  
       ++INCLUDE ACGOBBLOCK                                                     
         PRINT ON                                                               
* ACOPTEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACOPTEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACNV01    08/16/00'                                      
         END                                                                    
