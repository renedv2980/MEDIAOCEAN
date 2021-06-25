*          DATA SET PRSFM1D    AT LEVEL 105 AS OF 05/05/20                      
*PHASE T41C1DB                                                                  
*INCLUDE PPBROWSE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*               T41C1D - CLIENT2 (CL2) MAINT                                    
*                                                                               
* SMUR 10/28/19 SPEC-37314 UCOMM BILL CONTROL NUMBER                            
*                                                                               
* MZEI 10/03/19 SPEC-38401                                                      
*                                                                               
* KWAN 12/03/18 ADD OPTIONAL SPECIAL REP FIELD                                  
*                                                                               
* SMUR 01/11/17 ADD OPTIONAL EXTENDED CLIENT NAME FIELD                         
*                                                                               
* KWAN 03/11/13 BILLED BY PRUCHASE ORDER#                                       
*                                                                               
* KWAN 09/08/11 MIDAS CLIENT FOR H7 (TEST SJ AND *B)                            
*                                                                               
* SMYE 6/18/10  DELETE REFERENCE TO POWER CODES NO LONGER ON DDS SYSTEM         
*                                                                               
* BPLA 09/07    UCOMM BILLING CONTROL (IN PCLTSTAT - X'80')                     
*                                                                               
* KWAN 06/06/05 BROWSE FUNCTION                                                 
*                                                                               
* KWAN 02/10/05 NEED TO GENERATE AN AUTO P41 T/A REPORT                         
*                                                                               
* KWAN 01/20/05 ALLOW COS2 INPUT WHEN NO ESTIMATE IS ADDED                      
*                                                                               
* KWAN 01/06/04 APPLY FEILD CONTROL FEATURE FOR OPTION FIELDS                   
*                                                                               
* KWAN 11/25/02 CONVERT CLIENT FROM FIL TO SFM                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS CHG, DISP                                    *         
*                                                                     *         
*  INPUTS       SCREEN T41CC7 (CLIENT 2 MAINTENANCE)                  *         
*                                                                     *         
*  OUTPUTS                                                            *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS                                                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41C1D - CLIENT 2 (CL2) MAINT/LIST'                             
*                                                                               
T41C1D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C1D,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         ST     R3,RELO                                                         
         B      *+8                                                             
RELO     DC     F'0'                                                            
*                                                                               
         BRAS  RE,INITIALZ         INITIALIZE WORKING STORAGES                  
         CLI   MODE,NEWSCR         SCR JUST BEING LOADED?                       
         JNE   *+12                YES, DISPLAY INIT'D SCR                      
         BRAS  RE,SETSAP                                                        
         J     EXIT                                                             
*                                                                               
* FOLLOWING ACTIONS ARE INVALID (SHOULD BE CHECKED IN PRSFM00)                  
*                                                                               
         CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTREST      ACTION RESTORE?                              
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         JE    RECACERR                                                         
*                                                                               
         CLI   PFAID,0             PFKEY IS PRESSED?                            
         BE    *+12                NO                                           
         BRAS  RE,CKPFKEYS                                                      
         JNE   PFKEYERR            INVALID PFKEY IS PRESSED                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,XRECPUT        AFTER PUT                                    
         JE    PPTRS                                                            
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VK       DS    0H                  VALIDATE KEY ROUTINE                         
         LA    R2,CL2MEDH          POINT TO MEDIA FLD                           
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   CL2MEDN,MEDNM                                                    
         OI    CL2MEDNH+6,X'80'    DISPLAY MEDIA NAME                           
*                                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R6,SVKEY                                                         
         USING PCLTKEY,R6                                                       
*                                                                               
         MVC   PCLTKAGY,AGENCY                                                  
         MVC   PCLTKMED,QMED                                                    
         MVI   PCLTKRCD,X'02'      RECORD CODE FOR CLIENT                       
*                                                                               
         LA    R2,CL2CLTH          POINT TO CLIENT FLD                          
         CLI   5(R2),0                                                          
         JE    MSSNGERR                                                         
*                                                                               
         CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK24                                                             
*                                                                               
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               0,(QMED,C' CLT'),0,RR=RELO                                       
         DC    H'0'                                                             
*                                                                               
VK24     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLC   =C'ALL',8(R2)       ALL CLIENT?                                  
         JE    INVFDERR            ALL IS NOT ALLOWED AS CLIENT CODE            
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
         MVC   PCLTKCLT,QCLT                                                    
         MVC   CL2CLTN,CLTNM                                                    
         OI    CL2CLTNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO1            RECORD WILL BE READ INTO AIO1                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VR       L     R6,AIO                                                           
         MVI   ELCODE,X'55'                                                     
         GOTO1 REMELEM             REMOVE EXTENDED CLIENT ELEMENT               
*                                                                               
         LA    R2,CL2CEXNH         EXTENDED CLIENT NAME                         
         CLI   5(R2),0                                                          
         BE    VR10                                                             
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         LA    R6,ELEM                                                          
         USING PCLTXEL,R6                                                       
         MVI   PCLTXEL,PCLTXLQ     ELEM CODE X'55'                              
         MVI   PCLTXLN,PCLTXELQ    ELEM LENGTH                                  
         MVC   PCLTXNME,WORK                                                    
         GOTO1 ADDELEM             ADD X'55' ELEM TO CLT RECORD                 
         DROP  R6                                                               
*                                                                               
VR10     L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                FIRST ELEM MUST BE THERE!                    
*                                                                               
         USING PCLTELEM,R6                                                      
         MVC   WKCLSTAT,PCLTSTAT                                                
         MVC   WKFINCLT,PCLTFIN                                                 
         MVC   WKCPROF,PCLTPROF                                                 
*                                                                               
* INITIALIZE ENTIRE PROFILE TO C'0'                                             
*                                                                               
         MVI   PCLTPROF,C'0'                                                    
         MVC   PCLTPROF+1(L'PCLTPROF-1),PCLTPROF                                
*                                                                               
         LA    R2,CL2CDIVH         CLIENT DIVISION                              
         CLI   5(R2),0                                                          
         BE    VR20                                                             
         CLC   8(1,R2),WKCPROF+00  SAME INPUT AS IN RECORD?                     
         BE    VR15                                                             
         CLI   8(R2),C'0'          NO DIVISIONS REQUIRED FOR BUYING?            
         BE    VR15                                                             
         CLI   8(R2),C'1'          DIV REQUIRED FOR PRD?                        
         BE    VR15                                                             
         CLI   8(R2),C'2'          LIKE 1, PUB DRD ASSIGNMENT REQ'D?            
         BE    VR15                                                             
*                                                                               
         J     INVFDERR            OTHER OPTIONS ARE INVALID FOR NOW            
*                                                                               
VR15     MVC   PCLTPROF+00(01),8(R2)                                            
*                                                                               
VR20     LA    R2,CL2BDCOH         BILL. DATE CALC OVVERRIDE                    
         CLI   5(R2),0                                                          
         BE    VR25                                                             
         CLC   8(1,R2),WKCPROF+01  SAME INPUT AS IN RECORD?                     
         BE    VR20M                                                            
         CLI   8(R2),C'0'          USE AGENCY BILLABLE DATE FORMULA?            
         BE    VR20M                                                            
         CLI   8(R2),C'1'          OVERRIDE BILL. DATE FORMULA?                 
         BE    VR20M                                                            
*                                                                               
         J     INVFDERR            OTHER OPTIONS ARE INVALID FOR NOW            
*                                                                               
VR20M    MVC   PCLTPROF+01(01),8(R2)                                            
*                                                                               
VR25     LA    R2,CL2BDBDH         BILLABLE DATE -> BASE DATE                   
         CLI   5(R2),0                                                          
         BE    VR28                                                             
         CLC   8(1,R2),WKCPROF+02  SAME INPUT AS IN RECORD?                     
         BE    VR25M                                                            
         CLI   8(R2),C'0'          INSERTION DATE?                              
         BE    VR25M                                                            
         CLI   8(R2),C'1'          PAYABLE DATE?                                
         BE    VR25M                                                            
         CLI   8(R2),C'2'          ON-SALE DATE?                                
         BE    VR25M                                                            
         CLI   8(R2),C'3'          CLOSING DATE?                                
         BE    VR25M                                                            
*                                                                               
         J     INVFDERR            OTHER OPTIONS ARE INVALID FOR NOW            
*                                                                               
VR25M    MVC   PCLTPROF+02(01),8(R2)                                            
*                                                                               
VR28     LA    R2,CL2BDDAH         BILLABLE DATE -> DATE ADJUSTMENT             
         CLI   5(R2),0                                                          
         BE    VR30                                                             
         CLC   8(1,R2),WKCPROF+03  SAME INPUT AS IN RECORD?                     
         BE    VR28M                                                            
         CLI   8(R2),C'0'          NO ADJUSTMENT TO BASE DATE?                  
         BE    VR28M                                                            
         CLI   8(R2),C'1'          1 MONTH BEFORE BASE DATE?                    
         BE    VR28M                                                            
         CLI   8(R2),C'2'          2 MONTHS BEFORE BASE DATE?                   
         BE    VR28M                                                            
         CLI   8(R2),C'3'          3 MONTHS BEFORE BASE DATE?                   
         BE    VR28M                                                            
         CLI   8(R2),C'A'          1 MONTH AFTER BASE DATE?                     
         BE    VR28M                                                            
         CLI   8(R2),C'B'          2 MONTHS AFTER BASE DATE?                    
         BE    VR28M                                                            
         CLI   8(R2),C'C'          3 MONTHS AFTER BASE DATE?                    
         BE    VR28M                                                            
         CLI   8(R2),C'J'          5 DAYS BEFORE BASE DATE?                     
         BE    VR28M                                                            
         CLI   8(R2),C'K'          7 DAYS BEFORE BASE DATE?                     
         BE    VR28M                                                            
         CLI   8(R2),C'L'          10 DAYS BEFORE BASE DATE?                    
         BE    VR28M                                                            
         CLI   8(R2),C'M'          11 DAYS BEFORE BASE DATE?                    
         BE    VR28M                                                            
         CLI   8(R2),C'N'          14 DAYS BEFORE BASE DATE?                    
         BE    VR28M                                                            
         CLI   8(R2),C'O'          15 DAYS BEFORE BASE DATE?                    
         BE    VR28M                                                            
         CLI   8(R2),C'P'          20 DAYS BEFORE BASE DATE?                    
         BE    VR28M                                                            
         CLI   8(R2),C'Q'          25 DAYS BEFORE BASE DATE?                    
         BE    VR28M                                                            
         CLI   8(R2),C'R'          30 DAYS BEFORE BASE DATE?                    
         BE    VR28M                                                            
*                                                                               
         J     INVFDERR            OTHER OPTIONS ARE INVALID FOR NOW            
*                                                                               
VR28M    MVC   PCLTPROF+03(01),8(R2)                                            
*                                                                               
VR30     LA    R2,CL2NNADH         NON-NEWSPAPER ADJUSTMENT                     
         CLI   5(R2),0                                                          
         BE    VR32                                                             
         CLC   8(1,R2),WKCPROF+04  SAME INPUT AS IN RECORD?                     
         BE    VR30M                                                            
         CLI   8(R2),C'0'          1 MONTH LATER THAN DATE CALC'D BY            
         BE    VR30M               BILLABLE DATE OVERRIDE FORMULA?              
         CLI   8(R2),C'1'          NO ADJUSTMENT TO DATE CALC'D BY              
         BE    VR30M               BILLABLE DATE OVERRIDE FORMULA?              
*                                                                               
         J     INVFDERR            OTHER OPTIONS ARE INVALID FOR NOW            
*                                                                               
VR30M    MVC   PCLTPROF+04(01),8(R2)                                            
*                                                                               
VR32     LA    R2,CL2CLTYH         CLIENT TYPE                                  
         CLI   5(R2),0                                                          
         BE    VR34                                                             
         CLC   8(1,R2),WKCPROF+05  SAME INPUT AS IN RECORD?                     
         BE    VR32M                                                            
         CLI   8(R2),C'0'          NORMAL CLIENT?                               
         BE    VR32M                                                            
         CLI   8(R2),C'1'          MASTER CLIENT?                               
         BE    VR32M                                                            
         CLI   8(R2),C'2'          SUB CLIENT?                                  
         BE    VR32M                                                            
*                                                                               
         J     INVFDERR            OTHER OPTIONS ARE INVALID FOR NOW            
*                                                                               
VR32M    MVC   PCLTPROF+05(01),8(R2)                                            
*                                                                               
VR34     LA    R2,CL2MASCH         IF SUB, MASTER CLIENT                        
         CLI   PCLTPROF+05,C'2'    CLIENT IS SUB?                               
         BNE   *+16                                                             
         CLI   5(R2),0             MASTER CLIENT IS ENTERED?                    
         JE    MSSNGERR                                                         
         B     VR34F                                                            
         CLI   5(R2),0                                                          
         JNE   PROFERR1            CANNOT HAVE INPUT IF NOT SUB                 
         B     VR36                                                             
*                                                                               
VR34F    MVC   FULL,SPACES                                                      
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FULL(0),8(R2)       GET INPUT                                    
         CLC   FULL(3),WKCPROF+06  SAME INPUT AS IN RECORD?                     
         BE    VR34M                                                            
*                                                                               
         MVC   SVWORK(25),KEY      SAVE KEY                                     
         MVC   AIO,AIO2            POINT TO A DIFFERENT AIO                     
         XC    KEY+4(25-4),KEY+4                                                
         MVC   KEY+4(3),CL2MASC                                                 
         OC    KEY+4(3),SPACES     PADDING TRAILING SPACE(S)                    
         MVC   KEYSAVE(25),KEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         JNE   MCLNFERR            MASTER CLIENT NOT ON FILE                    
*                                                                               
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         CLI   3(RE),X'02'                                                      
         BE    *+6                                                              
         DC    H'0'                MUST BE CLIENT RECORD!                       
*                                                                               
         MVC   FULL(3),4(RE)       SAVE OFF CLIENT CODE                         
*                                                                               
         LA    RE,33(RE)           POINT TO FIRST ELEM OF CLIENT REC            
         CLI   0(RE),X'02'                                                      
         BE    *+6                                                              
         DC    H'0'                IT HAS TO BE THERE!                          
*                                                                               
* CK IF CLIENT JUST READ IS A MASTER CLIENT                                     
*                                                                               
         CLI   PCLTPROF-PCLTELEM+5(RE),C'1'                                     
         JNE   INVFDERR                                                         
*                                                                               
         MVC   KEY(25),SVWORK                                                   
         GOTO1 HIGH                RESTORE SEQUENCE                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            POINT BACK TO WORKING RECORD                 
*                                                                               
VR34M    MVC   PCLTPROF+06(03),FULL                                             
*                                                                               
VR36     LA    R2,CL2PEBFH         PRD/EST BILLING FORMULAS                     
         CLI   5(R2),0                                                          
         BE    VR38                                                             
         CLC   8(1,R2),WKCPROF+10  SAME INPUT AS IN RECORD?                     
         BE    VR36M                                                            
         CLI   8(R2),C'0'          CANNOT ADD NOR CHANGE PRD/EST BF?            
         BE    VR36M                                                            
         CLI   8(R2),C'1'          CAN ADD AND CHANGE PRD/EST BF?               
         BE    VR36M                                                            
*                                                                               
         J     INVFDERR            OTHER OPTIONS ARE INVALID FOR NOW            
*                                                                               
VR36M    MVC   PCLTPROF+10(01),8(R2)                                            
*                                                                               
VR38     LA    R2,CL2CORQH         CONTRACTS REQUIRED                           
         CLI   5(R2),0                                                          
         BE    VR40                                                             
         CLC   8(1,R2),WKCPROF+12  SAME INPUT AS IN RECORD?                     
         BE    VR38M                                                            
         CLI   8(R2),C'Y'          REQUIRE CONTRACTS?                           
         BNE   *+12                                                             
         MVI   PCLTPROF+12,C'0'    CAN BE ENTERED AS "Y", STORE AS "0"          
         B     VR40                                                             
         CLI   8(R2),C'0'          REQUIRE CONTRACTS?                           
         BE    VR38M               EXCEPT FOR SLIDING SCALE NEWSPAPERS          
         CLI   8(R2),C'N'          NO CONTRACTS REQUIRED?                       
         BE    VR38M                                                            
         CLI   8(R2),C'T'          CONTRACTS NOT REQUIRED FOR TST INS?          
         BE    VR38M                                                            
*                                                                               
         J     INVFDERR            OTHER OPTIONS ARE INVALID FOR NOW            
*                                                                               
VR38M    MVC   PCLTPROF+12(01),8(R2)                                            
*                                                                               
VR40     LA    R2,CL2BDTYH         BUDGET $ TYPE                                
         CLI   5(R2),0                                                          
         BE    VR42                                                             
         CLC   8(1,R2),WKCPROF+14  SAME INPUT AS IN RECORD?                     
         BE    VR40M                                                            
         CLI   8(R2),C'0'          BUDGET PROGRAM NOT USED?                     
         BE    VR40M                                                            
         CLI   8(R2),C'G'          GROSS $ IN BUDGET PROGRAM?                   
         BE    VR40M                                                            
         CLI   8(R2),C'N'          NET $ IN BUDGET PROGRAM?                     
         BE    VR40M                                                            
         CLI   8(R2),C'C'          COST $ IN BUDGET PROGRAM?                    
         BE    VR40M                                                            
*                                                                               
         J     INVFDERR            OTHER OPTIONS ARE INVALID FOR NOW            
*                                                                               
VR40M    MVC   PCLTPROF+14(01),8(R2)                                            
*                                                                               
VR42     LA    R2,CL2ESRDH         ESTIMATE ROUNDING                            
         CLI   5(R2),0                                                          
         BE    VR43                                                             
         CLC   8(1,R2),WKCPROF+18  SAME INPUT AS IN RECORD?                     
         BE    VR42M                                                            
         CLI   8(R2),C'0'          NO ROUNDING ON 52/PEC DETAILS OR             
         BE    VR42M               SUMMARIES?                                   
         CLI   8(R2),C'1'          ROUND TO NEAREST $ ON EST, PRD, DIV,         
         BE    VR42M               REG, AND DIST TOTALS AND SUMMARIES?          
*                                                                               
         J     INVFDERR            OTHER OPTIONS ARE INVALID FOR NOW            
*                                                                               
VR42M    MVC   PCLTPROF+18(01),8(R2)                                            
         DROP  R6                  FOR PCLTELEM                                 
*                                                                               
VR43     MVI   ELCODE,PSAPELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         CLI   SAPAGY,C'Y'                                                      
         BNE   VR44                                                             
*                                                                               
         LA    R2,CL2SAPH                                                       
         GOTO1 ANY                                                              
         MVC   ELEM,SPACES                                                      
         MVI   ELEM,PSAPELQ                                                     
         MVI   ELEM+1,12                                                        
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   ELEM+2(0),8(R2)                                                  
         GOTO1 ADDELEM                                                          
*                                                                               
VR44     CLI   SECMNOV,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR46                                                             
         CLI   SECMNOV,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR46                                                             
         MVI   ELCODE,X'41'                                                     
         GOTO1 REMELEM             REMOVE MEDIA NAME OVERRIDE ELEM              
*                                                                               
         LA    R2,CL2MNOVH         MEDIA NAME OVERRIDE                          
         CLI   5(R2),0                                                          
         BE    VR46                                                             
*                                                                               
         MVC   ELEM,SPACES         SPACES, NOT NULLS!                           
         MVI   ELEM+00,X'41'       ELEM CODE                                    
         MVI   ELEM+01,X'0C'       ELEM LENGTH                                  
         LA    R6,ELEM                                                          
         USING PCLTMEL,R6                                                       
*                                                                               
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         CHI   RE,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD LENGTH                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PCLTMNAM(0),8(R2)                                                
         GOTO1 ADDELEM             ADD X'02' ELEM TO CLT RECORD                 
         DROP  R6                                                               
*                                                                               
VR46     CLI   SECSFHG,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR48                                                             
         CLI   SECSFHG,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR48                                                             
         NI    WKCLSTAT,X'FF'-X'01'                                             
         LA    R2,CL2SFHH          SPECIAL FINANCIAL HANDLING                   
         CLI   5(R2),0                                                          
         BE    VR48                                                             
*                                                                               
         CLI   5(R2),1                                                          
         JH    INVFDERR                                                         
         CLI   8(R2),C'Y'          SHOULD IT BE ON?                             
         BNE   *+12                NO                                           
         OI    WKCLSTAT,X'01'      YES                                          
         B     VR48                                                             
         CLI   8(R2),C'N'                                                       
         JNE   INVFDERR            MUST BE "Y" OR "N"                           
*                                                                               
VR48     CLI   SECFROP,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR50                                                             
         CLI   SECFROP,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR50                                                             
         MVI   ELCODE,X'47'                                                     
         GOTO1 REMELEM             REMOVE ALL FREEZE-STATUS ELEM                
*                                                                               
* TURN OFF "FRZ" OPTIONS                                                        
*                                                                               
         NI    WKCLSTAT,X'FF'-X'12'                                             
*                                                                               
         LA    R2,CL2FROPH         FROZEN OPTIONS                               
         CLI   5(R2),0                                                          
         BE    VR50                                                             
*                                                                               
         CLC   =C'FRZ',8(R2)                                                    
         BNE   VR48D                                                            
         CLI   5(R2),3                                                          
         JNE   INVFDERR                                                         
         OI    WKCLSTAT,X'02'      INDICATES FROZEN                             
         B     VR50                                                             
VR48D    CLI   5(R2),1                                                          
         BH    VR48H               CHECK DATE ENTRY                             
*                                                                               
         CLI   8(R2),C'Y'          SHOULD "FRZ" OPTION BE ON?                   
         BE    VR48U               YES                                          
         CLI   8(R2),C'N'                                                       
         JNE   INVFDERR            MUST BE 'Y' OR 'N'                           
         B     VR50                                                             
*                                                                               
VR48H    DS    0H                  TEST FOR VALID DATE                          
         CLI   5(R2),3                                                          
         JL    INVFDERR            TOO SHORT FOR M/Y                            
         CLI   5(R2),7                                                          
         JH    INVFDERR            TOO LONG  FOR M/Y (AND "+" OR "-")           
*                                                                               
         MVI   BYTE,X'02'                                                       
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LA    RE,8(R2)                                                         
         AHI   R1,-1                                                            
         AR    RE,R1               RE POINTING TO LAST CHAR IN INPUT            
         CLI   0(RE),C'+'          PLUS  (+) ?                                  
         BNE   *+8                 NO                                           
         MVI   BYTE,X'08'                                                       
         CLI   0(RE),C'-'          MINUS (+) ?                                  
         BNE   *+8                 NO                                           
         MVI   BYTE,X'04'                                                       
         CLI   BYTE,X'02'          PLUS OR MINUS FOUND ?                        
         BE    *+8                 NO                                           
         MVI   0(RE),X'00'         YES - CLEAR IT FROM END OF INPUT             
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         JZ    INVDTERR            INVALID DATE                                 
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM+0,X'47'        ELEM CODE                                    
         MVI   ELEM+1,X'08'        ELEM LENGTH                                  
         LA    R6,ELEM                                                          
         USING PCLTFEL,R6                                                       
*                                                                               
* WORK HAS YYMMDD FROM DATVAL                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
*                                                                               
         MVC   PCLTFDTE,WORK+10    MOVE ONLY YM TO ELEMENT                      
         OC    PCLTFIND,BYTE       X'08'(+) OR X'04'(-) OR X'02'                
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
         OI    WKCLSTAT,X'12'      INDICATES FROZEN WITH DATE                   
*                                                                               
VR48U    OI    WKCLSTAT,X'02'      INDICATES FROZEN                             
*                                                                               
VR50     CLI   SECRLPG,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR52                                                             
         CLI   SECRLPG,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR52                                                             
         MVI   ELCODE,X'46'                                                     
         GOTO1 REMELEM             REMOVE ALL T/A RLP ELEM                      
*                                                                               
         LA    R2,CL2RLPGH         RLP GROUP                                    
         CLI   5(R2),0                                                          
         BE    VR52                                                             
*                                                                               
         CLC   AGENCY,=C'SJ'                                                    
         JNE   INVFDERR            ALLOW ONLY SJR FOR TESTING                   
*                                                                               
         CLC   SVAGPINI,=X'0000'   EXISTING RLP ID NUMBER?                      
         JNH   INVFDERR            NO                                           
*                                                                               
         CLI   5(R2),2                                                          
         JL    INVFDERR                                                         
         CLI   5(R2),8                                                          
         JH    INVFDERR                                                         
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LA    RE,8(R2)                                                         
*                                                                               
VR50H    CLI   0(RE),C' '          BLANK?                                       
         JL    INVFDERR                                                         
         CLI   0(RE),C'9'                                                       
         JH    INVFDERR                                                         
         LA    RE,1(RE)                                                         
         BCT   R1,VR50H                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM+0,X'46'        ELEM CODE                                    
         MVI   ELEM+1,X'0B'        ELEM LENGTH                                  
         LA    R6,ELEM                                                          
         USING PCLTTAEL,R6                                                      
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PCLTTAGRP(0),8(R2)                                               
         OC    PCLTTAGRP,SPACES                                                 
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     R1,DMCB             GET SYS NUM TO SWITCH BACK TO                
         USING FACTSD,R1                                                        
         MVC   SYSSW,FASYS         CONNECTED SYSTEM                             
         DROP  R1                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'0A'                                                       
         GOTO1 (RF),DMCB           SWITCH TO CONTROL SYSTEM                     
*                                                                               
         LA    R4,KEY                                                           
         USING GRPKEYD,R4                                                       
*                                                                               
         MVC   SVWORK(25),KEY      SAVE KEY                                     
         XC    KEY,KEY                                                          
         MVC   SVWORK(2),=X'002F'                                               
         MVI   GRPKSYST,C'P'                                                    
         MVC   GRPKAGY,AGENCY                                                   
         MVC   GRPKUSER,SVAGPINI                                                
         MVC   GRPKGRP,PCLTTAGRP                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SYSSW                                                    
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 (RF),DMCB,,0        SWITCH BACK TO CURRENT SYSTEM                
*                                                                               
         CLC   KEYSAVE(15),KEY                                                  
         JNE   INVFDERR                                                         
*                                                                               
         GOTO1 ADDELEM                                                          
         DROP  R4,R6                                                            
*                                                                               
         MVC   KEY(25),SVWORK                                                   
         GOTO1 HIGH                RESTORE SEQUENCE                             
*                                                                               
VR52     CLI   SECCOS2,C'N'        AUTHORIZED TO CHANGE COS2?                   
         JE    VR54                                                             
         CLI   SECCOS2,C'Y'        READ-ONLY ACCESS?                            
         JE    VR54                                                             
         MVI   COS2FLAG,0          INIT COST2 FACTOR FLAG                       
         TM    WKCLSTAT,X'04'      COS2=Y?                                      
         BNO   *+12                                                             
         OI    COS2FLAG,X'80'      TURN ON COST2 FACTOR FLAG                    
         B     VR52C                                                            
         TM    WKCLSTAT,X'08'      COS2=9.999999?                               
         BNO   *+8                                                              
         OI    COS2FLAG,X'80'      TURN ON COST2 FACTOR FLAG                    
*                                                                               
VR52C    L     R6,AIO                                                           
         MVI   ELCODE,X'45'        SEARCH FOR COST2 FACTOR ELEM                 
         BRAS  RE,GETEL                                                         
         BNE   VR52F                                                            
         OI    COS2FLAG,X'80'      COST2 FACTOR ELEM EXIST                      
         GOTO1 REMELEM             REMOVE ALL COST2 FACTOR ELEM                 
*                                                                               
* CLEAR COS2 STATUS BITS                                                        
*                                                                               
VR52F    NI    WKCLSTAT,X'FF'-X'08'-X'04'                                       
*                                                                               
         LA    R2,CL2COS2H         COST 2                                       
         CLI   5(R2),0                                                          
         BNE   VR52H                                                            
         TM    COS2FLAG,X'80'      COST2 FACTOR DATA PRESENT?                   
         JZ    VR54                NO                                           
         J     MSSNGERR            CANNOT REMOVE COS2 DATA!                     
*                                                                               
VR52H    CLI   WKFINCLT,C'Y'       FINANCIAL CLIENT?                            
         JE    INVFDERR            COS2 IS NOT ALLOWED FOR FIN CLT              
*                                                                               
         CLI   F0PROF+4,C'$'       PROFILE ALLOW COS2=Y?                        
         BE    VR52K                                                            
         CLI   F0PROF+4,C'F'       PROFILE ALLOW COS2=9.9999999?                
         BE    VR52P                                                            
         J     INVFDERR            PROFILE DOES NOT ALLOW COS2 INPUT            
*                                                                               
VR52K    CLI   5(R2),1                                                          
         BNE   *+16                                                             
         CLI   8(R2),C'Y'          COS2=Y?                                      
         JNE   INVFDERR                                                         
         B     VR52M                                                            
         CLI   5(R2),3                                                          
         JNE   INVFDERR                                                         
         CLC   8(3,R2),=C'YES'     COS2=YES?                                    
         JNE   INVFDERR                                                         
*                                                                               
* ON CHANGE, IF COS2 ELEM OR BIT IS NOT PRESENT, THEN CANNOT ADD ONE            
* UNLESS THERE IS NO ESTIMATE RECORD UNDER CURRENT CLIENT                       
*                                                                               
VR52M    TM    COS2FLAG,X'80'      COS2 FACTOR ELEM PRESENT?                    
         JNZ   *+12                                                             
         BRAS  RE,CKESTREC         HAVE ESTIMATE RECORD ALREADY?                
         JNE   INVFDERR                                                         
*                                                                               
         OI    WKCLSTAT,X'04'                                                   
         NI    WKCLSTAT,X'FF'-X'08'                                             
*                                                                               
         B     VR54                DONE WITH COS2=Y                             
*                                                                               
* ON CHANGE, IF COS2 ELEM OR BIT IS NOT PRESENT, THEN CANNOT ADD ONE            
* UNLESS THERE IS NO ESTIMATE RECORD UNDER CURRENT CLIENT                       
*                                                                               
VR52P    TM    COS2FLAG,X'80'      COS2 FACTOR ELEM PRESENT?                    
         JNZ   *+12                                                             
         BRAS  RE,CKESTREC         HAVE ESTIMATE RECORD ALREADY?                
         JNE   INVFDERR                                                         
*                                                                               
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(6,8(R2)),(R4)                                      
         CLI   DMCB,0                                                           
         JNE   INVFDERR                                                         
*                                                                               
         L     R6,4(R1)                                                         
         C     R6,=F'9999999'                                                   
         JH    INVFDERR            MAX INPUT IS 9.999999                        
         CHI   R6,0                                                             
         JNH   INVFDERR            NEGATIVE OR ZERO IS NO GOOD                  
*                                                                               
         CVD   R6,DUB                                                           
         OI    WKCLSTAT,X'08'                                                   
         NI    WKCLSTAT,X'FF'-X'04'                                             
*                                                                               
         L     R6,AIO                                                           
         USING PCLTKEY,R6                                                       
         CLC   PCLTLEN,=H'500'     CK FOR MAX REC LENGTH                        
         JH    MAXLNERR            MAX REC LENGHT ERROR                         
         DROP  R6                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM+0,X'45'        ELEM CODE                                    
         MVI   ELEM+1,X'07'        ELEM LENGTH                                  
         LA    R6,ELEM                                                          
         USING PCLTCFEL,R6                                                      
         MVC   PCLTCF,DUB+3        COST2 FACTOR (PACKED)                        
         GOTO1 ADDELEM                                                          
         DROP  R6                  DONE WITH COS2=9.9999999                     
*                                                                               
VR54     DS    0H                  UCOMM BILL CONTROL                           
         CLI   SECUCBC,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR56                                                             
         CLI   SECUCBC,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR56                                                             
         NI    WKCLSTAT,X'FF'-X'80'                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'85'                                                     
         GOTO1 REMELEM             REMOVE EXTENDED CLIENT ELEMENT               
*                                                                               
         LA    R2,CL2UCBCH                                                      
         CLI   5(R2),0         ANY INPUT?                                       
         BE    VR56                                                             
*                                                                               
         CLI   8(R2),C'N'                                                       
         BE    VR56                                                             
*                                                                               
         OI    WKCLSTAT,X'80'      PRESET UCOMM FLAG                            
*                                                                               
         CLI   8(R2),C'Y'                                                       
         BE    VR56                                                             
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         NI    WKCLSTAT,X'FF'-X'80'                                             
         CLI   WORK,C'1'           VALID UCOMM 1-4                              
         JL    INVFDERR                                                         
         CLI   WORK,C'4'                                                        
         JH    INVFDERR                                                         
         PACK  DUB,WORK(1)         NUMBER OF UCOMMS                             
         CVB   R0,DUB                                                           
*                                                                               
         LA    R6,ELEM                                                          
         USING PCLTUEL,R6                                                       
         MVI   PCLTUEL,PCLTULQ     ELEM CODE X'85'                              
         MVI   PCLTULN,PCLTUELQ    ELEM LENGTH                                  
         STC   R0,PCLTUCOM         NUMBER OF UCOMMS                             
         GOTO1 ADDELEM             ADD X'85' ELEM TO CLT RECORD                 
         DROP  R6                                                               
*                                                                               
         OI    WKCLSTAT,X'80'                                                   
*                                                                               
VR56     DS    0H                  MIDAS CLIENT                                 
         LA    R2,CL2MIDCH                                                      
         CLC   AGENCY,=C'SJ'                                                    
         JE    VR56H                                                            
         CLC   AGENCY,=C'*B'                                                    
         JE    VR56H                                                            
         CLC   AGENCY,=C'H7'                                                    
         JE    VR56H                                                            
         CLC   AGENCY,=C'ST'       SHERWOORD TRADING? ADDED FEB28/2017          
         JE    VR56H                                                            
         J     VR56X                                                            
VR56H    TM    WKCLSTAT,X'40'      ALREADY MIDAS CLIENT?                        
         JZ    VR56M                                                            
         CLI   5(R2),0             ANY INPUT?                                   
         JE    VR56K                                                            
         CLI   8(R2),C'N'          NO - MIDAS CLIENT?                           
         JE    VR56K                                                            
         CLI   8(R2),C'Y'          STILL YES?                                   
         JE    VR56X                                                            
         J     INVFDERR                                                         
VR56K    BRAS  RE,CKESTREC         HAVE ESTIMATE RECORD ALREADY?                
         JNE   CCFLDERR                                                         
         NI    WKCLSTAT,X'FF'-X'40'                                             
         J     VR56X                                                            
VR56M    CLI   5(R2),0             ANY INPUT?                                   
         JE    VR56X                                                            
         CLI   8(R2),C'N'          NO - MIDAS CLIENT?                           
         JE    VR56X                                                            
         CLI   8(R2),C'Y'          YES - MIDAS CLIENT?                          
         JNE   INVFDERR                                                         
         CLI   F0PROF+4,C'$'       COS2 $ CLIENT?                               
         JNE   INVFDERR                                                         
         BRAS  RE,CKESTREC         HAVE ESTIMATE RECORD ALREADY?                
         JNE   CCFLDERR                                                         
         OI    WKCLSTAT,X'40'      SET MIDAS CLIENT BIT                         
VR56X    DS    0H                  END OF MIDAS CLIENT VALIDATION               
*                                                                               
         BRAS  RE,CKBILPO#         VALIDATE BILLED BY PURCHASE ORDER#           
         JE    VR58                                                             
         CLI   WKERRNUM,68         INVALID DATE FORMAT ERROR?                   
         JE    INVDTERR                                                         
         CLI   WKERRNUM,79         FIELD CANNOT BE CHANGED ERROR?               
         JE    CCFLDERR                                                         
         CLI   WKERRNUM,MISSING    FIELD MISSING ERROR?                         
         JE    MSSNGERR                                                         
         J     INVFDERR            DEFAULT TO INVALID FIELD ERROR               
*                                                                               
VR58     BRAS  RE,CKSREPCD         CHECK FOR SPECIAL REP CODE                   
         JE    *+14                                                             
         MVC   ERROR,WKERRNUM      ERROR NUMBER                                 
         J     TRAPERR                                                          
*                                                                               
VR62     DS    0H                                                               
*                                                                               
VRLAST   DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                FIRST ELEM MUST BE THERE!                    
         USING PCLTELEM,R6                                                      
         MVC   PCLTSTAT,WKCLSTAT   PUT BACK UPDATED CLT STATUS                  
         DROP  R6                                                               
*                                                                               
VRX      DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       DS    0H                  DISPLAY RECORD                               
         MVC   CL2CEXN,SPACES      CLEAR EXTENDED CLIENT NAME FIELD             
         OI    CL2CEXNH+6,X'80'    TRANSMIT                                     
         L     R6,AIO                                                           
         MVI   ELCODE,X'55'        EXTENDED CLIENT NAME ELEM                    
         BRAS  RE,GETEL                                                         
         JNE   DR01                                                             
         USING PCLTXEL,R6                                                       
         MVC   CL2CEXN,PCLTXNME                                                 
         DROP  R6                                                               
*                                                                               
DR01     L     R6,AIO                                                           
         CLI   3(R6),X'02'         CLIENT RECORD CODE?                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PCLTELEM,R6                                                      
         MVI   ELCODE,X'02'        FIRST CLIENT ELEM CODE                       
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WKCLSTAT,PCLTSTAT                                                
*                                                                               
         XC    CL2CDIV,CL2CDIV                                                  
         MVC   CL2CDIV,PCLTPROF+00                                              
         OI    CL2CDIVH+6,X'80'    CLIENT DIVISION                              
*                                                                               
         XC    CL2BDCO,CL2BDCO                                                  
         MVC   CL2BDCO,PCLTPROF+01                                              
         OI    CL2BDCOH+6,X'80'    BILL. DATE CALC. OVERRIDE                    
*                                                                               
         XC    CL2BDBD,CL2BDBD                                                  
         MVC   CL2BDBD,PCLTPROF+02                                              
         OI    CL2BDBDH+6,X'80'    BILL. DATE -> BASE DATE                      
*                                                                               
         XC    CL2BDDA,CL2BDDA                                                  
         MVC   CL2BDDA,PCLTPROF+03                                              
         OI    CL2BDDAH+6,X'80'    BILL. DATE -> DATE ADJ.                      
*                                                                               
         XC    CL2NNAD,CL2NNAD                                                  
         MVC   CL2NNAD,PCLTPROF+04                                              
         OI    CL2NNADH+6,X'80'    NON-NEWSPAPER ADJUSTMENT                     
*                                                                               
         XC    CL2CLTY,CL2CLTY                                                  
         MVC   CL2CLTY,PCLTPROF+05                                              
         OI    CL2CLTYH+6,X'80'    CLIENT TYPE                                  
*                                                                               
         XC    CL2MASC,CL2MASC                                                  
         CLC   =C'000',PCLTPROF+06 NO MASTER CLIENT PRESENT?                    
         BE    *+10                                                             
         MVC   CL2MASC,PCLTPROF+06                                              
         OI    CL2MASCH+6,X'80'    IF SUB, MASTER CLIENT (POS 7,8,9)            
*                                                                               
         MVC   CL2PEBF,PCLTPROF+10                                              
         OI    CL2PEBFH+6,X'80'    PRD/EST BILLING FORMULAS                     
*                                                                               
         XC    CL2CORQ,CL2CORQ                                                  
         MVC   CL2CORQ,PCLTPROF+12                                              
         CLI   CL2CORQ,C'0'                                                     
         BNE   *+8                                                              
         MVI   CL2CORQ,C'Y'        DISPLAY IT AS "Y"                            
         OI    CL2CORQH+6,X'80'    CONTRACTS REQUIRED                           
*                                                                               
         XC    CL2BDTY,CL2BDTY                                                  
         MVC   CL2BDTY,PCLTPROF+14                                              
         OI    CL2BDTYH+6,X'80'    BUDGET $ TYPE                                
*                                                                               
         XC    CL2ESRD,CL2ESRD                                                  
         MVC   CL2ESRD,PCLTPROF+18                                              
         OI    CL2ESRDH+6,X'80'    ESTIMATE ROUNDING                            
*                                                                               
DR2      XC    CL2SFH,CL2SFH                                                    
         CLI   SECSFHG,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    DR20X                                                            
         TM    PCLTSTAT,X'01'      SFH CLIENT?                                  
         BO    DR20H                                                            
*                                                                               
* DO NOT DISPLAY DEFAULT VALUE FOR WESTERN AGENCY                               
*                                                                               
         CLC   AGENCY,=C'WI'                                                    
         BE    DR20X                                                            
         CLC   AGENCY,=C'WJ'                                                    
         BE    DR20X                                                            
         CLC   AGENCY,=C'WT'                                                    
         BE    DR20X                                                            
         CLC   AGENCY,=C'WR'                                                    
         BE    DR20X                                                            
         MVI   CL2SFH,C'N'         DEFAULT SFH                                  
         B     DR20X                                                            
*                                                                               
DR20H    MVI   CL2SFH,C'Y'         SPECIAL FINANCIAL HANDLING                   
DR20X    OI    CL2SFHH+6,X'80'                                                  
*                                                                               
DR22     XC    CL2MIDC,CL2MIDC                                                  
         CLC   AGENCY,=C'SJ'                                                    
         JE    DR22H                                                            
         CLC   AGENCY,=C'*B'                                                    
         JE    DR22H                                                            
         CLC   AGENCY,=C'H7'                                                    
         JE    DR22H                                                            
         CLC   AGENCY,=C'ST'       SHERWOORD TRADING? ADDED FEB28/2017          
         JE    DR22H                                                            
         J     DR22X                                                            
DR22H    TM    PCLTSTAT,X'40'      MIDAS CLIENT?                                
         JZ    DR22X                                                            
         MVI   CL2MIDC,C'Y'        YES - MIDAS CLIENT                           
DR22X    OI    CL2MIDCH+6,X'80'                                                 
*                                                                               
DR25     XC    CL2FROP,CL2FROP                                                  
         CLI   SECFROP,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    DR25X                                                            
         TM    PCLTSTAT,X'02'      FROZEN?                                      
         BZ    DR25X               NO                                           
         MVC   CL2FROP(3),=C'FRZ'  FROZEN                                       
         TM    PCLTSTAT,X'10'      FROZEN WITH DATE?                            
         BZ    DR25X               NO                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'47'        FREEZE STATUS ELEM                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         USING PCLTFEL,R6                                                       
         MVI   BYTE,C' '                                                        
         MVC   WORK(L'PCLTFDTE),PCLTFDTE                                        
         MVI   WORK+2,X'01'        DAY                                          
         TM    PCLTFIND,X'08'      + INDICATED?                                 
         BZ    *+8                 NO                                           
         MVI   BYTE,C'+'                                                        
         TM    PCLTFIND,X'04'      - INDICATED?                                 
         BZ    *+8                 NO                                           
         MVI   BYTE,C'-'                                                        
         DROP  R6                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(6,CL2FROP)                                 
*                                                                               
         CLI   BYTE,C' '           ANYTHING IN BYTE?                            
         BNH   *+10                NO                                           
         MVC   CL2FROP+6(1),BYTE                                                
*                                                                               
DR25X    OI    CL2FROPH+6,X'80'                                                 
*                                                                               
         XC    CL2SAP,CL2SAP                                                    
         OI    CL2SAPH+6,X'80'                                                  
*                                                                               
         CLI   SAPAGY,C'Y'                                                      
         BNE   DR30                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'51'        SAP ELEM                                     
         BRAS  RE,GETEL                                                         
         JNE   DR30                                                             
         USING PSAPEL,R6                                                        
         MVC   CL2SAP,PSAPCODE                                                  
         DROP  R6                                                               
*                                                                               
DR30     XC    CL2MNOV,CL2MNOV                                                  
         CLI   SECMNOV,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    DR30X                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'41'        MEDIA NAME OVERRIDE ELEM                     
         BRAS  RE,GETEL                                                         
         BNE   DR30X                                                            
         USING PCLTMEL,R6                                                       
         MVC   CL2MNOV,PCLTMNAM    MEDIA NAME OVERRIDE                          
*                                                                               
DR30X    OI    CL2MNOVH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DR35     XC    CL2COS2,CL2COS2                                                  
         CLI   SECCOS2,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    DR35X                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'45'        COST2 FACTOR ELEM                            
         BRAS  RE,GETEL                                                         
         BE    DR35F                                                            
         TM    WKCLSTAT,X'04'      COST 2 $?                                    
         BZ    DR35X                                                            
         MVI   CL2COS2,C'Y'                                                     
         B     DR35X                                                            
*                                                                               
         USING PCLTCFEL,R6                                                      
DR35F    TM    WKCLSTAT,X'08'      COST 2 FACTOR?                               
         BO    *+6                                                              
         DC    H'0'                IMPOSSIBLE                                   
*                                                                               
         CP    PCLTCF,=P'0'        COST2 FACTOR IS ZERO?                        
         BNE   *+6                                                              
         DC    H'0'                IMPOSSIBLE                                   
*                                                                               
         LA    R1,CL2COS2          POINT TO FLD                                 
         UNPK  WORK(10),PCLTCF                                                  
         OI    WORK+9,X'F0'                                                     
         LA    RE,WORK                                                          
         MVC   0(1,R1),3(RE)       SKIP FIRST 3 DIGITS                          
         LA    R1,1(R1)                                                         
         LA    RE,4(RE)                                                         
         MVI   0(R1),C'.'          DECIMAL POINT                                
         LA    R1,1(R1)                                                         
         LA    RF,6                SIX DIGITS AFTER DECIMAL PT                  
         MVC   0(1,R1),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,*-14                                                          
*                                                                               
         LA    RF,8                MAX LOOP IS 8 TIMES                          
DR35K    BCTR  R1,0                                                             
         CLI   0(R1),C'.'          DECIMAL POINT?                               
         BE    DR35M                                                            
*                                                                               
         CLI   0(R1),C'0'          TRAILING ZERO?                               
         BNE   DR35X                                                            
         MVI   0(R1),X'00'         CLEAR TRAILING ZERO                          
         BCT   RF,DR35K                                                         
*                                                                               
         DC    H'0'                SOMETHING IS WRONG...                        
*                                                                               
DR35M    AHI   R1,1                                                             
         MVI   0(R1),C'0'          NON-SIGNIFICANT ZERO                         
*                                                                               
DR35X    OI    CL2COS2H+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DR36     DS    0H                  UCOMM BILLING CONTROL                        
         XC    CL2UCBC,CL2UCBC                                                  
         CLI   SECUCBC,C'N'        NOT AUTHORIZED (FIELD SECURITY)              
         BE    DR36X                                                            
         MVI   CL2UCBC,C'N'        DEFAULT                                      
         TM    WKCLSTAT,X'80'                                                   
         BNO   DR36X                                                            
         MVI   CL2UCBC,C'Y'        PRESET TO Y                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'85'        UCOMM BILL CONTROL NUMBER                    
         BRAS  RE,GETEL                                                         
         JNE   DR36X                                                            
         USING PCLTUEL,R6                                                       
         LLC   R0,PCLTUCOM                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CL2UCBC,DUB                                                      
         DROP  R6                                                               
*                                                                               
DR36X    OI    CL2UCBCH+6,X'80'                                                 
*                                                                               
DR40     XC    CL2RLPG,CL2RLPG                                                  
         CLI   SECRLPG,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    DR40X                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'46'        T/A RLP GROUP ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   DR45                                                             
         USING PCLTTAEL,R6                                                      
         MVC   CL2RLPG(L'PCLTTAEL),PCLTTAEL                                     
*                                                                               
DR40X    OI    CL2RLPGH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DR45     XC    CL2BPO#,CL2BPO#     CLEAR BILLED ON PO#                          
         XC    CL2PEDT,CL2PEDT     CLEAR PO EFFECTIVE DATE                      
         XC    CL2POLV,CL2POLV     CLEAR PO LEVEL                               
         XC    CL2LBOV,CL2LBOV     CLEAR PO LABEL OVERRIDE                      
         CLI   SECPOBC,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         JE    DR45X                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,PCLTPOEQ                                                  
         BRAS  RE,GETEL                                                         
         JNE   DR45X                                                            
         USING PCLTPOEL,R6                                                      
         MVI   CL2BPO#,C'N'                                                     
         TM    PCLTPOF1,P_POBILQ                                                
         JZ    *+8                                                              
         MVI   CL2BPO#,C'Y'                                                     
         OC    PCLTPOED,PCLTPOED   HAVE PO EFFECTIVE DATE?                      
         JZ    DR45H                                                            
         GOTO1 DATCON,DMCB,(3,PCLTPOED),(14,CL2PEDT)                            
DR45H    MVC   CL2POLV,PCLTPOLV                                                 
         TM    PCLTPOF1,P_POLOVQ   HAVE PO LABEL OVERRIDE?                      
         JZ    DR45X                                                            
         MVC   CL2LBOV,PCLTPONM                                                 
         DROP  R6                                                               
DR45X    OI    CL2BPO#H+6,X'80'                                                 
         OI    CL2PEDTH+6,X'80'                                                 
         OI    CL2POLVH+6,X'80'                                                 
         OI    CL2LBOVH+6,X'80'                                                 
*                                                                               
         BRAS  RE,DISPSREP         DISPLAY SPECIAL REP                          
*                                                                               
DR46     DS    0H                  FOR FUTURE FLDS                              
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       DS    0H                  DISPLAY KEY                                  
         L     R6,AIO                                                           
         CLI   3(R6),X'02'         CLIENT RECORD CODE?                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PCLTKEY,R6                                                       
         MVC   CL2MED,PCLTKMED                                                  
         MVC   CL2CLT,PCLTKCLT                                                  
         DROP  R6                                                               
*                                                                               
         MVI   USEIONUM,2          USE AIO2 FOR MED/CLT/PRD/EST RECS            
*                                                                               
         OI    CL2MEDH+6,X'80'     TRANSMIT MEDIA CODE                          
         XC    CL2MEDN,CL2MEDN                                                  
         OI    CL2MEDNH+6,X'80'    CLEARED MEDIA NAME                           
         MVI   CL2MEDH+5,1         INPUT LENGTH                                 
         LA    R2,CL2MEDH                                                       
         GOTO1 VALIMED                                                          
         MVC   CL2MEDN,MEDNM                                                    
         OI    CL2MEDNH+6,X'80'    TRANSMIT MEDIA NAME                          
*                                                                               
         OI    CL2CLTH+6,X'80'     TRANSMIT CLIENT CODE                         
         XC    CL2CLTN,CL2CLTN                                                  
         OI    CL2CLTNH+6,X'80'    CLEARED CLIENT NAME                          
         MVI   CL2CLTH+5,3         INPUT LENGTH                                 
         LA    R2,CL2CLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   CL2CLTN,CLTNM                                                    
         OI    CL2CLTNH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
DKX      DS    0H                                                               
         MVC   AIO,AIO1            AIO1 HAS REC TO BE DISPLAYED                 
         MVI   USEIONUM,1          RESET TO AIO1                                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPTRS    DS    0H                  REC IS JUST CHANGED                          
*                                                                               
         BRAS  RE,PUTREQRC         TO REQUEST AUTO T/A REPORTS                  
         JE    PPTRS_X                                                          
         LHI   R2,65               CANNOT GENERATE REQ FOR T/A REPORT           
         BRAS  RE,GET_ITXT                                                      
         LA    R2,CONACTH                                                       
         J     TRAPERR2            ERROR OR COMPLETION MSG IS SET               
*                                                                               
PPTRS_X  B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
TRAPERR2 GOTO1 ERREX2                                                           
*                                                                               
MSSNGERR MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
*                                                                               
INVFDERR MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
*                                                                               
RECACERR MVI   ERROR,INVRCACT      INVALID RECORD ACTION ERROR                  
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
CCFLDERR MVI   ERROR,79            FIELD CANNOT BE CHANGED                      
         J     TRAPERR                                                          
*                                                                               
CLTRQERR MVI   ERROR,85            SPECIFIC CLT REQUIRED (SECURITY)             
         J     TRAPERR                                                          
*                                                                               
CLTACERR MVI   ERROR,89            CLIENT LIMIT ACCESS ERROR                    
         J     TRAPERR                                                          
*                                                                               
RECNFERR MVI   ERROR,53            RECORD NOT FOUND                             
         J     TRAPERR                                                          
*                                                                               
INVCLERR MVI   ERROR,62            INVALID CLIENT                               
         J     TRAPERR                                                          
*                                                                               
INVDTERR MVI   ERROR,68            INVALID DATE FORMAT                          
         J     TRAPERR                                                          
*                                                                               
MAXLNERR MVI   ERROR,90            MAXIMUM RECORD SIZE EXCEEDED                 
         J     TRAPERR                                                          
*                                                                               
PROFERR1 MVI   ERROR,96            MASTER CLT (7-9) MUST BE 000                 
         J     TRAPERR                                                          
*                                                                               
MCLNFERR MVI   ERROR,94            MASTER CLT NOT FOUND                         
         J     TRAPERR                                                          
*                                                                               
PFKEYERR MVI   ERROR,88            INVALID PFKEY                                
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITIALZ NTR1  BASE=*,LABEL=*      INITIALIZE WORKING STORAGES                  
*                                                                               
         CLI   TRANSSW,C'Y'        TRANSFERRED INTO PROGRAM?                    
         BNE   INITI50                                                          
         CLI   PFAID,0             PF KEY PRESSED?                              
         BE    INITI50             NO                                           
*                                                                               
         OC    KEY(25),KEY         HAVE KEY?                                    
         BZ    INITI50                                                          
         LA    RE,KEY                                                           
         CLI   3(RE),X'02'         CLIENT RECORD CODE?                          
         BNE   INITI50                                                          
         USING PCLTKEY,RE                                                       
         LA    R2,CL2MEDH          MEDIA FLD ON MAINT SCR                       
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTMEDH          POINT TO MEDIA FLD ON LIST SCR               
         MVC   8(1,R2),PCLTKMED                                                 
         MVI   5(R2),1             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,CL2CLTH          CLIENT FLD ON MAINT SCR                      
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTCLTH          POINT TO CLIENT FLD ON LIST SCR              
         MVC   8(3,R2),PCLTKCLT                                                 
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         DROP  RE                                                               
*                                                                               
INITI50  MVI   ACTELOPT,C'N'       NO ACTIVITY ELEM WILL BE ADDED               
*                                                                               
         XC    SECVALS(SECVALSL),SECVALS                                        
*                                                                               
         L     RF,ATWA                                                          
         OC    4(2,RF),4(RF)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RF),6(RF)       TEST ANY LIMIT ACCESS                        
         BZ    INITI70                                                          
*                                                                               
         LA    R2,SECFLDS          R2=A(FIELD SECURITY DISPLACEMENTS)           
         LHI   R0,SECFLDSN         R0=N'SECURITY FIELDS                         
INITI60  GOTOR SECRET,DMCB,('SECPFLDP',ASECBLK),1(R2)                           
         BE    INITI60H                                                         
         LA    RF,C'Y'             C'Y'=READ ONLY                               
         BH    *+8                                                              
         LA    RF,C'N'             C'N'=NO ACCESS                               
         SR    RE,RE                                                            
         IC    RE,0(R2)            RE=DISPLACEMENT TO SECURITY VALUE            
         LA    RE,SECVALS(RE)                                                   
         STC   RF,0(RE)            YES - DISPLAY AND CHANGE                     
INITI60H AHI   R2,L'SECFLDS        BUMP TO NEXT DISPLACEMENT                    
         BCT   R0,INITI60          DO FOR NUMBER OF SECURITY FIELDS             
*                                                                               
         LA    RF,SECMNOV                                                       
         LA    R2,CL2MNTLH                                                      
         BRAS  RE,PRCFLD                                                        
*                                                                               
         LA    RF,SECSFHG                                                       
         LA    R2,CL2SFTLH                                                      
         BRAS  RE,PRCFLD                                                        
*                                                                               
         LA    RF,SECFROP                                                       
         LA    R2,CL2FOTLH                                                      
         BRAS  RE,PRCFLD                                                        
*                                                                               
         LA    RF,SECRLPG                                                       
         LA    R2,CL2RGTLH                                                      
         BRAS  RE,PRCFLD                                                        
*                                                                               
         LA    RF,SECCOS2                                                       
         LA    R2,CL2C2TLH                                                      
         BRAS  RE,PRCFLD                                                        
*                                                                               
         LA    RF,SECUCBC                                                       
         LA    R2,CL2UCTLH                                                      
         BRAS  RE,PRCFLD                                                        
*                                                                               
         LA    RF,SECPOBC                                                       
         LA    R2,CL2BPTLH         TITLE FIELD - BILLED BY PO                   
         BRAS  RE,PRCFLD                                                        
         LA    RF,SECPOBC                                                       
         LA    R2,CL2EDTLH         TITLE FIELD - EFFECTIVE DATE                 
         BRAS  RE,PRCFLD                                                        
         LA    RF,SECPOBC                                                       
         LA    R2,CL2LVTLH         TITLE FIELD - PO LEVEL                       
         BRAS  RE,PRCFLD                                                        
         LA    RF,SECPOBC                                                       
         LA    R2,CL2LOTLH         TITLE FIELD - PO LABEL OVERRIDE              
         BRAS  RE,PRCFLD                                                        
*                                                                               
INITI70  CLC   AGENCY,=C'SJ'                                                    
         JE    INITI80                                                          
         CLC   AGENCY,=C'*B'                                                    
         JE    INITI80                                                          
         CLC   AGENCY,=C'H7'                                                    
         JE    INITI80                                                          
         CLC   AGENCY,=C'ST'       SHERWOORD TRADING? ADDED FEB28/2017          
         JE    INITI80                                                          
         XC    CL2MITL,CL2MITL     CLEAR FIELD TITLE                            
         OI    CL2MITLH+1,X'20'    PROTECT FIELD                                
         OI    CL2MITLH+6,X'80'    TRANSMIT                                     
         XC    CL2MIDC,CL2MIDC     CLEAR FIELD TITLE                            
         OI    CL2MIDCH+1,X'20'    PROTECT FIELD                                
         OI    CL2MIDCH+6,X'80'    TRANSMIT                                     
         J     INITI80                                                          
INITI80  DS    0H                                                               
                                                                                
*                                                                               
INITIX   J     EXIT                                                             
*                                                                               
* RF POINTS FLD SECURITY VALUE                                                  
* R2 POINTS TITLE FLD TO BE PROCESSED                                           
*                                                                               
PRCFLD   DS    0H                  CLR TITLE & PROTECT FLDS                     
         SR    R1,R1                                                            
         IC    R1,0(R2)            TOTAL FLD LENGTH                             
         CLI   0(RF),0             FULL ACCESS?                                 
         BNE   PRCF20                                                           
         AR    R2,R1               TO TO INPUT FLD                              
         NI    1(R2),X'FF'-X'20'   UNPROTECT INPUT FLD                          
         B     PRCFLDX                                                          
PRCF20   CLI   0(RF),C'Y'          READ ONLY?                                   
         BE    PRCF30                                                           
         CLI   0(RF),C'N'          NO ACCESS?                                   
         BE    *+6                                                              
         DC    H'0'                INVALID SECURITY VALUE!                      
         SHI   R1,8+1              MINUS OVERHEAD AND ONE FOR EX                
         CHI   R1,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD TITLE FLD LENGTH                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'         TRANSMIT CLEARED TITLE FLD                   
PRCF30   IC    R1,0(R2)                                                         
         AR    R2,R1               BUMP FROM TITLE TO INPUT FLD                 
         OI    1(R2),X'20'         PROTECT INPUT FLD                            
PRCFLDX  OI    6(R2),X'80'         TRANSMIT PROTECTED INPUT FLD                 
         BR    RE                                                               
*                                                                               
* SECURITY FIELDS TABLE - DISPLACEMENT (1 BYTE) AND FLD # (1 BYTE)              
*                                                                               
SECFLDS  DS    0XL2                ** DISPS. TO SECURITY VALUES **              
         DC    AL1(SECMNOV-SECVALS,001)                                         
         DC    AL1(SECSFHG-SECVALS,002)                                         
         DC    AL1(SECFROP-SECVALS,003)                                         
         DC    AL1(SECRLPG-SECVALS,004)                                         
         DC    AL1(SECCOS2-SECVALS,005)                                         
         DC    AL1(SECUCBC-SECVALS,019)                                         
         DC    AL1(SECPOBC-SECVALS,020)                                         
*                                                                               
SECFLDSN EQU   (*-SECFLDS)/L'SECFLDS                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPFKEYS NTR1  BASE=*,LABEL=*      CKING FOR PK KEYS                            
*                                                                               
         CLI   PFAID,2             PF2, CLIENT?                                 
         BE    CKPFK10                                                          
         CLI   PFAID,5             PF5, CLT LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,6             PF6, PRD LIST?                               
         BE    CKPFK10                                                          
*                                                                               
         J     SETCCNEQ            VALID PFKEY IS NOT ENTERED                   
*                                                                               
CKPFK10  XC    WORK,WORK           ESTABLISH AS XCTL ELEMENT                    
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
*                                                                               
         MVC   GLVXFRSY,=C'PRI'    SET FROM SYSTEM                              
         MVC   GLVXFRPR,=C'SFM'    SET FROM PROGRAM                             
         MVC   GLVXTOSY,=C'PRI'    SET TO   SYSTEM                              
         MVC   GLVXTOPR,=C'SFM'    SET TO   PROGRAM                             
         OI    GLVXFLG1,GLV1RETN                                                
         OI    GLVXFLG1,GLV1RETG                                                
*                                                                               
* SEND XCTL ELM                                                                 
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL                           
*                                                                               
         MVC   DUB,SPACES          PREPARE IT FOR RECORD FLD                    
*                                                                               
         CLI   PFAID,2             RECORD IS CLIENT2?                           
         BNE   CKPFK15                                                          
CKPFK10H MVC   DUB,=C'CLIENT  '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK15  CLI   PFAID,5             RECORD IS CLIENT?                            
         BNE   CKPFK20                                                          
         B     CKPFK10H                                                         
*                                                                               
CKPFK20  CLI   PFAID,6             RECORD IS PRD?                               
         BNE   CKPFK21                                                          
         MVC   DUB,=C'PRODUCT '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK21  DS    0H                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET RECORD FLD                                                                
*                                                                               
CKPFK25  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXREC                             
*                                                                               
         MVC   DUB,SPACES          PREPARE IT FOR ACTION FLD                    
*                                                                               
         CLI   PFAID,2             CLIENT MAINT?                                
         BNE   CKPFK30                                                          
         MVC   DUB,=C'CHANGE  '                                                 
         CLI   ACTNUM,ACTCHA       CHANGE ACTION?                               
         BE    CKPFK40                                                          
         CLI   THISLSEL,C'C'       SEL CODE IS CHANGE ON LIST?                  
         BE    CKPFK40                                                          
         CLI   MODE,VALREC         MODE IS VALREC?                              
         BE    CKPFK40                                                          
         CLI   MODE,RECPUT         MODE IS PUTREC? (STILL CHG)                  
         BE    CKPFK40                                                          
         CLI   MODE,XRECPUT        MODE IS XPUTREC?                             
         BE    CKPFK40                                                          
         MVC   DUB,=C'DISPLAY '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK30  CLI   PFAID,5             CLIENT LIST?                                 
         BNE   CKPFK31                                                          
CKPFK30H MVC   DUB,=C'LIST    '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK31  CLI   PFAID,6             PRD LIST?                                    
         BNE   CKPFK35                                                          
         B     CKPFK30H                                                         
*                                                                               
CKPFK35  DS    0H                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET ACTION FLD                                                                
*                                                                               
CKPFK40  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXACT                             
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',CL2MEDH,,GLVPRKEY   KEY                   
         GOTO1 VGLOBBER,DMCB,=C'PUTF',CL2MEDH,,GLVPRMD    MEDIA                 
         GOTO1 VGLOBBER,DMCB,=C'PUTF',CL2CLTH,,GLVPRCLT   CLIENT                
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CC EQUAL     - BILLED PO# FIELDS ARE VALIDATED                                
* CC NOT EQUAL - ERROR IN BILLED PO# FIELDS                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKBILPO# NTR1  BASE=*,LABEL=*      VALIDATE BILLED BY PURCHASE ORDER#           
*                                                                               
         CLI   SECPOBC,C'N'        AUTHORIZED TO CHANGE BILLED BY PO#?          
         JE    CKBPO#_X                                                         
         CLI   SECPOBC,C'Y'        READ-ONLY ACCESS?                            
         JE    CKBPO#_X                                                         
*                                                                               
         MVI   WKPO#SW1,0          INIT PURCHASE ORDER# SWITCH 1                
         MVI   WKERRNUM,0          INIT ERROR NUMBER                            
         XC    SVPO#FST(SVPO#FLQ),SVPO#FST                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PCLTPOEQ                                                  
         BRAS  RE,GETEL                                                         
         JNE   CKBPO#14                                                         
         USING PCLTPOEL,R6                                                      
         MVC   SVPO#PF1,PCLTPOF1   SAVE FLAG1 BEFORE CHANGE                     
         MVC   SVPO#LVL,PCLTPOLV   SAVE LEVEL BEFORE CHANGE                     
         MVC   SVPO#EFD,PCLTPOED   SAVE EFFECTIVE DATE BEFORE CHANGE            
         OI    WKPO#SW1,PO#_ELMQ   PURCHASE ORDER# ELEMENT FOUND                
         NI    PCLTPOF1,X'FF'-P_POLOVQ                                          
         DROP  R6                                                               
*                                                                               
         XC    ELEM,ELEM           PREPARE TO BUILD BILLED BY PO# ELEM          
         LA    R2,ELEM                                                          
         USING PCLTPOEL,R2                                                      
         MVI   PCLTPOEL,PCLTPOEQ   BILLED BY PURCHASE ORDER# ELEM CODE          
         MVI   PCLTPOLN,PCLTPOLQ   DEFAULT ELEMENT LENGTH                       
         CLI   CL2BPO#,C'Y'        BILLED ON PURCHASE ORDER#?                   
         JNE   *+8                                                              
         OI    PCLTPOF1,P_POBILQ                                                
         MVC   PCLTPOLV,CL2POLV    PO LEVEL                                     
*                                                                               
         CLI   CL2PEDTH+5,0        HAVE EFFECTIVE DATE?                         
         JNH   CKBPO#08                                                         
         GOTO1 DATVAL,DMCB,(2,CL2PEDT),WORK                                     
         OC    DMCB(4),DMCB                                                     
         JZ    CKBPO#12                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
         MVC   PCLTPOED,WORK+10                                                 
         MVI   PCLTPOED+2,1        SET TO FIRST DAY OF THE MONTH                
         DROP  R2                                                               
*                                                                               
CKBPO#08 CLC   ELEM+2(PCLTPOLQ-2),2(R6)                                         
         JNE   CKBPO#12            ELEM CHANGED, REVALIDATE FIELDS              
         L     R6,AIO                                                           
         MVI   ELCODE,PCLTPOEQ                                                  
         GOTO1 REMELEM             REMOVE BILLED BY PO# ELEM                    
         LA    R6,ELEM                                                          
         USING PCLTPOEL,R6                                                      
         J     CKBPO#50            VALIDATE PO OVERRIDE LABEL                   
*                                                                               
CKBPO#12 L     R6,AIO                                                           
         MVI   ELCODE,PCLTPOEQ                                                  
         GOTO1 REMELEM             REMOVE BILLED BY PO# ELEM                    
*                                                                               
CKBPO#14 XC    ELEM,ELEM           PREPARE TO BUILD BILLED BY PO# ELEM          
         LA    R6,ELEM                                                          
         USING PCLTPOEL,R6                                                      
*                                                                               
         LLC   RE,CL2BPO#H+5       LENGTH OF BILLED ON PURCHASE ORDER#          
         STC   RE,FULL+0                                                        
         LLC   RE,CL2PEDTH+5       LENGTH OF PO EFFECTIVE DATE                  
         STC   RE,FULL+1                                                        
         LLC   RE,CL2POLVH+5       LENGTH OF PO LEVEL                           
         STC   RE,FULL+2                                                        
         LLC   RE,CL2LBOVH+5       LENGTH OF PO LABEL OVERRIDE                  
         STC   RE,FULL+3                                                        
         OC    FULL,FULL           HAVE BILLED BY PO# INPUTS?                   
         JNZ   *+18                                                             
         OC    SVPO#FST(SVPO#FLQ),SVPO#FST                                      
         JZ    CKBPO#_X                                                         
         J     CKBPO#15                                                         
*                                                                               
         L     RE,FULL                                                          
         CHI   RE,L'PCLTPONM       HAVE PO LABLE OVERRIDE ONLY?                 
         JH    CKBPO#15                                                         
         J     CKBPO#50            VALIDATE PO OVERRIDE LABEL                   
*                                                                               
CKBPO#15 BRAS  RE,CKPO#REC         HAVE PO# UNDER CLIENT CODE?                  
         CLI   WKSWTICH,C'Y'                                                    
         JNE   *+8                                                              
         OI    WKPO#SW1,PO#_RECQ   PURCHASE ORDER# RECORD FOUND                 
*                                                                               
         BRAS  RE,CKBILREC         CLIENT CODE IS BILLED?                       
         CLI   WKSWTICH,C'Y'                                                    
         JNE   *+8                                                              
         OI    WKPO#SW1,PO#_BILQ   BILLING ACTIVITY EXIST FOR CLIENT            
*                                                                               
         LA    R2,CL2BPO#H         BILLED ON PURCHASE ORDER#                    
         CLI   5(R2),0                                                          
         JE    CKBPO#30                                                         
         TM    WKPO#SW1,PO#_RECQ   PO# RECORD DEFINED UNDER CLIENT?             
         JNZ   CKBPO#22                                                         
         CLI   8(R2),C'Y'          YES?                                         
         JNE   CKBPO#16                                                         
         OI    PCLTPOF1,P_POBILQ                                                
         J     CKBPO#24                                                         
CKBPO#16 CLI   8(R2),C'N'          NO?                                          
         JNE   CKBPO#_E                                                         
         NI    PCLTPOF1,X'FF'-P_POBILQ                                          
         J     CKBPO#24                                                         
*                                                                               
CKBPO#22 MVI   WKERRNUM,79         FIELD CANNOT BE CHANGED                      
         J     CKBPO#_E                                                         
*                                                                               
CKBPO#24 MVI   PCLTPOEL,PCLTPOEQ   BILLED BY PURCHASE ORDER# ELEM CODE          
         MVI   PCLTPOLN,PCLTPOLQ   DEFAULT ELEMENT LENGTH                       
*                                                                               
CKBPO#30 LA    R2,CL2PEDTH         PO EFFECTIVE DATE                            
         CLI   5(R2),0                                                          
         JH    CKBPO#32                                                         
         TM    PCLTPOF1,P_POBILQ   BILLED BY PURCHASE ORDER IS YES?             
         JZ    CKBPO#40                                                         
         TM    WKPO#SW1,PO#_BILQ   BILLING ACTIVITY EXIST FOR CLIENT?           
         JZ    CKBPO#40                                                         
         MVI   WKERRNUM,MISSING    REQUIRED IF BILLING IS DONE                  
         J     CKBPO#_E                                                         
CKBPO#32 TM    PCLTPOF1,P_POBILQ   BILLED BY PURCHASE ORDER IS YES?             
         JNZ   CKBPO#34                                                         
         CLI   CL2BPO#H+5,0                                                     
         JH    CKBPO#_E                                                         
         LA    R2,CL2BPO#H                                                      
         MVI   WKERRNUM,MISSING    REQUIRED IF EFFECTIVE DATE PRESENT           
         J     CKBPO#_E                                                         
CKBPO#34 GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         JNZ   *+12                                                             
         MVI   WKERRNUM,68         INVALID DATE FORMAT                          
         J     CKBPO#_E                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
         MVC   PCLTPOED,WORK+10                                                 
         MVI   PCLTPOED+2,1        SET TO FIRST DAY OF THE MONTH                
         OC    SVPO#EFD,SVPO#EFD   HAVE SAVED PO# EFFECTIVE DATE?               
         JZ    CKBPO#40                                                         
         CLI   ACTNUM,ACTCHA       RECORD ACTION IS CHANGE?                     
         JNE   CKBPO#40                                                         
         CLC   SVPO#EFD,PCLTPOED   STILL SAME?                                  
         JNE   CKBPO#22                                                         
*                                                                               
CKBPO#40 LA    R2,CL2POLVH         PO LEVEL                                     
         CLI   5(R2),0                                                          
         JH    CKBPO#42                                                         
         TM    PCLTPOF1,P_POBILQ   BILLED BY PURCHASE ORDER IS YES?             
         JZ    CKBPO#50                                                         
         MVI   WKERRNUM,MISSING    REQUIRED IF BILLED BY PO#                    
         J     CKBPO#_E                                                         
CKBPO#42 TM    PCLTPOF1,P_POBILQ   BILLED BY PURCHASE ORDER IS YES?             
         JZ    CKBPO#_E                                                         
         TM    WKPO#SW1,PO#_RECQ   PURCHASE ORDER# RECORD FOUND?                
         JNZ   CKBPO#_E                                                         
         CLI   8(R2),P_POLVCQ      CLIENT LEVEL?                                
         JE    CKBPO#44                                                         
         CLI   8(R2),P_POLVPQ      PRODUCT LEVEL?                               
         JE    CKBPO#44                                                         
         CLI   8(R2),P_POLVEQ      ESTIMATE LEVEL?                              
         JNE   CKBPO#_E                                                         
CKBPO#44 MVC   PCLTPOLV,8(R2)                                                   
*                                                                               
CKBPO#50 LA    R2,CL2LBOVH         PO LABEL OVERRIDE                            
         CLI   5(R2),0                                                          
         JE    CKBPO#80                                                         
         MVI   PCLTPOEL,PCLTPOEQ   BILLED BY PURCHASE ORDER# ELEM CODE          
         OI    PCLTPOF1,P_POLOVQ   PO LABEL OVERRIDE                            
         MVC   PCLTPONM,8(R2)                                                   
         MVI   PCLTPOLN,PCLTPOXQ   ELEMENT LENGTH WITH PO OVERRIDE              
         DROP  R6                                                               
*                                                                               
CKBPO#80 OC    ELEM(PCLTPOXQ),ELEM                                              
         JZ    CKBPO#_X                                                         
         GOTO1 ADDELEM                                                          
*                                                                               
CKBPO#_X J     SETCCEQ             SET CC EQUAL                                 
*                                                                               
CKBPO#_E LTR   RB,RB               SET CC NOT EQUAL (ERROR)                     
         XIT1  REGS=(R2)           DEFAULT ERROR IS "INVALID FIELD"             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CC EQUAL     - NO ESTIMATE RECORD FOUND UNDER CLIENT                          
* CC NOT EQUAL - ESTIMATE RECORD FOUND, CANNOT CHANGE FIELD                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKESTREC NTR1  BASE=*,LABEL=*      SCANNING FOR ESTIMATE RECORDS                
*                                                                               
         CLI   ACTNUM,ACTCHA       RECORD ACTION IS CHANGE?                     
         JNE   SETCCEQ                                                          
*                                                                               
         MVI   WKSWTICH,C'N'                                                    
         MVC   WKKEY,KEY                                                        
         XC    KEY+7(25-7),KEY+7                                                
         MVI   KEY+3,X'07'         ESTIMATE RECORD CODE                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE      EST REC FOR CURRENT CLT FOUND?               
         BNE   *+8                                                              
         MVI   WKSWTICH,C'Y'       YES, FOUND                                   
*                                                                               
         MVC   KEY,WKKEY                                                        
         GOTO1 HIGH                RESTORE DMGR SEQUENCE                        
*                                                                               
         CLI   WKSWTICH,C'N'                                                    
         JE    SETCCEQ                                                          
         J     SETCCNEQ            CANNOT ADD COS2 INFO IF EST FOUND            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CC EQUAL     - NO PURCHASE ORDER# RECORD UNDER CLIENT                         
* CC NOT EQUAL - PURCHASE ORDER# RECORD FOUND, CANNOT CHANGE FIELD              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPO#REC NTR1  BASE=*,LABEL=*      SCANNING FOR PO# RECORDS                     
*                                                                               
         MVI   WKSWTICH,0                                                       
         CLI   ACTNUM,ACTCHA       RECORD ACTION IS CHANGE?                     
         JNE   SETCCEQ                                                          
*                                                                               
         MVI   WKSWTICH,C'N'                                                    
         MVC   WKKEY,KEY                                                        
         XC    KEY+7(25-7),KEY+7                                                
         MVI   KEY+3,PPO#KIDQ      PURCHASE ORDER# RECORD                       
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE      PO# REC FOR CURRENT CLT FOUND?               
         JNE   *+8                                                              
         MVI   WKSWTICH,C'Y'       YES, FOUND                                   
*                                                                               
         MVC   KEY,WKKEY                                                        
         GOTO1 HIGH                RESTORE DMGR SEQUENCE                        
*                                                                               
         CLI   WKSWTICH,C'N'                                                    
         JE    SETCCEQ                                                          
         J     SETCCNEQ            PO# RECORD FOUND                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CC EQUAL     - NO BILLING RECORD UNDER CLIENT                                 
* CC NOT EQUAL - BILLING RECORD, CANNOT CHANGE FIELD                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKBILREC NTR1  BASE=*,LABEL=*      SCANNING FOR BILLING RECORDS                 
*                                                                               
         MVI   WKSWTICH,0                                                       
         CLI   ACTNUM,ACTCHA       RECORD ACTION IS CHANGE?                     
         JNE   SETCCEQ                                                          
*                                                                               
         MVI   WKSWTICH,C'N'                                                    
         MVC   WKKEY,KEY                                                        
         XC    KEY+7(25-7),KEY+7                                                
         MVI   KEY+3,X'08'         BILLING RECORD                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE      PO# REC FOR CURRENT CLT FOUND?               
         JNE   *+8                                                              
         MVI   WKSWTICH,C'Y'       YES, FOUND                                   
*                                                                               
         MVC   KEY,WKKEY                                                        
         GOTO1 HIGH                RESTORE DMGR SEQUENCE                        
*                                                                               
         CLI   WKSWTICH,C'N'                                                    
         JE    SETCCEQ                                                          
         J     SETCCNEQ            BILLING RECORD FOUND                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*===========================================================                    
* MODE=NEWSCR CLEAR SAP FIELDS IF NOT AN SAP AGENCY                             
* GET THE ACCESS RECORD TO SEE IF SAP AGENCY                                    
*==============================================================                 
*                                                                               
SETSAP   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING CT5REC,R4                                                        
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCY                                                  
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO2            USE IO2 AS IO1 USED BY KEYMERGE!             
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',WORK,AIO                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SAPAGY,C'N'                                                      
         L     R4,AIO                                                           
         LA    R4,CT5DATA-CT5REC(R4)                                            
         SR    R0,R0                                                            
*                                                                               
SETSAP2  CLI   0(R4),0                                                          
         JE    SETSAP10                                                         
         CLI   0(R4),X'B4'                                                      
         BE    SETSAP4                                                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     SETSAP2                                                          
*                                                                               
         USING CTAGDD,R4                                                        
*                                                                               
SETSAP4  TM    CTAGOPTS,CTAGSAP    TEST SAP AGY                                 
         JZ    SETSAP10                                                         
         MVI   SAPAGY,C'Y'                                                      
         J     EXIT                                                             
         DROP  R4                                                               
                                                                                
*=========================================================                      
* NOT AN SAP AGENCY - CLEAR TITLE AND PROTECT SAP FIELD                         
*=========================================================                      
                                                                                
SETSAP10 XC    CL2SAP,CL2SAP       SAP INPUT FIELD                              
         OI    CL2SAPH+6,X'80'                                                  
         OI    CL2SAPH+1,X'20'     SET TO PROTECTED                             
*                                                                               
         XC    CL2STTL,CL2STTL     SAP TITLE                                    
         OI    CL2STTLH+6,X'80'                                                 
         J     EXIT                                                             
         LTORG                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PUTREQRC NTR1  BASE=*,LABEL=*      PUT A REQUEST CARD FOR T/A REPORT            
*                                                                               
         XC    QCTL,QCTL                                                        
         MVC   QAREA,SPACES                                                     
         MVC   QAREA(2),=C'41'                                                  
         MVC   QAREA+2(2),AGENCY                                                
         MVC   QAREA+4(1),QMED                                                  
         MVC   QAREA+5(3),QCLT                                                  
         MVC   QAREA+68(7),=C'AUTOREQ'                                          
*                                                                               
         MVI   QCTL+10,41                                                       
         MVI   QCTL+14,106                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QCTL,QCTL                    
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         JZ    SETCCEQ                                                          
         J     SETCCNEQ                                                         
*                                                                               
GET_ITXT ST    RE,SAVERE                                                        
         XC    FULL,FULL                                                        
         MVI   FULL+1,25           SYSTEM                                       
         L     R3,FULL                                                          
         GOTOR GETTXT,DMCB+12,(R2),0,(C'I',DMCB),0,0,(R3)                       
         OI    CONHEADH+6,X'80'                                                 
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSREPCD NTR1  BASE=*,LABEL=*      CHECK FOR SPECIAL REP CODE                   
*                                                                               
         MVI   ELCODE,PCLREPEQ                                                  
         GOTO1 REMELEM             REMOVE CLIENT SPECIAL REP CODE ELEM          
*                                                                               
         MVI   WKERRNUM,0          INIT ERROR NUMBER                            
         XC    CL2SRNM,CL2SRNM     SPECIAL REP NAME (PROTECTED)                 
         XC    ELEM,ELEM           PREPARE TO BUILD SPECIAL REP ELEM            
         LA    R3,ELEM                                                          
         USING PCLREPEL,R3                                                      
         LA    R2,CL2SREPH         POINT TO SPECIAL REP FIELD HEADER            
         CLI   5(R2),0                                                          
         JE    CKSREP_X                                                         
*                                                                               
         MVI   WKERRNUM,071        INPUT IS TOO LONG                            
         CLI   5(R2),4                                                          
         JH    CKSREP_E                                                         
*                                                                               
         MVI   WKERRNUM,003        INPUT IS NOT NUMERIC                         
         TM    4(R2),X'08'                                                      
         JZ    CKSREP_E                                                         
*                                                                               
         MVI   WKERRNUM,053        RECORD NOT FOUND                             
         BRAS  RE,PACK                                                          
         OI    DUB+7,X'0F'                                                      
         MVC   SVWORK(L'KEY),KEY                                                
         XC    KEY+03(L'KEY-03),KEY+03                                          
         MVI   KEY+03,X'11'                                                     
         UNPK  KEY+04(04),DUB                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     REP RECORD FOUND?                            
         JE    *+12                FOUND                                        
         OI    CL2SRNMH+6,X'80'    DISPLAY CLEARED REP NAME                     
         J     CKSREP_E                                                         
*                                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     RE,AIO2                                                          
         USING PREPREC,RE                                                       
         MVI   PCLREPEC,PCLREPEQ   CLIENT LEVEL SPECIAL REP ELEM CODE           
         MVI   PCLREPLN,PCLREPLQ   CLIENT LEVEL SPECIAL REP ELEM LENGTH         
         MVC   PCLREPCD,PREPKREP   VALIDATED REP CODE                           
         MVC   CL2SRNM,PREPNAME                                                 
         OI    CL2SRNMH+6,X'80'    DISPLAY SPECIAL REP NAME                     
         DROP  R3,RE                                                            
*                                                                               
         MVC   KEY(25),SVWORK                                                   
         GOTO1 HIGH                RESTORE SEQUENCE                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            POINT BACK TO WORKING RECORD                 
*                                                                               
CKSREP80 OC    ELEM(PCLREPLQ),ELEM                                              
         JZ    CKSREP_X                                                         
         GOTO1 ADDELEM                                                          
*                                                                               
CKSREP_X J     SETCCEQ             SET CC EQUAL                                 
*                                                                               
CKSREP_E LTR   RB,RB               SET CC NOT EQUAL (ERROR)                     
         XIT1  REGS=(R2)           DEFAULT ERROR IS "INVALID FIELD"             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PACK     NTR1  BASE=*,LABEL=*      RETURN PACKED VALUE IN DUB                   
*                                                                               
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         LLC   R1,5(R2)                                                         
         CHI   R1,0                                                             
         JNH   PACK_X              EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         JZ    PACK_X              OR NON NUMERIC                               
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         J     PACK_X                                                           
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
*                                                                               
PACK_X   XIT1  REGS=(R0)           R0 = VALUE IN BINARY                         
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DISPSREP NTR1  BASE=*,LABEL=*      DISPLAY SPECIAL REP                          
*                                                                               
         XC    CL2SREP,CL2SREP                                                  
         XC    CL2SRNM,CL2SRNM                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PCLREPEQ     SPECIAL REP ELEMENT CODE                     
         BRAS  RE,GETEL                                                         
         JNE   D_SREP80                                                         
         USING PCLREPEL,R6                                                      
*                                                                               
         MVC   CL2SREP,PCLREPCD                                                 
         MVC   SVWORK(L'KEY),KEY                                                
         XC    KEY+03(L'KEY-03),KEY+03                                          
         MVI   KEY+03,X'11'                                                     
         MVC   KEY+04(04),PCLREPCD                                              
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(25),KEY     REP CODE ON DIRECTORY?                       
         JE    *+14                                                             
         MVC   CL2SRNM(21),=C'** REP NOT ON FILE **'                            
         J     D_SREP80                                                         
         MVC   DUB(L'AIO),AIO      SAVE ORIGINAL AIO                            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     RE,AIO2                                                          
         USING PREPREC,RE                                                       
         MVC   CL2SRNM,PREPNAME                                                 
         MVC   AIO,DUB             RESTORE AIO AND KEY                          
         MVC   KEY,SVWORK                                                       
         DROP  RE,R6                                                            
*                                                                               
D_SREP80 OI    CL2SREPH+6,X'80'                                                 
         OI    CL2SRNMH+6,X'80'                                                 
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMC7D          CLIENT2 (CL2) MAINT SCREEN                   
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMC4D          CLIENT LIST SCREEN                           
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMWORKD                                                     
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
SVWORK   DS    XL64                GENERAL WORKING STORAGE                      
WKCLSTAT DS    X                   SAVE CLT STATUS BYTE                         
SYSSW    DS    X                                                                
COS2FLAG DS    X                   FLAG FOR COST2 FACTOR ELEM                   
WKFINCLT DS    X                   'Y' = FINANCIAL CLT                          
WKCPROF  DS    CL(L'PCLTPROF)                                                   
WKKEY    DS    XL(L'KEY)                                                        
WKSWTICH DS    X                                                                
*                                                                               
SAVERE   DS    F                   FOR SAVING RETURN ADDRESSES                  
QCTL     DS    CL26                                                             
QAREA    DS    CL80                                                             
*                                                                               
SVPO#FST DS    0X                  SAVE PURCHASE ORDER# FIELDS START            
SVPO#PF1 DS    XL(L'PCLTPOF1)      SAVE PURCHASE ORDER# FLAG 1                  
SVPO#LVL DS    CL(L'PCLTPOLV)      SAVE PURCHASE ORDER# LEVEL                   
SVPO#EFD DS    CL(L'PCLTPOED)      SAVE PO# EFFECTIVE DATE                      
SVPO#FLQ EQU   *-SVPO#FST          SAVE PURCHASE ORDER# FIELDS LENGTH           
*                                                                               
WKPO#SW1 DS    XL1                 PURCHASE ORDER# WORKING SWTICH 1             
PO#_ELMQ EQU   X'80'               BILLED BY PO# ELEMENT EXIST                  
PO#_RECQ EQU   X'40'               PO# RECORD EXIST FOR CLIENT CODE             
PO#_BILQ EQU   X'20'               BILLING ACTIVITY EXIST FOR CLIENT            
*                                                                               
WKERRNUM DS    X                   ERROR NUMBER                                 
*                                                                               
* SECURITY VALUES ARE TRANSLATED AS FOLLOW:                                     
* C'Y'   READ ONLY (FIELD WILL BE PROTECTED)                                    
* C'N'   NO ACCESS (FIELD WILL BE HIDDEN AND PROTECTED)                         
* X'00'  WRITE                                                                  
*                                                                               
SECVALS  DS    0X                  ** FIELD SECURITY VALUES **                  
*                                                                               
SECMNOV  DS    C                   MEDIA NAME OVERRIDE                          
SECSFHG  DS    C                   SPECIAL FINANCIAL HANDLING                   
SECFROP  DS    C                   FROZEN OPTIONS                               
SECRLPG  DS    C                   RLP GROUP                                    
SECCOS2  DS    C                   COST 2                                       
SECUCBC  DS    C                   UCOMM BILL CONTROL                           
SECPOBC  DS    C                   PURCHASE ORDER BILL CONTROL                  
*                                                                               
SECVALSL EQU   *-SECVALS           MAX IS 255                                   
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PCLTREC           DSECT CLIENT RECORD                          
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPGENPO#          PRINTPAK PURCHASE ORDER# RECORD              
         EJECT                                                                  
*                                                                               
       ++INCLUDE PREPREC           PRINTPAK REP RECORD                          
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLVXCTLD        GLOBBER TRANSFER CONTROLS                    
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLOBEQUS        DSECT FOR GLOBBER                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENRFP                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE CTGENFILE                                                      
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'105PRSFM1D   05/05/20'                                      
         END                                                                    
