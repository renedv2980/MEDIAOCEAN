*          DATA SET EDREQXPLOD AT LEVEL 060 AS OF 01/08/21                      
*PROCESS USING(WARN(15))                                                        
*PHASE REQXPLDA                                                                 
***********************************************************************         
* 08JAN21 060 GHOA ADD ACT7 TO THE LIST OF ACC JOBS TO SEPARATE.      *         
* 31JUL19 059 JSAY SEPARATE ACCPAK VE JOBS                            *         
* 22MAR18 058 GHOA SEPARATE ACCPAK AB JOBS                            *         
* 21FEB15 056 RCRI SUPPORT TWO CHR SYSTEMS FOR 74 CON/PRO REQUESTS    *         
* 23JAN15 055 GHOA SEPARATE TALENT PX JOBS                            *         
* 04SEP13 054 BPLA P10/SBT/NBT BREAK-OUT BY FILE TYPE                           
* 26FEB13 052 JSHA ADD ACTT TO THE LIST OF ACC JOBS TO SEPARATE.      *         
* 02FEB13 052 AHYD REMOVE DMSPTNET - USE DDNAME TO GET INFO.          *         
* 02FEB21 052 DEIS CHANGED USING WARNING AND FIX USING AMBIGUITY      *         
* 04JAN11 051 GHOA SEPARATE TALENT NH JOBS                            *         
* 17APR08 046 AHYD FIX QSUBSYS WITH SE#, PASS VXTRAS FROM PEEL PROG.  *         
* 21MAR06 045 GHOA SEPARATE TALENT KD JOBS                            *         
* 23JAN04 041 GHOA SEPARATE TALENT HF/SN JOBS                         *         
* 16JAN03 037 YKVA GENERATE ANOTHER REQUEST FOR F-RATE BUYS           *         
* 20MAR02 034 AHYD FIX MMOS AND GENERATING THE CHECK REGS. AC56       *         
***********************************************************************         
REQXPLOD TITLE '- MODULE TO ADJUST REQUESTS'                                    
REQXPLOD CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**XPLD**,R9,R8                                                 
         LM    R2,R5,0(R1)         P1 = A(N*RECLEN STACK)                       
         L     RF,20(,R1)          P6                                           
         MVC   VXTRAS(VXTRALNQ),0(RF)                                           
         L     R6,VCPRINT                                                       
                                                                                
         USING DPRINT,R6                                                        
         USING EODREQD,R2                                                       
         USING REQHDRD,R5                                                       
         ST    R5,AREQHDR                                                       
         LR    RE,R2               GET STACK ENTRY LENGTH                       
         SHI   RE,2                                                             
         MVC   EODLEN,0(RE)                                                     
                                                                                
         CLC   SAVESIN,QDATA       TEST CONTINUING A SPOOF REQUEST              
         BNE   *+14                NO                                           
         MVC   QSORTFLD(L'SAVEPRNT),SAVEPRNT                                    
         B     XP08                KEEP ALL CARDS OF A REQUEST TOGETHER         
                                                                                
         XC    SAVESIN,SAVESIN     CLEAR SAVESIN FOR SPOOF TESTS                
         CLI   QDATA+4,C' '        CHARACTER MUST BE BLANK                      
         BNE   XP08                                                             
                                                                                
         MVC   DUB(6),EZEROES      NEXT 6 CHARS MUST BE NUMERIC (SIN)           
         MVZ   DUB(6),QDATA+5                                                   
         CLC   DUB(6),EZEROES                                                   
         BNE   XP08                                                             
                                                                                
         LA    R1,QDATA+12                                                      
         CLC   =C'00',0(R1)        TEST SCREEN ID                               
         BNE   XP02                                                             
         MVC   DUB(2),EZEROES      NEXT 2 CHARS MUST BE NUMERIC (LEN)           
         MVZ   DUB(2),2(R1)                                                     
         CLC   DUB(2),EZEROES                                                   
         BNE   XP08                                                             
         PACK  DUB,2(2,R1)                                                      
         CVB   RE,DUB                                                           
         LA    R1,5(R1,RE)                                                      
                                                                                
XP02     CLC   =C'02',0(R1)        NEXT MUST BE ' 02'                           
         BNE   XP08                                                             
                                                                                
         MVC   DUB(2),EZEROES      NEXT 2 CHARS MUST BE NUMERIC (LEN)           
         MVZ   DUB(2),2(R1)                                                     
         CLC   DUB(2),EZEROES                                                   
         BNE   XP08                                                             
         PACK  DUB,2(2,R1)         LENGTH OF FIELD                              
         CVB   RE,DUB                                                           
         LA    RF,5(R1,RE)         POINT RF TO NEXT FIELD                       
                                                                                
         MVC   SAVEPRNT,SPACES     SAVE FIELD 2 - COULD BE REQUESTOR            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SAVEPRNT(0),4(R1)                                                
                                                                                
         CLC   =C'03',0(RF)        NEXT MUST BE ' 03'                           
         BNE   XP08                                                             
                                                                                
         MVC   DUB(2),EZEROES      NEXT 2 CHARS MUST BE NUMERIC (LEN)           
         MVZ   DUB(2),2(RF)                                                     
         CLC   DUB(2),EZEROES                                                   
         BNE   XP08                                                             
         PACK  DUB,2(2,RF)         LENGTH OF FIELD                              
         CVB   RE,DUB                                                           
         LA    RF,5(RE,RF)         BUMP TO NEXT FIELD                           
                                                                                
XP04     CLC   =C'04',0(RF)        TEST WHEN FIELD IS PRESENT                   
         BE    XP06                YES - SORT ON FIELD 2                        
         CLC   =C'05',0(RF)        TEST REQUESTOR FIELD                         
         BNE   XP08                NO                                           
                                                                                
         MVC   DUB(2),EZEROES      NEXT 2 CHARS MUST BE NUMERIC (LEN)           
         MVZ   DUB(2),2(RF)                                                     
         CLC   DUB(2),EZEROES                                                   
         BNE   XP08                                                             
                                                                                
         PACK  DUB,2(2,RF)         IT'S A SPOOF REQUEST                         
         CVB   RE,DUB                                                           
         MVC   SAVEPRNT,SPACES     SORT ON FIELD 5                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SAVEPRNT(0),4(RF)                                                
                                                                                
XP06     MVC   QSORTFLD(L'SAVEPRNT),SAVEPRNT                                    
         MVC   SAVESIN,QDATA       HANG ON TO ID AND SIN                        
         EJECT                                                                  
XP08     CLC   =C'74*',TYPE        TEST FOR PROFILE T/A'S                       
         BNE   XP10                                                             
         BAS   RE,CT74                                                          
         B     XIT                                                              
                                                                                
XP10     CLI   QSYSTEM,C'S'                                                     
         BE    SPOT                                                             
         CLI   QSYSTEM,C'F'        TRAFFIC                                      
         BE    SPOT                IS A LOT LIKE SPOT                           
         CLI   QSYSTEM,C'P'                                                     
         BE    PRINT                                                            
         CLI   QSYSTEM,C'R'                                                     
         BE    REP                                                              
         CLI   QSYSTEM,C'A'                                                     
         BE    ACC                                                              
         CLI   QSYSTEM,C'M'                                                     
         BE    MED                                                              
         CLI   QSYSTEM,C'T'                                                     
         BE    TALENT                                                           
         MVC   QUSER,QDATA+2                                                    
         B     XIT                                                              
                                                                                
DELETE   LR    R0,R2                                                            
         LH    R1,EODLEN                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
*              ROUTINES FOR SPOT                                                
***********************************************************************         
SPOT     ST    R2,SVEODREQ                                                      
*                                                                               
         OC    SAVESIN,SAVESIN     TEST SPOOF REQUEST                           
         BNZ   SPOT02                                                           
         MVI   QLANG,C' '          NO - INITIALIZE QLANG                        
         CLI   QDATA+30,C' '       TEST FOR A SECOND CARD                       
         BNH   SPOT02                                                           
         MVC   QLANG,QDATA2+1      COMBINE JOBS OF SAME LANGUAGE                
*                                                                               
SPOT02   CLC   TYPE,=C'B1'                                                      
         BE    *+14                                                             
         CLC   TYPE,=C'D1'                                                      
         BNE   SPB1X                                                            
         CLC   AGENCY,=C'DF'                                                    
         BNE   SPMTRD                                                           
*                                  CHILD SPOT SPECIAL                           
         CLI   QDATA+80+31,C'C'    TEST CASH BILL REQUEST                       
         BNE   SPMTRD              NO                                           
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVI   QDATA+80+31,C'T'    GENERATE A TRADE REQUEST TOO                 
         B     SPB1                                                             
*                                  GROUP M CASH/TRADE REQUEST                   
SPMTRD   CLI   QDATA+80+40,C'C'    TEST CASH BILL REQUEST                       
         BNE   SPFRT               NO                                           
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVI   QDATA+80+40,C'T'    GENERATE A TRADE REQUEST TOO                 
         B     SPB1                                                             
*                                                                               
SPFRT    DS    0H                                                               
         CLI   QDATA+80+30,X'40'   ANYTHING IN THIS FIELD ?                     
         BE    SPB1                                                             
         BAS   RE,COPYREQ          YES, COPY REQUEST                            
         OI    QDATA+80+30,X'40'   ADD A UPPERCASE 'F' REQUEST                  
*                                  LEAVE POINTING AT 2ND REQUEST                
SPB1     CLC   TYPE,=C'B1'                                                      
         BNE   SPB1X               IF D1, DONE                                  
*                                                                               
SPB1A    MVC   DUB(1),QDATA+30     SAVE 2ND CARD INDICATOR                      
         CLI   QDATA+66,C'2'       TEST WANT A2 WITH BILLING                    
         BE    *+12                                                             
         CLI   QDATA+66,C'B'       TEST WANT AB WITH BILLING                    
         BNE   SPBILBT                                                          
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVI   TYPE,C'A'                                                        
         MVC   TYPE+1(1),QDATA+66                                               
         MVC   QDATA+29(39),SPACES   CLEAR UP TO REQUESTOR                      
         MVC   QDATA+30(1),DUB       RESTORE 2ND CARD INDICATOR                 
         MVC   QDATA+31(2),=C'ES'                                               
         MVI   QDATA+67,C'Y'         SET 'ACCOMPANIES BILLING' IND              
         MVC   QDATA+80+20(50),SPACES  CLEAR BILLING USER FIELDS                
         CLC   QDATA+80(80),SPACES     ANY F...ING DATA ON CARD 2 ?             
         BNE   *+8                                                              
         MVI   QDATA+30,C' '       SUPPRESS 2ND CARD IND                        
*                                                                               
SPBILBT  LR    RE,R2                                                            
         L     R2,SVEODREQ         LOAD ORIGINAL REQUEST POINTER                
         CLI   QDATA+80+48,C'B'    WANT TO GENERATE BT, NOT INV REG             
         LR    R2,RE               DOESN'T SET CONDITION CODE                   
         BNE   SPB1X                                                            
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVC   QDATA+05(75),SPACES CLEAR BILLING USER FIELDS                    
         MVC   QDATA+80(80),SPACES CLEAR 2ND REQ CARD                           
         MVC   TYPE,=C'BT'         CREATE BT REQUEST                            
         MVC   CLIENT,=C'ALL'                                                   
         MVC   PRODUCT,=C'ALL'                                                  
         MVC   ESTIMATE,=C'ALL'                                                 
         GOTO1 VDATCON,PARAS,(5,0),(0,START)                                    
         MVC   END,START                                                        
         MVI   OPTIONS,C'N'                                                     
         MVC   QUESTOR(8),=C'*BILLING'                                          
*                                                                               
SPB1X    L     R2,SVEODREQ         RESTORE ORIGINAL REQUEST POINTER             
*                                                                               
SPBU     CLC   TYPE,=C'BU'         NET BILLING                                  
         BE    *+14                                                             
         CLC   TYPE,=C'DU'                                                      
         BNE   SPK4                                                             
*                                                                               
         CLI   QDATA+80+49,C'C'    MAKE A DECISION IF LEAVING REQUEST           
         BL    SMEDCHK                                                          
         GOTO1 VDATCON,PARAS,(5,0),(0,TODAY)                                    
         CLI   QDATA+80+49,C'1'    IF INCLUDE NXT MONTH                         
         BL    BUMOSCK             FUDGE TODAY'S DATE TO SAY NXT MONTH          
         GOTO1 VADDAY,PARAS,(C'M',TODAY),TODAY,F'1'                             
BUMOSCK  CLC   START(4),TODAY      CHECK THAT MOS IS NOT HIGHER                 
         BH    DELETE              IF HIGHER DELETE IT                          
SMEDCHK  CLI   OPTIONS+4,C'*'      NEED TO EXPAND SUB-MEDIA?                    
         BNE   SPBUBT                                                           
         MVI   OPTIONS+4,C'N'      SUB-MEDIA N                                  
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVI   OPTIONS+4,C'C'      SUB-MEDIA C                                  
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVI   OPTIONS+4,C'S'      SUB-MEDIA S                                  
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVI   OPTIONS+4,C'O'      SUB-MEDIA O                                  
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVI   OPTIONS+4,C'D'      SUB-MEDIA D                                  
*                                                                               
SPBUBT   CLI   QDATA+80+48,C'B'    WANT TO GENERATE BT, NOT INV REG             
         BNE   SPBUX                                                            
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVC   QDATA+05(75),SPACES CLEAR BILLING USER FIELDS                    
         MVC   QDATA+80(80),SPACES CLEAR 2ND REQ CARD                           
         MVC   TYPE,=C'BT'         CREATE BT REQUEST                            
         MVC   CLIENT,=C'ALL'                                                   
         MVC   PRODUCT,=C'ALL'                                                  
         MVC   ESTIMATE,=C'ALL'                                                 
         GOTO1 VDATCON,PARAS,(5,0),(0,START)                                    
         MVC   END,START                                                        
         MVI   OPTIONS,C'N'                                                     
         MVC   QUESTOR(8),=C'*BILLING'                                          
*                                                                               
SPBUX    L     R2,SVEODREQ         RESTORE ORIGINAL REQUEST POINTER             
*                                                                               
SPK4     CLC   TYPE,=C'K4'         K4 -PRODUCT ALLOCATION                       
         BE    SPK4D                                                            
         CLC   TYPE,=C'K5'         K5 IS ALIAS                                  
         BNE   SPK4X                                                            
SPK4D    DS    0H                                                               
         CLC   AGENCY,=C'GB'       FOR GB ONLY                                  
         BE    SPK4F                                                            
         CLC   AGENCY,=C'SJ'       AND SJR                                      
         BNE   SPK4X                                                            
SPK4F    DS    0H                                                               
         CLI   QUESTOR,C'*'        QUESTOR=* MEANS SEPARATE JOB                 
         BNE   SPK4X                                                            
         LH    R1,SPK4CNT          BUMP COUNTER                                 
         AHI   R1,1                                                             
         STH   R1,SPK4CNT                                                       
         STC   R1,QSUBUSER                                                      
*                                                                               
SPK4X    DS    0H                                                               
                                                                                
SPKL     CLC   TYPE,=C'KL'         KL -CANADIAN LOCAL NTWK ALLOC                
         BNE   SPKLX                                                            
         LH    R1,SPKLCNT          ONE REQUEST PER JOB                          
         AHI   R1,1                                                             
         STH   R1,SPKLCNT                                                       
         STC   R1,QSUBUSER                                                      
*                                                                               
SPKLX    DS    0H                                                               
                                                                                
SPD8     DS    0H                  WI/D8 GETS 1 JOB PER REQUEST                 
         CLC   TYPE,=C'D8'                                                      
         BNE   SPD8X                                                            
         CLC   AGENCY,=C'WI'                                                    
         BNE   SPD8X                                                            
         CLC   MARKET(3),SPACES    SPACES MAY BE THERE IN PLACE OF ALL          
         BE    SPD8A                                                            
         CLC   MARKET(3),=C'ALL'   SINGLE MARKETS GO TOGETHER                   
         BE    SPD8A                                                            
         SR    R1,R1               SET FOR 0                                    
         B     SPD8B                                                            
*                                                                               
SPD8A    LH    R1,SD8WICNT                                                      
         AHI   R1,1                                                             
         STH   R1,SD8WICNT                                                      
*                                                                               
SPD8B    STC   R1,QSUBUSER                                                      
*                                                                               
SPD8X    DS    0H                                                               
*                                                                               
SPD5     DS    0H                                                               
         CLC   =C'D5MIC',TYPE      TEST MI/C                                    
         BNE   SPD5X                                                            
         CLC   MARKET(3),SPACES                                                 
         BE    SPD5A                                                            
         CLC   MARKET(3),=C'ALL'                                                
         BE    SPD5A                                                            
         SR    R1,R1                                                            
         B     SPD5B                                                            
*                                                                               
SPD5A    LH    R1,SD5MICNT                                                      
         AHI   R1,1                                                             
         STH   R1,SD5MICNT                                                      
*                                                                               
SPD5B    STC   R1,QSUBUSER                                                      
*                                                                               
SPD5X    DS    0H                                                               
                                                                                
SPU3     CLC   TYPE,=C'U3'                                                      
         BNE   SPU3X                                                            
         CLC   AGENCY(5),=C'DFTAO'                                              
         BE    SPU3DF                                                           
         CLC   AGENCY(5),=C'DFTCO'                                              
         BE    SPU3DF                                                           
         CLC   AGENCY(6),=C'DFTPTA'                                             
         BNE   SPU3X                                                            
SPU3DF   MVC   QSORTFLD(3),CLIENT      SORT ON CLIENT/MKT/STA                   
         MVC   QSORTFLD+3(9),MARKET                                             
*                                                                               
SPU3X    DS    0H                                                               
                                                                                
SPU4     CLC   TYPE,=C'U4'         FOR U4'S                                     
         BNE   SPU4X                                                            
         CLC   AGENCY(6),=C'NWTKP '    SUPPRESSED FOR NW/T/KP                   
         BE    DELETE                                                           
         CLC   AGENCY(2),=C'BA'        SUPPRESSED FOR BATES TORONTO             
         BE    DELETE                                                           
                                                                                
SPU4X    DS    0H                                                               
                                                                                
SPU5     CLC   TYPE,=C'U5'         FOR U5'S                                     
         BNE   SPU5X                                                            
         CLC   AGENCY,=C'SM'           EXCEPT FOR SM                            
         BE    SPU5X                                                            
         CLC   AGENCY,=C'TR'                  AND TR                            
         BE    SPU5X                                                            
         CLC   AGENCY,=C'BA'                  AND BATES TORONTO                 
         BE    SPU5X                                                            
         CLC   AGENCY,=C'JW'                  AND JW (INV)                      
         BNE   SPU5B                                                            
         CLC   QUESTOR(3),=C'INV'                                               
         BE    SPU5X                                                            
                                                                                
SPU5B    DS    0H                  CREATE A U4                                  
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVC   TYPE,=C'U4'                                                      
         MVI   QDATA+59,C'B'                                                    
         MVC   QDATA+61(5),SPACES  OPTIONS SUPPRESSED                           
         MVC   STATION,SPACES                                                   
         SH    R2,EODLEN                                                        
                                                                                
SPU5X    DS    0H                                                               
         EJECT                                                                  
SPU8     CLC   TYPE,=C'U8'                                                      
         BE    SPU8A                                                            
         CLC   TYPE,=C'U9'                                                      
         BNE   SPU8X                                                            
         CLC   AGENCY,=C'PE'       SUPPRESSED FOR PEPSI                         
         BE    DELETE                                                           
                                                                                
SPU8A    CLC   AGENCY,=C'YR'       IF YR                                        
         BE    SPU8B                                                            
         CLC   AGENCY,=C'BO'          BO                                        
         BE    SPU8B                                                            
         B     SPU8D                                                            
                                                                                
SPU8B    CLI   QDATA+62,C'G'       AND G IN COL 63                              
         BNE   SPU8D                                                            
         CLC   CLIENT,=CL3'BM'     AND BM                                       
         BNE   SPU8D                                                            
         MVC   SAVEPROD,PRODUCT                                                 
         MVC   PRODUCT,=C'POL'                                                  
         MVC   AGENCY,=C'BM'                                                    
         MVC   TYPE,=C'U8'                                                      
         MVC   QORIGIN,=AL2(BRM)   (ORIGIN BM)                                  
*                                  ALSO CREATE A U9 T/A                         
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVC   TYPE,=C'U9'                                                      
         MVC   PRODUCT,SAVEPROD                                                 
         SH    R2,EODLEN                                                        
         B     SPU8F                                                            
                                                                                
SPU8D    CLI   QDATA+62,C'G'       IF COL 63=G (T/A REQUEST)                    
         BNE   SPU8F                                                            
         CLC   QORIGIN,=AL2(BSYR)                                               
         BE    SPU8BS                                                           
         CLC   QORIGIN,=AL2(BSLB)                                               
         BE    SPU8BS                                                           
         CLC   QORIGIN,=AL2(BSJW)                                               
         BE    SPU8BS                                                           
         B     SPU8F                                                            
SPU8BS   DS    0H                                                               
         BAS   RE,COPYREQ          COPY THE REQUEST                             
         MVC   QORIGIN,=AL2(BSNY)    FOR BSNY                                   
         B     SPU8X                                                            
                                                                                
SPU8F    CLI   MEDIA,C'T'                                                       
         BNE   SPU8H                                                            
         CLC   AGENCY,=C'FT'       FT GET U8'S                                  
         BE    SPU8K                                                            
                                                                                
SPU8H    MVI   QSORTFLD,C'A'       ENSURE POL SORTS LAST                        
         CLC   PRODUCT,=C'POL'                                                  
         BNE   *+8                                                              
         MVI   QSORTFLD,C'Z'                                                    
         CLI   QDATA+62,C'C'                                                    
         BE    *+8                                                              
         MVI   QDATA+62,C' '                                                    
         B     SPU8X                                                            
                                                                                
SPU8K    MVC   TYPE,=C'U8'                                                      
         MVC   PRODUCT,=C'POL'                                                  
                                                                                
SPU8X    DS    0H                                                               
                                                                                
SPU9     CLC   TYPE,=C'U9'         FOR U9'S                                     
         BNE   SPU9X                                                            
         CLC   MARKET(3),=C'ALL'                                                
         BE    *+10                                                             
         MVC   MARKET,=C'ALL '         MAKE MARKET=ALL                          
                                                                                
SPU9X    DS    0H                                                               
*                                                                               
         EJECT                                                                  
SP21     CLC   TYPE,=C'21'         FOR 21'S                                     
         BNE   SP21X                                                            
         MVC   TYPE,=C'U7'         CHANGE TO U7                                 
                                                                                
SP21X    DS    0H                                                               
                                                                                
SP30     CLC   TYPE,=C'30'         FOR 30'S                                     
         BNE   SP30X                                                            
         B     DELETE                  SKIP                                     
                                                                                
SP30X    DS    0H                                                               
                                                                                
SP31     CLC   TYPE,=C'31'         FOR 31'S                                     
         BNE   SP31X                                                            
         B     DELETE                  SKIP                                     
                                                                                
SP31X    DS    0H                                                               
                                                                                
SP32     CLC   TYPE,=C'32'         FOR 32'S                                     
         BNE   SP32X                                                            
         B     DELETE                  SKIP                                     
                                                                                
SP32X    DS    0H                                                               
                                                                                
SP52     CLC   TYPE,=C'52'         FOR 52'S                                     
         BE    SP52A                                                            
         CLC   TYPE,=C'A2'         OR A2'S                                      
         BNE   SP52X                                                            
                                                                                
SP52A    CLC   AGENCY,=C'JW'       OR JW                                        
         BNE   SP52C                                                            
         CLC   QUESTOR(3),=C'CA-'  AND CA-                                      
         BNE   SP52X                                                            
         B     SP52D                                                            
                                                                                
SP52C    CLC   AGENCY,=C'FM'                                                    
         BNE   SP52X                                                            
         CLC   QUESTOR(3),=C'UV-'                                               
         BNE   SP52X                                                            
                                                                                
SP52D    MVI   QOUTSPEC,C'I'                                                    
                                                                                
SP52X    DS    0H                                                               
                                                                                
SP54     CLC   TYPE,=C'54'         FOR 54'S                                     
         BNE   SP54X                                                            
         CLI   QDATA+61,C'Y'                                                    
         BNE   SP54X                                                            
         MVI   QOUTSPEC,C'I'                                                    
                                                                                
SP54X    DS    0H                                                               
                                                                                
SPJW     CLC   AGENCY,=C'JW'       JWT SPECIALS                                 
         BNE   SPJWX                                                            
                                                                                
SPJW2    CLC   QUESTOR(3),=C'RB-'  REMOTE BILLING FORCED TO JW                  
         BNE   SPJW6                                                            
         OC    QDEST,QDEST                                                      
         BNZ   *+10                                                             
         MVC   QDEST,=AL2(JWNY)    SEND TO JWNY                                 
         B     SPJW8                                                            
                                                                                
SPJW6    BAS   RE,OFFIX                                                         
                                                                                
SPJW8    CLC   TYPE,=C'D2'                                                      
         BE    SPJW60                                                           
         CLC   TYPE,=C'D3'                                                      
         BE    SPJW60                                                           
         CLC   TYPE,=C'D4'                                                      
         BE    SPJW60                                                           
         B     SPJWX                                                            
                                                                                
SPJW60   MVI   QSORTFLD,C'A'                                                    
         CLC   PRODUCT(10),=C'ALLALL ALL'                                       
         BNE   *+8                                                              
         MVI   QSORTFLD,C'B'       ALL ALL 60'S SORT LAST                       
                                                                                
SPJWX    DS    0H                                                               
                                                                                
SPTR     CLC   AGENCY,=C'TR'       TRACY                                        
         BNE   SPTRX                                                            
         BAS   RE,OFFIX                                                         
                                                                                
SPTRX    DS    0H                                                               
                                                                                
SPBD     CLC   AGENCY,=C'BD'       BBDO                                         
         BNE   SPBDX                                                            
         BAS   RE,OFFIX                                                         
                                                                                
SPBDX    DS    0H                                                               
                                                                                
SPBO     CLC   AGENCY,=C'BO'       BOCLARO                                      
         BE    SPBOBM                                                           
         CLC   AGENCY,=C'BM'       OF BRISTOL MYERS                             
         BNE   SPBOX                                                            
                                                                                
SPBOBM   CLC   QUESTOR(4),=C'COM-'                                              
         BNE   SPBOX                                                            
         MVI   QOUTSPEC,C'P'                                                    
                                                                                
SPBOX    DS    0H                                                               
                                                                                
SPMI     CLC   AGENCY,=C'MI'       MKTO                                         
         BNE   SPMIX                                                            
         CLC   =C'CONTROL',QUESTOR  TEST TURNAROUND                             
         BNE   SPMIX                NO                                          
         CLC   TYPE,=C'L2'          SUPPRESS L2 AND 44                          
         BE    DELETE                                                           
         CLC   TYPE,=C'44'                                                      
         BE    DELETE                                                           
                                                                                
SPMIX    DS    0H                                                               
                                                                                
SPDF     CLC   AGENCY,=C'DF'       SAATCHI                                      
         BNE   SPDFX                                                            
         CLC   TYPE,=C'W3'         W3 WRITER                                    
         BNE   SPDFX                                                            
         LH    R1,SPW3CNT          GENERATE 1 REQUEST PER JOB                   
         CLC   QDATA+12(2),=C'02'                                               
         BNE   SPDF2                                                            
         AHI   R1,1                                                             
         CHI   R1,36               NO MORE THAN 36 JOBS!                        
         BNH   *+8                                                              
         LHI   R1,1                                                             
         STH   R1,SPW3CNT                                                       
SPDF2    STC   R1,QSUBUSER                                                      
                                                                                
SPDFX    DS    0H                                                               
                                                                                
SPWI     CLC   AGENCY,=C'WI'       FOR WILA                                     
         BNE   SPWIX                                                            
         CLC   TYPE,=C'TR'         TRANSMIT REPORT                              
         BNE   SPWIX                                                            
         LH    R1,SWITRCNT         GENERATE 1 REQUEST PER JOB                   
         CLC   QDATA+12(2),=C'02'                                               
         BNE   SPWI2                                                            
         AHI   R1,1                                                             
         CHI   R1,36               NO MORE THAN 36 JOBS!                        
         BNH   *+8                                                              
         LHI   R1,1                                                             
         STH   R1,SWITRCNT                                                      
SPWI2    STC   R1,QSUBUSER                                                      
                                                                                
SPWIX    DS    0H                                                               
                                                                                
SPMX     CLC   TYPE,=C'MX'         SEPARATE MX FOR BS BY MEDIA                  
         BNE   SPMXX                                                            
         CLC   AGENCY,=C'BS'                                                    
         BNE   SPMXX                                                            
         MVC   QSUBUSER,MEDIA                                                   
SPMXX    DS    0H                                                               
                                                                                
SPX5     CLC   TYPE,=C'X5'         X5 - P&G TAPE                                
         BNE   SPX5X                                                            
         MVC   QSUBUSER,OPTIONS+1  SEPARATE THE DATA TYPES                      
                                                                                
SPX5X    DS    0H                                                               
*                                                                               
SPYT     CLC   TYPE,=C'YT'         SEPARATE YT BY MEDIA                         
         BNE   SPYTX                                                            
         MVC   QSUBUSER,MEDIA                                                   
SPYTX    DS    0H                                                               
                                                                                
I2       CLC   TYPE,=C'I2'         I2 - INVOICE MATHCING                        
         BNE   SPI2X                                                            
         CLC   AGENCY,=C'OU'                                                    
         BNE   SPI2                                                             
         LH    R1,I2OUGRP          GROUP OF 100 REQUEST                         
         LH    RF,I2OUREQ                                                       
         AHI   RF,1                ADD ONE TO NUMBER OF I2 REQUEST              
         STH   RF,I2OUREQ                                                       
         CHI   RF,100                                                           
         BNH   SPI2OU                                                           
         XC    I2OUREQ,I2OUREQ     RESET FOR NEXT 100 REQUEST                   
         AHI   R1,1                                                             
         STH   R1,I2OUGRP                                                       
SPI2OU   STC   R1,QSUBUSER         STORE GROUP OF 100                           
*                                                                               
SPI2     CLI   CONTREQ,C'*'        TEST RRS REQUEST WANTED ALSO                 
         BNE   SPI2X                                                            
         CLI   REQ2USER,C' '                                                    
         BNH   SPI2X                                                            
         LH    R1,EODLEN           YES-COPY REQUEST TO NEXT ENTRY               
         LR    R4,R2                                                            
         AR    R4,R1                                                            
         LR    RF,R4                                                            
         LR    R5,R1                                                            
         LR    R0,R2                                                            
         MVCL  R4,R0                                                            
         LR    R4,RF                                                            
*                                                                               
REQ2     USING EODREQD,R4                                                       
         DROP  R2                                                               
*                                                                               
REQ1     USING EODREQD,R2                                                       
         MVC   REQ2.QDATA1,SPACES    CLEAR REQUEST CARDS                        
         MVC   REQ2.QDATA2,SPACES                                               
         MVC   REQ2.QPROGRAM,=C'RW'  AND FORMAT RRS REQUEST                     
         MVC   REQ2.TYPE,=C'RW'                                                 
         MVC   REQ2.AGENCY,REQ1.AGENCY                                          
         LA    R1,REQ2.QDATA                                                    
         MVC   4(14,R1),=C'*.RRS.REP..OV,'                                      
         MVC   18(3,R1),REQ1.QUESTOR                                            
         MVC   21(7,R1),=C'.......'                                             
         MVC   28(1,R1),REQ1.MEDIA                                              
         MVI   29(R1),C'.'                                                      
         MVC   30(3,R1),REQ1.CLIENT                                             
         MVI   33(R1),C'.'                                                      
         MVC   34(3,R1),REQ1.PRODUCT                                            
         MVI   37(R1),C'.'                                                      
         MVC   38(3,R1),REQ1.ESTIMATE                                           
         CLI   40(R1),C' '                                                      
         BH    SPI2A                                                            
         BCTR  R1,0                                                             
         CLI   39(R1),C' '                                                      
         BH    SPI2A                                                            
         BCTR  R1,0                                                             
*                                                                               
SPI2A    MVI   41(R1),C'.'                                                      
         CLI   REQ1.MKTGRP,C' '                                                 
         BNH   SPI2B                                                            
         MVC   42(4,R1),=C'MGR='                                                
         MVC   46(1,R1),REQ1.MKTGRP                                             
         MVC   47(4,R1),REQ1.STATION                                            
         MVI   51(R1),C'.'                                                      
         AHI   R1,52                                                            
         B     SPI2C                                                            
*                                                                               
SPI2B    CLC   REQ1.MARKET(3),=C'ALL'                                           
         BE    *+14                                                             
         CLC   REQ1.MARKET,SPACES                                               
         BH    *+18                                                             
         MVC   42(2,R1),=C'..'                                                  
         AHI   R1,44                                                            
         B     SPI2C                                                            
         MVC   42(4,R1),REQ1.MARKET                                             
         MVI   46(R1),C'.'                                                      
         MVC   47(5,R1),REQ1.STATION                                            
         MVI   52(R1),C'.'                                                      
         AHI   R1,53                                                            
*                                                                               
SPI2C    MVC   DUB(4),REQ1.START                                                
         MVC   DUB+4(2),=C'01'                                                  
         LR    R5,R1                                                            
         GOTO1 VDATCON,PARAS,DUB,(6,(R5))                                       
         MVC   6(2,R5),=C'.*'                                                   
         DROP  REQ1                                                             
         DROP  REQ2                                                             
         USING EODREQD,R2                                                       
*                                                                               
SPI2X    DS    0H                                                               
*                                                                               
SPU2     CLC   =C'U2CK',TYPE       COKE U2 REQUEST                              
         BNE   SPU2X                                                            
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVC   TYPE,=C'I5'         GENERATE AN I5 REQUEST                       
         MVI   OPTIONS,C'Y'        OPT1=Y TO ADD BUYS                           
SPU2X    DS    0H                                                               
*                                                                               
SPBG     CLC   =C'BGCK*.',TYPE     BG                                           
         BNE   SPBGX                                                            
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVC   QDATA2,QDATA                                                     
         MVC   TYPE,=C'CC'         MAKE IT A CC REQUEST                         
         MVC   CLIENT(75),SPACES   CLEAR REQUEST                                
         MVC   MEDIA,QDATA2+28                                                  
         MVC   CLIENT,QDATA2+30                                                 
         MVC   PRODUCT,QDATA2+34                                                
         MVC   ESTIMATE,QDATA2+38                                               
         MVC   MARKET,QDATA2+42                                                 
         MVC   QDATA+31(2),=C'ES'                                               
         MVC   QUESTOR(9),=C'MEGAHERTZ'                                         
         MVC   QDATA2,SPACES                                                    
*                                                                               
SPBGX    DS    0H                                                               
*                                                                               
SPI5     CLC   =C'I5CK*',TYPE      BG                                           
         BNE   SPI5X                                                            
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVC   TYPE,=C'I2'         MAKE IT AN I2 REQUEST                        
         MVC   QDATA+54(14),SPACES  CLEAR THE OPTIONS                           
SPI5X    DS    0H                                                               
*                                                                               
SPBT     CLC   TYPE,=C'BT'                                                      
         BNE   SPBTX                                                            
         MVC   QSUBUSER(1),QDATA+61 QOPT1  FILE OR NO                           
         MVC   QSUB2(1),QDATA+62    QOPT2  FILE TYPE                            
*                                                                               
SPBTX    DS    0H                                                               
*                                                                               
SPEND    L     R2,SVEODREQ         POINT TO FIRST REQUEST IN STACK              
         BAS   RE,GETUSER                                                       
         B     XIT                                                              
                                                                                
I2OUREQ  DC    H'0'                COUNT EVERY 100                              
I2OUGRP  DC    H'0'                ALPHA OU GROUP OF 100                        
SPW3CNT  DC    H'0'                                                             
SPK4CNT  DC    H'0'                                                             
SPKLCNT  DC    H'0'                                                             
SD8WICNT DC    H'0'                                                             
SD5MICNT DC    H'0'                                                             
SWITRCNT DC    H'0'                                                             
         EJECT                                                                  
*              ROUTINE TO EXTRACT USER FROM POOL                                
                                                                                
GETUSER  NTR1  ,                                                                
         MVC   SAVESPEC,QOUTSPEC                                                
                                                                                
GETUSER2 MVC   QUSER,AGENCY                                                     
         MVC   QOUTSPEC,SAVESPEC   PROPOGATE SPECIAL CODE                       
         AH    R2,EODLEN                                                        
         OC    0(256,R2),0(R2)                                                  
         BZ    XIT                                                              
         BCT   R3,GETUSER2                                                      
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*              ROUTINE TO EXTRACT OFFICE CODE                                   
**********************************************************************          
OFFIX    L     R5,AREQHDR          DEVELOP OFFICE CODES FROM HEADER             
         CLI   RQHOFF,C'*'                                                      
         BCR   8,RE                                                             
         BR    RE                  THIS CODING NOW SUSPENDED                    
         CLI   RQHOFF,0                                                         
         BCR   8,RE                                                             
         CLI   RQHOFF,C'0'                                                      
         BCR   8,RE                                                             
         CLI   RQHOFF,C' '                                                      
         BCR   8,RE                                                             
         MVC   AGENCY+1(1),RQHOFF                                               
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINES FOR PRINT                                               
                                                                                
PRINT    MVC   QUSER,QDATA+2                                                    
         CLC   QDATA(2),=C'12'     FOR CONTRACTS                                
         BE    PRINT2                                                           
         CLC   QDATA(2),=C'13'                                                  
         BE    PRINT2                                                           
         CLC   QDATA(2),=C'15'                                                  
         BE    PRINT2                                                           
         CLC   QDATA(2),=C'72'     AND INSERTION ORDERS                         
         BNE   PRINT4                                                           
                                                                                
PRINT2   MVI   QSUBUSER,C'M'                                                    
         CLI   QDATA+4,C'N'                                                     
         BNE   *+8                                                              
         MVI   QSUBUSER,C'N'                                                    
         CLI   QDATA+4,C'O'                                                     
         BNE   *+8                                                              
         MVI   QSUBUSER,C'O'                                                    
         B     XIT                                                              
                                                                                
PRINT4   DS    0H                                                               
         CLC   QDATA(2),=C'16'                                                  
         BNE   PRINT6                                                           
         CLI   QDATA+59,C'Y'       MARLENE SAYS THE Y IS HERE                   
         BNE   XIT                                                              
         CLI   QDATA+63,C'Y'       TEST RUN                                     
         BE    XIT                                                              
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVC   TYPE,=C'15'                                                      
         MVC   QDATA+68(12),=CL12'AUTO'                                         
         MVC   QDATA+61(7),SPACES      OPTIONS SUPPRESSED                       
* WE HOPE THIS WILL GENERATE A JOB BY MEDIA                                     
         MVI   QSUBUSER,C'M'       USE M                                        
         CLI   QDATA+4,C'N'                                                     
         BNE   *+8                                                              
         MVI   QSUBUSER,C'N'       EXCEPT FOR NEWSPAPERS                        
         CLI   QDATA+4,C'O'                                                     
         BNE   *+8                                                              
         MVI   QSUBUSER,C'O'       AND OUTDOOR                                  
         B     XIT                                                              
PRINT6   DS    0H                                                               
         CLC   =C'49',TYPE         SEPERATE REQUESTED P49'S                     
         BNE   PRB1                                                             
         MVI   QSUBUSER,X'01'                                                   
         CLI   QDATA+62,C'R'       SEE IF REQUESTED THROUGH $REQ                
         BNE   XIT                 QOPT2 COL 63                                 
         MVI   QSUBUSER,X'02'                                                   
         B     XIT                                                              
*                                                                               
                                                                                
PRB1     CLC   =C'B1',TYPE                                                      
         BNE   PRB1X                                                            
*                                                                               
         MVC   QLANG,QDATA2+1      TRY TO GET JOB BY LANGUAGE                   
*                                                                               
         CLI   QDATA+60,C'*'       TEST 2 CARD REQUEST                          
         BNE   PRB1A               NO - CAN'T BE REVERSAL                       
*                                                                               
PRB1A    MVC   DUB+1(1),QDATA+60   SAVE 2ND CARD INDICATOR                      
*                                                                               
         TM    QDATA+78,X'40'      TEST 52 TO ACCOMPANY BILLING                 
         BO    PRB1A4              NO                                           
         OI    QDATA+78,X'40'      SET BIT BACK ON                              
         MVC   DUB(1),QDATA+77     SAVE ORIGIONAL QDATA+77                      
*                                  SO I CAN GENERATE BOTH 52 AND EC             
*                                                                               
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVC   TYPE,=C'52'                                                      
         MVC   QDATA+26(42),SPACES  BLANK ALL BUT REQUESTOR                     
         CLC   QDATA+20(3),SPACES   MIGHT BE BLANK IF FILTERS USED              
         BNE   *+10                                                             
         MVC   QDATA+20(3),=C'ALL'   52/EC NEED 'ALL'                           
         MVC   START(2),=C'ES'                                                  
         MVC   QDATA+60(1),DUB+1    RESTORE 2ND CARD INDIATOR                   
         XC    QCNTLDT,QCNTLDT      SET CONTROL DATE =X'00' AS FLAG             
         MVC   QDATA+77(1),DUB      RESTORE SAVED QDATA+77                      
*                                  NOW SEE IF I MUST CREATE EC ALSO             
*                                                                               
PRB1A4   TM    QDATA+77,X'40'      TEST EC TO ACCOMPANY BILLING                 
         BO    PRB1X               NO                                           
         OI    QDATA+77,X'40'      SET BIT BACK ON                              
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVC   TYPE,=C'EC'                                                      
         MVC   QDATA+26(42),SPACES  BLANK ALL BUT REQUESTOR                     
         CLC   QDATA+20(3),SPACES   MIGHT BE BLANK IF FILTERS USED              
         BNE   *+10                                                             
         MVC   QDATA+20(3),=C'ALL'   52/EC NEED 'ALL'                           
         MVC   QDATA+60(1),DUB+1     RESTORE 2ND CARD INDICATOR                 
         MVC   START(2),=C'ES'                                                  
         XC    QCNTLDT,QCNTLDT      SET CONTROL DATE =X'00' AS FLAG             
*                                                                               
PRB1X    DS    0H                                                               
*                                                                               
*        LIMIT WRITER REQUESTS TO 40 PER JOB                                    
*                                                                               
         CLC   =C'WR',QDATA          SKIP IF NOT A WRITER REQUEST               
         BNE   PRWRX                                                            
*                                                                               
         LA    R1,PWRTAB           POINT TO WRITER TABLE                        
PWR      USING PWRTAB,R1           ESTABLISH TABLE                              
*                                    CL2'AGENCY'                                
*                                    XL1'CURRENT NUMBER OF REQS'                
*                                    XL1'CURRENT QSUBUSER'                      
*                                                                               
*        FIND ENTRY IN TABLE FOR AGENCY                                         
*              IF NONE, CREATE ONE                                              
*                                                                               
         LHI   R0,PWRTABMQ         MAX NUMBER OF ENTRIES IN TABLE               
*                                                                               
PWRTABLP DS    0H                                                               
*                                                                               
         OC    PWR.PWRTAGY,PWR.PWRTAGY  IF EMPTY TABLE ENTRY REACHED            
         BZ    PWRTABNW               ADD NEW ENTRY TO TABLE                    
*                                                                               
         CLC   PWR.PWRTAGY,QDATA+2 MATCH ON AGENCY                              
         BE    PWRTABFD                                                         
*                                                                               
PWRTABCN DS    0H                                                               
*                                                                               
         LA    R1,PWR.PWRTAB+PWRTABL   NEXT ENTRY IN TABLE                      
         BCT   R0,PWRTABLP                                                      
*                                                                               
PWRTABDN DS    0H                  TABLE FULL                                   
         B     PRWRX               USE CURRENT QSUBUSER                         
*                                                                               
PWRTABNW DS    0H                  NEW ENTRY IN TABLE                           
*                                                                               
         MVC   PWR.PWRTAGY,QDATA+2 SET AGENCY ID                                
         LHI   RF,1                FIRST REQUEST                                
         MVI   PWR.PWRTSUSR,1      STARTING SUBUSER NUMBER                      
*                                                                               
         B     PWRTAB90                                                         
*                                                                               
PWRTABFD DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PWR.PWRTRQNM   CURRENT NUMBER OF REQS FOR AGENCY            
         AHI   RF,1                BUMP NUMBER                                  
*                                                                               
         CHI   RF,PWRREQMQ         IF AT OR PAST MAX ALLOWED                    
         BNH   PWRTAB90                                                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,PWR.PWRTSUSR   GET CURRENT QSUBUSER NUM FOR AGY             
         AHI   RE,1                   BUMP SUBUSER COUNT                        
         STCM  RE,1,PWR.PWRTSUSR                                                
         LHI   RF,1                SET TO FIRST OF TYPE                         
*                                     FORCES NEW JOB FOR WRITER REQS            
PWRTAB90 DS    0H                                                               
*                                                                               
         STCM  RF,1,PWR.PWRTRQNM   UPDATE NUMBER OF REQUESTS                    
         MVC   QSUBUSER,PWR.PWRTSUSR   SET QSUBUSER                             
*                                                                               
PRWRX    DS    0H                                                               
PR10     CLC   =C'10',QDATA          SKIP IF NOT A P10 REQUEST                  
         BNE   PR10X                                                            
*                                                                               
         MVC   QSUBUSER(1),QDATA+64   FILE TYPE (BLANK= NO FILE)                
*                                     QOPT4                                     
PR10X    DS    0H                                                               
         B     XIT                                                              
*                                                                               
PWRREQMQ EQU   40                  MAX REQUESTS IN A JOB                        
PWRTABMQ EQU   100                 MAX ENTRIES IN TABLE                         
*                                                                               
PWRTAB   DS    0F                  PRINT WRITER TABLE                           
PWRTAGY  DS    CL2                 CL2'AGENCY'                                  
PWRTRQNM DS    XL1                 XL1'CURRENT NUMBER OF REQS'                  
PWRTSUSR DS    XL1                 XL1'CURRENT QSUBUSER'                        
PWRTABL  EQU   *-PWRTAB            LENGTH OF TABLE ENTRY                        
         ORG   PWRTAB                                                           
         DC    100XL(PWRTABL)'00'  TABLE                                        
*                                                                               
         DC    F'0'                EXTRA TAB ENTRY FOR INSURANCE                
*                                                                               
         DROP  PWR                                                              
*                                                                               
         EJECT                                                                  
*              ROUTINES FOR REP                                                 
                                                                                
REP      MVC   QUSER,QDATA+2                                                    
         CLC   SAVESIN,QDATA       SPOOF REQUEST                                
         BNE   REP1                NO                                           
         CLC   =C'PD',QDATA        TEST PD                                      
         BE    RPD                                                              
         CLC   =C'LT',QDATA        TEST A GIANT'S LINEBACKER                    
         BE    RLT                                                              
         B     XIT                                                              
REP1     CLI   QDATA,C'O'          IGNORE FOR OVERNIGHTS                        
         BE    REP8                                                             
         CLI   QDATA,C'S'             AND FOR NEW RESEARCH                      
         BE    REP8                                                             
         CLI   QDATA,C'A'             AND FOR NEW SRA (ATHENA)                  
         BE    REP8                                                             
         CLC   QDATA(2),=C'BA'        AND SFM BUDGET ALLOCATOR                  
         BE    REP8                                                             
         CLC   QDATA(2),=C'DI'        AND SFM DIRECT RESPONSE                   
         BE    REPX                                                             
*                                                                               
         MVC   QSUBUSER,QDATA+73   SEPARATE RADIO FROM TV                       
         CLI   QSUBUSER,C' '                                                    
         BNE   *+8                 IF STATION GROUP IS BLANK                    
         MVI   QSUBUSER,C'T'        THEN LUMP IT WITH TV                        
         CLC   QDATA(2),=C'11'                                                  
         BNE   REP2                                                             
         CLC   QDATA+4(2),=C'NY'                                                
         BE    REP8                                                             
         MVI   QSUBUSER,C' '                                                    
         B     REP8                                                             
*                                                                               
REP2     LA    R1,RRGTAB                                                        
*                                                                               
REP4     CLI   0(R1),X'FF'         SEARCH FOR RRG REQUEST NOS                   
         BE    REP8                                                             
         CLC   QDATA(2),0(R1)                                                   
         BNE   REP6                                                             
         MVC   QDATA+77(2),QDATA   RRG REQUEST                                  
         MVI   QDATA,C'R'                                                       
         MVC   QDATA+1(1),2(R1)                                                 
         B     REP8                                                             
*                                                                               
REP6     AHI   R1,3                                                             
         B     REP4                                                             
*                                                                               
REP8     DS    0H                                                               
         CLC   QDATA(2),=C'15'     SEPARATE JOB FOR EACH RE15                   
         BNE   REP8A                RE15-MULTI STEP JOB                         
         LH    R1,REI1CNT                                                       
         AHI   R1,1                                                             
         STH   R1,REI1CNT                                                       
         STC   R1,QSUBUSER                                                      
REP8A    DS    0H                                                               
         CLC   QDATA(2),=C'37'     SEPARATE JOB FOR EACH RE37                   
         BNE   REPX                 RE37-INTERFACE TAPE OUTPUT                  
         LH    R1,REI1CNT                                                       
         AHI   R1,1                                                             
         STH   R1,REI1CNT                                                       
         STC   R1,QSUBUSER                                                      
*                                                                               
REPX     CLI   QDATA,C'R'          RRG REQUEST?                                 
         BE    RRGCOUNT            YES - SET COUNT W/IN RUN GROUP               
         B     XIT                                                              
                                                                                
REI1CNT  DC    H'0'                                                             
                                                                                
RRGTAB   DC    CL3'121'            TABLE OF RRG REQUEST NUMBERS                 
         DC    CL3'131'                                                         
         DC    CL3'221'                                                         
         DC    CL3'231'                                                         
         DC    CL3'262'                                                         
         DC    CL3'272'                                                         
         DC    CL3'282'                                                         
         DC    CL3'292'                                                         
         DC    CL3'461'                                                         
         DC    CL3'503'                                                         
         DC    CL3'513'                                                         
         DC    CL3'524'                                                         
         DC    CL3'534'                                                         
         DC    CL3'544'                                                         
         DC    CL3'554'                                                         
         DC    CL3'564'                                                         
         DC    CL3'574'                                                         
         DC    CL3'583'                                                         
         DC    CL3'593'                                                         
         DC    CL3'602'                                                         
         DC    CL3'612'                                                         
         DC    CL3'623'                                                         
         DC    CL3'633'                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
RPD      L     RE,RPDJOBID                                                      
         MVC   QSUBUSER,0(RE)      MOVE LETTER FOR JOBNAME                      
         AHI   RE,1                POINT TO NEXT                                
         CLI   0(RE),X'FF'                                                      
         BNE   *+8                                                              
         LA    RE,RPDJOBS                                                       
         ST    RE,RPDJOBID                                                      
         B     XIT                                                              
                                                                                
*==========================================================                     
* REP LT REQUEST NEEDS TO SORT ON STATION TO AVOID DUPS                         
*==========================================================                     
                                                                                
RLT      LHI   R0,20                                                            
         LA    R1,QDATA+38                                                      
*                                                                               
RLT2     CLC   0(4,R1),=C' 110'    FIND STATION FIELD                           
         BE    RLT4                                                             
         AHI   R1,1                                                             
         BCT   R0,RLT2                                                          
         B     XIT                                                              
*                                                                               
RLT4     XC    QSORTFLD,QSORTFLD   CLEAR SORT FIELD                             
         MVC   QSORTFLD(6),4(R1)   MOVE STATION                                 
         B     XIT                                                              
*                                                                               
RPDJOBID DC    A(RPDJOBS)          ADDRESS OF NEXT JOB LETTER                   
RPDJOBS  DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'                          
         DC    X'FF'                                                            
         EJECT                                                                  
*   EACH RRG RUN MUST BE COUNTED WITHIN ITS RUN GROUP.  NO MORE                 
*     THAN 39 REQUESTS ARE TO BE WRAPPED WITHIN A JOB.                          
                                                                                
RRGCOUNT SR    RF,RF                                                            
         IC    RF,QDATA+1          RRG RUN GROUP #                              
         SLL   RF,28               STRIP OFF ZONES                              
         SRL   RF,28               PUT IT BACK                                  
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         SLL   RF,2                MULTIPLY BY 4 FOR DISPLACEMENT               
         LA    RE,TVBUCKET         A(RRG COUNTS FOR TV)                         
         CLI   QSUBUSER,C'T'       TV REQUEST?                                  
         BE    RRGC0020            YES                                          
         LA    RE,RABUCKET         NO  - A*RRG COUNTS FOR RADIO                 
RRGC0020 AR    RE,RF               ADD DISPLACEMENT                             
         LH    R1,0(RE)            LOAD QSUBUSER FOR RRG GROUP                  
         LH    RF,2(RE)            LOAD REQUEST COUNT                           
         AHI   RF,1                BUMP # REQS COUNTER                          
         STC   R1,QSUBUSER         INSERT QSUBUSER                              
         CLC   =C'RZ',QDATA        RRG RUN GROUP 'RZ'?                          
         BE    RRGC0040            YES - IGNORE COUNT TEST                      
         CHI   RF,24               NO  - MAX FOR RRG GROUP REACHED?             
         BNE   RRGC0090            NO  -                                        
RRGC0040 SR    RF,RF               YES - SET # REQS COUNTER TO ZERO             
         AHI   R1,1                BUMP QSUBUSER FOR RRG GROUP                  
         STH   R1,0(RE)            REPLACE NEW QSUBUSER                         
RRGC0090 STH   RF,2(RE)            REPLACE # REQS COUNTER                       
         B     XIT                 EXIT                                         
*                                                                               
         DS    0F                  FULL-WORD ALIGN                              
TVBUCKET DC    10X'00010000'                                                    
RABUCKET DC    10X'00820000'                                                    
*                                                                               
*   TVBUCKET/RABUCKET:  TEN SETS OF 2 HALF-WORDS:                               
*        H-WORD  1  =  RRG GROUP NUMBER (STARTS WITH 1)                         
*        H-WORD  2  =  REQUEST COUNT W/IN RRG GROUP NUMBER                      
*                                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINES FOR ACCPAK                                              
*                                                                               
         USING ACQD,R4                                                          
ACC      DS    0H                                                               
         LA    R4,QDATA                                                         
         L     R5,AREQHDR                                                       
         CLC   SAVESIN,QDATA       SPOOF REQUEST                                
         BE    XIT                                                              
         CLC   ACQPROG,=C'09'      REQUESTS UNDER 10                            
         BH    ACC1                                                             
         CLC   ACQPROG,=C'01'                                                   
         BL    ACC1                                                             
         MVI   QSUBUSER,X'FF'      ARE PSEUDO                                   
*                                                                               
ACC1     CLC   ACQPROG,=C'54'      54'S ARE ALSO PSEUDO                         
         BNE   *+14                                                             
         MVI   QSUBUSER,X'FF'                                                   
         MVC   QSUB2,ACQLDG        AND SPLIT BY LEDGER                          
*                                                                               
         CLC   ACQPROG,=C'08'      08'S SPLIT BY GROUP                          
         BNE   *+10                                                             
         MVC   QSUB2,ACQLDG        LEDGER                                       
*                                                                               
         CLC   ACQPROG,=C'DJ'      DAILY JOUNAL                                 
         BNE   *+14                                                             
         MVC   QSUB2,ACQLDG        LEDGER                                       
         MVI   QSUBUSER,X'FF'      ALSO PSEUDO                                  
*                                                                               
         LA    R1,ACCTAB           TABLE REQUEST(FROM ODDS FILE)                
ACC1A    CLC   ACQPROG,0(R1)       MATCH REQUEST TO TABLE                       
         BNE   *+12                                                             
         MVI   QSUBUSER,X'FF'      DON'T GENERATE REQUEST                       
         B     ACC1B                                                            
         AHI   R1,2                                                             
         CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   ACC1A                                                            
*                                                                               
ACC1B    CLC   ACQPROG,=C'55'      CHECK REQUESTS                               
         BNE   ACC2                                                             
         CLI   ACQSEL+3,C' '       TEST POSSIBLE PRODUCT                        
         BE    ACC1X                                                            
         CLI   ACQSEL,C' '         NO CLIENT PRODUCT                            
         BNH   ACC1X                                                            
         CLI   ACQSEL,C'+'         CLIENT INCLUDE LIST                          
         BE    ACC1X                                                            
         CLI   ACQSEL,C'-'         CLIENT EXCLUDE LIST                          
         BE    ACC1X                                                            
         TM    ACQSEL+3,X'40'      ALL FOR THIS CLT EXCEPT THIS PRODUCT         
         BZ    ACC1X                                                            
         TM    ACQSEL,X'40'        ALL EXCEPT THIS CLIENT PRODUCT               
         BZ    ACC1X                                                            
*&&US*&& MVI   ACQCHKSQ,1          PRODUCT SORTS FIRST                          
*&&UK*&& MVI   ACQSRTSQ,1          PRODUCT SORTS FIRST                          
         CLI   ACQACT,C' '         UNLESS THERE IS ALSO A VENDOR                
         BE    ACC1X                                                            
*&&US*&& MVI   ACQCHKSQ,0          VENDOR/PRODUCT SORTS FIRST                   
*&&UK*&& MVI   ACQSRTSQ,0          VENDOR/PRODUCT SORTS FIRST                   
*                                                                               
ACC1X    ST    R2,SVR2             SAVE A(CHECK REQUEST)                        
         MVC   SAVEQ,QDATA                                                      
         MVC   QSUBUSER,ACQLDG     SET TO BREAK JOBS BY LEDGER                  
*                                                                               
         BAS   RE,COPYREQ          COPY REQ. TO SECOND SLOT AND BUMP R2         
         LA    R4,QDATA            SET ACQD USING OFF OF R2                     
         XC    QOUTTYPE,QOUTTYPE                                                
         MVC   ACQCARD1,SPACES                                                  
         XC    ACQCARD2,ACQCARD2                                                
         XC    ACQCARD3,ACQCARD3                                                
         MVC   ACQCPY(3),SAVEQ+9   COMPANY/UNIT/LEDGER                          
         MVC   ACQPROG,=C'56'      FOR THE REGISTER                             
         L     R2,SVR2             RESTORE A(CHECK/REGISTER)                    
         LA    R4,QDATA            RESET ACQD USING OFF OF R2                   
         B     ACC6                                                             
*                                                                               
ACC2     CLC   ACQPROG,=C'57'      FOR BANK RECS                                
         BNE   ACC3                                                             
         LH    R1,AC57CNT          SEPARATE JOB FOR EACH AC57                   
         AHI   R1,1                                                             
         STH   R1,AC57CNT                                                       
         STC   R1,QSUBUSER                                                      
         MVI   QOUTSPEC,C' '       ONLY REQUESTS                                
         CLI   ACQOPT2,C'Y'        WITH A 'Y' IN OPT. 2                         
         BNE   *+8                                                              
         MVI   QOUTSPEC,C'I'       SHOULD BE ON INTERFACE LIST                  
         B     ACC6                                                             
*                                                                               
ACC3     CLC   ACQPROG,=C'I1'      INTERFACE TAPES                              
         BNE   ACC4                                                             
         MVI   QSUBUSER,C' '                                                    
         CLI   ACQOPT1,C'Y'        OPTION 1 = Y (TAPE)                          
         BNE   ACC6                                                             
         LH    R1,ACI1CNT          SEPARATE JOB FOR EACH ACI1                   
         AHI   R1,1                                                             
         STH   R1,ACI1CNT                                                       
         STC   R1,QSUBUSER                                                      
         B     ACC6                                                             
*                                                                               
ACC4     CLC   ACQPROG,=C'IR'                                                   
         BNE   ACC5                                                             
         LH    R1,ACIRCNT          SEPARATE JOB FOR EACH ACIR                   
         AHI   R1,1                                                             
         STH   R1,ACIRCNT                                                       
         STC   R1,QSUBUSER                                                      
         B     ACC6                                                             
*                                                                               
ACC5     CLC   ACQPROG,=C'TT'                                                   
         BNE   ACC5A                                                            
         LH    R1,ACTTCNT          SEPARATE JOB FOR EACH ACTT                   
         AHI   R1,1                                                             
         STH   R1,ACTTCNT                                                       
         STC   R1,QSUBUSER                                                      
         B     ACC6                                                             
*                                                                               
ACC5A    CLC   ACQPROG,=C'AB'                                                   
         BNE   ACC5B                                                            
         LH    R1,ACABCNT          SEPARATE JOB FOR EACH ACTT                   
         AHI   R1,1                                                             
         STH   R1,ACABCNT                                                       
         STC   R1,QSUBUSER                                                      
         B     ACC6                                                             
*                                                                               
ACC5B    CLC   ACQPROG,=C'VE'                                                   
         BNE   ACC5C                                                            
         LH    R1,ACVECNT          SEPARATE JOB FOR EACH ACVE                   
         AHI   R1,1                                                             
         STH   R1,ACVECNT                                                       
         STC   R1,QSUBUSER                                                      
         B     ACC6                                                             
*                                                                               
ACC5C    CLC   ACQPROG,=C'T7'                                                   
         BNE   ACC5D                                                            
         LH    R1,ACT7CNT          SEPARATE JOB FOR EACH ACT7                   
         AHI   R1,1                                                             
         STH   R1,ACT7CNT                                                       
         STC   R1,QSUBUSER                                                      
         B     ACC6                                                             
*                                                                               
ACC5D    DS    0H                                                               
*                                                                               
ACC6     DS    0H                                                               
*                                  CHECK FOR Z REQUEST REPORTS                  
ACC12    CLC   ACQPROG,=C'14'      FOR SELECTED PROGRAMS                        
         BE    ACC14                                                            
         CLC   ACQPROG,=C'15'                                                   
         BE    ACC14                                                            
         CLC   ACQPROG,=C'22'                                                   
         BE    ACC14                                                            
         CLC   ACQPROG,=C'67'                                                   
         BE    ACC14                                                            
         CLC   ACQPROG,=C'63'                                                   
         BNE   ACC16                                                            
*                                                                               
ACC14    CLC   ACQACT,SPACES       IF ALL CLIENTS                               
         BNE   *+8                                                              
         MVI   ACQPROG,C'Z'        CHANGE REQUESTS TO Z*                        
         B     ACC17                                                            
*                                                                               
ACC16    CLC   ACQPROG,=C'25'      GENERAL LEDGER UPDATE                        
         BNE   ACC17                                                            
         CLI   ACQOPT2,C'D'        DRAFT - OPTION 2                             
         BNE   XIT                                                              
         MVC   ACQPROG,=C'DU'      CHANGE TO DRAFT UPDATE                       
         B     XIT                                                              
*                                                                               
ACC17    DS    0H                                                               
         CLC   ACQPROG,=C'98'      PEEL                                         
         BNE   ACC20                                                            
         CLI   ACQOPT1,C'D'        DRAFT - OPTION 1                             
         BNE   XIT                                                              
         MVC   ACQPROG,=C'DP'      CHANGE TO DRAFT PEEL                         
         B     XIT                                                              
*                                                                               
ACC20    DS    0H                                                               
                                                                                
ACC40    B     XIT                                                              
         DROP  R4                                                               
                                                                                
AC57CNT  DC    H'0'                                                             
ACI1CNT  DC    H'0'                                                             
ACIRCNT  DC    H'0'                                                             
ACTTCNT  DC    H'0'                                                             
ACABCNT  DC    H'0'                                                             
ACVECNT  DC    H'0'                                                             
ACT7CNT  DC    H'0'                                                             
*                                                                               
ACCTAB   DC    C'AUMAMGMRMUMWDATJ',X'FF'                                        
         EJECT                                                                  
***********************************************************************         
*              ROUTINES FOR CONTROL                                             
***********************************************************************         
CT74     NTR1  ,                                                                
         MVI   TYPE+2,C' '         REMOVE THE *                                 
         CLC   QDATA+61(2),SPACES                                               
         BE    XIT                                                              
         MVC   QPROGRAM,=C'74'                                                  
         MVC   DUB,SPACES                                                       
         MVC   DUB(2),=C'S='       SET UP S=SX TO SEARCH FOR SYSTEM             
         MVC   DUB+2(2),QDATA+61   MOVE IN SYS CHR AND ONE CHR SYS ID           
         MVC   DUB1,DUB            NB. DONT WORK FOR TWO CHR SYSTEMS            
*                                                                               
         CLI   QDATA+70,C'#'       TEST IF SE NUMBER PASSED IN QDATA+62         
         BNE   CT74R02                                                          
         MVC   DUB,SPACES                                                       
         MVC   DUB(4),=C'SE=0'     SET UP SE=0X TO SEARCH FOR SE NUM X          
         MVC   DUB+4(1),QDATA+62   MOVE IN SE NUMBER                            
         MVC   DUB1,DUB                                                         
         GOTO1 VHEXOUT,PARAS,DUB+4,DUB1+3,1,(24,0)                              
                                                                                
CT74R02  GOTO1 VDDNAME,PARAS,=C'DDNAME',DUB                                     
         L     R3,8(R1)                                                         
         USING DDNAMED,R3                                                       
         CLI   8(R1),0             ERROR ?                                      
         BE    CT74R06                                                          
                                                                                
         MVC   P+20(22),=C'SYSTEM NOT RECOGNISED '                              
         MVC   P+42(8),DUB1        DUB1 HAS SE NUM OR NAME OF SYSTEM            
         GOTOR VLOGIO,PARAS,1,(70,P)                                            
         GOTOR VPRINTER                                                         
         MVI   QSUBSYS,0                                                        
         MVC   QSYSTEM,QDATA+61                                                 
         B     CT74R08                                                          
                                                                                
CT74R06  MVC   QSYSTEM(1),QDATA+61                                              
         MVC   QSUBSYS(1),DDNASENO                                              
                                                                                
CT74R08  MVC   QUSER,QDATA+59                                                   
         CLI   QSYSTEM,C'S'                                                     
         BE    CT74R20                                                          
         CLI   QSYSTEM,C'F'        TRAFFIC LOOKS A LOT                          
         BE    CT74R20             LIKE SPOT                                    
         CLI   QSYSTEM,C'N'        TEST NETWORK                                 
         BE    CT74R20                                                          
         CLI   QSYSTEM,C'P'                                                     
         BNE   CT74R10                                                          
         MVI   MEDIA,C'M'          FORCE A VALID MEDIA FOR PPG                  
         B     CT74R30                                                          
                                                                                
CT74R10  MVC   QDATA+59(3),QDATA+63                                             
         MVC   QDATA+62(4),SPACES                                               
         CLI   QSYSTEM,C'A'                                                     
         BE    CT74R50                                                          
         CLI   QSYSTEM,C'M'                                                     
         BE    CT74R40                                                          
         B     CT74RXIT                                                         
                                                                                
CT74R20  MVC   CLIENT,=C'ALL'                                                   
         CLI   QSYSTEM,C'N'                                                     
         BNE   CT74R30             ONLY PROCESS FOR NET                         
         CLI   DDNASYN3,C'N'       QSUBSYS NET CHANGE QSYSTEM TO SPOT           
         BNE   CT74R30                                                          
         MVI   QSYSTEM,C'S'        MAKE NET SPOT FOR NOW                        
         MVI   MEDIA,C'N'          SPONSOR MOVES IN A 'T'                       
         DROP  R3                                                               
*                                                                               
CT74R30  MVC   AGENCY,QUSER                                                     
         MVC   QDATA+61(3),QDATA+63                                             
         MVC   QDATA+59(2),SPACES                                               
         MVC   QDATA+64(2),SPACES                                               
         MVC   QUESTOR,=C'FILE CONTROL '                                        
         MVC   QUESTOR-2(2),SPACES                                              
         B     CT74RXIT                                                         
                                                                                
CT74R40  MVC   AGENCY,QUSER                                                     
*                                                                               
CT74R50  MVC   QUESTOR-2(12),=C'FILE CONTROL '                                  
*                                                                               
CT74RXIT MVC   QDATA+9(2),SPACES                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              ROUTINES FOR MEDLINE                                             
***********************************************************************         
MED      MVC   QUSER,QDATA+2                                                    
*        CLI   QSUBSYS,C' '        FOR NEW MEDLINE                              
*        BNH   MED1                                                             
         CLC   QDATA(2),=C'19'     19'S RUN BY MEDIA                            
         BE    MED2                                                             
         B     MED4                                                             
*&&DO                                                                           
MED1     CLC   QDATA(2),=C'10'     SOME JOBS RUN BY MEDIA                       
         BE    MED2                                                             
         CLC   QDATA(2),=C'18'                                                  
         BE    MED2                                                             
         CLC   QDATA(2),=C'19'                                                  
         BE    MED2                                                             
         CLC   QDATA(2),=C'40'                                                  
         BE    MED2                                                             
         CLC   QDATA(2),=C'50'                                                  
         BE    MED2                                                             
         CLC   QDATA(2),=C'60'                                                  
         BE    MED2                                                             
         B     MED4                                                             
*&&                                                                             
MED2     MVC   QSUBUSER,QDATA+4                                                 
                                                                                
MED4     DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              ROUTINES FOR TALENT                                              
***********************************************************************         
TALENT   MVC   QUSER,QDATA+2                                                    
                                                                                
         CLC   =C'RL',QDATA        IF THIS IS RELEASE LETTERS                   
         BNE   TALRLX                                                           
         BAS   RE,COPYREQ          COPY REQUEST                                 
         MVC   QDATA(2),=C'RS'     CREATE A RELEASE SUMMARY REQUEST             
         MVC   QDATA+16(3),=C'RS '                                              
         B     TALX                                                             
TALRLX   DS    0H                                                               
*                                                                               
         LA    RF,TACOUNTS         TEST IF JOB REQUIRES SEP JOB PER REQ         
TAL40    CLI   0(RF),X'FF'                                                      
         BE    TALX                                                             
         CLC   QDATA(2),0(RF)                                                   
         BE    *+12                                                             
         AHI   RF,4                                                             
         B     TAL40                                                            
         LH    R1,2(RF)            USE SAVED SUB NUMBER COUNT                   
         CLC   QDATA+12(2),=C'02'  IF FIRST FIELD IS RECORD                     
         BE    TAL45                                                            
         CLC   QDATA+12(2),=C'00'  OR RLP ONE                                   
         BNE   *+8                    BUMP SUB NUMBER                           
TAL45    AHI   R1,1                                                             
         STH   R1,2(RF)                                                         
         STC   R1,QSUBUSER                                                      
                                                                                
TALX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*    ROUTINE TO COPY REQUEST TO NEXT STACK ENTRY AND POINT R2 TO IT             
***********************************************************************         
COPYREQ  LH    R1,EODLEN                                                        
         LR    R0,R2                                                            
         AR    R0,R1                                                            
         LR    RF,R3               SAVE R3 IN CASE USED ELSEWHERE               
         LR    R3,R1                                                            
         MVCL  R0,R2                                                            
         LR    R3,RF               R2 POINTS TO NEXT STACK POSITION             
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              TALENT JOBS THAT REQUIRE SEPERATE REQUEST                        
***********************************************************************         
TACOUNTS DS    0H                  JOBS REQUIRING SEPARATE JOB PER REQ.         
         DC    C'AI',H'0'                                                       
         DC    C'BK',H'0'                                                       
         DC    C'BT',H'0'                                                       
         DC    C'DB',H'0'                                                       
         DC    C'DM',H'0'                                                       
         DC    C'HD',H'0'                                                       
         DC    C'HF',H'0'                                                       
         DC    C'KD',H'0'                                                       
         DC    C'NE',H'0'                                                       
         DC    C'NH',H'0'                                                       
         DC    C'NT',H'0'                                                       
         DC    C'OI',H'0'                                                       
         DC    C'PD',H'0'                                                       
         DC    C'PK',H'0'                                                       
         DC    C'PX',H'0'                                                       
         DC    C'SN',H'0'                                                       
         DC    C'UC',H'0'                                                       
         DC    C'UD',H'0'                                                       
         DC    C'UI',H'0'                                                       
         DC    C'UK',H'0'                                                       
         DC    C'UT',H'0'                                                       
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*  WORK SPACE ETC                                                               
***********************************************************************         
DUB      DS    D                                                                
DUB1     DS    D                                                                
AREQHDR  DS    A                                                                
SVEODREQ DS    A                   SAVE AREA FOR REQ LIST POINTER               
SVR2     DS    A                                                                
PARAS    DS    6F                                                               
EZEROES  DC    C'000000'                                                        
EODLEN   DS    H                                                                
SAVEQ    DS    CL80                                                             
SAVESPEC DS    CL1                                                              
SAVEPROD DS    CL3                                                              
SAVESIN  DC    XL11'00'                                                         
SAVEPRNT DS    CL8                                                              
TODAY    DS    CL6                 TODAY'S DATE                                 
*                                                                               
BSYR     EQU   2855                                                             
BSLB     EQU   2781                                                             
BSJW     EQU   2866                                                             
BSNY     EQU   0294                                                             
JWNY     EQU   0022                                                             
BRM      EQU   0024                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINES PASSED FROM PEEL PROGRAM                                             
***********************************************************************         
VXTRAS   DS    0A                                                               
VSSB     DS    A                                                                
VUTL     DS    A                                                                
VDATAMGR DS    A                                                                
VDADDS   DS    A                                                                
VDATCON  DS    A                                                                
VADDAY   DS    A                                                                
VDDNAME  DS    A                                                                
VGETDAY  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VSORTER  DS    A                                                                
VCARDS   DS    A                                                                
VPANIC   DS    A                                                                
VDDSIO   DS    A                                                                
VSTXITER DS    A                                                                
VLOGIO   DS    A                                                                
VWORKER  DS    A                                                                
VHEXIN   DS    A                                                                
VHEXOUT  DS    A                                                                
VPRNTBL  DS    A                                                                
VXTRALNQ EQU   *-VXTRAS                                                         
         EJECT                                                                  
         LTORG                                                                  
* EDREQD                                                                        
       ++INCLUDE EDREQD                                                         
         ORG   QDATA                                                            
TYPE     DS    CL2                 QDATA+00                                     
AGENCY   DS    CL2                 QDATA+02                                     
MEDIA    DS    CL1                 QDATA+04                                     
CLIENT   DS    CL3                 QDATA+05                                     
PRDGRP   DS    CL1                 QDATA+08                                     
MKTGRP   DS    CL1                 QDATA+09                                     
CLIOFF   DS    CL1                 QDATA+10                                     
PRODUCT  DS    CL3                 QDATA+11                                     
MARKET   DS    CL4                 QDATA+14                                     
STATION  DS    CL5                 QDATA+18                                     
ESTIMATE DS    CL3                 QDATA+23                                     
ESTEND   DS    CL3                 QDATA+26                                     
         DS    CL1                 QDATA+29                                     
CONTREQ  DS    CL1                 QDATA+30                                     
         DS    CL6                 QDATA+31                                     
START    DS    CL6                 QDATA+37                                     
END      DS    CL6                 QDATA+43                                     
         DS    CL12                QDATA+49                                     
OPTIONS  DS    CL7                 QDATA+61                                     
QUESTOR  DS    CL12                QDATA+68                                     
*                                                                               
         ORG   QDATA+57                                                         
QCNTLDT  DS    CL3                 FOR PRINT                                    
*                                                                               
         ORG   QDATA2                                                           
         DS    CL20                                                             
REQ2USER DS    CL50                                                             
         DS    CL10                                                             
         ORG                                                                    
         EJECT                                                                  
*                                                                               
* DMREQHDRA                        NEW REQUEST HEADER                           
*                                                                               
REQHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
       EJECT                                                                    
* ACGENFILE                        ACCOUNTING FILE DSECTS                       
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACSCRDSECT                       SCRIBE TYPE ELEMENT                          
         PRINT OFF                                                              
       ++INCLUDE ACSCRDSECT                                                     
         PRINT ON                                                               
* ACQD                             SOFTER NAMES FOR ACC                         
         PRINT OFF                                                              
       ++INCLUDE ACQD                                                           
         PRINT ON                                                               
* DMDDNAMED                        DDNAME                                       
         PRINT OFF                                                              
DDNAMED  DSECT                                                                  
       ++INCLUDE DMDDNAMED                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060EDREQXPLOD01/08/21'                                      
         END                                                                    
