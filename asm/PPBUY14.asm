*          DATA SET PPBUY14    AT LEVEL 075 AS OF 06/02/20                      
*PHASE T41114A                                                                  
*INCLUDE CHOPPER                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY14 - WSJ   BUY/CHA/DEL'                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 03/02/20 SPEC-37363 NO RATE CHANGE FOR FROZEN CLIENTS                    
*                                                                               
* JSAY 08/30/19 SKIP VALIDATION OF EXTENDED ALLOCATIONS                         
*                                                                               
* KWAN 10/05/06 NODE CHANGE FOR PRD ALLOCATION PHASE (T41104)                   
*                                                                               
* KWAN 10/03/05 DEL MAT= REPEAT PASSIVE FOR DEL, INS DATE CHG & AD CHG          
*                                                                               
* KWAN 08/19/05 COS2 FACTOR ROUNDING OPTION IN F0 PROFILE                       
*                                                                               
* SMYE 09/04    FOR PBDMDATE CHANGE NON-WORK DAY                                
*                 DATES TO FIRST PRIOR WORK DAY IN EDTDT ROUTINE                
*                 IF PROFILE SO INDICATES                                       
*                                                                               
* SMYE 09/02/04 ADD WEBIO ELEM (X'71') TO CKIOEL ERROR EDITING                  
*                                                                               
* KWAN 07/22/02 FIX MASTER/SUB CLIENT RECORD LOCKING BUG                        
*                                                                               
* KWAN 05/02/01 BEFORE CHANGING REC, NEED TO CHECK FOR LOCKS                    
*                                                                               
* KWAN 02/16/01 EDTDT AND EDTCDT, LEAP YEARS CHKS ARE NO GOOD FOR 2001+         
*                                                                               
* KWAN 02/09/01 BUY RECS ARE EXPANED TO 4000 BYTES                              
*                                                                               
* KWAN 01/29/01 EXDATE (EXTENSION DATE), USES SAME LOGIC AS EDTCSHDT            
*                                                                               
* SMYE 12/00    ADD CLIENT-FROZEN-BY-DATE LOGIC AT LABEL CHG2B                  
*                                                                               
* KWAN 02/22/00 REVISE PRODUCT EXCLUSION WARNING MSG                            
*                                                                               
* KWAN 02/03/00 NO RATE CHANGES AND BUY DELETIONS FOR FULLY                     
*               PAID BUYS (CONTROLLED BY BYPROF+7)                              
*                                                                               
* KWAN 12/15/99 VALIDATING BUYS WITH EXCLUSION CLASS RESTRICTIONS               
*                                                                               
* KWAN 11/23/99 DISALLOW ADDING EMPTY ELEM WHEN BUYING (CAUSE DUMP)             
*                                                                               
* BPLA 5/99     CHANGES FOR COST2                                               
*               DISALLOW "CLE=NNNN"  IN EDIT OF OPTIONS                         
*               IT PROBABLY SHOULD NEVER HAVE BEEN                              
*               ALLOWED IN THE FIRST PLACE                                      
*               LARGE CHUNKS OF CODE NO-OPED (CDTLNS)                           
*               (TO FIX ADDRESSIBILITY ERRORS)                                  
*               DISALLOW IMPS= - REALLY SHOULDN'T                               
*               APPLY TO WSJ SCREEN                                             
*               WE MAY WANT/NEED TO CODE LATER                                  
*                                                                               
* KWAN 01/12/98 ADDED CODES FOR EXTENSION DAYS ELEMENT                          
*               FIXREC SUBROUTINE TO GET RID OF REPEATED CODES                  
*                                                                               
* BPLA 04/98    CHANGES FOR FROZEN CLIENTS                                      
*               ALSO FIX IN FMTTOT                                              
*                                                                               
* BPLA 2/3/98   ALLOW FOR LONGER FREE FORM COMMENTS BUT                         
* SMYE 2/98     IF OVER 47 CHARACTERS USE CHOPPER                               
*               TO FIX COMMENT ELEMENTS                                         
*                                                                               
* BPLA 11/97    SAVE CHANGE OF SFH STATUS IN PCHGELEM                           
*                                                                               
* BPLA 11/97    USE VGETINS                                                     
*                                                                               
* BPLA 10/97    CHANGES FOR SFH (SPECIAL FINANCIAL HANDLING) BUYS               
*                                                                               
* BPLA 06/97    ALLOW "N" BEFORE PREMIUM CHARGE FOR PREMIUMS AT NET             
*               PBDPCTYP WILL BE SET TO "N" TO INDICATE                         
*               PREMIUM ENTERED AT NET.                                         
*                                                                               
* BPLA 05/97    CHANGE TO EDTCDT AND EDTCSHDT TO                                
*               ACCOMODATE NEW DATCON (5/97)                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41114   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41114,RR=R9                                                   
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         LA    R8,4095(RB)                                                      
         LA    R8,1(R8)                                                         
         USING T41114+4096,R8      **NOTE - SECOND BASE REGISTER **             
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         GOTO1 VCALLOV,DMCB,(3,0),(RA)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   VT41103,0(R1)                                                    
         XC    OLDINS(12),OLDINS                                                
         EJECT                                                                  
*                                                                               
         CLI   SVSCRN,X'FE'                                                     
         BE    FNDCLNS                                                          
         DC    H'0'                                                             
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
FNDCLNS  DS    0H                  FIRST TIME THRU JUST CALC                    
*                                  COMMON NATIONAL UNITS                        
*                                                                               
*                                  MUST HAVE A TRANSACTION CODE                 
         LA    R2,BUYTR1H                                                       
         CLI   5(R2),0                                                          
         BNE   FNDC3                                                            
TRINV    LA    R3,NOTRERR                                                       
         B     ERROR                                                            
*                                                                               
FNDC3    ST    R2,TRADDR                                                        
         BAS   R9,EDTINS                                                        
         BAS   R9,EDTJOB           IF NO JOB INPUT EDTJOB WILL CHK              
         LA    R2,BUYTR1H                                                       
         CLC   =C'DL',8(R2)                                                     
         BE    BUYW                SKIP THIS LOGIC FOR DELETES                  
         CLI   8(R2),C'C'          CHANGE                                       
         BE    FNDC3B                                                           
         CLI   8(R2),C'B'          NEW BUY                                      
         BE    FNDC3A                                                           
         CLI   8(R2),C'R'          RECALL                                       
         BNE   TRINV                                                            
         MVC   8(2,R2),=C'R '                                                   
         B     FNDC3B                                                           
*                                                                               
FNDC3A   TM    SVCLPROF+30,X'02'   SEE IF FROZEN                                
         BO    CFRZERR                                                          
*                                  JOB ON EST HEADER                            
FNDC3B   MVI   COMPASS,1                                                        
*                                                                               
FNDCX    ZAP   TOTCOST,=P'0'                                                    
         XC    SPCTAB,SPCTAB       CLEAR SPACE TABLE                            
         ZAP   SPCTAB+1(3),=P'0'                                                
         XC    COMNLN,COMNLN       CLEAR COMMON NATIONAL UNITS                  
         MVI   COMUIND,0                                                        
         MVI   PBDUIND,C'Z'                                                     
         ZAP   PBDUNITS,=P'0'      CLEAR FOR FIRST EDITION                      
         LA    R5,SPCTAB                                                        
         ST    R5,ANXTSPCE                                                      
         XC    EDTADDR,EDTADDR                                                  
FNDC2    BAS   RE,NXTEDT                                                        
         OC    EDTADDR,EDTADDR                                                  
         BZ    FNDC4                                                            
         L     R2,EDTADDR                                                       
         BAS   R9,EDTLNS           FOR LINES AND PREMIUM EDITS                  
         L     R5,ANXTSPCE                                                      
         MVC   0(1,R5),PBDUIND                                                  
         MVC   1(3,R5),PBDUNITS                                                 
         LA    R5,4(R5)                                                         
         ST    R5,ANXTSPCE                                                      
         MVI   PBDUIND,C'Z'                                                     
         ZAP   PBDUNITS,=P'0'      CLEAR FOR NEXT EDITION                       
         B     FNDC2                                                            
*                                                                               
FNDC4    DS    0H                                                               
         LA    R2,BUYTR1H                                                       
         LA    RF,12                                                            
         BAS   RE,BUMPFLDS         GET ME TO FIRST EDITION LINES                
         LA    R3,SPCTAB                                                        
         CP    1(3,R3),=P'0'                                                    
         BE    FNDCERR                                                          
         MVC   COMNLN,1(R3)                                                     
         MVC   COMUIND,0(R3)                                                    
         B     FNDC10                                                           
FNDC5    CLC   COMUIND,0(R3)       CAN'T MIX UNIT TYPES                         
         BNE   FNDCERR                                                          
         CP    1(3,R3),=P'0'                                                    
         BE    FNDCERR                                                          
         CP    1(3,R3),COMNLN                                                   
         BNL   FNDC10                                                           
         ZAP   COMNLN,1(3,R3)      SAVE LOWEST UNITS                            
*                                                                               
FNDC10   LA    R3,4(R3)                                                         
         LA    RF,4                                                             
         BAS   RE,BUMPFLDS         NEXT EDITION                                 
         CLI   0(R3),0             END OF TABLE                                 
         BNE   FNDC5                                                            
         B     FNDC20                                                           
*                                                                               
FNDCERR  LA    R3,INVERR                                                        
         B     ERROR                                                            
*                                                                               
CFRZERR  LA    R3,FRZERR           CLIENT FROZEN                                
         B     ERROR                                                            
*                                                                               
FNDC20   DS    0H                                                               
*                                                                               
BUYW     DS    0H                  WSJ EDITION LIST BUYING                      
***TEST                                                                         
         MVI   TESTPASS,0                                                       
***TEST                                                                         
         LA    R2,BUYTR1H                                                       
         MVC   SVTRCODE,8(R2)      SAVE REAL TRCODE                             
*                                                                               
BUYW2    MVI   COMPASS,0                                                        
         BAS   RE,NXTEDT                                                        
         OC    EDTADDR,EDTADDR                                                  
         BZ    ALLDONE                                                          
BUYW5    LA    R2,BUYTR1H          RESET R2 TO COMMON TR FIELD                  
         MVC   TRCODE,8(R2)                                                     
         ST    R2,TRADDR                                                        
         L     RE,LSTPTR           SET PUB NUMBER                               
         MVC   BPUB,0(RE)                                                       
*                                                                               
         MVI   BFREQ,0             SET TO REQUIRE MONTH DAY                     
         MVI   BFLAT,0                                                          
         CLC   =C'DL',8(R2)                                                     
         BE    DEL                                                              
         BAS   R9,BLDREC                                                        
         CLI   8(R2),C'C'                                                       
         BNE   BUYW8                                                            
         MVC   SAVTRA,TRADDR                                                    
         MVC   TRADDR,EDTADDR      NEED EDTADDR FOR FNDINS                      
         BAS   R9,FNDINS                                                        
         MVC   TRADDR,SAVTRA       RESTORE 'MAIN' TRADDR                        
BUYW8    BAS   R9,EDTINS                                                        
         BAS   R9,EDTJOB                                                        
         OC    PBDJOB,PBDJOB       JOB REQUIRED FOR WSJ BUYS                    
         BNZ   BUYW10                                                           
         LA    R3,MSSNGERR                                                      
         B     ERROR                                                            
*                                                                               
BUYW10   BAS   R9,NEDTRCD          EDIT COMMON RATE CODE                        
         BAS   R9,EDTRCD           EDIT EXCESS RATE CODE                        
         BAS   R9,EDTDT            MAT CLOSE DATE                               
         MVC   PBDMDATE,DUB                                                     
*                                                                               
         ST    R2,SAVE14R2                                                      
         L     R2,EDTADDR          SET R2 T0 EDT FIELD                          
         BAS   R9,EDTLNS           FOR LINES AND PREMIUM EDITS                  
         BAS   RE,BUMPFLD          SINCE EDTLNS NO LONGER BUMPS FIELDS          
         BAS   R9,EDTPR                                                         
         L     R2,SAVE14R2                                                      
*                                                                               
         BAS   RE,BUMPFLD                                                       
         BAS   R9,EDTCOM           REALLY JUST OPTIONS                          
         BAS   R9,RTSUB            RTSUB GOES TO RATE LOOK                      
*                                  WHICH WILL LOOK UP RATES FOR BOTH            
*                                  RATE CODE AND NATIONAL RATE CODE             
**NEW 3/30/89                                                                   
         CLI   SVESPROF+28,C'C'    SEE IF 'C' RATE ESTIMATE                     
         BNE   BUYW10C                                                          
         CLI   PBDCOSIN,C'C'       SEE IF 'C' RATE BUY                          
         BE    BUYW10B                                                          
         CLI   PBDCOSIN,C' '                                                    
         BNE   BUYWERR             RATE ERROR                                   
         MVI   PBDCOSIN,C'C'                                                    
*                                                                               
* SPECIAL FOR 'C' RATE -OVERRIDE LOOKED-UP OR ENTERED DATA                      
*                                                                               
BUYW10B  DS    0H                                                               
         L     R2,TRADDR                                                        
         CLI   8(R2),C'C'          SEE IF NEW BUY                               
         BE    BUYW10B5                                                         
         ZAP   PBDCD,=P'0'         ZERO CD                                      
         XC    PBDTAX,PBDTAX       NO TAX                                       
         B     BUYW10X                                                          
*                                                                               
BUYW10B5 ZAP   PBDCD,=P'1'         WILL BE CHANGED TO ZERO                      
         MVC   PBDTAX,=X'000001'   WILL BE CHANGED TO ZERO                      
         B     BUYW10X                                                          
*                                                                               
BUYW10C  CLI   PBDCOSIN,C'C'       ONLY 'C' RATE ESTIMATES                      
         BNE   BUYW10X             CAN HAVE 'C' RATE BUYS                       
BUYWERR  LA    R3,INVRTERR                                                      
         L     R2,TRADDR                                                        
         LA    RF,3                                                             
         BAS   RE,BUMPFLDS                                                      
         B     ERROR                                                            
*                                                                               
BUYW10X  DS    0H                                                               
**NEW 3/30/89                                                                   
*                                                                               
         OC    PBDMDATE,PBDMDATE                                                
         BNZ   BUYW25                                                           
         CLI   SVAGPROF+24,C'M'    MATERIALS CLOSING DATE REQUIRED              
         BE    BUYW20                                                           
         CLI   SVAGPROF+24,C'B'    OR BOTH                                      
         BNE   BUYW25                                                           
BUYW20   LA    R3,MSSNGERR                                                      
         L     R2,TRADDR                                                        
         LA    RF,5                                                             
         BAS   RE,BUMPFLDS                                                      
         B     ERROR                                                            
*                                                                               
BUYW25   BAS   R9,CHKGRS                                                        
*                                                                               
         L     R2,EDTADDR                                                       
         BAS   RE,BUMPFLD                                                       
         BAS   R9,FMTPR            FORMAT PREMIUM CHARGE                        
         BAS   RE,BUMPFLD                                                       
         GOTO1 =A(FMTINFO),DMCB,(RC),(RA),RR=RELO                               
*                                                                               
* NOTE COMMENTS NOT INPUT                                                       
*                                                                               
         L     R2,TRADDR                                                        
         CLI   8(R2),C'C'                                                       
         BE    CHG                                                              
*                                                                               
         BRAS  RE,BEXCL            CKING FOR PRODUCT EXCLUSION                  
         BNE   EXIT                EXCLUSION CONFLICT                           
*                                                                               
         BAS   R9,EDIODAT                                                       
*                                                                               
         BAS   R9,COM              FIRST DO COMPETITIVE BRAND CHECK             
*                                                                               
         BAS   R9,ASC              AUTOMATIC SCHEDULE CHECKING                  
*                                                                               
         CLC   =C'ZZZ',BUYPR                                                    
         BE    POL                                                              
*                                                                               
         CLI   TESTPASS,0          IF TEST PASS DON'T ADD RECORD                
         BE    BUYWX                                                            
*                                                                               
         BAS   R9,ADDLINE                                                       
*                                                                               
         BAS   R9,ASR              AUTO SPACE RESV                              
*                                                                               
BUYWX    BAS   RE,NXTEDT           GET NEXT EDITION                             
*                                  SAVES ADDR OF NEXT LINES FIELD               
*                                  IN EDTADDR                                   
         OC    EDTADDR,EDTADDR                                                  
         BNZ   BUYW5                                                            
*                                                                               
         CLI   TESTPASS,0                                                       
         BE    BUYWX5                                                           
*                                                                               
         BAS   R9,FMTTR            ONLY FORMAT TR AND INS AT END                
         BAS   R9,FMTINS                                                        
BUYWX5   L     R2,TRADDR                                                        
         CLI   8(R2),C'D'          SEE IF DELETE                                
         BE    ALLDONE                                                          
*                                                                               
         CLI   TESTPASS,0                                                       
         BE    ALLDONE                                                          
*                                                                               
         BAS   R9,FMTTOT           DISPLAY BOOKING TOTAL COST                   
         B     ALLDONE                                                          
         EJECT                                                                  
CHG      DS    0H                                                               
*                                                                               
         MVC   SAVTRA,TRADDR                                                    
         MVC   TRADDR,EDTADDR      SINCE FNDINS NEEDS EDTADDR                   
         BAS   R9,FNDINS                                                        
         MVC   TRADDR,SAVTRA                                                    
*                                                                               
         B     CHG00               X'24' CHG ELEMS FOR EVERYONE NOW             
*                                                                               
CHG00    GOTO1 VGETINS,DMCB,REC,PVALUES,REC+7                                   
         MVC   SVGROSS(12),GROSS   MUST SAVE OLD GROSS,AC,CD                    
*                                  MAY NEED TO STORE IN NEW PCHGELEM            
         MVI   MATSW,0             MATCHED STATUS BYTE                          
*                                                                               
         TM    PBDSTAT-NEWREC+REC,X'40'                                         
         BZ    CHG0                NOT MATCHED - SKIP PAID CHECK                
         OI    MATSW,X'40'         MATCHED                                      
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'25'                                                     
CHG00C   BAS   R9,NEXTEL                                                        
         BNE   CHG0                                                             
         OC    2(3,R5),2(R5)       SEE IF PAID                                  
         BZ    CHG00C                                                           
         OI    MATSW,X'80'         PAID                                         
*                                                                               
CHG0     LA    R7,PBDDATE-NEWREC+REC    TEST LAST CHANGE TODAY                  
         CLC   0(3,R7),BTODAY                                                   
         BE    CHG1B                                                            
         TM    REC+(PBUYCNTL-PBUYREC),X'80'                                     
         BNZ   *+10                                                             
         MVC   0(3,R7),BTODAY                                                   
         MVI   PBDDTIND-NEWREC+REC,0                                            
         MVI   PBDDTIN2-NEWREC+REC,0                                            
         MVI   PBDDTIN3-NEWREC+REC,0                                            
*                                                                               
CHG1B    DS    0H                                                               
         MVI   CHGIND1,0           MUST RESET FOR EACH BUY                      
         MVI   CHGIND2,0                                                        
         MVI   CHGIND3,0                                                        
         MVI   CHGIND4,0                                                        
         LA    R7,PBDFREQ-NEWREC+REC                                            
         MVC   0(1,R7),PBDFREQ                                                  
*                                                                               
         LA    R7,PBDBUYER-NEWREC+REC                                           
         MVC   0(3,R7),BUYNM       SET CURRENT BUYER ID IN REC                  
*                                                                               
         CLI   BUYNM,C'*'   IF STARTS WITH * - DON'T MOVE IT                    
         BNE   *+10                                                             
         MVC   0(3,R7),BUYNM+1                                                  
*                                                                               
         LA    R7,PBDJOB-NEWREC+REC     JOB NUMBER                              
         CLC   PBDJOB,0(R7)                                                     
         BE    CHG1D                                                            
         OC    0(6,R7),0(R7)       DONT SET CHNG IND IF THERE                   
         BNZ   CHG1B5              WAS NO OLD AD                                
         OI    CHGIND3,X'40'       AD CODE ADDED                                
         B     CHG1B6                                                           
CHG1B5   OI    CHGIND2,X'08'                                                    
CHG1B6   MVC   0(6,R7),PBDJOB                                                   
         OC    PBDJOB,PBDJOB                                                    
         BNZ   CHG1D                                                            
         MVI   ELCODE,X'70'        NON-WEB INSERTION ORDER ELEM                 
         BAS   R9,CKIOEL                                                        
         BE    CHG1B8              ERROR                                        
         MVI   ELCODE,X'71'        WEB INSERTION ORDER ELEM                     
         BAS   R9,CKIOEL                                                        
         BNE   CHG1D                                                            
CHG1B8   LA    R3,JBDERR           CANNOT REMOVE AD NO IF IO THERE              
         B     ERROR                                                            
*                                                                               
CHG1D    DS    0H                                                               
         CLC   REC+16(3),NEWREC+16      TEST NEW INSERTION DATE                 
         BE    CHG2                                                             
*                                                                               
* NOTE THAT DATE CHANGES FOR MATCHED INSERTIONS IS CHECKED IN T41100            
*                                                                               
         BAS   R9,FMTTR            FORMAT TR CODE TO TEST BILLED/PAID           
         L     RE,TRADDR                                                        
         CLC   =C'**',8(RE)                                                     
         BNE   *+16                                                             
         L     R2,TRADDR                                                        
         LA    R3,NOCHGERR                                                      
         B     ERROR                                                            
         MVI   8(RE),C'C'          RESET TO 'C' FOR POL ROUTINES                
*                                                                               
         BAS   R9,COM              FIRST DO COMPETITIVE BRAND CHECK             
*                                                                               
         BAS   R9,ASC              AUTOMATIC SCHEDULE CHECKING                  
*                                                                               
         OI    CHGIND1,X'08'       DATE CHANGE                                  
         CLC   =C'ZZZ',BUYPR       POL LOGIC WILL DO PUTREC AFTER EDIT          
         BE    CHG2                OF ALLOCATIONS                               
***TEST                                                                         
         CLI   TESTPASS,0          SEE IF DOING TEST PASS                       
         BE    CHG2                                                             
***TEST                                                                         
         GOTO1 VT41103,DMCB,(RC),(RA),C'CHG'                                    
         BAS   R9,TESTERR                                                       
***TEST                                                                         
*                                                                               
CHG2     DS    0H                                                               
         LA    R7,PBDRLIND-PBUYREC+REC     RATE LOOK IND AND DATE               
         MVC   0(4,R7),PBDRLIND                                                 
*                                  BFD                                          
***TESTBUY***                                                                   
*                                                                               
CHG2A    CLI   PBDBFD-NEWREC+REC,C'T'   SEE IF IT WAS A TEST BUY                
         BE    CHG2B                    YES - CAN MAKE LIVE                     
*                                        UNLESS EST IS STILL TEST               
         CLI   PBDBFD,C'T'                                                      
         BNE   CHG2B5                                                           
CHG2ERR  LA    R3,INVERR              CAN'T MAKE LIVE BUY TEST                  
         L     R2,TRADDR                                                        
         BAS   RE,BUMPFLD                                                       
         B     ERROR                                                            
*                                                                               
************  NEW FREEZE-BY-DATE LOGIC BELOW   ***********************          
*                                                                               
CHG2ERR1 LA    R3,FRZERR        CLIENT FROZEN - CAN'T MAKE LIVE                 
         B     CHG2ERRX                                                         
CHG2ERR2 LA    R3,FDTERR           ERROR 139 - DATE FROZEN FOR CLIENT           
*                                                                               
CHG2ERRX L     R2,TRADDR                                                        
         BAS   RE,BUMPFLD                                                       
         B     ERROR                                                            
*                                                                               
CHG2B    CLI   PBDBFD,C'T'             SEE IF STILL TEST                        
         BE    CHG2B5                                                           
*              NOTE - SVCLPROF+30 IS REALLY PCLTSTAT                            
*                     SAVED THERE IN T41101                                     
         TM    SVCLPROF+30,X'02'      SEE IF CLIENT FROZEN                      
*NOP*    BO    CHG2ERR1                                                         
         BNO   C2BOK               NO                                           
         TM    SVCLPROF+30,X'10'   FROZEN WITH DATE ?                           
         BNO   CHG2ERR1            NO                                           
*                                                                               
* SVCLPROF+27 CONTAINS INDICATOR FROM FREEZE STATUS ELEM IN PCLTREC             
*                                                                               
         TM    SVCLPROF+27,X'08'   LOCK THIS MONTH AND ALL FORWARD ?            
         BO    C2BFORW             YES                                          
         TM    SVCLPROF+27,X'04'   LOCK THIS MONTH AND ALL PRIOR   ?            
         BO    C2BPAST             YES                                          
         TM    SVCLPROF+27,X'02'   LOCK THIS MONTH ONLY            ?            
         BO    *+6                 YES                                          
         DC    H'0'                SHOULD NOT HAPPEN                            
*                                                                               
* SVCLPROF+28 CONTAINS DATE (YM) FROM FREEZE STATUS ELEM IN PCLTREC             
*                                                                               
         CLC   PBUYKDAT(2),SVCLPROF+28                                          
         BNE   C2BOK                                                            
         B     CHG2ERR2             NO BUYING FOR THIS MONTH                    
*                                                                               
C2BFORW  CLC   PBUYKDAT(2),SVCLPROF+28                                          
         BL    C2BOK                                                            
         B     CHG2ERR2            NO BUYING FOR THIS MONTH & FORWARD           
*                                                                               
C2BPAST  CLC   PBUYKDAT(2),SVCLPROF+28                                          
         BH    C2BOK                                                            
         B     CHG2ERR2            NO BUYING FOR THIS MONTH & PRIOR             
*                                                                               
C2BOK    DS    0H                                                               
*                                                                               
************  NEW FREEZE-BY-DATE LOGIC ABOVE   ***********************          
*                                                                               
         TM    SVESPROF+29,X'80'      SEE IF TEST ESTIMATE                      
         BNZ   CHG2ERR                THEN MUST STILL BE A TEST BUY             
*                                                                               
         BAS   R9,COM               FIRST DO COMPETITIVE BRAND CHECK            
*                                                                               
         BAS   R9,ASC             DO ASC WHEN MAKING LIVE                       
*                                                                               
         OI    CHGIND3,X'04'      MEANS TEST BUY MADE LIVE                      
         MVI   PBDDTIND-NEWREC+REC,0  CLEAR OLD CHANGE INDICATORS               
         MVI   PBDDTIN2-NEWREC+REC,0  CLEAR OLD CHANGE INDICATORS               
         MVI   PBDDTIN3-NEWREC+REC,0  CLEAR OLD CHANGE INDICATORS               
         MVC   PBDBUYDT-NEWREC+REC(3),BTODAY  CHG NEW BUY DATE TODAY            
         MVI   ELCODE,X'24'                                                     
CHG2B2   LA    R5,REC+33                                                        
         BAS   R9,NEXTEL                                                        
         BNE   CHG2B4                                                           
         GOTO1 VRECUP,DMCB,(1,REC),(R5),0       DELETE OLD ELEM                 
         B     CHG2B2                                                           
*                                                                               
CHG2B4   DS    0H             CLEAR END OF RECORD                               
*                                                                               
         BAS   R9,CLEARREC                                                      
*                                                                               
***TESTBUY***                                                                   
CHG2B5   LA    R7,PBDBFD-NEWREC+REC                                             
         MVC   0(1,R7),PBDBFD                                                   
*                                                                               
*                                                                               
         LA    R7,PBDSPACE-NEWREC+REC                                           
         OC    PBDSPACE,=CL17' '     OR WITH SPACES                             
         OC    0(17,R7),=CL17' '                                                
         CLC   PBDSPACE,0(R7)                                                   
         BE    CHG3                                                             
*                                                                               
         TM    MATSW,X'40'             SEE IF MATCHED                           
         BZ    CHG2B7                                                           
*                                                                               
MCHGERR  LA    R3,MATERR                                                        
         B     ERROR                                                            
*                                                                               
CHG2B7   DS    0H                                                               
         CLI   0(R7),C'*'         SEE IF IT WAS A "*" BUY                       
         BNE   CHG2B8                                                           
         CLI   PBDSPACE,C'*'      SEE IF IT STILL IS                            
         BE    CHG2B9                                                           
         CLI   PBDSPACE,C'#'      OR CHANGED TO #                               
         BE    CHG2B9                                                           
*                                                                               
         BAS   R9,COM               FIRST DO COMPETITIVE BRAND CHECK            
*                                                                               
         BAS   R9,ASC       MUST DO ASC WHEN REMOVING "*"                       
         B     CHG2B9                                                           
*                                                                               
CHG2B8   DS    0H                                                               
         CLI   0(R7),C'#'         SEE IF IT WAS A "#" BUY                       
         BNE   CHG2B9                                                           
         CLI   PBDSPACE,C'#'      SEE IF IT STILL IS                            
         BE    CHG2B9                                                           
         CLI   PBDSPACE,C'*'      OR CHANGED TO *                               
         BE    CHG2B9                                                           
*                                                                               
         BAS   R9,COM               FIRST DO COMPETITIVE BRAND CHECK            
*                                                                               
         BAS   R9,ASC       MUST DO ASC WHEN REMOVING "#"                       
*                                                                               
CHG2B9   MVC   0(L'PBDSPACE,R7),PBDSPACE                                        
         OI    CHGIND1,X'10'       SPACE DESCRIPTION CHANGE                     
*                                                                               
CHG3     LA    R7,PBDCOSIN-NEWREC+REC                                           
         CLC   PBDCOSIN(7),0(R7)   COST FIELD + INDICATORS                      
         BE    CHG3B                                                            
*                                                                               
         TM    MATSW,X'C0'                                                      
         BM    MCHGERR           MIXED - MUST BE MATCHED/NOT PAID               
*                                                                               
         MVI   BYTE,C'C'           INDICATE "CHANGING"                          
         BRAS  RE,CKRATE           CHECK RATE BEFORE CHANGING                   
         BNE   EXIT                RATE CHANGES ARE NOT ALLOWED                 
*                                                                               
         MVC   0(7,R7),PBDCOSIN                                                 
         OI    CHGIND1,X'40'       COST CHANGE                                  
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
CHG3B    LA    R7,PBDRCODE-NEWREC+REC                                           
         CLC   PBDRCODE,0(R7)                                                   
         BE    CHG3D                                                            
*                                                                               
         TM    MATSW,X'C0'                                                      
         BM    MCHGERR           MIXED - MUST BE MATCHED/NOT PAID               
*                                                                               
         MVI   BYTE,C'C'           INDICATE "CHANGING"                          
         BRAS  RE,CKRATE           CHECK RATE BEFORE CHANGING                   
         BNE   EXIT                RATE CHANGES ARE NOT ALLOWED                 
*                                                                               
         MVC   0(3,R7),PBDRCODE                                                 
         OI    CHGIND1,X'40'       COST/RATE CODE CHANGE                        
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
CHG3D    LA    R7,PBDCTYP-NEWREC+REC                                            
         CLC   PBDCTYP(1),0(R7)                                                 
         BE    CHG3F                                                            
*                                                                               
         TM    MATSW,X'C0'                                                      
         BM    MCHGERR           MIXED - MUST BE MATCHED/NOT PAID               
*                                                                               
         MVI   BYTE,C'C'           INDICATE "CHANGING"                          
         BRAS  RE,CKRATE           CHECK RATE BEFORE CHANGING                   
         BNE   EXIT                RATE CHANGES ARE NOT ALLOWED                 
*                                                                               
         MVC   0(1,R7),PBDCTYP          NET COST CHANGE                         
         OI    CHGIND1,X'40'       SET COST CHANGE IND                          
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
*   NO CLOSING DATE FOR NEWSPAPERS    SKIP TO MATERIAL CLOSING DATE             
*                                                                               
***      LA    R7,PBDCDATE-NEWREC+REC                                           
***      CLC   PBDCDATE,0(R7)      CLOSE DATE                                   
***      BE    *+14                                                             
***      MVC   0(3,R7),PBDCDATE                                                 
***      OI    CHGIND2,X'80'       CLOSE DATE CHANGE                            
*                                                                               
CHG3F    LA    R7,PBDMDATE-NEWREC+REC                                           
         CLC   PBDMDATE,0(R7)      MAT CLOSE DATE                               
         BE    *+14                                                             
         MVC   0(3,R7),PBDMDATE                                                 
         OI    CHGIND3,X'02'       MAT CLOSE DATE CHANGE                        
*                                                                               
*    NO ON-SALE DATE FOR NEWSPAPERS                                             
*                                                                               
***      LA    R7,PBDSDATE-NEWREC+REC                                           
***      CLC   PBDSDATE,0(R7)      SALE DATE                                    
***      BE    *+14                                                             
***      MVC   0(3,R7),PBDSDATE                                                 
***      OI    CHGIND2,X'40'       SALE DATE CHANGE                             
*                                                                               
*                                                                               
*                                                                               
CHG4     LA    R7,PBDPDATE-NEWREC+REC                                           
         CLC   PBDPDATE,0(R7)      TEST PAYABLE DATE CHANGE                     
         BE    CHG4A               NO                                           
         OC    PBDPDATE,PBDPDATE   TEST OVERRIDE                                
         BZ    CHG4A               NO                                           
         OI    CHGIND2,X'10'       PAYABLE DATE CHANGE                          
         MVC   0(3,R7),PBDPDATE                                                 
         MVI   ELCODE,X'25'        TEST PAID                                    
         LA    R5,REC+33                                                        
         BAS   R9,NEXTEL                                                        
         BNE   CHG4A                                                            
         OC    2(3,R5),2(R5)                                                    
         BZ    *-14                                                             
CHG4ERR  L     R2,TRADDR                                                        
         LA    R3,OVRDERR                                                       
         B     ERROR                                                            
CHG4A    CLC   PBDBDATE,3(R7)                                                   
         BE    CHG4B                                                            
         OC    PBDBDATE,PBDBDATE   TEST OVERRIDE                                
         BZ    CHG4B               NO                                           
         OI    CHGIND2,X'20'       BILLABLE DATE CHANGE                         
         MVC   3(3,R7),PBDBDATE                                                 
*                                                                               
         MVI   ELCODE,X'26'        TEST BILLED                                  
         LA    R5,REC+33                                                        
         BAS   R9,NEXTEL                                                        
         BNE   CHG4A6                                                           
         OC    5(3,R5),5(R5)                                                    
         BZ    *-14                                                             
         B     CHG4ERR                                                          
*                                                                               
CHG4A6   DS    0H                                                               
         MVI   ELCODE,X'28'        TEST OPEN BILLING                            
         LA    R5,REC+33                                                        
         BAS   R9,NEXTEL                                                        
         BNE   CHG4A8                                                           
         OC    5(3,R5),5(R5)                                                    
         BZ    *-14                                                             
         B     CHG4ERR                                                          
*                                                                               
CHG4A8   DS    0H                                                               
         B     CHG4B                                                            
*                                                                               
CHG4B    CP    PBDCD,=P'0'         TEST IF CD PRESENT                           
         BE    CHG4D                NO - DIDN'T DO RATELOOK                     
         CP    PBDCD,=P'1'         TEST CD OVERRIDE OF 0                        
         BNE   *+10                                                             
         ZAP   PBDCD,=P'0'         RESET TO REAL VALUE                          
         LA    R7,PBDCD-NEWREC+REC                                              
         CLC   PBDCD,0(R7)         CASH DISCOUNT                                
         BE    CHG4D                                                            
*                                                                               
         TM    MATSW,X'C0'          SEE IF MATCHED/NOT PAID                     
         BM    MCHGERR                                                          
*                                                                               
         MVC   0(L'PBDCD,R7),PBDCD                                              
         OI    CHGIND2,X'02'       C.D. CHANGE                                  
*                                  AC CHANGE                                    
CHG4D    DS    0H                                                               
         CP    PBDACP,=P'0'                                                     
         BE    CHG5                                                             
         CP    PBDACP,=P'1'                                                     
         BNE   *+10                                                             
         ZAP   PBDACP,=P'0'                                                     
         LA    R7,PBDACP-NEWREC+REC                                             
         CLC   PBDACP,0(R7)                                                     
         BE    CHG5                                                             
*                                                                               
         TM    MATSW,X'C0'          SEE IF MATCHED/NOT PAID                     
         BM    MCHGERR              NO AC CHANGE                                
*                                                                               
**NEW 3/30/89                                                                   
         CLI   PBDCOSIN,C'C'           SEE IF 'C' RATE BUY                      
         BNE   CHG4G                                                            
         BAS   R9,FMTTR                                                         
         L     RE,TRADDR                                                        
         CLC   =C'**',8(RE)                                                     
         BNE   CHG4F                                                            
         L     R2,TRADDR                                                        
         LA    R3,NOACCHG                                                       
         B     ERROR                                                            
*                                                                               
CHG4F    MVI   8(RE),C'C'                                                       
*                                                                               
CHG4G    DS    0H                                                               
**NEW 3/30/89                                                                   
         MVC   0(L'PBDACP,R7),PBDACP                                            
         OI    CHGIND2,X'04'                                                    
*                                                                               
CHG5     DS    0H                                                               
         LA    R7,PBDPLCOS-NEWREC+REC                                           
         CLC   PBDPLCOS,0(R7)        TEST PLANNED COST                          
         BE    CHG5D                                                            
         OC    PBDPLCOS,PBDPLCOS                                                
         BE    CHG5D                                                            
         OI    CHGIND3,X'10'                                                    
         CLI   PBDPLCOS,X'FF'      TEST 'NONE'                                  
         BNE   *+10                                                             
         XC    PBDPLCOS,PBDPLCOS                                                
         MVC   0(4,R7),PBDPLCOS                                                 
*                                                                               
CHG5D    DS    0H                                                               
*                                                                               
CHG6     DS    0H                                                               
         LA    R7,PBDIDAT2-NEWREC+REC                                           
         CLC   PBDIDAT2(4),0(R7)        TEST 2ND INS DATE AND E/M IND           
         BE    CHG6D                                                            
         OC    PBDIDAT2,PBDIDAT2                                                
         BE    CHG6D                                                            
         OI    CHGIND3,X'80'                                                    
         CLI   PBDIDAT2,X'FF'      TEST 'NONE' FOR DATE                         
         BNE   *+10                                                             
         XC    PBDIDAT2,PBDIDAT2                                                
         MVC   0(4,R7),PBDIDAT2                                                 
*                                                                               
CHG6D    DS    0H                                                               
CHG7     DS    0H                                                               
         CLC   PBDSPACE(2),=C'* '  TEST SPACE BUY                               
         BNH   CHG7A               NO - TREAT LINES NORMALLY                    
*                                  ELSE-                                        
         CP    PBDUNITS,=P'0'      TEST LINES INPUT                             
         BE    CHG7A5               NO - NO CHANGE                              
         CP    PBDUNITS,=P'-1'                                                  
         BNE   *+16                                                             
         ZAP   PBDUNITS,=P'0'                                                   
         ZAP   PBDCLMS,=P'0'                                                    
*                                                                               
CHG7A    DS    0H                                                               
*                                                                               
         LA    R7,PBDUNITS-NEWREC+REC                                           
         CLC   PBDUNITS,0(R7)                                                   
         BE    CHG7A2                                                           
*                                                                               
         TM    MATSW,X'40'          SEE IF MATCHED                              
         BO    MCHGERR                                                          
*                                                                               
         MVC   0(L'PBDUNITS,R7),PBDUNITS                                        
         OI    CHGIND1,X'20'       UNITS CHANGE                                 
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
CHG7A2   LA    R7,PBDUIND-NEWREC+REC                                            
         CLC   PBDUIND,0(R7)                                                    
         BE    CHG7A5                                                           
*                                                                               
         TM    MATSW,X'40'          SEE IF MATCHED                              
         BO    MCHGERR                                                          
*                                                                               
         MVC   0(1,R7),PBDUIND                                                  
         OI    CHGIND1,X'20'       UNITS CHANGE                                 
*                                                                               
CHG7A5   DS    0H                                                               
         LA    R7,PBDCL-NEWREC+REC                                              
         CLC   PBDCL(7),0(R7)      CLRS+IND+PR COST                             
         BE    CHG7A6                                                           
*                                                                               
         TM    MATSW,X'40'          SEE IF MATCHED                              
         BO    MCHGERR                                                          
*                                                                               
         MVC   0(7,R7),PBDCL                                                    
         OI    CHGIND1,X'04'       PREMIUM CHANGE                               
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
CHG7A6   DS    0H                  PREMIUM ENTERED AT NET?                      
*                                   NOTE - A CHANGE IS OK                       
*                                   EVEN IF MATCHED SINCE A                     
*                                   CHANGE IN PREM COST WAS CAUGHT              
*                                   AT CHG7A6                                   
*                                  *THE PREMIUM CHANGE INDICATOR                
*                                   NEED NOT BE SET EITHER                      
         LA    R7,PBDPCTYP-NEWREC+REC                                           
         CLC   PBDPCTYP,0(R7)                                                   
         BE    CHG7A7                                                           
         MVC   0(1,R7),PBDPCTYP                                                 
*                                                                               
CHG7A7   LA    R7,PBDCLMS-NEWREC+REC    COLUMNS                                 
         CLC   PBDCLMS,0(R7)                                                    
         BE    CHG7B                                                            
*                                                                               
         TM    MATSW,X'40'          SEE IF MATCHED                              
         BO    MCHGERR                                                          
*                                                                               
         MVC   0(2,R7),PBDCLMS                                                  
         OI    CHGIND1,X'20'       COLUMNS                                      
*                                                                               
CHG7B    DS    0H                                                               
         LA    R7,PBDIODAT-NEWREC+REC                                           
         CLC   PBDIODAT,0(R7)                                                   
         BE    CHG8                                                             
         OC    PBDIODAT,PBDIODAT                                                
         BZ    CHG8                                                             
         OI    CHGIND2,X'01'       IO DATE CHANGE                               
         MVC   0(3,R7),PBDIODAT                                                 
*                                                                               
CHG8     DS    0H                                                               
         BAS   R9,EDIODAT                                                       
*                                                                               
CHG9     DS    0H                  SEARCH NEWREC FOR MANIO ELEMS                
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'70'                                                     
CHG9B    BAS   R9,NEXTEL                                                        
         BNE   CHG12               DONE                                         
         OC    2(3,R5),2(R5)       CHK FOR DATE                                 
         BZ    CHG9B               NONE - IGNORE                                
         CLI   10(R5),C'X'         SPECIAL DELETE CODE                          
         BE    CHG9P                                                            
         CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   CHG9ERR                                                          
         LR    R7,R5               SAVE NEWREC'S R5                             
         LA    R5,REC+33                                                        
CHG9D    BAS   R9,NEXTEL                                                        
         BNE   CHG9J                                                            
         OC    2(3,R5),2(R5)       SEE IF NOT USED                              
         BNZ   CHG9E                                                            
         MVC   0(50,R5),0(R7)      CAN JUST MOVE IN NEW ELEM                    
         B     CHG9W                                                            
*                                                                               
CHG9E    CLC   2(3,R5),2(R7)       COMPARE DATES                                
         BL    CHG9D               LOW - GO CHK NEXT OLD ELEM                   
*                                  EQU OR HIGH - ADD NEW MANIO HERE             
CHG9J    GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
         B     CHG9W                                                            
*                                                                               
CHG9P    LR    R7,R5               SAVE NEWREC'S R5                             
         SR    R4,R4                                                            
         LA    R5,REC+33                                                        
CHG9Q    BAS   R9,NEXTEL                                                        
         BE    CHG9S                                                            
         LTR   R4,R4               WILL BE 0 IF MANIO ELEM NOT FND              
         BZ    CHG9ER2                                                          
         B     CHG9U               GO DELETE ELEM                               
*                                                                               
CHG9S    CLC   2(3,R5),2(R7)       CHK DATE                                     
         BL    CHG9Q               LOW SKIP                                     
         BE    CHG9T                                                            
         SR    R4,R4               SUBSEQUENT IO ISSUED CAN'T DELETE            
         B     CHG9Q                                                            
*                                                                               
CHG9T    CLC   5(5,R5),5(R7)       CHK IO NUMBER                                
         BNE   CHG9Q                                                            
         CLI   11(R5),C'M'         MUST BE MANUAL IO                            
         BNE   CHG9Q                                                            
         LR    R4,R5               SAVE ADDR OF MANIO ELEM TO DELETE            
         B     CHG9Q                                                            
*                                                                               
CHG9U    GOTO1 VRECUP,DMCB,(1,REC),(R4)                                         
*                                                                               
         BAS   R9,CLEARREC                                                      
*                                                                               
CHG9W    LR    R5,R7               RESTORE NEWREC'S R5                          
         B     CHG9B               GO GET NEXT MANIO ELEM                       
*                                                                               
CHG9ERR  LA    R3,MAXSERR                                                       
         B     ERROR                                                            
*                                                                               
CHG9ER2  LA    R3,IOTYPER          MANIO NOT FOUND OR SUBSEQUENT                
         L     R2,TRADDR                                                        
         LA    RF,7                                                             
         BAS   RE,BUMPFLDS         CURSOR TO COMMENTS                           
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHG12    DS    0H                                                               
         MVI   ELCODE,X'86'        SHIP DATE ELEMENT                            
         BRAS  RE,FIXREC                                                        
         BNE   ERROR                                                            
*                                                                               
         MVI   ELCODE,X'96'        EXTENSION DATE ELEMENT                       
         BRAS  RE,FIXREC                                                        
         BNE   ERROR                                                            
*                                                                               
         MVI   ELCODE,X'89'        EXTENSION DAYS ELEMENT                       
         BRAS  RE,FIXREC                                                        
         BNE   ERROR                                                            
*                                                                               
         MVI   ELCODE,X'83'        REFERENCE NUMBER ELEMENT                     
         BRAS  RE,FIXREC                                                        
         BNE   ERROR                                                            
*                                                                               
CHG15    DS    0H                                                               
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'80'        SEARCH NEWREC REP ELEMS                      
CHG15B   BAS   R9,NEXTEL                                                        
         BNE   CHG17               DONE                                         
         CLI   2(R5),C'X'          SPECIAL DELETE CODE                          
         BE    CHG15P                                                           
         CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   CHG15ERR                                                         
         LR    R7,R5               SAVE NEWREC'S R5                             
         LA    R5,REC+33                                                        
CHG15D   BAS   R9,NEXTEL                                                        
         BNE   CHG15J                                                           
         CLC   0(6,R5),0(R7)       SEE IF SAME REP                              
         BE    CHG17               YES - SKIP TO CHG17                          
*                                  OLD REP ELEM FOUND JUST REPLACE IT           
         MVC   0(10,R5),0(R7)      CAN JUST MOVE IN NEW ELEM                    
         B     CHG15W                                                           
*                                                                               
* ADD NEW REP ELEM HERE                                                         
*                                                                               
CHG15J   GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
         B     CHG15W                                                           
*                                                                               
CHG15P   LR    R7,R5               SAVE NEWREC'S R5                             
         SR    R4,R4                                                            
         LA    R5,REC+33                                                        
CHG15Q   BAS   R9,NEXTEL                                                        
         BE    CHG15S                                                           
         LTR   R4,R4               WILL BE 0 IF REP ELEM NOT FND                
         BZ    CHG15ER2                                                         
         B     CHG15U              GO DELETE ELEM                               
*                                                                               
CHG15S   LR    R4,R5               SAVE ADDR OF REP ELEM TO DELETE              
         B     CHG15Q                                                           
*                                                                               
CHG15U   GOTO1 VRECUP,DMCB,(1,REC),(R4)                                         
*                                                                               
         BAS   R9,CLEARREC                                                      
*                                                                               
CHG15W   LR    R5,R7               RESTORE NEWREC'S R5                          
         OI    CHGIND3,X'20'       REP CHANGED                                  
         B     CHG15B              GO GET REP ELEM - SHOULD NOT                 
*                                  FIND ANY MORE                                
CHG15ERR LA    R3,MAXSERR                                                       
         B     ERROR                                                            
*                                                                               
CHG15ER2 LA    R3,NFNDERR          SEP REP ELEM NOT FOUND                       
         L     R2,TRADDR                                                        
         LA    RF,7                                                             
         BAS   RE,BUMPFLDS         CURSOR TO OPTION LINE                        
         B     ERROR                                                            
*                                                                               
CHG17    DS    0H                  SEARCH NEWREC OPEN RATE ELEMS                
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'30'                                                     
CHG17B   BAS   R9,NEXTEL                                                        
         BNE   CHG18               DONE                                         
         CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   CHG17ERR                                                         
         LR    R7,R5               SAVE NEWREC'S R5                             
         LA    R5,REC+33                                                        
CHG17D   BAS   R9,NEXTEL                                                        
         BNE   CHG17J                                                           
         CLC   0(13,R5),0(R7)      SEE IF SAME RATE                             
         BE    CHG18               YES - SKIP TO CHG18                          
*                                  OLD RATE ELEM FOUND JUST REPLACE IT          
         MVC   0(13,R5),0(R7)      CAN JUST MOVE IN NEW ELEM                    
         B     CHG17W                                                           
*                                                                               
* ADD NEW OPEN RATE ELEM HERE                                                   
*                                                                               
CHG17J   GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
         B     CHG17W                                                           
*                                                                               
CHG17W   LR    R5,R7               RESTORE NEWREC'S R5                          
         OI    CHGIND1,X'40'       RATE CHANGED                                 
         B     CHG17B              GO GET RATE ELEM - SHOULD NOT                
*                                  FIND ANY MORE                                
*                                                                               
CHG17ERR LA    R3,MAXSERR                                                       
         B     ERROR                                                            
*                                                                               
CHG18    DS    0H                  SEARCH WSJ ELEMS                             
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'35'                                                     
CHG18B   BAS   R9,NEXTEL                                                        
         BNE   CHG19               DONE                                         
         CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   CHG18ERR                                                         
         LR    R7,R5               SAVE NEWREC'S R5                             
         LA    R5,REC+33                                                        
CHG18D   BAS   R9,NEXTEL                                                        
         BNE   CHG18J                                                           
         CLC   0(23,R5),0(R7)      SEE IF SAME WSJ INFO                         
         BE    CHG19               YES - SKIP TO CHG19                          
*                                                                               
* OLD WSJ ELEM FOUND JUST REPLACE IT                                            
*                                                                               
         CLC   PWSJUNTS-PWSJELEM(3,R5),PWSJUNTS-PWSJELEM(R7)                    
         BE    *+8                                                              
         OI    CHGIND1,X'20'       COMMON UNITS CHANGE                          
         MVC   0(23,R5),0(R7)      CAN JUST MOVE IN NEW ELEM                    
         B     CHG19                                                            
*                                                                               
* ADD NEW WSJ ELEM HERE                                                         
*                                                                               
CHG18J   GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
         OI    CHGIND1,X'20'       COMMON UNITS CHANGE                          
         B     CHG19                                                            
*                                                                               
CHG18ERR LA    R3,MAXSERR                                                       
         B     ERROR                                                            
*                                                                               
CHG19    DS    0H                                                               
         TM    PBDSTAT,X'0C'       SEE IF SFH ENTERED IN NEWREC                 
         BZ    CHG20                                                            
         MVC   BYTE,PBDSTAT-NEWREC+REC    SAVE PRE-CHANGE PBDSTAT               
         NI    BYTE,X'0C'                                                       
         TM    PBDSTAT-NEWREC+REC,X'08'   PRE-CHANGE SFH HELD                   
         BNO   CHG19B              NO                                           
         TM    PBDSTAT,X'08'       IS HELD ON IN NEWREC                         
         BO    CHG19B                                                           
         NI    PBDSTAT-NEWREC+REC,X'F7'   SET OFF X'08' IN REC                  
*                                                                               
CHG19B   OC    PBDSTAT-NEWREC+REC(1),PBDSTAT                                    
         MVC   BYTE2,PBDSTAT                                                    
         NI    BYTE2,X'0C'                                                      
         CLC   BYTE,BYTE2                                                       
         BE    *+8                                                              
         OI    CHGIND4,X'80'       SFH STATUS CHANGE                            
*                                                                               
CHG20    LA    R7,PBDDTIND-NEWREC+REC                                           
         OC    0(1,R7),CHGIND1                                                  
         LA    R7,PBDDTIN2-NEWREC+REC                                           
         OC    0(1,R7),CHGIND2                                                  
         LA    R7,PBDDTIN3-NEWREC+REC                                           
         OC    0(1,R7),CHGIND3                                                  
*                                                                               
         B     CHG20C              X'24' CHG ELEMS FOR EVERYONE NOW             
*                                                                               
CHG20C   CLI   CHGIND1,0                                                        
         BNE   CHG21                                                            
         CLI   CHGIND2,0                                                        
         BNE   CHG21                                                            
         CLI   CHGIND3,0                                                        
         BNE   CHG21                                                            
         CLI   CHGIND4,0                                                        
         BE    CHG40               NO 'REAL' CHANGE                             
*                                                                               
* TRY AND FIND ACTIVITY ELEM FOR TODAY                                          
*                                                                               
CHG21    DS    0H                                                               
         GOTO1 VDATCON,DMCB,(3,BTODAY),(2,DUB)                                  
         GOTO1 VGETINS,DMCB,REC,PVALUES,REC+7                                   
         XC    DUMEL(25),DUMEL                                                  
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'24'                                                     
CHG22    BAS   R9,NEXTEL                                                        
         BNE   CHG24                                                            
         CLC   2(2,R5),DUB         LOOK FOR TODAY                               
         BNE   CHG22                                                            
*                                                                               
* FOUND  - THEN SAVE OLD ELEM AND DELETE IT                                     
*                                                                               
         MVC   DUMEL(25),0(R5)                                                  
         GOTO1 VRECUP,DMCB,(1,REC),(R5)         DELETE OLD ELEM                 
*                                                                               
         BAS   R9,CLEARREC                                                      
*                                                                               
CHG24    LA    R3,DUMEL                                                         
         USING PCHGELD,R3                                                       
*                                                                               
         LA    RE,PCHG_XSS         START OF EXTENSION FOR SHORT ELEM            
         CLI   PCHGLEN,PCHGNEWL    LENGTH WITH COST INFO?                       
         BNE   *+8                                                              
         LA    RE,PCHG_XLS         START OF EXTENSION FOR LONG ELEM             
         USING PCHGEXT,RE                                                       
         MVC   BYTE,PCHGIND5       SAVE 5TH CHANGE INDICATOR                    
         DROP  RE                                                               
*                                                                               
         MVI   PCHGELEM,X'24'                                                   
         CLI   PCHGLEN,0                                                        
         BNE   *+8                                                              
         MVI   PCHGLEN,8                                                        
         MVC   PCHGDAT,DUB         TODAY'S DATE - PACKED                        
         OC    PCHGIND1(2),CHGIND1 OR ON CHG BITS                               
         OC    PCHGIND3,CHGIND3    OR ON CHG BITS                               
         OC    PCHGIND4,CHGIND4    OR ON CHG BITS                               
         CLC   SVGROSS(12),GROSS                                                
         BE    CHG30               NO CHG IN GROSS,AC,CD SO DONE                
         CLI   PCHGLEN,20                                                       
         BE    CHG30                                                            
         MVI   PCHGLEN,20                                                       
         MVC   PCHGGRS(12),SVGROSS                                              
*                                                                               
CHG30    LA    RE,PCHG_XSS         START OF EXTENSION FOR SHORT ELEM            
         CLI   PCHGLEN,PCHGNEWL    LENGTH WITH COST INFO?                       
         BNE   *+8                                                              
         LA    RE,PCHG_XLS         START OF EXTENSION FOR LONG ELEM             
         USING PCHGEXT,RE                                                       
         MVC   PCHGIND5,BYTE       RESTORE 5TH CHANGE INDICATOR                 
         DROP  R3,RE                                                            
*                                                                               
         CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   CHG15ERR                                                         
*                                                                               
* R5 SHOULD STILL BE IN REC                                                     
*                                                                               
         GOTO1 VRECUP,DMCB,(1,REC),DUMEL,(R5)                                   
*                                                                               
* TEST ESTIMATE PRINT CHANGE                                                    
*                                                                               
CHG40    TM    CHGIND1,X'FC'                                                    
         BNZ   CHG50                                                            
         TM    CHGIND2,X'06'       CD OR AC                                     
         BNZ   CHG50                                                            
         TM    CHGIND3,X'FF'                                                    
         BNZ   CHG50                                                            
         MVC   BYTE,SVCLPROF+31    TEST JOB NO TO PRINT                         
         OC    BYTE,SVESPROF+31                                                 
         CLI   BYTE,C'0'                                                        
         BE    CHG54                                                            
         TM    PBDDTIN2,X'08'      JOB NO.                                      
         BZ    CHG54                                                            
*                                                                               
CHG50    MVC   PBDCHGDT-NEWREC+REC(3),BTODAY      SET DATE                      
*                                                                               
CHG54    CLC   =C'ZZZ',BUYPR                                                    
         BE    POL                                                              
*                                                                               
* OTHER BUY PHASES NOW DO DATE CHANGE DIRECTORY CHANGES HERE                    
* THIS IS NOT NEEDED SINCE THIS PHASE HAS A TEST PASS                           
*                                                                               
CHGPUT   DS    0H                                                               
         CLI   TESTPASS,0          NO PUTREC ON TEST PASS                       
         BE    CHGX                                                             
*                                                                               
         BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BE    *+16                                                             
         LA    R2,BUYTR1H                                                       
         LA    R3,DATALOCK                                                      
         B     ERROR                                                            
*                                                                               
         BRAS  RE,CKEIOELM         CK EIO ELEM                                  
*                                                                               
         CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         BE    *+8                                                              
         BAS   RE,PUTREC                                                        
*                                                                               
         CLI   BUYNM,C'*'          NO ASR IF * IN BUYNM                         
         BE    CHGPUT9                                                          
*                                                                               
         TM    CHGIND1,X'7C'       CHANGE IN DATE,SPACE,RATE,LINES              
         BNZ   *+12                                                             
         TM    CHGIND3,X'04'       TEST BUY MADE LIVE                           
         BZ    *+8                                                              
         BAS   R9,ASR              AUTO SPACE RESV                              
*                                                                               
CHGPUT9  TM    CHGIND1,X'08'       DATE CHANGED?                                
         BNZ   *+12                                                             
         TM    CHGIND2,X'08'       AD CODE IS CHANGED?                          
         BZ    *+8                                                              
         BRAS  RE,CKMATPTR         CK FOR MAT= REPEAT PASSIVE PTR               
*                                                                               
CHGX     DS    0H                                                               
         MVC   NEWREC(25),REC      MOVE KEY TO NEWREC FOR FMTINS                
         L     R2,TRADDR                                                        
         MVC   8(2,R2),SVTRCODE    RESTORE REAL TRCODE                          
         B     BUYWX                                                            
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DEL      DS    0H                                                               
         MVC   SAVTRA,TRADDR                                                    
         MVC   TRADDR,EDTADDR      SINCE FNDINS NEEDS EDTADDR                   
         BAS   R9,FNDINS           RETRIEVE INSERTION                           
         MVC   TRADDR,SAVTRA       RESTORE TRADDR                               
         BAS   R9,FMTTR            FORMAT TR CODE TO TEST BILLED/PAID           
*                                                                               
         MVI   BYTE,C'D'           INDICATE "DELETING"                          
         BRAS  RE,CKRATE           CHECK RATE BEFORE DELETION                   
         BNE   EXIT                BUY DELETIONS ARE NOT ALLOWED                
*                                                                               
         CLI   TESTPASS,0          TEST?                                        
         BE    DEL10                                                            
*                                                                               
         MVI   DMOUTBTS,0          RESET FOR NO ERROR TESTS                     
         XC    KEY,KEY                                                          
         MVC   KEY(25),REC                                                      
         BAS   RE,READ                                                          
         BAS   RE,CHECK                                                         
         OI    KEY+25,X'80'        SET DELETED IND                              
*                                                                               
         BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BE    *+16                                                             
         LA    R2,BUYTR1H                                                       
         LA    R3,DATALOCK                                                      
         B     ERROR                                                            
         BAS   RE,WRITE                                                         
         BAS   RE,CHECK                                                         
*                                                                               
         MVI   KEY+3,X'21'         DELETE CLT/PUB POINTER                       
         MVC   KEY+7(6),REC+10     PUB                                          
         MVC   KEY+13(3),REC+7     PRD                                          
         BAS   RE,READ                                                          
         BAS   RE,CHECK                                                         
         OI    KEY+25,X'80'                                                     
*                                                                               
         BAS   RE,WRITE            LOCK IS CHECKED EARLIER                      
         BAS   RE,CHECK                                                         
         BAS   RE,GETREC                                                        
         BAS   RE,CHECK                                                         
*                                                                               
         OI    REC+27,X'80'                                                     
         LA    R7,PBDDATE-NEWREC+REC                                            
         MVC   0(3,R7),BTODAY      SET CHANGE DATE IN REC                       
*                                                                               
         LA    R7,PBDBUYER-NEWREC+REC                                           
         MVC   0(3,R7),BUYNM       SET BUYER ID IN REC                          
*                                                                               
         CLI   BUYNM,C'*'          IF STARTS WITH * - DON'T MOVE IT             
         BNE   *+10                                                             
         MVC   0(3,R7),BUYNM+1                                                  
*                                                                               
DELPID   DS    0H                  X'A7' ELEM, WHO DELETED INSERTION            
         XC    WORK(6),WORK                                                     
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'A7'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   DELPID10                                                         
         USING PPIDELD,WORK                                                     
         MVC   PPIDELM(4),0(R5)                                                 
         GOTO1 VRECUP,DMCB,(1,REC),(R5),0                                       
*                                                                               
DELPID10 OC    WORK(4),WORK                                                     
         BNZ   DELPID20                                                         
         MVI   PPIDELM,PPIDELQ     PID DELETED ELEM CODE                        
         MVI   PPIDELL,PPIDLEQ     PID DELETED ELEM LENGTH                      
         XC    PPIDADD,PPIDADD                                                  
         MVC   PPIDDEL,SVPID       SAVE INFO WHO DELETED                        
         B     DELPID50                                                         
*                                                                               
DELPID20 MVI   PPIDELL,PPIDLEQ     PID DELETED ELEM LENGTH                      
         MVC   PPIDDEL,SVPID                                                    
*                                                                               
DELPID50 GOTO1 VRECUP,DMCB,(1,REC),WORK,(R5)                                    
*                                                                               
         BRAS  RE,CKEIOELM         CK EIO ELEM                                  
*                                                                               
         CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         BE    *+12                                                             
         BAS   RE,PUTREC                                                        
         BAS   RE,CHECK                                                         
*                                                                               
         BRAS  RE,CKMATPTR         CK FOR MAT= REPEAT PASSIVE PTR               
*                                                                               
DEL10    MVI   DMOUTBTS,X'FD'      RESET DMOUTBTS                               
         GOTO1 VT41103,DMCB,(RC),(RA),C'CLR'                                    
         BAS   R9,TESTERR                                                       
*                                                                               
         CLI   TESTPASS,0                                                       
         BE    CHGX                                                             
*                                                                               
         CLC   =C'ZZZ',BUYPR       GO DELETE PASSIVE POINTERS                   
         BE    POL1A               SKIP ALLOC EDIT                              
*                                                                               
         BAS   R9,ASR              AUTO SPACE RESV                              
*                                                                               
         B     CHGX                                                             
*                                                                               
CHECK    CLI   DMCB+8,0                                                         
         BCR   8,RE                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
POL      DS    0H                  SPECIAL ROUTINES FOR POL                     
         L     R2,TRADDR                                                        
         LA    RF,9                                                             
         BAS   RE,BUMPFLDS                                                      
*                                                                               
         CLI   5(R2),0             TEST INPUT                                   
         BNE   POLA                                                             
         CLI   SVESTALO,C' '                                                    
         BNH   POLA                                                             
         MVC   8(47,R2),SVESTALO   USE EST ALO                                  
         FOUT  (R2)                                                             
         LA    R5,47(R2)                                                        
         CLI   7(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         SR    R5,R2                                                            
         STC   R5,5(R2)            LENGHT                                       
*                                                                               
POLA     DS    0H                                                               
         MVI   X,C' '                                                           
         MVC   X+1(46),X                                                        
         OC    8(L'PJOBALO,R2),X                                                
*                                                                               
         OC    PBDJOB,PBDJOB                                                    
         BZ    POL1                                                             
         L     R4,AJOBIO                                                        
         CLI   PJOBALO-PJOBREC(R4),C' '     SEE IF AD HAS ALLO                  
         BNH   POL1                NO - SKIP MATCH CHK                          
         CLC   8(L'PJOBALO,R2),PJOBALO-PJOBREC(R4)                              
         BE    POL1                                                             
         LA    R3,ALOERR                                                        
         B     ERROR                                                            
POL1     DS    0H                                                               
         TM    GENBYSW1,ALLOEXTQ   EXTENDED ALLOCS NOT VALIDATED                
         JNZ   POL01               ALREADY?                                     
         CLI   SVESTALO,C' '                                                    
         BNH   POL01                                                            
*                                                                               
         CLC   8(47,R2),SVESTALO        TEST VS EST ALO                         
         BE    POL01                                                            
         LA    R3,ALOERR2                                                       
         B     ERROR                                                            
*                                                                               
POL01    DS    0H                                                               
         L     RE,TRADDR                                                        
         CLI   8(RE),C'C'                                                       
         BNE   POL1A                                                            
         TM    CHGIND1,X'08'       DATE CHANGE - MUST DO POL ROUTINES           
         BO    POL1A                                                            
*                                                                               
         TM    4(R2),X'20'         TEST FIELD MODIFIED                          
         BO    CHGPUT              NO - SKIP EDIT                               
*                                  POL ASR DONE IN CHGPUT THEN                  
POL1A    DS    0H                                                               
         GOTO1 VCALLOV,DMCB,(4,0),(RA)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         ST    RA,DMCB+4                                                        
         MVI   DMCB+4,0                                                         
         CLI   TESTPASS,0                                                       
         BNE   *+8                                                              
         MVI   DMCB+4,C'T'         TEST PASS SW (FOR POL ROUTINE)               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC)                                                   
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
*                                                                               
         GOTO1 VCALLOV,DMCB,(1,0),(RA)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TESTPASS,0          DON'T DO ASR ON TEST PASS                    
         BE    CHGX                                                             
*                                                                               
         L     RE,TRADDR                                                        
         CLI   8(RE),C'C'                                                       
         BNE   POL5                                                             
***                                                                             
         CLI   BUYNM,C'*'        NO ASR IF * IN BUYNM                           
         BE    POL7                                                             
***                                                                             
         TM    CHGIND1,X'7C'    CHANGE IN DATE, SPACE, RATE                     
         BNZ   POL5                                                             
         TM    CHGIND3,X'04'    OR MADE LIVE                                    
         BZ    *+8                                                              
POL5     BAS   R9,ASR           AUTO SPACE RESV                                 
*                                                                               
POL7     B     CHGX                                                             
         EJECT                                                                  
*                                                                               
* SUBROUTINE TO EDIT LINES                                                      
*                                                                               
EDTLNS   DS    0H                                                               
         ST    R9,FULL                                                          
         GOTO1 =A(EDTLIN),DMCB,(RC),(RA),RR=RELO                                
*                                                                               
         CLI   ERRAREA,0      CHECK FOR ERROR                                   
         BNE   EXXMOD                                                           
*                                                                               
         L     R9,FULL                                                          
         BR    R9              RETURN                                           
*                                                                               
         EJECT                                                                  
*                                                                               
* SUBROUTINE TO EDIT `REMIUM DESC                                               
*                                                                               
EDTPR    DS    0H                                                               
         MVI   PBDPCTYP,0      RESET PREMIUN INPUT AT NET INDICATOR             
         BAS   RE,BUMPFLD                                                       
         LA    R3,INVERR                                                        
         CLI   5(R2),0                                                          
         BCR   8,R9                                                             
         CLC   =C'NONE',8(R2)                                                   
         BE    EDTPR3                                                           
         LA    R6,8(R2)                                                         
         SR    R7,R7                                                            
         IC    R7,5(R2)                                                         
         CLI   9(R2),C'C'                                                       
         BNE   EDTPR2                                                           
         CLI   8(R2),C'1'                                                       
         BL    ERROR                                                            
         CLI   8(R2),C'4'                                                       
         BH    ERROR                                                            
         MVC   PBDCL,8(R2)                                                      
         MVC   PBDPRIN,PBDCOSIN    SET DEFAULT PREMIUM RATE IND                 
         CLI   5(R2),2             TEST INPUT LENGTH                            
         BE    EDTPR3                                                           
         MVI   PBDPRIN,C' '                                                     
         CLI   10(R2),C'/'                                                      
         BNE   ERROR                                                            
         LA    R6,11(R2)                                                        
         AHI   R7,-3                                                            
         BZ    ERROR                                                            
*                                                                               
EDTPR2   DS    0H                                                               
         CLI   0(R6),C'S'          TEST NON-COMM OVERRIDE                       
         BNE   EDTPR2C                                                          
         LA    R6,1(R6)            ADJUST POINTER                               
         BCTR  R7,0                 AND LEN                                     
         MVI   PBDPRIN,C'S'        SET IND IN PBDELEM                           
         CLI   PBDCOSIN,C'S'       MUST MATCH                                   
         BNE   ERROR                                                            
         B     EDTPR2D                                                          
*                                                                               
EDTPR2C  DS    0H                                                               
**COM                                                                           
******** CLC   PBUYKAGY,=C'SJ'     ONLY FOR SJR                                 
******** BNE   EDTPR2D                                                          
**COM                                                                           
         CLI   0(R6),C'C'          TEST COMMISSION RATE                         
         BNE   EDTPR2D                                                          
         LA    R6,1(R6)            ADJUST POINTER                               
         BCTR  R7,0                AND LEN                                      
         MVI   PBDPRIN,C'C'        SET IND IN PBDELEM                           
         CLI   PBDCOSIN,C'C'       MUST MATCH                                   
         BNE   ERROR                                                            
*                                                                               
EDTPR2D  DS    0H                                                               
**NEW 3/30/89                                                                   
         CLI   SVESPROF+28,C'C'    SEE IF 'C' RATE EST                          
         BNE   EDTPR2F                                                          
         CLI   PBDPRIN,C'C'        SEE IF 'C' INPUT                             
         BE    EDTPR2G                                                          
         CLI   PBDPRIN,C' '                                                     
         BNE   ERROR                                                            
         MVI   PBDPRIN,C'C'         SET TO 'C'                                  
         B     EDTPR2G                                                          
*                                                                               
EDTPR2F  CLI   PBDPRIN,C'C'     NO 'C' PREMIUM FOR NON 'C' RATE EST             
         BE    ERROR                                                            
*                                                                               
EDTPR2G  DS    0H                                                               
         CLI   0(R6),C'N'          NET TO BE GROSSED UP                         
         BNE   EDTPR2H                                                          
         CLI   PBDPRIN,C' '   CAN'T BE ENTERED IF PBDPRIN PRESENT               
         BH    ERROR                                                            
         MVI   PBDPCTYP,C'N'                                                    
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
         B     EDTPR2                                                           
*                                                                               
EDTPR2H  DS    0H                                                               
*                                                                               
         GOTO1 VCASHVAL,DMCB,(R6),(R7)                                          
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         ZAP   PBDPRCOS,=P'1'      SET TO .01 IF FREE                           
         L     R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    EDTPR3                                                           
         CVD   R0,DUB                                                           
         ZAP   PBDPRCOS,DUB                                                     
*                                                                               
EDTPR3   DS    0H                                                               
         OC    PBDJOB,PBDJOB                                                    
         BZR   R9                                                               
         L     R4,AJOBIO                                                        
         CLC   PBDCL,PJOBPRM-PJOBREC(R4)                                        
         BER   R9                                                               
         OI    WARN,X'80'          SPACE WARNING                                
         BR    R9                                                               
         EJECT                                                                  
EDTCOM   DS    0H                                                               
         ST    R9,SAVER9                                                        
         GOTO1 =A(EDTC),DMCB,(RC),(RA),RR=RELO                                  
*                                                                               
         CLI   ERRAREA,0      CHECK FOR ERROR                                   
         BNE   EXXMOD                                                           
*                                                                               
         L     R9,SAVER9                                                        
         BR    R9                                                               
*                                                                               
* SUBROUTINE TO EDIT DATES (MAT CLOSE DATE)                                     
*                                                                               
EDTDT    LA    R3,INVDTERR                                                      
         BAS   RE,BUMPFLD                                                       
         XC    DUB,DUB                                                          
         CLI   5(R2),0                                                          
         BE    EDTDTX                                                           
         GOTO1 VDATVAL,DMCB,(1,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         GOTO1 VDATCON,(R1),(0,WORK),(3,DUB)                                    
         MVC   DUB(1),NEWREC+16    MOVE INSERTION YEAR                          
         CLC   DUB+1(1),NEWREC+17  COMPARE THIS M TO INS MO.                    
         BNH   EDTDT5              IF LOW USE THIS YEAR                         
         IC    RE,DUB              ELSE USE PREVIOUS YEAR                       
         BCTR  RE,0                                                             
         STC   RE,DUB                                                           
*                                                                               
EDTDT5   GOTO1 VDATCON,(R1),(3,DUB),(5,WORK)                                    
         GOTO1 VDATVAL,(R1),(0,WORK),WORK+10                                    
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR               CATCH ERR ESCAPED FROM PREVIOUS DVAL         
*                                                                               
         CLI   BYPROF+10,C'Y'      "PRIOR" DATE IF DATE NOT WORKDAY ?           
         BNE   EDTDTX              NO                                           
*                      ADJUST DATE IN DUB TO PRIOR WORKDAY IF NECESSARY         
         GOTOR VPPWKDAY,(R1),(NATION,DUB),DUB,ACOMFACS                          
*                                                                               
EDTDTX   BR    R9                                                               
         EJECT                                                                  
*                                                                               
* SUBROUTINE TO EDIT RATE CODES                                                 
*                                                                               
EDTRCD   LA    R3,INVRTERR                                                      
         BAS   RE,BUMPFLD                                                       
         XC    PBDRCODE,PBDRCODE   CLEAR RATE CODE                              
         MVI   PBDCOSTY,C'U'       PRESET RATE TYPE                             
         MVI   PBDCOSIN,C' '                                                    
         MVI   PBDCTYP,0                                                        
         LA    R6,8(R2)                                                         
         ZIC   R7,5(R2)                                                         
         CLI   5(R2),0                                                          
         BNE   EDTRCD5                                                          
         LA    R3,MSSNGERR                                                      
         B     ERROR                                                            
*                                                                               
EDTRCD5  CLI   0(R6),C'*'          FROZEN RATE                                  
         BNE   EDTR6D                                                           
         OI    PBDRLIND,X'08'                                                   
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
*                                                                               
EDTR6D   DS    0H                    RATE CODE                                  
         CLI   0(R6),C' '                                                       
         BNH   ERROR                                                            
         CHI   R7,3                     MAX 3 CHARS                             
         BH    ERROR                                                            
         MVC   PBDRCODE,0(R6)           SAVE CODE IN PBUYREC                    
         OC    PBDRCODE,=3C' '                                                  
*                                       NEW RATE LOOK WILL FIND RATE            
         BR    R9                         RETURN                                
*                                                                               
         EJECT                                                                  
*                                   NATIONAL RATE CODE                          
NEDTRCD  LA    R3,INVRTERR                                                      
         BAS   RE,BUMPFLD                                                       
         LA    R6,8(R2)                                                         
         ZIC   R7,5(R2)                                                         
         CLI   5(R2),0                                                          
         BNE   NEDTRCD5                                                         
         LA    R3,MSSNGERR                                                      
         B     ERROR                                                            
*                                                                               
NEDTRCD5 DS    0H                                                               
*                                                                               
NEDTR6D  DS    0H                    RATE CODE                                  
         CLI   0(R6),C' '                                                       
         BNH   ERROR                                                            
         CHI   R7,3                     MAX 3 CHARS                             
         BH    ERROR                                                            
*                                       ADD WSJ ELEM                            
         LA    R5,NEWREC+33                                                     
         XC    WORK+10(25),WORK+10                                              
         LA    R4,WORK+10                                                       
         USING PWSJELEM,R4                                                      
         MVC   WORK+10(2),=X'3517'      SET CODE AND LENGHT                     
         ZAP   PWSJECOS,=P'0'                                                   
         ZAP   PWSJUNTS,COMNLN          COMMON NATIONAL UNITS                   
         ZAP   PWSJNCOS,=P'0'                                                   
         MVC   PWSJNRTE,0(R6)           FOR NOW CAN JUST SET RATE CODE          
         OC    PWSJNRTE(3),=3C' '                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,X'35'                                                     
         ST    R9,FULL                                                          
         BAS   R9,NEXTEL                                                        
         BNE   *+6                                                              
         DC    H'0'                     IMPOSSIBLE TO ALREADY HAVE ONE          
         L     R9,FULL                                                          
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+10,(R5)                              
*                                       NEW RATE LOOK WILL FIND RATE            
         BR    R9                         RETURN                                
*                                                                               
         EJECT                                                                  
* INSERTION ORDER DATE EDIT                                                     
*                                                                               
EDIODAT  DS    0H                                                               
         OC    PBDIODAT,PBDIODAT                                                
         BZ    EDIODX              NO DATE INPUT                                
         CLI   PBDIODAT,X'FF'                                                   
         BE    EDIODX              'NONE'                                       
         LA    R3,IODERR1                                                       
         CLC   PBDIODAT,BTODAY                                                  
         BL    ERROR               BEFORE TODAY                                 
         LA    R3,IODERR2                                                       
         B     EDIOD4              **NOP DATE TESTS**                           
         CLC   PBDIODAT,PBUYKDAT                                                
         BNL   ERROR               AFTER INS DATE                               
         OC    PBDCDATE,PBDCDATE                                                
         BZ    EDIOD4                                                           
         LA    R3,IODERR3                                                       
         CLC   PBDIODAT,PBDCDATE                                                
         BNL   ERROR               AFTER CLOSING DATE                           
EDIOD4   DS    0H                                                               
         LA    R3,IODERR4                                                       
         OC    PBDJOB,PBDJOB                                                    
         BZ    ERROR               NO JOB                                       
         LA    R3,IODERR5                                                       
         CLI   PBDSPACE,C'*'       NO I.O. IF SPACE = *                         
         BE    ERROR                                                            
*                                                                               
EDIODX   DS    0H                                                               
         BR    R9                                                               
         SPACE 3                                                                
CKIOEL   DS    0H                  TEST ANY IO'S PRINTED                        
*SMY*    MVI   ELCODE,X'70'                                                     
*                                  ELCODE SET BEFORE CALL TO CKIOEL             
         LA    R5,REC+33                                                        
         LR    RF,R9                                                            
         BAS   R9,NEXTEL                                                        
         BNE   CKIOELX             NO IO PRINTED                                
         OC    2(3,R5),2(R5)                                                    
         BZ    *-14                                                             
         SR    R0,R0               SET CC =                                     
CKIOELX  LR    R9,RF                                                            
         BR    R9                                                               
         EJECT                                                                  
EDTINS   GOTO1 VEDTINS,DMCB,(RC),(RA)                                           
*                                                                               
         CLC   PBUYKDAT,BKILL                                                   
         BL    *+12                                                             
         LA    R3,KDATERR                                                       
         B     ERROR                                                            
*                                                                               
         SR    R0,R0               POINT TO NEXT FIELD                          
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     TESTERR                                                          
         SPACE 2                                                                
FMTINS   GOTO1 VFMTINS,DMCB,NEWREC                                              
         B     TESTERR                                                          
         SPACE 2                                                                
FMTTR    GOTO1 VFMTTR,DMCB,REC     FORMAT TR FIELD AT TRADDR                    
         B     TESTERR                                                          
*                                                                               
FMTPR    GOTO1 VFMTPR                                                           
         B     TESTERR                                                          
*                                                                               
*                                                                               
         SPACE 2                                                                
FMTTOT   CP    TOTCOST,=P'0'                                                    
         BE    FMTTOTX                                                          
         LA    R2,BUYTR1H                                                       
         LA    RF,43                                                            
         BAS   RE,BUMPFLDS                                                      
         CLI   SVTRCODE,C'B'       SEE IF BUYING                                
         BNE   FMTTOT3                                                          
         XC    8(55,R2),8(R2)     CLEAR OLD $                                   
         MVC   8(21,R2),=C'TOTAL COST OF BOOKING'                               
         EDIT  (P8,TOTCOST),(14,31(R2)),2,COMMAS=YES,ALIGN=LEFT                 
         FOUT  (R2)                                                             
******** BAS   RE,BUMPFLD                                                       
******** XC    8(55,R2),8(R2)     CLEAR OLD $ IN 2ND FIELD                      
******** FOUT  (R2)                                                             
         B     FMTTOTX                                                          
*                                                                               
FMTTOT3  CLC   8(5,R2),=C'TOTAL' SEE IF TOTAL DISPLAYED BY T41105               
         BNE   FMTTOT5                                                          
         MVC   8(5,R2),=C'PRIOR'    CHANGE IT TO PRIOR                          
FMTTOT4  FOUT  (R2)                                                             
*                                                                               
FMTTOT5  BAS   RE,BUMPFLD                                                       
FMTTOT6  MVC   8(21,R2),=C'TOTAL COST OF BOOKING'                               
         EDIT  (P8,TOTCOST),(14,31(R2)),2,COMMAS=YES,ALIGN=LEFT                 
         FOUT  (R2)                                                             
FMTTOTX  BR    R9                                                               
*                                                                               
NXTTR    GOTO1 VNXTTR                                                           
         CLI   ERRAREA,0                                                        
         BNE   TESTERR                                                          
         XC    INSDA,INSDA                                                      
         XC    INSKEY,INSKEY                                                    
         XC    INSADR,INSADR                                                    
         XC    BINSDT,BINSDT                                                    
         MVI   BSUBLN,0                                                         
         L     R2,TRADDR           GET NEW TR ADDR                              
         MVC   TRCODE,8(R2)                                                     
         LTR   R2,R2                                                            
         BR    R9                                                               
         SPACE 2                                                                
*                                                                               
NXTEDT   NTR                                                                    
         L     R2,EDTADDR             FIRST TIME                                
         LTR   R2,R2                                                            
         BNZ   NXTEDT7                                                          
         LA    R2,BUYTR1H                                                       
         LA    RE,SVLST                                                         
         ST    RE,LSTPTR                                                        
         LA    RF,11                                                            
         BAS   RE,BUMPFLDS            GETS ME TO FIRST EDT FIELD                
         B     NXTEDT3                                                          
NXTEDT2  LA    R3,INVERR                                                        
         CLI   0(R2),0                                                          
         BE    NXTEDLST               LAST FIELD                                
NXTEDT3  OC    8(3,R2),8(R2)          CHK FOR EDITION CODE                      
         BZ    NXTEDLST               MEANS I'M DONE                            
         B     NXTEDT9                                                          
*                                                                               
NXTEDT7  DS    0H                GETS ME PAST LAST PREMIUM FIELD                
         LA    RF,4                                                             
         BAS   RE,BUMPFLDS              NEXT EDITION                            
         L     RE,LSTPTR                                                        
         LA    RE,6(RE)                                                         
         ST    RE,LSTPTR          GET NEXT PUB                                  
         CLI   0(RE),X'FF'         END OF LIST                                  
         BE    NXTEDLST                                                         
         B     NXTEDT2                                                          
*                                                                               
NXTEDT9  ST    R2,EDTADDR                                                       
*                                                                               
         L     RE,LSTPTR                                                        
         MVC   BPUB,0(RE)                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(1),BUYMD                                                     
         MVC   KEY+1(6),BPUB                                                    
         MVC   KEY+7(2),AGYALPHA                                                
         MVI   KEY+9,X'81'                                                      
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(25),KEYSAVE                                                  
         BE    NXTEDT10                                                         
         CLI   SVAGPROF+16,C'0'                                                 
         BE    NXTEDT8                                                          
         MVC   KEYSAVE+7(2),=C'ZZ'                                              
         CLC   KEYSAVE(25),KEY          SEE IF I FOUND DEFAULT                  
         BE    NXTEDT10                                                         
*                                                                               
NXTEDT8  MVC   KEY,KEYSAVE                                                      
         BAS   RE,READPUB                                                       
*                                                                               
NXTEDT10 MVC   SVPUBDA,KEY+27           SAVE DISK ADDR                          
         MVI   BFREQ,0                                                          
         XC    INSDA,INSDA              CLEAR DISK ADDR                         
         XC    INSKEY,INSKEY                                                    
         XC    INSADR,INSADR                                                    
         MVI   BSUBLN,0                                                         
         B     NXTEDTX                                                          
*                                                                               
NXTEDLST XC    EDTADDR,EDTADDR                                                  
NXTEDTX  XIT                                                                    
*                                                                               
TESTERR  CLI   ERRAREA,0                                                        
         BCR   8,R9                                                             
         B     EXXMOD                                                           
         EJECT                                                                  
BLDREC   GOTO1 VT41103,DMCB,(RC),(RA),C'BLD'                                    
         B     TESTERR                                                          
*                                                                               
RTSUB    GOTO1 VT41103,DMCB,(RC),(RA),C'RTS'                                    
         B     TESTERR                                                          
*                                                                               
ADDLINE  GOTO1 VT41103,DMCB,(RC),(RA),C'ADD'                                    
         B     TESTERR                                                          
*                                                                               
FNDINS   GOTO1 VT41103,DMCB,(RC),(RA),C'FND'                                    
         B     TESTERR                                                          
*                                                                               
ASR      GOTO1 VT41103,DMCB,(RC),(RA),C'ASR'                                    
         B     TESTERR                                                          
*                                                                               
ASC      GOTO1 VT41103,DMCB,(RC),(RA),C'ASC'                                    
         B     TESTERR                                                          
*                                                                               
COM      GOTO1 VT41103,DMCB,(RC),(RA),C'COM'                                    
         B     TESTERR                                                          
*                                                                               
EDTJOB   DS    0H                                                               
         BAS   RE,BUMPFLD                                                       
         CLI   5(R2),0                                                          
         BNE   EDTJOB2             HAVE INPUT                                   
         CLI   SVESTJOB,C' '       TEST HAVE ESTJOB                             
         BNHR  R9                  NO                                           
         MVC   8(6,R2),SVESTJOB                                                 
         MVI   5(R2),6             LENGTH                                       
         FOUT  (R2)                                                             
         B     EDTJOB4                                                          
*                                                                               
EDTJOB2  DS    0H                                                               
         CLC   =C'NONE',8(R2)                                                   
         BER   R9                                                               
         CLI   SVESTJOB,C' '                                                    
         BNH   EDTJOB4                                                          
         MVI   DUB,C' '                                                         
         MVC   DUB+1(5),DUB                                                     
         OC    DUB(6),8(R2)                                                     
         CLC   DUB(6),SVESTJOB      TEST INPUT VS EST JOB                       
         BE    EDTJOB4                                                          
         LA    R3,JOBERR4                                                       
         B     ERROR                                                            
EDTJOB4  DS    0H                                                               
         GOTO1 VT41103,DMCB,(RC),(RA),C'EDJ'                                    
         B     TESTERR                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
BUMPFLDS SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,BUMPFLDS       RF SET TO NUMBER OF FIELDS TO SKIP             
         BR    RE                                                               
*                                                                               
BUMPFLD  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
CLEARREC DS    0H             CLEAR END OF REC                                  
         MVC   HALF,REC+25                                                      
         SR    R1,R1                                                            
         LH    R1,HALF                                                          
         LA    RE,REC                                                           
         AR    RE,R1                                                            
         LA    RF,REC                                                           
         AHI   RF,4000                                                          
         SR    RF,RE                                                            
         XCEF                                                                   
         BR    R9                                                               
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    NEXTELX             CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NEXTEL                                                           
         LTR   R5,R5               CC IS NOT EQUAL                              
NEXTELX  BR    R9                  RETURN VIA R9                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CHECK NEWS GROSS NOT TOO LARGE                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKGRS   DS    0H                                                               
         CLI   PBDCOSTY,C'U'                                                    
         BNER  R9                                                               
         CP    PBDCOS,=P'300000000'                                             
         BH    CHKGRS8             PROBABLY TOTAL COST                          
*                                  NOT LINE OR INCH RATE                        
*                                  TRY TO PROTECT MULTILPY PACKED INS           
         ZAP   DUB,PBDUNITS                                                     
CHKGRS5  MP    DUB,PBDCOS          PBDCOS HAS 5 DECIMALS                        
         DP    DUB,=P'100'                                                      
         ZAP   DUB,DUB(6)                                                       
         DP    DUB,=P'10'                                                       
         CP    DUB(6),=P'2100000000'   MAX FOR FULL WORD                        
         BLR   R9                                                               
CHKGRS8  L     R2,TRADDR                                                        
         LA    RF,2                                                             
         BAS   RE,BUMPFLDS                                                      
         LA    R3,INVRTERR                                                      
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
ALLDONE  CLI   TESTPASS,1                                                       
         BE    ALLDONEX                                                         
         MVI   TESTPASS,1                                                       
         ZAP   TOTCOST,=P'0'                                                    
         L     R2,TRADDR                                                        
         MVC   8(2,R2),SVTRCODE    RESTORE REAL TRCODE                          
         B     BUYW2               NOW GO DO LIVE PASS                          
*                                                                               
ALLDONEX XC    BUYMSG,BUYMSG                                                    
         FOUT  BUYMSGH,CMPMSG                                                   
         MVI   ERRAREA,C'D'        FAKE ERROR TO SEND THIS MSG                  
         LA    R2,BUYPBH           PUT CURSOR TO PUB                            
         B     EXIT                                                             
*                                                                               
CMPMSG   DC    C'** ACTION COMPLETED **'                                        
         EJECT                                                                  
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
*                                                                               
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
*                                                                               
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBDIR)                                      
*                                                                               
READPUB  MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
*                                                                               
HIGHPUB  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
*                                                                               
PUBDIRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
*                                                                               
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* EXITS FROM PROGRAM                                                            
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
PATCH    DC    30X'00'                                                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FIXREC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,NEWREC+33                                                     
FIXREC05 BAS   R9,FRNXTEL                                                       
         BNE   FIXRECX                                                          
*                                                                               
* CHECKING FOR SPECIAL DELETE CODES                                             
*                                                                               
         CLI   ELCODE,X'86'                                                     
         BNE   *+18                                                             
         CLC   2(3,R5),=3X'FF'                                                  
         BE    FIXREC25                                                         
         B     FIXREC10                                                         
*                                                                               
         CLI   ELCODE,X'96'        EXTENSION DATE ELEM                          
         BNE   *+18                                                             
         CLC   2(3,R5),=3X'FF'                                                  
         BE    FIXREC25                                                         
         B     FIXREC10                                                         
*                                                                               
         CLI   ELCODE,X'83'                                                     
         BNE   *+16                                                             
         CLI   2(R5),X'FF'                                                      
         BE    FIXREC25                                                         
         B     FIXREC10                                                         
*                                                                               
         CLI   2(R5),C'X'                                                       
         BE    FIXREC25                                                         
*                                                                               
FIXREC10 CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   FIXERR1                                                          
         LR    R7,R5               SAVE R5, WHICH POINTS TO NEWREC              
         LA    R5,REC+33                                                        
         BAS   R9,FRNXTEL                                                       
         BNE   FIXREC15                                                         
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,1,1(R5)          ELEM LENGTH                                  
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R7)                                                    
         B     FIXREC70                                                         
*                                                                               
FIXREC15 CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   FIXERR1                                                          
         GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
         B     FIXREC70                                                         
*                                                                               
FIXREC25 LR    R7,R5               SAVE R5, WHICH POINTS TO NEWREC              
         SR    R4,R4                                                            
         LA    R5,REC+33           POINT R5 TO REC                              
FIXREC28 BAS   R9,FRNXTEL                                                       
         BE    FIXREC30                                                         
         LTR   R4,R4                                                            
         BZ    FIXERR2             ELEM NOT FOUND                               
         B     FIXREC50            GO DELETE ELEM                               
*                                                                               
FIXREC30 LR    R4,R5                                                            
         B     FIXREC28                                                         
*                                                                               
FIXREC50 GOTO1 VRECUP,DMCB,(1,REC),(R4)                                         
*                                                                               
         MVC   HALF,REC+25         CLEAR END OF RECORD                          
         SR    R1,R1                                                            
         LH    R1,HALF                                                          
         LA    RE,REC                                                           
         AR    RE,R1                                                            
         LA    RF,REC                                                           
         AHI   RF,4000                                                          
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
FIXREC70 LR    R5,R7               RESTORE R5 TO POINT TO NEWREC                
         B     FIXREC05            NEXTEL AGAIN, SHOULDN'T BE ANY MORE          
*                                                                               
FIXERR1  LA    R3,MAXSERR          ELEM MAXIMUM LENGTH ERROR                    
         B     FIXRERR                                                          
*                                                                               
FIXERR2  LA    R3,INVERR           NO ELEMENT IS FOUND                          
         L     R2,TRADDR                                                        
         LA    RF,7                                                             
         BAS   RE,FRBUMPFS         POINT R2 TO ERROR FIELD                      
         B     FIXRERR                                                          
*                                                                               
FIXRECX  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
FIXRERR  LTR   RB,RB               NOT EQUAL (ERROR)                            
         XIT1  REGS=(R2,R3)        ERROR MSG AND CURSOR POSITION                
*                                                                               
FRBUMPFS SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,FRBUMPFS         RF SET TO NUMBER OF FIELDS TO SKIP           
         BR    RE                                                               
*                                                                               
FRNXTEL  SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,R9                                                             
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
FRNXTELX LTR   R5,R5                                                            
         BR    R9                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* PRODUCT EXCLUSION RESTRICTIONS (BYPROF+6 N,X,W)                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BEXCL    NTR1  BASE=*,LABEL=*      BEGINNING OF EXCLUSION CLASS VAL             
*                                                                               
         CLI   BYPROF+6,0                                                       
         BE    BEXCLX              NOTHING IN PROFILE, DONE                     
         CLI   BYPROF+6,C'N'                                                    
         BE    BEXCLX              NO NEED TO CHECK FOR EXCL CLASS              
*                                                                               
         CLI   SVPEXCL,0                                                        
         BE    BEXCLX              NO EXCL CLASS, DONE                          
*                                                                               
         L     R5,APUBIO                                                        
         LA    R5,33(R5)           POINT TO ELEMENTS                            
         MVI   ELCODE,X'20'                                                     
         BAS   R9,BXNXTEL          LOOKING FOR PRODUCTION ELEM                  
         BNE   BEXCLX              NOT FOUND, DONE WITH EXCL CLASS              
         USING PUBGENEL,R5                                                      
         CLI   PUBEXCL,0                                                        
         BE    BEXCLX              NO EXCL CLASS, DONE                          
         MVC   BYTE,PUBEXCL                                                     
         NC    BYTE,SVPEXCL                                                     
         BZ    BEXCLX              NO CONFLICTS IN EXCL CLASS, DONE             
*                                                                               
         XC    BUYMSG,BUYMSG                                                    
         CLI   BYPROF+6,C'X'                                                    
         BE    BEXCLER1            EXCL CLASS IS NOT ALLOWED IN BOOKING         
         CLI   BYPROF+6,C'W'                                                    
         BNE   BEXCLX              NO OTHER VALUES IN PROF+6                    
*                                                                               
         LA    R2,BUYTR1H                                                       
         LA    RF,44               BUMP 44 FIELDS OVER                          
         BAS   RE,BXBUMPFS                                                      
         XC    8(55,R2),8(R2)                                                   
         MVC   8(L'EXCLWAR1,R2),EXCLWAR1                                        
         OI    6(R2),OI1T                                                       
*                                                                               
         OI    WARN,X'10'          TURN ON EXCL CLASS WARNING BIT               
         B     BEXCLX                                                           
         DROP  R5                                                               
*                                                                               
BEXCLER1 MVC   BUYMSG+1(L'EXCLERR1),EXCLERR1                                    
         LA    R2,BUYTR1H                                                       
         MVI   ERRAREA,X'FF'                                                    
         B     BEXCLERR                                                         
*                                                                               
EXCLERR1 DC    C'ERROR: EXCLUSION CLASS CONFLICT, BOOKING NOT ADDED'            
EXCLWAR1 DC    C'** WARNING: EXCLUSION CLASS CONFLICT, BOOKING ADDED'           
*                                                                               
BEXCLX   CR    RB,RB               EQUAL                                        
         B     *+6                                                              
BEXCLERR LTR   RB,RB               NOT EQUAL (ERROR)CLASS VALIDATION            
         XIT1                                                                   
*                                                                               
BXBUMPFS SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,BXBUMPFS                                                      
         BR    RE                                                               
*                                                                               
BXNXTEL  SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,R9                                                             
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
BXNXTELX LTR   R5,R5                                                            
         BR    R9                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* NO RATE CHANGES FOR FULLY PAID BUYS  (BYPROF+7 IS "R" OR "B")                 
* NO BUY DELETIONS FOR FULLY PAID BUYS (BYPROF+7 IS "D" OR "B")                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKRATE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LHI   R3,NOFRZCER         NO FINANCIAL CHANGES TO FRZ CLIENT           
         CLI   BYPROF+14,C'Y'      NO CHANGE TO FRZ CLIENT?                     
         JNE   CKR08                                                            
         TM    SVCLPROF+30,X'02'   FROZEN CLIENT?                               
         JZ    CKR08                                                            
*                                                                               
         TM    SVCLPROF+27,X'08'   LOCK THIS MONTH AND ALL FORWARD?             
         JNZ   CKR06B                                                           
         TM    SVCLPROF+27,X'04'   LOCK THIS MONTH AND ALL PRIOR?               
         JNZ   CKR06H                                                           
         TM    SVCLPROF+27,X'02'   LOCK THIS MONTH ONLY?                        
         JNZ   CKR06M                                                           
*                                                                               
         J     CKR48ERR            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKR06B   CLC   PBUYKDAT(2),SVCLPROF+28                                          
         JL    CKR08                                                            
         J     CKR48ERR            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKR06H   CLC   PBUYKDAT(2),SVCLPROF+28                                          
         JH    CKR08                                                            
         J     CKR48ERR            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKR06M   CLC   PBUYKDAT(2),SVCLPROF+28                                          
         JNE   CKR08                                                            
         J     CKR48ERR            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKR08    CLI   BYTE,C'C'           CHANGING?                                    
         BNE   CKR10                                                            
         CLI   BYPROF+7,C'R'       CKING PROFILE                                
         BE    CKR20                                                            
         CLI   BYPROF+7,C'B'                                                    
         BE    CKR20                                                            
         B     CKRXX                                                            
*                                                                               
CKR10    CLI   BYTE,C'D'           DELETING?                                    
         BNE   CKR15                                                            
         CLI   BYPROF+7,C'D'       CKING PROFILE                                
         BE    CKR20                                                            
         CLI   BYPROF+7,C'B'                                                    
         BE    CKR20                                                            
         B     CKRXX                                                            
*                                                                               
CKR15    DC    H'0'                THERE'S NO OTHER ACTION AT THIS TIME         
*                                                                               
CKR20    DS    0H                                                               
*                                                                               
         GOTO1 VGETINS,DMCB,REC,PVALUES,REC+7                                   
*                                                                               
         CLC   PGROSS(12),GROSS    COMPARE UP TO CASH DISCOUNT                  
         BNE   CKRXX                                                            
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'25'        PAY ELEM EXIST?                              
         BAS   R9,CRNXTEL                                                       
         BNE   CKRXX               ALLOW RATE CHG/DEL                           
         OC    2(3,R5),2(R5)                                                    
         BZ    CKRXX               NO DATE, ALLOW RATE CHG/DEL                  
*                                                                               
CKR38ERR XC    BUYMSG,BUYMSG                                                    
         CLI   BYTE,C'C'                                                        
         BNE   CKR40                                                            
         MVC   BUYMSG+1(L'RCHAERR1),RCHAERR1                                    
         B     CKR50                                                            
CKR40    MVC   BUYMSG+1(L'RDELERR1),RDELERR1                                    
         B     CKR50                                                            
*                                                                               
CKR48ERR XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG+1(L'NOFRZRR1),NOFRZRR1                                    
*                                                                               
CKR50    LA    R2,BUYTR1H                                                       
         MVI   ERRAREA,X'FF'                                                    
         B     CKRERR                                                           
*                                                                               
RCHAERR1 DC    C'ERROR: PROFILE DOES NOT ALLOW RATE CHANGES'                    
RDELERR1 DC    C'ERROR: PROFILE DOES NOT ALLOW BUY DELETIONS'                   
NOFRZRR1 DC    C'NO FINANCIAL CHANGES TO FROZEN CLIENT CODE'                    
*                                                                               
CKRXX    CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKRERR   LTR   RB,RB               NOT EQUAL (ERROR)                            
EXIT_XX  XIT1                                                                   
*                                                                               
CRNXTEL  SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,R9                                                             
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
CRNXTELX LTR   R5,R5                                                            
         BR    R9                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKEIOELM NTR1  BASE=*,LABEL=*      CK EIO ELEM                                  
*                                                                               
         LA    R5,REC+33                                                        
         USING PWIOELEM,R5                                                      
         MVI   ELCODE,PWIOELCQ                                                  
CKEIO10  BRAS  R9,NEXTEL                                                        
         BNE   CKEIO20                                                          
         LR    RF,R5                                                            
         B     CKEIO10                                                          
*                                                                               
CKEIO20  LR    R5,RF                                                            
         CLI   PWIOELCO,PWIOELCQ   LAST EIO ELEM FOUND?                         
         BNE   CKEIO_X                                                          
         CLI   PWIOINSL,0          NO INSERTION LINE NUMBER?                    
         BNE   CKEIO_X                                                          
         MVC   PWIOINSL,REC+24     SAVE INSERTION LINE NUMBER                   
*                                                                               
CKEIO_X  J     EXIT_XX                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKMATPTR NTR1  BASE=*,LABEL=*      CK FOR MAT= REPEAT PASSIVE PTR               
*                                                                               
         MVC   BYTE4,DMINBTS                                                    
         LA    R5,REC+33                                                        
         USING PWIOELEM,R5                                                      
         MVI   ELCODE,PWIOELCQ                                                  
CKMPP10  BRAS  R9,NEXTEL                                                        
         BNE   CKMPP20                                                          
         LR    RF,R5                                                            
         B     CKMPP10                                                          
*                                                                               
CKMPP20  LR    R5,RF                                                            
         CLI   PWIOELCO,PWIOELCQ   LAST EIO ELEM FOUND?                         
         BNE   CKMPP_X                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PWIOINDT),(2,DUB)                                
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING PMTPKEY,RF                                                       
         MVC   KEY(10),REC         AGY/MED/X/CLT/PRD                            
         MVI   PMTPKRCD,PMTPKRCQ   MAT= REPEAT PASSIVE CODE                     
         MVC   PMTPKADC,PWIOADCD   AD CODE                                      
         MVC   PMTPKPUB,REC+10     PUB                                          
         TM    PWIOSTAT,PWIOSMAQ   MAT= REPEAT ACROSS ZONE/EDITION?             
         BNZ   *+10                                                             
         MVC   PMTPKZON(2),REC+14                                               
         MVC   PMTPKDAT,DUB        COMPRESSED INSERTION DATE                    
         MVC   PMTPKLIN,PWIOINSL   INSERTION LINE NUMBER                        
         CLI   PMTPKLIN,0                                                       
         BH    *+6                                                              
         DC    H'0'                INSERTION LINE NUMBER IS NOT SET             
         NI    DMINBTS,X'FF'-X'08' NO DELETES                                   
         BRAS  RE,HIGH                                                          
         CLC   KEY(L'PMTPKEY),KEYSAVE                                           
         BNE   CKMPP_X                                                          
         OI    KEY+25,PMTPC1DQ     SET TO DELETE                                
         BRAS  RE,WRITE                                                         
         BRAS  RE,CHECK                                                         
*                                                                               
CKMPP_X  MVC   DMINBTS,BYTE4                                                    
         J     EXIT_XX                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R5,RF                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TEST DATA LOCKED BY OFFLINE APPLICATION                                       
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS               
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,BUYCL                                                  
*                                                                               
TSTLK2   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK2                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         BNE   TSTLK4                                                           
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK3   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK3                                                           
*                                                                               
TSTLK4   XC    LKUPKEY,LKUPKEY                                                  
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BP'    CLIENT LOCK                                  
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,BUYCL                                                  
         MVC   L.LOCKPUB,REC+10    PACKED BASE PUB NUMBER                       
         XC    L.LOCKPUB,=4X'FF'   COMPLEMENT PUB (NO BINARY ZERO)              
*                                                                               
TSTLK5   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK5                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         BNE   TSTLKEQ                                                          
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK6   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK6                                                           
*                                                                               
TSTLKEQ  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB               NOT EQUAL                                    
         XIT1                                                                   
*                                                                               
         DROP  L                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TSTLKWKA DS    0H                                                               
*                                                                               
LKUPKEY  DS    XL16                LOCKUP KEY                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SUB ROUTINE TO EDIT LINES                                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTLIN   CSECT                                                                  
         NMOD1 0,EDTLIN,RR=R9                                                   
         ST    R9,ERELO                                                         
         B     *+8                                                              
ERELO    DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         MVI   RATIND,0       USED FOR NNXNNNL FORMAT TO TELL                   
*                             EDTRTN TO EDIT FOR RATE                           
         BAS   RE,BUMPFLD                                                       
         LA    R3,INVERR                                                        
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         LA    R4,8(R2)                                                         
         CLI   0(R4),C'*'          TEST NO INSERTION IND                        
         BE    EDTLNS0                                                          
         CLI   0(R4),C'#'          TEST NO ASC IND                              
         BNE   EDTLNS1                                                          
*                                                                               
EDTLNS0  DS    0H                                                               
         MVC   PBDSPACE(1),0(R4)   SET NO INSERTION IND                         
         LA    R4,1(R4)            BUMP POINTER                                 
         BCTR  R5,0                AND LENGTH                                   
         LTR   R5,R5                                                            
         BZ    EDTLNS8                                                          
*                                                                               
EDTLNS1  STM   R4,R5,DUB           SAVE PTR/LEN                                 
EDTLNS2  CLI   0(R4),C'0'          VERIFY NUMERIC                               
         BL    EDTLNS3                                                          
         CLI   0(R4),C'9'                                                       
         BH    ERROR                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,EDTLNS2                                                       
*                                                                               
EDTLNS3  DS    0H                                                               
         L     RE,DUB              START OF NUMBER                              
         SR    R4,RE               R4 = LENGTH                                  
         BNP   EDTL9                                                            
         BCTR  R4,R0                                                            
         EX    R4,PACKLN                                                        
         CP    WORK(5),=P'99999'   MAX LINES OR INCHES                          
         BH    ERROR               THAT CAN BE CARRIED IN PBDUNITS              
         ZAP   PBDUNITS,WORK(5)                                                 
*                                                                               
         MVI   PBDUIND,C'L'                                                     
         LTR   R5,R5                                                            
         BZ    EDTLNS8             NO MORE INPUT                                
         LA    R4,1(RE,R4)         R4 = BYTE PAST NUMBER                        
         CLI   0(R4),C'L'                                                       
         BE    EDTLNS4                                                          
         CLI   0(R4),C'/'                                                       
         BE    EDTLNS5                                                          
         MVI   PBDUIND,C'I'                                                     
         CLI   0(R4),C'I'                                                       
         BE    EDTLNS4                                                          
         CLI   0(R4),C'X'          NEW SAU NNXNN.NN                             
*                                  COLUMNS X INCHES (2 DECIMALS)                
         BNE   EDTLNS3C            NO                                           
         CP    PBDUNITS,=P'13'     MAX COLS =13                                 
         BH    EDTL9               TREAT AS SPACE                               
         CP    PBDUNITS,=P'0'                                                   
         BNH   EDTL9                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,*+8              NO MORE INPUT                                
         B     EDTL9               NO MUST BE SPACE                             
         CLC   0(2,R4),=C'FD'                                                   
         BNE   EDTLNS3B                                                         
         CLI   2(R4),C' '                                                       
         BNL   EDTL9               INPUT AFTER FD                               
         L     R5,APUBIO                                                        
         CLI   0(R5),0             SEE IF PUB THERE                             
         BNE   EDTLNS32                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+27(4),SVPUBDA                                                
*                                  MUST REREAD PUB                              
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PUBFILE',         X        
               KEY+27,APUBIO,(TERMNAL,DMWORK)                                   
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                MUST FIND PUB                                
*                                                                               
EDTLNS32 LA    R5,33(R5)                                                        
         MVI   ELCODE,X'20'                                                     
         ST    R9,DUB              SAVE R9                                      
         BAS   R9,NEXTEL                                                        
         BE    EDTLNS3A                                                         
         L     R9,DUB              RESTORE R9                                   
         B     EDTL9               CAN'T FIND PROD ELEM                         
*                                                                               
         USING PUBGENEL,R5                                                      
EDTLNS3A L     R9,DUB              RESTORE R9                                   
         OC    PUBFD,PUBFD         SEE IF I HAVE FD                             
         BZ    EDTL9               NO - TREAT AS SPACE                          
         ZAP   PBDCLMS,PBDUNITS                                                 
         ZAP   DUB,PBDUNITS                                                     
         MP    DUB,PUBFD                                                        
         CP    DUB,=P'99999'       MAX IS 999.99 COL INCHES                     
         BH    ERROR                                                            
         ZAP   PBDUNITS,DUB                                                     
         NI    PBDUIND,X'BF'       MAKE I LOWER CASE                            
         B     EDTL9B                                                           
*                                                                               
         DROP  R5                                                               
*                                                                               
EDTLNS3B LR    R1,R4                                                            
         AR    R1,R5                                                            
         BCTR  R1,0                                                             
         CLI   0(R1),C'L'       SEE IF NNXNNNL FORMAT                           
         BNE   EDTLN5                                                           
         CHI   R5,2                                                             
         BL    EDTL9             MUST BE A LEAST NL                             
*                                IF NOT TREAT AS SPACE                          
*                                                                               
         BCTR  R5,0                                                             
         GOTO1 VCASHVAL,DMCB,(2,0(R4)),(R5)                                     
         CLI   DMCB,X'FF'                                                       
         BE    EDTL9                                                            
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    EDTL9                                                            
         ZAP   PBDCLMS,PBDUNITS                                                 
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'           CAN'T BE NEGATIVE                            
         BNH   EDTL9                                                            
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET REMAINDER ZERO                      
         BNE   EDTL9                                                            
         ZAP   DUB,DUB(6)                                                       
         MP    DUB,PBDCLMS                                                      
         CP    DUB,=P'99999'       MAX LINES                                    
         BH    EDTL9                                                            
         MVI   PBDUIND,C'L'        RESET TO LINES - L                           
         ZAP   PBDUNITS,DUB                                                     
         MVI   RATIND,1                                                         
         B     EDTL9B              SAVES SPACE                                  
*                                                                               
EDTLN5   GOTO1 VCASHVAL,DMCB,(2,0(R4)),(R5)                                     
         CLI   DMCB,X'FF'                                                       
         BE    EDTL9                                                            
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    EDTL9                                                            
         ZAP   PBDCLMS,PBDUNITS                                                 
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'           CAN'T BE NEGATIVE                            
         BNH   EDTL9                                                            
         MP    DUB,PBDCLMS                                                      
         CP    DUB,=P'99999'       MAX COLUMN INCHES                            
         BH    EDTL9                                                            
         NI    PBDUIND,X'BF'       MAKE LOWER CASE I                            
         ZAP   PBDUNITS,DUB                                                     
         B     EDTL9B              SAVES SPACE                                  
*                                                                               
EDTLNS3C DS    0H                  CHK FOR DECIMAL POINT                        
         CLI   0(R4),C'.'                                                       
         BNE   EDTL9               TREAT AS SPACE                               
         BCT   R5,*+8                                                           
         B     EDTL9               NO MORE INPUT                                
         LA    R4,1(R4)                                                         
         CLI   0(R4),C'I'                                                       
         BNE   EDTLNS3E                                                         
         BCT   R5,*+8                                                           
         B     EDTLNS8             TREAT NN.I AS NN (NO DECIMALS)               
         B     EDTL9               INPUT AFTER I TREAT AS SPACE                 
*                                                                               
EDTLNS3E ST    R4,DUB                                                           
EDTLNS3F CLI   0(R4),C'I'                                                       
         BE    EDTLNS3G                                                         
         CLI   0(R4),C'0'                                                       
         BL    EDTL9                                                            
         CLI   0(R4),C'9'                                                       
         BH    EDTL9                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,EDTLNS3F                                                      
         B     EDTL9               IF DOESN'T END WITH I ASSUME SPACE           
*                                                                               
EDTLNS3G BCTR  R5,0                MUST DECREMENT R5                            
         LTR   R5,R5                                                            
         BNZ   EDTL9               MORE INPUT - TREAT AS SPACE                  
         L     RE,DUB              START OF NUMBER                              
         SR    R4,RE               R4 = LENGTH                                  
         BCTR  R4,R0               ADJUST FOR I                                 
         EX    R4,PACKLN                                                        
         ZAP   DUB,WORK(5)                                                      
         CP    DUB,=P'0'           SEE IF NN.00  INPUT                          
         BE    EDTLNS8             TREAT AS NNI                                 
         XC    WORK(13),WORK       BUILD LINE FOR CASHVAL                       
         ZIC   R5,5(R2)            INPUT LENGHT                                 
         LA    R4,8(R2)                                                         
         CLI   0(R2),20            SEE IF DOING CLE                             
         BNH   EDTLNS3I                                                         
         AHI   R5,-4               ADJUST FOR CLE=                              
         LA    R4,4(R4)            BUMP PAST CLE=                               
*                                                                               
EDTLNS3I BCTR  R5,0                                                             
         BCTR  R5,0                SO I WON'T MOVE THE I                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4)                                                    
         LA    R5,1(R5)            RESTORE FROM THE EX                          
         GOTO1 VCASHVAL,DMCB,(2,WORK),(R5)                                      
         CLI   DMCB,X'FF'                                                       
         BE    EDTL9               IF ERROR TREAT AS SPACE                      
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'99999'                                                    
         BH    ERROR               CAN'T FIT IN PBDUNITS                        
         ZAP   PBDUNITS,DUB                                                     
         NI    PBDUIND,X'BF'       MAKE I LOWER CASE 'I'                        
*                                  INCHES TO 2 DECIMALS                         
         B     EDTL9B              SAVES SPACE                                  
*                                                                               
EDTLNS4  DS    0H                                                               
         BCT   R5,*+8              NO MORE INPUT AFTER L OR I                   
         B     EDTLNS8                                                          
         CLI   1(R4),C'/'          '/'  MUST BE NEXT                            
         BNE   EDTL9                                                            
         LA    R4,1(R4)                                                         
*                                                                               
EDTLNS5  DS    0H                                                               
         LA    R4,1(R4)                                                         
*                                                                               
EDTLNS6  DS    0H                                                               
         BCT   R5,*+8              NO MORE INPUT                                
         B     EDTL9                                                            
         ST    R4,DUB              START OF NUMBER                              
EDTLNS6A CLI   0(R4),C'0'                                                       
         BL    EDTL9                                                            
         CLI   0(R4),C'9'                                                       
         BH    EDTL9                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,EDTLNS6A                                                      
         L     RE,DUB              START OF NUMBER                              
         SR    R4,RE               R4 = LENGTH                                  
         BCTR  R4,R0                                                            
         EX    R4,PACKLN                                                        
         CP    WORK(5),=P'999'     MAX COLUMNS IS 999                           
         BH    ERROR               CAN'T FIT IN PDBCLMS                         
         LA    R3,DVSERR           DIVISIBILITY ERROR                           
         ZAP   PBDCLMS,WORK(5)                                                  
         BZ    ERROR                                                            
         ZAP   DUB,PBDUNITS                                                     
         BZ    ERROR                                                            
         DP    DUB,PBDCLMS                                                      
         CP    DUB+8-L'PBDCLMS(L'PBDCLMS),=P'0'                                 
         BNE   ERROR                                                            
*                                  CHECK VS JOB RECORD                          
EDTLNS8  DS    0H                                                               
         CLI   COMPASS,1           COMMON LINE PASS                             
         BE    EDTLINX                                                          
*                                                                               
         CLI   0(R2),20            SKIP IF CLE EDIT                             
         BH    EDTLINX                                                          
         OC    PBDJOB,PBDJOB                                                    
         BZ    EDTLINX                                                          
         L     R4,AJOBIO                                                        
         CLC   PBDSPACE(2),=C'* '  TEST SPACE BUY                               
         BNH   EDTLNS8A            NO                                           
         MVI   X,C' '                                                           
         MVC   X+1(16),X                                                        
         OC    X(17),PBDSPACE                                                   
         CLC   X(17),PJOBSPC-PJOBREC(R4)                                        
         BE    EDTLINX                                                          
         B     EDTLNS8B                                                         
*                                                                               
EDTLNS8A DS    0H                                                               
         CP    PBDUNITS,PJOBTUNS-PJOBREC(3,R4)                                  
         BNE   EDTLNS8B                                                         
         CP    PBDCLMS,PJOBCOLS-PJOBREC(3,R4)                                   
         BNE   EDTLNS8B                                                         
         CLC   PBDUIND,PJOBUIND-PJOBREC(R4)                                     
         BNE   EDTLNS8B                                                         
         B     EDTLINX                                                          
EDTLNS8B DS    0H                                                               
         OI    WARN,X'80'          SPACE WARNING                                
         B     EDTLINX                                                          
         SPACE 3                                                                
EDTL9    DS    0H                                                               
*                                  TREAT AS SPACE DESC                          
         B     ERROR               MUST BE LINES OR INCHES                      
*                                                                               
EDTL9B   OI    4(R2),X'20'                                                      
         MVC   PBDSPACE(8),8(R2)                                                
         B     EDTLNS8                                                          
*                                                                               
PACKLN   PACK  WORK(5),0(0,RE)                                                  
*                                                                               
EDTLINX  XMOD1 1                                                                
         LTORG                                                                  
LPATCH   DC    40X'00'                                                          
         EJECT                                                                  
FMTINFO  CSECT                                                                  
         NMOD1 0,FMTI                                                           
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
         SR    R0,R0               POINT TO NEXT FIELD                          
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    8(50,R2),8(R2)        CLEAR LINE FIRST                           
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'35'          FIND WSJ ELEM                              
         BAS   R9,FNEXTEL                                                       
         BNE   FMTINFX                                                          
         USING PWSJELEM,R5                                                      
         LA    R4,8(R2)                                                         
         USING DLINE,R4                                                         
         CP    PWSJUNTS,=P'0'      SEE IF ANY COMMON UNITS                      
         BE    FMTINF10                                                         
         CLI   PBDUIND,X'89'                                                    
         BE    FMTINF5                                                          
         EDIT  (P3,PWSJUNTS),(7,NUNITS),0,TRAIL=C'L'                            
         CLI   PBDUIND,C'L'                                                     
         BE    FMTINF8                                                          
         EDIT  (P3,PWSJUNTS),(7,NUNITS),0,TRAIL=C'I'                            
         B     FMTINF8                                                          
FMTINF5  EDIT  (P3,PWSJUNTS),(7,NUNITS),2,TRAIL=C'I'                            
*                                                                               
FMTINF8  MVC   NAT,=C'AT'                                                       
*                                                                               
         EDIT  (P5,PWSJNCOS),(8,NRATE),5,ALIGN=LEFT                             
FMTINF10 ZAP   DUB,PBDUNITS                                                     
         SP    DUB,PWSJUNTS                                                     
*                                                                               
         DROP  R5                                                               
*                                                                               
         ZAP   DOUBLE,DUB       NOW I CAN USE DOUBLE                            
         CP    DOUBLE,=P'0'     CHK FOR EXCESS UNITS                            
         BNH   FMTINF20                                                         
         CLI   PBDUIND,X'89'                                                    
         BE    FMTINF15                                                         
         EDIT  (P8,DOUBLE),(7,EUNITS),0,TRAIL=C'L'                              
         CLI   PBDUIND,C'L'                                                     
         BE    FMTINF18                                                         
         EDIT  (P8,DOUBLE),(7,EUNITS),0,TRAIL=C'I'                              
         B     FMTINF18                                                         
FMTINF15 EDIT  (P8,DOUBLE),(7,EUNITS),2,TRAIL=C'I'                              
*                                                                               
FMTINF18 MVC   EAT,=C'AT'                                                       
         USING PWSJELEM,R5             NEED R5 FOR PWSJECOS                     
         EDIT  (P5,PWSJECOS),(8,ERATE),5,ALIGN=LEFT                             
         DROP  R5                                                               
*                                                                               
FMTINF20 ZAP   DOUBLE,PBDCOS                                                    
         AP    DOUBLE,PBDPRCOS         AND PREMIUM CHARGE                       
         EDIT  (P8,DOUBLE),(10,TCOST),2,ALIGN=LEFT                              
         AP    TOTCOST,DOUBLE                                                   
FMTINFX  FOUT  (R2)                                                             
         XIT1                                                                   
**                                                                              
FNEXTEL  SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,R9                                                             
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
FNEXTELX LTR   R5,R5                                                            
         BR    R9                                                               
         SPACE 3                                                                
         LTORG                                                                  
FPATCH   DC    30X'00'                                                          
*                                                                               
         EJECT                                                                  
EDTC     CSECT                                                                  
         NMOD1 0,EDTC,RR=R9                                                     
*                                                                               
         ST    R9,CRELO                                                         
         B     *+8                                                              
CRELO    DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         MVI   PRVTAB,0           USED IN THIS PROGRAM                          
*                                 TO TELL ME IF KEY WORD WAS ENTERED            
*                                                                               
         BAS   RE,CUMPFLD         NEXT FIELD                                    
         L     R7,ACOMWRK                                                       
         CLI   5(R2),0             CHK FOR INPUT                                
         BE    EDTCM10                                                          
*                                                                               
         GOTO1 VSCANNER,DMCB,0(R2),0(R7)                                        
         ZIC   R9,DMCB+4        NUMBER OF LINES                                 
         CLI   DMCB+4,0                                                         
         BE    EDTCMX           SCANNER ERROR                                   
         USING SCAND,R7                                                         
*                               USE SCANNER FOR OPTIONS FIELD                   
*                                                                               
EDTCOM1A CLI   FLD1L,2                                                          
         BNE   EDTCOM1B                                                         
         CLC   =C'CD',FLD1                                                      
         BE    EDTCCD                                                           
         CLC   =C'AC',FLD1                                                      
         BE    EDTCAC                                                           
         CLC   =C'PD',FLD1                                                      
         BE    EDTCPDDT                                                         
         CLC   =C'BD',FLD1                                                      
         BE    EDTCBLDT                                                         
         CLC   =C'ID',FLD1                                                      
         BE    EDTCIODT                                                         
         CLC   =C'SD',FLD1                                                      
         BE    EDTCSHDT                                                         
         CLC   =C'PC',FLD1                                                      
         BE    EDTPLCOS                                                         
         B     EDTCMX                                                           
*                                                                               
EDTCOM1B CLI   FLD1L,3                                                          
         BNE   EDTCOM1E                                                         
         CLC   =C'CLE',FLD1                                                     
         BE    EDTCLE            NOW SENDS ERROR                                
         CLC   =C'REF',FLD1                                                     
         BE    EDTREF                                                           
         CLC   =C'SFH',FLD1                                                     
         BE    EDTSFH                                                           
         B     EDTCMX                                                           
*                                                                               
EDTCOM1E DS    0H                                                               
         CLI   FLD1L,4                                                          
         BNE   EDTCOM1K                                                         
         CLC   =C'SREP',FLD1                                                    
         BE    EDTSREP                                                          
         CLC   =C'IMPS',FLD1                                                    
         BE    EDTIMPS             ERROR FOR NOW                                
         B     EDTCMX                                                           
*                                                                               
EDTCOM1K DS    0H                                                               
         CLI   FLD1L,6                                                          
         BNE   EDTCMX                                                           
         CLC   =C'EXDAYS',FLD1     EXTENSION DAYS                               
         BE    EDTEXD                                                           
         CLC   =C'EXDATE',FLD1     EXTENSION DATE                               
         BE    EDTEXDT                                                          
         B     EDTCMX                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
* GET HERE IF SCANNER FINDS AN ERROR OR IF A NON-KEY WORD IS ENTERED            
* TREAT AS A 'FREE FORM' COMMENT                                                
*                                                                               
EDTCMX   DS    0H                                                               
         L     R1,TRADDR                                                        
         CLI   8(R1),C'B'          SEE IF BUYING                                
         BNE   FLDINV              IF NOT - THEN INVALID                        
*                                                                               
         CLI   PRVTAB,1            MEANS KEY WORD WAS ENTERED                   
         BE    FLDINV              CAN'T DO COMMENT AND KEY WORD(S)             
*                                                                               
         CLI   5(R2),70            MAX COMMENT SIZE                             
         BH    FLDINV                                                           
*                                                                               
* SEE IF FIELD STARTED WITH A KEY WORD                                          
* IF SO,THEN I GOT HERE WITH A SCANNER ERROR                                    
* (THEY TRIED TO ENTER A KEY WORD AND A COMMENT)                                
*                                                                               
         CLC   =C'CD=',8(R2)                                                    
         BE    FLDINV                                                           
         CLC   =C'AC=',8(R2)                                                    
         BE    FLDINV                                                           
         CLC   =C'PD=',8(R2)                                                    
         BE    FLDINV                                                           
         CLC   =C'BD=',8(R2)                                                    
         BE    FLDINV                                                           
         CLC   =C'SD=',8(R2)                                                    
         BE    FLDINV                                                           
         CLC   =C'ID=',8(R2)                                                    
         BE    FLDINV                                                           
         CLC   =C'PC=',8(R2)                                                    
         BE    FLDINV                                                           
         CLC   =C'CLE=',8(R2)                                                   
         BE    FLDINV                                                           
**IMPS**                                                                        
         CLC   =C'IMPS=',8(R2)                                                  
         BE    FLDINV                                                           
**IMPS**                                                                        
         CLC   =C'SREP=',8(R2)                                                  
         BE    FLDINV                                                           
         CLC   =C'REF=',8(R2)                                                   
         BE    FLDINV                                                           
*                                                                               
         SR    RE,RE                                                            
         MVI   0(R7),X'66'                                                      
         IC    RE,5(R2)                                                         
         LA    R4,8(R2)                                                         
         CLC   =C'IC=',8(R2)                                                    
         BE    EDTCM1B                                                          
         CLC   =C'IB=',8(R2)                                                    
         BE    EDTCM1B                                                          
**PI                                                                            
         CLC   =C'PI=',8(R2)                                                    
         BNE   EDTCM2                                                           
         MVI   0(R7),X'68'                                                      
         B     EDTCM1C                                                          
*                                                                               
EDTCM1B  MVI    0(R7),X'67'                                                     
EDTCM1C  AHI    RE,-3                                                           
*                                                                               
         LA    R4,11(R2)                                                        
         CLC   0(4,R4),=C'COM='                                                 
         BNE   EDTCM2                                                           
*                                  STANDARD COMMENT                             
         CHI   RE,10               1-6 CHARS                                    
         BH    FLDINV                                                           
         CHI   RE,5                                                             
         BL    FLDINV                                                           
         ST    RE,FULL             SAVE RE IN FULL  NEEDED IN EDTCM2            
*                                                                               
         AHI   RE,-4                                                            
         MVC   WORK(6),=6C' '                                                   
         LA    R5,WORK+6                                                        
         SR    R5,RE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),4(R4)       RIGHT ALIGN IN WORK                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD      MEDIA                                        
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),WORK                                                    
         BAS   RE,HIGH                                                          
         L     RE,FULL             RESTORE RE FOR EDTCM2                        
         CLC   KEY(10),KEYSAVE                                                  
         BE    EDTCM2              FOUND - OK                                   
*                                                                               
         LA    R3,NFNDERR                                                       
         B     ERROR                                                            
*                                                                               
EDTCM2   DS    0H                                                               
         LA    R0,2(RE)                                                         
         STC   R0,1(R7)                                                         
         LTR   RE,RE                                                            
         BNZ   EDTCM2D                                                          
         CLC   =C'IB=',8(R2)                                                    
         BNE   NEXTD                                                            
         MVC   80(2,R7),=X'6602'   SO IB= WILL DELETE BOTH 67 AND 66            
EDTCM2B  LA    R7,80(R7)                                                        
         B     NEXTD                                                            
*                                                                               
EDTCM2D  BCTR  RE,0                                                             
         EX    RE,MVCOM                                                         
         CLC   =C'IB=',8(R2)       IB=  SO GENERATE BOTH 66 AND 67              
         BNE   EDTCM3                                                           
         LA    R3,2                FOR BCT                                      
         MVI   80(R7),X'66'                                                     
         STC   R0,81(R7)           STORE LENGHT                                 
         EX    RE,MVCOM2                                                        
EDTCM3   OC    INSDA,INSDA         TEST ADD                                     
         BC    7,EDTCM2B           NEXT COMMENT LINE                            
EDTCM4   MVI   ELCODE,X'FF'        TO GET TO END OF REC                         
         LA    R5,NEWREC+33                                                     
         LR    RE,R9                                                            
         BAS   R9,CNEXTEL                                                       
         LR    R9,RE                                                            
         GOTO1 VRECUP,DMCB,(1,NEWREC),0(R7),(R5)                                
         CLC   =C'IB=',8(R2)                                                    
         BNE   EDTCM10                                                          
         LA    R7,80(R7)           TO GET PAST 67 ELEMENT                       
         BCT   R3,EDTCM4           TO ADD THE OTHER ELEMENT                     
         B     EDTCM10             +4 SO I WON'T BUMP R7 TWICE                  
*                                                                               
MVCOM    MVC   2(0,R7),0(R4)       EXECUTED                                     
MVCOM2   MVC   82(0,R7),0(R4)      EXECUTED                                     
MVCOM4   MVC   0(0,R7),0(R5)       EXECUTED                                     
*                                                                               
NEXTD    LA    R7,32(R7)           NEXT SCANNER FIELD                           
         OI    PRVTAB,X'01'        SET KEY WORK INPUT                           
         OC    0(2,R7),0(R7)       CHK FOR NO LENGTHS                           
         BZ    EDTCM10                                                          
*                                  NEEDED SINCE SCANNER HAS A BUG               
*                                  AND RETURNS THE WRONG NUMBER                 
*                                  OF LINES IF SECOND FIELD IS MISSING          
         BCT   R9,EDTCOM1A                                                      
*                                  NOW SEE IF ESTHDR HAD REP                    
*                                  WAS SAVED IN SVESPROF+15                     
*                                                                               
EDTCM10  DS    0H                                                               
*                                  CHECK FOR LONG COMMENTS                      
*                                  AND FIX LENGTHS                              
         L     R1,TRADDR                                                        
         CLI   8(R1),C'B'          MUST BE BUYING                               
         BNE   EDTCM80                                                          
EDTCM10B LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'66'        FIRST D0 NORMAL COMMENTS                     
EDTCM10N BAS   R9,CNEXTEL                                                       
         BNE   EDTCM20                                                          
         CLI   1(R5),49            SEE IF TOO LONG                              
         BNH   EDTCM10N            NO                                           
*****    LA    R3,46               MAXIMUM ELEMENT LENGTH                       
*                                  (TO CONFORM TO X'67' AFTER SPLIT)            
         BAS   RE,EDTCSPL          DELETE, SPLIT AND ADD "NEW" ELEMS            
         B     EDTCM10B            TEST NEXT                                    
*                                                                               
EDTCM20  LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'67'        D0 I/0 COMMENTS                              
EDTCM20N BAS   R9,CNEXTEL                                                       
         BNE   EDTCM30                                                          
         CLI   1(R5),46            SEE IF TOO LONG                              
         BNH   EDTCM20N            NO                                           
*****    LA    R3,46               MAXIMUM ELEMENT LENGTH                       
*                                                                               
         BAS   RE,EDTCSPL          DELETE, SPLIT AND ADD "NEW" ELEMS            
         B     EDTCM20             TEST NEXT                                    
*                                                                               
EDTCM30  LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'68'        D0 PI COMMENTS                               
EDTCM30N BAS   R9,CNEXTEL                                                       
         BNE   EDTCM80             DONE WITH SPLIT                              
         CLI   1(R5),46            SEE IF TOO LONG                              
         BNH   EDTCM30N            NO                                           
*****    LA    R3,46               MAXIMUM ELEMENT LENGTH                       
*                                                                               
         BAS   RE,EDTCSPL          DELETE, SPLIT AND ADD "NEW" ELEMS            
         B     EDTCM30             TEST NEXT                                    
*                                                                               
* USE CHOPPER TO SPLIT "LONG" ELEMENT (DELETE LONG ELEM FIRST)                  
* R5 POINTS TO ELEMENT TO BE DELETED AND SPLIT IN NEWREC                        
* MAXIMUM ELEMENT LENGTH IS 46 FOR ALL SPLIT ELEMENTS HERE                      
*                                                                               
EDTCSPL  NTR1                                                                   
         L     R7,ACOMWRK                                                       
         XC    0(200,R7),0(R7)     CLEAR 200 BYTES                              
         ZIC   R6,1(R5)            ELEMENT LENGTH                               
         BCTR  R6,0                                                             
         EX    R6,MVCOM4           ENTIRE ELEMENT TO 0(R7)                      
         LA    R6,1(R6)            BUMP BACK TO ACTUAL ELEMENT LENGTH           
*                                                                               
         GOTO1 VRECUP,DMCB,(1,NEWREC),(R5),0   DELETE ELEMENT                   
*                                                                               
         GOTO1 =V(CHOPPER),DMCB,((R6),(R7)),(46,100(R7)),(50,2),       X        
               RR=CRELO                                                         
*                                                                               
         LA    R7,100(R7)          POINT R7 TO NEW ELEMENTS                     
         LA    R0,2                LUP COUNTER                                  
EDTCSP2  LA    R4,49(R7)           END OF "FIRST" ELEM AREA                     
EDTCSP4  CLI   0(R4),C' '          NOT "BLANK" ?                                
         BH    EDTCSP6             YES - DONE WITH EXAM                         
         BCTR  R4,0                BACKSPACE 1                                  
         B     EDTCSP4                                                          
EDTCSP6  DS    0H                                                               
         LA    R4,1(R4)            ADD BACK LAST SIGNIFICANT CHARACTER          
         SR    R4,R7               R7 POINTS TO BEGINNING OF NEW ELEM           
         STC   R4,1(R7)            NEW ELEMENT LENGTH                           
*                                                                               
         GOTO1 VRECUP,DMCB,(1,NEWREC),0(R7),(R5)   ADD ELEMENT                  
*                                                                               
         MVC   2(48,R7),50(R7)     MOVE 2ND ELEM TO AREA TO BE ADDED            
         AR    R5,R4               POINT R5 AFTER ELEM ADDED TO NEWREC          
         BCT   R0,EDTCSP2          DO 2ND ELEMENT                               
         XIT1                                                                   
*                                                                               
EDTCM80  DS    0H                                                               
         CLI   SVESPROF+15,C' '    NO EST REP                                   
         BE    NEXTD5                                                           
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'80'        CHK NEWREC FOR SREP ELEM                     
         BAS   R9,CNEXTEL                                                       
         BE    NEXTD5              FOUND -  SO I'M DONE                         
         L     R1,TRADDR                                                        
         CLI   8(R1),C'B'          SEE IF BUYING                                
         BNE   NEXTD5                                                           
*                                                                               
         XC    WORK(10),WORK       NOT FOUND -  MUST ADD ELEM                   
         MVC   WORK(2),=X'800A'                                                 
         MVC   WORK+2(4),SVESPROF+15                                            
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK,(R5)                                 
*                                                                               
NEXTD5   BRAS  RE,PRCC2FAC         PROCESS COS2 FACTOR                          
*                                                                               
NEXTDX   XMOD1 1                                                                
*                                                                               
* EDIT PLANNED COST                                                             
*                                                                               
EDTPLCOS DS    0H                                                               
         MVI   PBDPLCOS,X'FF'      SET PC=NONE                                  
         CLC   =C'NONE',FLD2                                                    
         BE    EDTPLC4                                                          
*                                                                               
         LA    R3,INVERR                                                        
         ZIC   R0,FLD2L            LENGTH OF DATA                               
         LTR   R0,R0                                                            
         BNP   ERROR                                                            
         GOTO1 VCASHVAL,DMCB,(2,FLD2),(R0)                                      
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         ICM   RF,15,4(R1)                                                      
         BZ    ERROR                                                            
         STCM  RF,15,PBDPLCOS                                                   
*                                                                               
EDTPLC4  DS    0H                                                               
         MVC   TRCODE,=C'RZ'       SET FOR EXPANDED RECALL                      
         B     NEXTD               NEXT COMMENT LINE                            
*                                                                               
* EDIT CASH DISCOUNT OVERRIDE. FORMAT IS CD=1.5                                 
*                                                                               
EDTCCD   DS    0H                                                               
         CP    PBDCD,=P'0'                                                      
         BNE   FLDINV                                                           
         LHI   R3,NOFRZCER         NO FINANCIAL CHANGES TO FRZ CLIENT           
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         BNE   ERROR                                                            
         LHI   R4,10000                                                         
         LA    R5,PBDCD                                                         
         LA    R6,(L'PBDCD-1)*16   HIGH NIBBLE                                  
*                                                                               
         MVI   BYTE3,1             CASH DISCOUNT HAS ONE DECIMAL                
         B     EDTPCT                                                           
*                                                                               
EDTCAC   DS    0H                                                               
         LHI   R3,NOFRZCER         NO FINANCIAL CHANGES TO FRZ CLIENT           
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         BNE   ERROR                                                            
         CLI   PBDCOSIN,C'S'                                                    
         BNE   *+12                                                             
         LA    R3,ACERR                                                         
         B     ERROR                                                            
*                                                                               
         CP    PBDACP,=P'0'        TEST ALREADY INUT                            
         BNE   FLDINV                                                           
         CLC   FLD2(3),=C'100'     TEST INPUT OF 100 PERCENT                    
         BNE   EDTCAC4                                                          
         ZAP   PBDACP,=P'-1'       SET 100 PCT TO -1 PCT                        
         B     EDTPCT4                                                          
EDTCAC4  DS    0H                                                               
         LA    R4,100                                                           
         LA    R5,PBDACP                                                        
         LA    R6,(L'PBDACP-1)*16  HIGH NIBBLE                                  
         MVI   BYTE3,3             AGY COMM HAS THREE DECIMALS                  
*                                                                               
EDTPCT   DS    0H                                                               
         LA    R3,INVERR                                                        
         ZIC   R0,FLD2L            LENGTH OF FIELD2                             
*                                                                               
         GOTOR VCASHVAL,DMCB,(BYTE3,FLD2),(X'40',(R0))                          
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
*                                                                               
         GOTOR VCASHVAL,DMCB,(5,FLD2),(R0)                                      
*                                                                               
         L     R1,4(R1)                                                         
         LTR   R1,R1                                                            
         BM    ERROR                                                            
         SR    R0,R0                                                            
         DR    R0,R4                                                            
         CVD   R1,DUB                                                           
         CP    DUB,=P'0'                                                        
         BNE   *+10                                                             
         ZAP   DUB,=P'1'                                                        
         EX    R6,*+8                                                           
         B     *+10                                                             
         ZAP   0(0,R5),DUB                                                      
         EX    R6,*+8                                                           
         B     *+10                                                             
         CP    0(0,R5),DUB          TEST INPUT NUMBER TOO LARGE                 
         BNE   ERROR                                                            
*                                                                               
EDTPCT4  DS    0H                                                               
         MVC   TRCODE,=C'RZ'                                                    
         B     NEXTD         NEXT COMMENT LINE                                  
*                                                                               
FLDINV   LA    R3,INVERR                                                        
         B     ERROR                                                            
         SPACE 3                                                                
EDTCLE   DS    0H                                                               
********                                                                        
********  NOTE ENTERING CLE=  WILL RETURN ERROR                                 
********  AS OF 5/14/99                                                         
********  REALLY ILLOGICAL ANYWAY                                               
********                                                                        
**NO-OP  CLI   BUYMD,C'N'                                                       
****     BNE   EDTCLE2                                                          
***      CLC   PBDSPACE(2),=C'* '                                               
***      BNH   EDTCLE2                                                          
***      LA    R4,FLD2             POINT TO INPUT                               
***      ZIC   R5,FLD2L                                                         
***      LTR   R5,R5                                                            
***      BNP   ERROR                                                            
***      ST    R9,FULL             SAVE R9                                      
***      BAS   R9,CDTLNS1                                                       
***      L     R9,FULL                                                          
***      CP    PBDUNITS,=P'0'                                                   
***      BNE   *+10                                                             
***      ZAP   PBDUNITS,=P'-1'     SET 0 TO -1 TO PREVENT LOOKUP                
***      B     NEXTD                                                            
***                                                                             
EDTCLE2  DS    0H                                                               
         LA    R3,CLEERR                                                        
         B     ERROR                                                            
*                                                                               
EDTIMPS  DS    0H                IMPRESSIONS DON'T APPLY TO WSJ?                
         B     FLDINV            SEND ERROR FOR NOW                             
*                                                                               
         EJECT                                                                  
*                           REFERENCE NUMBER                                    
*                           GOES INTO PBREFEL                                   
EDTREF   DS    0H                                                               
*                                                                               
EDTREF2  LA    R4,FLD2             POINT TO INPUT                               
         LA    R3,INVERR                                                        
         XC    WORK(10),WORK                                                    
         ZIC   R5,FLD2L                                                         
         CHI   R5,0                                                             
         BNE   EDTREF4                                                          
*                                                                               
         MVI   WORK,X'FF'          SPECIAL DELETE CODE                          
         L     RE,TRADDR                                                        
         CLI   8(RE),C'B'          ADDING EMPTY ELEM ON NEW BUY?                
         BE    ERROR                                                            
         B     EDTREF10                                                         
*                                                                               
EDTREF4  CLC   AGYALPHA,=C'BS'                                                  
         BNE   EDTREF6                                                          
         CHI   R5,0                MAX IS CHARACTERS FOR BACKER                 
         BNE   EDTREF5                                                          
         L     RE,TRADDR                                                        
         CLI   8(RE),C'B'          REF= (0 LEN) ON NEW BUY IS INVALID           
         BE    ERROR                                                            
EDTREF5  CHI   R5,6                MAX IS CHARACTERS FOR BACKER                 
         BH    ERROR                                                            
*                                                                               
EDTREF6  CHI   R5,10               SJR + OTHERS - MAX IS 10 CHARS               
         BH    ERROR                                                            
*                                                                               
         MVC   WORK(10),0(R4)                                                   
*                                                                               
EDTREF10 DS    0H                  NOW STORE IN PBREFEL                         
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'83'                                                     
         ST    R9,FULL                                                          
         BAS   R9,CNEXTEL                                                       
         BNE   EDTREF20             MUST ADD ONE                                
*                                                                               
         LA    R3,INVERR                                                        
         B     ERROR               CAN'T ALREADY HAVE REF ELEM                  
*                                                                               
*                                                                               
EDTREF20 L     R9,FULL             MUST RESTORE R9                              
         XC    WORK+15(15),WORK+15                                              
         MVC   WORK+15(2),=X'830F'    CODE AND LENGHT                           
         MVC   WORK+17(10),WORK       REF NUMBER OR X'FF'                       
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+15,(R5)                              
*                                                                               
EDTREF30 B     NEXTD                                                            
         EJECT                                                                  
EDTSFH   DS    0H                EDIT SPECIAL FINANCIAL HANDLING                
*                                                                               
         LA    R3,INVERR                                                        
*                                                                               
         LA    R4,8+4(R2)         POINT TO INPUT                                
         ZIC   R5,5(R2)                                                         
         CHI   R5,5                                                             
         BL    ERROR                                                            
         CLC   0(4,R4),=C'HOLD'                                                 
         BE    EDTSFHH                                                          
         CLC   0(4,R4),=C'HELD'                                                 
         BE    EDTSFHH                                                          
         CLI   0(R4),C'H'                                                       
         BE    EDTSFHH                                                          
         CLC   0(3,R4),=C'REL'                                                  
         BE    EDTSFHR                                                          
         CLI   0(R4),C'R'                                                       
         BE    EDTSFHR                                                          
         B     ERROR                                                            
*                                                                               
EDTSFHH  DS    0H                                                               
         OI    PBDSTAT,X'0C'      SET ON SFH AND HOLD BITS                      
         L     R1,TRADDR         SEE IF BUYING                                  
         CLI   8(R1),C'B'                                                       
         BE    EDTSFHX                                                          
*                                ON CHANGES LOOK FOR I/O ELEMEMT                
*                                WITH A DATE                                    
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'70'                                                     
         ST    R9,FULL                                                          
EDTSFHH1 BAS   R9,CNEXTEL                                                       
         BNE   EDTSFHHX                                                         
         OC    2(3,R5),2(R5)      CHECK FOR DATE                                
         BZ    EDTSFHH1                                                         
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(34),=C'CAN''T HOLD- BUY HAS BEEN ON AN I/O'               
         MVI   ERRAREA,X'FF'                                                    
         MVC   TRCODE,=C'RI'      SET FOR I/O RECALL                            
         B     EXIT                                                             
*                                                                               
EDTSFHHX L     R9,FULL            MUST RESTORE R9                               
         B     EDTSFHX                                                          
*                                                                               
EDTSFHR  DS    0H                                                               
         OI    PBDSTAT,X'04'      SFH BIT                                       
         NI    PBDSTAT,X'F7'     SET OFF X'08' HOLD BIT                         
*                                 IF NEW BUY COULD ALREADY BE ON                
EDTSFHX  B     NEXTD                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SHIP DATE      - GOES INTO PBSHPDEL (X'86')                                   
* EXTENSION DATE - GOES INTO PEXDATEL (X'96')                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTCSHDT DS    0H                  BYTE2 IS USED TO INDICATE ELEM CODE          
*                                                                               
EDTSHP2  LA    R4,FLD2             POINT TO INPUT                               
         LA    R3,INVERR                                                        
         XC    WORK(20),WORK                                                    
         ZIC   R5,FLD2L                                                         
         LTR   R5,R5                                                            
         BP    EDTSHP4                                                          
*                                                                               
EDTSHP3  L     R1,TRADDR                                                        
         CLI   8(R1),C'B'          SEE IF BUYING                                
         BE    ERROR               ERROR                                        
*                                                                               
EDTSHP3H MVC   WORK(3),=3X'FF'     SPECIAL DELETE CODE                          
         B     EDTSHP10                                                         
*                                                                               
EDTSHP4  DS    0H                                                               
         LA    R3,INVERR           RESET DEFAULT ERR MSG, JUST IN CASE          
         CHI   R5,8                MAX IS MMMDD/YY (8 CHARS)                    
         BH    ERROR                                                            
*                                                                               
         CLI   BYTE2,X'96'         EXTENSION DATE?                              
         BNE   *+14                                                             
         CLC   =C'NONE',0(R4)                                                   
         BE    EDTSHP3H            DELETE EXTENSION DATE ELEM                   
*                                                                               
         MVC   WORK(8),0(R4)                                                    
         GOTO1 VDATVAL,DMCB,(0,WORK),WORK+10                                    
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
*                                                                               
* NOTE - DATVAL RETURNS 00 IN YEAR IF NOT ENTERED                               
*                                                                               
         GOTO1 VDATCON,(R1),(0,WORK+10),(3,WORK)                                
*                                                                               
         CLI   BYTE2,X'96'         EXTENSION DATE?                              
         BNE   EDTSHP4H                                                         
         CLC   WORK(3),PBUYKDAT                                                 
         BNH   EDTSHP4H                                                         
         LA    R3,EXDERR1          EXDATE MUST BE ON/BEFORE INS DATE            
         B     ERROR                                                            
*                                                                               
EDTSHP4H XC    WORK+3(2),WORK+3                                                 
         CLC   WORK+10(2),=C'00'   TEST HAVE YEAR                               
         BNE   EDTSHP4X                                                         
         MVC   WORK+0(1),PBUYKDAT  SET YEAR                                     
         CLC   WORK(3),PBUYKDAT                                                 
         BH    EDTSHP4X                                                         
         ZIC   RF,PBUYKDAT                                                      
         LA    RF,1(RF)            DATE IN NEXT YEAR                            
         STC   RF,WORK                                                          
*                                                                               
EDTSHP4X DS    0H                                                               
*                                                                               
EDTSHP10 DS    0H                  NOW STORE IN ELEM                            
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'86'                                                     
         LR    R1,R9               SAVE R9                                      
         CLI   BYTE2,X'96'         EXTENSION DATE?                              
         BNE   *+8                                                              
         MVI   ELCODE,X'96'                                                     
         BAS   R9,CNEXTEL                                                       
         BNE   EDTSHP20            MUST ADD ONE                                 
*                                                                               
         LA    R3,INVERR                                                        
         B     ERROR               CAN'T ALREADY HAVE DATE ELEM                 
*                                                                               
EDTSHP20 LR    R9,R1               MUST RESTORE R9                              
         XC    WORK+15(15),WORK+15                                              
         MVC   WORK+15(2),=X'8607' CODE AND LENGHT                              
         CLI   BYTE2,X'96'         EXTENSION DATE?                              
         BNE   *+10                                                             
         MVC   WORK+15(2),=X'9605' CODE AND LENGHT FOR EXTENSION DATE           
         MVC   WORK+17(5),WORK                                                  
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+15,(R5)                              
*                                                                               
EDTSHP30 B     NEXTD                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* EDIT SPECIAL REP CODE - GOES INTO PBSREPEL                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTSREP  DS    0H                                                               
         L     R1,TRADDR                                                        
         CLI   8(R1),C'B'          SEE IF BUYING                                
         BE    EDTSR2                                                           
*                                                                               
* SHOULD BE SURE PAYMENTS NET TO ZERO                                           
*                                                                               
         GOTO1 VGETINS,DMCB,REC,PVALUES,REC+7                                   
         OC    PGROSS,PGROSS                                                    
         BZ    EDTSR2                                                           
         LA    R3,SRPDERR                                                       
         B     ERROR                                                            
*                                                                               
EDTSR2   LA    R4,FLD2             POINT TO INPUT                               
         LA    R3,INVERR                                                        
         ZIC   R5,FLD2L                                                         
         LTR   R5,R5                                                            
         BNP   ERROR                                                            
         CHI   R5,4                                                             
         BH    ERROR                                                            
         LR    R1,R5               SAVE LENGHT                                  
EDTSR5   CLI   0(R4),C'0'          CHK FOR NUMERICS                             
         BL    ERROR                                                            
         CLI   0(R4),C'9'                                                       
         BH    ERROR                                                            
         LA    R4,1(R4)                                                         
         BCT   R1,EDTSR5                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD2(0)                                                      
         OI    DUB,X'0F'                                                        
         UNPK  WORK(4),DUB                                                      
         CLC   WORK(4),=C'0000'                                                 
         BNE   EDTSR8                                                           
         MVI   WORK,C'X'           SPECIAL DELETE CODE                          
         B     EDTSR10                                                          
*                                                                               
EDTSR8   XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD      MEDIA                                        
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),WORK                                                    
         BAS   RE,HIGH                                                          
         CLC   KEY(9),KEYSAVE                                                   
         BE    EDTSR10             FOUND - OK                                   
*                                                                               
         LA    R3,NFNDERR                                                       
         B     ERROR                                                            
*                                                                               
EDTSR10  DS    0H                  NOW STORE IN PBSREPEL                        
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'80'                                                     
         ST    R9,FULL                                                          
         BAS   R9,CNEXTEL                                                       
         BNE   EDTSR20             MUST ADD ONE                                 
*                                                                               
         LA    R3,INVERR                                                        
         B     ERROR               CAN'T ALREADY HAVE REP ELEM                  
*                                                                               
EDTSR20  L     R9,FULL             MUST RESTORE R9                              
         XC    WORK+10(10),WORK+10                                              
         MVC   WORK+10(2),=X'800A' CODE AND LENGHT                              
         MVC   WORK+12(4),WORK     REP CODE OR C'X'                             
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+10,(R5)                              
*                                                                               
EDTSR30  B     NEXTD                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTEXD   DS    0H                                                               
         ST    R9,FULL                                                          
         LA    R3,EXDERR2          EXDAYS/EXDATE ERROR                          
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'96'        IF DOING EXDAYS, CANNOT HAVE EXDATE          
         BAS   R9,CNEXTEL                                                       
         BE    ERROR                                                            
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'96'        IF DOING EXDAYS, CANNOT HAVE EXDATE          
         BAS   R9,CNEXTEL                                                       
         BE    ERROR                                                            
         L     R9,FULL                                                          
*                                                                               
         LA    R4,FLD2             POINT TO INPUT                               
         LA    R3,INVERR                                                        
         ZIC   R5,FLD2L                                                         
         CHI   R5,3                                                             
         BH    ERROR               MAX IS 999                                   
*                                                                               
         XC    WORK(10),WORK                                                    
         CHI   R5,0                                                             
         BNE   EDTEXD5                                                          
*                                                                               
         MVI   WORK,C'X'           SPECIAL DELETE CODE                          
         L     RE,TRADDR                                                        
         CLI   8(RE),C'B'          ADDING EMPTY ELEM ON NEW BUY?                
         BE    ERROR                                                            
         B     EDTEXD10                                                         
*                                                                               
EDTEXD5  TM    FLD2VAL,X'80'       INPUT IS NUMBERIC?                           
         BNO   ERROR                                                            
         ICM   R5,15,FLD2B         CONVERTING BINARY VALUE TO PACK              
         CHI   R5,0                                                             
         BNE   EDTEXD7                                                          
         L     RE,TRADDR                                                        
         CLI   8(RE),C'B'          EXDAYS=0 ON NEW BUY IS INVALID               
         BE    ERROR                                                            
EDTEXD7  CVD   R5,DUB                                                           
         MVC   WORK(2),DUB+6       EXTENSION DAYS IS STORED AS PL2              
*                                                                               
EDTEXD10 DS    0H                                                               
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'89'                                                     
         ST    R9,FULL                                                          
         BAS   R9,CNEXTEL                                                       
         BNE   EDTEXD20            MUST ADD ONE                                 
*                                                                               
         LA    R3,INVERR                                                        
         B     ERROR               CAN'T ALREADY HAVE ELEM                      
*                                                                               
EDTEXD20 L     R9,FULL             MUST RESTORE R9                              
         XC    WORK+15(10),WORK+10                                              
         MVI   WORK+15,X'89'       ELEM CODE                                    
         MVI   WORK+16,X'04'       ELEM LENGHT                                  
         MVC   WORK+17(10),WORK    EXTENSION DAYS OR X'FF'                      
*                                                                               
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+15,(R5)                              
*                                                                               
EDTEXDX  B     NEXTD                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTEXDT  DS    0H                                                               
         ST    R9,FULL                                                          
         LA    R3,EXDERR2          EXDAYS/EXDATE ERROR                          
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'89'        IF DOING EXDATE, CANNOT HAVE EXDAYS          
         BAS   R9,CNEXTEL                                                       
         BE    ERROR               NO GOOD, EXDAYS ELEM EXIST                   
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'89'        IF DOING EXDATE, CANNOT HAVE EXDAYS          
         BAS   R9,CNEXTEL                                                       
         BE    ERROR               NO GOOD, EXDAYS ELEM EXIST                   
         L     R9,FULL                                                          
         MVI   BYTE2,X'96'         FLAG FOR DOING EXTENSION DATE                
         LA    R4,8+7(R2)          POINT TO INPUT                               
         ZIC   R5,5(R2)                                                         
         AHI   R5,-7               ADJUST INPUT LENGTH (EXDATE=)                
         BP    EDTSHP4             SHARE CODE WITH SHIPPING DATE                
         B     EDTSHP3             NO DATE IS ENTERED                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
EDTCPDDT LA    R4,PBDPDATE         PAYABLE DATE FORMAT IS MMMDD/YY              
         OC    PBDPDATE,PBDPDATE                                                
         BNZ   FLDINV                                                           
         LA    R5,FLD2                                                          
         MVI   BYTE,0                                                           
         B     EDTCDT                                                           
*                                                                               
EDTCBLDT LA    R4,PBDBDATE         BILLABLE DATE FORMAT IS MMMDD/YY             
         OC    PBDBDATE,PBDBDATE                                                
         BNZ   FLDINV                                                           
         LA    R5,FLD2                                                          
         MVI   BYTE,0                                                           
         B     EDTCDT                                                           
*                                                                               
EDTCIODT LA    R4,PBDIODAT                                                      
         OC    PBDIODAT,PBDIODAT                                                
         BNZ   FLDINV                                                           
         MVC   PBDIODAT,=3X'FF'    IOD = 'NONE'                                 
         CLC   FLD2(4),=C'NONE'                                                 
         BE    NEXTD            NEXT COMMENT LINE                               
         LA    R5,FLD2                                                          
         MVI   BYTE,0                                                           
*                                                                               
EDTCDT   DS    0H                                                               
         GOTO1 VDATVAL,DMCB,(BYTE,0(R5)),WORK                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
*                                                                               
* PUT DATE IN RECORD                                                            
*                                                                               
         GOTO1 VDATCON,(R1),(0,WORK),(3,(R4))                                   
*                                                                               
         CLC   WORK(2),=C'00'      TEST HAVE YEAR                               
         BNE   EDTCDT4                                                          
         MVC   0(1,R4),PBUYKDAT    SET YEAR                                     
         CLC   0(3,R4),PBUYKDAT                                                 
         BNL   EDTCDT2                                                          
         IC    RF,PBUYKDAT                                                      
         LA    RF,1(RF)            DATE IN NEXT YEAR                            
         STC   RF,0(R4)                                                         
*                                                                               
EDTCDT2  GOTO1 VDATCON,(R1),(3,0(R4)),(5,WORK+10)                               
         GOTO1 VDATVAL,(R1),(0,WORK+10),WORK+20                                 
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR               CATCH ERR ESCAPED FROM PREVIOUS DVAL         
*                                                                               
EDTCDT4  DS    0H                                                               
         MVC   TRCODE,=C'RI'                                                    
         CLC   =C'ID=',8(R2)                                                    
         BE    NEXTD            NEXT COMMENT LINE                               
         MVC   TRCODE,=C'RZ'                                                    
         B     NEXTD         NEXT COMMENT LINE                                  
         SPACE 2                                                                
EDTCD2   DS    0H                  2ND INS DATE                                 
         MVI   PBDEMIND,0          M/E IND                                      
         MVI   PBDIDAT2,X'FF'      SET FOR 'NONE'                               
         CLC   FLD2(4),=C'NONE'                                                 
         BE    NEXTD                                                            
         MVC   WORK(6),FLD2                                                     
         CLI   PBDFREQ,C'M'        FOR 'MONTHLIES' ONLY MMM                     
         BNE   EDTCD2B                                                          
         CLI   WORK+3,C' '                                                      
         BH    ERROR                                                            
         MVC   WORK+3(2),=C'01'                                                 
         B     EDTCD2D                                                          
*                                                                               
EDTCD2B  DS    0H                                                               
         LA    RF,WORK+5                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         CLI   0(RF),C'0'                                                       
         BNL   EDTCD2D                                                          
         MVC   PBDEMIND,0(RF)                                                   
         CLI   PBDEMIND,C'E'                                                    
         BE    EDTCD2D                                                          
         CLI   PBDEMIND,C'M'                                                    
         BNE   ERROR                                                            
*                                                                               
EDTCD2D  DS    0H                                                               
         LA    R4,PBDIDAT2                                                      
         LA    R5,WORK                                                          
         MVI   BYTE,1                                                           
         B     EDTCDT                                                           
*                                                                               
***                                                                             
***      THIS ROUTINE NO-OPED ON 5/14/99                                        
***      IT WAS CALLED BY EDTCLE                                                
***      NOT NEEDED SINCE CLE= IS NO LONGER ACCEPTED                            
***      IN THE OPTIONS LINE                                                    
***                                                                             
***      NOTE ALL THE TAGS BELOW USED TO BEGIN WITH CDT                         
***      THIS IS NOW ***                                                        
***                                                                             
***LNS1  STM   R4,R5,DUB           SAVE PTR/LEN                                 
***LNS2  CLI   0(R4),C'0'          VERIFY NUMERIC                               
***      BL    CDTLNS3                                                          
***      CLI   0(R4),C'9'                                                       
***      BH    ERROR                                                            
***      LA    R4,1(R4)                                                         
***      BCT   R5,CDTLNS2                                                       
***                                                                             
***LNS3  DS    0H                                                               
***      L     RE,DUB              START OF NUMBER                              
***      SR    R4,RE               R4 = LENGTH                                  
***      BNP   CDTL9                                                            
***      BCTR  R4,R0                                                            
***      EX    R4,CPACKLN                                                       
***      CP    WORK(5),=P'99999'   MAX LINES OR INCHES                          
***      BH    ERROR               THAT CAN BE CARRIED IN PBDUNITS              
***      ZAP   PBDUNITS,WORK(5)                                                 
***                                                                             
***      MVI   PBDUIND,C'L'                                                     
***      LTR   R5,R5                                                            
***      BZ    CDTLNS8             NO MORE INPUT                                
***      LA    R4,1(RE,R4)         R4 = BYTE PAST NUMBER                        
***      CLI   0(R4),C'L'                                                       
***      BE    CDTLNS4                                                          
***      CLI   0(R4),C'/'                                                       
***      BE    CDTLNS5                                                          
***      MVI   PBDUIND,C'I'                                                     
***      CLI   0(R4),C'I'                                                       
***      BE    CDTLNS4                                                          
***      CLI   0(R4),C'X'          NEW SAU NNXNN.NN                             
***                                COLUMNS X INCHES (2 DECIMALS)                
***      BNE   CDTLNS3C            NO                                           
***      CP    PBDUNITS,=P'13'     MAX COLS =13                                 
***      BH    CDTL9               TREAT AS SPACE                               
***      CP    PBDUNITS,=P'0'                                                   
***      BNH   CDTL9                                                            
***      LA    R4,1(R4)                                                         
***      BCT   R5,*+8              NO MORE INPUT                                
***      B     CDTL9               NO MUST BE SPACE                             
***      CLC   0(2,R4),=C'FD'                                                   
***      BNE   CDTLNS3B                                                         
***      CLI   2(R4),C' '                                                       
***      BNL   CDTL9               INPUT AFTER FD                               
***      L     R5,APUBIO                                                        
***      CLI   0(R5),0             SEE IF PUB THERE                             
***      BNE   CDTLNS32                                                         
***      XC    KEY,KEY                                                          
***      MVC   KEY+27(4),SVPUBDA                                                
***                                MUST REREAD PUB                              
***      GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PUBFILE',                  
***            KEY+27,APUBIO,(TERMNAL,DMWORK)                                   
************                                                                    
************   SPECIAL NOTE - IF RESTORING THIS CODE                            
************   BE SURE TO ADD AN "X" TO COLUMN 80 OF THE                        
************  "GOTO1 VDATAMGER" CARD ABOVE                                      
************                                                                    
***      MVC   BYTE,DMCB+8                                                      
***      NC    BYTE,DMOUTBTS                                                    
***      BZ    *+6                                                              
***      DC    H'0'                MUST FIND PUB                                
***                                                                             
***LNS32 LA    R5,33(R5)                                                        
***      MVI   ELCODE,X'20'                                                     
***      ST    R9,DUB              SAVE R9                                      
***      BAS   R9,NEXTEL                                                        
***      BE    CDTLNS3A                                                         
***      L     R9,DUB              RESTORE R9                                   
***      B     CDTL9               CAN'T FIND PROD ELEM                         
***                                                                             
***      USING PUBGENEL,R5                                                      
***LNS3A L     R9,DUB              RESTORE R9                                   
***      OC    PUBFD,PUBFD         SEE IF I HAVE FD                             
***      BZ    CDTL9               NO - TREAT AS SPACE                          
***      ZAP   PBDCLMS,PBDUNITS                                                 
***      ZAP   DUB,PBDUNITS                                                     
***      MP    DUB,PUBFD                                                        
***      CP    DUB,=P'99999'       MAX IS 999.99 COL INCHES                     
***      BH    ERROR                                                            
***      ZAP   PBDUNITS,DUB                                                     
***      NI    PBDUIND,X'BF'       MAKE I LOWER CASE                            
***      B     CDTL9B                                                           
***                                                                             
***      DROP  R5                                                               
***                                                                             
***LNS3B GOTO1 VCASHVAL,DMCB,(2,0(R4)),(R5)                                     
***      CLI   DMCB,X'FF'                                                       
***      BE    CDTL9                                                            
***      OC    DMCB+4(4),DMCB+4                                                 
***      BZ    CDTL9                                                            
***      ZAP   PBDCLMS,PBDUNITS                                                 
***      L     R0,DMCB+4                                                        
***      CVD   R0,DUB                                                           
***      CP    DUB,=P'0'           CAN'T BE NEGATIVE                            
***      BNH   CDTL9                                                            
***      MP    DUB,PBDCLMS                                                      
***      CP    DUB,=P'99999'       MAX COLUMN INCHES                            
***      BH    CDTL9                                                            
***      NI    PBDUIND,X'BF'       MAKE LOWER CASE I                            
***      ZAP   PBDUNITS,DUB                                                     
***      B     CDTL9B              SAVES SPACE                                  
***                                                                             
***LNS3C DS    0H                  CHK FOR DECIMAL POINT                        
***      CLI   0(R4),C'.'                                                       
***      BNE   CDTL9               TREAT AS SPACE                               
***      BCT   R5,*+8                                                           
***      B     CDTL9               NO MORE INPUT                                
***      LA    R4,1(R4)                                                         
***      CLI   0(R4),C'I'                                                       
***      BNE   CDTLNS3E                                                         
***      BCT   R5,*+8                                                           
***      B     CDTLNS8             TREAT NN.I AS NN (NO DECIMALS)               
***      B     CDTL9               INPUT AFTER I TREAT AS SPACE                 
***                                                                             
***LNS3E ST    R4,DUB                                                           
***LNS3F CLI   0(R4),C'I'                                                       
***      BE    CDTLNS3G                                                         
***      CLI   0(R4),C'0'                                                       
***      BL    CDTL9                                                            
***      CLI   0(R4),C'9'                                                       
***      BH    CDTL9                                                            
***      LA    R4,1(R4)                                                         
***      BCT   R5,CDTLNS3F                                                      
***      B     CDTL9               IF DOESN'T END WITH I ASSUME SPACE           
***                                                                             
***LNS3G BCTR  R5,0                MUST DECREMENT R5                            
***      LTR   R5,R5                                                            
***      BNZ   CDTL9               MORE INPUT - TREAT AS SPACE                  
***      L     RE,DUB              START OF NUMBER                              
***      SR    R4,RE               R4 = LENGTH                                  
***      BCTR  R4,R0               ADJUST FOR I                                 
***      EX    R4,CPACKLN                                                       
***      ZAP   DUB,WORK(5)                                                      
***      CP    DUB,=P'0'           SEE IF NN.00  INPUT                          
***      BE    CDTLNS8             TREAT AS NNI                                 
***      XC    WORK(13),WORK       BUILD LINE FOR CASHVAL                       
***      ZIC   R5,FLD2L                                                         
***      LA    R4,FLD2                                                          
***                                                                             
***LNS3I BCTR  R5,0                                                             
***      BCTR  R5,0                SO I WON'T MOVE THE I                        
***      EX    R5,*+8                                                           
***      B     *+10                                                             
***      MVC   WORK(0),0(R4)                                                    
***      LA    R5,1(R5)            RESTORE FROM THE EX                          
***      GOTO1 VCASHVAL,DMCB,(2,WORK),(R5)                                      
***      CLI   DMCB,X'FF'                                                       
***      BE    CDTL9               IF ERROR TREAT AS SPACE                      
***      L     R0,DMCB+4                                                        
***      CVD   R0,DUB                                                           
***      CP    DUB,=P'99999'                                                    
***      BH    ERROR               CAN'T FIT IN PBDUNITS                        
***      ZAP   PBDUNITS,DUB                                                     
***      NI    PBDUIND,X'BF'       MAKE I LOWER CASE 'I'                        
***                                INCHES TO 2 DECIMALS                         
***      B     CDTL9B              SAVES SPACE                                  
***                                                                             
***LNS4  DS    0H                                                               
***      BCT   R5,*+8              NO MORE INPUT AFTER L OR I                   
***      B     CDTLNS8                                                          
***      CLI   1(R4),C'/'          '/'  MUST BE NEXT                            
***      BNE   CDTL9                                                            
***      LA    R4,1(R4)                                                         
***                                                                             
***LNS5  DS    0H                                                               
***      LA    R4,1(R4)                                                         
***                                                                             
***LNS6  DS    0H                                                               
***      BCT   R5,*+8              NO MORE INPUT                                
***      B     CDTL9                                                            
***      ST    R4,DUB              START OF NUMBER                              
***LNS6A CLI   0(R4),C'0'                                                       
***      BL    CDTL9                                                            
***      CLI   0(R4),C'9'                                                       
***      BH    CDTL9                                                            
***      LA    R4,1(R4)                                                         
***      BCT   R5,CDTLNS6A                                                      
***      L     RE,DUB              START OF NUMBER                              
***      SR    R4,RE               R4 = LENGTH                                  
***      BCTR  R4,R0                                                            
***      EX    R4,CPACKLN                                                       
***      CP    WORK(5),=P'999'     MAX COLUMNS IS 999                           
***      BH    ERROR               CAN'T FIT IN PDBCLMS                         
***      LA    R3,DVSERR           DIVISIBILITY ERROR                           
***      ZAP   PBDCLMS,WORK(5)                                                  
***      BZ    ERROR                                                            
***      ZAP   DUB,PBDUNITS                                                     
***      BZ    ERROR                                                            
***      DP    DUB,PBDCLMS                                                      
***      CP    DUB+8-L'PBDCLMS(L'PBDCLMS),=P'0'                                 
***      BNE   ERROR                                                            
***                                CHECK VS JOB RECORD                          
***LNS8  DS    0H                                                               
***      BR    R9                                                               
***      SPACE 3                                                                
***L9    DS    0H                                                               
***      B     ERROR               NO SPACE FOR CLE                             
***                                TREAT AS SPACE DESC                          
***L9B   DS    0H                                                               
***      MVC   PBDSPACE(8),FLD2                                                 
***      B     CDTLNS8                                                          
*                                                                               
CPACKLN  PACK  WORK(5),0(0,RE)                                                  
         EJECT                                                                  
CUMPFLD2 SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
CUMPFLD  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
CNEXTEL  SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    CNEXTELX                                                         
         CLI   0(R5),0                                                          
         JNE   CNEXTEL                                                          
         LTR   R5,R5                                                            
CNEXTELX BR    R9                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
CPATCH   DC    30X'00'                                                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCC2FAC NTR1  BASE=*,LABEL=*      PROCESS COS2 FACTOR                          
*                                                                               
         TM    SVCLPROF+30,X'08'   SEE IF FACTOR TYPE COST2 CLT                 
         BNO   P_C2F_X                                                          
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'91'        CK FOR NEWREC FOR FACTOR ELEM                
         BRAS  R9,CNEXTEL                                                       
         BE    P_C2F_X             FOUND                                        
*                                                                               
         L     R1,TRADDR                                                        
         CLI   8(R1),C'C'          CHANGE?                                      
         BNE   P_C2F46                                                          
         TM    GENBYSW1,C2FOVRDQ   FACTOR IS OVERRIDDEN IN BUY?                 
         BNZ   P_C2F46                                                          
         LR    RF,R5               SAVE NEWREC POINTER                          
         LA    R5,REC+33                                                        
         BRAS  R9,CNEXTEL                                                       
         BNE   P_C2F36                                                          
         USING PCOS2FEL,R5                                                      
         TM    PCOS2FLG,PCOS2OVR   FACTOR IS OVERRIDDEN IN BUY?                 
         BZ    P_C2F36                                                          
         ZAP   SVE2FAC,PCOS2FAC                                                 
         ZAP   SVC2FAC,PCOS2FAC                                                 
         OI    GENBYSW1,C2FOVRDQ                                                
         DROP  R5                                                               
P_C2F36  LR    R5,RF               RESTORE NEWREC POINTER                       
*                                                                               
P_C2F46  LA    R1,SVE2FAC                                                       
         OC    SVE2FAC,SVE2FAC                                                  
         BZ    *+14                                                             
         CP    SVE2FAC,=P'0'                                                    
         BNE   *+8                                                              
         LA    R1,SVC2FAC                                                       
*                                                                               
         XC    WORK(10),WORK       NOT FOUND -  MUST ADD ELEM                   
         LA    RE,WORK                                                          
         USING PCOS2FEL,RE                                                      
         MVC   WORK(2),=X'9108'                                                 
         MVC   PCOS2FAC,0(R1)                                                   
         TM    GENBYSW1,C2ROUNDQ   COS2 FACTOR ROUNDING?                        
         BZ    *+8                                                              
         OI    PCOS2FLG,PCOS2RND                                                
         TM    GENBYSW1,C2FOVRDQ   FACTOR IS OVERRIDDEN IN BUY?                 
         BZ    *+8                                                              
         OI    PCOS2FLG,PCOS2OVR                                                
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK,(R5)                                 
         NI    GENBYSW1,X'FF'-C2FOVRDQ                                          
         DROP  RE                                                               
*                                                                               
P_C2F_X  J     EXIT_X                                                           
*                                                                               
EXIT_X   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCLTFRZ NTR1  BASE=*,LABEL=*      CHECK CLIENT FROZEN OPTION                   
*                                                                               
         CLI   BYPROF+14,C'Y'      NO CHANGE TO FRZ CLIENT?                     
         JNE   CKCFRZ_X                                                         
         TM    SVCLPROF+30,X'02'   FROZEN CLIENT?                               
         JZ    CKCFRZ_X                                                         
*                                                                               
         TM    SVCLPROF+27,X'08'   LOCK THIS MONTH AND ALL FORWARD?             
         JNZ   CKCFRZ20                                                         
         TM    SVCLPROF+27,X'04'   LOCK THIS MONTH AND ALL PRIOR?               
         JNZ   CKCFRZ40                                                         
         TM    SVCLPROF+27,X'02'   LOCK THIS MONTH ONLY?                        
         JNZ   CKCFRZ60                                                         
*                                                                               
         J     CKCFRZER            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKCFRZ20 CLC   PBUYKDAT(2),SVCLPROF+28                                          
         JL    CKCFRZ_X                                                         
         J     CKCFRZER            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKCFRZ40 CLC   PBUYKDAT(2),SVCLPROF+28                                          
         JH    CKCFRZ_X                                                         
         J     CKCFRZER            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKCFRZ60 CLC   PBUYKDAT(2),SVCLPROF+28                                          
         JNE   CKCFRZ_X                                                         
         J     CKCFRZER            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKCFRZ_X CR    RB,RB               EQUAL                                        
         J     *+6                                                              
CKCFRZER LTR   RB,RB               NOT EQUAL (ERROR)                            
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SCAND    DSECT                                                                  
FLD1L    DS    CL1                    LENGTH OF FIELD 1                         
FLD2L    DS    CL1                    LENGTH OF FIELD 2                         
FLD1VAL  DS    CL1                    VALIDATION OF FIELD 1                     
FLD2VAL  DS    CL1                    VALIDATION OF FIELD 2                     
FLD1B    DS    CL4                    BINARY VALUE OF FLIELD 1                  
FLD2B    DS    CL4                    BINARY VALUE OF FLIELD 2                  
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
         SPACE 2                                                                
*                                                                               
DLINE    DSECT                        FOR DISPLAYING WSJ BUY INFO               
NUNITS   DS    CL7                                                              
         DS    CL1                                                              
NAT      DS    CL2                                                              
         DS    CL1                                                              
NRATE    DS    CL8                                                              
         DS    CL1                                                              
EUNITS   DS    CL7                                                              
         DS    CL1                                                              
EAT      DS    CL2                                                              
         DS    CL1                                                              
ERATE    DS    CL8                                                              
         DS    CL1                                                              
TCOST    DS    CL10                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
*                                                                               
         ORG   NEWREC                                                           
*                                                                               
       ++INCLUDE PPBUYWRK2                                                      
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075PPBUY14   06/02/20'                                      
         END                                                                    
