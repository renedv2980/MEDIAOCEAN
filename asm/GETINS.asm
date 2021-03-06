*          DATA SET GETINS     AT LEVEL 028 AS OF 06/13/16                      
*PHASE T00AABA                PHASE IS T00AAB (NOTE "A" APPENDED)               
*&&DO*&& CATALP GETINS                                                          
           TITLE 'LOG OF PGM CHANGES'                                           
*                                                                               
* BPLA 09/12    SUPPORT FOR BIGGER BILL ELEMENTS                                
*               PO# BILLING                                                     
*               NEW VALUES FOR PARAMETER 4                                      
*               P = REPORT BILLING FOR A REQUESTED PO#                          
*                   (ALSO EXCLUDES FX CHGS - LIKE X)                            
*               B = REPORT $ FOR AN ADDITIONAL CHG                              
*                   ALSO FILTERING ON A PO#                                     
*                                                                               
************    WHEN USING P OR B                                               
************                                                                    
************    SPECIAL NOTE - CALLING PROGRAM SHOULD CLEAR                     
************                   ORDERED $ IF THE REQUESTED PO# DOESN'T           
************                   MATCH THE INSERTION'S CURRENT PO#                
*                                                                               
* BPLA 11/12    QST CHANGE 1/1/13  9.5% TO 9.975%                               
*               ALSO NOT NOW CASCADING - QST BASIS                              
*               NOW EQUAL GST BASIS  - NOT AMT+GST                              
*               PGSTTAB - NEW VERSION                                           
*                                                                               
* BPLA 11/11    QST CHANGE 1/1/12  8.5% TO 9.5%                                 
*               NEW PGSTTAB - LEVEL 19                                          
* BOBY 09/11    KEEP ADDITIONAL CHARGE TC OUT OF GROSS & NET DLRS               
*                                                                               
* BPLA 12/10    QST CHANGE 1/1/11  7.5% TO 8.5%                                 
*               NEW PGSTTAB                                                     
*                                                                               
* BOBY 06/10    NEW PSTS FOR ON AND BC                                          
*                                                                               
* BOBY 12/07    HANDLE FX ADDITIONAL CHARGE                                     
*                                                                               
* BOBY 11/07    HANDLE NEW GST/HST RATE IN CANADA                               
*                                                                               
* BOBY 10/06    RETRIEVE PLANNED COSTS DATA                                     
*               ORDERED AMOUNTS GROSS/NET REPLACED BY PLANNED COSTS             
*                                                                               
* BOBY 7/06     HANDLE NEW GST/HST RATE IN CANADA                               
*                                                                               
* BOBY 8/05     ADD OPTION TO ROUND COST 2 DOLLARS TO NEAREST $5                
*                                                                               
* BOBY 2/01     CHANGES FOR SPECIAL CHARGES                                     
*                                                                               
* BPLA 2/99     CHANGES FOR 2 TYPES OF COST2'S                                  
*               FACTOR (CARAT) - LOOK FOR X'91' FACTOR ELEMENT                  
*               COST (RJ PALMER) - USE X'30' (FINANCIAL OPEN RATE)              
*               PARAMETER 2=C'O' WILL RETURN COST2 VALUES                       
*              NOTE THAT COST2 (OPEN) CD IS BASED ON CONTRACT NET               
*              AND THAT COST2 BILLING USES FINANCIAL BILLING                    
*              ELEMENTS (X'28')                                                 
*                                                                               
*                                                                               
* BPLA 2/97     CHANGES FOR HST                                                 
*                                                                               
* BOBY JAN94    ADD PST CALCULATIONS                                            
* BPLA 7/9/92   REDO COMMISSION BILLING OPTIONS TO INCLUDE NET                  
*               PARS+4 = 'C' OR 'N'                                             
*               ALSO SPECIAL HANDLING OF UFC COMM AND NET                       
*               BILLING ELEMENTS (UNLESS PARS+4 = 'C' OR 'N')                   
*                                                                               
* BPLA 4/29/92  OPTION TO RETURN COMMISSION BILLING                             
*               'C' IN BYTE 0 PARAMETER 2                                       
* BPLA 4/23/91 IN LEFTCOMP DON'T GIVE THE PRODUCT A PENNY REMAINDER             
*              IF SHARE IS ZERO                                                 
* ROSA 2/7/91 CLEAR ALL GST FIELDS IF A COMMISSION BUY                          
*                                                                               
*                                                                               
* BPLA 2/5/91  BUG FIXED IN GETX                                                
*              IMPROPER ALLOCATION OF GSTTAXPD                                  
*                                                                               
* BPLA 1/22/91 ADD GST BILLED CALCULATION                                       
*              ALSO USE GVALUES DSECT TO COVER RETURNED                         
*              GST INFO. *NOTE THAT BILLED GST IS "PAYABLE GST"                 
*                                                                               
* BPLA/ROSA 11/16/90-1/10/91 CHANGES FOR GST                                    
*      IF PARAMETER 4 POINTS TO CONSTANT                                        
*      'BOTH' PASS COUNT AND GST DATA ----                                      
*      'COUNT' PASS ONLY PAY/BILLED COUNTS ONLY                                 
*      'GST'   PASS ONLY GST INFO      ---                                      
*      IN ALL 3 CASES REPLACE THE ADDRESS OF THE FOURTH PARAMETER               
*       WITH THE ADDRESS OF GSPAYCNT.                                           
*====================================================================           
*  GST TAX CALCUALTED AS (GROSS-A/C)*GST TAX %                    !             
*  TAX (SALES OR PROVINCIAL) CALCULATED AS                        !             
*           (GROSS-A/C+GST TAX)* SALES TAX PCT                    !             
*  TAX (SALES OR PROVINCIAL) IS CALCULATED ON NET IF                            
*             GSTCODE IS 'T'                                                    
*  GROSS DOLLARS INCLUDES SALES TAX // NOT GST \\                 !             
*               ---------------------- -------                    !             
*====================================================================           
*                                                                               
*      PBUYREC NOW HAS PBDCNDA = X'80' IF CANADIAN AGY                          
*      AND PBDGST IS GST CODE FROM PUBREC NAME ELEM                             
*                                                                               
* ROSA 1/12/88  ADD THREE OPTIONS TO BYTE 0 AND ADD NEW PARAMETER               
*      BYTES 12-15.  THIS WORD WILL POINT TO A START AND END DATE               
*      IN BINARY.                                                               
*      IF BYTE 0 IS A C'P' ALL PAID ELEMENTS MUST FALL ON OR BTWN               
*              START AND END DATES                                              
*      IF BYTE 0 C'B' BILLING ELEMENTS MUST PASS SAME TESTS                     
*      IF BYTE O C'T' BOTH BILLING AND PAID MUST PASS TEST                      
*                                                                               
*      NEW VALUE FOR PBDCOSIN - C'C' COMMISSION RATE                            
*                                                                               
*          EVEN THOUGH PBDACP CAN BE SET TO ANYTHING THE USER                   
*          DESIRES, GETINS WILL CALCULATE AGYCOM AS IF IT WERE                  
*          100.0 (TO INSURE ZERO NET ON REPORTS).                               
*          BILL AND PAY ELEMS ALSO REFELECT 100.0 PCT AC.                       
*                                                                               
*          $BUY NOTES - 'C' CAN NOT BE ADDED/REMOVED FROM RATE                  
*                       AFTER BILLING/PAYING                                    
*                       PBDACP CANNOT BE CHANGED AFTER BILL/PAY                 
*                       PBDCD SHOULD BE 0.0                                     
*                       PBDTAX SHOULD ALSO BE 0                                 
*                                                                               
*          IF VALUE TABLE HAS =C'CRAT' GETINS WILL USE PBDACP                   
*          THIS PARAMETER IS ONLY NEEDED FOR THOSE REPORTS                      
*          THAT NEED TO SHOW 'TRUE AC'  - CURRENTLY ONLY THE                    
*          L1 AND THE NEW WRITER AND MAYBE BILLING                              
*                                                                               
*                                                                               
         TITLE 'GETINS - DERIVE ORD/PAID/BILLED BUY DATA'                       
* PARAMETER LIST IS                                                             
*                                                                               
*  PARAMETER 1                                                                  
*  BYTE 0      C'X' IF BILL/PAY DATA NOT REQUIRED                               
*  BYTE 0      C'P' PAID ELEMENTS MUST PASS DATE TEST                           
*  BYTE 0      C'B' BILL ELEMENTS MUST PASS DATE TEST                           
*  BYTE 0      C'T' BOTH PAID AND BILLED MUST PASS DATE TEST                    
*                                                                               
*       1-3    A(BUY RECORD)                                                    
*----------->                                                                   
*  PARAMETER 2                                                                  
*       0      C'O' DO OPEN RATE ORDERED AND BILLING                            
*              (IF OPEN RATE ELEM EXISTS)                                       
*                   ALSO WILL RETURN COS2 DOLLARS IF COS2 FACTOR                
*                     FOUND                                                     
*                                                                               
*       THE FOLLOWING 2 OPTIONS REALLY ONLY NEEDED FOR BILLING                  
*                                                                               
*              C'C' NORMAL ORDERED/PAID AMOUNTS BUT UFC COMM BILLING            
*                   (PBLILST X'01') REPORTED AS IT IS STORED                    
*                                                                               
*              C'N' NORMAL ORDERED/PAID AMOUNTS BUT UFC NET BILLING             
*                   (PBLILST X'02') REPORTED AS IT IS STORED                    
*                                                                               
*              C'P' ORDERED AMOUNTS REPLACED BY PLANNED COSTS                   
*                   PAYABLE DATA IGNORED                                        
*                   BILLABLE DATA BASED ON PLANNED COSTS                        
*                   NEW PLANNED COSTS ONLY                                      
*                   ON RETURN THIS PARM IS SET TO C'X' IF THERE IS              
*                                        NO PLANNED COST DATA                   
*                                                                               
*              C'B' PLANNED COSTS WITH COS2 FACTOR APPLIED                      
*                                                                               
*       1-3    A(VALUE TABLE)                                                   
*              (IF CONTAINS C'CRAT' PROCESS 'C' RATE BUYS USING                 
*              PBDACP INSTEAD OF 100.0 PCT)                                     
*              *NOTE - MUST CALCULATE 'TRUE AC' FROM BILL/PAY                   
*              ELEMENTS, SINCE THEY HAVE GROSS=AC                               
*----------->                                                                   
*  PARAMETER 3                                                                  
*       0      C'Y' OTHER AGENCY OPTION                                         
*       1-3    A(PRD CODE)                                                      
*                                                                               
*----------->                                                                   
*  PARAMETER 4                                                                  
*        0     X - EXCLUDE SPECIAL CHARGES FROM CALCULATIONS                    
*              A - PARAMETER 6 IS ACTIVE FOR SPECIAL CHARGES OPTIONS            
*              F - PARAMETER 6 IS ACTIVE FOR SPECIAL CHARGES OPTIONS            
*                  'FX' ADDITIONAL CHARGE NOT INCLUDED IN OPTIONS               
*              P - EXCLUDE SPECIAL CHARGES AND FILTER ON PO#                    
*                  PASSED IN PARAMETER 6                                        
*              B - REPORT THE ADDITIONAL CHARGE IN PARAMETER 6                  
*                  AND REPORT ONLY ADDITIONAL PO# THAT FOLLOWS                  
*                  THE CHARGE CODE                                              
*                                                                               
*       1-3    A(START/END DATES)    3 BYTE BINARY                              
*                                                                               
*----------->                                                                   
*  PARAMETER 5                                                                  
*              OPTIONALLY PASS BACK COUNT, GST DATA (ORDERED & PAID)            
*              OR BOTH                                                          
*       1-3    A(=C'COUNT')                                                     
*              ON RETURN THIS PARAMETER IS SET TO ADDRESS OF                    
*              2 FULL WORD BIN COUNTERS  - PAY ELEMS/BILL ELEMS                 
*       1-3    A(=C'GST')                                                       
*              ON RETURN THIS PARAMETER IS SET TO ADDRESS OF                    
*              OPTIONS FIELD AND WILL PASS BACK GST DATA (SEE DSECT)            
*       1-3    A(=C'PST')                                                       
*              ON RETURN THIS PARAMETER IS SET TO ADDRESS OF                    
*              OPTIONS FIELD AND WILL PASS BACK GST & PST DATA                  
*              (SEE DSECT)                                                      
*       1-3    A(=C'BOTH')                                                      
*                                                                               
*              ON RETURN THIS PARAMETER IS SET TO ADDRESS OF                    
*              OPTIONS FIELD AND WILL PASS COUNT AND GST/PST DATA               
*-----------------------------------------------------------------              
*----------->                                                                   
*  PARAMETER 6                                                                  
*              SPECIAL CHARGES OPTIONS                                          
*              ACTIVE ONLY IF PARAMETER 4-0 IS 'A','B','P','F'                  
*                                                                               
*              IF PARAMETER 4-0 IS A OR F                                       
*                                                                               
*       1-3    A(=C'ALL')                                                       
*              RETURN DATA FOR ALL SPECIAL CHARGES LUMPED                       
*              TOGETHER - EXCLUDES BASIC CHARGES                                
*              AND IF PARM4-0 = 'F' EXCLUDES FX ADDITIONAL CHARGE               
*       1-3    A(=C'XX')                                                        
*              RETURN DATA ONLY FOR SPECIAL CHARGE 'XX'                         
*       1-3    X'000000'                                                        
*              IF PARM4-0 IS 'F' RETURNS TOTAL COST LESS FX ADD CHG             
*                                                                               
*              IF PARAMETER 4-0 IS P                                            
*                                                                               
*              EXCLUDE ADDITIONAL CHARGES AND REPORT DATA FOR PO SEQ #          
*       1-3    A(=XL2) PO SEQ #                                                 
*              RETURN DATA ONLY FOR PO#                                         
*                                                                               
*              IF PARAMETER 4-0 IS B                                            
*              RETURN DATA FOR SPECIAL CHARGE XX AND PO SEQ #                   
*       1-3    A(=C'XX',SPACE,=XL2(PO SEQ#)                                     
*              RETURN DATA ONLY FOR CHARGE XX AND PO SEQ#                       
*              NOTE THE SPACE IS TO ALLOW FOR PROGRAMMERS                       
*              WHO USE THE "ALL" CALL - FOR ALL CHARGES                         
*                                                                               
*-----------------------------------------------------------------              
*                                                                               
         SPACE 2                                                                
GETINS   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 GETWORKL,GETINS,CLEAR=YES                                        
         USING GETWORK,RC                                                       
*                                                                               
         MVC   PARS(20),0(R1)       SAVE PARAMTER CALL ARGUMENTS                
         ST    R1,APARMS           SAVE ADDRESS OF PARAMETER LIST               
*                                                                               
         CLI   12(R1),0            SKIP IF NO SPECIAL CHARGES OPTION            
         BE    ACHGOPTX                                                         
*                                                                               
         CLI   12(R1),C'X'         IF EXCLUDING SPECIAL CHARGES                 
         BNE   *+12                                                             
         OI    ACSWITCH,ACSWNONQ      SET SWITCH                                
         B     ACHGOPTX                                                         
*                                                                               
         CLI   12(R1),C'P'         IF EXCLUDING SPECIAL CHARGES                 
         BNE   GETIN3              AND FILTERING ON PO#                         
         OI    ACSWITCH,ACSWNONQ      SET SWITCH                                
*                                                                               
         MVC   ACOPTA,20(R1)       SAVE A(ADDITINAL CHG OPTIONS)                
         SR    RF,RF                                                            
         ICM   RF,7,ACOPTA+1                                                    
         BZ    ACHGOPTX            ADDRESS OF PO# SEQ                           
         XC    POSEQ#,POSEQ#                                                    
         OC    0(2,RF),0(RF)       DO I HAVE A SEQ NUMBER?                      
         BZ    ACHGOPTX            IF NOT,DON'T SET SWITCH                      
*                                                                               
         OI    ACSWITCH,POSWONEQ   ELSE SET SWITCH                              
         MVC   POSEQ#,0(RF)        SAVE PO SEQ #                                
*                                                                               
         B     ACHGOPTX                                                         
*                                                                               
GETIN3   CLI   12(R1),C'B'         REPORTING SPECIAL CHARGES                    
         BNE   GETIN5              AND FILTERING ON PO#                         
*                                                                               
         MVC   ACOPTA,20(R1)       SAVE A(ADDITINAL CHG OPTIONS)                
         SR    RF,RF                                                            
         ICM   RF,7,ACOPTA+1                                                    
         BZ    ACHGOPTX            NO AC OR PO# ENTERED                         
*                                                                               
         CLC   =C'ALL',0(RF)       IF 'ALL' CHARGES TOGETHER                    
         BNE   *+12                                                             
         OI    ACSWITCH,ACSWALLQ      SET SWITCH                                
         B     GETIN4                                                           
*                                                                               
         OI    ACSWITCH,ACSWONEQ   ELSE SET SWITCH                              
         MVC   ACID,0(RF)               SAVE SPECIAL CHG ID                     
*                                                                               
GETIN4   XC    POSEQ#,POSEQ#                                                    
         OC    3(2,RF),3(RF)       PO SEQ # PRESENT?                            
         BZ    ACHGOPTX                                                         
         OI    ACSWITCH,POSWONEQ   ELSE SET SWITCH                              
         MVC   POSEQ#,3(RF)        SAVE PO SEQ #                                
*                                                                               
         B     ACHGOPTX                                                         
*                                                                               
GETIN5   CLI   12(R1),C'A'         OPTION MUST BE 'A'                           
         BE    *+8                                                              
         CLI   12(R1),C'F'         OR     MUST BE 'F'                           
         BNE   ACHGOPTX            UNKNOWN OPTION                               
*                                                                               
         CLI   12(R1),C'F'         IF EXCLUDING FOREIGN EXCHANGE                
         BNE   *+8                                                              
         OI    ACSWITCH,ACSWNFXQ      SET SWITCH                                
*                                                                               
         MVC   ACOPTA,20(R1)       SAVE A(ADDITINAL CHG OPTIONS)                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,ACOPTA+1                                                    
         BZ    ACHGOPTX            NO ADDITIONAL CHARGE OPTION                  
*                                                                               
         CLC   =C'ALL',0(RF)       IF 'ALL' CHARGES TOGETHER                    
         BNE   *+12                                                             
         OI    ACSWITCH,ACSWALLQ      SET SWITCH                                
         B     ACHGOPTX                                                         
*                                                                               
         OI    ACSWITCH,ACSWONEQ   ELSE SET SWITCH                              
         MVC   ACID,0(RF)               SAVE SPECIAL CHG ID                     
*                                                                               
ACHGOPTX DS    0H                                                               
*                                                                               
         L     RA,0(R1)            ESTABLISH BUYREC                             
         USING REC,RA                                                           
*                                                                               
         MVC   WRKAGY,PBUYKAGY     SAVE AGENCY CODE                             
*                                                                               
*        INIT GST, PST AND COUNT RELATED FIELDS                                 
*                                                                               
         MVI   GSTSW,0                                                          
         XC    GSPAYCNT,GSPAYCNT   CLEAR                                        
         XC    GSBILCNT,GSBILCNT   CLEAR                                        
         XC    GVALUES(GVALUESL),GVALUES  INIT GST FIELDS                       
*                                                                               
         LA    R0,10               10 PROVINCES                                 
         LA    RF,PSTAREA          CLEAR PST AREA                               
*                                                                               
         XC    0(PSTAREAL,RF),0(RF)                                             
         LA    RF,PSTAREAL(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
NOGST    MVI   COUNTYN,0                                                        
         L     RF,PARS+16                                                       
*                                                                               
         CLC   0(3,RF),=C'GST'     GST OPTION                                   
         BNE   *+12                                                             
         MVI   GSTSW,C'G'                                                       
         B     YISGST              LOAD ADDRESS TO CALLER                       
*                                                                               
         CLC   0(3,RF),=C'PST'     PST OPTION                                   
         BNE   NOGSTA                                                           
         MVI   GSTSW,C'G'                                                       
         MVI   PSTSW,C'P'                                                       
         B     YISGST              LOAD ADDRESS TO CALLER                       
*                                                                               
NOGSTA   CLC   0(4,RF),=C'BOTH'    PST/GST AND COUNT                            
         BNE   CKCOUNT                                                          
         MVI   GSTSW,C'G'                                                       
         MVI   PSTSW,C'P'                                                       
         B     YISBOTH                                                          
*                                                                               
CKCOUNT  CLC   0(5,RF),=C'COUNT'                                                
         BNE   NOCOUNT                                                          
*                                                                               
YISBOTH  MVI   COUNTYN,C'Y'                                                     
*                                                                               
YISGST   LA    RE,GSPAYCNT         POINT TO TOTAL TO BE PASSED                  
         ST    RE,16(R1)                                                        
*                                                                               
NOCOUNT  DS    0H                                                               
*                                                                               
         USING ELEM,R5                                                          
*                                                                               
         L     R2,4(R1)                                                         
         USING PVALUESD,R2                                                      
*                                                                               
         MVI   CRSW,0                                                           
         XC    STAX,STAX                                                        
*                                                                               
         CLC   0(4,R2),=C'CRAT'     CRATE                                       
         BNE   *+8                                                              
         MVI   CRSW,C'C'                                                        
*                                                                               
         XC    PVALUES(PVALUESX-PVALUES),PVALUES                                
         XC    FULL,FULL                                                        
*                                                                               
         MVC   SAVEAC,PBDACP      SAVE PBUYREC'S PBDACP                         
*                                                                               
         MVC   GPBDCIND,PBDCOSIN   SET RATE TYPE                                
         MVC   GPBDCTYP,PBDCOSTY   COST TYPE                                    
         MVC   GPBDNIND,PBDCTYP    SET NET INDICATOR                            
*                                                                               
         ZAP   GPBDCOS,=P'0'       CLEAR COST                                   
         TP    PBDCOS              MAKE SURE WE HAVE A COST                     
         BNZ   *+10                NO                                           
         ZAP   GPBDCOS,PBDCOS      COST OF BUY                                  
*                                                                               
*                                                                               
         CLI   PARS+4,C'P'         IF DOING PLANNED COSTS                       
         BE    *+8                                                              
         CLI   PARS+4,C'B'         IF DOING PLANNED COSTS                       
         BNE   *+8                                                              
         BRAS  RE,GETPC               GET PC ELEMENT                            
*                                                                               
         CLI   GPBDCIND,C'C'      SEE IF 'C' RATE BUY                           
         BNE   GET0                                                             
         CLI   CRSW,C'C'       SPECIAL C RATE PROCESSING                        
         BE    GET0                                                             
         ZAP   PBDACP,=P'-1'       SET TO 100.00 PCT                            
*                                                                               
GET0     L     RE,8(R1)                                                         
         MVC   PRDCODE,0(RE)                                                    
         CLI   PARS+8,C'Y'         TEST OTHER AGY OPTION                        
         BE    GT1                                                              
         CLI   PRDCODE,C'*'        SKIP O/A PRODUCT                             
         BE    GETX                                                             
*                                                                               
GT1      DS    0H                                                               
*                                                                               
         DROP  R5                                                               
*                                                                               
         GOTOR GETCAN,DMCB,0       GET DEFAULT GST/PST CODES/TAX RATES          
*                                                                               
         EJECT                                                                  
*------------------->  GROSS COST                                               
         MVI   GETPASS,0           SET FOR FIRST PASS                           
         MVI   BILELCOD,X'26'                                                   
*                                                                               
         TM    ACSWITCH,ACSWALLQ+ACSWONEQ SKIP IF ADDL CHGS ONLY                
         BZ    *+14                                                             
         ZAP   GPBDCOS,=P'0'       SET COST                                     
         B     GETACHG                                                          
*                                                                               
         SPACE 3                                                                
*----------------------------------------------------------------*              
* NOTE -- GETPASS HAS TWO SETTINGS 0 AND 1.  0 IS USED FOR NORMAL*              
* PROCESSING AND 1 IS USED TO CALCULATE OPEN RATES (WHEN         *              
*      PARAMATER 2 BYTE 0 ='O').                                 *              
*                                                                *              
*----------------------------------------------------------------*              
         SPACE 3                                                                
GT2      ZAP   DUB,GPBDCOS         CONVERT COST TO BIN                          
         CVB   R1,DUB              LOAD COST TO REG 1                           
         ZAP   DUB,PBDUNITS        CONVERT UNITS TO BINARY                      
         CVB   R0,DUB              LOAD TO 0                                    
         ST    R0,UNITS            SAVE BIN UNITS                               
         CLI   GPBDCTYP,C'U'       IF COST TYPE IS U THEN 5 DECIMALS            
         BNE   GET2                                                             
*-------------------------------  BRING TO 2 DECIMALS AND ROUND                 
         ZAP   P16,PBDUNITS                                     !               
         MP    P16,GPBDCOS                                      !               
         SRP   P16,64-3,5             ROUND                                     
         B     GT4                                              !               
*                                                                               
         CP    P16+8(3),=P'0'      SEE IF NEGATIVE              !               
         BL    GT3                                              !               
         CP    P16+8(3),=P'500'    ROUNDING                     !               
         BL    *+10                                             !               
         AP    P16(8),=P'1'                                     !               
         B     GT4                                              !               
*                                                               !               
GT3      CP    P16+8(3),=P'-500'                                !               
         BH    GT4                                              !               
         SP    P16(8),=P'1'                                     !               
*                                                               !               
GT4      DS    0H                                               !               
         ZAP   DUB,P16                                          !               
******   CVB   R1,DUB            COST IN R1                     !               
*---------------------------------------------------------------^               
         SPACE 3                                                                
*                                                                               
*--------- SEE IF UNITS IN 2 DECIMALS ---IF SO CONVERT -----------*             
*                                                                 !             
         CLI   PBDUIND,X'89'       LOWER CASE I                   !             
         BNE   GT8                                                              
*                                  MEANS UNITS TO 2 DECIMALS      !             
*                                  MUST DIVIDE COST BY 100 TO GET CENTS         
         SRP   DUB,64-2,5          ROUND                                        
         B     GT7                                                              
*                                                                               
         DP    DUB,=P'100'                                        !             
         CP    DUB+6(2),=P'0'      SEE IF NEGATIVE                !             
         BL    GT6                                                !             
         CP    DUB+6(2),=P'50'                                    !             
         BL    GT7                                                !             
         AP    DUB(6),=P'1'                                       !             
         B     GT7                                                !             
*                                                                 !             
GT6      CP    DUB+6(2),=P'-50'                                   !             
         BH    GT7                                                !             
         SP    DUB(6),=P'1'                                       !             
*GT7      ZAP   DUB,DUB(6)                                        !             
*                                                                               
GT7      DS    0H                                                               
         CVB   R1,DUB                                             !             
         B     GET2                                                             
*                                                                               
GT8      DS    0H                ***USED TO BE DONE AT GT4                      
         CVB   R1,DUB            COST IN R1                     !               
*-----------------------------------------------------------------^             
         SPACE 3                                                                
*--------------------------------> PREMIUM COST                                 
GET2     ZAP   DUB,PBDPRCOS                                                     
         CVB   RF,DUB                                                           
         ST    RF,PREMIUM          SAVE PREMIUM COST                            
         AR    R1,RF               AND ADD TO GROSS                             
         ST    R1,GROSS            SAVE COST                                    
         LR    RF,R1                                                            
*                                                                               
         SPACE 3                                                                
*                                                                               
*--------------------------------> CALCULATE AGENCY COMMISSION    !             
*                                                                 !             
         CLI   GPBDCIND,C'S'       BUY MADE WHERE COST = NET      !             
         BE    GET4                NO AC                          !             
         OC    PBDACP,PBDACP       AGENCY COMMISSION OVERRIDE     !             
         BZ    GET3                ASSUME 15%                     !             
         CP    PBDACP,=P'-1'       = 100 PCT                      !             
         BNE   GET3B                                              !             
         SR    RF,RF               ZERO NET                       !             
         B     GET4                                               !             
*                                                                 !             
GET3     DS    0H                                                 !             
         ZAP   PBDACP,=P'15000'                                   !             
GET3B    DS    0H                                                 !             
         ZAP   DUB,PBDACP                                         !             
         BZ    GET4                NO AC                          !             
         ZAP   P16,=P'100000'                                                   
         SP    P16,DUB                                                          
         CVD   R1,DUB              GROSS                                        
         MP    P16,DUB             AGENCY COMMISSION                            
         SRP   P16,64-5,5          ROUND                                        
         ZAP   DUB,P16                                                          
         CVB   RF,DUB                                                           
         B     GET4                                                             
*                                                                               
         CVB   R0,DUB                                             !             
         L     RF,=F'100000'                                      !             
         SR    RF,R0                                              !             
         MR    RE,R1                                              !             
         SLDL  RE,1                                               !             
         D     RE,=F'100000'                                      !             
         LTR   RF,RF                                              !             
         BM    *+8                                                !             
         AH    RF,=H'1'                                           !             
         SRA   RF,1                RF=NET                         !             
*                                                                 !             
GET4     DS    0H                                                 !             
         LR    R0,R1               GROSS                          !             
         SR    R0,RF               LESS NET                       !             
         ST    R0,AGYCOM                                          !             
*-----------------------------------------------------------------^             
*                                                                               
         SPACE 3                                                                
*--------------------------------->  CALCULATE CASH DISCOUNT                    
GET4B    DS    0H                                                               
         CLI   GETPASS,1           WHAT PASS                                    
         BE    GET4C          DON'T STORE CD FOR PASS 1                         
*                                                                               
         ZAP   P16,PBDCD           CD                                           
         CVD   RF,DUB              GROSS                                        
         MP    P16,DUB             CD TO 5 DECIMALS                             
         SRP   P16,64-3,5          ROUND                                        
         ZAP   DUB,P16                                                          
         CVB   RF,DUB                                                           
         B     GET4B1                                                           
*                                                                               
         CVB   RE,DUB                                                           
         MR    RE,RE                                                            
         SLDL  RE,1                                                             
         D     RE,=F'1000'                                                      
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
*                                                                               
GET4B1   DS    0H                                                               
         ST    RF,CSHDSC           SAVE CASH DISCOUNT                           
*                                                                               
GET4C    DS    0H                                                               
*                                                                               
*--------------------------------->  END CALCULATE CASH DISCOUNT                
*                                                                               
         SPACE 3                                                                
*                                                                               
*        CALCULATE ANY ADDITIONAL CHARGES                                       
*                                                                               
GETACHG  DS    0H                                                               
*                                                                               
         TM    ACSWITCH,ACSWNONQ   SKIP IF ADDL CHGS EXCLUDED                   
         BO    GETACHGX                                                         
*                                                                               
         L     R6,PARS             GET A(BUYREC)                                
         LA    R6,33(R6)           POINT TO FIRST ELEMENT IN RECORD             
         SR    R0,R0                                                            
*                                                                               
*        SEARCH FOR ADDITIONAL CHARGE ELEMENTS                                  
*                                                                               
GTACLOOP DS    0H                                                               
*                                                                               
         USING ELEM,R6             ESTABLISH ELEMENT DSECTS                     
*                                                                               
         CLI   0(R6),0             DONE AT END OF RECORD                        
         BE    GTACDONE                                                         
*                                                                               
         CLI   0(R6),X'44'         SKIP UNLESS ADDL CHG ELEMENT                 
         BNE   GTACCONT                                                         
*                                                                               
         TM    ACSWITCH,ACSWONEQ   IF ONLY LOOKING FOR ONE TYPE                 
         BNO   *+14                                                             
         CLC   ACID,PACCODE           MATCH ON CHARGE ID                        
         BNE   GTACCONT                                                         
*                                                                               
         CLC   =C'H7',WRKAGY       SKIP IF NOT GROUP M                          
         BE    *+10                                                             
         CLC   =C'SJ',WRKAGY       AND  IF NOT SJR                              
         BE    *+10                                                             
         CLC   =C'HD',WRKAGY       AND  IF NOT HDTO                             
         BE    *+10                                                             
         CLC   =C'*B',WRKAGY       AND  IF NOT DDSB                             
         BNE   *+22                                                             
         TM    ACSWITCH,ACSWONEQ   SKIP IF ONLY LOOKING FOR ONE TYPE            
         BO    *+14                                                             
         CLC   =C'TC',PACCODE      DO NOT INCLUDE TRADE COSTS                   
         BE    GTACCONT                                                         
*                                                                               
         TM    ACSWITCH,ACSWNFXQ   IF EXCLUDING FOREIGN EXCHANGE                
         BNO   *+14                                                             
         CLC   =C'FX',PACCODE         SKIP FX CHARGE ELEMENT                    
         BE    GTACCONT                                                         
*                                                                               
*        GROSS FOR ADDITIONAL CHARGE TO BE ADDED TO COST                        
*                                                                               
         ZAP   DUB,PACAMT          GET GROSS COST FOR ADDL CHG                  
         CVB   RF,DUB              CVB                                          
         A     RF,GROSS            UPDATE BUY GROSS                             
         ST    RF,GROSS                                                         
*                                                                               
*        CALCULATE AGENCY COMMISSION FOR ADDITIONAL CHARGE                      
*                                                                               
         CVB   RF,DUB              COPY ADDL CHG GROSS                          
*                                                                               
         CLI   PACAC,C'Y'          SKIP IF NO AGENCY COMMISSION                 
         BNE   GTACAGCX                                                         
*                                                                               
         CVB   R1,DUB              COPY ADDL CHG GROSS                          
*                                                                               
         OC    PACACOM,PACACOM     IF AGENCY COMM OVERRIDE EXISTS               
         BZ    *+14                                                             
         ZAP   DUB,PACACOM            USE IT                                    
         B     GTACAGC1                                                         
*                                                                               
*        DEFAULT TO BUY AGENCY COMMISION                                        
*                                                                               
         CLI   GPBDCIND,C'S'       BUY MADE WHERE COST = NET      !             
         BE    GTACAGCX            NO AC                          !             
*                                                                               
         OC    PBDACP,PBDACP       IF NO AGENCY COMMISSION OVERRIDE             
         BNZ   *+14                                                             
         ZAP   DUB,=P'15000'          ASSUME 15%                                
         B     GTACAGC1                                                         
*                                                                               
         CP    PBDACP,=P'-1'       IF AGENCY COMM = 100 PCT                     
         BNE   *+10                                               !             
         SR    RF,RF                  ZERO NET                                  
         B     GTACAGC4                                           !             
*                                                                 !             
         ZAP   DUB,PBDACP                                         !             
         BZ    GTACAGC4            NO AC                          !             
*                                                                 !             
GTACAGC1 DS    0H                                                 !             
*                                                                 !             
         CVB   R0,DUB              CALCULATE AGENCY COMMISSION    !             
         L     RF,=F'100000'                                      !             
         SR    RF,R0                                              !             
         MR    RE,R1                                              !             
         SLDL  RE,1                                               !             
         D     RE,=F'100000'                                      !             
         LTR   RF,RF                                              !             
         BM    *+8                                                !             
         AH    RF,=H'1'                                           !             
         SRA   RF,1                RF=NET                         !             
*                                                                 !             
GTACAGC4 DS    0H                                                 !             
*                                                                 !             
         LR    R0,R1               COPY GROSS                     !             
         SR    R0,RF               SUBTRACT NET                   !             
*                                                                 !             
         A     R0,AGYCOM           UPDATE AGENCY COMMISSION                     
         ST    R0,AGYCOM                                          !             
*                                                                 !             
GTACAGCX DS    0H                                                               
*                                                                               
*        CALCULATE CASH DISCOUNT                                                
*                                                                               
         CLI   GETPASS,1           WHAT PASS                                    
         BE    GTACCDX             DON'T STORE CD FOR PASS 1                    
*                                                                               
         CLI   PACCD,C'Y'          SKIP IF NOT SUBJECT TO CASH DISCOUNT         
         BNE   GTACCDX                                                          
*                                                                               
         ZAP   DUB,PBDCD           GET CASH DISCOUNT RATE                       
         CVB   RE,DUB                                                           
*                                                                               
         MR    RE,RE               RF HAS NET FOR ADDL CHARGE                   
         SLDL  RE,1                                                             
         D     RE,=F'1000'                                                      
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
*                                                                               
         A     RF,CSHDSC           UPDATE CASH DISCOUNT                         
         ST    RF,CSHDSC                                                        
*                                                                               
GTACCDX  DS    0H                                                               
*                                                                               
GTACCONT DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,1(R6)            GET ELEMENT LENGTH                           
         LA    R6,0(RF,R6)         BUMP TO NEXT ELEMENT                         
         B     GTACLOOP                                                         
*                                                                               
GTACDONE DS    0H                                                               
*                                                                               
GETACHGX DS    0H                                                               
*                                                                               
         L     R1,GROSS            GET GROSS                                    
         S     R1,CSHDSC           GROSS-CSHDSC                                 
         ST    R1,BLABLE           * BILLABLE DOLLARS                           
*                                                                               
         S     R1,AGYCOM           BLABLE-AGYCOM                                
         ST    R1,PYABLE           * PAYABLE DOLLARS                            
*                                                                               
*              SAVE BILLABLE DATE                                               
*                                                                               
         OC    BLBLDT,BLBLDT                                                    
         BNZ   *+10                                                             
         MVC   BLBLDT,PBDBDATE                                                  
*                                                                               
         SPACE 3                                                                
*-------------------->   CALCULATE TAX AND ADD TO GROSS                         
*                                  BLABLE, AND PYABLE                           
GET6     DS    0H                                                               
**       CLI   PBDELEM+1,105                                                    
**       BL    GET18               ONLY ON NEW PBDELEMS                         
*                                  TAX CHECK NO-OPED                            
***      OC    PBDTAX,PBDTAX       CHK FOR TAX                                  
***      BZ    GET18                                                            
         CLI   GETPASS,0        FOR GETPASS=1 LEAVE TAX ALONE                   
         BE    GET10                                                            
         MVC   FULL,STAX           USE STAX FROM GETPASS=0                      
         B     GET15                                                            
*                                                                               
GET10    L     RF,GROSS                                                         
         S     RF,AGYCOM                                                        
         ST    RF,WRKNET           SAVE WORKING NET                             
*                                                                               
GETACGRX DS    0H                                                               
*                                                                               
*---------------------------> CALCULATE GST                                     
*        ALWAYS CALCULATE FOR CANADIAN BUY                                      
*          BECAUSE PROVINCIAL SALES TAX MAY DEPEND ON GST                       
*                                                                               
         TM    PBDCNDA,X'80'       SKIP IF NOT CANADIAN                         
         BZ    GET10X                                                           
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,GSTPCT         GST PER CENT                                 
         BZ    GET10F              NO TAX                                       
*                                                                               
         MR    RE,RE               RF HAS NET                                   
         SLDL  RE,1                                                             
         D     RE,=F'100000'        GST TAX HAS 3 DECIMALS                      
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
*                                                                               
         STCM  RF,15,GSTTAX        RETURN TAX                                   
*                                                                               
GET10F   DS    0H                                                               
*                                                                               
*        CALCULATE PST IF NEEDED                                                
*                                                                               
GTPPST   DS    0H                                                               
*                                                                               
         LA    R9,PSTAREA          ESTABLISH PSTAREA                            
         USING PSTAREA,R9                                                       
*                                                                               
         LA    R0,10               10 PROVINCES                                 
*                                                                               
GTPPSTLP DS    0H                                                               
*                                                                               
         OC    PSTPROV,PSTPROV     DONE IF NO MORE PROVINCES                    
         BZ    GTPPSTDN                                                         
*                                                                               
         L     RF,GROSS            CALCULATE NET=GROSS-AGYCOM                   
         S     RF,AGYCOM                                                        
*                                                                               
         TM    PSTBASIS,X'01'      SKIP PST ON NET ONLY                         
         BO    *+8                                                              
         A     RF,GSTTAX           ELSE ON NET + GST TAX                        
*                                                                               
         LR    RE,RF               UPDATE BASIS TAX CALCULATED ON               
         A     RE,PST$BS                                                        
         ST    RE,PST$BS                                                        
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,PSTPCT         GET PST PERCENT                              
         MR    RE,RE               CALCULATE PST TAX                            
*                                                                               
         SLDL  RE,1                DOUBLE RESULT                                
         D     RE,=F'100000'       PER CENT HAS 3 DECIMALS                      
         LTR   RF,RF               CHECK FOR NEGATIVE NUMBER                    
         BM    *+8                                                              
         AH    RF,=H'1'            PREPARE FOR ROUNDING                         
         SRA   RF,1                ROUND                                        
*                                                                               
         STCM  RF,15,PSTTAX        SAVE TAX                                     
*                                                                               
GTPPSTCN DS    0H                                                               
*                                                                               
         LA    R9,PSTAREAL(R9)     BUMP TO NEXT PSTAREA                         
         BCT   R0,GTPPSTLP                                                      
*                                                                               
GTPPSTDN DS    0H                                                               
*                                                                               
GTPPSTX  DS    0H                                                               
*                                                                               
         DROP  R9                                                               
*                                                                               
*        CALCULATE SALES TAX                                                    
*                                                                               
         L     RF,GROSS                                                         
         S     RF,AGYCOM                                                        
*                                                                               
         TM    GSTBASIS,X'01'                                                   
         BO    GET10X             SALES TAX IS ON NET                           
         A     RF,GSTTAX          SALES TAX IS ON NET + GST                     
*                                                                               
GET10X   XC    FULL,FULL                                                        
         CLI   PBDELEM+1,105                                                    
         BL    GET13                                                            
         OC    PBDTAX,PBDTAX                                                    
         BZ    GET13                                                            
         MVC   FULL+1(3),PBDTAX                                                 
         L     RE,FULL                                                          
         MR    RE,RE                                                            
         SLDL  RE,1                                                             
         D     RE,=F'1000000'        PBDTAX HAS 4 DECIMALS                      
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,FULL                                                          
         MVC   TAX,FULL                                                         
         MVC   STAX,FULL                                                        
*                                                                               
GET13    DS    0H                                                               
*                                                                               
GET15    L     R0,GROSS                                                         
         A     R0,FULL                                                          
         ST    R0,GROSS                                                         
         L     R0,BLABLE            BILLABLE = GROSS -CD                        
         A     R0,FULL                                                          
         ST    R0,BLABLE                                                        
         L     R0,PYABLE            PAYABLE = AGYCOM                            
         A     R0,FULL                                                          
         ST    R0,PYABLE                                                        
*                                                                               
GET16    DS    0H                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
GET18    CLI   GETPASS,1                                                        
         BE    GET20                                                            
         CLI   PARS+4,C'O'          SEE IF DOING OPEN RATES                     
         BE    *+8                                                              
         CLI   PARS+4,C'B'         IF DOING PLANNED COSTS & COS2                
         BNE   GET19                                                            
*                                                                               
         LA    R5,PBDELEM                                                       
         MVI   ELCODE,X'91'         FIRST SEARCH FOR COST2 FACTOR               
         BAS   R9,NEXTEL                                                        
         BNE   GET18O               NOT FOUND - SEARCH FOR OPEN                 
*                                                                               
         USING PCOS2FEL,R5         ESTABLISH COST2 ELEMENT                      
*                                                                               
         ZAP   P16,GPBDCOS         GET CURRENT GROSS                            
*                                                                               
         TM    PCOS2FLG,PCOS2RND   IF SPECIAL ROUNDING                          
         BNO   GET18A                                                           
*                                                                               
         BRAS  RE,COS2RND             ROUND TO NEAREST $5                       
         B     GET18D                                                           
*                                                                               
GET18A   DS    0H                                                               
*                                  APPLY FACTOR TO GPBDCOS                      
         MP    P16,PCOS2FAC         MULIPLY BY FACTOR (6 DECIMALS)              
******   DP    P16,=P'1000000'                                                  
         SRP   P16,64-6,5                                                       
         B     GET18C1                                                          
*                                                                               
         CP    P16+7(4),=P'0'       SEE IF NEGATIVE                             
         BL    GET18B                                                           
         CP    P16+7(4),=P'50000'                                               
         BL    *+10                                                             
         AP    P16(7),=P'1'                                                     
         B     GET18C                                                           
*                                                                               
GET18B   CP    P16+7(4),=P'-50000'                                              
         BH    GET18C                                                           
         SP    P16(7),=P'1'                                                     
*                                                                               
GET18C   DS    0H                                                               
         ZAP   P16,P16(7)                                                       
*                                                                               
GET18C1  DS    0H                                                               
*                                                                               
GET18D   ZAP   GPBDCOS,P16         STORE FACTORED COST (OR RATE)                
         B     GET18X              AND PROCEED                                  
GET18O   DS    0H                                                               
         LA    R5,PBDELEM                                                       
         MVI   ELCODE,X'30'         SEARCH FOR OPEN RATE ELEM                   
         BAS   R9,NEXTEL                                                        
         BNE   GET20                NOT FOUND - DONE AS REGULAR                 
         MVC   GPBDCTYP,2(R5)    OVERLAY COST TYPE AND GROSS WITH               
         MVC   GPBDCOS,3(R5)     OPEN RATE DATA                                 
*                                                                               
GET18X   DS    0H                                                               
         MVI   BILELCOD,X'28'       SEARCH FOR OPEN BILL ELEMS                  
         MVI   GETPASS,1         GO BACK AND PROCESS OPEN GROSS/TYPE            
         B     GT2                                                              
*                                                                               
GET19    DS    0H                                                               
         EJECT                                                                  
*----------------------> COLLECT PAID DATA                                      
*                                                                               
GET20    CLI   PARS,C'X'           TEST IF REQUESTED                            
         BE    GETX           NO IF C'X'                                        
*                                                                               
         LM    R6,R8,PGROSS                                                     
         MVI   ELCODE,X'25'                                                     
         LA    R5,PBDELEM                                                       
*                                                                               
GETP2    BAS   R9,NEXTEL                                                        
         BNE   GETPX                                                            
*                                                                               
         OC    PPDDATE,PPDDATE                                                  
         BZ    GETP2                                                            
* TEST TO SEE IF DATE TEST IS TO BE MADE FOR PAID DATA                          
         CLI   PARS,C'T'   BOTH BILLING AND PAID                                
         BE    YBOTH                                                            
         CLI   PARS,C'P'   PAID ONLY                                            
         BNE   NOPTEST                                                          
YBOTH    L     R9,PARS+12   LOAD ADD OF START AND END DATE                      
         CLC   PPDDATE,0(R9)  PAID TO START                                     
         BL    GETP2         BYPASS                                             
         CLC   PPDDATE,3(R9)  PAID TO END                                       
         BH    GETP2                                                            
*************                                                                   
NOPTEST  DS    0H                                                               
*                                                                               
*        CHECK FOR NEW PAYELEMS LENGTHS AND STATS                               
*                                                                               
*                                                                               
         CLI   1(R5),PPACCODE-PPAYELEM IF NO ROOM FOR ADDITIONAL CHGS           
         BNH   PDACHG1A                   ASSUME ELM NOT FOR ADD CHG            
*                                                                               
         TM    PPDSTAT,X'01'       IF SECONDARY SEQ # PRESENT                   
         BNO   PDACHG1                                                          
*                                                                               
         CLI   1(R5),PPSEQN2-PPAYELEM    NEW LENGTH TO BE CHECKED               
         BH    PDACHG1             THERE IS AN ADDITIONAL CHARGE                
*                                                                               
PDACHG1A DS    0H                                                               
*                                                                               
         TM    ACSWITCH,ACSWALLQ+ACSWONEQ  AND ONLY ADDL CHGS WANTED            
         BNZ   GETP2                  SKIP ELEMENT                              
*                                                                               
         B     PDACHG2             ELSE ACCEPT                                  
*                                                                               
PDACHG1  DS    0H                  ADDITIONAL CHARGE PAY ELEMENT                
*                                                                               
         TM    ACSWITCH,ACSWNONQ   SKIP IF ADDL CHGS NOT WANTED.                
         BO    GETP2                                                            
*                                                                               
         TM    ACSWITCH,ACSWONEQ   IF SINGLE ADDL CHG TYPE WANTED               
         BNO   *+14                                                             
         CLC   ACID,PPACCODE          MATCH TO ELEMENT CODE                     
         BNE   GETP2                                                            
*                                                                               
         CLC   =C'H7',WRKAGY       SKIP IF NOT GROUP M                          
         BE    *+10                                                             
         CLC   =C'SJ',WRKAGY       AND  IF NOT SJR                              
         BE    *+10                                                             
         CLC   =C'HD',WRKAGY       AND  IF NOT HDTO                             
         BE    *+10                                                             
         CLC   =C'*B',WRKAGY       AND  IF NOT DDSB                             
         BNE   *+22                                                             
         TM    ACSWITCH,ACSWONEQ   SKIP IF SINGLE ADDL CHG TYPE WANTED          
         BO    *+14                                                             
         CLC   =C'TC',PPACCODE     DROP TC ADDITIONAL CHARGEO                   
         BE    GETP2                                                            
*                                                                               
         TM    ACSWITCH,ACSWNFXQ   IF NO FOREIGN EXCHANGE                       
         BNO   *+14                                                             
         CLC   =C'FX',PPACCODE        SKIP IF FX CHARGE                         
         BE    GETP2                                                            
*                                                                               
PDACHG2  DS    0H                                                               
*                                                                               
         MVC   WORK(12),PPGROSS                                                 
         CLI   GPBDCIND,C'C'            SEE IF 'C' RATE                         
         BNE   GETP5                    NO                                      
         CLI   CRSW,C'C'             SPECIAL HANDLING                           
         BNE   GETP5                                                            
         BAS   R9,FIXAC                                                         
GETP5    A     R6,WORK                                                          
         A     R7,WORK+4                                                        
         A     R8,WORK+8                                                        
         CLI   GSTSW,C'G'          SEE IF GETTING GST                           
         BNE   GETP10                                                           
*                                                                               
         MVI   GETGSTSW,C'P'       SET DOING PAID GST                           
*                                                                               
         GOTOR GETCAN,DMCB,PPAYELEM GET GST/PST RATE                            
*                                                                               
         BRAS  RE,GETGST                                                        
*                                                                               
GETP10   L     RF,GSPAYCNT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,GSPAYCNT                                                      
         B     GETP2                                                            
*                                                                               
*                                                                               
GETPX    STM   R6,R8,PGROSS                                                     
         SR    R6,R7                                                            
         SR    R6,R8                                                            
         ST    R6,PAID                                                          
         B     GETB                                                             
*                                                                               
         EJECT                                                                  
*----------------------> COLLECT BILLED DATA                                    
*                                                                               
*                                                                               
GETB     MVC   ELCODE,BILELCOD                                                  
         LA    R5,PBDELEM                                                       
         LM    R6,R8,BGROSS                                                     
GETB2    BAS   R9,NEXTEL                                                        
         BNE   GETBX                                                            
         OC    PBLDATE,PBLDATE                                                  
         BZ    GETB2                                                            
*                                                                               
         CLI   PARS+4,C'C'         UFC COMMISSION BILLING                       
         BNE   GETB2C                                                           
         TM    PBBILST,X'01'       CHK RIGHT TYPE                               
         BZ    GETB2                                                            
         B     GETB2F                                                           
*                                                                               
GETB2C   CLI   PARS+4,C'N'         UFC NET BILLING                              
         BNE   GETB2F                                                           
         TM    PBBILST,X'02'       CHK RIGHT TYPE                               
         BZ    GETB2                                                            
*                                                                               
GETB2F   DS    0H                                                               
         CLC   PRDCODE,REC+7       REQUESTED PRD TO KEY                         
         BE    *+14                                                             
         CLC   PRDCODE,PBPRD       REQUESTED PRD TO ELEM                        
         BNE   GETB2                                                            
*                                                                               
* TEST TO SEE IF DATE TEST IS TO BE MADE FOR BILLING DATA                       
*                                                                               
         CLI   PARS,C'T'   BOTH BILLING AND PAID                                
         BE    YBOTHA                                                           
         CLI   PARS,C'B'   BILLING ONLY                                         
         BNE   NOBTEST                                                          
YBOTHA   L     R9,PARS+12   LOAD ADD OF START AND END DATE                      
         CLC   PBLDATE,0(R9)  BILLED TO START                                   
         BL    GETB2         BYPASS                                             
         CLC   PBLDATE,3(R9) BILLED TO END                                      
         BH    GETB2         BYPASS                                             
*************                                                                   
NOBTEST  DS    0H                                                               
*                                                                               
*        CHECKING LENGTH TO SEE IF CHARGE OR PO# CAN BE PRESENT                 
*     ** NOTE  PBACCODE+2 WILL BE PBPOSEQ IN NEW BILL ELEMENT DSECT             
*                                                                               
         CLI   1(R5),PBACCODE-PBILELEM IF NO ADDITIONAL CHARGES                 
         BH    BLACHG1                                                          
*                                                                               
         TM    ACSWITCH,ACSWALLQ+ACSWONEQ  AND ONLY ADDL CHGS WANTED            
         BNZ   GETB2                     SKIP ELEMENT                           
         TM    ACSWITCH,POSWONEQ    OR ONLY ONE PO# WANTED                      
         BNZ   GETB2                     SKIP ELEMENT                           
*                                                                               
         B     BLACHG2             ELSE ACCEPT                                  
*                                                                               
BLACHG1  DS    0H                  ADDITIONAL CHARGE PAY ELEMENT                
         OC    PBACCODE,PBACCODE   SEE IF CHARGE PRESENT                        
         BNZ   BLACHG1C            YES                                          
         TM    ACSWITCH,ACSWONEQ   WAS A SINGLE CHARGE WANTED                   
         BO    GETB2               IF SO, THEN SKIP                             
*                                                                               
         TM    ACSWITCH,POSWONEQ   IF SINGLE PO# WANTED                         
         BNO   *+14                                                             
         CLC   POSEQ#,PBACCODE+2    MATCH TO ELEMENT CODE                       
         BNE   GETB2                                                            
         B     BLACHG2             ELSE PROCESS                                 
*                                  BIGGER ELEMENTS MAY HAVE PO#                 
*                                                                               
BLACHG1C TM    ACSWITCH,ACSWNONQ   SKIP IF ADDL CHGS NOT WANTED.                
         BO    GETB2                                                            
*                                                                               
         TM    ACSWITCH,ACSWONEQ   IF SINGLE ADDL CHG TYPE WANTED               
         BNO   *+14                                                             
         CLC   ACID,PBACCODE        MATCH TO ELEMENT CODE                       
         BNE   GETB2                                                            
*                                                                               
         TM    ACSWITCH,POSWONEQ   IF SINGLE PO# WANTED                         
         BNO   *+14                                                             
         CLC   POSEQ#,PBACCODE+2      MATCH TO ELEMENT CODE                     
         BNE   GETB2                                                            
*                                                                               
         CLC   =C'H7',WRKAGY       SKIP IF NOT GROUP M                          
         BE    *+10                                                             
         CLC   =C'SJ',WRKAGY       AND  IF NOT SJR                              
         BE    *+10                                                             
         CLC   =C'HD',WRKAGY       AND  IF NOT HDTO                             
         BE    *+10                                                             
         CLC   =C'*B',WRKAGY       AND  IF NOT DDSB                             
         BNE   *+22                                                             
         TM    ACSWITCH,ACSWONEQ   SKIP IF SINGLE ADDL CHG TYPE WANTED          
         BO    *+14                                                             
         CLC   =C'TC',PBACCODE     IGNORE TC ADDITIONAL CHARGE                  
         BE    GETB2                                                            
*                                                                               
         TM    ACSWITCH,ACSWNFXQ   IF NOT INCLUDING FOREIGN EXCHANGE            
         BNO   *+14                                                             
         CLC   =C'FX',PBACCODE        SKIP IF FX ELEMENT                        
         BE    GETB2                                                            
*                                                                               
BLACHG2  DS    0H                                                               
*                                                                               
         MVC   WORK(12),PBGROSS                                                 
*                                                                               
         CLI   GPBDCIND,C'C'            SEE IF 'C' RATE                         
         BNE   GETB5                    NO                                      
         CLI   CRSW,C'C'             SPECIAL HANDLING                           
         BNE   GETB5                                                            
         BAS   R9,FIXAC                                                         
GETB5    DS    0H                                                               
         CLI   PARS+4,C'C'           DON'T DO SPECIAL HANDLING                  
         BE    GETB8                 FOR UFC COMM OR NET BILLING                
         CLI   PARS+4,C'N'                                                      
         BE    GETB8                                                            
*                                                                               
*        SPECIAL HANDLING OF UFC COMM AND NET BILL ELEMS                        
*        UNLESS PARS+4 = 'C' OR 'N'                                             
*                                                                               
         TM    PBBILST,X'01'          SEE IF UFC COMM BILLING                   
         BZ    GETB7                                                            
         MVC   WORK(4),WORK+4         SET GROSS TO AC                           
         XC    WORK+8(8),WORK+8       CLEAR CD                                  
*                                     "EFFECTIVE GROSS" = AC                    
         B     GETB8                   (NET WILL BE ZERO)                       
*                                                                               
GETB7    TM    PBBILST,X'02'          SEE IF UFC NET BILLING                    
         BZ    GETB8                                                            
         L     R0,WORK                                                          
         S     R0,WORK+4              SET GROSS TO NET (GROSS - AC)             
         ST    R0,WORK                AND LEAVE AC AND CD ALONE                 
         XC    WORK+4(4),WORK+4       CLEAR AC (SO NET WILL BE NET)             
*                                     "EFFECTIVE GROSS" = NET                   
GETB8    A     R6,WORK                                                          
         A     R7,WORK+4                                                        
         A     R8,WORK+8                                                        
*                                                                               
         CLI   GSTSW,C'G'           SEE IF GETTING GST                          
         BNE   GETB10                                                           
*                                                                               
         MVI   GETGSTSW,C'B'        GET GST BILLED FROM ELEMENT                 
*                                                                               
         GOTOR GETCAN,DMCB,PBILELEM GET GST/PST RATE                            
*                                                                               
         BRAS  RE,GETGST                                                        
*                                                                               
GETB10   L     RF,GSBILCNT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,GSBILCNT                                                      
         B     GETB2                                                            
*                                                                               
GETBX    STM   R6,R8,BGROSS                                                     
         SR    R6,R7                                                            
         SR    R6,R8                                                            
         ST    R6,BILLED                                                        
         EJECT                                                                  
GETX     DS    0H                                                               
         MVC   PBDACP,SAVEAC         RESTORE PBUYREC'S PBDACP                   
*                                                                               
         CLC   PRDCODE,REC+7         IF NOT EQ DO POL ALLOCATIN                 
         BE    GETX8                                                            
*                                                                               
* SAVE PAID DATA.  BILL DATA WILL BE REPLACED WITH GST TAX DATA                 
*  THAT WILL BE SPLIT BY PRODUCT.  AT THE END OF ALLOCATION BILLING             
*  DATA WILL BE RESTORED.                                                       
*                                                                               
         MVC   SAVE2W(8),BGROSS                                                 
         MVC   BGROSS(8),GSTTAX    GST DATA OVERLAYING BILLED                   
*                                                                               
         IC    R3,PBDWTSUM         WEIGHTING FACTOR FOR BUY                     
         N     R3,=F'127'                                                       
         BAS   RE,REMCOMP                                                       
         SPACE 3                                                                
*                                                                               
*------------------> FIND PRDELEM TO CALCULATE PRODUCT SHARE                    
*                                                                               
         MVI   ELCODE,X'21'                                                     
         LA    R5,PBDELEM                                                       
GETX2    EQU   *                                                                
*                                                                               
         BAS   R9,NEXTEL           FIND PRODUCT ALLOCATION                      
         BE    GETX3                                                            
*                                                                               
         XC    PVALUES(PVALUESX-PVALUES),PVALUES  CLEAR - NO PRODUCT            
         XC    SAVE2W(8),SAVE2W                                                 
*                                                                               
         LA    R0,10               10 PROVINCES                                 
         LA    RF,PSTAREA          CLEAR PST AREA                               
*                                                                               
         XC    0(PSTAREAL,RF),0(RF)                                             
         LA    RF,PSTAREAL(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         B     GETX8A                                                           
*                                                                               
GETX3    IC    R4,PPRCOST          COST SHARE IN UNITS                          
         N     R4,=F'127'                                                       
         BAS   RE,LEFTCOMP                                                      
         CLC   PPRCODE,PRDCODE                                                  
         BNE   GETX2                                                            
         BAS   R9,NEXTEL           TEST IF AT LAST PRODUCT                      
         BE    GETX3PDN            NO                                           
         MVC   WORK+1(14),WORK+29      YES- MOVE LEFTOVERS TO THIS SHR          
*                                                                               
*        MOVE PST LEFTOVERS TO THIS SHARE                                       
*                                                                               
         LA    RE,10               10 PROVINCES                                 
         LA    R8,PSTWORK          LEFTOVERS SAVEAREA                           
*                                                                               
GETX3PLP DS    0H                                                               
*                                                                               
         LA    R6,PSTFLDNM         SET NUMBER OF FIELDS TO PROCESS              
*                                                                               
GETX3PTL DS    0H                                                               
*                                                                               
         LR    RF,R6               CALCULATE INDEX                              
         BCTR  RF,0                                                             
         MH    RF,=H'3'            3 BYTES OF WORK FOR EACH FIELD               
         LA    RF,0(RF,R8)                                                      
         MVC   0(1,RF),2(RF)       USE PST LEFTOVERS                            
*                                                                               
GETX3PTC DS    0H                                                               
*                                                                               
         BCT   R6,GETX3PTL                                                      
*                                                                               
GETX3PTD DS    0H                                                               
*                                                                               
GETX3PCN DS    0H                                                               
*                                                                               
         LA    R8,L'PSTWORK(R8)    NEXT PST WORKAREA                            
         BCT   RE,GETX3PLP                                                      
*                                                                               
GETX3PDN DS    0H                                                               
*                                                                               
         BAS   RE,GETSHR                                                        
*                                                                               
GETX8A   DS    0H                                                               
         MVC   GSTTAX(8),BGROSS    MOVE ALLOCATED TAX                           
         MVC   BGROSS(8),SAVE2W                                                 
*                                                                               
GETX8    DS    0H                                                               
         CLI   GPBDCIND,C'C'         SEE IF COMMISSION RATE BUY                 
         BNE   GETX9                                                            
*                                                                               
         XC    GSTTAX,GSTTAX         COMMISSION - CLEAR TAX                     
         XC    GSTTAXPD,GSTTAXPD     CLEAR PAID                                 
         XC    GSTTAXBL,GSTTAXBL     CLEAR BILLED                               
         XC    PYABLE,PYABLE         CLEAR NET ORDERED                          
         XC    PAID,PAID             CLEAR NET PAID                             
         XC    BILLED,BILLED         CLEAR NET BILLED                           
*                                                                               
         LA    R0,10               10 PROVINCES                                 
         LA    R9,PSTAREA          CLEAR PST TAXES                              
         USING PSTAREA,R9          ESTABLISH PSTAREA                            
*                                                                               
         XC    PSTTAX,PSTTAX       CLEAR TAX                                    
         XC    PSTTAXPD,PSTTAXPD   CLEAR TAX PAID                               
         XC    PSTTAXBL,PSTTAXBL   CLEAR TAX BILLED                             
         XC    PST$BS,PST$BS       CLEAR TAX DOLLAR BASIS                       
         XC    PST$BSPD,PST$BSPD   CLEAR TAX PAID DOLLAR BASIS                  
         XC    PST$BSBL,PST$BSBL   CLEAR TAX BILLED DOLLAR BASIS                
         LA    RF,PSTAREAL(RF)                                                  
         BCT   R0,*-22                                                          
*                                                                               
GETX9    DS    0H                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R9                                                               
*                                                                               
*  NOTE: TAX REMAINS AS CALCULATED ON NET FOR COMMISSION RATE BUY               
*  NOTE: PROGRAMS USING 'CRAT' IN VALUES USE PYABLE, PAID,AND                   
*        BILLED FOR THEIR NETS - NOT SUBTRACT AGYCOM FROM GROSS                 
*        TO GET NET.                                                            
*                                                                               
*EXIT     CLI   COUNTYN,C'Y'                                                    
*         BNE   EXITX                                                           
*         LA    RF,GSPAYCNT                                                     
*         ST    RF,PARS+16                                                      
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
         EJECT                                                                  
*--------------------------->  BREAK OUT COSTS FOR POOL PRODUCTS                
*                                                                               
GETSHR   NTR1                                                                   
*                                                                               
         LA    R7,GROSS                                                         
         LA    R6,14                ***** NOW 14       X                        
*                                                                               
GETSHR2  EQU   *                                                                
*                                                                               
         L     RF,0(R7)            AMT                                          
         SR    R0,R0                                                            
         IC    R0,WORK(R6)                                                      
         SR    RE,RE                                                            
         LTR   RF,RF                                                            
         BZ    GETSHR6                                                          
         BP    *+10                                                             
         LCR   R0,R0                                                            
         L     RE,=F'-1'                                                        
         DR    RE,R3               / WTSUM (NO ROUND)                           
         MR    RE,R4               X THIS SHARE                                 
         AR    RF,R0               ADD SHARE OF PENNIES                         
         ST    RF,0(R7)                                                         
GETSHR6  EQU   *                                                                
         LA    R7,4(R7)                                                         
         BCT   R6,GETSHR2                                                       
*                                                                               
*        REMAINDERS FOR PST                                                     
*                                                                               
GSHPST   DS    0H                                                               
*                                                                               
         LA    R2,10               10 PROVINCES                                 
         LA    R9,PSTAREA          ESTABLISH PSTAREA                            
         USING PSTAREA,R9                                                       
         LA    R8,PSTWORK          POINT TO PST WORKAREA                        
*                                                                               
GSHPSTLP DS    0H                                                               
*                                                                               
         LA    R6,PSTFLDNM         NUMBER OF PST FIELDS TO PROCESS              
*                                                                               
GSHPSTTL DS    0H                                                               
*                                                                               
         LR    R1,R6               CALCULATE INDEX                              
         BCTR  R1,0                                                             
         MH    R1,=H'3'            3 BYTES OF WORK FOR EACH FIELD               
         LA    R1,0(R1,R8)                                                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
*                                                                               
         LR    R7,R6               CALCULATE INDEX                              
         BCTR  R7,0                                                             
         MH    R7,=H'4'            4 BYTES FOR EACH FIELD                       
         AHI   R7,PSTFLDT-GETINS                                                
         AR    R7,RB               A(NEXT FIELD TO PROCESS)                     
*                                                                               
         L     R7,0(R7)            DISPLACEMENT OF NEXT FIELD                   
         LA    R7,0(R7,R9)         POINT TO NEXT FIELD                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RF,15,0(R7)         NEXT AMT                                     
         BZ    GSHPSTTC                                                         
         BP    *+10                                                             
         LCR   R0,R0                                                            
         L     RE,=F'-1'                                                        
*                                                                               
         DR    RE,R3               / WTSUM (NO ROUND)                           
         MR    RE,R4               X THIS SHARE                                 
         AR    RF,R0               ADD SHARE OF PENNIES                         
*                                                                               
         ST    RF,0(R7)                                                         
*                                                                               
GSHPSTTC DS    0H                                                               
*                                                                               
         BCT   R6,GSHPSTTL                                                      
*                                                                               
GSHPSTTD DS    0H                                                               
*                                                                               
GSHPSTCN DS    0H                                                               
*                                                                               
         LA    R9,PSTAREAL(R9)     NEXT PSTAREA                                 
         LA    R8,L'PSTWORK(R8)    NEXT PST WORKAREA                            
         BCT   R2,GSHPSTLP                                                      
*                                                                               
GSHPSTDN DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R9                                                               
*                                                                               
         EJECT                                                                  
NEXTEL   CLI   0(R5),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
NEXTEL2  DS    0H                                                               
         IC    R0,1(R5)                                                         
         LTR   R0,R0                                                            
         JNP   NEXTELX                                                          
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,R9                RETURN WITH CC =                             
         CLI   0(R5),0                                                          
         JNE   NEXTEL2                                                          
NEXTELX  LTR   R5,R5               SET CC TO NOT =                              
         BR    R9                                                               
         EJECT                                                                  
*                                  CALCULATE REMAINDER (AMT/WTSUM)              
*                                  STC REMAINDER IN WORK(R6) AND                
*                                  WORK+14(R6)                                  
REMCOMP  NTR1                                                                   
         LA    R6,14               ***** NOW 14                                 
         LA    R7,GROSS                                                         
REM2     EQU   *                                                                
         L     RF,0(R7)                                                         
         M     RE,=F'1'                                                         
         DR    RE,R3                                                            
         LPR   RE,RE                                                            
         STC   RE,WORK(R6)                                                      
         STC   RE,WORK+14(R6)                                                   
         LA    R7,4(R7)                                                         
         BCT   R6,REM2                                                          
*                                                                               
*        REMAINDERS FOR PST                                                     
*                                                                               
REMPST   DS    0H                                                               
*                                                                               
         LA    R2,10               10 PROVINCES                                 
         LA    R9,PSTAREA          ESTABLISH PSTAREA                            
         USING PSTAREA,R9                                                       
         LA    R8,PSTWORK          POINT TO PST WORKAREA                        
*                                                                               
REMPSTLP DS    0H                                                               
*                                                                               
         LA    R6,PSTFLDNM         NUMBER OF FIELDS TO PROCESS                  
*                                                                               
REMPSTTL DS    0H                                                               
*                                                                               
         LR    R7,R6               CALCULATE INDEX                              
         BCTR  R7,0                                                             
         MH    R7,=H'4'            4 BYTES FOR EACH FIELD                       
         AHI   R7,PSTFLDT-GETINS                                                
         AR    R7,RB               A(NEXT FIELD TO PROCESS)                     
*******  LA    R7,PSTFLDT(R7)      A(NEXT FIELD TO PROCESS)                     
*                                                                               
         L     R7,0(R7)            DISPLACEMENT OF NEXT FIELD                   
         LA    R7,0(R7,R9)         POINT TO NEXT FIELD                          
*                                                                               
         L     RF,0(R7)                                                         
         M     RE,=F'1'                                                         
         DR    RE,R3                                                            
         LPR   RE,RE                                                            
*                                                                               
         LR    R1,R6               CALCULATE INDEX                              
         BCTR  R1,0                                                             
         MH    R1,=H'3'            3 BYTES OF WORK FOR EACH FIELD               
*                                                                               
         STC   RE,0(R1,R8)                                                      
         STC   RE,1(R1,R8)         SINCE THERE ARE 2 PST FLDS                   
*                                                                               
REMPSTTC DS    0H                                                               
*                                                                               
         BCT   R6,REMPSTTL                                                      
*                                                                               
REMPSTTD DS    0H                                                               
*                                                                               
REMPSTCN DS    0H                                                               
*                                                                               
         LA    R9,PSTAREAL(R9)     NEXT PSTAREA                                 
         LA    R8,L'PSTWORK(R8)    NEXT PST WORKAREA                            
         BCT   R2,REMPSTLP                                                      
*                                                                               
REMPSTDN DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R9                                                               
*                                                                               
         EJECT                                                                  
*                                  DETERMINE PART OF REMAINDER FOR              
*                                  THIS SHARE                                   
*                                                                               
LEFTCOMP NTR1                                                                   
         MVC   WORK+29(14),WORK+1      SAVE CURRENT LEFTOVERS                   
         LA    R6,14                                                            
LEFT2    SR    R1,R1                                                            
         IC    R1,WORK+14(R6)      ORIG. REM                                    
         LTR   R1,R1                                                            
         BZ    LEFT4               NONE                                         
         SR    RF,RF                                                            
         IC    RF,WORK(R6)         LEFT OVER                                    
         LTR   RF,RF                                                            
         BZ    LEFT4               NONE LEFT                                    
         MR    R0,R4               X THIS SHARE                                 
         SLDL  R0,1                                                             
         DR    R0,R3               / WTSUM                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LTR   R1,R1                                                            
         BNZ   LEFT3                                                            
*                                  NEW CODE 4/23/91                             
         LTR   R4,R4               SEE IF ZERO SHARE                            
         BZ    LEFT3               YES - THEN NO PENNY                          
*                                                                               
         LA    R1,1                AT LEAST 1                                   
LEFT3    LR    R0,RF                                                            
         SR    RF,R1                                                            
         BNM   *+8                                                              
         SR    RF,RF                                                            
         LR    R1,R0                                                            
         STC   RF,WORK(R6)         SAVE LEFT                                    
         CLC   PPRCODE,PRDCODE     UNLESS THIS IS OUR PRODUCT                   
         BNE   LEFT4                                                            
         STC   R1,WORK(R6)         THEN SAVE THIS SHARE                         
LEFT4    EQU   *                                                                
         BCT   R6,LEFT2                                                         
*                                                                               
*        REMAINDERS FOR PST                                                     
*                                                                               
LFTPST   DS    0H                                                               
*                                                                               
         LA    R2,10               10 PROVINCES                                 
         LA    R9,PSTAREA          ESTABLISH PSTAREA                            
         USING PSTAREA,R9                                                       
         LA    R8,PSTWORK          POINT TO PST WORKAREA                        
*                                                                               
LFTPSTLP DS    0H                                                               
*                                                                               
         LA    R6,PSTFLDNM         DO PSTTAX & PSTTAXPD                         
*                                                                               
LFTPSTTL DS    0H                                                               
*                                                                               
         ST    R6,FULL             SAVE COUNTER                                 
         BCTR  R6,0                CALCULATE INDEX                              
         MH    R6,=H'3'            3 BYTES OF WORK FOR EACH FIELD               
         SR    R1,R1                                                            
*                                                                               
         IC    R1,0(R6,R8)         CURRENT LEFTOVER                             
         STC   R1,2(R6,R8)         SAVE                                         
*                                                                               
         IC    R1,1(R6,R8)         ORIG. REM                                    
*                                                                               
         LTR   R1,R1                                                            
         BZ    LFTPSTTC            NONE                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,0(R6,R8)         LEFT OVER                                    
         LTR   RF,RF                                                            
         BZ    LFTPSTTC            NONE LEFT                                    
*                                                                               
         MR    R0,R4               X THIS SHARE                                 
         SLDL  R0,1                                                             
         DR    R0,R3               / WTSUM                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LTR   R1,R1                                                            
         BNZ   LFTPSTT3                                                         
*                                  NEW CODE 4/23/91                             
         LTR   R4,R4               SEE IF ZERO SHARE                            
         BZ    LFTPSTT3            YES - THEN NO PENNY                          
*                                                                               
         LA    R1,1                AT LEAST 1                                   
LFTPSTT3 LR    R0,RF                                                            
         SR    RF,R1                                                            
         BNM   *+8                                                              
         SR    RF,RF                                                            
         LR    R1,R0                                                            
         STC   RF,0(R6,R8)         SAVE LEFT                                    
         CLC   PPRCODE,PRDCODE     UNLESS THIS IS OUR PRODUCT                   
         BNE   LFTPSTTC                                                         
         STC   R1,0(R6,R8)         THEN SAVE THIS SHARE                         
LFTPSTTC EQU   *                                                                
*                                                                               
         L     R6,FULL             RESTORE COUNTER                              
         BCT   R6,LFTPSTTL                                                      
*                                                                               
LFTPSTTD DS    0H                                                               
*                                                                               
LFTPSTCN DS    0H                                                               
*                                                                               
         LA    R9,PSTAREAL(R9)     NEXT PSTAREA                                 
         LA    R8,L'PSTWORK(R8)    NEXT PST WORKAREA                            
         BCT   R2,LFTPSTLP                                                      
*                                                                               
LFTPSTDN DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
         DROP  R9                                                               
*                                                                               
         EJECT                                                                  
FIXAC    EQU   *            ALTERS AGYCOM FROM BILL/PAY ELEM                    
*                           TO REFLECT PBDACP - FOR SPECIAL                     
*                           HANDLING OF 'C' RATE BUYS                           
         SR    R0,R0                                                            
         ZAP   DUB,PBDACP       AC=0.0                                          
         BZ    FIXACX5                                                          
         CVB   R0,DUB                                                           
         L     R1,WORK          GROSS FROM BILL/PAY ELEM                        
         L     RF,=F'100000'                                                    
         SR    RF,R0                                                            
         MR    RE,R1                                                            
         SLDL  RE,1                                                             
         D     RE,=F'100000'                                                    
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                RF=NET                                       
*                                                                               
FIXACX   DS    0H                                                               
         LR    R0,R1                                                            
         SR    R0,RF                                                            
FIXACX5  ST    R0,WORK+4          STORE 'FIXED' BILL/PAY AGYCOM                 
         BR    R9                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*        TABLE OF DISPLACEMENTS OF PST FIELDS                                   
*        THAT NEED TO HAVE PENNIES ALLOCATED WHEN COSTS ARE                     
*        ALLOCATED BETWEEN PRODUCTS                                             
*                                                                               
PSTFLDT  DS    0A                  TABLE OF PST FLDS FOR PENNY ALLOC            
         DC    AL4(PSTTAX-PSTAREA)   PST TAX PAYABLE                            
         DC    AL4(PSTTAXPD-PSTAREA) PST TAX PAID                               
         DC    AL4(PST$BS-PSTAREA)   PST TAX PAYABLE DOLLAR BASIS               
         DC    AL4(PST$BSPD-PSTAREA) PST TAX PAID    DOLLAR BASIS               
PSTFLDNM EQU   (*-PSTFLDT)/4       NUMBER OF FIELDS IN TABLE                    
*                                                                               
         TITLE 'GETINS - FIND PLANNED COST ELEMENT - GETPC'                     
***********************************************************************         
*                                                                     *         
*        FIND PLANNED COST ELEMENT                                    *         
*              IF FOUND SET WORK FIELDS WITH PC EQUIVALENTS           *         
*                                                                     *         
*              IF NOT FOUND, CONTINUE AS USUAL AND                    *         
*              SET C'X' IF RETURNED PARAMETER LIST                    *         
*                                                                     *         
*NTRY    RA==> BUY RECORD                                             *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
GETPC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    PCELMSV,PCELMSV     INIT PC ELM SAVEAREA                         
*                                                                               
         LA    R5,PBDELEM                                                       
         MVI   ELCODE,BYPCIDQ       SERCH FOR PC ELEMENT                        
         BRAS  R9,NEXTEL                                                        
         BNE   GPCNOTF              NOT FOUND -                                 
*                                                                               
         USING BYPCELD,R5          ESTABLISH PC ELM                             
*                                                                               
         MVC   GPBDCIND,BYPCIND    SET COST INDICATOR                           
         MVC   GPBDNIND,BYPCNIND   SET NET  INDICATOR                           
         MVC   GPBDCTYP,BYPCTYP    SET COST TYPE                                
         ZAP   GPBDCOS,BYPCCST     SET COST                                     
*                                                                               
         MVC   PCELMSV,BYPCELM     SAVE PC ELEMENT                              
*                                                                               
         CR    RB,RB               SET CC TO EQ                                 
*                                                                               
         B     GETPCX                                                           
*                                                                               
*        NO PC ELEMENT - RETURN C'X' IN PARAMETER LIST                          
*                                                                               
GPCNOTF  DS    0H                                                               
*                                                                               
         L     R1,APARMS           POINT TO PARAMETER LIST                      
         MVI   4(R1),C'X'          SET RETURN CODE                              
*                                                                               
         LTR   RB,RB               SET CC TO NEQ                                
*                                                                               
GETPCX   XIT1                      RETURN                                       
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'GETINS - CALCULATE GST AND PST - GETPST'                        
***********************************************************************         
*                                                                     *         
*        CALCULATE GST AND PST ON INDIVIDUAL PAY AND BILL ELEMENTS    *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
GETGST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WRKGST,WRKGST       INIT GST AMOUNT                              
*                                                                               
         TM    PBDCNDA,X'80'       SKIP IF NOT CANADIAN                         
         BZ    GETGSTX                                                          
*                                                                               
         L     RF,WORK         GROSS                                            
         S     RF,WORK+4       MINUS AGYCOM                                     
*                                                                               
         ST    RF,WRKNET           SAVE AS NET                                  
*                                                                               
         OC    PBDTAX,PBDTAX       SEE IF I HAVE TAX                            
         BZ    GETPG20                                                          
*                                                                               
*                          FOR TAX BUYS WORK INCLUDES TAX                       
*                                                                               
GETPG10G DS    0H          EXTRACT NET ELEMENT                                  
*                     RETURNS NET IN RF  FOR GETPG10G                           
         L     R0,WORK                 PPGROSS                                  
         S     R0,WORK+4              MINUS PPAGYCOM                            
         ST    R0,DUB                                                           
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),PBDTAX                                                 
         L     RF,FULL           RE = SALES TAX PCT                             
         SR    RE,RE                                                            
*                                                                               
         TM    GSTBASIS,X'01'     SEE IF SALES TAX IS ON NET                    
         BO    GETPG25                                                          
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),GSTPCT   GSTTAX PCT                                    
         L     R0,FULL                                                          
         A     R0,=F'100000'                                                    
         ST    R0,FULL                                                          
*                                                                               
         M     RE,FULL                                                          
         D     RE,=F'100000'                                                    
*                                                                               
GETPG25  A     RF,=F'1000000'                                                   
         ST    RF,FULL                                                          
*                                                                               
         L     RF,DUB                                                           
         M     RE,=F'1000000'                                                   
         SLDL  RE,1                                                             
         D     RE,FULL                                                          
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
*                                                                               
*                                   NOTE - RF HAS NET AT THIS POINT             
GETPG20  DS    0H                                                               
*                                                                               
         ST    RF,WRKNET           SAVE NET                                     
*                                                                               
         CLI   GETGSTSW,C'P'       SEE IF GETTING PAID GST                      
         BNE   GETPG5                                                           
*                                                                               
         TM    PBDCNDA,X'01'       SEE IF PAID WITH GST                         
         BZ    GETPGX              NO - DON'T CALCULATE GST PAID                
*                                                                               
GETPG5   DS    0H                                                               
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),GSTPCT                                                 
         L     RE,FULL             GST PERCENTAGE                               
*                                                                               
         MR    RE,RE                                                            
         SLDL  RE,1                                                             
         D     RE,=F'100000'       GST TAX HAS 3 DECIMALS                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,FULL             ROUNDED GST                                  
         ST    RF,WRKGST           SAVE GST                                     
*                                                                               
GETPGX   DS    0H                                                               
*                                                                               
*        CALCULATE PST IF NEEDED                                                
*                                                                               
GETPST   DS    0H                                                               
*                                                                               
         CLI   PSTSW,C'P'          SKIP IF PST NOT NEEDED                       
         BNE   GETPSTX                                                          
*                                                                               
         CLI   GETGSTSW,C'P'       SEE IF GETTING PAID GST                      
         BNE   GETPST01                                                         
*                                                                               
         TM    PBDCNDA,X'02'       SEE IF PAID WITH PST                         
         BZ    GETPSTX             NO - DON'T CALCULATE PST PAID                
*                                                                               
GETPST01 DS    0H                                                               
*                                                                               
         LA    R9,PSTAREA          ESTABLISH PSTAREA                            
         USING PSTAREA,R9                                                       
*                                                                               
         LA    R0,10               10 PROVINCES                                 
*                                                                               
GETPSTLP DS    0H                                                               
*                                                                               
         OC    PSTPROV,PSTPROV     SKIP IF NO MORE PROVINCES WITH PST           
         BZ    GETPSTDN                                                         
*                                                                               
         L     RF,WRKNET           COPY NET                                     
*                                                                               
         TM    PSTBASIS,X'01'      SKIP PST ON NET ONLY                         
         BO    *+8                                                              
         A     RF,WRKGST           NET + GSTTAX                                 
*                                                                               
         ST    RF,WRK$BS           SAVE PST DOLLAR BASIS                        
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,PSTPCT         GET PST PERCENT                              
         MR    RE,RE               CALCULATE PST TAX                            
*                                                                               
         SLDL  RE,1                DOUBLE RESULT                                
         D     RE,=F'100000'       PER CENT HAS 3 DECIMALS                      
         LTR   RF,RF               CHECK FOR NEGATIVE NUMBER                    
         BM    *+8                                                              
         AH    RF,=H'1'            PREPARE FOR ROUNDING                         
         SRA   RF,1                ROUND                                        
*                                                                               
         CLI   GETGSTSW,C'P'       IF LOOKING FOR PAID PST                      
         BNE   GETPST40                                                         
*                                                                               
         L     RE,PST$BSPD            ACCUMULATE PAID PST $ BASIS               
         A     RE,WRK$BS                                                        
         ST    RE,PST$BSPD                                                      
*                                                                               
         L     RE,PSTTAXPD                                                      
         AR    RE,RF                  ACCUMULATE PAID PST                       
         ST    RE,PSTTAXPD                                                      
         B     GETPST50                                                         
*                                                                               
GETPST40 DS    0H                                                               
*                                                                               
         CLI   GETGSTSW,C'B'       IF LOOKING FOR BILLED PST                    
         BNE   GETPST50                                                         
*                                                                               
         L     RE,PST$BSBL            ACCUMULATE BILLED PST $ BASIS             
         A     RE,WRK$BS                                                        
         ST    RE,PST$BSBL                                                      
*                                                                               
         L     RE,PSTTAXBL                                                      
         AR    RE,RF                  ACCUMULATE BILLED PST                     
         ST    RE,PSTTAXBL                                                      
*                                                                               
GETPST50 DS    0H                                                               
*                                                                               
GETPSTCN DS    0H                                                               
*                                                                               
         LA    R9,PSTAREAL(R9)     BUMP TO NEXT PSTAREA                         
         BCT   R0,GETPSTLP                                                      
*                                                                               
GETPSTDN DS    0H                                                               
*                                                                               
GETPSTX  DS    0H                                                               
*                                                                               
         DROP  R9                                                               
*                                                                               
         MVC   FULL,WRKGST         RESTORE GST                                  
*                                                                               
GETPG30  DS    0H                                                               
*                                                                               
         CLI   GETGSTSW,C'P'                                                    
         BNE   GETPG40                                                          
*                                                                               
         L     RE,GSTTAXPD         ACCUMLATE GST PAID                           
         A     RE,FULL                                                          
         ST    RE,GSTTAXPD                                                      
         B     GETGSTX                                                          
*                                                                               
GETPG40  CLI   GETGSTSW,C'B'                                                    
         BNE   GETPSTX                                                          
         L     RE,GSTTAXBL         ACCUMLATE GST BILLED                         
         A     RE,FULL                                                          
         ST    RE,GSTTAXBL                                                      
         B     GETGSTX                                                          
GETGSTX  XIT1                      RETURN                                       
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'GETINS - ROUND COST2 AMOUNT - COS2RND'                          
***********************************************************************         
*                                                                     *         
*        COS2RND - ROUND COST2 AMOUNT                                 *         
*                                                                     *         
*NTRY    P16   GROSS                                                  *         
*        R5==> PCOS2FEL                                               *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
COS2RND  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CP    P16,=P'0'           IF POSITIVE RATE                             
         BL    COS2A                                                            
*                                                                               
         CP    P16,=P'2000'           SKIP IF LESS THAN $20                     
         BNH   COS2RNDX                                                         
*                                                                               
         B     COS2B                                                            
*                                                                               
COS2A    DS    0H                  ELSE IF NEGATIVE RATE                        
*                                                                               
         CP    P16,=P'-2000'          SKIP IF GREATER THAN -$20                 
         BNL   COS2RNDX                                                         
*                                                                               
COS2B    DS    0H                                                               
*                                                                               
         USING PCOS2FEL,R5         ESTABLISH COST2 FACTOR ELM                   
*                                                                               
         MP    P16,PCOS2FAC        MULTIPLY BY COST2 FACTOR                     
         MP    P16,=P'2'           DOUBLE FOR ROUNDING TO $5                    
         SRP   P16,64-9,5          ROUND TO NEAREST $10                         
         SRP   P16,3,0             *1000 TO GET PENNIES                         
         DP    P16,=P'2'           UNDO DOUBLING TO GET NEAREST $5              
         ZAP   P16,P16(L'P16-1)                                                 
*                                                                               
COS2RNDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
         TITLE 'GETINS - FIND GST/PST CODES/RATES - GETCAN'                     
***********************************************************************         
*                                                                     *         
*        FOR CANADIAN BUYS FIND GST CODE, PERCENT AND BASIS           *         
*        ALSO SIMILAR INFORMATION FOR EACH PROVINCE WITH PST          *         
*                                                                     *         
*        FOR ORDERED AMOUNTS                                          *         
*           REFERENCE DATE IS THE INSERTION DATE                      *         
*                          OR THE FIRST PAID DATE IF EARLIER          *         
*                                                                     *         
*        FOR BILLED AMOUNTS                                           *         
*           REFERENCE DATE IS THE BILLED    DATE                      *         
*                                                                     *         
*        FOR PAID   AMOUNTS                                           *         
*           REFERENCE DATE IS THE PAID      DATE                      *         
*                                                                     *         
*        STARTING JULY 2010                                           *         
*                                                                     *         
*        FOR BILLED AMOUNTS                                           *         
*           REFERENCE DATE IS THE BILLABLE  DATE                      *         
*                                                                     *         
*        FOR PAID   AMOUNTS                                           *         
*           REFERENCE DATE IS THE INSERTION DATE                      *         
*                                                                     *         
*NTRY    RA==> BUY RECORD                                             *         
*        PARM0 A(PAY/BILL ELEMENT)                                    *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETCAN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING PBUYREC,RA          ESTABLISH BUY RECORD                         
*                                                                               
         L     R6,0(R1)            POINT TO PASSED BILL/PAY ELEMENT             
         LA    R6,0(R6)            CLEAR HIGH ORDER BYTE                        
*                                                                               
         TM    PBDCNDA,X'80'       SKIP IF NOT CANADIAN BUY                     
         BZ    GETCANX                                                          
*                                                                               
         MVC   FULL(1),PBDGST      TAKE GST CODE FROM BUY                       
*                                                                               
         CLI   FULL,0              IF CODE NOT ENTERED IN BUY                   
         BNE   *+8                                                              
         MVI   FULL,C'S'              DEFAULT TO STANDARD                       
*                                                                               
*---------------------------> VERIFY GST CODE TO TABLE                          
*                                                                               
*        GST/PST DETERMINED BY INSERT DATE OR PAID OR BILLED DATE               
*           OF PASSED ELEMENT                                                   
*                                                                               
         MVC   WGSTDATE,PBUYKDAT   INIT GST DATE WITH INSERT DATE               
         MVI   WPSTIND,0           INIT PAID BUY INDICATOR FOR PST              
*                                                                               
         TM    PBUYCNTL,X'80'      IF DELETED BUY                               
         BO    GTCDTEX                USE INSERTION DATE FOR GST SEARCH         
*                                                                               
         LTR   R6,R6               IF NO ELEMENT PASSED                         
         BNZ   GTCDTE10               SEE IF THERE IS A PAY ELEMENT             
*                                                                               
         B     GTCDTEX             ALWAYS USE INSERTION DATE                    
*                                                                               
         USING ELEM,R6             ESTABLISH AS PAY ELEMENT                     
*                                                                               
         SR    R0,R0                                                            
*                                                                               
         L     R6,PARS             GET A(BUYREC)                                
         LA    R6,33(R6)           POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
GTCDTELP DS    0H                                                               
*                                                                               
         CLI   0(R6),0             DONE AT END OF RECORD                        
         BE    GTCDTEDN                                                         
*                                                                               
         CLI   0(R6),X'25'         WANT PAY ELEMENT                             
         BNE   GTCDTECN                                                         
         OC    PPDDATE,PPDDATE     THAT HAS BEEN PAID                           
         BZ    GTCDTECN                                                         
*                                                                               
         CLI   1(R5),PPACCODE-PPAYELEM IF NO ROOM FOR ADDITIONAL CHGS           
         BNH   GTCDTE04                   ASSUME ELM NOT FOR ADD CHG            
*                                                                               
         TM    PPDSTAT,X'01'       IF SECONDARY SEQ # PRESENT                   
         BNO   GTCDTE05                                                         
*                                                                               
         CLI   1(R5),PPSEQN2-PPAYELEM    NEW LENGTH TO BE CHECKED               
         BH    GTCDTE05            THERE IS AN ADDITIONAL CHARGE                
*                                                                               
GTCDTE04 DS    0H                                                               
*                                                                               
         TM    ACSWITCH,ACSWALLQ+ACSWONEQ  AND ONLY ADDL CHGS WANTED            
         BNZ   GTCDTECN            IGNORE                                       
*                                                                               
         B     GTCDTE07               ELSE ACCEPT                               
*                                                                               
GTCDTE05 DS    0H                                                               
*                                                                               
         TM    ACSWITCH,ACSWNONQ   SKIP IF ADDL CHGS NOT WANTED                 
         BO    GTCDTECN                                                         
*                                                                               
         TM    ACSWITCH,ACSWONEQ   IF ONE ADDITIONAL CHARGE WANTED              
         BNO   *+14                                                             
         CLC   ACID,PPACCODE          IGNORE IF FOR WRONG ADDL CHG              
         BNE   GTCDTECN                                                         
*                                                                               
         CLC   =C'H7',WRKAGY       SKIP IF NOT GROUP M                          
         BE    *+10                                                             
         CLC   =C'SJ',WRKAGY       AND  IF NOT SJR                              
         BE    *+10                                                             
         CLC   =C'HD',WRKAGY       AND  IF NOT HDTO                             
         BE    *+10                                                             
         CLC   =C'*B',WRKAGY       AND  IF NOT DDSB                             
         BNE   *+22                                                             
         TM    ACSWITCH,ACSWONEQ   SKIP IF ONE ADDITIONAL CHARGE WANTED         
         BO    *+14                                                             
         CLC   =C'TC',PPACCODE     IGNORE TC ADDITIONAL CHARGE                  
         BE    GTCDTECN                                                         
*                                                                               
         TM    ACSWITCH,ACSWNFXQ   IF EXCLUDING FX CHARGE                       
         BNO   *+14                                                             
         CLC   =C'FX',PPACCODE        IGNORE IF FX ELEMENT                      
         BE    GTCDTECN                                                         
*                                                                               
GTCDTE07 DS    0H                                                               
*                                                                               
         MVI   WPSTIND,C'P'        INDICATE PAID BUY                            
*                                                                               
         MVC   WGSTDATE,PPDDATE    SET TO USE PAID DATE                         
*                                                                               
         CLC   WGSTDATE,=AL1(94,6,1)  IF PAID JUNE 1,1994                       
         BNE   *+10                                                             
         MVC   WGSTDATE,=AL1(94,5,31)    USE MAY 31,1994 AS DATE                
*                                                                               
         CLC   PBUYKDAT,WGSTDATE   IF INSERT DATE PRIOR TO PAY DATE             
         BNL   *+10                                                             
         MVC   WGSTDATE,PBUYKDAT      USE IT FOR TABLE SEARCH                   
         B     GTCDTEDN                                                         
*                                                                               
GTCDTECN DS    0H                                                               
*                                                                               
         IC    R0,1(R6)            GET ELEMENT LENGTH                           
         AR    R6,R0               POINT TO NEXT ELEMENT                        
         B     GTCDTELP                                                         
*                                                                               
GTCDTEDN DS    0H                                                               
*                                                                               
         B     GTCDTEX                                                          
*                                                                               
GTCDTE10 DS    0H                                                               
*                                                                               
         CLI   0(R6),X'25'         IF PAID ELEMENT PASSED                       
         BNE   GTCDTE20                                                         
*                                                                               
         B     GTCDTEX             ALWAYS USE INSERTION DATE                    
*                                                                               
         MVC   WGSTDATE,PPDDATE       USE PAID DATE                             
*                                                                               
         B     GTCDTEX                                                          
*                                                                               
GTCDTE20 DS    0H                                                               
*                                                                               
         CLI   0(R6),X'26'         IF BILL ELEMENT PASSED                       
         BNE   GTCDTE30                                                         
*                                                                               
         MVC   WGSTDATE,PBDBDATE      USE BILLABLE DATE                         
*                                                                               
         B     GTCDTEX                                                          
*                                                                               
*        OLD CODE BYPASSED                                                      
*                                                                               
         CLC   WGSTDATE,=AL1(110,07,01) IF INSERTION DATE AFTER JUL1/10         
         BL    *+14                                                             
         MVC   WGSTDATE,PBDBDATE      USE BILLABLE DATE                         
         B     GTCDTEX                                                          
*                                                                               
         MVC   WGSTDATE,PBLDATE       ELSE USE BILLED DATE                      
*                                                                               
         B     GTCDTEX                                                          
*                                                                               
GTCDTE30 DS    0H                  UNKONWN OPTION - USE DEFAULT                 
*                                                                               
GTCDTEX  DS    0H                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
*        TABLE IS CL1   GST CODE                                                
*                 XL1   SALES TAX BASIS X'00' NET+GST                           
*                                       X'01' NET                               
*                 XL2   GST PERCENT TO 3 DECIMALS                               
*                 XL3   START DATE BINARY                                       
*                 XL1   SPARE                                                   
*                                                                               
         LA    R5,PGSTTAB                                                       
*                                                                               
GTCGSTLP DS    0H                                                               
*                                                                               
         CLC   0(1,R5),FULL        FIND GST CODE IN TABLE                       
         BNE   GTCGSTCN                                                         
*                                                                               
         CLC   WGSTDATE,4(R5)      FIND STARTING DATE                           
         BNL   GTCGSTFD                                                         
*                                                                               
GTCGSTCN DS    0H                                                               
*                                                                               
         LA    R5,8(R5)            BUMP TO NEXT TABLE ENTRY                     
         CLI   0(R5),X'FF'         IF NOT END OF TABLE                          
         BNE   GTCGSTLP               GO CHECK NEXT TABLE ENTRY                 
*                                                                               
         DC    H'0'                BAD GST CODE                                 
*                                                                               
GTCGSTFD DS    0H                                                               
*                                                                               
         CLC   2(2,R5),=X'0000'   SKIP IF NO GST TAX PCT                        
         BE    GTCGSTX                                                          
*                                                                               
         MVC   GSTBASIS,1(R5)      SAVE BASIS BYTE                              
         MVC   GSTCODE(1),0(R5)    SAVE CODE                                    
         MVC   GSTPCT,2(R5)        SAVE PERCENT                                 
*                                                                               
GTCGSTX  DS    0H                                                               
*                                                                               
*        INIT PST AREA                                                          
*                                                                               
GTCPST   DS    0H                                                               
*                                                                               
         LA    R9,PSTAREA          ESTABLISH PSTAREA                            
         USING PSTAREA,R9                                                       
*                                                                               
         L     R6,PARS             GET A(BUYREC)                                
         LA    R6,33(R6)           POINT TO FIRST ELEMENT IN RECORD             
         SR    R0,R0                                                            
*                                                                               
GTCPSTLP DS    0H                                                               
*                                                                               
         CLI   0(R6),0             EOR - NO PST ELEMENT                         
         BE    GTCPSTX                SKIP PST CALCULATIONS                     
*                                                                               
         CLI   0(R6),PBYPSTQ       LOOKING FOR BUY PST ELEMENT                  
         BE    GTCPSTFD                                                         
*                                                                               
GTCPSTCN DS    0H                                                               
*                                                                               
         IC    R0,1(R6)            GET ELEMENT LENGTH                           
         AR    R6,R0               POINT TO NEXT ELEMENT                        
         B     GTCPSTLP                                                         
*                                                                               
GTCPSTFD DS    0H                                                               
*                                                                               
         MVC   WRKPSTC,PBYPSTC-PBYPSTEL(R6)   SAVE PST CODES                    
*                                                                               
*        FIND PROVINCE WITH PST CODE                                            
*                                                                               
         LA    R7,1                PROVINCE COUNTER                             
         LA    R6,WRKPSTC          PST CODES BY PROVINCE                        
*                                                                               
GTCPSTPL DS    0H                                                               
*                                                                               
         CLI   0(R6),0             SKIP IF NO PST CODE PRESENT                  
         BE    GTCPSTPC                                                         
*                                                                               
*        FIND PROVINCIAL CODE                                                   
*                                                                               
         LR    R1,R7               COPY PROVINCE COUNTER                        
         BCTR  R1,0                DECREMENT FOR INDEXING                       
         SLL   R1,1                EACH ENTRY IS 2 BYTES                        
         LA    R1,PROVTAB(R1)      POINT TO PROVINCE CODE                       
*                                                                               
*        FIND PST TABLE ENTRY FOR PROVINCE AND PST CODE                         
*                                                                               
GTCPSTT  DS    0H                                                               
*                                                                               
         LA    R8,PPSTTAB          POINT TO PST TABLE                           
         LA    RE,PPSTTABN         NUMBER OF ENTRIES IN TABLE                   
*                                                                               
GTCPSTTL DS    0H                                                               
*                                                                               
         CLC   0(2,R8),0(R1)       MATCH ON PROVINCE CODE                       
         BE    *+10                                                             
         CLC   0(2,R8),=C'ZZ'      OR DEFAULT CODE                              
         BNE   GTCPSTTC                                                         
*                                                                               
         CLC   2(1,R8),0(R6)       MATCH ON PST CODE                            
         BNE   GTCPSTTC                                                         
*                                                                               
         CLC   WGSTDATE,5(R8)      PST/GST MUST BE AFTER INIT DATE              
         BL    GTCPSTTC                                                         
*                                                                               
         B     GTCPSTTF            TABLE ENTRY FOUND                            
*                                                                               
GTCPSTTC DS    0H                                                               
*                                                                               
         LA    R8,PPSTTABL(R8)     NEXT TABLE ENTRY                             
         BCT   RE,GTCPSTTL                                                      
*                                                                               
         B     GTCPSTTD            SKIP PST SEARCH                              
*                                                                               
GTCPSTTF DS    0H                  GOT PST CODE ENTRY                           
*                                                                               
         MVC   PSTPROV,0(R1)       SET PROVINCE CODE                            
         MVC   PSTCODE,0(R6)       SET PST CODE                                 
*                                                                               
         CLC   PSTPROV,=C'BC'      DOING BRITISH COLUMBIA?                      
         BNE   GTCPSTTG                                                         
         CLC   WGSTDATE,=X'710401' DATE BEFORE APR1/2013                        
         BL    GTCPSTTG                                                         
         B     GTCPSTTH            DON'T CLEAR GST %                            
*                                                                               
GTCPSTTG CLI   PSTCODE,C'H'        SEE IF HST CODE FOUND                        
         BNE   *+10                                                             
         XC    GSTPCT,GSTPCT       CLEAR GST PERCENTAGE                         
*                                  SO GST WILL ALWAYS BE ZERO                   
*                                                                               
GTCPSTTH MVC   PSTPCT,3(R8)        SET PST PER CENT                             
         MVC   PSTBASIS,8(R8)      SET PST BASIS                                
*                                                                               
         LA    R9,PSTAREAL(R9)     BUMP TO NEXT PST AREA                        
*                                                                               
GTCPSTTD DS    0H                                                               
*                                                                               
GTCPSTPC DS    0H                                                               
*                                                                               
         LA    R6,1(R6)            BUMP CODE POINTER                            
         LA    R7,1(R7)            BUMP PROVINCE COUNTER                        
         CH    R7,=H'10'           MAX 10 PROVINCES                             
         BNH   GTCPSTPL                                                         
*                                                                               
GTCPSTPD DS    0H                                                               
*                                                                               
GTCPSTX  DS    0H                                                               
*                                                                               
GETCANX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R9                                                               
*                                                                               
       ++INCLUDE PGSTTAB                                                        
*                                                                               
         TITLE 'GETINS - DSECTS AND INCLUDES'                                   
*                                                                               
GETWORK  DSECT                                                                  
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
NET      DS    F                                                                
PARS     DS    5A                                                               
ACOPTA   DS    A                   A(SPECIAL CHARGES OPTION)                    
APARMS   DS    A                   A(PARAMETER LIST)                            
*                                                                               
ACSWITCH DS    XL1                 ADDITIONAL CHARGES SWITCH                    
ACSWALLQ EQU   X'80'               ALL ADDITIONAL CHARGES TOGETHER              
ACSWONEQ EQU   X'40'               SINGLE ADDITIONAL CHARGE - SEE ACID          
ACSWNONQ EQU   X'20'               EXCLUDE ADDITIONAL CHARGES                   
ACSWNFXQ EQU   X'10'               EXCLUDE FOREIGN EXCHANGE CHARGES             
POSWONEQ EQU   X'01'               SINGLE PO# - SEE POSEQ#                      
*                                                                               
ACID     DS    XL2                 'XX' ADDITIONAL CHARGE                       
         DS    XL1                 SPARE                                        
POSEQ#   DS    XL2                 PO SEQ #  FILTERING                          
*                                                                               
SAVE2W   DS    2F                                                               
DMCB     DS    6F                  PARAMETER AREA                               
P16      DS    PL16                                                             
ELCODE   DS    C                                                                
GETPASS  DS    C                                                                
PRDCODE  DS    CL3                                                              
GPBDCIND DS    CL1                 COST INDICATOR                               
GPBDNIND DS    CL1                 NET INDICATOR                                
GPBDCTYP DS    CL1                 COST TYPE                                    
GPBDCOS  DS    PL5                 COST OF BUY                                  
STAX     DS    F                PAYABLE SALES TAX                               
BILELCOD DS    CL1                                                              
SAVEAC   DS    PL3               SAVED AND RESTORED TO PBDACP                   
CRSW     DS    CL1              'C' FOR SPECIAL 'C' RATE HANDLING               
GSTSW    DS    CL1              'G' IF FINDING GST                              
PSTSW    DS    CL1              'P' IF FINDING GST                              
GETGSTSW DS    CL1              C'P'= PAID GST, C'B'= BILLED GST                
*                               GETGSTSW IS FOR GSTGST ROUTINE                  
*                                                                               
COUNTYN  DS    CL1                                                              
WGSTDATE DS    XL3                 DATE USED FOR PST/GST TABLE                  
WPSTIND  DS    CL1                 C'P' BUY HAS BEEN PAID                       
*                                                                               
PCELMSV  DS    XL128               SAVEAREA FOR PLANNED COST ELEMENT            
*                                                                               
*        THESE FIELDS BELOW ARE RETURNED TO CALLER                              
*        IF PARAMETER 5 IS POINTING TO =C'COUNT' OR =C'BOTH'                    
WORK     DS    15F                                                              
WRKNET   DS    F                   WORKING NET                                  
WRKGST   DS    F                   WORKING GST                                  
WRKPST   DS    F                   WORKING PST                                  
WRK$BS   DS    F                   WORKING PST $ BASIS                          
WRKPSTC  DS    XL(L'PBYPSTC)       WORKING PST CODES FROM BUY                   
*                                                                               
WRKAGY   DS    CL2                 AGENCY ALPHA FROM BUY                        
*                                                                               
PSTWORK  DS    XL(PSTFLDNM*3)      3 ITEMS FOR EACH PST ITEM                    
*                                  'PSTFLDNM' PST ITEMS PROCESSED               
         ORG   PSTWORK                                                          
         DS    10XL(L'PSTWORK)     10 PROVINCES                                 
*                                                                               
       ++INCLUDE GVALUES                                                        
GETWORKL EQU   *-GETWORK           WORKAREA LENGTH                              
*                                                                               
         EJECT                                                                  
PVALUESD DSECT                                                                  
       ++INCLUDE PVALUES                                                        
         EJECT                                                                  
REC      DSECT                                                                  
       ++INCLUDE PBUYREC                                                        
         EJECT                                                                  
       ++INCLUDE PBDELEM                                                        
         EJECT                                                                  
ELEM     DSECT                                                                  
*                                                                               
       ++INCLUDE PPAYELEM                                                       
         SPACE 2                                                                
         ORG   ELEM                                                             
       ++INCLUDE PBILELEM                                                       
         ORG   ELEM                                                             
       ++INCLUDE PPRELEM                                                        
         ORG   ELEM                                                             
       ++INCLUDE PACELEM                                                        
         ORG   ELEM                                                             
       ++INCLUDE PCOS2FACEL                                                     
*                                                                               
PBYPSTD  DSECT PST ELEMENT                                                      
PBYPSTQ  EQU   X'84'               PST ELEMENT ID                               
       ++INCLUDE PBYPSTEL                                                       
         EJECT                                                                  
       ++INCLUDE PPGENBYPC                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028GETINS    06/13/16'                                      
         END                                                                    
