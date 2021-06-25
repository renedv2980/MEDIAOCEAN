*          DATA SET SPWRI1B    AT LEVEL 156 AS OF 10/10/16                      
*PHASE T2041BA                                                                  
*INCLUDE SPFMTINO                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT'                        
***********************************************************************         
*                                                                     *         
*        PRODUCES THE CASHFLOW REPORT                                 *         
*        SCREEN IS VALIDATED IN SPWRI13                               *         
*                                                                     *         
* AKAT 09/15/15 - DO NOT TEST ENTIRE BILLING KEY AS IT CAN BREAK                
*               - BILLING INV$ REPORTING ACROSS CANADIAN NETWORKS               
* AKAT 05/22/15 - NEW WR2 PROFILE OPTION TO REPORT VENDOR CHK DATE    *         
* AKAT 03/24/15 - MOFLOW SUPPORT                                      *         
* AKAT 01/28/15 - PROTECT AGAINST CLT APPLIED % THAT WOULD BLOW CVB   *         
* AKAT 04/07/14 - DO NOT TEST ENTIRE BILLING KEY FOR AGENCY OU AS IT  *         
*               - CAN BREAK BILLING INV$ REPORTING ACROSS NETWORKS    *         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ---------- -------- ------------------------------------------ *         
* AKAT SPEC-6073  10/10/16 SUPPORT NEW CLDT SPECIAL                   *         
***********************************************************************         
         SPACE 2                                                                
T2041B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2041B,RA,RR=R2                                                
*                                                                               
         L     R6,4(R1)            ESTABLISH WORKING STORAGE                    
         USING WORKD,R6                                                         
*                                                                               
         ST    R2,RELO                                                          
*                                                                               
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
*                                                                               
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    START                                                            
         CLI   RPMODE,RPINPUT      INPUT                                        
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         CLI   RPMODE,RPFINAL      FINAL HOOK                                   
         BE    FIN                                                              
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - START'                
***********************************************************************         
*                                                                     *         
*        START OF REPORT PROCESSING                                   *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
START    DS    0H                                                               
*                                                                               
         BRAS  RE,INIT             INITIALIZATION                               
*                                                                               
         BRAS  RE,CSHINIT          CASH FLOW INIALIZATION                       
*                                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - INPUT'                
***********************************************************************         
*                                                                     *         
*        INPUT ROUTINE                                                *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
INPUT    L     R4,AGLOBAL          ** INPUT ROUTINE **                          
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCSP                                                  
         BE    PROCBUY                                                          
         B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - PROCBUY'              
***********************************************************************         
*                                                                     *         
*        PROCESS BUY RECORD                                           *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
PROCBUY  DS    0H                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - DRHOOK'               
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK                                                  *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLINIT       INITIALIZATION                               
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLINCOMP     INTERNAL COMPUTES                            
         BE    INTCOMP                                                          
         CLI   GLHOOK,GLPUTSRT     PUTTING RECORD TO SORT                       
         BE    PUTSRT                                                           
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
         CLI   GLHOOK,GLFIRST      FIRSTS                                       
         BE    FIRSTS                                                           
         CLI   GLHOOK,GLLAST       LASTS                                        
         BE    LASTS                                                            
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEAD                                                             
*                                                                               
DRHOOKX  B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - DRVINIT'              
***********************************************************************         
*                                                                     *         
*        DRIVER INITIALIZATION                                        *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
DRVINIT  MVC   GLOPTS+2(1),BILLOPT     SET BILLING OPTION                       
*                                                                               
         CLI   BILLOPT,C'Y'        IF NO BILL OPTION SET                        
         BE    *+8                                                              
         CLI   BILLOPT,C'1'                                                     
         BE    *+8                                                              
         CLI   BILLOPT,C'2'                                                     
         BE    *+16                                                             
         TM    CASHOPT,X'80'       BUT CASH OPTION SET                          
         BNO   *+8                                                              
         MVI   GLOPTS+2,C'Y'        SET FOR NORMAL BILL INVOICE NUMBER          
*                                                                               
         MVC   GLOPTS+4(1),DOLLAR      SET GROSS/NET                            
*                                                                               
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         MVI   GLOPTS+5,C'Y'           SET REPORT WIDTH (Y=WIDE)                
*                                                                               
         MVC   GLOPTS+6(1),DAYS        SET DAYS OPTION                          
         MVC   GLOPTS+7(1),SPOTOPT     SHOW SPOTS OPTION                        
         MVC   GLOPTS+8(1),ORDERED     ORDERED                                  
*                                                                               
         CLI   BILLOPT,C'Y'        IF NOT SHOWING INVOICE DATA                  
         BE    *+8                                                              
         CLI   BILLOPT,C'1'                                                     
         BE    *+8                                                              
         CLI   BILLOPT,C'2'                                                     
         BE    *+16                                                             
         CLI   CASHOPT,X'00'       IF NOT SHOWING INVOICE DATA                  
         BNE   *+8                                                              
         NI    BLNK,X'FF'-X'03'       KILL INVOICE DUE/RUN DATE OPT             
*                                                                               
         TM    BLNK,X'04'          IF NO DUE DATE TO BE REPORTED                
         BNO   *+8                                                              
         NI    BLNK,X'FF'-X'07'       KILL INVOICE DUE/RUN DATE OPT             
*                                                                               
         MVC   GLOPTS+9(1),BLNK        LEAVE A BLANK LINE                       
*                                                                               
         MVC   GLOPTS+10(1),CALC                                                
*                                                                               
         CLI   PSUBT,C'Y'          IF SUB TOTALS NEEDED                         
         BNE   *+8                                                              
         OI    GLOPTS+11,X'80'                                                  
*                                                                               
         CLI   VENDOPT,C'V'        IF VENDOR OPTION                             
         BNE   *+8                                                              
         OI    GLOPTS+11,X'40'                                                  
*                                                                               
         CLI   NOVDRDTE,C'Y'       IF NO VENDOR DATE OPTION                     
         BNE   *+8                                                              
         OI    GLOPTS+11,X'20'                                                  
*                                                                               
         MVC   GLOPTS+12(1),CASHOPT    INCLUDE CASH                             
*                                                                               
         TM    CASHOPT,X'80'       IF REPORTING CLIENT CASH                     
         BNO   DRVIN12X                                                         
*                                                                               
         CLI   XPCTOPT,C'Y'           IF DROPPING CASH PERCENT                  
         BNE   *+8                                                              
         OI    GLOPTS+12,X'40'           SET SPECIAL OPTION                     
*                                                                               
         CLI   BANKOPT,C'Y'           IF BANK DEPOSIT DATE                      
         BNE   *+8                                                              
         OI    GLOPTS+12,X'20'           SET SPECIAL OPTION                     
*                                                                               
DRVIN12X DS    0H                                                               
*                                                                               
*                                                                               
         MVC   GLOPTS+13(1),INVDTOPT   INCLUDE INVOICE DATE                     
*                                                                               
         TM    DOWNOPT,GLDLTOTS                                                 
         BNO   *+8                                                              
         MVI   GLOPTS+14,C'T'          DOWN LOADING TOTALS                      
*                                                                               
         MVI   GLOPTS+15,0         INIT OPTION                                  
*                                                                               
         CLI   CHKSOPT,C'Y'        SUPPRESS PAY DATA                            
         BNE   *+8                                                              
         OI    GLOPTS+15,X'80'                                                  
*                                                                               
         CLI   CHKSOPT,C'0'        IF PAYPEND                                   
         BNE   *+8                                                              
         OI    GLOPTS+15,X'40'                                                  
*                                                                               
         TM    GLOPTS+15,X'80'     IF SHOWING CHECK DATA                        
         BO    *+16                                                             
         CLI   CHKIVOPT,C'Y'       AND SHOWING CHK INVOICE                      
         BNE   *+8                                                              
         OI    GLOPTS+15,X'20'                                                  
*                                                                               
         TM    GLOPTS+15,X'80'     IF SHOWING CHECK DATA                        
         BO    *+16                                                             
         CLI   CHKRPOPT,C'Y'       AND SHOWING CHK REP                          
         BNE   *+8                                                              
         OI    GLOPTS+15,X'10'                                                  
*                                                                               
         TM    GLOPTS+15,X'80'     SHOWING CHECK DATA?                          
         BO    *+16                NO                                           
         CLI   CHKDTOPT,C'Y'       SHOWING CHECK DATE (CLDT)?                   
         BNE   *+8                 NO                                           
         OI    GLOPTS+17,X'80'     YES                                          
*                                                                               
         TM    GLOPTS+15,X'80'     IF SHOWING CHECK DATA                        
         BO    *+16                                                             
         CLI   VDRBNKDT,C'Y'       AND SHOWING CHK BANK DATE                    
         BNE   *+8                                                              
         OI    GLOPTS+3,X'80'                                                   
*                                                                               
         CLI   MOFLOW,C'Y'         MONEYFLOW OPTION?                            
         BNE   *+8                 NO                                           
         OI    GLOPTS+16,X'80'     YES                                          
*                                                                               
         CLI   DOWNOPT,0           IF DOWNLOADING                               
         BZ    *+8                                                              
         OI    GLDWNLD2,GLDADDCH      KEEP CHARACTER FIELDS CHARACTER           
*                                                                               
         CLI   PSUBT,C'N'                                                       
         BE    *+8                                                              
         OI    GLINDS,GLPALTOT         PRINT ALL TOTALS                         
*                                                                               
         OI    GLINDS,GLISDONT         SUPPRESS REJECTED LINE                   
         OI    GLNORBOX,X'40'          SUPRESS ROW BOXES FOR TOTALS             
*                                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - RESOLVE'              
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                     *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),FF                                                         
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         OI    DATAIND2,DIBYPAID   NEED BUY PAID DATA                           
         B     XIT                                                              
         SPACE 1                                                                
RTNLIST  DS    0F                                                               
         DC    CL8'ISTAMOS ',A(ISTAMOS)                                         
         DC    CL8'OSTAMOS ',A(OSTAMOS)                                         
         DC    CL8'ISTAT   ',A(ISTAT)                                           
         DC    CL8'OSTAT   ',A(OSTAT)                                           
         DC    CL8'IINV    ',A(IINV)                                            
         DC    CL8'OINV    ',A(OINV)                                            
         DC    CL8'IINV1   ',A(IINV1)                                           
         DC    CL8'ICSHREC ',A(ICSHREC)                                         
         DC    CL8'ICSH    ',A(ICSH)                                            
         DC    CL8'OCSH    ',A(OCSH)                                            
         DC    CL8'HCSH    ',A(HCSH)                                            
         DC    CL8'OMONTH  ',A(OMON)                                            
         DC    CL8'ODOLDAY ',A(ODOLDAY)                                         
         DC    CL8'SUMMOS  ',A(OSUM)                                            
         DC    CL8'IPDNET  ',A(IPDNET)                                          
         DC    CL8'OPDNET  ',A(OPDNET)                                          
         DC    CL8'OPAID   ',A(OPAID)                                           
         DC    CL8'ACTVDATE',A(IDT)                                             
         DC    CL8'OCSHREC ',A(OCSHREC)                                         
         DC    CL8'ODATE   ',A(ODATE)                                           
         DC    CL8'HDATE   ',A(HDATE)                                           
         DC    CL8'ICLR    ',A(ICLR)                                            
         DC    CL8'OCLR    ',A(OCLR)                                            
         DC    CL8'OCHKN   ',A(OCHKN)                                           
         DC    CL8'ODAYS   ',A(ODAYS)                                           
         DC    CL8'OINVDATE',A(OINVDATE)                                        
         DC    CL8'ONORD   ',A(ONORD)                                           
         DC    CL8'OSPOTS  ',A(OSPOTS)                                          
         DC    CL8'OCDT    ',A(OCDT)                                            
         DC    CL8'IBLL    ',A(IBLL)                                            
         DC    CL8'OBILL$  ',A(OBILL$)                                          
         DC    X'FF'                                                            
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - INTCOMP'              
***********************************************************************         
*                                                                     *         
*        INTERNAL COMPUTES HOOK                                       *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
INTCOMP  DS    0H                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - EXEC'                 
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK TO EXECUTE ROUTINES                              *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BASR  RE,RF                                                            
         XIT1                                                                   
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - ISTAMOS'              
***********************************************************************         
*                                                                     *         
*        IF DOING PAYPENDING OPTION                                   *         
*           PASS STA/MOS                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ISTAMOS  DS    0H                                                               
*                                                                               
         MVC   0(L'SVSTAMOS,R2),SVSTAMOS    RETURN STATION/MOS                  
         XC    2(2,R2),2(R2)       KILL MARKET                                  
*                                                                               
ISTAMOSX DS    0H                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - OSTAMOS'              
***********************************************************************         
*                                                                     *         
*        IF DOING PAYPENDING OPTION                                   *         
*           SAVE STA/MOS                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OSTAMOS  DS    0H                                                               
*                                                                               
         MVI   STATSW,0            INI STATUS SWITCH                            
*                                                                               
         CLC   OPSTAMOS,0(R2)      ON CHANGE IN STA/MOS                         
         BE    *+8                                                              
         MVI   STATSW,C'Y'         SET STATUS SWITCH                            
*                                                                               
         MVC   OPSTAMOS,0(R2)       SAVE STATION/MOS                            
*                                                                               
OSTAMOSX DS    0H                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        IF DOING PAYPENDING OPTION                                   *         
*           SET CURRENT LINE PRINT STATUS                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ISTAT    DS    0H                                                               
*                                                                               
         MVC   0(1,R2),LINESTAT      RETURN LINE PRINT STATUS                   
         MVI   LINESTAT,0            CLEAR SWITCH                               
*                                                                               
ISTATX   DS    0H                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                     *         
*        IF DOING PAYPENDING OPTION                                   *         
*           SAVE LINE STATUS                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OSTAT    DS    0H                                                               
*                                                                               
         CLI   0(R2),C'P'          IF LINE TO PRINT                             
         BE    *+8                                                              
         CLI   STATSW,C'Y'         OR ON CHANGE IN STATUS SWITCH                
         BNE   *+10                                                             
         MVC   LINESTAT,0(R2)         SAVE LINE STATUS                          
*                                                                               
OSTATX   DS    0H                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        LINE SEQUENCE NUMBER OUTPUT ROUTINE                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OCSHREC  DS    0H                                                               
*                                                                               
         MVC   LSQN,0(R2)          SAVE LINE SEQUENCE NUMBER                    
*                                                                               
OCSHRECX DS    0H                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* INPUT INVOICE ROUTINE                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IINV     DS    0H                                                               
*                                                                               
         USING GLOBALD,R4                                                       
*                                                                               
         CLI   SBMODE,SBPROCBL                                                  
         BE    *+8                                                              
         CLI   SBMODE,SBPROCCA                                                  
         BNE   IINVX                                                            
*                                                                               
         XC    0(3,R2),0(R2)                                                    
*                                                                               
         L     R3,SBACURCH                                                      
         USING STABELEM,R3                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,STABBDT),(0,DUB)                                  
*                                                                               
         LAY   R1,SBB1PROF         SET A(B1 PROFILE)                            
         ST    R1,DMCB+8                                                        
         MVC   DMCB+8(1),SBMED                                                  
*                                                                               
         LAY   R1,SBB1XPRF         SET A(B1X PROFILE)                           
         ST    R1,DMCB+12                                                       
*                                                                               
*******  GOTO1 =V(SPFMTINO),DMCB,(C'B',DUB),(2,STABINV),,,SVBHDELA              
*                                                                               
         GOTO1 =V(SPFMTINO),DMCB,(0,DUB),(2,STABINV)                            
         L     R1,DMCB+4                                                        
         MVC   0(07,R2),0(R1)                                                   
*                                                                               
         CLI   GLARGS,C'1'         DONE IF DASHES WANTED                        
         BE    IINVX                                                            
*                                                                               
         LR    RF,R2               STRIP DASHES                                 
         LR    RE,R2                                                            
         LHI   R0,10               10 BYTES                                     
*                                                                               
         CLI   0(RF),C'-'                                                       
         BE    *+14                                                             
         MVC   0(1,RE),0(RF)       KEEP NON-DASH CHARACTERS                     
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)            NEXT CHARACTER                               
         BCT   R0,*-22                                                          
*                                                                               
         MVC   8(2,R2),=CL2' '     CLEAR LAST 2 POSITIONS                       
*                                                                               
IINVX    B     XIT                                                              
*                                                                               
         DROP  R3                                                               
         SPACE 2                                                                
*                                                                               
*        OUTPUT INVOICE NUMBER ROUTINE                                          
*                                                                               
OINV     DS    0H                                                               
*                                                                               
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE                         
         BO    OINVX                                                            
*                                                                               
         XC    OINVSAVE,OINVSAVE                                                
*                                                                               
         CLI   GLARGS,C'1'         FULL INVOICE NUMBER                          
         BNE   *+20                                                             
         MVC   0(10,R3),0(R2)                                                   
         MVC   OINVSAVE(10),0(R2)      SAVE INVOICE NUMBER                      
         B     OINVX                                                            
*                                                                               
         CLI   GLARGS,C'2'         FULL INVOICE W/O DASHES                      
         BNE   *+20                                                             
         MVC   0(8,R3),0(R2)                                                    
         MVC   OINVSAVE(8),0(R2)      SAVE INVOICE NUMBER                       
         B     OINVX                                                            
*                                                                               
         OC    0(7,R2),0(R2)                                                    
         BZ    OINVX                                                            
*                                                                               
         MVC   0(7,R3),0(R2)                                                    
         MVC   OINVSAVE,0(R2)      SAVE INVOICE NUMBER                          
*                                                                               
*&&DO                                                                           
         OC    0(3,R2),0(R2)       NO INVOICE?                                  
         BZ    OINVX                                                            
*                                                                               
         EDIT  (1,0(R2)),(2,0(R3)),FILL=0                                       
         EDIT  (2,1(R2)),(4,2(R3)),FILL=0                                       
*&&                                                                             
*                                                                               
OINVX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
OINVSAVE DS    CL10                INVOICE NUMBER SAVEAREA                      
         SPACE 2                                                                
*                                                                               
* OUTPUT MONTH ROUTINE                                                          
*                                                                               
OMON     DS    0H                                                               
         OC    0(4,R2),0(R2)       OTHER POSSIBLE RESPONSES                     
         BNZ   *+14                  FROM IPER (SFC)                            
         MVC   0(6,R3),=C'*PRIOR'                                               
         B     OMONX                                                            
         CLC   0(4,R2),=X'FEFEFEFE'                                             
         BNE   *+14                                                             
         MVC   0(6,R3),=C'*AFTER'                                               
         B     OMONX                                                            
         CLC   0(4,R2),XFF                                                      
         BNE   *+14                                                             
         MVC   0(5,R3),=C'*ALL*'                                                
         B     OMONX                                                            
         CLC   0(4,R2),=X'FDFDFDFD'                                             
         BNE   *+14                                                             
         MVC   0(6,R3),=C'*????*'                                               
         B     OMONX                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R2)),(3,WORK)   LAST DAY OF MON                 
         GOTO1 DATCON,DMCB,(2,2(R2)),(0,LDMON)                                  
         MVC   LDMON1,LDMON                                                     
         OC    SBWRPROF+4(2),SBWRPROF+4         IF 00 - LEAVE AS                
         BZ    OMON60                           LAST DAY OF MON                 
         CLI   SBWRPROF+3,C'C'     CURRENT MONTH                                
         BE    OMON50                                                           
         CLI   SBWRPROF+3,C'A'     2 MONS AHEAD                                 
         BE    OMON40                                                           
         CLI   SBWRPROF+3,C'N'     NEXT MONTH                                   
         BNE   OMON20                                                           
         CLI   WORK+1,X'0C'        IF THIS IS DECEMBER                          
         BNE   OMON10                                                           
         MVI   WORK+1,X'01'        SET MONTH TO JAN                             
         ZIC   R1,WORK             AND INCREMENT YEAR                           
         LA    R1,1(R1)                                                         
         STC   R1,WORK                                                          
         B     OMON50                                                           
*                                                                               
OMON10   ZIC   R1,WORK+1           INCREMENT MONTH                              
         LA    R1,1(R1)                                                         
         STC   R1,WORK+1                                                        
         B     OMON50                                                           
*                                                                               
OMON20   CLI   SBWRPROF+3,C'P'     PREV MONTH                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   WORK+1,X'01'        IF THIS IS JANUARY                           
         BNE   OMON30                                                           
         MVI   WORK+1,X'0C'        SET MONTH TO DECEMBER                        
         ZIC   R1,WORK             AND DECREMENT YEAR                           
         BCTR  R1,0                                                             
         STC   R1,WORK                                                          
         B     OMON50                                                           
*                                                                               
OMON30   ZIC   R1,WORK+1           DECREMENT MONTH                              
         BCTR  R1,0                                                             
         STC   R1,WORK+1                                                        
         B     OMON50                                                           
*                                                                               
OMON40   CLI   WORK+1,X'0D'        IF THIS IS NOVEMBER                          
         BNE   OMON42                                                           
         MVI   WORK+1,X'01'        SET MONTH TO JAN                             
         B     OMON43                                                           
*                                                                               
OMON42   CLI   WORK+1,X'0C'        IF THIS IS DECEMBER                          
         BNE   OMON45                                                           
         MVI   WORK+1,X'02'        SET MONTH TO FEB                             
*                                                                               
OMON43   ZIC   R1,WORK             AND INCREMENT YEAR                           
         LA    R1,1(R1)                                                         
         STC   R1,WORK                                                          
         B     OMON50                                                           
*                                                                               
OMON45   ZIC   R1,WORK+1           INCREMENT MONTH                              
         LA    R1,2(R1)                                                         
         STC   R1,WORK+1                                                        
*                                                                               
OMON50   GOTO1 DATCON,DMCB,(3,WORK),(0,LDMON)                                   
         ZIC   R1,SBWRPROF+4                                                    
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LDMON+4(1),DUB                                                   
         ZIC   R1,SBWRPROF+5                                                    
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LDMON+5(1),DUB                                                   
         BAS   RE,CHKMON                GO CHECK TO SEE IF IT'S VALID           
*                                                                               
OMON60   TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE                         
         BO    OMONX               YES - EXIT                                   
         CLI   GLARGS,C'D'                                                      
         BE    OMON70                                                           
         GOTO1 DATCON,DMCB,(0,LDMON1),(6,1(R3))                                 
         B     OMONX                                                            
*                                                                               
OMON70   ST    R3,DMCB+4                                                        
         MVI   DMCB+4,8                                                         
*                                                                               
         TM    OPTIND2,OPTIYMD     FORMAT DATE AS YYMMDD?                       
         BZ    *+12                                                             
         MVI   DMCB+4,0                                                         
         OI    DMCB+4,X'20'        OUTPUT PRINTABLE DATES                       
*                                                                               
*         GOTO1 DATCON,DMCB,(0,LDMON1),(8,0(R3))                                
         GOTO1 DATCON,DMCB,(0,LDMON1)                                           
*                                                                               
OMONX    B     XIT                                                              
         SPACE 2                                                                
*                                                                               
OCDT     DS    0H                                                               
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE                         
         BO    OCDTX               YES - EXIT                                   
         ST    R3,DMCB+4                                                        
         MVI   DMCB+4,8                                                         
*                                                                               
         TM    OPTIND2,OPTIYMD     FORMAT DATE AS YYMMDD?                       
         BZ    *+12                                                             
         MVI   DMCB+4,0                                                         
         OI    DMCB+4,X'20'        OUTPUT PRINTABLE DATES                       
*                                                                               
*         GOTO1 DATCON,DMCB,(0,LDMON),(8,0(R3))                                 
         GOTO1 DATCON,DMCB,(0,LDMON)                                            
OCDTX    DS    0H                                                               
         B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* INPUT INVOICE ROUTINE - FULL INVOICE WITH MEDIA                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IINV1    DS    0H                                                               
*                                                                               
         USING GLOBALD,R4                                                       
*                                                                               
         CLI   SBMODE,SBPROCBL     SKIP IF NOT DOING BILLS                      
         BNE   IINV1X                                                           
*                                                                               
         XC    0(10,R2),0(R2)      INIT OUTPUT AREA                             
*                                                                               
         L     R3,SBACURCH         ESTABLISH SPOT CHUNK                         
         USING STABELEM,R3                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,STABBDT),(0,DUB)   CONVERT BILL DATE              
*                                                                               
         LAY   R1,SBB1PROF         SET A(B1 PROFILE)                            
         ST    R1,DMCB+8                                                        
         MVC   DMCB+8(1),SBMED                                                  
*                                                                               
         LAY   R1,SBB1XPRF         SET A(B1X PROFILE)                           
         ST    R1,DMCB+12                                                       
*                                                                               
         GOTO1 =V(SPFMTINO),DMCB,(C'B',DUB),(2,STABINV),,,SVBHDELA              
         L     R1,DMCB                                                          
         MVC   0(10,R2),0(R1)                                                   
*                                                                               
         CLI   GLARGS,C'1'         DONE IF DASHES WANTED                        
         BE    IINV1X                                                           
*                                                                               
         LR    RF,R2               STRIP DASHES                                 
         LR    RE,R2                                                            
         LHI   R0,10               10 BYTES                                     
*                                                                               
         CLI   0(RF),C'-'                                                       
         BE    *+14                                                             
         MVC   0(1,RE),0(RF)       KEEP NON-DASH CHARACTERS                     
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)            NEXT CHARACTER                               
         BCT   R0,*-22                                                          
*                                                                               
         MVC   8(2,R2),=CL2' '     CLEAR LAST 2 POSITIONS                       
*                                                                               
IINV1X   B     XIT                                                              
*                                                                               
         DROP  R3                                                               
         SPACE 2                                                                
*                                                                               
* ACTIVITY DATE INPUT ROUTINE                                                   
*                                                                               
IDT      DS    0H                                                               
*                                                                               
         XC    0(2,R2),0(R2)       INIT RETURNED DATE                           
*                                                                               
         CLI   SBMODE,SBPROCSP     IF PROCESSING BUY                            
         BNE   IDTSPN                                                           
*                                                                               
         CLI   GLARGS,C' '         IF THERE IS AN ARGUMENT                      
         BNH   *+12                                                             
         CLI   GLARGS,C'P'            IT MUST BE FOR PAYMENT DATE               
         BNE   IDTSPX                                                           
*                                                                               
         L     R1,SBACURCH                                                      
         USING SCHUNKD,R1                                                       
*                                                                               
         LA    RF,SCCHKDT         ASSUME CHECK DATE WANTED                      
*                                                                               
         CLC   SCCHKNUM,SPACES     IF NO CHECK CUT YET                          
         BH    IDTDSP05                                                         
*                                                                               
         ICM   R5,15,ACLRST01      AND CLEARANCE MADE                           
         BZ    IDTDSP05                                                         
*                                                                               
         CLI   CBUOPT,C'Y'         AND REPORTING CBU                            
         BNE   IDTDSP05                                                         
*                                                                               
         LA    RF,SBBTODAY            USE TODAY                                 
         MVI   2(R2),C'*'             AND SET FLAG                              
*                                                                               
         B     IDTSP10                                                          
*                                                                               
IDTDSP05 DS    0H                                                               
*                                                                               
         CLI   MOFLOW,C'Y'         MONEYFLOW OPTION?                            
         BNE   *+16                NO                                           
         CLI   GLARGS+1,C'M'       WANT VENDOR BANK DATE?                       
         BE    IDTDSP06            YES                                          
         B     IDTSP10             NO                                           
*                                                                               
         CLI   SBWRPROF+11,C'Y'       IF VENDOR BANK CLR DATE WANTED            
         BE    *+8                                                              
         CLI   VDRBNKDT,C'Y'      IF CHECK BANK CLEARANCE DATE WANTED           
         BNE   IDTSP10                                                          
*                                                                               
IDTDSP06 OC    SCBNKDT,SCBNKDT    IF BANK CLEARANCE DATE PRESENT                
         BZ    *+12                                                             
         LA    RF,SCBNKDT            USE IT                                     
         B     IDTSP10                                                          
*                                                                               
         CLI   SBWR2PRF,C'Y'      CHK DAT INSTEAD OF LAST DAY OF MONTH?         
         BE    IDTSP10            YES - RF IS POINTING TO CHECK DATE            
*                                                                               
*        DEFAULT IS LAST DAY OF TODAY'S PREVIOUS MONTH                          
*                                                                               
***      GOTO1 DATCON,DMCB,(5,0),(0,WORK)  TODAY AS YYMMDD                      
         GOTO1 DATCON,DMCB,(2,SBBTODAY),(0,WORK)  TODAY AS YYMMDD               
         LHI   RF,-1              GET LAST DAY OF PREVIOUS MONTH                
         ST    RF,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,(C'M',WORK),(X'80',WORK+6)                            
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,0(R2))     RETURN DATE                 
*                                                                               
         CLC   SBAGY,=C'H9'       SKIP IF MEDIAVEST                             
         BE    *+8                                                              
         MVI   2(R2),C'*'         FLAG AS DEFAULT                               
*                                                                               
         B     IDTSP20                                                          
*                                                                               
IDTSP10  DS    0H                                                               
*                                                                               
         MVC   0(2,R2),0(RF)         RETURN CHECK DATE                          
*****                                                                           
*****    OC    0(2,RF),0(RF)       IF NO DATE PRESENT                           
*****    BNZ   IDTSP20                                                          
*****                              DEFAULT TO TODAY                             
*****    GOTO1 DATCON,DMCB,(5,0),(0,WORK)  TODAY AS YYMMDD                      
*****    GOTO1 DATCON,DMCB,(0,WORK),(2,0(R2))     RETURN DATE                   
*****                                                                           
*****    CLC   SBAGY,=C'H9'       SKIP IF MEDIAVEST                             
*****    BE    *+8                                                              
*****    MVI   2(R2),C'*'         FLAG AS DEFAULT                               
*****                                                                           
IDTSP20  DS    0H                                                               
*                                                                               
IDTSPX   DS    0H                                                               
*                                                                               
         B     IDTX                                                             
*                                                                               
         DROP  R1                                                               
*                                                                               
IDTSPN   DS    0H                                                               
*                                                                               
         CLI   SBMODE,SBPROCBL     IF PROCESSING STATION BILL RECORDS           
         BE    *+8                                                              
         CLI   SBMODE,SBPROCCA                                                  
         BNE   IDTBLN                                                           
*                                                                               
         CLI   GLARGS,C' '         IF THERE IS AN ARGUMENT                      
         BNH   *+12                                                             
         CLI   GLARGS,C'D'            DEFAULT IS DUE DATE                       
         BNE   IDTBL1                                                           
*                                                                               
         MVC   0(2,R2),BDUEDTE        RETURN INV/DUE DATE                       
*                                                                               
         B     IDTBLX                                                           
*                                                                               
IDTBL1   DS    0H                                                               
*                                                                               
         CLI   GLARGS,C'I'         INVOICE DATE?                                
         BNE   IDTBL2                                                           
*                                                                               
         MVC   0(2,R2),BINVDATE    RETURN BILL INVOICE DATE                     
*                                                                               
         B     IDTBLX                                                           
*                                                                               
IDTBL2   DS    0H                                                               
*                                                                               
         CLI   GLARGS,C'R'         INVOICE RUN DATE?                            
         BNE   IDTBLX                                                           
*                                                                               
         MVC   0(2,R2),BRUNDATE    RETURN BILL RUN DATE                         
*                                                                               
IDTBLX   DS    0H                                                               
*                                                                               
         B     IDTX                                                             
*                                                                               
IDTBLN   DS    0H                                                               
*                                                                               
IDTX     DS    0H                                                               
         B     XIT                                                              
*                                                                               
* ACTIVITY DATE OUTPUT ROUTINE                                                  
*                                                                               
ODATE    DS    0H                                                               
*                                                                               
         USING GLOBALD,R4                                                       
*                                                                               
         LA    R5,BACTDT           ASSUME BILL BILLING                          
*                                                                               
         CLI   GLARGS,C'C'         IF AGENCY CHECK DATE                         
         BNE   *+8                                                              
         LA    R5,CACTDT                                                        
*                                                                               
         XC    0(6,R5),0(R5)       INIT ACTIVITY DATE                           
*                                                                               
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE                         
         BO    XIT                 YES - EXIT                                   
*                                                                               
         MVC   0(9,R3),SPACES      INIT OUTPUT                                  
*                                                                               
         OC    0(2,R2),0(R2)       SKIP IF NO DATE                              
         BZ    XIT                                                              
*                                                                               
         ST    R3,DMCB+4                                                        
         MVI   DMCB+4,8                                                         
*                                                                               
         TM    OPTIND2,OPTIYMD     FORMAT DATE AS YYMMDD?                       
         BZ    *+12                                                             
         MVI   DMCB+4,0                                                         
         OI    DMCB+4,X'20'        OUTPUT PRINTABLE DATES                       
*                                                                               
         CLI   WDATEOPT,0          IF THERE IS A DATE OPTION                    
         BE    *+10                                                             
         MVC   DMCB+4(1),WDATEOPT     USE IT                                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,0(R2))                                            
*                                                                               
         CLI   GLARGS,C'C'        IF CHECK DATE                                 
         BNE   *+10                                                             
         MVC   8(1,R3),2(R2)         PRINT ANY FLAG                             
*                                                                               
         CLC   0(6,R3),SPACES                                                   
         BNH   ODATE9                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(2,0(R2)),(0,(R5))   ACTIVITY DATE                   
*                                                                               
ODATE9   DS    0H                                                               
*                                                                               
         CLI   LSQN,1              IF FIRST LINE IN SEQUENCE                    
         BNE   ODATE10                                                          
         CLI   GLARGS,C'C'         AND NOT CHECK DATE                           
         BE    ODATE10                                                          
*                                                                               
         MVC   LBACTDT,0(R5)          SAVE BILLING ACTIVITY DATE                
*                                                                               
ODATE10  DS    0H                                                               
*                                                                               
ODATEX   DS    0H                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'SPWRI13 - CASH APPLIED DATA - INPUT - HDATE'                    
***********************************************************************         
*                                                                     *         
*        CASH APPLIED DATE HEADER ROUTINE                             *         
*                                                                     *         
*NTRY    GLARGS     C'N'  - HEADLINE NUMBER                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
HDATE    DS    0H                                                               
*                                                                               
         CLI   GLARGS,C'2'        SKIP IF NOT SECOND HEADLINE                   
         BNE   HDATEH2N                                                         
*                                                                               
         MVC   0(8,R3),=CL8'CHECK'   DEFAULT TO HEADLINE OF 'CHECK'             
*                                                                               
         CLI   VDRBNKDT,C'Y'       IF BANK CLEARED DATE WANTED                  
         BNE   *+10                                                             
         MVC   0(8,R3),=CL8'BANK'        CHANGE HEADER                          
*                                                                               
         CLI   BNKDTOPT,C'Y'       IF BANK CLEARED DATE WANTED                  
         BE    *+8                                                              
         CLI   SBWRPROF+11,C'Y'    IF BANK CLEARED DATE WANTED                  
         BNE   *+10                                                             
         MVC   0(8,R3),=CL8'BANK'        CHANGE HEADER                          
*                                                                               
         B     HDATEX                                                           
*                                                                               
HDATEH2N DS    0H                                                               
*                                                                               
         CLI   GLARGS,C'3'        SKIP IF NOT FOURTH HEADLINE                   
         BNE   HDATEH3N                                                         
*                                                                               
******   LA    RF,198(R3)          BUMP TO NEXT PRINT LINE                      
*                                    (APPARENT DRIVHEAD BUG)                    
*                                                                               
****     CLI   BNKDTOPT,C'Y'       IF BANK CLEARED DATE WANTED                  
****     BE    *+8                                                              
****     CLI   SBWRPROF+11,C'Y'    IF BANK CLEARED DATE WANTED                  
****     BNE   *+10                                                             
*                                                                               
         MVC   0(8,R3),=CL8'DATE'  DEFAULT TO 'DATE'                            
*                                                                               
         B     HDATEX                                                           
*                                                                               
HDATEH3N DS    0H                                                               
*                                                                               
HDATEX   DS    0H                                                               
         B     XIT                                                              
*                                                                               
* CALCULATE PERCENTAGE OF PD AMOUNT ON THIS CHECK                               
*                                                                               
ICLR     DS    0H                                                               
*                                                                               
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE                         
         BO    ICLRX               YES - EXIT                                   
*                                                                               
         ZAP   8(8,R2),=P'0'       SET ITEM COUNTER                             
         ZAP   0(8,R2),=P'000000'  DEFAULT TO 00.000%                           
*                                                                               
         CLI   SBMODE,SBPROCSP     SKIP IF NOT PROCESSING BUY                   
         BNE   ICLRX                                                            
*                                                                               
         ZAP   8(8,R2),=P'1'       SET ITEM COUNTER                             
         ZAP   0(8,R2),=P'100000'  DEFAULT TO 100.000%                          
*                                                                               
         ICM   R3,15,ACLRST03      DONE IF NO INVOICE ELEMENT                   
         BZ    ICLRX                                                            
*                                                                               
         LLC   RF,1(R3)            GET ELEMENT LENGTH                           
         LA    R3,0(RF,R3)         POINT TO NEXT ELEMENT                        
*                                                                               
         CLI   0(R3),X'05'         DONE IF NOT AN 05 ELEMENT                    
         BNE   ICLRX                                                            
*                                                                               
         USING CLSTEL05,R3         ESTABLISH 05 ELEMENT                         
*                                                                               
         ICM   R5,15,ACLRST01      POINT TO 01 ELEMENT                          
         BZ    ICLRX               SKIP IF NONE THERE                           
*                                                                               
         USING CLSTEL01,R5         ESTABLISH 01 ELEMENT                         
*                                                                               
         ICM   RF,15,CLS5GRS       ASSUME CLEARED GROSS                         
         TM    CLSTSTAT,X'20'      IF CLEARED NET                               
         BNO   *+8                                                              
         ICM   RF,15,CLS5NET          USE NET AMOUNT                            
*                                                                               
         CVD   RF,DUB                                                           
         ZAP   WPKDIVD,DUB         SAVE CLEARED AMOUNT                          
         SRP   WPKDIVD,6,0         SCALE UP FOR ROUNDING                        
*                                                                               
         ICM   RF,15,CLSTGRS       ASSUME CLEARED GROSS                         
         TM    CLSTSTAT,X'20'      IF CLEARED NET                               
         BNO   *+8                                                              
         ICM   RF,15,CLSTNET          USE NET AMOUNT                            
*                                                                               
         LTR   RF,RF               SKIP IF ZERO DIVIDE                          
         BZ    ICLRX                                                            
*                                                                               
         CVD   RF,DUB                                                           
*                                                                               
         DP    WPKDIVD,DUB         CALCULATE PERCENT FOR INV                    
*                                                                               
         SRP   WPKQUOT,64-1,5      ROUND TO 5 DECIMALS                          
*                                                                               
         ZAP   0(8,R2),WPKQUOT                                                  
*                                                                               
ICLRX    DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
*        SAVE PERCENT FOR THE INVOICE                                           
*                                                                               
OCLR     DS    0H                                                               
*                                                                               
         ZAP   WPKDIVD,0(8,R2)     GET TOTAL PER CENT                           
         SRP   WPKDIVD,1,0         SCALE UP FOR ROUNDING                        
         DP    WPKDIVD,8(8,R2)     DIVIDE BY # OF OCCURRENCES                   
         ZAP   CLRPCT,WPKQUOT      SAVE PER CENT                                
*                                                                               
OCLRX    DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
* OUTPUT CHECK NUMBER                                                           
*                                                                               
OCHKN    DS    0H                                                               
*                                                                               
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE                         
         BO    XIT                 YES - EXIT                                   
*                                                                               
         MVC   CHKNUM,0(R2)                                                     
         OC    CHKNUM,SPACES                                                    
         MVC   0(L'CHKNUM,R3),CHKNUM                                            
*                                                                               
         CLI   CHKSOPT,C'0'        IF PRINTING ONLY NON DISBURSED               
         BNE   OCHKNONX                                                         
*                                                                               
OCHKNONX DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
* OUTPUT NUMBER OF DAYS                                                         
*                                                                               
ODAYS    DS    0H                                                               
*                                                                               
         TM    GLINDS,GLTOTLIN     SKIP IF A TOTAL LINE                         
         BO    ODDAYTOT                                                         
*                                                                               
         SR    R5,R5               INIT BILL DAYS                               
*                                                                               
         CLC   CSHCHKNM,SPACES     IF  CLIENT HASN'T PAID                       
         BH    ODDAY1                                                           
         OC    CACTDT,CACTDT       AND AGENCY HASN'T PAID                       
         BNZ   ODDAY1                                                           
         CLI   SBWRPROF+8,C'Y'     AND PROFILE SET                              
         BE    ODDAYX                  SKIP PRINTING DAYS                       
*                                                                               
ODDAY1   DS    0H                                                               
*                                                                               
         LA    RF,BACTDT           ASSUME BILLING DAYS                          
         CLI   GLARGS,C'C'         CHECK FOR CASH DAYS                          
         BNE   *+8                                                              
         LA    RF,CSHACTDT                                                      
*                                                                               
         CLI   SBWRPROF+15,C'Y'    IF USING TODAY AS BASE DATE                  
         BNE   ODDAY11                                                          
*                                                                               
         LR    R0,RF                                                            
***      GOTO1 DATCON,DMCB,(5,0),(0,LDMON)  TODAY AS YYMMDD                     
         GOTO1 DATCON,DMCB,(2,SBBTODAY),(0,LDMON)  TODAY AS YYMMDD              
         LR    RF,R0                                                            
*                                                                               
ODDAY11  DS    0H                                                               
*                                                                               
         OC    0(L'BACTDT,RF),0(RF)  SKIP IF NO ACTIVITY DATE                   
         BZ    ODDAYBLN                                                         
*                                                                               
         OC    LDMON,LDMON         SKIP IF NO LAST DAY OF MON KNOWN             
         BZ    ODDAYBLN                                                         
*                                                                               
         GOTO1 PERVERT,DMCB,LDMON,0(RF)    NUMBER OF DISBURSE DAYS              
*                                                                               
         LH    R5,8(R1)            NUMBER OF DAYS (INCLUSIVE)                   
         LTR   R5,R5                                                            
         BM    ODDAY25                                                          
         SH    R5,=H'1'                                                         
         B     *+8                                                              
*                                                                               
ODDAY25  AH    R5,=H'1'                                                         
*                                                                               
         LCR   R5,R5               BILLING DAYS ARE CONSIDERED NEGATIVE         
*                                                                               
         L     R1,BDAYSTOT                                                      
         AR    R1,R5                                                            
         ST    R1,BDAYSTOT                                                      
*                                                                               
ODDAYBLN DS    0H                                                               
*                                                                               
         ST    R5,FULL             SAVE BILLING DAYS                            
*                                                                               
         SR    R5,R5               INIT CHECK DAYS                              
*                                                                               
         OC    CACTDT,CACTDT        SKIP IF NO CHECK ACTIVITY DATE              
         BZ    ODDAYCKN                                                         
*                                                                               
         LA    RF,LDMON            ASSUME NORMAL BASE DATE                      
*                                                                               
         CLI   SBWRPROF+6,C'Y'     SKIP IF NO BILLING OPTION OFF                
         BNE   ODDAY30                                                          
*                                                                               
         CLC   LBACTDT,SPACES      IF NO BILLING FOR DETAILS                    
         BH    *+8                                                              
         LA    RF,SBQTODAY            USE TODAY'S DATE                          
*                                                                               
ODDAY30  DS    0H                                                               
         OC    0(6,RF),0(RF)       SKIP IF DATE NOT KNOWN                       
         BZ    ODDAYCKN                                                         
*                                                                               
         GOTO1 PERVERT,DMCB,(RF),CACTDT   NUMBER OF DISBURSE DAYS               
*                                                                               
         LH    R5,8(R1)            NUMBER OF DAYS (INCLUSIVE)                   
         LTR   R5,R5                                                            
         BM    ODDAY35                                                          
         SH    R5,=H'1'                                                         
         B     *+8                                                              
*                                                                               
ODDAY35  AH    R5,=H'1'                                                         
*                                                                               
         L     R1,CDAYSTOT                                                      
         AR    R1,R5                                                            
         ST    R1,CDAYSTOT                                                      
*                                                                               
ODDAYCKN DS    0H                                                               
*                                                                               
         A     R5,FULL             ADD IN BILLING DAYS                          
*                                                                               
         LTR   R5,R5               DON'T PRINT NULLS                            
         BZ    ODDAYX                                                           
*                                                                               
         EDIT  (R5),(8,0(R3)),FLOAT=-,ZERO=BLANK  PRINT DAYS TO DISB            
*                                                                               
         B     ODDAYX                                                           
*                                                                               
ODDAYTOT DS    0H                                                               
*                                                                               
         XC    BDAYSTOT,BDAYSTOT                                                
         XC    CDAYSTOT,CDAYSTOT                                                
*                                                                               
         ST    R3,TOTDAYA          SAVE OUTPUT ADDRESS                          
*                                                                               
ODDAYX   B     XIT                                                              
         SPACE 2                                                                
* OUTPUT INVOICE DATE                                                           
*                                                                               
OINVDATE DS    0H                                                               
*                                                                               
         TM    GLINDS,GLTOTLIN     SKIP TOTAL LINES                             
         BO    OINVDATX                                                         
*                                                                               
         LA    R0,17               DEFAULT DATE FORMAT                          
*                                                                               
         CLI   WDATEOPT,0          IF THERE IS A DATE OPTION                    
         BE    *+8                                                              
         IC    R0,WDATEOPT             USE IT                                   
*                                                                               
         GOTO1 DATCON,DMCB,(2,0(R2)),((R0),0(R3))                               
*                                                                               
OINVDATX DS    0H                                                               
         B     XIT                                                              
*                                                                               
* OUTPUT NET COST                                                               
*                                                                               
ONORD    DS    0H                                                               
*                                                                               
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE                         
         BNO   ONORDX                                                           
*                                                                               
         EDIT  (4,0(R2)),(14,0(R3)),2,FLOAT=-,COMMAS=YES,ZERO=BLANK             
*                                                                               
ONORDX   B     XIT                                                              
         SPACE 2                                                                
* OUTPUT SPOTS                                                                  
*                                                                               
OSPOTS   DS    0H                                                               
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE                         
         BNO   OSPX                                                             
         EDIT  (4,0(R2)),(6,0(R3)),ZERO=BLANK                                   
*                                                                               
OSPX     B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - ODOLDAY'              
***********************************************************************         
*                                                                     *         
*        DOLLAR DAYS OUTPUT ROUTINE                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ODOLDAY  DS    0H                                                               
*                                                                               
         TM    GLINDS,GLTOTLIN     SKIP IF THIS IS A TOTAL LINE                 
         BO    ODDYTOT                                                          
*                                                                               
         SR    R5,R5               INIT BILL DAYS                               
         ZAP   SVBLLAMT,=P'0'      INIT BILL DOLLARS                            
         ZAP   SVBLLDLR,=P'0'      INIT BILL DOLLAR-DAYS                        
*                                                                               
         CLI   CHKSOPT,C'0'        SKIP IF NOT PAYPENDING                       
         BNE   *+12                                                             
         CLI   LINESTAT,C'P'       SKIP IF NOT PRINTING LINE                    
         BNE   ODOLDAYX                                                         
*                                                                               
         CLC   CSHCHKNM,SPACES     IF  CLIENT HASN'T PAID                       
         BH    ODDY1                                                            
         OC    CACTDT,CACTDT       AND AGENCY HASN'T PAID                       
         BNZ   ODDY1                                                            
         CLI   SBWRPROF+8,C'Y'     AND PROFILE SET                              
         BE    ODOLDAYX                SKIP PRINTING DAYS                       
*                                                                               
ODDY1    DS    0H                                                               
*                                                                               
         LA    RF,BACTDT           ASSUME BILL DAYS                             
*                                                                               
         CLI   GLARGS,C'C'         CHECK FOR CASH DAYS                          
         BNE   *+8                                                              
         LA    RF,CSHACTDT            SUBSTITUTE CASH APPLIED DATE              
*                                                                               
         OC    0(L'BACTDT,RF),0(RF) SKIP IF NO ACTIVITY DATE                    
         BZ    ODDYBLN                                                          
*                                                                               
         OC    LDMON,LDMON         SKIP IF NO LAST DAY OF MON KNOWN             
         BZ    ODDYBLN                                                          
*                                                                               
         GOTO1 PERVERT,DMCB,LDMON,0(RF)    NUMBER OF DISBURSE DAYS              
*                                                                               
         LH    R5,8(R1)            NUMBER OF DAYS (INCLUSIVE)                   
         LTR   R5,R5                                                            
         BM    ODDY25                                                           
*                                                                               
         SH    R5,=H'1'                                                         
         B     *+8                                                              
ODDY25   AH    R5,=H'1'                                                         
*                                                                               
         LCR   R5,R5               BILLING DAYS ARE CONSIDERED NEGATIVE         
*                                                                               
         ZAP   WPKDIVD,BLLAMTP     GET BILLED AMOUNT                            
*                                                                               
         CLI   GLARGS,C'C'         CHECK FOR CASH DAYS                          
         BNE   *+10                                                             
         ZAP   WPKDIVD,CSHAMTP     SUBSTITUTE CASH APPLIED DOLLARS              
*                                                                               
         ZAP   SVBLLAMT,WPKDIVD    SAVE BILLED AMOUNT                           
*                                                                               
         CVD   R5,DUB              DAYS                                         
*                                                                               
         MP    WPKDIVD,DUB         BILL-DOLLAR DAYS                             
*                                                                               
         ZAP   SVBLLDLR,WPKDIVD    SAVE BILL DOLLAR DAYS                        
*                                                                               
ODDYBLN DS     0H                                                               
*                                                                               
         CP    SVBLLAMT,=P'0'      TEST FOR DEBIT OR CREDIT                     
         BL    ODDYBLCR            CREDIT                                       
*                                  DEBIT                                        
         LA    RF,TBKBLDDB         ==> TO BILLING-DOLLAR TOTAL BKTS             
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RF),SVBLLDLR INCREMENT ALL LVL BUCKETS                
         LA    RF,L'TOTBKS(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         B     ODDY30                                                           
*                                                                               
ODDYBLCR DS    0H                                                               
*                                                                               
         LA    RF,TBKBLDCR         ==> TO BILLING-DOLLAR TOTAL BKTS             
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RF),SVBLLDLR INCREMENT ALL LVL BUCKETS                
         LA    RF,L'TOTBKS(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
ODDY30   DS    0H                                                               
*                                                                               
         SR    R5,R5               INIT CHECK DAYS                              
         ZAP   SVCHKDLR,=P'0'      INIT CHECK DOLLAR-DAYS                       
*                                                                               
         OC    CACTDT,CACTDT        SKIP IF NO CHECK ACTIVITY DATE              
         BZ    ODDYCKN                                                          
*                                                                               
         LA    RF,LDMON            ASSUME NORMAL BASE DATE                      
*                                                                               
         CLI   SBWRPROF+6,C'Y'     SKIP IF NO BILLING OPTION OFF                
         BNE   ODDY31                                                           
*                                                                               
         CLC   LBACTDT,SPACES      IF NO BILLING FOR DETAILS                    
         BH    *+8                                                              
         LA    RF,SBQTODAY            USE TODAY'S DATE                          
*                                                                               
ODDY31   DS    0H                                                               
         OC    0(6,RF),0(RF)       SKIP IF DATE NOT KNOWN                       
         BZ    ODDYCKN                                                          
*                                                                               
         GOTO1 PERVERT,DMCB,(RF),CACTDT   NUMBER OF DISBURSE DAYS               
*                                                                               
         LH    R5,8(R1)            NUMBER OF DAYS (INCLUSIVE)                   
         LTR   R5,R5                                                            
         BM    ODDY35                                                           
*                                                                               
         SH    R5,=H'1'                                                         
         B     *+8                                                              
ODDY35   AH    R5,=H'1'                                                         
*                                                                               
         CVD   R5,DUB              CVD                                          
*                                                                               
         ZAP   WPKDIVD,CLRAMT      GET CLEARED AMOUNT                           
*                                                                               
         MP    WPKDIVD,DUB         CHECK DOLLAR DAYS                            
*                                                                               
         ZAP   SVCHKDLR,WPKDIVD    SAVE CHK-DOLLAR DAYS                         
*                                                                               
ODDYCKN DS     0H                                                               
*                                                                               
         CP    CLRAMT,=P'0'        TEST FOR DEBIT OR CREDIT                     
         BL    ODDYCKCR            CREDIT                                       
*                                  DEBIT                                        
         LA    RF,TBKCLDDB         ==> TO CLEARED-DOLLAR TOTAL BKTS             
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RF),SVCHKDLR  INCREMENT ALL LVL BUCKETS               
         LA    RF,L'TOTBKS(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         B     ODDY40                                                           
*                                                                               
ODDYCKCR DS    0H                                                               
*                                                                               
         LA    RF,TBKCLDCR         ==> TO CLEARED-DOLLAR TOTAL B                
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RF),SVCHKDLR  INCREMENT ALL LVL BUCKETS               
         LA    RF,L'TOTBKS(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
ODDY40   DS    0H                                                               
*                                                                               
         AP    SVCHKDLR,SVBLLDLR   ADD IN BILLING DOLLAR DAYS                   
         BZ    ODOLDAYX            DON'T PRIN NULLS                             
*                                                                               
         CLI   DAYS,C'Y'           SKIP IF NO DAYS                              
         BNE   ODDY20                                                           
*                                                                               
***      EDIT  SVCHKDLR,(17,0(R3)),2,COMMAS=YES,FLOAT=-,ZERO=BLANK              
*                                                     PRINT DOLLAR DAYS         
         XC    EBLOCK,EBLOCK                                                    
         ST    R3,EBAOUT           A(OUTPUT)                                    
*                                                                               
         MVI   EBLOUT,18           L'OUTPUT                                     
         MVI   EBDECS,2            N'DECIMAL PLACES                             
*                                                                               
         LA    R1,SVCHKDLR                                                      
         ST    R1,EBAIN            A(INPUT)                                     
         MVI   EBLIN,8             L'INPUT                                      
         MVI   EBTIN,C'P'          TYPE OF INPUT                                
         MVI   EBFLOAT,C'-'        FLOAT = -                                    
         OI    EBOPT,EBOQZEN       ZERO=NOBLANK                                 
         OI    EBOPT,EBOQCEY       COMMA=YES                                    
         MVI   EBALIGN,C'R'        ALIGN = RIGHT                                
*                                                                               
         GOTO1 EDITOR,DMCB,EBLOCK                                               
*                                                                               
ODDY20   DS    0H                                                               
*                                                                               
         B     ODOLDAYX                                                         
*                                                                               
*        TOTAL ROUTINE                                                          
*                                                                               
ODDYTOT  DS    0H                                                               
*                                                                               
         CLI   DAYS,C'Y'           IF NO DAYS THEN NO TOTALS                    
         BNE   ODOLDAYX                                                         
*                                                                               
         CLI   SBWRPROF+6,C'Y'     IF PROFILE SET                               
         BNE   *+14                                                             
         CLC   GLLEVEL,QPERLEV     AND GLLEVEL > PERIOD LEVEL                   
         BNH   ODOLDAYX               SKIP PRINTING ADD=                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,GLLEVEL        GET CURRENT TOTAL LEVEL                      
         BZ    *+6                 GRAND TOTAL                                  
         BCTR  RF,0                DECREMENT FOR INDEXING                       
*                                                                               
         LA    RE,L'TOTBKS         BUCKET LENGTH                                
         MR    RE,RE               RF = DISP INTO BUCKETS OF THIS LEVEL         
*                                                                               
         LA    RE,TBKBLDDB         ==> TO BILLING DEBIT TOTAL BUCKETS           
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   TBDOLDB,0(L'TOTBKS,R1)  SAVE THIS LEVEL TOTAL                    
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR THIS LEVEL TOTAL                   
*                                                                               
         LA    RE,TBKBLDCR         ==> TO BILLING CREDT TOTAL BUCKETS           
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   TBDOLCR,0(L'TOTBKS,R1)  SAVE THIS LEVEL TOTAL                    
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR THIS LEVEL TOTAL                   
*                                                                               
         LA    RE,TBKCLDDB         ==> TO CLEARED TOTAL BUCKETS                 
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   TCDOLDB,0(L'TOTBKS,R1)  SAVE THIS LEVEL TOTAL                    
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR THIS LEVEL TOTAL                   
*                                                                               
         LA    RE,TBKCLDCR         ==> TO CLEARED TOTAL BUCKETS                 
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   TCDOLCR,0(L'TOTBKS,R1)  SAVE THIS LEVEL TOTAL                    
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR THIS LEVEL TOTAL                   
*                                                                               
         ZAP   DOLTOT,TBDOLDB      FIND DOLLAR DAYS TOTAL                       
         AP    DOLTOT,TBDOLCR      FIND DOLLAR DAYS TOTAL                       
         AP    DOLTOT,TCDOLDB                                                   
         AP    DOLTOT,TCDOLCR                                                   
*                                                                               
         EDIT  DOLTOT,(17,0(R3)),2,COMMAS=YES,FLOAT=-,ZERO=BLANK                
*                                                                               
         CLI   SBWRPROF+6,C'Y'     IF PROFILE SET                               
         BNE   *+14                                                             
         CLC   GLLEVEL,QPERLEV     AND GLLEVEL > PERIOD LEVEL                   
         BNH   ODOLDAYX               SKIP PRINTING ADD=                        
*                                                                               
         CLC   GLLEVEL,QPERLEV     CFMON TOTAL TAKEN CARE OF ELSEWHERE          
         BE    *+8                                                              
         BAS   RE,OSUMNTR             PRINT ADD                                 
*                                                                               
ODOLDAYX DS    0H                                                               
*                                                                               
         XC    BACTDT,BACTDT                                                    
         XC    CACTDT,CACTDT                                                    
         XC    CSHACTDT,CSHACTDT                                                
*                                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - OSUM'                 
***********************************************************************         
*                                                                     *         
*        MONTH OF SERVICE SUMMARY                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OSUMNTR  NTR1  LABEL=*             SPECIAL INTERNAL ENTRY                       
*                                                                               
OSUM     DS    0H                                                               
*                                                                               
         TM    DOWNOPT,GLDLTOTS    SKIP IF DOWNLOADING TOTALS                   
         BO    OSUMX                                                            
*                                                                               
         LR    R5,R3               SAVE STARTING PRINT POSITION                 
*                                                                               
         XC    EBLOCK,EBLOCK                                                    
*                                                                               
         CLI   DAYS,C'Y'           DAYS                                         
         BNE   OSUMX                                                            
*                                                                               
         ZAP   MYREMAIN,TCDOLDB    GET CHECKING-DOLLAR DEBIT DAYS               
         AP    MYREMAIN,TBDOLCR    ADD IN BILL DOLLAR CREDIT DAYS               
         SRP   MYREMAIN,2,0        *100                                         
*                                                                               
         ZAP   MYQUOTE,=P'0'       INIT QUOTIENT                                
*                                                                               
         ZAP   MYDUB,CLRTOTDB      SUM CHK DEBIT                                
         SP    MYDUB,BLLTOTCR      AND BLL CREDIT                               
*                                                                               
         CP    MYDUB,=P'0'                                                      
         BE    *+16                                                             
         XC    MYQUOTE,MYQUOTE                                                  
         DP    MYQUOTE(16),MYDUB                                                
*                                                                               
         ZAP   DUB,MYQUOTE         SAVE ADD                                     
*                                                                               
         ZAP   MYREMAIN,TCDOLCR    GET CHECKING-DOLLAR CREDIT DAYS              
         AP    MYREMAIN,TBDOLDB    ADD IN BILL DOLLAR  DEBIT  DAYS              
         SRP   MYREMAIN,2,0        *100                                         
*                                                                               
         ZAP   MYQUOTE,=P'0'       INIT QUOTIENT                                
*                                                                               
         ZAP   MYDUB,BLLTOTDB      SUM BLL DEBIT                                
         SP    MYDUB,CLRTOTCR      AND CHK CREDIT                               
*                                                                               
         CP    MYDUB,=P'0'                                                      
         BE    *+16                                                             
         XC    MYQUOTE,MYQUOTE                                                  
         DP    MYQUOTE(16),MYDUB                                                
*                                                                               
         AP    DUB,MYQUOTE         SAVE ADD                                     
*                                                                               
         ZAP   MYQUOTE,DUB         AVERAGE CHK DAYS PLUS AVG BILL DAYS          
*                                                                               
         SRP   MYQUOTE,64-1,5      ROUND TO ONE DECIMAL                         
*                                                                               
         CP    MYQUOTE,=P'0'       SKIP IF ZERO                                 
         BE    OSUM35                                                           
*                                                                               
         L     R3,TOTDAYA          PRINT IN DAYS COLUMN                         
*                                                                               
         LR    R1,R3               INIT PRINT POSITION                          
*                                                                               
         MVC   0(4,R3),=C'ADD='                                                 
         LA    R1,4(R3)                                                         
*                                                                               
         ST    R1,EBAOUT           A(OUTPUT)                                    
*                                                                               
         MVI   EBLOUT,6            L'OUTPUT                                     
         MVI   EBDECS,1            N'DECIMAL PLACES                             
*                                                                               
         LA    R1,MYQUOTE                                                       
         ST    R1,EBAIN            A(INPUT)                                     
         MVI   EBLIN,8             L'INPUT                                      
         MVI   EBTIN,C'P'          TYPE OF INPUT                                
         MVI   EBFLOAT,C'-'        FLOAT = -                                    
         OI    EBOPT,EBOQZEN       ZERO=NOBLANK                                 
         TM    DOWNOPT,GLDLTOTS    SKIP IF DOWNLOADING TOTALS                   
         BO    *+8                                                              
         MVI   EBALIGN,C'R'        ALIGN = RIGHT                                
*                                                                               
         CLI   DAYS,C'Y'           DAYS                                         
         BNE   OSUM35                                                           
*                                                                               
         GOTO1 EDITOR,DMCB,EBLOCK                                               
*                                                                               
OSUM35   XC    EBLOCK,EBLOCK                                                    
*                                                                               
OSUMX    DS    0H                                                               
*                                                                               
         XC    BACTDT,BACTDT                                                    
         XC    CACTDT,CACTDT                                                    
         XC    CSHACTDT,CSHACTDT                                                
*                                                                               
         MVC   LBACTDT,SPACES                                                   
         ZAP   TCDOLDB,=P'0'                                                    
         ZAP   TCDOLCR,=P'0'                                                    
         ZAP   TBDOLDB,=P'0'                                                    
         ZAP   TBDOLCR,=P'0'                                                    
         LR    R3,R5               RESTORE                                      
         B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - OPAID'                
***********************************************************************         
*                                                                     *         
*        PAID OUTPUT ROUTINE                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OPAID    DS    0H                                                               
*                                                                               
         L     R1,GLADTENT                                                      
         USING DROD,R1             ESTABLISH DRIVE TABLE ENTRY                  
*                                                                               
         TM    COLIND,COLIRND      IF ROUNDING                                  
         BNO   *+12                                                             
         MVI   DRODEC,0               NO DECIMALS FOR ROUNDING                  
         MVI   DRODIV,2               ROUND OUT PENNIES                         
*                                                                               
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE                         
         BO    OPDTOT                                                           
*                                                                               
         ICM   RF,15,0(R2)         CONVERT INPUT                                
         CVD   RF,DUB                                                           
         ZAP   CLRAMT,DUB          SAVE CLEARED AMOUNT                          
*                                                                               
*        PRINT PAID AMOUNT                                                      
*                                                                               
         CLI   CHKIVOPT,C'Y'       IF SHOWING CHK INVOICE                       
         BNE   OPDPCTX                                                          
*                                                                               
         ZAP   WPKDIVD,CLRAMT      FIND % ON INVOICE                            
         MP    WPKDIVD,CLRPCT        * CLRST PER CENT                           
         SRP   WPKDIVD,64-6,5        ROUND TO 2 DECIMALS                        
         ZAP   CLRAMT,WPKDIVD                                                   
*                                                                               
OPDPCTX  DS    0H                                                               
*                                                                               
         ZAP   DUB,CLRAMT          CONVERT TO BINARY                            
         CVB   RF,DUB                                                           
         STCM  RF,15,0(R2)         RESET INPUT                                  
*                                                                               
         CLI   CHKSOPT,C'0'        SKIP IF PAYPENDING                           
         BE    OPDEDIT                                                          
*                                                                               
         CP    CLRAMT,=P'0'        DO NOT PRINT ZERO AMOUNTS                    
         BE    OPAIDX                                                           
         BM    OPDCR               CREDIT                                       
*                                                                               
         LA    RF,TBKCLRDB         ==> TO CLEARED TOTAL BUCKETS                 
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RF),CLRAMT  INCREMENT ALL LVL BUCKETS                 
         LA    RF,L'TOTBKS(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         B     OPD10                                                            
*                                                                               
OPDCR    DS    0H                  CREDITS                                      
*                                                                               
         LA    RF,TBKCLRCR         ==> TO CLEARED TOTAL BUCKETS                 
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RF),CLRAMT INCREMENT ALL LVL BUCKETS                  
         LA    RF,L'TOTBKS(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
OPD10    DS    0H                                                               
*                                                                               
         B     OPDEDIT                                                          
*                                                                               
*        TOTALS                                                                 
*                                                                               
OPDTOT   DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,GLLEVEL        GET CURRENT TOTAL LEVEL                      
         BZ    *+6                 GRAND TOTAL                                  
         BCTR  RF,0                DECREMENT FOR INDEXING                       
*                                                                               
         LA    RE,L'TOTBKS         BUCKET LENGTH                                
         MR    RE,RE               DISP INTO BUCKETS OF THIS LEVEL              
*                                                                               
         LA    RE,TBKCLRDB         ==> TO CLEARED DEBIT BUCKETS                 
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   CLRTOTDB,0(L'TOTBKS,R1)  SAVE THIS LEVEL TOTAL                   
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR THIS LEVEL TOTAL                   
*                                                                               
         LA    RE,TBKCLRCR         ==> TO CLEARED CREDIT BUCKETS                
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   CLRTOTCR,0(L'TOTBKS,R1)   ADD IN THIS LEVEL TOTAL                
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR  THIS LEVEL TOTAL                  
*                                                                               
         ZAP   CLRTOT,CLRTOTDB     PRINT COMBINED TOTAL                         
         AP    CLRTOT,CLRTOTCR                                                  
*                                                                               
         B     OPDEDIT             LET DRIVER PRINT IT                          
*                                                                               
         EDIT  CLRTOT,(14,0(R3)),2,COMMAS=YES,FLOAT=-,ZERO=BLANK                
*                                                                               
         B     OPAIDX                                                           
*                                                                               
OPDEDIT  DS    0H                                                               
*                                                                               
OPDEDIT1 DS    0H                                                               
*                                                                               
         MVI   GLHOOK,GLEDIT       LET DRIVER PRINT IT                          
*                                                                               
OPAIDX   B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - OBILL$'               
***********************************************************************         
*                                                                     *         
*        BILLED OUTPUT ROUTINE                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OBILL$   DS    0H                                                               
*                                                                               
         L     R1,GLADTENT                                                      
         USING DROD,R1             ESTABLISH DRIVE TABLE ENTRY                  
*                                                                               
         TM    COLIND,COLIRND      IF ROUNDING                                  
         BNO   *+8                                                              
         MVI   DRODEC,0               NO DECIMALS FOR ROUNDING                  
*                                                                               
*****    L     R5,0(R2)                                                         
*                                                                               
         TM    GLINDS,GLTOTLIN     SKIP IF A TOTAL LINE                         
         BO    OBLTOT                YES                                        
*                                    NO                                         
*        PRINT BILL AMOUNT                                                      
*                                                                               
*****    CVD   R5,BLLAMTP          SAVE BILLED AMOUNT                           
         ZAP   BLLAMTP,0(8,R2)     SAVE BILLED AMOUNT                           
*                                                                               
         CLI   CHKSOPT,C'0'        IF PAYPENDING                                
         BNE   *+12                                                             
         CLI   LINESTAT,C'P'          SKIP IF NOT PRINTING LINE                 
         BNE   OBL10                                                            
*                                                                               
         CP    BLLAMTP,=P'0'       DO NOT PRINT ZERO AMOUNTS                    
         BZ    OBLX                                                             
         BM    OBLCR               CREDIT                                       
*                                                                               
         LA    RF,TBKBLLDB         ==> TO DEBIT BILLING TOTAL BUCKETS           
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RF),BLLAMTP  INCREMENT ALL LVL BUCKETS                
         LA    RF,L'TOTBKS(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         B     OBL10                                                            
*                                                                               
OBLCR    DS    0H                  AMOUNT IS A CREDIT                           
*                                                                               
         LA    RF,TBKBLLCR         ==> TO CREDIT BILLING TOTAL BUCKTS           
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RF),BLLAMTP  INCREMENT ALL LVL BUCKETS                
         LA    RF,L'TOTBKS(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
OBL10    DS    0H                                                               
*                                                                               
         B     OBLEDIT             LET DRIVER PRINT IT                          
*                                                                               
OBLTOT   DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,GLLEVEL        GET CURRENT TOTAL LEVEL                      
         BZ    *+6                 GRAND TOTAL                                  
         BCTR  RF,0                DECREMENT FOR INDEXING                       
*                                                                               
         LA    RE,L'TOTBKS         BUCKET LENGTH                                
         MR    RE,RE               RF = DISP INTO BUCKETS OF THIS LEVEL         
*                                                                               
         LA    RE,TBKBLLDB         ==> TO DEBIT BILLING TOTAL BUCKETS           
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   BLLTOTDB,0(L'TOTBKS,R1)   COPY THIS LEVEL TOTAL                  
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR THIS LEVEL TOTAL                   
*                                                                               
         LA    RE,TBKBLLCR         ==> TO CREDIT BILLING TOTAL BUCKTS           
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   BLLTOTCR,0(L'TOTBKS,R1)   ADD IN THIS LEVEL TOTAL                
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR THIS LEVEL TOTAL                   
*                                                                               
         ZAP   BLLTOT,BLLTOTDB                                                  
         AP    BLLTOT,BLLTOTCR                                                  
*                                                                               
OBLPNDX  DS    0H                                                               
*                                                                               
OBLEDIT  DS    0H                                                               
         MVI   GLHOOK,GLEDIT          LET DRIVER PRINT IT                       
*                                                                               
OBLX     B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - OCSH'                 
***********************************************************************         
*                                                                     *         
*        CASH APPLIED ROUTINES - OUTPUT                               *         
*                                                                     *         
*        GLARGS C'#' - CLIENT CHECK NUMBER                            *         
*               C'%' - CASH APPLIED PERCENT                           *         
*               C'$' - CASH APPLIED AMOUNT                            *         
*               C'%' - CASH APPLIED PERCENT                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OCSH     DS    0H                                                               
*                                                                               
         L     R1,GLADTENT                                                      
         USING DROD,R1             ESTABLISH DRIVE TABLE ENTRY                  
*                                                                               
         TM    COLIND,COLIRND      IF ROUNDING                                  
         BNO   *+8                                                              
         MVI   DRODEC,0               NO DECIMALS FOR ROUNDING                  
*                                                                               
*        DETERMINE ROUTINE BASED ON FIRST ARGUMENT                              
*                                                                               
         USING GLOBALD,R4                                                       
*                                                                               
         CLI   GLARGS,C'#'         CHECK NUMBER                                 
         BE    OCSHCK#                                                          
*                                                                               
         CLI   GLARGS,C'D'         CHECK DATE                                   
         BE    OCSHCKD                                                          
*                                                                               
         CLI   GLARGS,C'%'         CASH APPLIED PERCENTAGE                      
         BE    OCSHPC                                                           
*                                                                               
         CLI   GLARGS,C'$'         CASH APPLIED DOLLARS                         
         BE    OCSH$                                                            
*                                                                               
         B     OCSHX               UNKNOWN ROUTINE                              
*                                                                               
*        CLIENT CHECK NUMBER                                                    
*                                                                               
OCSHCK#  DS    0H                                                               
*                                                                               
         MVC   0(6,R3),SPACES                                                   
*                                                                               
         TM    GLINDS,GLTOTLIN     SKIP IF THIS IS A TOTAL LINE                 
         BO    OCSHCK#X                                                         
*                                                                               
         CLC   0(6,R2),SPACES                                                   
         BNH   *+10                                                             
         MVC   0(6,R3),0(R2)       PRINT CHECK NUMBER                           
*                                                                               
OCSHCK#X DS    0H                                                               
*                                                                               
         MVC   CSHCHKNM,0(R3)      SAVE CHECK NUMBER                            
*                                                                               
         B     OCSHX                                                            
*                                                                               
*        CLIENT CHECK DATE                                                      
*                                                                               
OCSHCKD  DS    0H                                                               
*                                                                               
         MVC   0(8,R3),SPACES                                                   
*                                                                               
         TM    GLINDS,GLTOTLIN     SKIP IF A TOTAL LINE                         
         BO    OCSHCKDX                                                         
*                                                                               
         OC    0(5,R2),0(R2)       SKIP IF NO DATE                              
         BZ    OCSHCKDX                                                         
*                                                                               
         ST    R3,DMCB+4                                                        
         MVI   DMCB+4,8                                                         
*                                                                               
         TM    OPTIND2,OPTIYMD     FORMAT DATE AS YYMMDD?                       
         BZ    *+12                                                             
         MVI   DMCB+4,0                                                         
         OI    DMCB+4,X'20'        OUTPUT PRINTABLE DATES                       
*                                                                               
         CLI   WDATEOPT,0          IF THERE IS A DATE OPTION                    
         BE    *+10                                                             
         MVC   DMCB+4(1),WDATEOPT     USE IT                                    
*                                                                               
         CLI   2(R2),C'*'          IF NO CHECK RECEIVED YET                     
         BNE   OCSHCKD1                                                         
*                                                                               
         CP    BLLAMTP,=P'0'       AND CREDIT BILL                              
         BNL   OCSHCKD1                                                         
*                                                                               
         MVC   0(2,R2),3(R2)          USE BILLING ACTIVITY DATE                 
*                                                                               
OCSHCKD1 DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,0(R2))                                            
*                                                                               
         CLI   2(R2),C' '          IF SPECIAL INDICATOR EXISTS                  
         BNH   *+10                                                             
         MVC   8(1,R3),2(R2)          PRINT IT                                  
*                                                                               
         XC    CSHACTDT,CSHACTDT   INIT ACTIVITY DATE                           
*                                                                               
         CLC   0(6,R3),SPACES      SKIP IF NO DATE GIVEN                        
         BNH   OCSHCKDX                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(2,0(R2)),(0,CSHACTDT)   ACTIVITY DATE               
*                                                                               
OCSHCKDX DS    0H                                                               
*                                                                               
         B     OCSHX                                                            
*                                                                               
*        CASH RECEIVED PER CENT                                                 
*                                                                               
OCSHPC   DS    0H                                                               
*                                                                               
         XC    CSHPCT,CSHPCT       INIT CASH PER CENT STORAGE AREA              
*                                                                               
         OC    0(8,R2),0(R2)       SKIP IF NO PER CENT AVAILABLE                
         BZ    OCSHPCX                                                          
*                                                                               
         TM    GLINDS,GLTOTLIN     SKIP IF A TOTAL LINE                         
         BO    OCSHPCX                                                          
*                                                                               
         ZAP   DUB,0(8,R2)         COPY PER CENT                                
*                                                                               
         CP    8(8,R2),=P'1'       SKIP IF ONLY ONE OCCURRENCE OF PCT           
         BNH   OCSHPCT1                                                         
*                                                                               
         ZAP   WPKDIVD,0(8,R2)     COPY TOTAL PERCENTAGES                       
         SRP   WPKDIVD,1,0         *10 FOR ROUNDING                             
*                                                                               
         DP    WPKDIVD,8(8,R2)     PERCENT/NUMBER OF TIMES * 10                 
         SRP   WPKQUOT,64-1,5      ROUND                                        
*                                                                               
         ZAP   DUB,WPKQUOT         RETURN PERCENTAGE                            
*                                                                               
OCSHPCT1 DS    0H                                                               
*                                                                               
         XR    R5,R5                                                            
         CP    DUB,=P'2147483647'  MAX   PACKED    NUMBER                       
         BH    OCSHPBIG            HIGH, OVERFLOW                               
         CP    DUB,=P'-2147483648' MIN   PACKED    NUMBER                       
         BL    OCSHPBIG            LOW,  OVERFLOW                               
*                                                                               
         CVB   R5,DUB                                                           
*                                                                               
OCSHPBIG ST    R5,CSHPCT           STORE FOR LATER                              
*                                                                               
         LTR   R5,R5               SKIP PRINTING IF NO PER CENT                 
         BZ    OCSHPCX                                                          
*                                                                               
         LTR   R3,R3               SKIP IF NOT PRINTING                         
         BZ    OCSHPCX                                                          
*                                                                               
         LPR   RF,R5                                                            
         C     RF,=F'100000'       SKIP IF LARGE PERCENT                        
         BH    OCSHPCT2                                                         
*                                                                               
         EDIT  (R5),(7,0(R3)),2,TRAIL=%,FLOAT=-,ZERO=BLANK                      
*                                                                               
         B     OCSHPCX                                                          
*                                                                               
OCSHPCT2 DS    0H                                                               
*                                                                               
         CVD   R5,DUB                                                           
         SRP   DUB,64-2,5          ROUND OUT DECIMALS                           
         CVB   R5,DUB                                                           
*                                  PRINT WITHOUT DECIMALS                       
         EDIT  (R5),(7,0(R3)),0,TRAIL=%,FLOAT=-,ZERO=BLANK                      
*                                                                               
OCSHPCX  DS    0H                                                               
*                                                                               
         B     OCSHX                                                            
*                                                                               
*        CASH APPLIED DOLLARS                                                   
*                                                                               
*        CALCULATED AS CASH PER CENT TIMES BILLED DOLLARS                       
*          GROSS,NET OR ACTUAL DETERMINED WHEN BILLED DOLLARS FOUND             
*                                                                               
*                                                                               
OCSH$    DS    0H                                                               
*                                                                               
         TM    GLINDS,GLTOTLIN     SKIP IF THIS A TOTAL LINE                    
         BO    OCSH$TL             YES                                          
*                                                                               
         ZAP   DUB,BLLAMTP         RETRIEVE BILLED AMOUNT                       
         CVB   RE,DUB                                                           
*                                                                               
         OC    OINVSAVE,OINVSAVE   IF  INVOICE ON PRINT LINE                    
         BZ    OCSH$IV1                                                         
*                                                                               
         CLI   LSQN,1              IF FIRST LINE IN SEQUENCE                    
         BE    OCSH$IV0                                                         
*                                                                               
         CLC   OCSHINV,OINVSAVE    OR INVOICE HAS CHANGED                       
         BE    OCSH$IV1                                                         
*                                                                               
OCSH$IV0 DS    0H                                                               
*                                                                               
         ST    RE,OCSHBLL$            SAVE BILL AMOUNT                          
         MVC   OCSHINV,OINVSAVE       UPDATE INVOICE NUMBER                     
*                                                                               
         B     OCSH$IV2                                                         
*                                                                               
OCSH$IV1 DS    0H                                                               
*                                                                               
         CLI   LSQN,1              IF NOT FIRST LINE IN SEQUENCE                
         BE    *+8                                                              
         L     RE,OCSHBLL$            USE AMOUNT FROM FIRST LINE                
*                                                                               
OCSH$IV2 DS    0H                                                               
*                                                                               
         CVD   RE,DUB              USING DECIMAL INSTRUCTIONS                   
         ZAP   WPKDIVD,DUB         PL16                                         
*                                                                               
         L     RF,CSHPCT                                                        
         CVD   RF,DUB                                                           
         MP    WPKDIVD,DUB                                                      
*                                                                               
         SRP   WPKDIVD,64-4,5      ROUND TO 2 DECIMALS                          
*                                                                               
         ZAP   CSHAMTP,WPKDIVD                                                  
*****                                                                           
*****    SRDA  RE,32                                                            
*****    M     RE,CSHPCT           PER CENT PAID BY CLIENT                      
*****                                                                           
*****    D     RE,=F'10000'        ROUND TO 2 DECIMALS                          
*****    SLL   RE,1                                                             
*****    C     RE,=F'10000'                                                     
*****    BL    *+8                                                              
*****    LA    RF,1(RF)                                                         
*****                                                                           
*****    CVD   RF,CSHAMTP          SAVE CASH PAID                               
*                                                                               
         CP    CSHAMTP,=P'0'       SAVE CASH AMOUNT                             
         BZ    OCSHX               SKIP ZERO AMOUNTS                            
         BM    OCSH$CR             CREDIT AMOUNT                                
*                                                                               
         LA    RF,TBKCSHDB         ==> TO CASH DEBIT TOTAL BUCKETS              
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RF),CSHAMTP INCREMENT ALL LVL BUCKETS                 
         LA    RF,L'TOTBKS(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         B     OCSH$10                                                          
*                                                                               
OCSH$CR  DS    0H                                                               
*                                                                               
         LA    RF,TBKCSHCR         ==> TO CASH CREDIT TOTAL BUCKETS             
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RF),CSHAMTP INCREMENT ALL LVL BUCKETS                 
         LA    RF,L'TOTBKS(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
OCSH$10  DS    0H                                                               
*                                                                               
         CLI   CHKSOPT,C'0'        IF PAYPENDING OPT                            
         BE    OCSHEDIT               LET DRIVER DO THE PRINTING                
*                                                                               
         TM    COLIND,COLIRND      IF NOT ROUNDING                              
         BO    OCSH$11                                                          
*                                                                               
****     EDIT  (R5),(14,0(R3)),2,COMMAS=YES,FLOAT=-,ZERO=BLANK                  
         EDIT  CSHAMTP,(14,0(R3)),2,COMMAS=YES,FLOAT=-,ZERO=BLANK               
*                                                                               
         B     OCSHX                                                            
*                                                                               
OCSH$11  DS    0H                  IF ROUNDING                                  
*                                                                               
*****    EDIT  (R5),(14,0(R3)),0,COMMAS=YES,FLOAT=-,ZERO=BLANK                  
         EDIT  CSHAMTP,(14,0(R3)),0,COMMAS=YES,FLOAT=-,ZERO=BLANK               
*                                                                               
         B     OCSHX                                                            
*                                                                               
*        PRINT TOTAL                                                            
*                                                                               
OCSH$TL  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,GLLEVEL        GET CURRENT TOTAL LEVEL                      
         BZ    *+6                 GRAND TOTAL                                  
         BCTR  RF,0                DECREMENT FOR INDEXING                       
*                                                                               
         LA    RE,L'TOTBKS         BUCKET LENGTH                                
         MR    RE,RE               RF = DISP INTO BUCKETS OF THIS LEVEL         
*                                                                               
         LA    RE,TBKCSHDB         ==> TO CASH DEBIT TOTAL BUCKETS              
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   CSHTOTDB,0(L'TOTBKS,R1)   SAVE  THIS LEVEL DEBIT  TOTAL          
         ZAP   0(L'TOTBKS,R1),=P'0'      CLEAR THIS LEVEL TOTAL                 
*                                                                               
         LA    RE,TBKCSHCR         ==> TO CASH CREDIT TOTAL BUCKETS             
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   CSHTOTCR,0(L'TOTBKS,R1)   SAVE  THIS LEVEL CREDIT TOTAL          
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR THIS LEVEL TOTAL                   
*                                                                               
         ZAP   CSHTOT,CSHTOTDB     ACCUMULATE TOTAL OF DEBITS AND CR'S          
         AP    CSHTOT,CSHTOTCR                                                  
*                                                                               
         CLI   CHKSOPT,C'0'        IF PAYPENDING OPT                            
         BE    OCSHEDIT               LET DRIVER DO THE PRINTING                
*                                                                               
         TM    COLIND,COLIRND      IF NOT ROUNDING                              
         BO    OCSHTL11                                                         
*                                                                               
         EDIT  CSHTOT,(14,0(R3)),2,COMMAS=YES,FLOAT=-,ZERO=BLANK                
*                                                                               
         B     OCSH$TLX                                                         
*                                                                               
OCSHTL11 DS    0H                  IF ROUNDING                                  
*                                                                               
         EDIT  CSHTOT,(14,0(R3)),0,COMMAS=YES,FLOAT=-,ZERO=BLANK                
*                                                                               
OCSH$TLX DS    0H                                                               
*                                                                               
         B     OCSHX                                                            
*                                                                               
OCSHEDIT DS    0H                  LET DRIVER DO THE PRINTING                   
*                                                                               
         MVI   GLHOOK,GLEDIT                                                    
*                                                                               
OCSHX    B     XIT                                                              
*                                                                               
OCSHBLL$ DS    F                   BILL AMOUNT SAVEAREA                         
OCSHINV  DS    CL10                CURRENT INVOICE NUMBER                       
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - CHKMON'               
***********************************************************************         
*                                                                     *         
*        CHECK IF DATE IS VALID FOR MONTH                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CHKMON   NTR1                                                                   
         L     R1,=A(MONTAB1)                                                   
         A     R1,RELO                                                          
*                                                                               
CKM10    CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LDMON+2(2),0(R1)    FIND RIGHT MONTH                             
         BE    CKM20                                                            
         LA    R1,4(R1)            BUMP TABLE                                   
         B     CKM10                                                            
*                                                                               
CKM20    CLC   LDMON+4(2),2(R1)    IS THE REQUESTED DATE                        
         BNH   CKMX                <= LAST DATE OF MONTH                        
         MVC   LDMON+4(2),2(R1)    NO - SET TO LAST DATE OF MONTH               
*                                                                               
CKMX     B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - PUTSRT'               
***********************************************************************         
*                                                                     *         
*        PUT RECORD TO SORT                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PUTSRT   DS    0H                                                               
*                                                                               
         CLI   CHKLINE,C'N'        IF THERE IS NO CHECK NUMBER                  
         BNE   *+8                                                              
         MVI   GLHOOK,GLDONT          DONT PUT RECORD TO SORT                   
*                                                                               
PUTSRTX  B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - PRINT'                
***********************************************************************         
*                                                                     *         
*        PRINT A LINE                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRINT    DS    0H                                                               
*                                                                               
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE                         
         BO    PR30                YES                                          
*                                                                               
PR05     CLC   CHKNUM(4),=C'VOID'  IF VOID - DONT PRINT                         
         BNE   PR10                                                             
*                                                                               
         XC    CHKNUM,CHKNUM       CLEAR CHECK NUMBER                           
         XC    ACTDT,ACTDT                                                      
*                                                                               
         B     PRX                ON SECOND THOUGHT, OKAY TO PRINT              
*                                                                               
PR10     DS    0H                                                               
*                                                                               
         CLI   CHKSOPT,C'0'        IF PAY PENDING OPTION                        
         BNE   PR11                                                             
*                                                                               
         TM    GLDWNLD2,GLDLHDLS      IF DOWNLOADING HEADLINES                  
         BO    PR11                      SKIP BACKOUT CHECK                     
*                                                                               
         CLI   LINESTAT,C'P'       IF NOT PRINTING                              
         BE    *+8                                                              
         MVI   GLHOOK,GLBCKOUT        BACKOUT LINE FROM REPORT                  
*                                                                               
PR11     DS    0H                                                               
*                                                                               
         B     PRX                 PRINT                                        
*                                                                               
PR20     MVI   GLHOOK,GLDONT       ELSE DONT PRINT THE LINE                     
         B     PRX                 PRINT                                        
*                                                                               
PR30     DS    0H                  TOTAL LINES                                  
*                                                                               
         CLI   CHKSOPT,C'0'        IF PAY PENDING OPTION                        
         BNE   PR40                                                             
*                                                                               
         CLI   LINESTAT,C'T'       IF NOT PRINTING                              
         BNE   PR40                                                             
*                                                                               
         MVI   GLHOOK,GLBCKOUT        BACKOUT LINE FROM REPORT                  
         MVI   LINESTAT,C' '                                                    
*                                                                               
PR40     DS    0H                                                               
*                                                                               
PRX      B     XIT                                                              
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - FIRSTS'               
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK FIRSTS                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FIRSTS   DS    0H                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - LASTS'                
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK LASTS                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LASTS    DS    0H                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - HEAD'                 
***********************************************************************         
*                                                                     *         
*        DRIVER HEADHOOK                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HEAD     L     R1,AH4                                                           
         LA    R1,1(R1)                                                         
         LR    RE,R1                                                            
         A     RE,PWIDTH                                                        
         CLI   0(RE),C' '          TEST WHETHER HEADS'VE BEEN FORMATTED         
         BH    HD1                 YES                                          
         MVC   0(50,R1),SPACES     NO-REMOVE MEDIA FROM FIRST HEADLINE          
         B     HDX                    AND EXIT                                  
*                                                                               
HD1      L     R5,=A(HEADTAB)                                                   
         USING HEADTABD,R5                                                      
         LA    R3,96               R3=DISPLACEMENT TO RHS                       
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         LA    R3,128                                                           
         L     R2,AH4                                                           
         A     R2,PWIDTH           R2=A(HEAD5)                                  
         LA    R5,0(R3,R2)                                                      
         A     R2,PWIDTH           R2=A(HEAD6 OR HEAD7)                         
*                                                                               
HDX      B     XIT                                                              
         SPACE 1                                                                
HDPOS    L     R1,AH4                                                           
         SH    R2,=H'4'                                                         
         BNP   *+12                                                             
         A     R1,PWIDTH                                                        
         BCT   R2,*-4                                                           
         LA    RF,0(R3,R1)                                                      
         BR    RE                                                               
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - FIN'                  
***********************************************************************         
*                                                                     *         
*        FINAL HOOK                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FIN      ICM   R3,15,BPATAB        FREE ANY GETMAINS HERE                       
         BZ    FINX                                                             
         L     R4,BPLTAB                                                        
         GOTO1 COVAIL,DMCB,C'FREE',(R3),(R4)                                    
*                                                                               
FINX     B     XIT                                                              
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - ERROR'                
***********************************************************************         
*                                                                     *         
*        ERROR EXITS AND MESSAGES                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
*                                                                               
         TITLE 'SPWRI13 - BILLING DOLLARS - INPUT - IBLL'                       
***********************************************************************         
*                                                                     *         
*        BILL RECORD DATA FIELDS                                      *         
*                                                                     *         
*NTRY    GLARGS     C'A'  - ACTUAL DOLLARS                            *         
*                   C'G'  - GROSS  DOLLARS                            *         
*                   C'N'  - NET    DOLLARS                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IBLL     DS    0H                                                               
*                                                                               
         L     R1,GLADTENT                                                      
         USING DRIND,R1            ESTABLISH DRIVE TABLE ENTRY                  
*                                                                               
         TM    COLIND,COLIRND      IF ROUNDING                                  
         BNO   *+8                                                              
         MVI   DRINDEC,0              NO DECIMALS FOR ROUNDING                  
*                                                                               
         CLI   SBMODE,SBPROCBL     SKIP IF NOT PROCESSING STA BLLS              
         BNE   IBLLX                                                            
*                                                                               
         L     R3,SBACURCH         ESTABLISH STATION BILL CHUNK                 
         USING STABELEM,R3                                                      
*                                                                               
         SR    R1,R1                                                            
*                                                                               
         CLI   GLARGS,C'A'         GET ACTUAL BILLING                           
         BNE   *+12                                                             
         ICM   R1,15,BILLCOST                                                   
         B     IBILL10                                                          
*                                                                               
         CLI   GLARGS,C'G'         GET GROSS  BILLING                           
         BNE   *+12                                                             
         ICM   R1,15,SBBILGRS                                                   
         B     IBILL10                                                          
*                                                                               
         CLI   GLARGS,C'N'         GET NET    BILLING                           
         BNE   *+12                                                             
         ICM   R1,15,SBBILNET                                                   
         B     IBILL10                                                          
*                                                                               
IBILL10  DS    0H                                                               
*                                                                               
         TM    COLIND,COLIRND      IF ROUNDING                                  
         BNO   *+18                                                             
         CVD   R1,DUB                 DIVIDE BY 100                             
         SRP   DUB,64-2,5                                                       
         CVB   R1,DUB                                                           
*                                                                               
****     STCM  R1,15,0(R2)         RETURN DOLLARS                               
*                                                                               
         CVD   R1,DUB              CVD                                          
         ZAP   SVBLLAMT,DUB        SAVE BILLED DOLLARS                          
         ZAP   0(8,R2),DUB         RETURN BILLED DOLLARS                        
*                                                                               
IBLLX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'SPWRI13 - CASH APPLIED DATA - INPUT - ICSH'                     
***********************************************************************         
*                                                                     *         
*        CASH APLIED DATA FIELDS                                      *         
*                                                                     *         
*NTRY    GLARGS     C'Q'  - DATA IN CASH APPLIED ELEMENTS             *         
*          GLARGS+1    C'D'  - CASH DATE                              *         
*            GLARGS+2     C'C' -  CLIENT CHECK DATE                   *         
*                         C'D' -  CLIENT CHECK DEPOSIT DATE           *         
*                         C'A' -  CASH   APPLIED       DATE           *         
*                      C'$'  - CASH AMOUNT                            *         
*                      C'%'  - CASH APPLIED PERCENT                   *         
*                      C'#'  - CLIENT CHECK NUMBER                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
ICSH     DS    0H                                                               
*                                                                               
*        HANDLE DATA IN CASH APPLIED ELEMENTS                                   
*                                                                               
         L     R1,GLADTENT                                                      
         USING DRIND,R1            ESTABLISH DRIVE TABLE ENTRY                  
*                                                                               
         TM    COLIND,COLIRND      IF ROUNDING                                  
         BNO   *+8                                                              
         MVI   DRINDEC,0              NO DECIMALS FOR ROUNDING                  
*                                                                               
         L     R4,AGLOBAL          ** INPUT ROUTINE **                          
         USING GLOBALD,R4                                                       
*                                                                               
         CLI   SBMODE,SBPROCBL     SKIP IF  NOT BILL                            
         BE    *+8                                                              
         CLI   SBMODE,SBPROCCA          AND NOT CASH APPLIED                    
         BNE   ICSHX                                                            
*                                                                               
         CLI   GLARGS,C'Q'         EXIT IF NOT CASH APPLIED FIELD               
         BNE   ICSHX                                                            
*                                                                               
         ICM   R5,15,SVMBTELA      ESTABLISH MEDIA TRANSFER ELEMENT             
         BZ    ICSHX                  SKIP IF DATA MISSING                      
*                                                                               
         USING MBTELD,R5                                                        
*                                                                               
         ICM   R7,15,SVTRNELA      ESTABLISH CASH TRANSACTION ELEMENT           
         BZ    ICSHDATA               SKIP IF DATA MISSING                      
*                                                                               
         USING TRNELD,R7                                                        
*                                                                               
*        FIND FOLLOWING RECEIVABLE ALLOCATION ELEMENT                           
*                                                                               
         LR    R3,R7               FIND FOLLOWING RECEIVABLE ALLOCATION         
         SR    RF,RF                                                            
*                                                                               
ICSHRALL DS    0H                                                               
*                                                                               
         USING RALELD,R3           ESTABLISH RECEIVABLE ALLOC ELM               
*                                                                               
         IC    RF,TRNLN            ELEMENT LENGTH                               
         LA    R3,0(RF,R3)         NEXT ELEMENT                                 
*                                                                               
         CLI   RALEL,0             DONE IF END OF RECORD REACHED                
         BE    ICSHRALD                                                         
*                                                                               
         CLI   RALEL,TRNELQ        DONE IF NEW TRANSACTION ELM REACHED          
         BE    *+8                                                              
         CLI   RALEL,X'FF'         DONE IF NEW TRANSACTION ELM REACHED          
         BE    ICSHRALD                                                         
*                                                                               
         CLI   RALEL,RALELQ        FIND RECEIVABLE ALLOC ELM                    
         BNE   ICSHRALC                                                         
*                                                                               
         CLI   RALTYPE,RALTALC     FIND REGULAR ALLOCATION ELEMENT              
         BE    ICSHRALF                                                         
*                                                                               
         CLI   RALTYPE,RALTOFS     FIND OFFSET                                  
         BE    ICSHRALF                                                         
*                                                                               
         CLI   RALTYPE,RALTWOF     FIND WRITE OFF                               
         BE    ICSHRALF                                                         
*                                                                               
ICSHRALC DS    0H                                                               
*                                                                               
         B     ICSHRALL                                                         
*                                                                               
ICSHRALD DS    0H                                                               
*                                                                               
         SR    R3,R3               NO RECEIVABLE ALLOC ELEMENT FOUND            
*                                                                               
ICSHRALF DS    0H                                                               
*                                                                               
ICSHDATA DS    0H                                                               
*                                                                               
*        DETERMINE DATA WANTED                                                  
*                                                                               
         CLI   GLARGS+1,C'D'       CASH DATES OF VARIOUS SORTS                  
         BE    ICSHCDT                                                          
*                                                                               
         LTR   R7,R7               DONE IF NO TRANSACTION ELEMENT               
         BZ    ICSHX                                                            
*                                                                               
         CLI   GLARGS+1,C'%'       CASH PER CENT                                
         BE    ICSHPCT                                                          
*                                                                               
         CLI   GLARGS+1,C'#'       CLIENT CHECK NUMBER                          
         BE    ICSHCHK                                                          
*                                                                               
*        ELSE MUST BE DOLLAR AMOUNT                                             
*                                                                               
         B     ICSHPCT             GET PER CENT RECEIVED                        
*                                                                               
*        VARIOUS DATES                                                          
*                                                                               
ICSHCDT  DS    0H                                                               
*                                                                               
         CLI   GLARGS+2,C'A'       IF CASH APPLIED DATE                         
         BNE   ICSHCDT1                                                         
*                                                                               
         LTR   R7,R7               SKIP IF NO CASH APPLIED YET                  
         BZ    *+8                                                              
         CLI   TRNEL,X'FF'         OR UNAPPLIED CASH ELEM                       
         BE    ICSHCDT5                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(1,TRNDATE),(2,(R2)) TRANSACTION DATE                
*                                                                               
         B     ICSHCDT5                                                         
*                                                                               
ICSHCDT1 DS    0H                                                               
*                                                                               
         LTR   R7,R7               SKIP IF NO TRNSACTION ELEMENT                
         BZ    ICSHCDT5                                                         
*                                                                               
         LTR   R3,R3               SKIP IF NO RECEIVABLE ALLOC AVAIL            
         BZ    ICSHCDT5                                                         
*                                                                               
         CLI   RALTYPE,RALTOFS     IF OFFSET                                    
         BNE   ICSHCDTA                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(1,RALODAT),(2,(R2))  OFFSET DATE                    
         MVI   2(R2),C'O'             SET OFFSET INDICATOR                      
*                                                                               
         B     ICSHCDT5                                                         
*                                                                               
ICSHCDTA DS    0H                                                               
*                                                                               
         CLI   RALTYPE,RALTWOF     IF WRITE OFF                                 
         BNE   ICSHCDTB                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(1,RALWDAT),(2,(R2))  WRITE OFF DATE                 
         MVI   2(R2),C'W'             SET WRITE OFF INDICATOR                   
*                                                                               
         B     ICSHCDT5                                                         
*                                                                               
ICSHCDTB DS    0H                                                               
*                                                                               
         CLI   GLARGS+2,C'C'       IF CLIENT CHECK DATE                         
         BNE   ICSHCDT2                                                         
*                                                                               
         CLI   MOFLOW,C'Y'         MONEYFLOW OPTION?                            
         BE    *+12                YES                                          
         CLI   SBWRPROF+10,C'Y'    SKIP IF DEPOSIT DATE WANTED                  
         BE    ICSHCDT3                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(1,RALADAT),(2,(R2)) CLIENT CHECK DATE               
*                                                                               
         B     ICSHCDT5                                                         
*                                                                               
ICSHCDT2 DS    0H                                                               
*                                                                               
         CLI   GLARGS+2,C'D'       IF CHECK DEPOSIT DATE                        
         BNE   ICSHCDT4                                                         
*                                                                               
*        SWAPS CLIENT AND DEPOSIT DATES IF ADD CALCULATED                       
*              OFF DEPOSIT DATE                                                 
*                                                                               
         CLI   MOFLOW,C'Y'         MONEYFLOW OPTION?                            
         BE    ICSHCDT3            YES                                          
         CLI   SBWRPROF+10,C'Y'    SKIP IF DEPOSIT DATE USED                    
         BNE   ICSHCDT3                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(1,RALADAT),(2,(R2)) CLIENT CHECK DATE               
*                                                                               
         B     ICSHCDT5                                                         
*                                                                               
ICSHCDT3 DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(1,RALADEP),(2,(R2)) CHECK DEPOSITED DATE            
*                                                                               
         B     ICSHCDT5                                                         
*                                                                               
ICSHCDT4 DS    0H                                                               
*                                                                               
ICSHCDT5 DS    0H                                                               
*                                                                               
         CLI   CHKSOPT,C'0'        SKIP IF PAYPEND OPTION                       
         BE    ICSHCDT7                                                         
*                                                                               
         LTR   R3,R3               IF NO CHECK ELEMENT                          
         BZ    *+6                                                              
         LTR   R7,R7               IF NO TRANSACTION ELEMENT                    
         BZ    *+8                                                              
         CLI   TRNEL,X'FF'         OR UNAPPLIED CASH ELEM                       
         BNE   ICSHCDT7                                                         
*                                                                               
         MVC   0(2,R2),SBBTODAY       USE TODAY'S DATE                          
*                                                                               
         CLI   MOFLOW,C'Y'            MONEYFLOW OPTION?                         
         BE    *+12                   YES                                       
         CLI   GLARGS+2,C'C'          IF AGENCY CHECK DATE                      
         BNE   *+10                                                             
         MVC   3(2,R2),BACTDATE          PASS INVOICE DATE                      
*                                                                               
         MVI   2(R2),C'*'             SET UNAPPLIED INDICATOR                   
*                                                                               
ICSHCDT7 DS    0H                                                               
*                                                                               
ICSHCDTX DS    0H                                                               
*                                                                               
         B     ICSHX               DONE                                         
*                                                                               
*              CASH PER CENT RECEIVED                                           
*                                                                               
ICSHPCT  DS    0H                                                               
*                                                                               
*        CALCULATE PERCENTAGE OF CASH APPLIED                                   
*                                                                               
*        DETERMINE TOTAL AMOUNT POSTED                                          
*                                                                               
         ZAP   ICSHPOST,=P'0'      INIT POSTED AMOUNT                           
         ZAP   ICSHSPCT,=P'0'      INIT PER CENT                                
*                                                                               
         LTR   R1,R5               POINT TO POSTING ELEMENT                     
         BZ    ICSHPCPX            SKIP IF NONE FOUND                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,MBTLLN-MBTELD(R1)                                             
         LA    R1,0(RF,R1)         POINT TO FOLLOWING ELEMENTS                  
*                                                                               
ICSHPCPL DS    0H                                                               
*                                                                               
         CLI   MBTEL-MBTELD(R1),0  DONE AT END OF ELEMENTS                      
         BE    ICSHPCPD                                                         
*                                                                               
         CLI   MBTEL-MBTELD(R1),MBTELQ  DONE AT NEXT POSTING ELEMENT            
         BE    ICSHPCPD                                                         
*                                                                               
         CLI   MBTEL-MBTELD(R1),TRNELQ   SKIP IF NOT A TRANSACTION ELM          
         BNE   ICSHPCPC                                                         
*                                                                               
         TM    TRNSTAT-TRNELD(R1),TRNSDR    SKIP IF NOT A DEBIT                 
         BNO   ICSHPCPC                                                         
*                                                                               
         AP    ICSHPOST,TRNAMNT-TRNELD(L'TRNAMNT,R1)  ACCUMULATE DEBITS         
*                                                                               
ICSHPCPC DS    0H                                                               
*                                                                               
         IC    RF,MBTLLN-MBTELD(R1)                                             
         LA    R1,0(RF,R1)         POINT TO NEXT ELEMENT                        
         B     ICSHPCPL                                                         
*                                                                               
ICSHPCPD DS    0H                                                               
*                                                                               
ICSHPCPX DS    0H                                                               
*                                                                               
         CP    ICSHPOST,TRNAMNT    IF CASH =POSTING                             
         BNE   *+14                                                             
         ZAP   ICSHSPCT,=PL4'100000'  RETURN 100%                               
         B     ICSHPCT1                                                         
*                                                                               
         CP    ICSHPOST,=P'0'      SKIP IF NOTHING POSTED                       
         BE    ICSHPCT1                                                         
*                                                                               
         ZAP   WPKDIVD,TRNAMNT     COPY CASH APPLIED                            
         SRP   WPKDIVD,6,0         *100.0000                                    
*                                                                               
         ZAP   DUB,ICSHPOST        FORCE FIELD LENGTH OF 8                      
         DP    WPKDIVD,DUB         CASH/BILL*100.0000                           
         SRP   WPKQUOT,64-1,5      ROUND TO 3 DECIMALS                          
*                                                                               
         ZAP   ICSHSPCT,WPKQUOT    RETURN PERCENTAGE                            
*                                                                               
ICSHPCT1 DS    0H                                                               
*                                                                               
         CLI   TRNEL,X'FF'         IF UNAPPLIED CASH ELEM                       
         BNE   ICSHPCT8                                                         
*                                                                               
         CLI   CHKSOPT,C'0'        AND PAYPEND OPTION                           
         BNE   ICSHPCT8                                                         
*                                                                               
         ZAP   ICSHSPCT,=P'0'          KILL PERCENT                             
*                                                                               
ICSHPCT8 DS    0H                                                               
*                                                                               
         CLC   GLARGS(2),=C'Q%'    IF PURE PCT                                  
         BNE   ICSHPCT9                                                         
*                                                                               
         ZAP   0(8,R2),ICSHSPCT       RETURN PER CENT                           
         SRP   0(8,R2),64-1,5         ROUND                                     
         ZAP   8(8,R2),=P'1'          SET COUNTER                               
*                                                                               
         B     ICSHAMTX                                                         
*                                                                               
ICSHPCT9 DS    0H                  MUST BE A DOLLAR AMOUNT                      
*                                                                               
         CLI   CHKSOPT,C'0'        SKIP IF NOT PAYPENDING                       
         BNE   ICSHAMTX                                                         
*                                                                               
         TP    SVBLLAMT            ID NOT PACKED FIELD                          
         BZ    *+10                                                             
         ZAP   SVBLLAMT,=P'0'         FORCE TO ZERO                             
*                                                                               
         ZAP   WPKDIVD,SVBLLAMT    BILLED DOLLARS                               
         MP    WPKDIVD,ICSHSPCT    * PER CENT                                   
         SRP   WPKDIVD,64-5,5      ROUND                                        
*                                                                               
         ZAP   0(8,R2),WPKDIVD     RETURN AMOUNT                                
*                                                                               
ICSHAMTX DS    0H                                                               
         B     ICSHX                                                            
*                                                                               
*        CLIENT CHECK NUMBER                                                    
*                                                                               
ICSHCHK  DS    0H                                                               
*                                                                               
         LTR   R3,R3               SKIP IF NO RECEIVABLE ALLOC AVAIL            
         BZ    ICSHCHKX                                                         
*                                                                               
         CLI   RALTYPE,RALTALC     IF AN ALLOCATION ELEMENT                     
         BNE   ICSHCHKA                                                         
*                                                                               
         MVC   0(6,R2),RALAREF        RETURN CLIENT CHECK NUMBER                
*                                                                               
         B     ICSHCHKX                                                         
*                                                                               
ICSHCHKA DS    0H                                                               
*                                                                               
         CLI   RALTYPE,RALTOFS     IF OFFSET ELEMENT                            
         BNE   ICSHCHKB                                                         
*                                                                               
         MVC   0(6,R2),=C'OFFSET'     RETURN 'OFFSET'                           
*                                                                               
         B     ICSHCHKX                                                         
*                                                                               
ICSHCHKB DS    0H                                                               
*                                                                               
         CLI   RALTYPE,RALTWOF     IF WRITE OFF                                 
         BNE   ICSHCHKC                                                         
*                                                                               
         MVC   0(6,R2),RALWREF        RETURN WRITE OFF REF #                    
*                                                                               
         B     ICSHCHKX                                                         
*                                                                               
ICSHCHKC DS    0H                                                               
*                                                                               
ICSHCHKX DS    0H                                                               
*                                                                               
         B     ICSHX               DONE                                         
*                                                                               
ICSHX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
ICSHPOST DS    PL8                 CASH APPLIED POSTED AMOUNT                   
ICSHSPCT DS    PL8                 CASH APPLIED PER CENT SAVEAREA               
ICSHWORK DS    CL32                CASH APPLIED WORKAREA                        
*                                                                               
NARRD    DS    0D                  NARRATIVE DSECT                              
         DC    C'CHECK NUMBER '                                                 
NARCHK#  DS    CL6                 CLIENT CHECK NUMBER                          
         DC    C' DATED '                                                       
NARCHKDT DS    CL8                 CLIENT CHECK DATE                            
         DC    C' DEPOSITED ON '                                                
NARDEPDT DS    CL8                 CLIENT CHECK DEPOSIT DATE                    
*                                                                               
         DROP  R4,R5,R7                                                         
*                                                                               
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - INIT'                 
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
INIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
*        IF OFF-LINE, LOAD OVERLAY TO HOLD TABLES                               
*                                                                               
         XC    DMCB(4),DMCB                                                     
         MVI   DMCB,X'1A'          OVERLAY CONTAINING TABLES                    
*                                                                               
         GOTO1 CALLOV,DMCB,,0,0                                                 
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                NEED TO FIND OVERLAY                         
*                                                                               
         L     RF,0(R1)            RETRIEVE OVERLAY ADDRESS                     
         USING CSHTBLD,RF          ESTABLISH DIRECTORY FOR OVERLAY              
*                                                                               
*        RELOCATE ADDRESSES IN TABLES OVERLAY                                   
*                                                                               
         LA    R1,CSHTBLAS         POINT TO START OF ADDRESS TABLE              
         LA    R2,CSHTBLVS         POINT TO ADDRESSES SAVEAREA                  
*                                                                               
INITTBLP DS    0H                                                               
*                                                                               
         ICM   RE,15,0(R1)         GET TABLE DISPLACEMENT                       
         BZ    INITTBDN            END OF TABLE                                 
*                                                                               
         AR    RE,RF               RELOCATE ADDRESS                             
         ST    RE,0(R2)            SAVE ADDRESS                                 
*                                                                               
INITTBCN DS    0H                                                               
*                                                                               
         LA    R1,4(R1)            BUMP POINTERS                                
         LA    R2,4(R2)                                                         
*                                                                               
         B     INITTBLP                                                         
*                                                                               
INITTBDN DS    0H                                                               
*                                                                               
         L     RE,VSTBTBL          GET A(BILL TABLE)                            
         USING ICSHSTTD,RE         ESTABLISH BILL TABLE                         
*                                                                               
         XC    ICSHSTTD(ICSHSTLQ),ICSHSTTD  INIT TABLE                          
*                                                                               
         L     RE,VINVTBL          GET A(INVOICE TABLE)                         
*                                                                               
         USING ICSHIVTD,RE         ESTABLISH INVOICE TABLE                      
         XC    ICSHIVTD(ICSHIVLQ),ICSHIVTD  INIT TABLE                          
*                                                                               
         DROP  RE,RF                                                            
*                                                                               
INITOVLX DS    0H                                                               
*                                                                               
         ZAP   DOLTOT,=P'0'                                                     
         ZAP   CLRTOT,=P'0'                                                     
         ZAP   CLRTOTDB,=P'0'                                                   
         ZAP   CLRTOTCR,=P'0'                                                   
         ZAP   BLLTOT,=P'0'                                                     
         ZAP   BLLTOTDB,=P'0'                                                   
         ZAP   BLLTOTCR,=P'0'                                                   
         ZAP   CSHTOT,=P'0'                                                     
         ZAP   CSHTOTDB,=P'0'                                                   
         ZAP   CSHTOTCR,=P'0'                                                   
         ZAP   TCDOLDB,=P'0'                                                    
         ZAP   TCDOLCR,=P'0'                                                    
         ZAP   TBDOLDB,=P'0'                                                    
         ZAP   TBDOLCR,=P'0'                                                    
         ZAP   BLLAMTP,=P'0'                                                    
*                                                                               
*        INIT ALL TOTALLING BUCKETS                                             
*                                                                               
         LA    RF,TOTBKS                 START OF BUCKETS                       
         LA    R0,TOTNMBKQ         TOTAL NUMBER OF BUCKETS                      
*                                                                               
         ZAP   0(L'TOTBKS,RF),=P'0' INIT BUCKET                                 
         LA    RF,L'TOTBKS(RF)     BUMP BUCKET                                  
         BCT   R0,*-10                                                          
*                                                                               
         CLI   SBQNETWK,0                                                       
         BNE   *+8                                                              
         MVI   SBQNETWK,C'N'                                                    
*                                                                               
         OI    SBQSKIP,SBQSKBIL+SBQSKGL   SKIP STATION BILL+GOAL                
         OI    SBQREAD,SBQRDCLS    READ CLEARANCE RECORDS                       
*                                                                               
         CLI   SBQMED,C'N'         FOR NETWORK                                  
         BE    INIT10                                                           
*                                                                               
         CLI   SBQMED,C'C'         & COMBINED                                   
         BNE   INIT10                                                           
*                                                                               
*NO-OP   MVI   SBQNETWK,C'L'       SET ONLY LOCAL NETWORKS                      
*                                                                               
INIT10   MVI   SBQPER,SBQPMN       SET PERIOD TO MONTH                          
         MVI   MONLO,1                                                          
         MVI   MONHI,24                                                         
*                                                                               
         CLI   WGTOPT,0            IF MARKET WEIGHTING OPTION NOT SET,          
         BNE   *+8                 TURN IT OFF                                  
         MVI   WGTOPT,C'N'                                                      
*                                                                               
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INIT70                                                           
*                                                                               
         TM    WHEN,X'20'          IF THIS IS SOON                              
         BNO   INIT20                                                           
*                                                                               
         OC    SBBCLT,SBBCLT       ALL CLIENTS IS INVALID                       
         BZ    ECLT                                                             
*                                                                               
INIT20   MVI   BILLOPT,C'N'        DEFAULT - NO BILLING                         
         MVI   CASHOPT,0                     NO CASH DATA                       
         MVI   VENDOPT,C'N'                  NO VENDOR DATA                     
         MVI   XPCTOPT,0                     DON'T DROP CLIENT % PAID           
         MVI   BANKOPT,0                     PRINT CHECK DEPOSIT DATE           
         MVI   INVDTOPT,C'N'                 NO INVOICE DATE                    
         MVI   CHKSOPT,C'N'                  NO CHECK DATA SUPPRESSED           
         MVI   CHKIVOPT,C'N'                 INIT CHECK INVOICE OPTION          
         MVI   DOLLAR,C'N'                   NET DOLLARS                        
         MVI   DTOPT,1                       INV DATE                           
         MVI   DAYS,C'N'                     NO DAYS                            
         MVI   SPOTOPT,C'N'                  SHOW N'SPOTS                       
         MVI   ORDERED,C' '                  ORDERED                            
         MVI   BLNK,0                        BLANK LINE                         
         MVI   CALC,C'N'                                                        
         MVI   PSUBT,C'Y'                                                       
         MVI   SVCHKTAB,X'FF'      INIT CHECK NUMBER TABLE                      
         MVI   WDATEOPT,0          NO SPECIAL DATE OPTIONS                      
*                                                                               
         OC    SBWRPROF+4(2),SBWRPROF+4      IF NOT 00 - SET EXTRA COL          
         BZ    *+8                                                              
         MVI   CALC,C'Y'                                                        
*                                                                               
INIT70   MVI   MYFIRSTH,12         SET DRIVER'S FIRST HEADLINE                  
         LA    R1,LEVELS           WHAT LEVEL IS QPER (CFMON) AT                
         LA    R2,12                                                            
         LA    R3,1                                                             
*                                                                               
INIT80   CLI   0(R1),QPER          SET LEVEL OF CFMON                           
         BE    INIT90                                                           
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R2,INIT80                                                        
*                                                                               
INIT90   STC   R3,QPERLEV          SET THE LEVEL THAT QPER IS AT                
*                                                                               
INITX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
ECLT     MVC   CONHEAD(49),=C'TOO MANY CLIENTS IN REQUEST. PLEASE RUN OX        
               VERNIGHT'                                                        
         LA    R2,SCACLTH                                                       
*                                                                               
         MVI   ERROR,X'FE'                                                      
         GOTOR CURSERR                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - MVDPG'                
***********************************************************************         
*                                                                     *         
*        MOVE DPG CODE AT ADPGPROG TO END OF WHAT DRONE JUST          *         
*        VALIDATED (DRCUFBUF)                                         *         
*                                                                     *         
* DRONEBLK MOVED IN RE-WORKED VERSION OF SPWRI (EJOR - 13JUL94)       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
MVDPG    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,4095(R9)                                                      
         LA    R5,1(R5)                                                         
         USING SYSD+4096,R5                                                     
*                                                                               
         L     R3,DRCURBUF         END OF BUFF-WHAT WAS JUST VALIDATED          
         L     R2,ADPGPROG         START OF DPG CODE                            
*                                                                               
MV10     CLI   0(R2),0             END OF CODE                                  
         BE    MV100                                                            
         ZIC   R1,1(R2)            L'ELEMENT                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)                                                    
         LA    R1,1(R1)            ADD 1 FOR BCTR                               
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         AR    R3,R1               BUMP TO NEXT AREA                            
         B     MV10                                                             
*                                                                               
MV100    MVC   ADPGPROG,DRSTBUF    SET START OF DPG CODE                        
         ST    R2,DRCURBUF                                                      
         AH    R2,=H'3'                                                         
         ST    R2,DRENDBUF                                                      
*                                                                               
MVDPGX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - CSHINIT'              
***********************************************************************         
*                                                                     *         
*                                                                     *         
*        INITIALIZE DDCASHIER CONTROL BLOCK                           *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
CSHINIT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         L     RF,VCLTREC          SET A(CLTREC)                                
         ST    RF,SBACLTRC                                                      
*                                                                               
                                                                                
         CLI   OFFLINE,C'Y'     IF OFF-LINE OPEN CONTROL SYSTEM FILES           
         BNE   CSHIUTLX                                                         
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A00'                                           
*                                                                               
         MVI   DMCB+7,QTSAROFF     GET A(TSAROFF)                               
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   VTSAROFF,DMCB                                                    
*                                                                               
         MVI   DMCB+7,QCASHIER     GET A(CASHIER)                               
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   VCASHIER,DMCB                                                    
*                                                                               
         L     RF,ATWA             POINT TO TWA                                 
         L     RF,TWAMASTC-TWATASK(RF) POINT TO MASTC                           
         L     RF,MCUTL-MCBLOCK(RF)  POINT TO UTL                               
         ST    RF,SVUTLA           SAVE ADDRESS                                 
*                                                                               
CSHIUTLX DS    0H                                                               
*                                                                               
         LA    RF,SBLOCK           ESTABLISH END OF SPOTBLOCK                   
         USING SBLOCK,RF                                                        
*                                                                               
         ICM   R4,15,SBACSHRC      SKIP IF INITIALIZATION DONE                  
         BNZ   CSHINITX                                                         
*                                                                               
         DROP  RF                                                               
*                                                                               
         LA    R4,CSHIERC          POINT TO CASHIER CONTROL BLOCK               
         USING CSHIERD,R4          ESTABLISH AREA                               
         XC    CSHIERD(CSHIERL),CSHIERD INIT CONTROL BLOCK                      
*                                                                               
         MVI   CSHACT,CSHINIQ      SET TO INITIALIZE                            
*                                                                               
         MVC   CSHAGYCH,SBAGY      SET AGENCY ALPHA                             
         MVI   CSHSYS,CSHSPTQ      SET TO SPOT SYSTEM                           
*                                                                               
         MVC   CSHBLLL,=Y(256)     MAX BILL HEADER RECORD LENGTH                
         MVI   CSHBLLKL,L'BKEY     BILL RECORD KEY LENGTH                       
*                                                                               
         MVC   CSHMAX,=F'3000'     MAX NUMBER OF BILL RECORDS                   
*                                                                               
         TM    WHEN,X'20'          IF THIS IS SOON                              
         BNO   *+10                                                             
         MVC   CSHMAX,=F'2000'        RESET MAX NUMBER OF BILL RECORDS          
*                                                                               
         OI    CSHCTL,CSHCBLLQ     INDICATE BILL DATA WANTED                    
*                                                                               
         OI    CSHCTL,CSHCCSHQ     INDICATE CASH DATA WANTED                    
*                                                                               
         MVC   CSHBLLA,SBAIO2      SET A(BILLREC)                               
         MVC   CSHCLTA,SBACLTRC    SET A(CLTREC)                                
*                                                                               
         L     RF,SBCOMFAC         COMFACS ADDRESS                              
         MVC   CSHDMGRA,CDATAMGR-COMFACSD(RF)   DATAMGR ADDRESS                 
         MVC   CSHGTPRA,CGETPROF-COMFACSD(RF)   GETPROF ADDRESS                 
         MVC   CSHDATCA,DATCON                  DATCON  ADDRESS                 
*                                                                               
         MVC   CSHTSARA,VTSAROFF   PASS V(TSAROFF)                              
         MVC   CSHUTLA,SVUTLA      PASS V(UTL)                                  
*                                                                               
         GOTO1 VCASHIER,DMCB,CSHIERD   INIT DDCASHIER                           
         CLI   CSHERR,0            NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CSHINITX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'SPWRI13 - CASH APPLIED DATA - INPUT - HCSH'                     
***********************************************************************         
*                                                                     *         
*        CASH APPLIED DATE HEADER ROUTINE                             *         
*                                                                     *         
*NTRY    GLARGS     C'N'  - HEADLINE NUMBER                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
HCSH     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AGLOBAL          ** INPUT ROUTINE **                          
         USING GLOBALD,R4                                                       
*                                                                               
         CLI   GLARGS,C'2'        SKIP IF NOT SECOND HEADLINE                   
         BNE   HCSHX                                                            
*                                                                               
         CLI   GLARGS+1,C'D'       SKIP IF FOR DEPOSIT DATE                     
         BE    HCSH1                                                            
*                                                                               
         MVC   0(5,R3),=C'CHECK'   DEFAULT TO HEADLINE OF 'CHECK'               
*                                                                               
         CLI   MOFLOW,C'Y'         MONEYFLOW OPTION?                            
         BE    HCSHX               YES                                          
         CLI   SBWRPROF+10,C'Y'    IF DEPOSIT DATE WANTED                       
         BNE   *+10                                                             
         MVC   0(7,R3),=C'DEPOSIT'    CHANGE HEADER                             
*                                                                               
         B     HCSHX                                                            
*                                                                               
HCSH1    DS    0H                  CHECK DEPOSIT DATE                           
*                                                                               
         MVC   0(7,R3),=C'DEPOSIT' DEFAULT TO HEADLINE OF 'DEPOSIT'             
*                                                                               
         CLI   MOFLOW,C'Y'         MONEYFLOW OPTION?                            
         BE    HCSHX               YES                                          
         CLI   SBWRPROF+10,C'Y'    IF DEPOSIT DATE FOR ADD CALC                 
         BNE   *+10                                                             
         MVC   0(7,R3),=CL7'CHECK'    CHANGE HEADER                             
*                                                                               
         B     HCSHX                                                            
*                                                                               
HCSHX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RA                                                               
*                                                                               
         TITLE 'SPWRITER - CASH APPLIED DATA - INPUT - ICSHREC'                 
***********************************************************************         
*                                                                     *         
*        IF DOING CASH OPTION                                         *         
*          FIND NEXT MEDIA TRANSFER AND CASH TRANSACTION ELEMENTS     *         
*             IN CASHIER TABLE ENTRY                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ICSHREC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   CHKLINE,0           CLEAR CLEARED INFO SWITCH                    
         MVI   LINESTAT,0          CLEAR STATUS                                 
*                                                                               
         NI    OPTIND4,X'FF'-OPTRPTDR    TURN OFF REPEAT DRIVIN CALL            
*                                                                               
         CLI   SBMODE,SBPROCSP     SKIP IF  NOT BUY                             
         BE    *+8                                                              
         CLI   SBMODE,SBPROCCA          AND NOT CASH APPLIED                    
         BE    *+8                                                              
         CLI   SBMODE,SBPROCBL          AND NOT BILL                            
         BE    *+8                                                              
         CLI   SBMODE,SBPROCNV          AND NOT INVOICE                         
         BNE   ICSHRECX                                                         
*                                                                               
         TM    STANETOP,STANETQ    SKIP IF NOT DOING STANET SUMMARY             
         BNO   *+16                                                             
         BRAS  RE,ICSHSNT             CHECK STANET CHANGE OF KEY                
         BNE   ICSHRC0A               RESET TABLES                              
         B     ICSHREC1               DO NOT RESET TABLES                       
*                                                                               
         L     R1,SBAIO1           POINT TO CURRENT BUY/STATION BILL            
*                                                                               
         LA    RF,13               DEFAULT TO CHECKING WHOLE KEY                
*                                                                               
         CLI   SBMODE,SBPROCSP     IF BUY RECORD                                
         BNE   *+8                                                              
         LA    RF,BUYKBUY-BUYKEY      CHECK FOR CHANGE OF ESTIMATE              
*                                                                               
         CLI   SBMODE,SBPROCNV     IF NEW INVOICE RECORD                        
         BNE   *+8                                                              
         LA    RF,SNVKINV-SNVKEY      CHECK FOR CHANGE OF MONTH                 
*                                                                               
         CLI   SBMODE,SBPROCBL     IF STATION BILL RECORD                       
         BE    *+8                                                              
         CLI   SBMODE,SBPROCCA          AND NOT CASH APPLIED                    
         BNE   ICSHREC0                                                         
*                                                                               
         LA    RE,SBAGYREC         ESTABLISH AGENCY RECORD                      
         USING AGYKEY,RE           ESTABLISH AGENCY RECORD                      
***                                                                             
* THE CODE BELOW WAS AN OLD "BAND-AID" FIX. THIS "FIX" CAUSED OTHER             
* PROBLEMS WITH MERGING AND SEQUENCING. THE REAL CULPRIT WAS THAT               
* SVNETWK+1 WAS NOT CLEARED (BEFORE ICSHBL10). NOW THAT IT CLEARED,             
* THERE IS NO NEED FOR THIS CODE WHICH IN FACT CAUSES SOME INVOICES             
* TO GET THE WRONG SEQUENCE NUMBER AND HAVE THEIR DOLLARS APPLIED               
* TO ANOTHER INVOICE AS PER DSSUP-5942                                          
***                                                                             
***      CLC   SBAGY,=C'TB'        SKIP FOR CERTAIN AGENCIES                    
***      BE    *+10                                                             
***      CLC   SBAGY,=C'H0'                                                     
***      BE    *+10                                                             
***      CLC   SBAGY,=C'OU'                                                     
***      BE    *+10                                                             
***      CLC   SBAGY,=C'U#'                                                     
***      BE    ICSHREC0                                                         
*                                                                               
         CLI   AGYPCNDA,C'C'       SKIP IF NOT CANADIAN AGENCY                  
         BNE   ICSHREC0                                                         
*                                                                               
         TM    STABKAM-STABUCK(R1),X'03' SKIP IF NOT CANADIAN NETWORK           
         BNO   *+8                                                              
         LA    RF,STABKMKT-STABUCK   ELSE CHECK FOR CHANGE OF ESTIMATE          
*                                                                               
         DROP  RE                                                               
*                                                                               
ICSHREC0 DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SVKEY(0),0(R1)      RESET SQN ON KEY CHANGE                      
         BE    ICSHREC1                                                         
*                                                                               
ICSHRC0A DS    0H                                                               
*                                                                               
*        RESET SQN AND TABLES                                                   
*                                                                               
         XC    SVNETWK,SVNETWK     INIT CANADIAN NETWORK                        
         XC    SVINV,SVINV                                                      
         XC    SVCHKNUM,SVCHKNUM                                                
         XC    SVLSQN,SVLSQN                                                    
         XC    SVLSQN2,SVLSQN2                                                  
         XC    SVCLRDT,SVCLRDT                                                  
         XC    SVCLRSQ,SVCLRSQ                                                  
         MVI   SVCHKTAB,X'FF'      INIT CHECK NUMBER TABLE                      
         MVC   SVKEY,0(R1)         SAVE CURRENT KEY                             
*                                                                               
         L     R3,VSTBTBL          POINT TO LINE SEQ NUMBER TABLE               
         XC    0(ICSHSTLQ,R3),0(R3)  INIT 1ST TABLE ENTRY                       
*                                                                               
         L     R3,VINVTBL          POINT TO LINE SEQ NUMBER TABLE               
         XC    0(ICSHIVLQ,R3),0(R3)  INIT 1ST TABLE ENTRY                       
         XC    SVMBTELA,SVMBTELA   INIT POINTERS                                
         XC    SVMBTNXA,SVMBTNXA   INIT POINTERS                                
         XC    SVTRNELA,SVTRNELA   INIT POINTERS                                
         XC    SVTRNNXA,SVTRNNXA   INIT POINTERS                                
*                                                                               
         MVI   SVINVNO,X'FF'       INIT VENDOR INVOICE NUMBER                   
         MVI   SVCHKTAB,X'FF'      RESET VENDOR INVOICE TABLE                   
*                                                                               
ICSHREC1 DS    0H                                                               
*                                                                               
         CLI   SBMODE,SBPROCSP     SKIP IF NOT BUY PROCESSING                   
         BNE   ICSHRBYN                                                         
*                                                                               
         L     R3,SBACURCH         ESTABLISH SPOT EXTRACT CHUNK                 
         USING SCHUNKD,R3                                                       
*                                                                               
         XC    SVNETWK,SVNETWK     INIT CANADIAN NETWORK                        
         XC    SVINV,SVINV                                                      
         XC    SVCHKNUM,SVCHKNUM                                                
         XC    SVLSQN,SVLSQN                                                    
         XC    SVLSQN2,SVLSQN2                                                  
         XC    SVCLRDT,SVCLRDT                                                  
         XC    SVCLRSQ,SVCLRSQ                                                  
*                                                                               
         MVC   SVCHKNUM,SCCHKNUM   COPY CHECK NUMBER                            
         ICM   RE,15,SCPAYN        COPY PAID AMOUNT                             
*                                                                               
         ICM   R5,15,ACLRST01      ESTABLISH CLEARANCE STATUS 01 ELM            
         BZ    ICSHRECA            SKIP IF NONE                                 
*                                                                               
         USING CLSTEL01,R5                                                      
*                                                                               
         MVC   SVCLRDT,CLSTCLRD    SAVE CLEARANCE DATE                          
         MVC   SVCLRSQ,CLSTCLSQ    SAVE CLEARANCE SEQ                           
*                                                                               
         TM    CLSTSTAT,X'02'      SKIP IF NO INVOICE ELEMENTS                  
         BNO   ICSHRECA                                                         
*                                                                               
         ICM   R5,15,ACLRST03      POINT TO CLRST 03 ELM                        
         BZ    ICSHRECA            SKIP IF NONE                                 
*                                                                               
         USING CLSTEL03,R5         ESTABLISH INVOICE ELEMENT                    
*                                                                               
         MVC   SVINV,CLS3INV       SAVE INVOICE NUMBER                          
*                                                                               
         LLC   RF,CLSTLN03         GET ELEMENT LENGTH                           
         LA    R5,0(RF,R5)         BUMP TO FOLLOWING 05 ELEMENT                 
*                                                                               
         USING CLSTEL05,R5         ESTABLISH CLRST 05 ELM                       
*                                                                               
         CLI   CLSTEL05,X'05'      SKIP IF NOT 05 ELEMENT                       
         BNE   ICSHRECA                                                         
*                                                                               
         MVC   SVCHKNUM,CLS5CHK    SAVE CHECK NUMBER                            
*                                                                               
         ICM   R1,15,ACLRST01      POINT TO CLRST 01 ELEM                       
         LLC   RF,1(R1)            GET ITS LENGTH                               
         LA    R1,0(RF,R1)         POINT TO FOLLOWING 03 ELEMENT                
*                                                                               
         CLM   R1,15,ACLRST03      IF NOT PROCESSING FIRST 03 ELM               
         BE    *+8                                                              
         ICM   RE,15,SVPAYN           REVERT TO CURRENT SAVED PAID NET          
*                                                                               
ICSHRECA DS    0H                                                               
*                                                                               
         TM    CLS5STAT,CLS5STAT_CK IF CK                                       
         BNO   ICSHRECB                                                         
         CLI   SBWRPROF+13,C'Y'    AND NOT PRINTING CK'S                        
         BE    ICSHRECB                                                         
*                                                                               
         XC    SVCHKNUM,SVCHKNUM      TREAT AS NO CHECK                         
         SR    RE,RE                  TREAT AS NO PAYMENT                       
*                                                                               
ICSHRECB DS    0H                                                               
*                                                                               
         CLC   SVCHKNUM,=CL6'VOID' IF VOID CHECK                                
         BNE   *+12                                                             
         XC    SVCHKNUM,SVCHKNUM      TREAT AS NO CHECK                         
         SR    RE,RE                  TREAT AS NO PAYMENT                       
*                                                                               
         CLC   SVCHKNUM,SPACES     IF NO DISBURSEMENT YET                       
         BH    ICSHRECD                                                         
*                                                                               
         CLI   CBUOPT,C'Y'            IF REPORTING CBU                          
         BNE   ICSHRECC                                                         
*                                                                               
         MVC   SVCLRDT,SBBTODAY            USE TODAY AS ACTIVE DATE             
*                                                                               
         B     ICSHRECD                                                         
*                                                                               
ICSHRECC DS    0H                                                               
*                                                                               
         SR    RE,RE                  ELSE TREAT AS NO PAYMENT                  
*                                                                               
ICSHRECD DS    0H                                                               
*                                                                               
         ST    RE,SVPAYN           SAVE PAID NET                                
*                                                                               
         DROP  R5                                                               
*                                                                               
ICSHREC2 DS    0H                                                               
*                                                                               
*        PAYPENDING OPTION                                                      
*                                                                               
         CLI   CHKSOPT,C'0'        SKIP IF NOT PAYPEND OPTION                   
         BNE   ICSHXPDX                                                         
*                                                                               
         OC    SCPAYTLN,SCPAYTLN   SKIP SPOTS THAT HAVE NOTHING CLEARED         
         BZ    ICSHXPDX                                                         
*                                                                               
         OC    SVCHKNUM,SVCHKNUM   SKIP SPOTS THAT HAVE CHECKS                  
         BNZ   ICSHXPDX                                                         
*                                                                               
         MVI   LINESTAT,C'P'          PRINT CLEARED BUT UNDISBURSED             
*                                                                               
ICSHXPDX DS    0H                                                               
*                                                                               
*        FIND MONTH OF SERVICE FOR SPOT                                         
*                                                                               
         L     RE,AMONTHS          POINT TO BROADCAST MONTH TABLE               
*                                                                               
ICSHMOSL DS    0H                                                               
*                                                                               
         OC    0(4,RE),0(RE)       DONE IF END OF TABLE                         
         BZ    ICSHMOSD                                                         
*                                                                               
         CLC   SCDATE,0(RE)        FIND MONTH OF SERVICE FOR SPOT               
         BL    ICSHMOSC                                                         
         CLC   SCDATE,2(RE)                                                     
         BNH   ICSHMOSF                                                         
*                                                                               
ICSHMOSC DS    0H                                                               
*                                                                               
         LA    RE,4(RE)            BUMP TO NEXT MONTH                           
         B     ICSHMOSL                                                         
*                                                                               
ICSHMOSD DS    0H                                                               
*                                                                               
         B     ICSHRECX            SKIP IF MOS NOT FOUND                        
*                                                                               
ICSHMOSF DS    0H                                                               
*                                                                               
         MVC   ICSHSVDT,2(RE)      SAVE MOS END DATE                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,ICSHSVDT),(3,DUB) DATE IN YMD                     
         MVC   SVMOS,DUB           SAVE MONTH OF SERVICE - YM                   
*                                                                               
*        FIND CHECK IN TABLE OR ADD NEW ENTRY                                   
*                                                                               
         LA    RF,SVCHKTAB         POINT TO START OF CHECK NUMBER TABLE         
         SR    RE,RE               INIT ENTRY POINTER                           
*                                                                               
         L     R1,SBAIO1           POINT TO CURRENT BUY RECORD                  
*                                                                               
ICSHCKLP DS    0H                                                               
*                                                                               
         CLI   0(RF),X'FF'         DONE AT END OF TABLE                         
         BE    ICSHCKDN                                                         
*                                                                               
         CLC   SVMOS,SVTMOS-SVCHKTAB(RF)    MATCH ON MOS                        
         BNE   ICSHCKCN                                                         
*                                                                               
         CLC   SCPRD1,SVTPRD-SVCHKTAB(RF)   MATCH ON PRODUCT                    
         BNE   ICSHCKCN                                                         
*                                                                               
         CLC   BUYKEST-BUYKEY(1,R1),SVTEST-SVCHKTAB(RF) MATCH ON EST            
         BNE   ICSHCKCN                                                         
*                                                                               
         LR    RE,RF               SAVE LATEST MOS ENTRY PTR                    
*                                                                               
*        SKIP INVOICE CHECK FOR NOW                                             
*                                                                               
*******  CLC   SVINV,SVTINV-SVCHKTAB(RF) MATCH INVOICE NUM TO TABLE             
*******  BNE   ICSHCKCN                                                         
*                                                                               
         CLC   SVCHKNUM,SVTCHK-SVCHKTAB(RF) MATCH CHECK NUM TO TABLE            
         BE    ICSHCKFD                                                         
*                                                                               
ICSHCKCN DS    0H                                                               
*                                                                               
         LA    RF,SVTENTL(RF)         BUMP TO NEXT TABLE ENTRY                  
         B     ICSHCKLP                                                         
*                                                                               
ICSHCKDN DS    0H                                                               
*                                                                               
*        NO MATCH - NEW ENTRY IN TABLE                                          
*                                                                               
         XC    0(SVTENTL,RF),0(RF) INIT TABLE ENTRY                             
*                                                                               
         MVC   SVTMOS-SVCHKTAB(L'SVTMOS,RF),SVMOS    SAVE CHECK MOS             
         MVC   SVTPRD-SVCHKTAB(L'SVTPRD,RF),SCPRD1   SAVE CHECK PRD             
         MVC   SVTEST-SVCHKTAB(L'SVTEST,RF),BUYKEST-BUYKEY(R1) EST              
         MVC   SVTCLRDT-SVCHKTAB(L'SVTCLRDT,RF),SVCLRDT  SAVE CLEAR DTE         
         MVC   SVTCLRSQ-SVCHKTAB(L'SVTCLRSQ,RF),SVCLRSQ  SAVE CLEAR SEQ         
         MVC   SVTINV-SVCHKTAB(L'SVTINV,RF),SVINV    SAVE CHECK INV             
         MVC   SVTCHK-SVCHKTAB(L'SVTCHK,RF),SVCHKNUM SAVE CHECK NUM             
*                                                                               
         LTR   RE,RE               IF NO OTHER ENTRY FOR MOS                    
         BNZ   ICSHCK05                                                         
*                                                                               
         MVI   SVTLSQN-SVCHKTAB(RF),1     SET LSQN TO 1                         
         MVI   SVTLSQN2-SVCHKTAB(RF),0    FORCED TO LINE 0                      
*                                                                               
         CLI   CHKSOPT,C'0'        IF PAYPEND OPTION                            
         BNE   ICSHCK10                                                         
*                                                                               
         OC    SVCHKNUM,SVCHKNUM      START CHECKS AT 2                         
         BZ    *+8                                                              
         MVI   SVTLSQN-SVCHKTAB(RF),2     SET LSQN TO 2                         
*                                                                               
         B     ICSHCK10                                                         
*                                                                               
ICSHCK05 DS    0H                                                               
*                                                                               
******   B     ICSHCK07            JUST BUMP MAJOR SQN                          
*                                                                               
*                                  IF SAME CLEAR DATE AND SEQ                   
         CLC   SVTCLRDT-SVCHKTAB(L'SVTCLRDT,RE),SVTCLRDT-SVCHKTAB(RF)           
         BNE   ICSHCK07                                                         
         CLC   SVTCLRSQ-SVCHKTAB(L'SVTCLRSQ,RE),SVTCLRSQ-SVCHKTAB(RF)           
         BNE   ICSHCK07                                                         
*                                  COPY MAJOR SQN                               
         MVC   SVTLSQN-SVCHKTAB(L'SVTLSQN,RF),SVTLSQN-SVCHKTAB(RE)              
*****    ICM   R1,1,SVTLSQN2-SVCHKTAB(RE) BUMP SECOND SEQ #                     
*****    AHI   R1,1                                                             
*****    STCM  R1,1,SVTLSQN2-SVCHKTAB(RF)                                       
*                                                                               
         ICM   R1,1,SVTLSQN-SVCHKTAB(RE) BUMP FIRST SEQ #                       
         AHI   R1,1                                                             
         STCM  R1,1,SVTLSQN-SVCHKTAB(RF)                                        
*                                                                               
         B     ICSHCK08                                                         
*                                                                               
ICSHCK07 DS    0H                                                               
*                                                                               
         ICM   R1,1,SVTLSQN-SVCHKTAB(RE) ELSE BUMP LAST SQN                     
         AHI   R1,1                                                             
         STCM  R1,1,SVTLSQN-SVCHKTAB(RF)                                        
*                                                                               
         MVI   SVTLSQN2-SVCHKTAB(RF),0 ZERO SECONDARY SQN                       
*                                                                               
ICSHCK08 DS    0H                                                               
*                                                                               
         CLI   CHKSOPT,C'0'        IF PAYPEND OPTION                            
         BNE   ICSHCK10                                                         
*                                                                               
         OC    SVCHKNUM,SVCHKNUM      NO CHECK SPOTS                            
         BNZ   *+12                                                             
         MVI   SVTLSQN-SVCHKTAB(RF),1     FORCED TO LINE 1                      
         MVI   SVTLSQN2-SVCHKTAB(RF),0    FORCED TO LINE 0                      
*                                                                               
ICSHCK10 DS    0H                                                               
*                                                                               
         MVI   SVTENTL(RF),X'FF'   RESET END OF TABLE                           
*                                                                               
ICSHCKD1 DS    0H                                                               
*                                                                               
ICSHCKFD DS    0H                                                               
*                                                                               
         MVC   SVLSQN,SVTLSQN-SVCHKTAB(RF) RESET LINE SEQUENCE NUMBER           
         MVC   SVLSQN2,SVTLSQN2-SVCHKTAB(RF) RESET LINE SEQUENCE NUMBER         
*                                                                               
ICSHRBYX DS    0H                                                               
*                                                                               
         MVC   0(2,R2),SVLSQN      RETURN LINE SEQUENCE NUMBER                  
*                                                                               
         CLI   CHKSOPT,C'0'        SKIP IF NOT PAYPEND OPTION                   
         BNE   ICSHBYPX                                                         
*                                                                               
         L     RF,SBAIO1           POINT TO BUY RECORD                          
         MVC   SVMKTSTA,BUYKMSTA-BUYKEY(RF) SAVE MKT/STATION                    
         MVC   SVPRD,SCPRD1                 SAVE PRODUCT                        
         MVC   SVEST,BUYKEST-BUYKEY(RF)     SAVE ESTIMATE                       
*                                                                               
         B     ICSHRECX                                                         
*                                                                               
ICSHBYPX DS    0H                                                               
*                                                                               
         CLI   CHKIVOPT,C'Y'       SKIP IF PRINTING CHECK INVOICES              
         BE    ICSHRECX                                                         
*******                                                                         
*******  CLC   SCCHKNUM,SPACES     IF THERE IS NO CHECK NUMBER                  
*******  BH    *+8                                                              
*******  MVI   CHKLINE,C'N'           DROP RECORD FROM SORT                     
*                                                                               
         CLC   SCCHKNUM,SPACES     IF THERE IS NO CHECK NUMBER                  
         BH    *+16                                                             
         XC    SCPAYN,SCPAYN          DROP PAID AMOUNT                          
         XC    SCPAY,SCPAY            DROP PAID AMOUNT                          
*                                                                               
         B     ICSHRECX                                                         
*                                                                               
         DROP  R3                                                               
*                                                                               
ICSHRBYN DS    0H                                                               
*                                                                               
*        STATION BILLING RECORD ELEMENTS                                        
*                                                                               
         CLI   SBMODE,SBPROCBL     SKIP IF  NOT BILL                            
         BE    *+8                                                              
         CLI   SBMODE,SBPROCCA          AND NOT CASH APPLIED                    
         BNE   ICSHRBLN                                                         
*                                                                               
         LA    RE,SBAGYREC                                                      
         USING AGYKEY,RE           ESTABLISH AGENCY RECORD                      
*                                                                               
         CLI   AGYPCNDA,C'C'       SKIP IF NOT CANADIAN AGENCY                  
         BNE   ICSHBL10                                                         
*                                                                               
         DROP  RE                                                               
*                                                                               
         ICM   RE,15,SBAIO1        POINT TO STATION BILL RECORD                 
         BZ    ICSHBH00            SKIP IF NONE                                 
*                                                                               
         TM    STABKAM-STABUCK(RE),X'03' SKIP IF NOT CANADIAN NETWORK           
         BNO   ICSHBL10                                                         
*                                                                               
         CLI   STABKSTA+2-STABUCK(RE),X'B0' IF CABLE NETWORK                    
         BL    *+14                                                             
         MVC   SVNETWK,STABKSTA-STABUCK(RE) SAVE CANADA CABLE NETWK             
         B     ICSHBL10                                                         
*                                                                               
         MVC   SVNETWK(1),STABKSTA+2-STABUCK(RE)  ELSE SAVE NETWORK             
         MVI   SVNETWK+1,0         CLEAR IN CASE LAST WAS CABLE!!               
*                                                                               
ICSHBL10 DS    0H                                                               
*                                                                               
         L     R5,SBACURCH         ESTABLISH CURRENT STATION BILL ELM           
         USING STABELEM,R5                                                      
*                                                                               
         CLI   SBWRPROF+7,C'Y'     SKIP IF KEEPING REVERSED/REVERSALS           
         BNE   ICSHREVN                                                         
*                                                                               
         TM    STABINV,STABINV_REVERSAL+STABINV_REVERSED                        
         BZ    *+12                                                             
         MVI   CHKLINE,C'N'        DROP RECORD FROM SORT                        
         B     ICSHRECX                                                         
*                                                                               
ICSHREVN DS    0H                                                               
*                                                                               
*        FIND CHECK NUMBER AND CHECK DATE                                       
*                                                                               
ICSHBCK  DS    0H                                                               
*                                                                               
         XC    SVCSHNM,SVCSHNM     INIT CHECK NUMBER AND DATE                   
         XC    SVCSHDT,SVCSHDT                                                  
         XC    SVCSHB#,SVCSHB#     INIT BATCH NUMBER                            
*                                                                               
         BRAS  RE,MBTNXT           GET 1ST/NEXT MBT ELM FOR BILL                
*                                                                               
         ICM   R7,15,SVMBTELA      ESTABLISH MEDIA TRANSFER ELEMENT             
         BZ    ICSHBCKX               SKIP IF NOT FOUND                         
*                                                                               
         USING MBTELD,R7                                                        
*                                                                               
         ICM   R7,15,SVTRNELA      ESTABLISH CASH TRANSACTION ELEMENT           
         BZ    ICSHBCKX               SKIP IF DATA MISSING                      
*                                                                               
         USING TRNELD,R7                                                        
*                                                                               
         MVC   SVCSHB#,TRNBTCH     SAVE BATCH NUMBER                            
*                                                                               
*        FIND FOLLOWING RECEIVABLE ALLOCATION ELEMENT                           
*                                                                               
ICSHBCKL DS    0H                                                               
*                                                                               
         USING RALELD,R7           ESTABLISH RECEIVABLE ALLOC ELM               
*                                                                               
         LLC   RF,RALLN            ELEMENT LENGTH                               
         LA    R7,0(RF,R7)         NEXT ELEMENT                                 
*                                                                               
         CLI   RALEL,0             DONE IF END OF RECORD REACHED                
         BE    ICSHBCKD                                                         
*                                                                               
         CLI   RALEL,TRNELQ        DONE IF NEW TRANSACTION ELM REACHED          
         BE    *+8                                                              
         CLI   RALEL,X'FF'         DONE IF NEW TRANSACTION ELM REACHED          
         BE    ICSHBCKD                                                         
*                                                                               
         CLI   RALEL,RALELQ        FIND RECEIVABLE ALLOC ELM                    
         BNE   ICSHBCKC                                                         
*                                                                               
         CLI   RALTYPE,RALTALC     FIND REGULAR ALLOCATION ELEMENT              
         BE    ICSHBCKF                                                         
*                                                                               
         CLI   RALTYPE,RALTOFS     FIND OFFSET                                  
         BE    ICSHBCKF                                                         
*                                                                               
         CLI   RALTYPE,RALTWOF     FIND WRITE OFF                               
         BE    ICSHBCKF                                                         
*                                                                               
ICSHBCKC DS    0H                                                               
*                                                                               
         B     ICSHBCKL                                                         
*                                                                               
ICSHBCKD DS    0H                                                               
*                                                                               
         SR    R7,R7               NO RECEIVABLE ALLOC ELEMENT FOUND            
*                                                                               
         B     ICSHBCKX                                                         
*                                                                               
ICSHBCKF DS    0H                                                               
*                                                                               
*        CLIENT CHECK NUMBER                                                    
*                                                                               
         LTR   R7,R7               SKIP IF NO RECEIVABLE ALLOC AVAIL            
         BZ    ICSHBCKX                                                         
*                                                                               
         CLI   RALTYPE,RALTALC     IF AN ALLOCATION ELEMENT                     
         BNE   ICSHBCK3                                                         
*                                                                               
         MVC   SVCSHNM,RALAREF        RETURN CLIENT CHECK NUMBER                
*                                                                               
         CLI   GLARGS+2,C'C'       IF CLIENT CHECK DATE                         
         BNE   ICSHBCK2                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(1,RALADAT),(2,SVCSHDT) CLIENT CHECK DATE            
*                                                                               
         B     ICSHBCK9                                                         
*                                                                               
ICSHBCK2 DS    0H                  ELSE DEPOSIT DATE                            
*                                                                               
         GOTO1 DATCON,DMCB,(1,RALADEP),(2,SVCSHDT) DEPOSIT CHECK DATE           
*                                                                               
         B     ICSHBCK9                                                         
*                                                                               
ICSHBCK3 DS    0H                                                               
*                                                                               
         CLI   RALTYPE,RALTOFS     IF OFFSET ELEMENT                            
         BNE   ICSHBCK5                                                         
*                                                                               
         MVC   SVCSHNM,=C'OFFSET'     RETURN 'OFFSET'                           
*                                                                               
         GOTO1 DATCON,DMCB,(1,RALODAT),(2,SVCSHDT)  OFFSET DATE                 
*                                                                               
         B     ICSHBCK9                                                         
*                                                                               
ICSHBCK5 DS    0H                                                               
*                                                                               
         CLI   RALTYPE,RALTWOF     IF WRITE OFF                                 
         BNE   ICSHBCK4                                                         
*                                                                               
         MVC   SVCSHNM,RALWREF        RETURN WRITE OFF REF #                    
*                                                                               
         GOTO1 DATCON,DMCB,(1,RALWDAT),(2,SVCSHDT)  WRITE OFF DATE              
*                                                                               
         B     ICSHBCK9                                                         
*                                                                               
ICSHBCK4 DS    0H                                                               
*                                                                               
ICSHBCK9 DS    0H                                                               
*                                                                               
ICSHBCKX DS    0H                                                               
*                                                                               
*        LINE SEQUENCE NUMBERS ARE STORED IN TABLE                              
*        INDEXED BY BILL PERIOD                                                 
*                                                                               
         L     R3,VSTBTBL          ESTABLISH SEQ NUMBER TABLE                   
         USING ICSHSTTD,R3                                                      
         SR    R0,R0               INIT ENTRY PTR SAVE                          
*                                                                               
ICSHBPLP DS    0H                                                               
*                                                                               
         OC    0(ICSHSTLQ,R3),0(R3)  CHECK FOR END OF TABLE                     
         BZ    ICSHBPDN              NOT IN TABLE                               
*                                                                               
         CLC   SVNETWK,STTNETWK    MATCH ON CANADIAN NETWORK                    
         BNE   ICSHBPCN              USUALLY NULLS                              
*                                                                               
         CLC   STABPER,STTPER        MATCH ON BILL PERIOD                       
         BNE   ICSHBPCN                                                         
*                                                                               
         LR    R0,R3               SAVE POINTER TO LAST MOS ENTRY               
*                                                                               
         CLC   STABBDT,STTBDT      MATCH ON BILLING DATE (COMPRESSED)           
         BNE   ICSHBPCN                                                         
*                                                                               
         CLC   STABINV,STTINV      MATCH ON BILL NUMBER                         
         BNE   ICSHBPCN                                                         
*                                                                               
         LA    RF,SBAGYREC         ESTABLISH AGENCY RECORD                      
         USING AGYKEY,RF           ESTABLISH AGENCY RECORD                      
*                                                                               
         CLI   AGYPCNDA,C'C'       SKIP IF NOT CANADIAN AGENCY                  
         BNE   ICSHBP10                                                         
*                                                                               
         DROP  RF                                                               
*                                                                               
         ICM   RE,15,SBAIO1        POINT TO STATION BILL RECORD                 
         BZ    ICSHBP10            SKIP IF NONE                                 
*                                                                               
         TM    STABKAM-STABUCK(RE),X'03' SKIP IF CANADIAN NETWORK               
         BO    ICSHBP15                                                         
*                                                                               
ICSHBP10 DS    0H                                                               
******                                                                          
******   CLI   STANETOP,STANETQ    SKIP IF DOING STANET SUMMARY                 
******   BE    ICSHBP15                                                         
******                                                                          
******   SR    RF,RF                                                            
******   ICM   RF,1,STTSQN         BUMP LINE SEQUENCE NUMBER                    
******   LA    RF,1(RF)                                                         
******   STCM  RF,1,STTSQN                                                      
******                                                                          
ICSHBP15 DS    0H                                                               
*                                                                               
         CLC   STTCSHB#,SVCSHB#    MATCH ON CLIENT BATCH NUMBER                 
         BNE   ICSHBPCN                                                         
*                                                                               
         CLC   STTCSHNM,SVCSHNM    MATCH ON CLIENT CHECK NUMBER                 
         BNE   ICSHBPCN                                                         
*                                                                               
         CLC   STTCSHDT,SVCSHDT     MATCH ON CLIENT CHECK DATE                  
         BNE   ICSHBPCN                                                         
*                                                                               
         B     ICSHBPFD                                                         
*                                                                               
ICSHBPCN DS    0H                                                               
*                                                                               
         LA    R3,ICSHSTLQ(R3)       BUMP TO NEXT TABLE ENTRY                   
         B     ICSHBPLP                                                         
*                                                                               
ICSHBPDN DS    0H                    MAKE NEW ENTRY IN BILL TABLE               
*                                                                               
         MVC   STTNETWK,SVNETWK      SAVE NETWORK                               
         MVC   STTPER,STABPER        SAVE BILL PERIOD                           
         MVC   STTBDT,STABBDT        SAVE BILLING DATE                          
         MVC   STTINV,STABINV        SAVE INVOICE #                             
         MVC   STTCSHB#,SVCSHB#      SAVE CLIENT BATCH NUMBER                   
         MVC   STTCSHNM,SVCSHNM      SAVE CLIENT CHECK NUMBER                   
         MVC   STTCSHDT,SVCSHDT      SAVE CLIENT CHECK DATE                     
*                                                                               
         MVI   STTSQN,1              DEFAULT LINE SEQ. NUMBER                   
*                                                                               
         LTR   RE,R0               IF ANOTHER BILL FOR MOS                      
         BZ    ICSHBPD1                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,STTSQN-ICSHSTTD(RE) BUMP LINE SEQUENCE NUMBER               
         LA    RF,1(RF)                                                         
         STCM  RF,1,STTSQN                                                      
*                                                                               
ICSHBPD1 DS    0H                    MAKE NEW ENTRY IN BILL TABLE               
*                                                                               
         CLI   ICSHSTLQ(R3),X'FF'    TABLE OVERFLOW                             
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ICSHSTLQ(ICSHSTLQ,R3),ICSHSTLQ(R3)  INIT NXT TABLE ENTRY         
*                                                                               
         B     ICSHBPX                                                          
*                                                                               
ICSHBPFD DS    0H                                                               
*                                                                               
ICSHBPX  DS    0H                                                               
*                                                                               
         MVC   SVLSQN,STTSQN       SAVE LINE SEQUENCE NUMBER                    
         MVC   0(1,R2),STTSQN      SET SEQUENCE NUMBER                          
*                                                                               
         CLI   CHKSOPT,C'0'        SKIP IF NOT PAYPEND OPTION                   
         BNE   ICSHBH00                                                         
*                                                                               
         ICM   RE,15,SBAIO1        POINT TO STATION BILL RECORD                 
         BZ    ICSHBH00            SKIP IF NONE                                 
*                                                                               
         MVC   SVPRD,STABKPRD-STABUCK(RE)     SAVE PRODUCT                      
         MVC   SVEST,STABKEST-STABUCK(RE)     SAVE ESTIMATE                     
         MVC   SVMKTSTA,STABKMKT-STABUCK(RE)  PASS MKT/STA                      
         MVC   SVMOS,STTPER                   PASS BILLING Y/M                  
*                                                                               
         DROP  R3                                                               
*                                                                               
ICSHBH00 DS    0H                                                               
*                                                                               
*        DETERMINE MODE                                                         
*                                                                               
ICSHROLD DS    0H                                                               
*                                                                               
         OC    SVTRNNXA,SVTRNNXA   IF CASH DATA TO COME                         
         BZ    *+8                                                              
         OI    OPTIND4,OPTRPTDR       ASK TO REPEAT DRIVIN CALL                 
*                                                                               
ICSRCSHX DS    0H                                                               
         B     ICSHRECX                                                         
*                                                                               
ICSHRBLN DS    0H                                                               
*                                                                               
*        HANDLE INVOICE RECORDS                                                 
*                                                                               
ICSHRNV  DS    0H                                                               
*                                                                               
         CLI   SBMODE,SBPROCNV     SKIP IF NOT PROCESSING INVOICES              
         BNE   ICSHRNVN                                                         
*                                                                               
         CLI   ELEM,0                                                           
         BE    ICSHRNVX                                                         
*                                                                               
         L     RF,SBACURCH         POINT TO EXTRACT CHUNK                       
         USING SNVIDELD,RF         ESTABLISH AS INVOICE DETAIL ELM              
*                                                                               
         L     RE,VINVTBL          POINT TO VENDOR INVOICE SQN TABLE            
         USING ICSHIVTD,RE         ESTABLISH TABLE ENTRY                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R1,15,SBAIO1        POINT TO INVOICE RECORD                      
         BZ    ICSHIVX             NONE AVAILABLE                               
*                                                                               
         USING SNVKEY,R1           ESTABLISH INVOICE RECORD                     
*                                                                               
         XC    SVMKTSTA,SVMKTSTA   SET STATION                                  
         MVC   SVMKTSTA+2(3),SNVKSTA                                            
*****                                                                           
*****    LA    R1,42(R1)           POINT TO FIRST ELEMENT IN RECORD             
*****                                                                           
*****    USING SNVHDELD,R1         ESTABLISH HEADER ELEMENT                     
*****                                                                           
*****    CLI   SNVHDEL,0           FIND HEADER ELEMENT                          
*****    BE    ICSHIV01               NO HEADER                                 
*****    CLI   SNVHDEL,SNVHDELQ    FIND HEADER ELEMENT                          
*****    BE    ICSHIV02                                                         
*****    ICM   R0,1,SNVHDLEN                                                    
*****    BZ    ICSHIV01                                                         
*****    AR    R1,R0                                                            
*****    B     *-26                                                             
*****                                                                           
ICSHIV01 DS    0H                                                               
*****    XC    WORK,WORK           DUMMY ELEMENT                                
*****    LA    R1,WORK                                                          
*****                                                                           
ICSHIV02 DS    0H                                                               
*****                                                                           
         MVC   SVDSLN,SNVIDSLN     SAVE SPOT LENGTH                             
*                                                                               
         LA    R3,SNVIDPRD         SAVE DETAIL PRODUCT                          
*                                                                               
         CLI   SNVIDPRD,0          IF NO DETAIL PRODUCT                         
         BNE   *+8                                                              
         LA    R3,SBBPRD              USE HEADER PRODUCT                        
*****    LA    R3,SNVHDPRD            SAVE HEADER PRODUCT                       
*                                                                               
         MVC   SVDPRD,0(R3)        SAVE PRODUCT                                 
*                                                                               
         LA    R3,SNVIDPR2         SAVE DETAIL PRODUCT 2                        
*                                                                               
         CLI   SNVIDPR2,0          IF NO DETAIL PRODUCT 2                       
         BNE   *+8                                                              
         LA    R3,SBBPRD2             USE HEADER PRODUCT 2                      
*****    LA    R3,SNVHDPR2            SAVE HEADER PRODUCT 2                     
*                                                                               
         MVC   SVDPR2,0(R3)        SAVE PRODUCT 2                               
*                                                                               
         LA    R3,SNVIDEST         SAVE DETAIL ESTIMATE                         
*                                                                               
         CLI   SNVIDEST,0          IF NO DETAIL ESTIMATE                        
         BNE   *+8                                                              
         LA    R3,SBBEST              USE HEADER ESTIMATE                       
*****    LA    R3,SNVHDEST            SAVE HEADER ESTIMATE                      
*                                                                               
         MVC   SVDEST,0(R3)        SAVE ESTIMATE                                
*                                                                               
         MVI   SVLSQN,0            INIT SEQUENCE NUMBER                         
*                                                                               
ICSHIVLP DS    0H                                                               
*                                                                               
         OC    ICSHIVTD(ICSHIVLQ),ICSHIVTD  CHECK FOR END OF TABLE              
         BZ    ICSHIVDN              NOT IN TABLE                               
*                                                                               
*****    CLC   IVTSLN,SVDSLN         MATCH ON SPOT LENGTH                       
*****    BNE   ICSHIVCN                                                         
*                                                                               
         CLC   IVTPRD,SVDPRD         MATCH ON PRODUCT                           
         BNE   ICSHIVCN                                                         
*                                                                               
         CLC   IVTPR2,SVDPR2         MATCH ON PRODUCT 2                         
         BNE   ICSHIVCN                                                         
*                                                                               
         CLC   IVTEST,SVDEST         MATCH ON ESTIMATE                          
         BNE   ICSHIVCN                                                         
*                                                                               
         MVC   SVLSQN,IVTSQN         SAVE SEQUENCE NUMBER                       
*                                                                               
         CLC   IVTINVNO,ELEM+1       MATCH ON INVOICE NUMBER                    
         BE    ICSHIVFD                                                         
*                                                                               
ICSHIVCN DS    0H                                                               
*                                                                               
         LA    RE,ICSHIVLQ(RE)       BUMP TO NEXT TABLE ENTRY                   
         B     ICSHIVLP                                                         
*                                                                               
ICSHIVDN DS    0H                    MAKE NEW ENTRY IN BILL TABLE               
*                                                                               
         MVC   IVTSLN,SVDSLN         SAVE SPOTLENGTH                            
         MVC   IVTPRD,SVDPRD         SAVE PRODUCT                               
         MVC   IVTPR2,SVDPR2         SAVE PRODUCT 2                             
         MVC   IVTEST,SVDEST         SAVE ESTIMATE                              
         MVC   IVTINVNO,ELEM+1       SAVE INVOICE NUMBER                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,SVLSQN           BUMP LINE SEQUENCE NUMBER                  
         LA    R1,1(R1)                                                         
         STCM  R1,1,IVTSQN                                                      
*                                                                               
         CLI   ICSHIVTD+ICSHIVLQ,X'FF'        TABLE OVERFLOW                    
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ICSHIVTD+ICSHIVLQ(ICSHIVLQ),ICSHIVTD+ICSHIVLQ INIT NXT           
*                                                                               
         B     ICSHIVX                                                          
*                                                                               
ICSHIVFD DS    0H                                                               
*                                                                               
ICSHIVX  DS    0H                                                               
*                                                                               
         MVC   SVLSQN,IVTSQN         SAVE LINE SEQUENCE NUMBER                  
         MVC   0(1,R2),IVTSQN        SET SEQUENCE NUMBER                        
         MVC   SVINVNO,IVTINVNO      SAVE INVOICE NUMBER                        
*                                                                               
         MVC   SVPRD,IVTPRD        SAVE PRODUCT                                 
         MVC   SVEST,IVTEST        SAVE ESTIMATE                                
*                                                                               
         DROP  R1,RE,RF                                                         
*                                                                               
ICSHRNVX DS    0H                                                               
*                                                                               
         B     ICSHRECX                                                         
*                                                                               
ICSHRNVN DS    0H                                                               
*                                                                               
ICSHRECX DS    0H                                                               
         XIT1                                                                   
*                                                                               
ICSHMBSW DC    C'N'                C'Y' - POSTING W/O CHECK                     
ICSHSVDT DS    XL2                 END OF MONTH OF SERVICE                      
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         TITLE 'SPWRITER - CASH APPLIED DATA - INPUT - MBT1ST'                  
***********************************************************************         
*                                                                     *         
*        FIND FIRST/NEXT MEDIA TRANSFER ELEMENT                       *         
*             AND TRANSACTION ELEMENT                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MBTNXT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,SBACURCH         ESTABLISH CURRENT STATION BILL ELM           
         USING STABELEM,R5         ESTABLISH STATION BILL ELM                   
*                                                                               
         CLC   SVSTAB,STABELEM     IF STATION BILL ELM CHANGED                  
         BE    MBTNXT05                                                         
*                                                                               
         MVC   SVSTAB,STABELEM     SAVE NEW BILL ELEMENT                        
         XC    SVMBTELA,SVMBTELA   RESET POINTERS                               
         XC    SVMBTNXA,SVMBTNXA   RESET POINTERS                               
         XC    SVTRNELA,SVTRNELA   RESET POINTERS                               
         XC    SVTRNNXA,SVTRNNXA   RESET POINTERS                               
*                                                                               
MBTNXT05 DS    0H                                                               
*                                                                               
         MVC   SVMBTELA,SVMBTNXA   BUMP TO NEXT MBT ELM                         
         MVC   SVTRNELA,SVTRNNXA   BUMP TO NEXT TRN ELM                         
*                                                                               
         XC    SVTRNNXA,SVTRNNXA   INIT NEXT CASH TRANSACTION ELM ADDR          
*                                                                               
         OC    SVMBTELA,SVMBTELA   IF NO MBT THEN FIRST TIME                    
         BNZ   MBNXBHX                                                          
*                                                                               
*        FIRST TIME                                                             
*                                                                               
*        FIND BILL HEADER RECORD                                                
*                                                                               
         XC    KEY,KEY             BUILD BILL HEADER KEY                        
         LA    R3,KEY                                                           
         USING BILLRECD,R3                                                      
*                                                                               
         MVC   BKEYAM,SBBAGYMD     AGENCY/MEDIA                                 
*                                                                               
         CLI   SBQMED,C'C'         IF COMBINED MEDIA                            
         BNE   MBNXBH20                                                         
*                                                                               
         NI    BKEYAM,X'F0'                                                     
*                                                                               
         CLI   SBMED,C'T'                                                       
         BNE   MBNXBH10                                                         
*                                                                               
         OI    BKEYAM,X'01'                                                     
*                                                                               
         B     MBNXBH20                                                         
*                                                                               
MBNXBH10 OI    BKEYAM,X'03'        SBMED MUST = N                               
*                                                                               
MBNXBH20 MVC   BKEYCLT,SBBCLT                                                   
         MVC   BKEYPRD,SBPRD                                                    
         MVC   BKEYEST,SBBEST                                                   
         MVC   BKEYYSRV(2),STABPER                                              
*                                                                               
         GOTO1 DATCON,DMCB,(2,STABBDT),(1,FULL)                                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,STABBDT),(3,DUB)                                  
*                                                                               
         SR    RF,RF                                                            
         IC    RF,DUB+1                                                         
         SLL   RF,28                                                            
         SRL   RF,28               RF=BILL MONTH                                
*                                                                               
         SR    RE,RE                                                            
         IC    RE,FULL                                                          
         SLL   RE,28                                                            
         SRL   RE,24               RE=BILL YEAR                                 
*                                                                               
         OR    RF,RE               COMBINE FOR YM                               
         STC   RF,BKEYMBIL                                                      
*                                                                               
         MVC   BKEYINV,STABINV                                                  
         NI    BKEYINV,X'FF'-(STABINV_REVERSAL+STABINV_REVERSED)                
*                                                                               
         CLC   BKEY,SVBKEY         TEST KEY SAME AS LAST                        
         BE    MBNXBHX             YES-NOTHING'S CHANGED                        
*                                                                               
         CLC   BKEYCLT,SVBKEY+BKEYCLT-BKEY  IF NEW CLIENT                       
         BE    MBNXBH22                                                         
*                                                                               
         OC    SVBKEY,SVBKEY       AND NOT FIRST TIME                           
         BZ    MBNXBH22                                                         
*                                                                               
         LA    R4,CSHIERC          POINT TO CASHIER CONTROL BLOCK               
         USING CSHIERD,R4          ESTABLISH AREA                               
*                                                                               
         MVI   CSHACT,CSHINIQ        RE-INITIALIZE CASHIER                      
         GOTO1 VCASHIER,DMCB,CSHIERD   INIT DDCASHIER                           
         CLI   CSHERR,0            NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MBNXBH22 DS    0H                                                               
*                                                                               
         SR    R0,R0               INIT SWITCH                                  
*                                                                               
         XC    SVBHDELA,SVBHDELA   INIT CASHIER BUFFER ADDRESS                  
*                                                                               
         MVC   SVBKEY,BKEY         SAVE BILL HEADER KEY                         
*                                                                               
*        RETRIEVE BILL HEADER FROM TSAROFF BUFFER                               
*                                                                               
         LA    R4,CSHIERC          POINT TO CASHIER CONTROL BLOCK               
         USING CSHIERD,R4          ESTABLISH AREA                               
*                                                                               
         MVI   CSHACT,CSHRDHQ      SET ACTION TO READ HIGH                      
         MVC   CSHAGYCH,SBAGY      SET AGENCY ALPHA                             
         MVC   CSHMED,SBMED        SET MEDIA                                    
*                                                                               
         ST    R3,CSHBLLA          SET A(BILLKEY)                               
*                                                                               
MBNXBH25 DS    0H                                                               
*                                                                               
         GOTO1 VCASHIER,DMCB,CSHIERD    READ BILL RECORD                        
*                                                                               
         ICM   RF,15,CSHRECA       POINT TO FOUND RECORD                        
         BZ    *+14                NONE FOUND                                   
         CLC   BKEY,0(RF)          CHECK IF RECORD FOUND                        
         BE    MBNXBHFD                                                         
*                                                                               
*        ADD BILL HEADER TO BUFFER                                              
*                                                                               
*        FIRST RETRIEVE BILL HEADER                                             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   BKEY,KEYSAVE                                                     
         BE    MBNXBH27                                                         
*                                                                               
         LTR   R0,R0               CONTINUE IF SWITCH NOT SET                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        TO CORRECT FILE PROBLEM LOOK FOR BILL HEADER                           
*        WITH INVOICE NUMBER ONE LESS AND USE IT INSTEAD                        
*                                                                               
         LA    R0,1                SET SWITCH TO ALLOW ONLY 1 PASS              
*                                                                               
         MVC   BKEY,KEYSAVE        RESTORE ORIGINAL KEY                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,BKEYINV        DECREMENT INVOICE NUMBER                     
         BCTR  RF,0                DECREMENT NUMBER                             
         STCM  RF,3,BKEYINV                                                     
*                                                                               
         B     MBNXBH20            READ FOR PREVIOUS HEADER ON FILE             
*                                                                               
MBNXBH27 DS    0H                                                               
*                                                                               
         L     R3,SBAIO2           READ BILL HEADER INTO IOA2                   
         ST    R3,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   CSHACT,CSHADDQ      SET ACTION TO ADD                            
*                                                                               
         ST    R3,CSHBLLA          SET A(BILL RECORD)                           
*                                                                               
         GOTO1 VCASHIER,DMCB,CSHIERD    ADD BILL RECORD TO BUFFER               
*                                                                               
         CLI   CSHERR,0            NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MBNXBHFD DS    0H                                                               
*                                                                               
         L     R0,CSHRECA          POINT TO CASHIER RECORD                      
         L     RE,VCSHREC          POINT TO SAVEAREA                            
         LHI   RF,4096             SAVEAREA LENGTH                              
         LR    R1,RF               COPY LENGTH                                  
*                                                                               
         MVCL  RE,R0               SAVE CASHIER RECORD                          
*                                                                               
         L     R3,VCSHREC          POINT TO SAVEAREA                            
         USING BILLRECD,R3                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(2,BDUEDTE)                             
*                                  SAVE INVOICE DUE DATE                        
*                                                                               
         CLI   DTOPT,1             CHECK FOR INVOICE DATE                       
         BNE   MBNXBHF1                                                         
*                                                                               
         CLI   SBWRPROF+14,C'Y'    IF TODAY IS ACTVITY DTE                      
         BNE   MBNXBHFP                                                         
*                                                                               
         MVC   BACTDATE,SBBTODAY      DEFAULT TO TODAY                          
*                                                                               
         B     MBNXBHFQ                                                         
*                                                                               
MBNXBHFP DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(2,BACTDATE)                            
*                                    DEFAULT TO DUE DATE                        
MBNXBHFQ DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,BDATE),(2,BRUNDATE)                               
*                                  SAVE BILL RUN DATE                           
*                                                                               
         CLI   INVDTOPT,C'Y'       IF WE NEED INVOICE DATE                      
         BNE   MBNXBHF2                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,BQDATE),(2,BINVDATE) SAVE INVOICE DATE            
*                                                                               
         B     MBNXBHF2                                                         
*                                                                               
MBNXBHF1 DS    0H                                                               
*                                                                               
         CLI   SBWRPROF+14,C'Y'    IF TODAY IS ACTVITY DTE                      
         BNE   MBNXBHFA                                                         
*                                                                               
         MVC   BACTDATE,SBBTODAY      DEFAULT TO TODAY                          
*                                                                               
         B     MBNXBHFB                                                         
*                                                                               
MBNXBHFA DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,BQDATE),(2,BACTDATE) SAVE INVOICE DATE            
*                                                                               
MBNXBHFB DS    0H                                                               
*                                                                               
         CLI   INVDTOPT,C'Y'       IF WE NEED INVOICE DATE                      
         BNE   MBNXBHFC                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,BQDATE),(2,BINVDATE) SAVE INVOICE DATE            
*                                                                               
MBNXBHFC DS    0H                                                               
*                                                                               
MBNXBHF2 DS    0H                                                               
*                                                                               
         ST    R3,SVBHDELA         SAVE BILL RECORD ADDRESS                     
*                                  ALSO NOW HAS CASH APPLIED DATA               
*                                                                               
MBNXBHX  DS    0H                                                               
*                                                                               
*        FIND MEDIA TRANSFER AND CASH TRANSACTION ELEMENTS                      
*                                                                               
         OC    SBQBHEST(4),SBQBHEST SKIP IF NO EDIT DATE FILTERING              
         BZ    MBNXBHDX                                                         
*                                                                               
         L     R3,SVBHDELA         LOAD BILL RECORD ADDRESS                     
         USING BILLRECD,R3                                                      
*                                                                               
         CLC   BEDIDTE,SBQBHEST    BILL MUST BE IN FILTER RANGE                 
         BL    *+14                                                             
         CLC   BEDIDTE,SBQBHEEN                                                 
         BNH   *+8                                                              
         MVI   CHKLINE,C'N'        DROP RECORD FROM SORT                        
*                                                                               
MBNXBHDX DS    0H                                                               
*                                                                               
         L     R5,SBACURCH         ESTABLISH CURRENT STATION BILL ELM           
         USING STABELEM,R5                                                      
*                                                                               
         TM    CASHOPT,X'80'       SKIP IF NOT COLLECTING CASH DATA             
         BNO   MBNXCSDN                                                         
*                                                                               
         ICM   R3,15,SVTRNELA      POINT TO CURRENT NEXT TRN ELM                
         BZ    MBNXCS10                                                         
*                                                                               
         USING TRNELD,R3           ESTABLISH AS TRANSACTION ELEMENT             
*                                                                               
         LLC   RF,TRNLN            BUMP TO NEXT ELEMENT                         
         LA    R3,0(RF,R3)                                                      
*                                                                               
         B     MBNXCS20                                                         
*                                                                               
MBNXCS10 DS    0H                                                               
*                                                                               
         L     R3,SVBHDELA         POINT TO BILL HEADER FROM CASHIER            
         LA    R3,256(R3)          POINT TO CASH APPLIED DATA                   
*                                                                               
MBNXCS20 DS    0H                                                               
*                                                                               
         XC    SVTRNNXA,SVTRNNXA   INIT NEXT CASH TRANSACTION ELM ADDR          
*                                                                               
MBNXCSLP DS    0H                                                               
*                                                                               
         USING MBTELD,R3           ESTABLISH AS MEDIA TRANSFER ELEMENT          
*                                                                               
         CLI   MBTEL,0             DONE IF END OF RECORD REACHED                
         BE    MBNXCSDN                                                         
*                                                                               
         CLI   MBTEL,MBTELQ        SKIP IF NOT A MEDIA TRANSFER ELMENT          
         BNE   *+12                                                             
         ST    R3,SVMBTNXA         SAVE ADDRESS                                 
         B     MBNXCSCN                                                         
*                                                                               
         USING TRNELD,R3           ESTABLISH AS TRANSACTION ELEMENT             
*                                                                               
         CLI   TRNEL,TRNELQ        SKIP IF NOT TRANSACTION ELEMENT              
         BE    *+8                                                              
         CLI   TRNEL,X'FF'         SKIP IF NOT TRANSACTION ELEMENT              
         BNE   MBNXCSCN                                                         
*                                                                               
         CLI   TRNTYPE,X'09'       SKIP IF ORIGINAL POSTING ELEMENT             
         BE    MBNXCSCN                                                         
*                                                                               
         ST    R3,SVTRNNXA         SAVE ADDRESS                                 
         B     MBNXCSFD                                                         
*                                                                               
MBNXCSCN DS    0H                                                               
*                                                                               
         USING MBTEL,R3            ESTABLISH AS MEDIA TRANSFER ELEMENT          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,MBTLLN           BUMP TO NEXT ELEMENT                         
         LA    R3,MBTEL(RF)                                                     
*                                                                               
         B     MBNXCSLP                                                         
*                                                                               
MBNXCSDN DS    0H                                                               
*                                                                               
         OC    SVTRNELA,SVTRNELA   IF NO CURRENT TRAN ELM                       
         BNZ   MBNXCSD1               (I.E. NO CASH APPLIED YET TO              
*                                     ZERO INVOICE)                             
         MVC   SVMBTELA,SVMBTNXA      SET POSTING ELM ADDRESS                   
*                                     ZERO INVOICE)                             
         OC    SVMBTELA,SVMBTELA      IF WE HAVE A POSTING ELM                  
         BZ    MBNXCSD1                                                         
*                                                                               
         XC    TRNELM,TRNELM                                                    
         LA    R3,TRNELM                 BUILD DUMMY TRANSACTION ELM            
         USING TRNELD,R3                                                        
*                                                                               
         MVI   TRNEL,X'FF'               MAKE IT UNAALOCATED                    
         MVI   TRNLN,30                  NORMAL ELEMENT LENGTH                  
         ZAP   TRNAMNT,=P'0'             NO CASH ALLOCATED                      
*                                                                               
         ST    R3,SVTRNNXA               USE THIS DUMMY ELEMENT                 
*                                                                               
MBNXCSD1 DS    0H                                                               
*                                                                               
         B     MBNXOLD                                                          
*                                                                               
MBNXCSFD DS    0H                                                               
*                                                                               
         OC    SVTRNELA,SVTRNELA   IF NO CURRENT TRAN ELM                       
         BNZ   *+20                   (I.E. FIRST TIME FOR CASH RECORD)         
         MVC   SVMBTELA,SVMBTNXA      USE FOUND ELEMENT                         
         MVC   SVTRNELA,SVTRNNXA      USE FOUND ELEMENT                         
         B     MBNXCSLP               AND GO BACK FOR MORE                      
*                                                                               
*        DETERMINE MODE                                                         
*                                                                               
MBNXOLD  DS    0H                                                               
*                                                                               
         OC    SVTRNNXA,SVTRNNXA   IF CASH DATA TO COME                         
         BZ    *+8                                                              
         OI    OPTIND4,OPTRPTDR       ASK TO REPEAT DRIVIN CALL                 
*                                                                               
MBTNXTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SPWRITER - CASH APPLIED DATA - INPUT - ICSHSNT'                 
***********************************************************************         
*                                                                     *         
*        STANET OPTION                                                *         
*          IDETERMINE WHEN TO RESET TABLES WHEN TOTALLING LOCAL       *         
*              CABLE NETWORKS                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ICSHSNT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,SBAIO1           POINT TO CURRENT BUY/STATION BILL            
*                                                                               
*        CHECK FOR SIGNIFICANT CHANGE IN BUY KEY                                
*                                                                               
ISTNBUY  DS    0H                                                               
*                                                                               
         CLI   SBMODE,SBPROCSP     IF BUY RECORD                                
         BNE   ISTNBUYN                                                         
*                                                                               
         MVC   ISTNMKST,BUYKMSTA-BUYKEY(R2)   SAVE MKT/STA                      
*                                                                               
*        UNPACK MARKET STATION                                                  
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',ISTNMKST),ISTNMKTC,ISTNSTAC                   
*                                                                               
         CLC   ISTNKEY(BUYKSTAC-BUYKEY),0(R2) RESET ON MKT CHANGE               
         BNE   ISTNRSET                                                         
*                                                                               
         B     ISTN100                                                          
*                                                                               
ISTNBUYN DS    0H                                                               
*                                                                               
*        CHECK FOR SIGNIFICANT CHANGE IN NEW INVOICE KEY                        
*                                                                               
ISTNSNV  DS    0H                                                               
*                                                                               
         CLI   SBMODE,SBPROCNV     IF NEW INVOICE RECORD                        
         BNE   ISTNSNVN                                                         
*                                                                               
         XC    ISTNMKT,ISTNMKT        CLEAR MARKET NUMBER                       
         MVC   ISTNSTA,SNVKSTA-SNVKEY(R2)   SAVE STATION CODE                   
*                                                                               
*        UNPACK MARKET STATION                                                  
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',ISTNMKST),ISTNMKTC,ISTNSTAC                   
*                                                                               
         CLC   ISTNKEY(SNVKSTA-SNVKEY),0(R2) RESET ON CLT CHANGE                
         BNE   ISTNRSET                                                         
*                                                                               
         CLC   SNVKMOS-SNVKEY+ISTNKEY,SNVKMOS-SNVKEY(R2)                        
         BNE   ISTNRSET            RESET ON MOS CHANGE                          
*                                                                               
         B     ISTN100                                                          
*                                                                               
ISTNSNVN DS    0H                                                               
*                                                                               
*        CHECK FOR SIGNIFICANT CHANGE IN STATION BILL KEY                       
*                                                                               
ISTNSTB  DS    0H                                                               
*                                                                               
         CLI   SBMODE,SBPROCBL     IF STATION BILL RECORD                       
         BE    *+8                                                              
         CLI   SBMODE,SBPROCCA          OR CASH APPLIED                         
         BNE   ISTNSTBN                                                         
*                                                                               
         MVC   ISTNMKT,STABKMKT-STABUCK(R2)   SAVE MARKET NUMBER                
         MVC   ISTNSTA,STABKSTA-STABUCK(R2)   SAVE STATION CODE                 
*                                                                               
*        UNPACK MARKET STATION                                                  
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',ISTNMKST),ISTNMKTC,ISTNSTAC                   
*                                                                               
         CLC   ISTNKEY(STABKSTA-STABUCK),0(R2) RESET ON MKT CHANGE              
         BNE   ISTNRSET                                                         
*                                                                               
         B     ISTN100                                                          
*                                                                               
ISTNSTBN DS    0H                                                               
*                                                                               
         CLC   ISTNKEY(L'BUYKEY),0(R2)   RESET ON KEY CHANGE                    
         BE    *+14                                                             
         XC    SVHEAD,SVHEAD       FORCE CHANGE IN HEAD END                     
         B     ISTNRSET                                                         
*                                                                               
         B     ISTNOKAY                                                         
*                                                                               
ISTN100  DS    0H                                                               
*                                                                               
*        TEST FOR CHANGE IN HEAD END                                            
*                                                                               
         CLC   SVHEAD,ISTNSTAC     RESET ON HEAD END CHANGE                     
         BE    ISTNOKAY                                                         
*                                                                               
         B     ISTNRSET                                                         
*                                                                               
ISTNRSET DS    0H                                                               
*                                                                               
         MVC   ISTNKEY,0(R2)       SAVE KEY                                     
         MVC   SVHEAD,ISTNSTAC     SAVE HEAD END                                
*                                                                               
         LTR   RB,RB               SET NE CC                                    
         B     ICSHSNTX                                                         
*                                                                               
ISTNOKAY DS    0H                                                               
         CR    RB,RB               SET EQ CC                                    
*                                                                               
ICSHSNTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
ISTNKEY  DS    XL(L'BUYKEY)        KEY SAVEAREA                                 
*                                                                               
ISTNMKST DS    0XL5                MARKET STATION                               
ISTNMKT  DS    XL2                 MARKET CODE                                  
ISTNSTA  DS    XL3                 STATION CODE                                 
*                                                                               
SVHEAD   DS    CL5                 HEADEND                                      
ISTNMKTC DS    CL4                 UNPACKED MARKET NUMBER                       
ISTNSTAC DS    CL8                 UNPACKED STATION CALL LETTERS                
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SPWRITER - CASH APPLIED DATA - INPUT - IPDNET'                  
***********************************************************************         
*                                                                     *         
*        PAID NET AMOUNT                                              *         
*          IF PAYPEND OPTION, RETURN ZERO FOR DISBURSED SPOTS         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IPDNET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   0(8,R2),=P'0'       INIT PAID AMOUNT                             
*                                                                               
         CLI   SBMODE,SBPROCSP     SKIP IF NOT PROCESSING BUY                   
         BNE   IPDNETX                                                          
*                                                                               
         L     R1,SBACURCH                                                      
         USING SCHUNKD,R1          ESTABLISH CHUNK                              
*                                                                               
         ICM   RF,15,SCPAYTLN      GET NET PAID AMOUNT                          
         CVD   RF,DUB              CVD                                          
         SRP   DUB,1,0             *10 FOR LATER ROUNDING                       
         ZAP   0(8,R2),DUB         RETURN PAID AMOUNT                           
*                                                                               
         CLI   CBUOPT,C'Y'         SKIP IF CBU                                  
         BE    *+8                                                              
         CLI   CHKSOPT,C'0'        OR PAYPRND                                   
         BE    IPDNET05                                                         
*                                                                               
         CLC   SCCHKNUM,SPACES     IF UNDISBURSED                               
         BH    IPDNET05                                                         
*                                                                               
         ZAP   0(8,R2),=P'0'          INIT PAID AMOUNT                          
*                                                                               
         B     IPDNET10                                                         
*                                                                               
IPDNET05 DS    0H                                                               
*                                                                               
*        DONE IF FIRST 03 ELEMENT                                               
*        ELSE RETURN ZERO                                                       
*                                                                               
         ICM   R5,15,ACLRST01      POINT TO 01 ELEMENT                          
         BZ    IPDNET10            SKIP IF NONE THERE                           
*                                                                               
         OC    ACLRST03,ACLRST03   DONE IF NO 03 ELEMENT                        
         BZ    IPDNET10                                                         
*                                                                               
         LLC   R3,1(R5)            GET ELEMENT LENGTH                           
         LA    R3,0(R3,R5)         POINT TO NEXT ELEMENT                        
*                                                                               
         CLM   R3,15,ACLRST03      DONE IF PASSED 03 ELEMENT                    
         BE    IPDNET10                                                         
*                                  ELSE 2ND OR OTHER 03 ELEMENT                 
         ZAP   0(8,R2),=P'0'          RETURN ZERO                               
*                                                                               
         B     IPDNET10                                                         
*                                                                               
         ZAP   0(8,R2),=P'0'       INIT PAID AMOUNT                             
*                                                                               
         TP    SCPAYNP             SKIP IF NOT PACKED                           
         BNZ   *+10                                                             
         ZAP   0(8,R2),SCPAYNP     RETURN PAID NET TO TENTHS OF A CENT          
*                                                                               
         B     IPDNET10                                                         
*                                                                               
*        DEACTIVATED                                                            
*                                                                               
*        CALCULATE PER CENT OF PAID NET FOR THIS LINE                           
*                                                                               
         ICM   R3,15,ACLRST03      DONE IF NO INVOICE ELEMENT                   
         BZ    IPDNET10                                                         
*                                                                               
         LLC   RF,1(R3)            GET ELEMENT LENGTH                           
         LA    R3,0(RF,R3)         POINT TO NEXT ELEMENT                        
*                                                                               
         CLI   0(R3),X'05'         DONE IF NOT AN 05 ELEMENT                    
         BNE   IPDNET10                                                         
*                                                                               
         USING CLSTEL05,R3         ESTABLISH 05 ELEMENT                         
*                                                                               
         ICM   R5,15,ACLRST01      POINT TO 01 ELEMENT                          
         BZ    IPDNET10            SKIP IF NONE THERE                           
*                                                                               
         USING CLSTEL01,R5         ESTABLISH 01 ELEMENT                         
*                                                                               
         ICM   RF,15,CLS5GRS       ASSUME CLEARED GROSS                         
         TM    CLSTSTAT,X'20'      IF CLEARED NET                               
         BNO   *+8                                                              
         ICM   RF,15,CLS5NET          USE NET AMOUNT                            
*                                                                               
         CVD   RF,WPKQUOT                                                       
         ZAP   WPKDIVD,WPKQUOT     SAVE CLEARED AMOUNT                          
         SRP   WPKDIVD,6,0         SCALE UP FOR ROUNDING                        
*                                                                               
         ICM   RF,15,CLSTGRS       ASSUME CLEARED GROSS                         
         TM    CLSTSTAT,X'20'      IF CLEARED NET                               
         BNO   *+8                                                              
         ICM   RF,15,CLSTNET          USE NET AMOUNT                            
*                                                                               
         LTR   RF,RF               SKIP IF ZERO DIVIDE                          
         BZ    IPDNET10                                                         
*                                                                               
         CVD   RF,DUB                                                           
*                                                                               
         DP    WPKDIVD,DUB         CALCULATE PERCENT FOR INV                    
*                                                                               
         SRP   WPKQUOT,64-1,5      ROUND TO 5 DECIMALS                          
*                                                                               
         ZAP   DUB,WPKQUOT                                                      
*                                                                               
         ZAP   WPKDIVD,0(8,R2)     TOTAL PAID AMOUNT                            
         MP    WPKDIVD,DUB         * PER CENT FOR THIS INVOICE                  
         SRP   WPKDIVD,64-4,05     ROUND TO TENTHS OF A PENNY                   
         ZAP   0(8,R2),WPKDIVD     10 * PAID NET FOR THIS INVOICE               
*                                                                               
IPDNET10 DS    0H                                                               
*                                                                               
         CLI   CHKSOPT,C'0'        SKIP IF NOT PAYPEND OPTION                   
         BNE   IPDNETX                                                          
*                                                                               
*        PAYPENDING OPTION                                                      
*                                                                               
         CLC   SCCHKNUM,SPACES     OK FOR SPOTS WITH NO CHECKS                  
         BNH   IPDNETX                                                          
*                                                                               
         CLC   =C'VOID',SCCHKNUM   OK FOR SPOTS WITH VOID CHECKS                
         BE    IPDNETX                                                          
*                                                                               
         ZAP   0(8,R2),=P'0'       ELSE ELIMINATE AMOUNT                        
*                                                                               
IPDNETX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - OPDNET'               
***********************************************************************         
*                                                                     *         
*        PAID OUTPUT ROUTINE                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OPDNET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AGLOBAL          ** INPUT ROUTINE **                          
         USING GLOBALD,R4                                                       
*                                                                               
         L     R1,GLADTENT                                                      
         USING DROD,R1             ESTABLISH DRIVE TABLE ENTRY                  
*                                                                               
         SRP   0(8,R2),64-1,5      ROUND TO PENNIES                             
*                                                                               
         TM    COLIND,COLIRND      IF ROUNDING                                  
         BNO   *+12                                                             
         MVI   DRODEC,0               NO DECIMALS FOR ROUNDING                  
         MVI   DRODIV,2               ROUND OUT PENNIES                         
*                                                                               
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE                         
         BO    OPNTOT                                                           
*                                                                               
         ZAP   CLRAMT,0(8,R2)      SAVE CLEARED AMOUNT                          
*                                                                               
*        PRINT PAID AMOUNT                                                      
*                                                                               
         CLI   CHKSOPT,C'0'        SKIP IF PAYPENDING                           
         BE    OPNEDIT                                                          
*                                                                               
         CP    0(8,R2),=P'0'       DO NOT PRINT ZERO AMOUNTS                    
         BE    OPDNETX                                                          
         BL    OPNCR               CREDIT                                       
*                                                                               
         LA    RF,TBKPDNDB         ==> TO CLEARED TOTAL BUCKETS                 
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RF),0(8,R2) INCREMENT ALL LVL BUCKETS                 
         LA    RF,L'TOTBKS(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         B     OPN10                                                            
*                                                                               
OPNCR    DS    0H                  CREDITS                                      
*                                                                               
         LA    RF,TBKPDNCR         ==> TO CLEARED TOTAL BUCKETS                 
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RF),0(8,R2) INCREMENT ALL LVL BUCKETS                 
         LA    RF,L'TOTBKS(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
OPN10    DS    0H                                                               
*                                                                               
         B     OPNEDIT                                                          
*                                                                               
*        TOTALS                                                                 
*                                                                               
OPNTOT   DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,GLLEVEL        GET CURRENT TOTAL LEVEL                      
         BZ    *+6                 GRAND TOTAL                                  
         BCTR  RF,0                DECREMENT FOR INDEXING                       
*                                                                               
         LA    RE,L'TOTBKS         BUCKET LENGTH                                
         MR    RE,RE               DISP INTO BUCKETS OF THIS LEVEL              
*                                                                               
         LA    RE,TBKPDNDB         ==> TO CLEARED DEBIT BUCKETS                 
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   CLRTOTDB,0(L'TOTBKS,R1)  SAVE THIS LEVEL TOTAL                   
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR THIS LEVEL TOTAL                   
*                                                                               
         LA    RE,TBKPDNCR         ==> TO CLEARED CREDIT BUCKETS                
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   CLRTOTCR,0(L'TOTBKS,R1)   ADD IN THIS LEVEL TOTAL                
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR  THIS LEVEL TOTAL                  
*                                                                               
         ZAP   CLRTOT,CLRTOTDB     PRINT COMBINED TOTAL                         
         AP    CLRTOT,CLRTOTCR                                                  
*                                                                               
         B     OPNEDIT             LET DRIVER PRINT IT                          
*                                                                               
         EDIT  CLRTOT,(14,0(R3)),2,COMMAS=YES,FLOAT=-,ZERO=BLANK                
*                                                                               
         B     OPDNETX                                                          
*                                                                               
OPNEDIT  DS    0H                                                               
*                                                                               
OPNEDIT1 DS    0H                                                               
*                                                                               
         MVI   GLHOOK,GLEDIT       LET DRIVER PRINT IT                          
*                                                                               
OPDNETX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SPWRITER - CASH APPLIED DATA - INPUT - WORKD'                   
*                                                                               
* WORKING STORAGE                                                               
*                                                                               
WORKD    DS    0D                                                               
*                                                                               
OPTERRM  DC    C'ERROR - INVALID OPTION'                                        
         DS    0D                                                               
         DS    0D                                                               
*                                                                               
CSHIERC  DS    XL(CSHIERL)         DDCASHIER CONTROL BLOCK                      
*                                                                               
BDPARM   DS    0F                  PARM LIST FOR BINSRCH CALL FOR               
BPIND    DS    0XL1                BILL FORMULA BUFFER                          
BPAREC   DS    A                                                                
BPATAB   DS    A                                                                
         DC    F'0'                                                             
BPLREC   DC    AL4(L'BDREC)                                                     
         DC    AL1(0),AL3(L'BDKEY)                                              
BPMAX    DC    F'10000'            MAX N'RECORDS                                
BPLTAB   DS    F                   L'TABLE                                      
*                                                                               
VCASHIER DS    V(CASHIER)          V(DDCASHIER)                                 
VTSAROFF DS    V(TSAROFF)          V(TSAROFF)                                   
*                                                                               
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
FF       EQU   X'FF'                                                            
WDATEOPT DS    XL1                 DATE FORMAT OPTION                           
         EJECT                                                                  
*                                                                               
* HEADLINE POSITION TABLE                                                       
*                                                                               
HEADTAB  DC    C'N',X'01',X'00',X'000000',X'00',X'00'                           
         DC    C'N',X'01',X'01',X'050000',X'00',X'00'                           
         DC    C'N',X'01',X'02',X'050600',X'00',X'00'                           
         DC    C'N',X'01',X'03',X'050607',X'00',X'00'                           
         DC    C'Y',X'01',X'00',X'000000',X'00',X'07'                           
         DC    C'Y',X'01',X'01',X'050000',X'00',X'07'                           
         DC    C'Y',X'01',X'02',X'050600',X'00',X'08'                           
         DC    C'Y',X'01',X'03',X'050607',X'00',X'08'                           
         DC    C'A',X'02',X'00',X'000000',X'05',X'07'                           
         DC    C'A',X'02',X'01',X'050000',X'06',X'08'                           
         DC    C'A',X'02',X'02',X'040500',X'06',X'08'                           
         DC    C'A',X'02',X'03',X'040506',X'07',X'08'                           
         DC    C'A',X'03',X'00',X'000000',X'05',X'07'                           
         DC    C'A',X'03',X'01',X'050000',X'06',X'08'                           
         DC    C'A',X'03',X'02',X'040500',X'06',X'08'                           
         DC    C'A',X'03',X'03',X'040506',X'07',X'08'                           
         DC    X'00'                                                            
*                                                                               
MONTAB1  DC    C'0131'                                                          
         DC    C'0228'                                                          
         DC    C'0331'                                                          
         DC    C'0430'                                                          
         DC    C'0531'                                                          
         DC    C'0630'                                                          
         DC    C'0731'                                                          
         DC    C'0831'                                                          
         DC    C'0930'                                                          
         DC    C'1031'                                                          
         DC    C'1130'                                                          
         DC    C'1231'                                                          
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
AXTRA    DS    0F               ** EXTENTION ROUTINE ADDRESSES **               
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
SAVERD   DS    A                                                                
RELO     DS    A                                                                
WPKDIVD  DS    0PL16               DIVIDEND  - PACKED                           
WPKQUOT  DS    PL8                 QUOTIENT  - PACKED                           
WPKREMD  DS    PL8                 REMAINDER - PACKED                           
*                                                                               
CSHTBLVS DS    0A                  ADDRESSES IN T2041A WORKAREA OVERLAY         
VINVTBL  DC    A(0)               A(INVOICE SQN NUMBER TABLE)                   
VSTBTBL  DC    A(0)               A(STATION BILLING SQN NUMBER TABLE)           
VCSHREC  DC    A(0)               A(CASHIER RECORD SAVE AREA)                   
VCLTREC  DC    A(0)               A(CLIENT RECORD SAVEAREA)                     
         DC    16A(0)              SPARE                                        
*                                                                               
MYQUOTE  DS    D                                                                
MYREMAIN DS    D                                                                
MYDUB    DS    D                                                                
*                                                                               
SVUTLA   DS    A                   A(UTL)                                       
SVBKEY   DC    XL(L'BKEY)'00'                                                   
SVKEY    DS    XL13                                                             
SVSTAB   DS    XL21                STATION BILL ELEMENT SAVEAREA                
SVINV    DS    CL(L'CLS3INV)       CHECK INVOICE SAVE                           
SVCHKNUM DS    CL(L'SCCHKNUM)      CHECK NUMBER SAVE                            
SVINVNO  DS    CL10                VENDOR INVOICE NUIMBER SAVE                  
SVDSLN   DS    XL1                 SPOTLENGTH SAVEAREA                          
SVDPRD   DS    XL1                 PRODUCT    SAVEAREA                          
SVDPR2   DS    XL1                 PRODUCT 2  SAVEAREA                          
SVDEST   DS    XL1                 ESTIMATE   SAVEAREA                          
SVCLRDT  DS    XL2                 CLEAR DATE SAVEARE                           
SVCLRSQ  DS    XL1                 CLEAR SEQ  SAVEAREA                          
SVCSHNM  DS    CL6                 CLIENT CHECK NUMBER                          
SVCSHDT  DS    XL2                 CLIENT CHECK DATE                            
SVPAYN   DS    F                   CURRENT PAID NET                             
*                                                                               
PRNT1ST  DC    C'Y'                FIRST TIME TO PRINTING SWITCH                
*                                                                               
QPERLEV  DS    XL1                                                              
*                                                                               
DOLLAR   DS    CL1                 NET/GROSS                                    
BILLOPT  DS    CL1                 Y/N/1/2                                      
DTOPT    DS    CL1                 INVOICE(DEFAULT)/DUE                         
INVDTOPT DS    CL1                 INVOICE DATE OPTION                          
DAYS     DS    CL1                 PRINT DAYS                                   
SPOTOPT  DS    CL1                 SHOW SPOTS                                   
ORDERED  DS    CL1                 ORDERED - GROSS/NET                          
BLNK     DS    CL1                 BLANK LINE                                   
CALC     DS    CL1                 CALC MONTH FROM WR PROFILE                   
PSUBT    DS    CL1                 PRINT SUBTOTALS                              
CASHOPT  DS    CL1                 Y/N                                          
VENDOPT  DS    CL1                 V/N - PRINT VENDOR INVOICE DATA              
CHKSOPT  DS    CL1                 Y/N                                          
XPCTOPT  DS    CL1                 Y/N - PRINT CLIENT PER CENT PAID             
BANKOPT  DS    CL1                 Y/N - PRINT BANK DEPOSIT DATE                
CHKIVOPT DS    CL1                 Y/N - CHECK INVOICE NUMBER                   
CHKRPOPT DS    CL1                 Y/N - CHECK REP                              
CBUOPT   DS    CL1                 Y/N - REPT CLRD BUT UNDISBURSED              
STANETOP DS    CL1                 Y/N - SUMMARISE AT HEADEND LEVEL             
STANETQ  EQU   C'Y'                C'Y' - OPTION IS ON                          
NOVDRDTE DS    CL1                 Y/N - NO VENDOR INVOICE DATE                 
*                                                                               
INVLINE  DS    CL1                 PRINT INVOICE ON THIS LINE                   
SW13PRSW DS    C                   C'X' - BACK LINE OUT OF TOTALS               
LSQN     DS    XL2                 LINE SEQUENCE NUMBER ON OUTPUT               
LBACTDT  DS    CL6                 BILL ACTIVITY DATE FOR LSQN = 1              
*                                                                               
LINESTAT DS    XL1                 LINE STATUS - 'P' -PRINT LINE                
STATSW   DS    XL1                 C'Y' - SAVE LINE STATUS                      
SVSTAMOS DS    0XL9                STATION/MOS                                  
SVPRD    DS    XL1                 PRODUCT CODE                                 
SVEST    DS    XL1                 ESTIMATE NUMBER                              
SVMKTSTA DS    CL5                 MARKET/STATION                               
SVMOS    DS    XL2                 MONTH OF SERVICE                             
SVNETWK  DS    XL2                 NETWORK CODE                                 
*                                                                               
*                                                                               
OPSTAMOS DS    XL9                 STAION/MOS - OUTPUT                          
SVBLLAMT DS    PL8                 CURRENT SAVED BILLED DOLLARS                 
SVCLRAMT DS    PL8                 CURRENT SAVED CHECK  DOLLARS                 
SVBLLDLR DS    PL8                 CURRENT SAVED BILL   DOLLAR DAYS             
SVCHKDLR DS    PL8                 CURRENT SAVED CHECK  DOLLAR DAYS             
*                                                                               
INVNO    DS    H                   INVOICE NUMBER                               
BILLYR   DS    XL1                                                              
BILLMON  DS    XL1                                                              
BILLYM   DS    XL1                                                              
*                                                                               
CHKDATE  DS    XL2                 CHECK DATE                                   
BACTDATE DS    XL2                 BILLING ACTIVITY DATE                        
BINVDATE DS    XL2                 BILLING INVOICE  DATE                        
BRUNDATE DS    XL2                 BILLING RUN      DATE                        
BDUEDTE  DS    XL2                 BILLING DUE      DATE                        
INVNUM   DS    XL3                 INVOICE NUMBER                               
***                                                                             
CHKNUM   DS    CL7                 CHECK NUMBER                                 
CHKLINE  DS    CL1                 THIS LINE HAS A CHECK NUMBER                 
LDMON    DS    CL6                 CALCULATION DATE                             
LDMON1   DS    CL6                 LAST DAY OF BROADCAST MONTH                  
ACTDT    DS    CL6                 ACTIVITY DATE                                
BACTDT   DS    CL6                 BILLING  ACTIVITY DATE                       
CACTDT   DS    CL6                 CHECKING ACTIVITY DATE                       
CSHACTDT DS    CL6                 CASH CHECKING ACTIVITY DATE                  
CSHCHKNM DS    CL6                 CASH CHECK NUMBER                            
BNKDTOPT DS    CL1                 C'Y' - USE BANK RECONCILIATION DATE          
VDRBNKDT DS    XL1                 C'Y' - USE VENDOR BANK DATE                  
         DS    0D                  ALIGNMENT                                    
CLRAMT   DC    PL8'0'              THIS ITEM'S CLEARANCE AMOUNT                 
CLRPCT   DC    PL8'0'              THIS ITEM'S PER CENT OF PD AMOUNT            
CLRTOT   DS    PL8                 CLEARANCE TOTAL                              
CLRTOTDB DS    PL8                 CLEARANCE TOTAL - DEBIT                      
CLRTOTCR DS    PL8                 CLEARANCE TOTAL - CREDIT                     
BLLAMTP  DS    PL8                 THIS ITEMS BILLED    AMOUNT                  
BLLTOT   DS    PL8                 BILLED    TOTAL                              
BLLTOTDB DS    PL8                 BILLED    TOTAL - DEBIT                      
BLLTOTCR DS    PL8                 BILLED    TOTAL - CREDIT                     
CSHPCT   DS    F                   PER CENT CASH APPLIED TO BILL                
CSHAMTP  DS    PL8                 THIS ITEMS CASH APPLIED                      
CSHTOT   DS    PL8                 CASH APPLIED TOTAL                           
CSHTOTDB DS    PL8                 CASH APPLIED TOTAL - DEBIT                   
CSHTOTCR DS    PL8                 CASH APPLIED TOTAL - CREDIT                  
DOLTOT   DS    PL8                 DOLLAR DAYS TOTAL                            
TBDOLDB  DS    PL8                 BILLING  DOLLAR DAYS TOTAL - DEBIT           
TBDOLCR  DS    PL8                 BILLING  DOLLAR DAYS TOTAL - CREDIT          
TCDOLDB  DS    PL8                 CHECKING DOLLAR DAYS TOTAL - DEBIT           
TCDOLCR  DS    PL8                 CHECKING DOLLAR DAYS TOTAL - CREDIT          
DAYSTOT  DS    F                   DAYS TOTAL                                   
BDAYSTOT DS    F                   BILLING/CASH DAYS TOTAL                      
CDAYSTOT DS    F                   CHECKING     DAYS TOTAL                      
*                                                                               
WRPROF   DS    CL16                WR - PROFILE                                 
*                                                                               
BDREC    DS    0CL17               BILL - ACTIVITY DATE TABLE RECORD            
BDKEY    DS    0CL11                                                            
BDCLT    DS    XL2                                                              
BDPRD    DS    CL3                                                              
BDEST    DS    XL1                                                              
BDYMSER  DS    XL2                                                              
BDBILYM  DS    XL1                                                              
BDINUM   DS    XL2                                                              
BDDATA   DS    0CL6                                                             
BDDATE   DS    CL6                                                              
*                                                                               
CHKDTOPT DS    CL1                 Y/N - CHECK DATE                             
*                                                                               
SVCSHB#  DS    CL6                 CLIENT BATCH NUMBER                          
*                                                                               
       ++INCLUDE DDEBLOCK                                                       
         DS    0D                                                               
SVLSQN   DS    XL1                 LINE SEQUENCE NUMBER SAVEAREA                
SVLSQN2  DS    XL1                 LINE SEQ # - 2       SAVEAREA                
SVBHDELA DS    A                   A(CASHIER BUFFER ENTRY)                      
SVMBTELA DS    A                   A(MEDIA TRANSFER    ELEMENT)                 
SVMBTNXA DS    A                   A(NEXT MEDIA TRANSFER    ELEMENT)            
SVTRNELA DS    A                   A(CASH  TRANSACTION ELEMENT)                 
SVTRNNXA DS    A                   A(NEXT CASH  TRANSACTION ELEMENT)            
TOTDAYA  DS    A                   A(DAYS TOTAL PRINT AREA)                     
TRNELM   DS    XL256               DUMMY TRANSACTION ELEMENT                    
*                                                                               
*        BUFFERS TO HANDLE TOTALING ON OUTPUT - MAX 20 LEVELS OF TOTALS         
*                                                                               
TOTBKS   DS    0PL8                TOTALS BUCKETS                               
TBKBLLDB DS    20PL8               BILL         TOTAL     BUCKETS-DEBIT         
TOTNMLVQ EQU   (*-TOTBKS)/L'TOTBKS NUMBER OF LEVEL BUCKETS                      
TBKBLLCR DS    20PL8               BILL         TOTAL     BUCKETS-CREDT         
TBKCLRDB DS    20PL8               CLEARANCE    TOTAL     BUCKETS-DEBIT         
TBKCLRCR DS    20PL8               CLEARANCE    TOTAL     BUCKETS-CREDT         
TBKCSHDB DS    20PL8               CASH         TOTAL     BUCKETS-DEBIT         
TBKCSHCR DS    20PL8               CASH         TOTAL     BUCKETS-CREDT         
TBKBLDDB DS    20PL8               DISBURSEMENT BILL      BUCKETS-DEBIT         
TBKBLDCR DS    20PL8               DISBURSEMENT BILL      BUCKETS-CREDT         
TBKCLDDB DS    20PL8               DISBURSEMENT CLEARANCE BUCKETS-DEBIT         
TBKCLDCR DS    20PL8               DISBURSEMENT CLEARANCE BUCKETS-CREDT         
TBKPDNDB DS    20PL8               PAID NET     TOTAL     BUCKETS-DEBIT         
TBKPDNCR DS    20PL8               PAID NET     TOTAL     BUCKETS-CREDT         
TOTNMBKQ EQU   (*-TOTBKS)/L'TOTBKS TOTAL NUMBER OF BUCKETS                      
*                                                                               
SVCHKTAB DS    0D                  CHECK NUMBER TABLE                           
SVTMOS   DS    XL2                 MONTH OF SERVICE                             
SVTPRD   DS    XL1                 PRODUCT                                      
SVTEST   DS    XL1                 ESTIMATE                                     
SVTCLRDT DS    XL2                 CLEARANCE DATE                               
SVTCLRSQ DS    XL1                 CLEARANCE SEQUENCE #                         
SVTINV   DS    XL(L'CLS3INV)       INVOICE NUMBER                               
SVTCHK   DS    XL(L'SCCHKNUM)      CHECK NUMBER                                 
SVTLSQN  DS    XL(L'SVLSQN)        LINE SEQUENCE NUMBER                         
SVTLSQN2 DS    XL(L'SVLSQN2)       LINE SEQ # - 2                               
SVTENTL  EQU   *-SVCHKTAB          LENGTH OF TABLE ENTRY                        
         DS    XL(400*SVTENTL)      REST OF TABLE                               
*                                                                               
         DS    0D                                                               
PRDLST   DS    XL256                                                            
*                                                                               
WORKL    EQU   *-WORKD                                                          
*                                                                               
         TITLE 'SPWRI13 - CASHFLOW - ICSHIVTD'                                  
***********************************************************************         
*                                                                     *         
*        VENDOR INVOICE SEQUENCE NUMBER TABLE ENTRY                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ICSHIVTD DSECT                                                                  
IVTSLN   DS    XL(L'SNVIDSLN)      SPOT LENGTH                                  
IVTPRD   DS    XL(L'SNVIDPRD)      PRODUCT CODE                                 
IVTPR2   DS    XL(L'SNVIDPR2)      SECOND PRODUCT CODE                          
IVTEST   DS    XL(L'SNVIDEST)      ESTIMATE CODE                                
IVTNETWK DS    XL(L'SVNETWK)       NETWORK CODE                                 
IVTMOS   DS    XL(L'SNVKMOS)       MONTH OF SERVICE COMPRESSED                  
IVTINVNO DS    XL(L'SNVKINV)       INVOICE NUMBER                               
IVTSQN   DS    XL1                 SEQUENCE NUMBER                              
ICSHIVLQ EQU   *-ICSHIVTD          ENTRY LENGTH                                 
*                                                                               
         TITLE 'SPWRI13 - CASHFLOW - ICSHSTTD'                                  
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF ADDRESSES IN WORKAREA OVERLAY T2041A      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CSHTBLD  DSECT                                                                  
         DC    CL8'*CASHTBLS'      EYECATCHER                                   
*                                                                               
*        DISPLACEMENTS TO WORKAREAS                                             
*                                                                               
CSHTBLAS DS    0A                  DISPLACEMENTS TABLE TO WORKAREAS             
ASTBTBL  DS    A                   STATION BILLING TABLE                        
AINVTBL  DS    A                   INVOICE         TABLE                        
ACSHREC  DS    A                   CASHIER RECORD SAVEAREA                      
ACLTREC  DS    A                   CLIENT RECORD  SAVEAREA                      
         DS    16A                 SPARE                                        
*                                                                               
         TITLE 'SPWRI13 - CASHFLOW - ICSHSTTD'                                  
***********************************************************************         
*                                                                     *         
*        STATION BILL SEQUENCE NUMBER TABLE ENTRY                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ICSHSTTD DSECT                                                                  
STTNETWK DS    XL(L'SVNETWK)       NETWORK CODE                                 
         DS    XL1                 SPARE                                        
STTPER   DS    XL(L'STABPER)       BILLING PERIOD                               
STTBDT   DS    XL(L'STABBDT)       INVOICE BILLING DATE (COMPRESSED)            
STTINV   DS    XL(L'STABINV)       INVOICE NUMBER                               
STTCSHB# DS    XL6                 CASH BATCH NUMBER                            
STTCSHNM DS    XL6                 CASH CHECK NUMBER                            
STTCSHDT DS    XL2                 CASH CHECK DATE                              
STTSQN   DS    XL1                 MAJOR SEQUENCE NUMBER                        
STTSQN1  DS    XL1                 MINOR SEQUENCE NUMBER                        
ICSHSTLQ EQU   *-ICSHSTTD          ENTRY LENGTH                                 
*                                                                               
         EJECT                                                                  
HEADTABD DSECT                                                                  
HDDPT    DS    X                   DAYPART OPTION                               
HDREP    DS    X                   REPORT NUMBER                                
HDNMGR   DS    X                   N'MARKET GROUPS                              
HDMGR1   DS    X                   HEADLINE FOR MARKET GROUP 1                  
HDMGR2   DS    X                   HEADLINE FOR MARKET GROUP 2                  
HDMGR3   DS    X                   HEADLINE FOR MARKET GROUP 3                  
HDMKTDPT DS    X                   HEADLINE FOR MARKET OR DAYPART               
HDSUM    DS    X                   HEADLINE FOR MKT OR DPT SUMMARY              
HEADTABL EQU   *-HEADTABD                                                       
         EJECT 1                                                                
       ++INCLUDE DDCASHIERD                                                     
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*SPOTTABD                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*DEDBLOCK                                                                       
*SPGENAGY                                                                       
*SPGENCLRST                                                                     
*DDCOMFACS                                                                      
*DDMASTD                                                                        
*SPWRIFFD                                                                       
*DDCOREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
       ++INCLUDE DDCOREQUS                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENSTAB                                                      
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
                                                                                
       ++INCLUDE SPGENSNV                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENCLRST                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRID3D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 2                                                                
* DDTSARD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'156SPWRI1B   10/10/16'                                      
         END                                                                    
